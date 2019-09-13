unit amLocalization.Translator.TM;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  Generics.Collections,
  System.SysUtils, System.Classes, Data.DB,

  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client,

  amLocale,
  amLocalization.Model,
  amLocalization.Translator,
  amLocalization.Dialog.TranslationMemory.SelectDuplicate;


type
  TTranslationMemoryMergeStats = record
    Added: integer;             // New row added with value
    Merged: integer;            // Value merged into existing row
    Skipped: integer;           // Value skipped (e.g. empty value)
    Duplicate: integer;         // Exact duplicate found
  end;

  TTranslationMemoryDuplicateAction = (
    tmDupActionPrompt,          // Prompt to select action
    tmDupActionAbort,           // Abort import
    tmDupActionAcceptAll,       // Accept all duplicates
    tmDupActionRejectAll        // Reject all duplicates
  );

// -----------------------------------------------------------------------------
//
// TDataModuleTranslationMemory
//
// -----------------------------------------------------------------------------
type
  TDataModuleTranslationMemory = class(TDataModule, ITranslationService)
    DataSourceTranslationMemory: TDataSource;
    TableTranslationMemory: TFDMemTable;
    procedure TableTranslationMemoryAfterModify(DataSet: TDataSet);
  private
    FLoaded: boolean;
    FFormSelectDuplicate: TFormSelectDuplicate;
    FDuplicateAction: TDuplicateAction;
    FLookupIndex: TDictionary<string, TList<integer>>;
    FConflictResolution: TDictionary<string, string>;
    FModified: boolean;
    FCreateDate: TDateTime;
  private type
    TDuplicate = record
      SourceValue: string;
      Value: string;
      RecordID: int64;
    end;
    TDuplicateValues = class(TList<TDuplicate>)
    public
      EmptyCount: integer;
    end;

    TDuplicateTerms = TObjectDictionary<string, TDuplicateValues>;

    TDuplicates = TObjectDictionary<TField, TDuplicateTerms>;
  private
    function FindField(LocaleItem: TLocaleItem): TField;
    function GetHasData: boolean;
    procedure FieldGetTextEventHandler(Sender: TField; var Text: string; DisplayText: Boolean);
    function GetAvailable: boolean;
    function DoAdd(SourceField: TField; const SourceValue, SanitizedSourceValue: string; TargetField: TField; const TargetValue: string;
      Duplicates: TDuplicates; var Stats: TTranslationMemoryMergeStats; DuplicateAction: TTranslationMemoryDuplicateAction): TTranslationMemoryDuplicateAction;
  protected
    // ITranslationService
    function BeginLookup(SourceLanguage, TargetLanguage: TLocaleItem): boolean;
    procedure EndLookup;
    function Lookup(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLocaleItem; const SourceValue: string; var TargetValue: string): boolean;
    function GetServiceName: string;
  public
    function Add(SourceLanguage: Word; const SourceValue: string; TargetLanguage: Word; const TargetValue: string; var Stats: TTranslationMemoryMergeStats; DuplicateAction: TTranslationMemoryDuplicateAction = tmDupActionPrompt): TTranslationMemoryDuplicateAction; overload;
    function Add(SourceField: TField; const SourceValue: string; TargetField: TField; const TargetValue: string; var Stats: TTranslationMemoryMergeStats; DuplicateAction: TTranslationMemoryDuplicateAction = tmDupActionPrompt): TTranslationMemoryDuplicateAction; overload;

    function LoadTranslationMemory(const Filename: string; Merge: boolean = False): TTranslationMemoryMergeStats;
    procedure SaveTranslationMemory(const Filename: string);
    function CheckSave: boolean;
    function CheckLoaded(Force: boolean = False): boolean;

    property IsLoaded: boolean read FLoaded;
    property IsAvailable: boolean read GetAvailable;
    property HasData: boolean read GetHasData;
    property Modified: boolean read FModified;
  end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  UITypes,
  Dialogs,
  Controls,
  IOUtils,
  DateUtils,
  StrUtils,
  Variants,
  Windows,
  XMLDoc, XMLIntf,
  Forms,
  cxGraphics,
  amCursorService,
  amVersionInfo,
  amProgressForm,
  amLocalization.Settings,
  amLocalization.Utils;

type
  TTerm = record
    Field: TField;
    Value: string;
  end;

  TTerms = TList<TTerm>;

// -----------------------------------------------------------------------------
//
// TDataModuleTranslationMemory
//
// -----------------------------------------------------------------------------
function TDataModuleTranslationMemory.GetServiceName: string;
resourcestring
  sTranslatorNameTM = 'Translation Memory';
begin
  Result := sTranslatorNameTM;
end;

// -----------------------------------------------------------------------------

function TDataModuleTranslationMemory.CheckLoaded(Force: boolean): boolean;
var
  Res: integer;
resourcestring
  sLocalizerNoTMFileTitle = 'Translation Memory does not exist';
  sLocalizerNoTMFile = 'The Translation Memory file does not exist.'#13#13'Filename: %s'#13#13'A new file will be created when you save the Translation Memory.'#13#13'Do you want to save an new empty file now?';
begin
  if (not FLoaded) and ((Force) or (TranslationManagerSettings.Translators.TranslationMemory.LoadOnDemand)) then
  begin
    if (not TFile.Exists(TranslationManagerSettings.Translators.TranslationMemory.Filename)) then
    begin
      Res := TaskMessageDlg(sLocalizerNoTMFileTitle, Format(sLocalizerNoTMFile, [TranslationManagerSettings.Translators.TranslationMemory.Filename]),
        mtConfirmation, [mbYes, mbNo, mbCancel], 0, mbNo);

      if (Res = mrCancel) then
        Exit(False);

      if (Res = mrYes) then
      begin
        // Save empty
        SaveTranslationMemory(TranslationManagerSettings.Translators.TranslationMemory.Filename);
        // ...and load it
        LoadTranslationMemory(TranslationManagerSettings.Translators.TranslationMemory.Filename);
      end else
        // Pretend we have loaded to avoid further prompts
        FLoaded := True;
    end else
      LoadTranslationMemory(TranslationManagerSettings.Translators.TranslationMemory.Filename);
  end;

  Result := FLoaded;
end;

// -----------------------------------------------------------------------------

function TDataModuleTranslationMemory.CheckSave: boolean;
var
  Res: integer;
resourcestring
  sLocalizerSaveTMPromptTitle = 'Translation Memory has not been saved';
  sLocalizerSaveTMPrompt = 'Your changes to the Translation Memory has not been saved.'#13#13'Do you want to save them now?';
begin
  if (Modified) then
  begin
    Res := TaskMessageDlg(sLocalizerSaveTMPromptTitle, sLocalizerSaveTMPrompt,
      mtConfirmation, [mbYes, mbNo, mbCancel], 0, mbCancel);

    if (Res = mrCancel) then
      Exit(False);

    if (Res = mrYes) then
    begin
      SaveCursor(crHourGlass);

      SaveTranslationMemory(TranslationManagerSettings.Translators.TranslationMemory.Filename);

      Result := (not Modified);
    end else
      Result := True;
  end else
    Result := True;
end;

// -----------------------------------------------------------------------------

procedure TDataModuleTranslationMemory.FieldGetTextEventHandler(Sender: TField; var Text: string; DisplayText: Boolean);
begin
  Text := Sender.AsString;
end;

// -----------------------------------------------------------------------------

function TDataModuleTranslationMemory.FindField(LocaleItem: TLocaleItem): TField;
var
  i: integer;
begin
  for i := 0 to TableTranslationMemory.FieldCount-1 do
    if (SameText(LocaleItem.LocaleName, TableTranslationMemory.Fields[i].FieldName)) then
      Exit(TableTranslationMemory.Fields[i]);
  Result := nil;
end;

// -----------------------------------------------------------------------------

function TDataModuleTranslationMemory.GetAvailable: boolean;
begin
  Result := (FLoaded) or (TranslationManagerSettings.Translators.TranslationMemory.LoadOnDemand);
end;

function TDataModuleTranslationMemory.GetHasData: boolean;
begin
  Result := (TableTranslationMemory.Active) and (TableTranslationMemory.RecordCount > 0);
end;

// -----------------------------------------------------------------------------

function TDataModuleTranslationMemory.DoAdd(SourceField: TField; const SourceValue, SanitizedSourceValue: string;
  TargetField: TField; const TargetValue: string;
  Duplicates: TDuplicates; var Stats: TTranslationMemoryMergeStats; DuplicateAction: TTranslationMemoryDuplicateAction): TTranslationMemoryDuplicateAction;

  function Truncate(const Value: string): string;
  begin
    Result := cxGetStringAdjustedToWidth(0, 0, Value, 250, mstEndEllipsis);
    Result := Result.Replace(#13, ' ',  [rfReplaceAll]).Replace(#10, ' ',  [rfReplaceAll]);
  end;

var
  DuplicateTerms: TDuplicateTerms;
  DuplicateValues: TDuplicateValues;
  Duplicate: TDuplicate;
  AllEmpty: boolean;
  i: integer;
  s: string;
  Res: Word;
  DuplicateFound: boolean;
  DuplicateTermPair: TPair<TField, TDuplicateTerms>;
  SourceLanguage, TargetLanguage: TLocaleItem;
  SourceLanguageName, TargetLanguageName: string;
  DuplicateCount: integer;
resourcestring
  sTranslationMemoryAddDuplicateTitle = 'Duplicates found';
  sTranslationMemoryAddDuplicate = 'You are adding a term that already has %d translation(s) in the dictionary.'#13#13+
    'Do you want to add the translation anyway?'+#13#13+
    '%s: "%s"'+#13#13+
    '%s: %s';
begin
  Assert(SourceField <> TargetField);
  Assert(SourceField <> nil);
  Assert(TargetField <> nil);

  Assert(TableTranslationMemory.Active);

  Result := DuplicateAction;

  if (TargetValue.IsEmpty) then
  begin
    Inc(Stats.Skipped);
    Exit;
  end;

  DuplicateTerms := nil;
  if (not Duplicates.TryGetValue(TargetField, DuplicateTerms)) or (not DuplicateTerms.TryGetValue(SanitizedSourceValue, DuplicateValues)) then
    DuplicateValues := nil;

  AllEmpty := True;
  if (DuplicateValues <> nil) then
    for i := 0 to DuplicateValues.Count-1 do
    begin
      if (AnsiSameText(SourceValue, DuplicateValues[i].SourceValue)) and (AnsiSameText(TargetValue, DuplicateValues[i].Value)) then
      begin
        Inc(Stats.Duplicate);
        Exit;
      end;
      if (not DuplicateValues[i].Value.IsEmpty) then
        AllEmpty := False;
    end;

  // Do not prompt for duplicate with only empty target values - just merge it
  if (DuplicateValues <> nil) and (DuplicateValues.Count > 0) and (not AllEmpty) then
  begin
    if (DuplicateAction = tmDupActionPrompt) then
    begin
      s := '';
      DuplicateCount := 0;
      // Target value on single line if only one, otherwise bullet list
      for i := 0 to DuplicateValues.Count-1 do
      begin
        if (DuplicateValues[i].Value.IsEmpty) then
          continue;
        if (DuplicateCount = 1) then
          s := #13 + '- ' + s;
        if (DuplicateCount >= 1) then
          s := s + #13 + '- ';
        s := s + '"'+Truncate(DuplicateValues[i].Value)+'"';
        Inc(DuplicateCount);
      end;
      Assert(DuplicateCount > 0);

      SourceLanguage := TLocaleItems.FindLocaleName(SourceField.FieldName);
      if (SourceLanguage <> nil) then
        SourceLanguageName := SourceLanguage.LanguageName
      else
        SourceLanguageName := SourceField.FieldName;
      TargetLanguage := TLocaleItems.FindLocaleName(TargetField.FieldName);
      if (TargetLanguage <> nil) then
        TargetLanguageName := TargetLanguage.LanguageName
      else
        TargetLanguageName := TargetField.FieldName;

      Res := TaskMessageDlg(sTranslationMemoryAddDuplicateTitle,
        Format(sTranslationMemoryAddDuplicate, [DuplicateCount, SourceLanguageName, Truncate(SourceValue), TargetLanguageName, s]),
        mtConfirmation, [mbYes, mbNo, mbYesToAll, mbNoToAll, mbAbort], 0, mbNo);

      case Res of
        mrAbort:
          Result := tmDupActionAbort;

        mrNo:
          begin
            Inc(Stats.Skipped);
            Exit;
          end;

        mrNoToAll:
          Result := tmDupActionRejectAll;

        mrYesToAll:
          Result := tmDupActionAcceptAll;
      end;
    end;

    if (Result in [tmDupActionAbort, tmDupActionRejectAll]) then
    begin
      Inc(Stats.Skipped);
      Exit;
    end;
  end;

  if (DuplicateValues <> nil) and (DuplicateValues.EmptyCount > 0) then
  begin
    // Find first empty entry.
    // First look for exact source match
    DuplicateFound := False;
    for i := 0 to DuplicateValues.Count-1 do
    begin
      if (AnsiSameText(DuplicateValues[i].SourceValue, SourceValue)) and (DuplicateValues[i].Value.IsEmpty) then
      begin
        DuplicateFound := True;
        Duplicate := DuplicateValues[i];
        Duplicate.Value := TargetValue;
        DuplicateValues[i] := Duplicate;
        break;
      end;
    end;
    // Then just for an empty entry
    if (not DuplicateFound) then
      for i := 0 to DuplicateValues.Count-1 do
      begin
        if (DuplicateValues[i].Value.IsEmpty) then
        begin
          DuplicateFound := True;
          Duplicate := DuplicateValues[i];
          Duplicate.Value := TargetValue;
          DuplicateValues[i] := Duplicate;
          break;
        end;
      end;
    Assert(DuplicateFound);
    Dec(DuplicateValues.EmptyCount);

    // Edit existing entry
    TableTranslationMemory.RecNo := Duplicate.RecordID;
    TableTranslationMemory.Resync([rmExact]);
    TableTranslationMemory.Edit;
    try
      // Source field already has the desired value
      // SourceField.AsString := SourceValue;

      TargetField.AsString := TargetValue;

      TableTranslationMemory.Post;

    except
      TableTranslationMemory.Cancel;
      raise;
    end;
    Inc(Stats.Merged);
  end else
  begin
    // Add new entry
    TableTranslationMemory.Append;
    try
      SourceField.AsString := SourceValue;
      TargetField.AsString := TargetValue;

      TableTranslationMemory.Post;

    except
      TableTranslationMemory.Cancel;
      raise;
    end;
    Inc(Stats.Added);

    // Add new to duplicate list and add empty to all other term lists
    Duplicate.SourceValue := SourceValue;
    Duplicate.RecordID := TableTranslationMemory.RecNo;
    for DuplicateTermPair in Duplicates do
    begin
      if (not DuplicateTermPair.Value.TryGetValue(SanitizedSourceValue, DuplicateValues)) then
      begin
        DuplicateValues := TDuplicateValues.Create;
        DuplicateTermPair.Value.Add(SanitizedSourceValue, DuplicateValues);
      end;

      if (DuplicateTermPair.Key = TargetField) then
        Duplicate.Value := TargetValue
      else
      begin
        Duplicate.Value := '';
        Inc(DuplicateValues.EmptyCount);
      end;

      DuplicateValues.Add(Duplicate);
    end;
  end;

  FModified := True;
end;

// -----------------------------------------------------------------------------

function TDataModuleTranslationMemory.Add(SourceField: TField; const SourceValue: string; TargetField: TField; const TargetValue: string; var Stats: TTranslationMemoryMergeStats; DuplicateAction: TTranslationMemoryDuplicateAction = tmDupActionPrompt): TTranslationMemoryDuplicateAction;
var
  Duplicates: TDuplicates;
  DuplicateTerms: TDuplicateTerms;
  DuplicateValues: TDuplicateValues;
  Duplicate: TDuplicate;
  SanitizedSourceValue: string;
begin
  Stats := Default(TTranslationMemoryMergeStats);
  Result := DuplicateAction;

  if (SourceField = TargetField) then
    Exit;

  if (not CheckLoaded) then
    Exit;

  TableTranslationMemory.DisableControls;
  try
    if (not TableTranslationMemory.Active) then
      TableTranslationMemory.Open;

    SanitizedSourceValue := SanitizeText(SourceValue, False);

    DuplicateTerms := nil;
    DuplicateValues := nil;
    Duplicates := TDuplicates.Create([doOwnsValues]);
    try
      Duplicate.SourceValue := SourceValue;
      TableTranslationMemory.First;
      while (not TableTranslationMemory.EOF) do
      begin
        if (SourceField.IsNull) or (not AnsiSameText(SanitizedSourceValue, SanitizeText(SourceField.AsString, False))) then
        begin
          TableTranslationMemory.Next;
          continue;
        end;

        if (not TargetField.IsNull) and (AnsiSameText(SourceValue, SourceField.AsString)) and (AnsiSameText(TargetValue, TargetField.AsString)) then
        begin
          Inc(Stats.Duplicate);
          Exit; // Exact duplicate - do nothing
        end;

        // Source same, target differs
        if (DuplicateTerms = nil) then
        begin
          DuplicateTerms := TDuplicateTerms.Create([doOwnsValues], TTextComparer.Create);
          Duplicates.Add(TargetField, DuplicateTerms);

          DuplicateValues := TDuplicateValues.Create;
          DuplicateTerms.Add(SanitizedSourceValue, DuplicateValues);
        end;

        Duplicate.Value := TargetField.AsString;
        Duplicate.RecordID := TableTranslationMemory.RecNo;

        DuplicateValues.Add(Duplicate);

        if (Duplicate.Value.IsEmpty) then
          Inc(DuplicateValues.EmptyCount);

        TableTranslationMemory.Next;
      end;

      Result := DoAdd(SourceField, SourceValue, SanitizedSourceValue, TargetField, TargetValue, Duplicates, Stats, DuplicateAction);

    finally
      Duplicates.Free;
    end;

  finally
    TableTranslationMemory.EnableControls;
  end;
end;

function TDataModuleTranslationMemory.Add(SourceLanguage: Word; const SourceValue: string; TargetLanguage: Word; const TargetValue: string; var Stats: TTranslationMemoryMergeStats; DuplicateAction: TTranslationMemoryDuplicateAction = tmDupActionPrompt): TTranslationMemoryDuplicateAction;

  function AddField(LocaleItem: TLocaleItem): TField;
  begin
    Result := TWideMemoField.Create(TableTranslationMemory);

    Result.FieldName := LocaleItem.LocaleSName;
    Result.DisplayLabel := LocaleItem.LanguageName;
    Result.Tag := LocaleItem.Locale;
    Result.OnGetText := FieldGetTextEventHandler; // Otherwise memo is edited as "(WIDEMEMO)"

    Result.DataSet := TableTranslationMemory;
    Result.DisplayWidth := 100;
  end;

var
  SourceLocaleItem, TargetLocaleItem: TLocaleItem;
  SourceField: TField;
  TargetField: TField;
  Clone: TFDMemTable;
begin
  Stats := Default(TTranslationMemoryMergeStats);
  Result := DuplicateAction;

  if (SourceLanguage = TargetLanguage) then
    Exit;

  if (not CheckLoaded) then
    Exit;

  SourceLocaleItem := TLocaleItems.FindLCID(SourceLanguage);
  TargetLocaleItem := TLocaleItems.FindLCID(TargetLanguage);
  Assert(SourceLocaleItem <> nil);
  Assert(TargetLocaleItem <> nil);

  SourceField := FindField(SourceLocaleItem);
  TargetField := FindField(TargetLocaleItem);

  TableTranslationMemory.DisableControls;
  try
    // If either the source- or target language doesn't exist in the dataset then we
    // will need to add them.
    // We save a copy of the dataset, close the original dataset, add the field(s) and
    // the restore the dataset from the copy.
    if (SourceField = nil) or (TargetField = nil) then
    begin
      Clone := TFDMemTable.Create(nil);
      try
        if (TableTranslationMemory.Fields.Count > 0) then
          Clone.CopyDataSet(TableTranslationMemory, [coStructure, coRestart, coAppend]);

        TableTranslationMemory.Close;

        if (SourceField = nil) then
          SourceField := AddField(SourceLocaleItem);
        if (TargetField = nil) then
          TargetField := AddField(TargetLocaleItem);

        TableTranslationMemory.Open;

        if (Clone.Fields.Count > 0) then
          TableTranslationMemory.CopyDataSet(Clone, [coAppend]);
      finally
        Clone.Free;
      end;
    end;

    Result := Add(SourceField, SourceValue, TargetField, TargetValue, Stats, DuplicateAction);

  finally
    TableTranslationMemory.EnableControls;
  end;
end;

// -----------------------------------------------------------------------------

function TDataModuleTranslationMemory.LoadTranslationMemory(const Filename: string; Merge: boolean): TTranslationMemoryMergeStats;
var
  Duplicates: TDuplicates;
  DuplicateTermsList: array of TDuplicateTerms;
  DuplicateTerms: TDuplicateTerms;
  DuplicateValues: TDuplicateValues;
  Duplicate: TDuplicate;
  XML: IXMLDocument;
  Body: IXMLNode;
  Node, ItemNode: IXMLNode;
  LanguageNode: IXMLNode;
  Language: string;
  LanguageName: string;
  Languages: TDictionary<string, TField>;
  Clone: TFDMemTable;
  Translations: TObjectList<TTerms>;
  Term: TTerm;
  Terms: TTerms;
  LocaleItem: TLocaleItem;
  i, j: integer;
  Field: TField;
  s: string;
  SourceValue, SanitizedSourceValue: string;
  SourceIndex: integer;
  SourceField: TField;
  FieldMap: array of integer;
  DuplicateAction: TTranslationMemoryDuplicateAction;
  Progress: IProgress;
resourcestring
  sLoading = 'Loading Translation Memory';
  sReadingTerms = 'Reading terms...';
  sIndexingTerms = 'Indexing terms...';
  sAddingTerms = 'Adding terms...';
  sLoadingTerms = 'Loading terms...';
begin
  Result := Default(TTranslationMemoryMergeStats);

  XML := TXMLDocument.Create(nil);
  XML.Active := True;

  XML.LoadFromFile(Filename);

  if (XML.DocumentElement.NodeName <> 'tmx') then
    raise Exception.CreateFmt('XML document root node is not named "tmx": %s', [XML.DocumentElement.NodeName]);

  Node := XML.DocumentElement.ChildNodes.FindNode('header');
  if (Node <> nil) and (not Merge) then
  begin
    s := VarToStr(Node.Attributes['creationdate']);
    if (s.IsEmpty) or (not TryISO8601ToDate(s, FCreateDate, True)) then
      FCreateDate := Now;
  end;

  Body := XML.DocumentElement.ChildNodes.FindNode('body');
  if (Body = nil) then
    raise Exception.Create('xliff node not found: tmx\body');

  Progress := ShowProgress(sLoading);
  Progress.Progress(psBegin, 0, Body.ChildNodes.Count, sReadingTerms);

  TableTranslationMemory.DisableControls;
  try
    Translations := TObjectList<TTerms>.Create;
    try

      if (Merge) then
        Clone := TFDMemTable.Create(nil)
      else
        Clone := nil;
      try
        if (Clone <> nil) and (TableTranslationMemory.Fields.Count > 0) then
          Clone.CopyDataSet(TableTranslationMemory, [coStructure, coRestart, coAppend]);

        FLoaded := False;
        TableTranslationMemory.Close;

        Languages := TDictionary<string, TField>.Create(TTextComparer.Create);
        try

          if (Merge) then
          begin
            for i := 0 to TableTranslationMemory.Fields.Count-1 do
              Languages.Add(TableTranslationMemory.Fields[i].FieldName, TableTranslationMemory.Fields[i]);
          end else
            TableTranslationMemory.Fields.Clear;

          ItemNode := Body.ChildNodes.First;
          while (ItemNode <> nil) do
          begin
            Progress.AdvanceProgress;

            if (ItemNode.NodeName = 'tu') then
            begin
              Terms := TTerms.Create;
              Translations.Add(Terms);

              LanguageNode := ItemNode.ChildNodes.First;
              while (LanguageNode <> nil) do
              begin
                if (LanguageNode.NodeName = 'tuv') then
                begin
                  Language := LanguageNode.Attributes['xml:lang'];

                  LocaleItem := TLocaleItems.FindLocaleName(Language);
                  if (LocaleItem <> nil) then
                    LanguageName := LocaleItem.LocaleSName
                  else
                    LanguageName := Language;

                  if (not Languages.TryGetValue(LanguageName, Field)) then
                  begin
                    Field := TWideMemoField.Create(TableTranslationMemory);

                    if (LocaleItem <> nil) then
                    begin
                      Field.DisplayLabel := LocaleItem.LanguageName;
                      Field.Tag := LocaleItem.Locale;
                    end else
                      ShowMessageFmt('Unknown language: %s', [Language]);

                    Field.FieldName := LanguageName;
                    Field.DataSet := TableTranslationMemory;
                    Field.DisplayWidth := 100;
                    Field.OnGetText := FieldGetTextEventHandler; // Otherwise memo is edited as "(WIDEMEMO)"

                    Languages.Add(Language, Field);
                  end;

                  Term.Field := Field;
                  Term.Value := VarToStr(LanguageNode.ChildValues['seg']);

                  Terms.Add(Term);
                end;

                LanguageNode := LanguageNode.NextSibling;
              end;
            end;

            ItemNode := ItemNode.NextSibling;
          end;

          Progress.Progress(psEnd, 1, 1);

        finally
          Languages.Free;
        end;

        // Now all fields has been created. Reload the old data..
        if (Merge) then
        begin
          TableTranslationMemory.Open;
          if (Clone.Fields.Count > 0) then
            TableTranslationMemory.CopyDataSet(Clone, [coAppend]);
        end;

      finally
        Clone.Free;
      end;

      if (not TableTranslationMemory.Active) then
        TableTranslationMemory.Open;

      (*
      ** Create index of duplicates per languag
      *)
      Duplicates := TDuplicates.Create([doOwnsValues]);
      try
        if (Merge) then
        begin
          Progress.Progress(psBegin, 0, TableTranslationMemory.RecordCount, sIndexingTerms);

          // Create one term list per language.
          // Assume language[0] is the source language.
          // A term list holds the target language terms that correspond to a given source language term.
          SetLength(DuplicateTermsList, TableTranslationMemory.Fields.Count);

          DuplicateTermsList[0] := nil;
          for i := 1 to TableTranslationMemory.Fields.Count-1 do
          begin
            DuplicateTerms := TDuplicateTerms.Create([doOwnsValues], TTextComparer.Create);
            DuplicateTermsList[i] := DuplicateTerms;
            Duplicates.Add(TableTranslationMemory.Fields[i], DuplicateTerms);
          end;

          Field := TableTranslationMemory.Fields[0];

          TableTranslationMemory.First;
          while (not TableTranslationMemory.EOF) do
          begin
            Progress.AdvanceProgress;

            // For each source language term...
            SourceValue := Field.AsString;
            SanitizedSourceValue := SanitizeText(SourceValue, False);

            // ..., For each target language...
            for i := 1 to TableTranslationMemory.Fields.Count-1 do
            begin
              // ... save the target term in the term list of the source language term
              if (not DuplicateTermsList[i].TryGetValue(SanitizedSourceValue, DuplicateValues)) then
              begin
                DuplicateValues := TDuplicateValues.Create;
                DuplicateTermsList[i].Add(SanitizedSourceValue, DuplicateValues);
              end;

              Duplicate.SourceValue := SourceValue;
              Duplicate.Value := TableTranslationMemory.Fields[i].AsString;
              Duplicate.RecordID := TableTranslationMemory.RecNo;

              DuplicateValues.Add(Duplicate);

              if (Duplicate.Value.IsEmpty) then
                Inc(DuplicateValues.EmptyCount);
            end;

            TableTranslationMemory.Next;
          end;
          Progress.Progress(psEnd, 1, 1);
        end;


        if (Merge) then
        begin
          // Post all terms to the dataset as individual source/target values
          FModified := True;

          Progress.Progress(psBegin, 0, Translations.Count, sAddingTerms);

          SourceField := TableTranslationMemory.Fields[0];
          DuplicateAction := tmDupActionPrompt;

          for i := 0 to Translations.Count-1 do
          begin
            Progress.AdvanceProgress;

            // Find source value
            SourceIndex := -1;
            for j := 0 to Translations[i].Count-1 do
              if (Translations[i][j].Field = SourceField) then
              begin
                SourceIndex := j;
                break;
              end;

            // Ignore if no source value
            if (SourceIndex = -1) then
              continue;

            SourceValue := Translations[i][SourceIndex].Value;
            SanitizedSourceValue := SanitizeText(SourceValue, False);

            for j := 0 to Translations[i].Count-1 do
            begin
              Field := Translations[i][j].Field;

              if (Field = SourceField) then
                continue;

              DuplicateAction := DoAdd(SourceField, SourceValue, SanitizedSourceValue, Field, Translations[i][j].Value, Duplicates, Result, DuplicateAction);

              if (DuplicateAction = tmDupActionAbort) then
                break;
            end;
            if (DuplicateAction = tmDupActionAbort) then
              break;
          end;
          Progress.Progress(psEnd, 1, 1);
        end else
        begin
          Progress.Progress(psBegin, 0, Translations.Count, sLoadingTerms);

          // Post all terms to the dataset, one row at a time
          for i := 0 to Translations.Count-1 do
          begin
            Progress.AdvanceProgress;

            TableTranslationMemory.Append;
            try

              for j := 0 to Translations[i].Count-1 do
                Translations[i][j].Field.AsString := Translations[i][j].Value;

              TableTranslationMemory.Post;
            except
              TableTranslationMemory.Cancel;
              raise;
            end;
          end;
          Progress.Progress(psEnd, 1, 1);
          FModified := False;
        end;
      finally
        Duplicates.Free;
      end;

    finally
      Translations.Free;
    end;
  finally
    TableTranslationMemory.EnableControls;
  end;

  FLoaded := True;
end;

// -----------------------------------------------------------------------------

procedure TDataModuleTranslationMemory.SaveTranslationMemory(const Filename: string);
var
  XML: IXMLDocument;
  Node, Body: IXMLNode;
  ItemNode: IXMLNode;
  LanguageNode: IXMLNode;
  i: integer;
  TempFilename, BackupFilename: string;
begin
  XML := TXMLDocument.Create(nil);
  XML.Active := True;

  XML.Options := [doNodeAutoIndent];
  XML.Active := True;

  XML.AddChild('tmx');
  XML.DocumentElement.Attributes['version'] := '1.4';

  Node := XML.DocumentElement.AddChild('header');
  Node.Attributes['creationtool'] := TPath.GetFileNameWithoutExtension(ParamStr(0));
  Node.Attributes['creationtoolversion'] := TVersionInfo.FileVersionString(ParamStr(0));
  Node.Attributes['datatype'] := 'plaintext';
  Node.Attributes['segtype'] := 'sentence';
  Node.Attributes['adminlang'] := 'en-us';
  Node.Attributes['srclang'] := '*all*'; // This is tecnically incorrect since we're treating the first language as the source language
  Node.Attributes['creationdate'] := DateToISO8601(FCreateDate, True);
  Node.Attributes['changedate'] := DateToISO8601(Now, False);

  Body := XML.DocumentElement.AddChild('body');

  if (HasData) then
  begin
    TableTranslationMemory.First;

    while (not TableTranslationMemory.EOF) do
    begin
      ItemNode := Body.AddChild('tu');

      for i := 0 to TableTranslationMemory.FieldCount-1 do
      begin
        if (not TableTranslationMemory.Fields[i].IsNull) and (not TableTranslationMemory.Fields[i].AsString.IsEmpty) then
        begin
          LanguageNode := ItemNode.AddChild('tuv');
          LanguageNode.Attributes['xml:lang'] := TableTranslationMemory.Fields[i].FieldName;
          LanguageNode.AddChild('seg').Text := TableTranslationMemory.Fields[i].AsString;
        end;
      end;

      TableTranslationMemory.Next;
    end;
  end;

  TempFilename := Filename;

  // Save to temporary file if destination file already exist
  if (TFile.Exists(Filename)) then
  begin
    i := 0;
    repeat
      TempFilename := Format('%s\savefile%.4X%s', [TPath.GetDirectoryName(Filename), i, '.tmx']);
      Inc(i);
    until (not TFile.Exists(TempFilename));
  end;

  // Save file
  XML.SaveToFile(TempFilename);

  // Save existing file as backup
  if (TempFilename <> Filename) then
  begin
    i := 0;
    repeat
      BackupFilename := Format('%s.$%.4X', [Filename, i]);
      Inc(i);
    until (not TFile.Exists(BackupFilename));

    TFile.Move(Filename, BackupFilename);

    // Rename temporary file to final file
    TFile.Move(TempFilename, Filename);
  end;

  FModified := False;
end;

// -----------------------------------------------------------------------------

procedure TDataModuleTranslationMemory.TableTranslationMemoryAfterModify(DataSet: TDataSet);
begin
  FModified := True;
end;

// -----------------------------------------------------------------------------

function TDataModuleTranslationMemory.Lookup(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLocaleItem; const SourceValue: string; var TargetValue: string): boolean;
var
  SourceField: TField;
  TargetField: TField;
  Duplicates: TStringList;
  RecordIndex: integer;
  List: TList<integer>;
  i: integer;
begin
  Result := False;
  TargetValue := '';

  if (SourceLanguage = TargetLanguage) then
    Exit;

  if (not HasData) then
    Exit;

  SourceField := FindField(SourceLanguage);
  TargetField := FindField(TargetLanguage);

  if (SourceField = nil) or (TargetField = nil) then
    // One or both languages doesn't exist in TM
    Exit;

  Duplicates := nil;
  try

    if (FLookupIndex.TryGetValue(SanitizeText(SourceValue, False), List)) then
    begin
      Result := True;

      if (List.Count > 1) then
        Duplicates := TStringList.Create;

      for RecordIndex in List do
      begin
        TableTranslationMemory.RecNo := RecordIndex+1;

        TargetValue := TargetField.AsString;

        if (Duplicates <> nil) then
        begin
          // Ignore exact duplicates
          for i := 0 to Duplicates.Count-1 do
            if (Duplicates[i] = TargetValue) then
            begin
              TargetValue := '';
              break;
            end;
          if (TargetValue <> '') then
            Duplicates.Add(TargetValue);
        end;
      end;
    end else
      Exit(False);

    if (Duplicates <> nil) and (Duplicates.Count > 0) then
    begin
      if (Duplicates.Count = 1) then
      begin
        TargetValue := Duplicates[0];
        Exit;
      end;

      // Attempt to resolve using previously resolved conflicts
      if (FConflictResolution.TryGetValue(Prop.Value, TargetValue)) then
        Exit(True);

      if (FFormSelectDuplicate = nil) then
        FFormSelectDuplicate := TFormSelectDuplicate.Create(nil);

      FFormSelectDuplicate.DuplicateAction := FDuplicateAction;

      if (not FFormSelectDuplicate.SelectDuplicate(Prop, Duplicates, TargetValue)) then
        Abort;

      FDuplicateAction := FFormSelectDuplicate.DuplicateAction;

      if (FFormSelectDuplicate.ApplyToIdentical) then
        FConflictResolution.Add(Prop.Value, TargetValue);

      Result := (not (FDuplicateAction in [daSkip, daSkipAll]));
    end;

  finally
    Duplicates.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TDataModuleTranslationMemory.BeginLookup(SourceLanguage, TargetLanguage: TLocaleItem): boolean;
var
  SourceField: TField;
  TargetField: TField;
  SourceValue: string;
  RecordIndex: integer;
  List: TList<integer>;
begin
  FDuplicateAction := daPrompt;
  FLookupIndex := TObjectDictionary<string, TList<integer>>.Create([doOwnsValues], TTextComparer.Create);
  FConflictResolution := TDictionary<string, string>.Create;

  if (not CheckLoaded) then
    Exit(False);

  SourceField := FindField(SourceLanguage);
  TargetField := FindField(TargetLanguage);

  // Do nothing if there is no data but pretend everything is OK so the user gets normal feedback
  if (HasData) then
  begin
    if (SourceField = nil) or (TargetField = nil) then
      // One or both languages doesn't exist in TM
      Exit(False);

    // Create dictionary of source terms
    TableTranslationMemory.First;
    RecordIndex := 0;
    while (not TableTranslationMemory.EOF) do
    begin
      if (not TargetField.IsNull) and (not SourceField.IsNull) then
      begin
        SourceValue := AnsiUppercase(SanitizeText(SourceField.AsString, False));

        if (not FLookupIndex.TryGetValue(SourceValue, List)) then
        begin
          List := TList<integer>.Create;
          FLookupIndex.Add(SourceValue, List);
        end;
        List.Add(RecordIndex);
      end;

      Inc(RecordIndex);

      TableTranslationMemory.Next;
    end;
  end;

  Result := True;
end;

procedure TDataModuleTranslationMemory.EndLookup;
begin
  FreeAndNil(FFormSelectDuplicate);
  FreeAndNil(FLookupIndex);
  FreeAndNil(FConflictResolution);
end;

// -----------------------------------------------------------------------------


end.
