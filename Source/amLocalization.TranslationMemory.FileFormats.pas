unit amLocalization.TranslationMemory.FileFormats;

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
  Classes,
  DB,
  amProgress,
  amLocalization.Model,
  amLocalization.Translator.TM;

// -----------------------------------------------------------------------------
//
// TTranslationMemoryFileFormat
//
// -----------------------------------------------------------------------------
type
  TFileFormatCapability = (ffcLoad, ffcSave);
  TFileFormatCapabilities = set of TFileFormatCapability;

  TTranslationMemoryFileFormat = class;
  TTranslationMemoryFileFormatClass = class of TTranslationMemoryFileFormat;

  TTranslationMemoryFileFormat = class abstract
  private
    class var FFileFormatRegistry: TList<TTranslationMemoryFileFormatClass>;
  private
    FTranslationMemory: ITranslationMemory;
    FTableTranslationMemory: TDataSet;
    FFileCreateDate: TDateTime;
  protected type
    TTerm = record
      Field: TField;
      Value: string;
    end;
    TTerms = TList<TTerm>;
    TTranslations = TObjectList<TTerms>;
    TLanguageFields = TDictionary<string, TField>;
  protected
    function DoLoadFromStream(Stream: TStream; const Progress: IProgress; DetailedProgress: boolean; Translations: TTranslations;
      Languages: TLanguageFields; Merge: boolean; var SourceLanguage: string): boolean; virtual; abstract;

    procedure CreateTermIndex(Duplicates: TDuplicates; SourceField: TField; const Progress: IProgress; DetailedProgress: boolean);

    property TranslationMemory: ITranslationMemory read FTranslationMemory;
    property TableTranslationMemory: TDataSet read FTableTranslationMemory;

  protected
    class procedure RegisterFileFormat(FileFormatClass: TTranslationMemoryFileFormatClass);
  public
    constructor Create(const ATranslationMemory: ITranslationMemory); virtual;
    class destructor Destroy;

    function LoadFromStream(Stream: TStream; Merge: boolean = False): TTranslationMemoryMergeStats; overload;
    function LoadFromStream(Stream: TStream; var DuplicateAction: TTranslationMemoryDuplicateAction; Merge: boolean = False; const Progress: IProgress = nil): TTranslationMemoryMergeStats; overload;

    function Prepare(const Filename: string): boolean; virtual;

    function LoadFromFile(const Filename: string; Merge: boolean = False): TTranslationMemoryMergeStats; overload;
    function LoadFromFile(const Filename: string; var DuplicateAction: TTranslationMemoryDuplicateAction; Merge: boolean = False; const Progress: IProgress = nil): TTranslationMemoryMergeStats; overload; virtual;

    procedure SaveToStream(Stream: TStream); virtual; abstract;
    procedure SaveToFile(const Filename: string);

    property FileCreateDate: TDateTime read FFileCreateDate write FFileCreateDate;

    class function FileFormatFileDescription: string; virtual;
    class function FileFormatFileType: string; virtual; abstract;
    class function FileFormatFileFilter: string; virtual;
    class function FileFormatCapabilities: TFileFormatCapabilities; virtual; abstract;

    class function FileFormatFileFilters(Capability: TFileFormatCapability; IncludeAllFilter: boolean = True): string;
    class function FindFileFormat(const Filename: string; Capability: TFileFormatCapability; Default: TTranslationMemoryFileFormatClass = nil): TTranslationMemoryFileFormatClass;
  end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  Dialogs, // temporary
  UITypes,
  IOUtils,
  DateUtils,
  Variants,
  SysUtils,
  msxmldom,
  XMLDoc, XMLIntf,
  FireDAC.Comp.Client,
  FireDAC.Comp.DataSet,
  amCursorService,
  amLocale,
  amVersionInfo,
  amFileUtils,
  amLocalization.Settings,
  amLocalization.Utils;

// -----------------------------------------------------------------------------
//
// TTranslationMemoryFileFormat
//
// -----------------------------------------------------------------------------
constructor TTranslationMemoryFileFormat.Create(const ATranslationMemory: ITranslationMemory);
begin
  inherited Create;

  FTranslationMemory := ATranslationMemory;
  FTableTranslationMemory := FTranslationMemory.TableTranslationMemory; // Cache for performance
  FFileCreateDate := Now;
end;

class destructor TTranslationMemoryFileFormat.Destroy;
begin
  FreeAndNil(FFileFormatRegistry);
end;

// -----------------------------------------------------------------------------

class function TTranslationMemoryFileFormat.FileFormatFileDescription: string;
begin
  Result := FileFormatFileType;
end;


class function TTranslationMemoryFileFormat.FileFormatFileFilter: string;
resourcestring
  sFileFilterGeneric = '%0:s files (*.%1:s)|*.%1:s';
begin
  Result := Format(sFileFilterGeneric, [FileFormatFileDescription, FileFormatFileType]);
end;

class function TTranslationMemoryFileFormat.FileFormatFileFilters(Capability: TFileFormatCapability; IncludeAllFilter: boolean): string;
var
  FileFormatClass: TTranslationMemoryFileFormatClass;
  AllFilter: string;
resourcestring
  sFileFilterAll = 'All supported files (%0:s)|%0:s';
begin
  Result := '';
  if (FFileFormatRegistry = nil) then
    Exit;

  AllFilter := '';

  for FileFormatClass in FFileFormatRegistry do
    if (Capability in FileFormatClass.FileFormatCapabilities) then
    begin
      if (Result <> '') then
      begin
        Result := Result + '|';
        if (IncludeAllFilter) then
          AllFilter := AllFilter + ';';
      end;

      Result := Result + FileFormatClass.FileFormatFileFilter;
      if (IncludeAllFilter) then
        AllFilter := AllFilter + '*.'+FileFormatClass.FileFormatFileType;
    end;

  if (IncludeAllFilter) then
    Result := Format(sFileFilterAll, [AllFilter]) + '|' + Result;
end;

class function TTranslationMemoryFileFormat.FindFileFormat(const Filename: string; Capability: TFileFormatCapability; Default: TTranslationMemoryFileFormatClass): TTranslationMemoryFileFormatClass;
var
  FileFormatClass: TTranslationMemoryFileFormatClass;
  FileType: string;
begin
  if (FFileFormatRegistry = nil) then
    Exit(nil);

  FileType := Copy(TPath.GetExtension(Filename), 2, MaxInt);

  for FileFormatClass in FFileFormatRegistry do
    if (Capability in FileFormatClass.FileFormatCapabilities) and (AnsiSameText(FileType, FileFormatClass.FileFormatFileType)) then
      Exit(FileFormatClass);

  Result := Default;
end;

// -----------------------------------------------------------------------------

class procedure TTranslationMemoryFileFormat.RegisterFileFormat(FileFormatClass: TTranslationMemoryFileFormatClass);
begin
  if (FFileFormatRegistry = nil) then
    FFileFormatRegistry := TList<TTranslationMemoryFileFormatClass>.Create;

  FFileFormatRegistry.Add(FileFormatClass);
end;

// -----------------------------------------------------------------------------

function TTranslationMemoryFileFormat.LoadFromFile(const Filename: string; Merge: boolean): TTranslationMemoryMergeStats;
var
  DuplicateAction: TTranslationMemoryDuplicateAction;
begin
  DuplicateAction := tmDupActionPrompt;
  Result := LoadFromFile(Filename, DuplicateAction, Merge);
end;

function TTranslationMemoryFileFormat.LoadFromFile(const Filename: string; var DuplicateAction: TTranslationMemoryDuplicateAction; Merge: boolean; const Progress: IProgress): TTranslationMemoryMergeStats;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(Stream, DuplicateAction, Merge, Progress);
  finally
    Stream.Free;
  end;
end;

function TTranslationMemoryFileFormat.LoadFromStream(Stream: TStream; Merge: boolean): TTranslationMemoryMergeStats;
var
  DuplicateAction: TTranslationMemoryDuplicateAction;
begin
  DuplicateAction := tmDupActionPrompt;
  Result := LoadFromStream(Stream, DuplicateAction, Merge);
end;

procedure TTranslationMemoryFileFormat.SaveToFile(const Filename: string);
begin
  SafeReplaceFile(Filename,
    function(const Filename: string): boolean
    var
      Stream: TStream;
    begin
      Stream := TFileStream.Create(Filename, fmCreate);
      try
        SaveToStream(Stream);
      finally
        Stream.Free;
      end;
      Result := True;
    end, TranslationManagerSettings.Backup.SaveBackups);
end;

// -----------------------------------------------------------------------------

procedure TTranslationMemoryFileFormat.CreateTermIndex(Duplicates: TDuplicates; SourceField: TField; const Progress: IProgress; DetailedProgress: boolean);
var
  DuplicateTermsList: array of TDuplicateTerms;
  DuplicateTerms: TDuplicateTerms;
  DuplicateValues: TDuplicateValues;
  Duplicate: TDuplicate;
  i: integer;
  SourceValue, SanitizedSourceValue: string;
begin
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

  TableTranslationMemory.First;
  while (not TableTranslationMemory.EOF) do
  begin
    if (DetailedProgress) then
      Progress.AdvanceProgress
    else
      Progress.ProcessMessages;

    // For each source language term...
    SourceValue := SourceField.AsString;
    SanitizedSourceValue := SanitizeText(SourceValue, False);

    // ..., For each target language...
    for i := 0 to TableTranslationMemory.Fields.Count-1 do
    begin
      if (TableTranslationMemory.Fields[i] = SourceField) then
        continue;

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
end;

// -----------------------------------------------------------------------------

function TTranslationMemoryFileFormat.LoadFromStream(Stream: TStream; var DuplicateAction: TTranslationMemoryDuplicateAction;
  Merge: boolean; const Progress: IProgress): TTranslationMemoryMergeStats;
var
  Duplicates: TDuplicates;
  Duplicate: TDuplicate;
  CreateDate: TDateTime;
  Language: string;
  LanguageName: string;
  Languages: TLanguageFields;
  Clone: IInterface;
  Translations: TTranslations;
  Term: TTerm;
  Terms: TTerms;
  LocaleItem: TLocaleItem;
  i, j: integer;
  Field: TField;
  s: string;
  SourceValue, SanitizedSourceValue: string;
  SourceIndex: integer;
  SourceLanguage, SourceLanguageTU: string;
  SourceField, SourceFieldTU: TField;
  LocalProgress: IProgress;
  DetailedProgress: boolean;
begin
  Result := Default(TTranslationMemoryMergeStats);

  DetailedProgress := (Progress = nil);
  if (DetailedProgress) then
    LocalProgress := ShowProgress(sTranslationMemoryLoading)
  else
    LocalProgress := Progress;

  if (not DetailedProgress) then
    LocalProgress.UpdateMessage(sTranslationMemoryLoading);

  TableTranslationMemory.DisableControls;
  try
    TranslationMemory.Lock;
    try
      Translations := TTranslations.Create;
      try

        if (Merge) then
          Clone := TranslationMemory.SaveTableTranslationMemoryClone
        else
          Clone := nil;
        try

          TableTranslationMemory.Close;

          Languages := TLanguageFields.Create(TTextComparer.Create);
          try

            if (Merge) then
            begin
              for i := 0 to TableTranslationMemory.Fields.Count-1 do
                Languages.Add(TableTranslationMemory.Fields[i].FieldName, TableTranslationMemory.Fields[i]);
            end else
              TableTranslationMemory.Fields.Clear;

            (*
            ** Call derived class to read terms from import file
            *)
            SourceLanguage := '';
            if (not DoLoadFromStream(Stream, LocalProgress, DetailedProgress, Translations, Languages, Merge, SourceLanguage)) then
              Exit;

            if (DetailedProgress) then
              LocalProgress.Progress(psEnd, 1, 1);

          finally
            Languages.Free;
          end;

        finally
          // Now all fields has been created. Reload the old data..
          Clone := nil;
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
            if (DetailedProgress) then
              LocalProgress.Progress(psBegin, 0, TableTranslationMemory.RecordCount, sTranslationMemoryIndexingTerms)
            else
              LocalProgress.UpdateMessage(sTranslationMemoryIndexingTerms);

            // Find the source field corresponding to the source language specified in the header
            if (SourceLanguage <> '') then
              SourceField := TableTranslationMemory.Fields.FindField(SourceLanguage)
            else
              SourceField := nil;

            if (SourceField <> nil) then
              Field := SourceField
            else
              Field := TableTranslationMemory.Fields[0];

            // Note: Duplicate detection doesn't work with per-tu source languages
            CreateTermIndex(Duplicates, Field, LocalProgress, DetailedProgress);

            if (DetailedProgress) then
              LocalProgress.Progress(psEnd, 1, 1);

            // Post all terms to the dataset as individual source/target values
            if (DetailedProgress) then
              LocalProgress.Progress(psBegin, 0, Translations.Count, sTranslationMemoryAddingTerms)
            else
              LocalProgress.UpdateMessage(sTranslationMemoryAddingTerms);

            for i := 0 to Translations.Count-1 do
            begin
              if (DetailedProgress) then
                LocalProgress.AdvanceProgress
              else
                LocalProgress.ProcessMessages;

              if (SourceField <> nil) then
              begin
                // Find source value
                SourceIndex := -1;
                for j := 0 to Translations[i].Count-1 do
                  if (Translations[i][j].Field = SourceField) then
                  begin
                    SourceIndex := j;
                    break;
                  end;
                SourceFieldTU := SourceField;
              end else
              if (Translations[i].Count > 0) then
              begin
                // Assume first language is source
                SourceIndex := 0;
                SourceFieldTU := Translations[i][0].Field;
              end else
                continue;

              // Ignore if no source value
              if (SourceIndex = -1) then
                continue;

              SourceValue := Translations[i][SourceIndex].Value;
              SanitizedSourceValue := SanitizeText(SourceValue, False);

              for j := 0 to Translations[i].Count-1 do
              begin
                Field := Translations[i][j].Field;

                if (Field = SourceFieldTU) then
                  continue;

                DuplicateAction := TranslationMemory.AddTerm(SourceFieldTU, SourceValue, SanitizedSourceValue, Field, Translations[i][j].Value, Duplicates, Result, DuplicateAction);

                if (DuplicateAction = tmDupActionAbort) then
                  break;
              end;
              if (DuplicateAction = tmDupActionAbort) then
                break;
            end;
            if (DetailedProgress) then
              LocalProgress.Progress(psEnd, 1, 1);
          end else
          begin
            if (DetailedProgress) then
              LocalProgress.Progress(psBegin, 0, Translations.Count, sTranslationMemoryLoadingTerms)
            else
              LocalProgress.UpdateMessage(sTranslationMemoryLoadingTerms);

            // Post all terms to the dataset, one row at a time
            for i := 0 to Translations.Count-1 do
            begin
              if (DetailedProgress) then
                LocalProgress.AdvanceProgress
              else
                LocalProgress.ProcessMessages;

              TableTranslationMemory.Append;
              try

                for j := 0 to Translations[i].Count-1 do
                  Translations[i][j].Field.AsString := Translations[i][j].Value;

                TableTranslationMemory.Post;
              except
                TableTranslationMemory.Cancel;
                raise;
              end;
              Inc(Result.Added);
            end;
            if (DetailedProgress) then
              LocalProgress.Progress(psEnd, 1, 1);
          end;
        finally
          Duplicates.Free;
        end;

      finally
        Translations.Free;
      end;

    finally
      TranslationMemory.Unlock;
    end;
  finally
    TableTranslationMemory.EnableControls;
  end;
end;

// -----------------------------------------------------------------------------

function TTranslationMemoryFileFormat.Prepare(const Filename: string): boolean;
begin
  Result :=True;
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

end.
