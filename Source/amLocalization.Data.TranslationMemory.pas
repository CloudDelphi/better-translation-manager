unit amLocalization.Data.TranslationMemory;

interface

uses
  Generics.Collections,
  System.SysUtils, System.Classes, Data.DB,
  dxmdaset,
  amLocale,
  amLocalization.Dialog.TranslationMemory.SelectDuplicate;

type
  TDataModuleTranslationMemory = class(TDataModule)
    TableTranslationMemory: TdxMemData;
    DataSourceTranslationMemory: TDataSource;
    procedure TableTranslationMemoryAfterDelete(DataSet: TDataSet);
    procedure TableTranslationMemoryAfterInsert(DataSet: TDataSet);
    procedure TableTranslationMemoryAfterPost(DataSet: TDataSet);
    procedure DataModuleCreate(Sender: TObject);
  private
    FFilename: string;
    FLoaded: boolean;
    FFormSelectDuplicate: TFormSelectDuplicate;
    FDuplicateAction: TDuplicateAction;
    FLookupIndex: TDictionary<string, TList<integer>>;
    FModified: boolean;
    FCreateDate: TDateTime;
    function FindField(LocaleItem: TLocaleItem): TField;
    function GetHasData: boolean;
    procedure FieldGetTextEventHandler(Sender: TField; var Text: string; DisplayText: Boolean);
  public
    procedure Add(SourceLanguage: Word; const SourceValue: string; TargetLanguage: Word; const TargetValue: string);
    procedure BeginLookup(SourceLanguage, TargetLanguage: TLocaleItem);
    procedure EndLookup;
    function Lookup(SourceLanguage, TargetLanguage: TLocaleItem; const SourceValue: string; var TargetValue: string): boolean;

    procedure LoadTranslationMemory(const Filename: string);
    procedure SaveTranslationMemory(const Filename: string);
    function CheckSave: boolean;

    property Filename: string read FFilename write FFilename;
    property IsLoaded: boolean read FLoaded;
    property HasData: boolean read GetHasData;
    property Modified: boolean read FModified;
  end;

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
  amCursorService,
  amLocalization.Model,
  amLocalization.Utils;

type
  TTerm = record
    Field: TField;
    Value: string;
  end;

  TTerms = TList<TTerm>;

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

      SaveTranslationMemory(Filename);

      Result := (not Modified);
    end else
      Result := True;
  end else
    Result := True;
end;

procedure TDataModuleTranslationMemory.FieldGetTextEventHandler(Sender: TField; var Text: string; DisplayText: Boolean);
begin
  Text := Sender.AsString;
end;

function TDataModuleTranslationMemory.FindField(LocaleItem: TLocaleItem): TField;
var
  i: integer;
begin
  for i := 0 to TableTranslationMemory.FieldCount-1 do
    if (SameText(LocaleItem.LocaleName, TableTranslationMemory.Fields[i].FieldName)) then
      Exit(TableTranslationMemory.Fields[i]);
  Result := nil;
end;

function TDataModuleTranslationMemory.GetHasData: boolean;
begin
  Result := (TableTranslationMemory.Active) and (TableTranslationMemory.RecordCount > 0);
end;

procedure TDataModuleTranslationMemory.Add(SourceLanguage: Word; const SourceValue: string; TargetLanguage: Word; const TargetValue: string);

  function AddField(LocaleItem: TLocaleItem): TField;
  begin
    Result := TWideMemoField.Create(TableTranslationMemory);

    Result.FieldName := LocaleItem.LocaleSName;
    Result.DisplayLabel := LocaleItem.LanguageName;
    Result.Tag := LocaleItem.CharSet;
    Result.OnGetText := FieldGetTextEventHandler; // Otherwise memo is edited as "(WIDEMEMO)"

    Result.DataSet := TableTranslationMemory;
    Result.DisplayWidth := 100;
  end;

var
  SourceLocaleItem, TargetLocaleItem: TLocaleItem;
  SourceField: TField;
  TargetField: TField;
  Duplicates: TStringList;
  Clone: TdxMemData;
resourcestring
  sTranslationMemoryAddDuplicateTitle = 'Duplicates found';
  sTranslationMemoryAddDuplicate = 'You are adding a term that already has %d translation(s) in the dictionary.'#13#13+
    'Do you want to add the translation anyway?'+#13#13+'%s';
begin
  if (SourceLanguage = TargetLanguage) then
    Exit;

  SourceLocaleItem := TLocaleItems.FindLCID(SourceLanguage);
  TargetLocaleItem := TLocaleItems.FindLCID(TargetLanguage);
  Assert(SourceLocaleItem <> nil);
  Assert(TargetLocaleItem <> nil);

  SourceField := FindField(SourceLocaleItem);
  TargetField := FindField(TargetLocaleItem);

  if (SourceField = nil) or (TargetField = nil) then
  begin
    Clone := TdxMemData.Create(nil);
    try
      Clone.CreateFieldsFromDataSet(TableTranslationMemory);
      Clone.LoadFromDataSet(TableTranslationMemory);

      TableTranslationMemory.Close;

      if (SourceField = nil) then
        SourceField := AddField(SourceLocaleItem);
      if (TargetField = nil) then
        TargetField := AddField(TargetLocaleItem);

      TableTranslationMemory.LoadFromDataSet(Clone);
    finally
      Clone.Free;
    end;
  end;

  if (not TableTranslationMemory.Active) then
    TableTranslationMemory.Open;

  Duplicates := nil;
  try

    TableTranslationMemory.First;
    while (not TableTranslationMemory.EOF) do
    begin
      if (not TargetField.IsNull) and (not SourceField.IsNull) then
      begin
        if (AnsiSameText(SourceValue, SourceField.AsString)) then
        begin
          if (AnsiSameText(TargetValue, TargetField.AsString)) then
            Exit; // Exact duplicate - do nothing

          // Source same, target differs
          if (TargetField.AsString <> '') then
          begin
            if (Duplicates = nil) then
              Duplicates := TStringList.Create;

            Duplicates.Add(TargetField.AsString);
          end;
        end;
      end;

      TableTranslationMemory.Next;
    end;

    if (Duplicates <> nil) then
    begin
      if (TaskMessageDlg(sTranslationMemoryAddDuplicateTitle,
        Format(sTranslationMemoryAddDuplicate, [Duplicates.Count, Duplicates.Text]),
        mtConfirmation, [mbYes, mbNo], 0, mbNo) <> mrYes) then
        Exit;
    end;

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

    FModified := True;

  finally
    Duplicates.Free;
  end;
end;

procedure TDataModuleTranslationMemory.LoadTranslationMemory(const Filename: string);
var
  XML: IXMLDocument;
  Body: IXMLNode;
  Node, ItemNode: IXMLNode;
  LanguageNode: IXMLNode;
  Language: string;
  Languages: TDictionary<string, TField>;
  Translations: TObjectList<TTerms>;
  Term: TTerm;
  Terms: TTerms;
  LocaleItem: TLocaleItem;
  i, j: integer;
  Field: TField;
  s: string;
begin
  XML := TXMLDocument.Create(nil);
  XML.Active := True;

  XML.LoadFromFile(Filename);

  if (XML.DocumentElement.NodeName <> 'tmx') then
    raise Exception.CreateFmt('XML document root node is not named "tmx": %s', [XML.DocumentElement.NodeName]);

  Node := XML.DocumentElement.ChildNodes.FindNode('header');
  if (Node <> nil) then
  begin
    s := VarToStr(Node.Attributes['creationdate']);
    if (s.IsEmpty) or (not TryISO8601ToDate(s, FCreateDate, True)) then
      FCreateDate := Now;
  end;

  Body := XML.DocumentElement.ChildNodes.FindNode('body');
  if (Body = nil) then
    raise Exception.Create('xliff node not found: tmx\body');


  FLoaded := False;
  TableTranslationMemory.Close;
  TableTranslationMemory.Fields.Clear;

  Languages := TDictionary<string, TField>.Create(TTextComparer.Create);
  Translations := TObjectList<TTerms>.Create;
  try

    ItemNode := Body.ChildNodes.First;
    while (ItemNode <> nil) do
    begin
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

            if (not Languages.TryGetValue(Language, Field)) then
            begin
              Field := TWideMemoField.Create(TableTranslationMemory);

              LocaleItem := TLocaleItems.FindLocaleName(Language);
              if (LocaleItem = nil) then
              begin
                ShowMessageFmt('Unknown language: %s', [Language]);
                Field.FieldName := Language;
              end else
              begin
                Field.FieldName := LocaleItem.LocaleSName;
                Field.DisplayLabel := LocaleItem.LanguageName;
                Field.Tag := LocaleItem.CharSet;
              end;

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

    // Post all the terms to the dataset
    TableTranslationMemory.Open;

    for i := 0 to Translations.Count-1 do
    begin
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

  finally
    Translations.Free;
    Languages.Free;
  end;

  FLoaded := True;
  FModified := False;
end;

procedure TDataModuleTranslationMemory.SaveTranslationMemory(const Filename: string);
var
  XML: IXMLDocument;
  Node, Body: IXMLNode;
  ItemNode: IXMLNode;
  LanguageNode: IXMLNode;
  i: integer;
  TempFilename, BackupFilename: string;
begin
  if (not HasData) then
    raise Exception.Create('Translation memory is empty');

  XML := TXMLDocument.Create(nil);
  XML.Active := True;

  XML.Options := [doNodeAutoIndent];
  XML.Active := True;

  XML.AddChild('tmx');
  XML.DocumentElement.Attributes['version'] := '1.4';

  Node := XML.DocumentElement.AddChild('header');
  Node.Attributes['creationtool'] := 'amTranslationManager';
  Node.Attributes['creationtoolversion'] := '1.0';
  Node.Attributes['datatype'] := 'plaintext';
  Node.Attributes['segtype'] := 'sentence';
  Node.Attributes['adminlang'] := 'en-us';
  Node.Attributes['srclang'] := '*all*';
  Node.Attributes['creationdate'] := DateToISO8601(FCreateDate, True);
  Node.Attributes['changedate'] := DateToISO8601(Now, False);

  Body := XML.DocumentElement.AddChild('body');

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

procedure TDataModuleTranslationMemory.TableTranslationMemoryAfterDelete(DataSet: TDataSet);
begin
  FModified := True;
end;

procedure TDataModuleTranslationMemory.TableTranslationMemoryAfterInsert(DataSet: TDataSet);
begin
  FModified := True;
end;

procedure TDataModuleTranslationMemory.TableTranslationMemoryAfterPost(DataSet: TDataSet);
begin
  FModified := True;
end;

function TDataModuleTranslationMemory.Lookup(SourceLanguage, TargetLanguage: TLocaleItem; const SourceValue: string; var TargetValue: string): boolean;
var
  SourceField: TField;
  TargetField: TField;
  Duplicates: TStringList;
  RecordIndex: integer;
  List: TList<integer>;
begin
  Result := False;
  TargetValue := '';

  if (SourceLanguage = TargetLanguage) then
    Exit;

  if (not TableTranslationMemory.Active) then
    Exit;

  SourceField := FindField(SourceLanguage);
  TargetField := FindField(TargetLanguage);

  if (SourceField = nil) or (TargetField = nil) then
    // One or both languages doesn't exist in TM
    Exit;

  Duplicates := nil;
  try

    if (FLookupIndex.TryGetValue(AnsiUppercase(SourceValue), List)) then
    begin
      if (List.Count > 1) then
        Duplicates := TStringList.Create;

      for RecordIndex in List do
      begin
        TableTranslationMemory.RecNo := RecordIndex+1;

        Assert(AnsiSameText(SourceValue, SanitizeText(SourceField.AsString)));

        TargetValue := TargetField.AsString;

        if (Duplicates <> nil) then
          Duplicates.Add(TargetValue);
      end;
    end else
      Exit(False);

(* Old brute force lookup
    begin
      TableTranslationMemory.First;

      while (not TableTranslationMemory.EOF) do
      begin
        if (not TargetField.IsNull) and (not SourceField.IsNull) then
        begin
          if (AnsiSameText(SourceValue, SanitizeText(SourceField.AsString))) then
          begin
            Result := True;

            if (TargetValue <> '') then
            begin
              if (Duplicates = nil) then
                Duplicates := TStringList.Create;

              Duplicates.Add(TargetField.AsString);
            end else
              TargetValue := TargetField.AsString;
          end;
        end;

        TableTranslationMemory.Next;
      end;
    end;
*)

    if (Duplicates <> nil) then
    begin
      if (FFormSelectDuplicate = nil) then
        FFormSelectDuplicate := TFormSelectDuplicate.Create(nil);

      FFormSelectDuplicate.DuplicateAction := FDuplicateAction;

      if (not FFormSelectDuplicate.SelectDuplicate(SourceValue, Duplicates, TargetValue)) then
        Abort;

      FDuplicateAction := FFormSelectDuplicate.DuplicateAction;

      Result := (not (FDuplicateAction in [daSkip, daSkipAll]));
    end;

  finally
    Duplicates.Free;
  end;
end;

procedure TDataModuleTranslationMemory.BeginLookup(SourceLanguage, TargetLanguage: TLocaleItem);
var
  SourceField: TField;
  TargetField: TField;
  SourceValue: string;
  RecordIndex: integer;
  List: TList<integer>;
begin
  FDuplicateAction := daPrompt;
  FLookupIndex := TObjectDictionary<string, TList<integer>>.Create([doOwnsValues]);

  if (TableTranslationMemory.Active) then
  begin
    SourceField := FindField(SourceLanguage);
    TargetField := FindField(TargetLanguage);

    if (SourceField = nil) or (TargetField = nil) then
      // One or both languages doesn't exist in TM
      Exit;

    // Create dictionary of source terms
    TableTranslationMemory.First;
    RecordIndex := 0;
    while (not TableTranslationMemory.EOF) do
    begin
      if (not TargetField.IsNull) and (not SourceField.IsNull) then
      begin
        SourceValue := AnsiUppercase(SanitizeText(SourceField.AsString));

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

end;

procedure TDataModuleTranslationMemory.DataModuleCreate(Sender: TObject);
begin
  FFilename := TPath.ChangeExtension(Application.ExeName, '.tmx');
end;

procedure TDataModuleTranslationMemory.EndLookup;
begin
  FreeAndNil(FFormSelectDuplicate);
  FreeAndNil(FLookupIndex);
end;

end.
