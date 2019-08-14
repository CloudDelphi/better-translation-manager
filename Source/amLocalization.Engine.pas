unit amLocalization.Engine;

interface

uses
  Windows,
  Generics.Collections,
  amLocalization.Model,
  amLocalization.ResourceWriter;

// -----------------------------------------------------------------------------
//
// TTranslateProc
//
// -----------------------------------------------------------------------------
type
  TTranslateProc = reference to function(Language: TTargetLanguage; Prop: TLocalizerProperty; var NewValue: string): boolean;


// -----------------------------------------------------------------------------
//
// TResourceStringSymbolMap
//
// -----------------------------------------------------------------------------
// Loads and parses Delphi DRC file.
// Contains map between resource string names and IDs.
// -----------------------------------------------------------------------------
type
  TResourceStringSymbolMap = class
  private
    FSymbols: TDictionary<string, Word>;
    FIDs: TDictionary<Word, string>;
    FConflicts: TList<string>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const Filename: string);

    procedure Clear;
    procedure Add(const Symbol: string; ID: Word);

    function TryLookupSymbol(const Symbol: string; var ID: Word): boolean;
    function TryLookupID(ID: Word; Var Symbol: string): boolean;

    function HasConflicts: boolean;
  end;


// -----------------------------------------------------------------------------
//
// TProjectResourceProcessor
//
// -----------------------------------------------------------------------------
type
  TLocalizerImportAction = (liaUpdateSource, liaUpdateTarget, liaTranslate);

type
  TProjectResourceProcessor = class
  private
  protected
    class function DefaultTranslator(Language: TTargetLanguage; LocalizerProperty: TLocalizerProperty; var NewValue: string): boolean; static;
  public
    procedure Execute(Action: TLocalizerImportAction; LocalizerProject: TLocalizerProject; Instance: HINST; Language: TTargetLanguage; const ResourceWriter: IResourceWriter; Translator: TTranslateProc = nil); overload;
    procedure Execute(Action: TLocalizerImportAction; LocalizerProject: TLocalizerProject; const Filename: string; Language: TTargetLanguage; const ResourceWriter: IResourceWriter; Translator: TTranslateProc = nil); overload;

    procedure ScanProject(LocalizerProject: TLocalizerProject; Instance: HINST); overload;
    procedure ScanProject(LocalizerProject: TLocalizerProject; const Filename: string); overload;
  end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  Classes,
  SysUtils,
  IOUtils;


// -----------------------------------------------------------------------------
//
// TModuleResourceProcessor
//
// -----------------------------------------------------------------------------
constructor TResourceStringSymbolMap.Create;
begin
  inherited Create;

  FSymbols := TDictionary<string, Word>.Create;
  FIDs := TDictionary<Word, string>.Create;
end;

destructor TResourceStringSymbolMap.Destroy;
begin
  FSymbols.Free;
  FIDs.Free;
  FConflicts.Free;

  inherited;
end;

// -----------------------------------------------------------------------------

function TResourceStringSymbolMap.HasConflicts: boolean;
begin
  Result := (FConflicts <> nil) and (FConflicts.Count > 0);
end;

// -----------------------------------------------------------------------------

procedure TResourceStringSymbolMap.Clear;
begin
  FSymbols.Clear;
  FIDs.Clear;
  FreeAndNil(FConflicts);
end;

// -----------------------------------------------------------------------------

procedure TResourceStringSymbolMap.LoadFromFile(const Filename: string);
var
  Reader: TStreamReader;
  i: integer;
  s: string;
  Symbol: string;
  Value: string;
  ResourceID: integer;
begin
  Clear;

  // DRC-file being encoded in ANSI is clearly a bug in Delphi but we'll have to deal with it.
  // Luckily the part of the file we're using (#define) is almost always ASCII.
  Reader := TStreamReader.Create(Filename, TEncoding.ANSI);
  try
    while (not Reader.EndOfStream) do
    begin
      s := Reader.ReadLine;

      if (not s.StartsWith('#define ')) then
        continue;

      Delete(s, 1, Length('#define '));

      i := Pos(' ', s);
      if (i < 2) then
        continue;

      Symbol := Copy(s, 1, i-1);
      Value := Copy(s, i+1, MaxInt);

      if (TryStrToInt(Value, ResourceID)) then
        Add(Symbol, ResourceID);
    end;

  finally
    Reader.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TResourceStringSymbolMap.Add(const Symbol: string; ID: Word);
var
  SymbolName, DuplicateName: string;
  DuplicateValue: Word;
begin
  SymbolName := AnsiUppercase(Symbol);
  // Look for existing symbol
  if (FSymbols.TryGetValue(SymbolName, DuplicateValue)) then
  begin
    // Existing symbol found
    if (DuplicateValue <> ID) then
    begin
      // Existing is conflict.
      // Update existing symbol->id
      FSymbols.AddOrSetValue(SymbolName, ID);
      // Get existing id->symbol
      DuplicateName := FIDs[DuplicateValue];
      // Replace existing id->symbol with new
      FIDs.Remove(DuplicateValue);
      FIDs.Add(ID, Symbol);
      // Save conflict
      if (DuplicateName <> Symbol) then
      begin
        if (FConflicts = nil) then
          FConflicts := TList<string>.Create;
        FConflicts.Add(DuplicateName);
      end;
    end;
  end else
  begin
    FSymbols.Add(SymbolName, ID);

    // Look for existing ID
    if (FIDs.TryGetValue(ID, DuplicateName)) then
    begin
      // Existing ID found.
      if (DuplicateName <> Symbol) then
      begin
        // Existing is conflict.
        // Update existing id->symbol
        FIDs.AddOrSetValue(ID, Symbol);
        // Remove existing symbol->id
        FSymbols.Remove(AnsiUppercase(DuplicateName));

        // Save conflict
        if (FConflicts = nil) then
          FConflicts := TList<string>.Create;
        FConflicts.Add(DuplicateName);
      end;
    end else
      FIDs.Add(ID, Symbol);
  end;
end;

// -----------------------------------------------------------------------------

function TResourceStringSymbolMap.TryLookupID(ID: Word; var Symbol: string): boolean;
begin
  Result := FIDs.TryGetValue(ID, Symbol);
end;

// -----------------------------------------------------------------------------

function TResourceStringSymbolMap.TryLookupSymbol(const Symbol: string; var ID: Word): boolean;
begin
  Result := FSymbols.TryGetValue(AnsiUppercase(Symbol), ID);
end;


// -----------------------------------------------------------------------------
//
// TModuleResourceProcessor
//
// -----------------------------------------------------------------------------
type
  TModuleResourceProcessor = class abstract
  private
    FLocalizerModule: TLocalizerModule;
    FInstance: HINST;
  protected
    property LocalizerModule: TLocalizerModule read FLocalizerModule;
    property Instance: HINST read FInstance;
  public
    constructor Create(ALocalizerModule: TLocalizerModule; AInstance: HINST);

    function Execute(Action: TLocalizerImportAction; ResourceWriter: IResourceWriter; Language: TTargetLanguage; Translator: TTranslateProc): boolean; overload; virtual; abstract;
    function Execute(Action: TLocalizerImportAction; ResourceWriter: IResourceWriter; Language: TTargetLanguage): boolean; overload;
  end;

// -----------------------------------------------------------------------------

constructor TModuleResourceProcessor.Create(ALocalizerModule: TLocalizerModule; AInstance: HINST);
begin
  inherited Create;
  FLocalizerModule := ALocalizerModule;
  FInstance := AInstance;
end;

// -----------------------------------------------------------------------------

function TModuleResourceProcessor.Execute(Action: TLocalizerImportAction; ResourceWriter: IResourceWriter; Language: TTargetLanguage): boolean;
begin
  Result := Execute(Action, ResourceWriter, Language, TProjectResourceProcessor.DefaultTranslator);
end;


// -----------------------------------------------------------------------------
//
// TModuleDFMResourceProcessor
//
// -----------------------------------------------------------------------------
type
  TModuleDFMResourceProcessor = class(TModuleResourceProcessor)
  private
    FStream: TStream;
    FReader: TReader;
    FWriter: TWriter;
    FLastCopied: integer;
  protected
    procedure ReadCollection(Action: TLocalizerImportAction; Language: TTargetLanguage; Translator: TTranslateProc; LocalizerItem: TLocalizerItem; const Path: string);
    procedure ReadProperty(Action: TLocalizerImportAction; Language: TTargetLanguage; Translator: TTranslateProc; LocalizerItem: TLocalizerItem; const Path: string);
    procedure ReadComponent(Action: TLocalizerImportAction; Language: TTargetLanguage; Translator: TTranslateProc; const Path: string);

    procedure CopyToHere(CopyPos: int64);
  public
    destructor Destroy; override;

    function Execute(Action: TLocalizerImportAction; ResourceWriter: IResourceWriter; Language: TTargetLanguage; Translator: TTranslateProc): boolean; override;
  end;

// -----------------------------------------------------------------------------

procedure TModuleDFMResourceProcessor.CopyToHere(CopyPos: int64);
var
  Buffer: TBytes;
  Count, Chunk: integer;
  SavePos: integer;
begin
  Assert(FLastCopied <= CopyPos);

  if (FWriter <> nil) then
  begin
    Count := CopyPos-FLastCopied;

    if (Count > 1024) then
      SetLength(Buffer, 1024)
    else
      SetLength(Buffer, Count);

    SavePos := FReader.Position;
    FReader.Position := FLastCopied;

    while (Count > 0) do
    begin
      if (Count > Length(Buffer)) then
        Chunk := Length(Buffer)
      else
        Chunk := Count;

      FReader.Read(Buffer, Chunk);
      FWriter.Write(Buffer, Chunk);

      Dec(Count, Chunk);
      Inc(FLastCopied, Chunk);
    end;

    FReader.Position := SavePos;

    Assert(FLastCopied = CopyPos);
  end else
    FLastCopied := CopyPos;
end;

// -----------------------------------------------------------------------------

destructor TModuleDFMResourceProcessor.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TModuleDFMResourceProcessor.Execute(Action: TLocalizerImportAction; ResourceWriter: IResourceWriter; Language: TTargetLanguage; Translator: TTranslateProc): boolean;
const
  FilerSignature: UInt32 = $30465054; // ($54, $50, $46, $30) 'TPF0'
var
  SourceStream: TStream;
  TargetStream: TMemoryStream;
  Signature: UInt32;
begin
  Assert(LocalizerModule.Kind = mkForm);

  if (FindResource(Instance, PChar(LocalizerModule.Name), RT_RCDATA) = 0) then
  begin
    if (Action = liaUpdateSource) then
      LocalizerModule.State := ItemStateUnused;
    Exit(False);
  end;

  SourceStream := TResourceStream.Create(Instance, LocalizerModule.Name, RT_RCDATA);
  try

    SourceStream.Read(Signature, SizeOf(Signature));
    if (Signature <> FilerSignature) then
    begin
      if (Action = liaUpdateSource) then
      begin
        LocalizerModule.Kind := mkOther;
        LocalizerModule.Status := ItemStatusDontTranslate;
      end;
      Exit(False);
    end;

    SourceStream.Position := 0;

    if (ResourceWriter <> nil) then
      TargetStream := TMemoryStream.Create
    else
      TargetStream := nil;
    try
      // When updating source we need to process the module even if it is marked as "don't translate".
      // Otherwise the items/properties will be left with lItemStateUnused state.
      if (Action = liaUpdateSource) or (LocalizerModule.Status = ItemStatusTranslate) then
      begin
        FReader := TReader.Create(SourceStream, 1024);
        try
          if (TargetStream <> nil) then
            FWriter := TWriter.Create(TargetStream, 1024)
          else
            FWriter := nil;
          try

            FReader.ReadSignature;

            ReadComponent(Action, Language, Translator, '');

            CopyToHere(SourceStream.Size);

          finally
            FreeAndNil(FWriter);
          end;
        finally
          FreeAndNil(FReader);
        end;
      end else
      if (ResourceWriter <> nil) and (TargetStream <> nil) then
        TargetStream.CopyFrom(SourceStream, 0);

      if (ResourceWriter <> nil) and (TargetStream <> nil) then
      begin
        // Write translated resource data
        TargetStream.Position := 0;
        ResourceWriter.WriteModule(LocalizerModule, PWideChar(LocalizerModule.Name), TargetStream);
      end;
    finally
      TargetStream.Free;
    end;
  finally
    SourceStream.Free;
  end;

  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TModuleDFMResourceProcessor.ReadCollection(Action: TLocalizerImportAction; Language: TTargetLanguage; Translator: TTranslateProc; LocalizerItem: TLocalizerItem; const Path: string);
var
  Index: integer;
  CollectionLocalizerItem: TLocalizerItem;
  Name: string;
begin
  // TReader.ReadCollection
  Index := 0;
  while (not FReader.EndOfList) do
  begin
    if (FReader.NextValue in [vaInt8, vaInt16, vaInt32]) then
      FReader.ReadInteger;

    Name := Format('%s[%d]', [Path, Index]);
    CollectionLocalizerItem := LocalizerModule.AddItem(Name, LocalizerItem.TypeName);

    FReader.ReadListBegin;
    while (not FReader.EndOfList) do
      ReadProperty(Action, Language, Translator, CollectionLocalizerItem, Name);
    FReader.ReadListEnd;

    if (CollectionLocalizerItem.State = ItemStateNew) and (CollectionLocalizerItem.Properties.Count = 0) then
      CollectionLocalizerItem.Free;

    Inc(Index);
  end;
  FReader.ReadListEnd;
end;

// -----------------------------------------------------------------------------

procedure TModuleDFMResourceProcessor.ReadComponent(Action: TLocalizerImportAction; Language: TTargetLanguage; Translator: TTranslateProc; const Path: string);
var
  Flags: TFilerFlags;
  Position: Integer;
  Name, ClassName: string;
  LocalizerItem: TLocalizerItem;
begin
  // TReader.ReadComponent
  FReader.ReadPrefix(Flags, Position);

  ClassName := FReader.ReadStr;
  Name := FReader.ReadStr;

  if (Path <> '') then
    Name := Path + '.' + Name;

  LocalizerItem := LocalizerModule.AddItem(Name, ClassName);

  // TReader.ReadDataInner
  while (not FReader.EndOfList) do
    ReadProperty(Action, Language, Translator, LocalizerItem, Name);
  FReader.ReadListEnd;

  // Remove empty item
  if (LocalizerItem.State = ItemStateNew) and (LocalizerItem.Properties.Count = 0) then
    LocalizerItem.Free;

  while (not FReader.EndOfList) do
    ReadComponent(Action, Language, Translator, Name);
  FReader.ReadListEnd;
end;

// -----------------------------------------------------------------------------

procedure TModuleDFMResourceProcessor.ReadProperty(Action: TLocalizerImportAction; Language: TTargetLanguage; Translator: TTranslateProc; LocalizerItem: TLocalizerItem; const Path: string);
var
  PropertyName: string;
  LocalizerProperty: TLocalizerProperty;
  ValueType: TValueType;
  StartPos, EndPos: int64;
  Value, NewValue: string;
begin
  // TReader.ReadProperty
  PropertyName := FReader.ReadStr;

  ValueType := FReader.NextValue;

  case ValueType of
  vaCollection:
    begin
      FReader.ReadValue;
      ReadCollection(Action, Language, Translator, LocalizerItem, Path+'.'+PropertyName);
    end;

  vaWString, vaUTF8String, vaString, vaLString:
    begin
      StartPos := FReader.Position;

      // TReader.ReadPropValue
      Value := FReader.ReadString;

      EndPos := FReader.Position;

      if (Action = liaUpdateSource) then
        LocalizerProperty := LocalizerItem.AddProperty(PropertyName, Value)
      else
      if (Action = liaTranslate) then
        LocalizerProperty := LocalizerItem.AddProperty(PropertyName)
      else
      if (Action = liaUpdateTarget) and (Language <> nil) then
      begin
        LocalizerProperty := LocalizerItem.AddProperty(PropertyName);
        LocalizerProperty.TranslatedValue[Language] := Value;
      end;

      // Perform translation
      if (Action = liaTranslate) and (Language <> nil) and (Assigned(Translator)) then
      begin
        NewValue := LocalizerProperty.Value;

        if (Translator(Language, LocalizerProperty, NewValue)) and (NewValue <> Value) and (FWriter <> nil) then
        begin
          // Copy up until original value
          CopyToHere(StartPos);

          // Write new value
          FWriter.WriteString(NewValue);

          // Skip original value in source so it doesn't get copied
          FLastCopied := EndPos;
        end;
      end;
    end;
  else
    FReader.SkipValue;
  end;
end;


// -----------------------------------------------------------------------------
//
// TModuleStringResourceProcessor
//
// -----------------------------------------------------------------------------
type
  TModuleStringResourceProcessor = class(TModuleResourceProcessor)
  private
    FSymbolMap: TResourceStringSymbolMap;
  protected
    procedure ExecuteGroup(Action: TLocalizerImportAction; ResourceGroupID: Word; ReadStream, WriteStream: TStream; Language: TTargetLanguage; Translator: TTranslateProc);
  public
    constructor Create(ALocalizerModule: TLocalizerModule; AInstance: HINST);
    destructor Destroy; override;

    function Execute(Action: TLocalizerImportAction; ResourceWriter: IResourceWriter; Language: TTargetLanguage; Translator: TTranslateProc): boolean; override;
  end;

// -----------------------------------------------------------------------------

constructor TModuleStringResourceProcessor.Create(ALocalizerModule: TLocalizerModule; AInstance: HINST);
var
  Size: integer;
  Filename: string;
begin
  inherited Create(ALocalizerModule, AInstance);

  FSymbolMap := TResourceStringSymbolMap.Create;

  Filename := LocalizerModule.Project.SourceFilename;

  if (not TFile.Exists(Filename)) then
  begin
    SetLength(Filename, MAX_PATH);
    Size := GetModuleFileName(Instance, PChar(Filename), Length(Filename)+1);
    SetLength(Filename, Size);
  end;

  Filename := TPath.ChangeExtension(Filename, '.drc');

  if (TFile.Exists(Filename)) then
    FSymbolMap.LoadFromFile(Filename);
end;

// -----------------------------------------------------------------------------

destructor TModuleStringResourceProcessor.Destroy;
begin
  FSymbolMap.Free;

  inherited;
end;

function TModuleStringResourceProcessor.Execute(Action: TLocalizerImportAction; ResourceWriter: IResourceWriter; Language: TTargetLanguage; Translator: TTranslateProc): boolean;
var
  SourceStream: TStream;
  TargetStream: TMemoryStream;
  ResourceID: Word;
  AnyFound: boolean;
begin
  Assert(LocalizerModule.Kind = mkString);

  if (LocalizerModule.ResourceGroups.Count = 0) then
    Exit(False);

  LocalizerModule.ResourceGroups.Sort;

  AnyFound := False;

  for ResourceID in LocalizerModule.ResourceGroups do
  begin
    if (FindResource(Instance, PChar(ResourceID), RT_STRING) <> 0) then
    begin
      SourceStream := TResourceStream.CreateFromID(Instance, ResourceID, RT_STRING);
      AnyFound := True;
    end else
      SourceStream := nil;
    try

      if (Action = liaTranslate) and (ResourceWriter <> nil) and (SourceStream <> nil) then
        TargetStream := TMemoryStream.Create
      else
        TargetStream := nil;
      try

        if (LocalizerModule.Status = ItemStatusTranslate) then
          ExecuteGroup(Action, ResourceID, SourceStream, TargetStream, Language, Translator)
        else
        if (Action = liaTranslate) and (ResourceWriter <> nil) and (SourceStream <> nil) and (TargetStream <> nil) then
          TargetStream.CopyFrom(SourceStream, 0);

        if (Action = liaTranslate) and (ResourceWriter <> nil) and (TargetStream <> nil) then
        begin
          // Write translated resource data
          TargetStream.Position := 0;
          ResourceWriter.WriteModule(LocalizerModule, PWideChar(ResourceID), TargetStream);
        end;
      finally
        TargetStream.Free;
      end;
    finally
      SourceStream.Free;
    end;
  end;

  if (Action = liaUpdateSource) and (not AnyFound) then
    LocalizerModule.State := ItemStateUnused;

  Result := True;
end;

procedure TModuleStringResourceProcessor.ExecuteGroup(Action: TLocalizerImportAction; ResourceGroupID: Word; ReadStream, WriteStream: TStream; Language: TTargetLanguage; Translator: TTranslateProc);
var
  i: integer;
  Size: Word;
  Value: string;
  Item: TLocalizerItem;
  ResourceID: Word;
  Name: string;
  LocalizerProperty: TLocalizerProperty;
begin
  i := 0;
  ResourceID := (ResourceGroupID-1) * 16;
  while (i < 16) do
  begin
    if (ReadStream <> nil) then
    begin
      ReadStream.Read(Size, SizeOf(Size));
      SetLength(Value, Size);

      if (Size > 0) then
        ReadStream.Read(PChar(Value)^, Size*SizeOf(Char));

      // Skip string if value is empty and ID is unknown
      Name := '';
      if (FSymbolMap.TryLookupID(ResourceID, Name)) or (not Value.IsEmpty) then
      begin
        if (not Name.IsEmpty) then
          Item := LocalizerModule.AddItem(Name, '')
        else
          // TODO : This could be considered an error. Should flag item as unmapped.
          Item := LocalizerModule.AddItem(ResourceID, '');


        if (Item.Status <> ItemStatusDontTranslate) then
        begin
          if (Action = liaUpdateSource) then
            LocalizerProperty := Item.AddProperty('', Value)
          else
          if (Action = liaTranslate) then
            LocalizerProperty := Item.AddProperty('')
          else
          if (Action = liaUpdateTarget) and (Language <> nil) then
          begin
            LocalizerProperty := Item.AddProperty('');
            LocalizerProperty.TranslatedValue[Language] := Value;
          end;

          // Perform translation
          if (Action = liaTranslate) and (Language <> nil) and (Assigned(Translator)) then
          begin
            Value := LocalizerProperty.Value;

            if (not Translator(Language, LocalizerProperty, Value)) then
              Value := LocalizerProperty.Value;
          end;
        end;
      end;

      if (Action = liaUpdateSource) and (WriteStream <> nil) then
      begin
        Size := Length(Value);
        WriteStream.Write(Size, SizeOf(Size));

        if (Size > 0) then
          WriteStream.Write(PChar(Value)^, Size*SizeOf(Char));
      end;
    end else
    if (Action = liaUpdateSource) then
    begin
      // Source item not found in resource - mark unused
      if (LocalizerModule.Items.TryGetValue('', Item)) then
        Item.State := ItemStateUnused;
    end;

    Inc(i);
    Inc(ResourceID);
  end;
end;


// -----------------------------------------------------------------------------
//
// TProjectProcessorDFMResource
//
// -----------------------------------------------------------------------------
class function TProjectResourceProcessor.DefaultTranslator(Language: TTargetLanguage; LocalizerProperty: TLocalizerProperty; var NewValue: string): boolean;
var
  Translation: TLocalizerTranslation;
begin
  Assert(Language <> nil);

  if (LocalizerProperty.Status <> ItemStatusTranslate) then
    Exit(False);

  if (not LocalizerProperty.Translations.TryGetTranslation(Language, Translation)) or (not (Translation.Status in [tStatusProposed, tStatusTranslated])) then
    Exit(False);

  NewValue := Translation.Value;
  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TProjectResourceProcessor.Execute(Action: TLocalizerImportAction; LocalizerProject: TLocalizerProject; const Filename: string; Language: TTargetLanguage; const ResourceWriter: IResourceWriter; Translator: TTranslateProc);
var
  Module: HModule;
begin
  Module := LoadLibraryEx(PChar(FileName), 0, LOAD_LIBRARY_AS_DATAFILE);
  if (Module = 0) then
    RaiseLastOSError;
  try

    Execute(Action, LocalizerProject, Module, Language, ResourceWriter, Translator);

  finally
    FreeLibrary(Module);
  end;
end;

// -----------------------------------------------------------------------------

procedure TProjectResourceProcessor.ScanProject(LocalizerProject: TLocalizerProject; const Filename: string);
begin
  Execute(liaUpdateSource, LocalizerProject, Filename, nil, nil);
end;

procedure TProjectResourceProcessor.ScanProject(LocalizerProject: TLocalizerProject; Instance: HINST);
begin
  Execute(liaUpdateSource, LocalizerProject, Instance, nil, nil);
end;

// -----------------------------------------------------------------------------

function ResourceIdentToOrdinal(Ident: PWideChar): WORD;
begin
  Result := DWORD(Ident) and $0000FFFF;
end;

function EnumResourceNamesProc(Module: HMODULE; ResType, ResName: PChar; LocalizerProject: TLocalizerProject): BOOL; stdcall;
var
  LocalizerModule: TLocalizerModule;
  ResourceID: WORD;
begin
  if (ResType = RT_RCDATA) then
  begin
    LocalizerModule := LocalizerProject.AddModule(ResName);
    LocalizerModule.Kind := mkForm;
  end else
  if (ResType = RT_STRING) then
  begin
    ResourceID := ResourceIdentToOrdinal(ResName);

    LocalizerModule := LocalizerProject.AddModule(sModuleNameResourcestrings);
    LocalizerModule.Kind := mkString;
    LocalizerModule.ResourceGroups.Add(ResourceID);
  end;

  Result := True;
end;

procedure TProjectResourceProcessor.Execute(Action: TLocalizerImportAction; LocalizerProject: TLocalizerProject; Instance: HINST; Language: TTargetLanguage; const ResourceWriter: IResourceWriter; Translator: TTranslateProc);
var
  LocalizerModule: TLocalizerModule;
  ModuleProcessor: TModuleResourceProcessor;
begin
  LocalizerProject.BeginUpdate;
  try
    LocalizerProject.BeginLoad(Action = liaUpdateSource);
    try

      EnumResourceNames(Instance, RT_RCDATA, @EnumResourceNamesProc, integer(LocalizerProject));

      EnumResourceNames(Instance, RT_STRING, @EnumResourceNamesProc, integer(LocalizerProject));

      if (ResourceWriter <> nil) then
        ResourceWriter.BeginWrite;
      try

        if (Action = liaTranslate) and (not Assigned(Translator)) then
          Translator := TProjectResourceProcessor.DefaultTranslator;

        for LocalizerModule in LocalizerProject.Modules.Values.ToArray do // ToArray for stability since we delete from the list
        begin
          if (LocalizerModule.Kind = mkForm) then
            ModuleProcessor := TModuleDFMResourceProcessor.Create(LocalizerModule, Instance)
          else
          if (LocalizerModule.Kind = mkString) then
            ModuleProcessor := TModuleStringResourceProcessor.Create(LocalizerModule, Instance)
          else
          begin
            LocalizerModule.Free;
            continue;
          end;
          try

            ModuleProcessor.Execute(Action, ResourceWriter, Language, Translator);

          finally
            ModuleProcessor.Free;
          end;

          if (LocalizerModule.State = ItemStateNew) and ((LocalizerModule.Kind = mkOther) or (LocalizerModule.Items.Count = 0)) then
          begin
            LocalizerModule.Free;
            continue;
          end;
        end;

        if (ResourceWriter <> nil) then
          ResourceWriter.EndWrite(True);

      except
        if (ResourceWriter <> nil) then
          ResourceWriter.EndWrite(False);

        raise;
      end;
    finally
      LocalizerProject.EndLoad;
    end;
  finally
    LocalizerProject.EndUpdate;
  end;
end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------


end.
