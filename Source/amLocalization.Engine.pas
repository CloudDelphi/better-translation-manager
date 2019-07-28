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
  TTranslateProc = reference to function(LocaleID: LCID; Prop: TLocalizerProperty; var NewValue: string): boolean;


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
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const Filename: string);

    procedure Clear;
    procedure Add(const Symbol: string; ID: Word);

    function TryLookupSymbol(const Symbol: string; var ID: Word): boolean;
    function TryLookupID(ID: Word; Var Symbol: string): boolean;
  end;


// -----------------------------------------------------------------------------
//
// TProjectResourceProcessor
//
// -----------------------------------------------------------------------------
type
  TProjectResourceProcessor = class
  private
    FSymbolMap: TResourceStringSymbolMap;
  protected
    class function DefaultTranslator(LocaleID: LCID; LocalizerProperty: TLocalizerProperty; var NewValue: string): boolean; static;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute(LocalizerProject: TLocalizerProject; Instance: HINST; LocaleID: LCID; const ResourceWriter: IResourceWriter; Translator: TTranslateProc = nil); overload;
    procedure Execute(LocalizerProject: TLocalizerProject; const Filename: string; LocaleID: LCID; const ResourceWriter: IResourceWriter; Translator: TTranslateProc = nil); overload;

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

  inherited;
end;

// -----------------------------------------------------------------------------

procedure TResourceStringSymbolMap.Clear;
begin
  FSymbols.Clear;
  FIDs.Clear;
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

  Reader := TStreamReader.Create(Filename);
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
begin
  FSymbols.Add(AnsiUppercase(Symbol), ID);
  FIDs.Add(ID, Symbol);
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
  protected
    property LocalizerModule: TLocalizerModule read FLocalizerModule;
  public
    constructor Create(ALocalizerModule: TLocalizerModule);

    procedure Execute(ReadStream, WriteStream: TStream; LocaleID: LCID; Translator: TTranslateProc); overload; virtual; abstract;
    procedure Execute(ReadStream, WriteStream: TStream; LocaleID: LCID); overload;
  end;

// -----------------------------------------------------------------------------

constructor TModuleResourceProcessor.Create(ALocalizerModule: TLocalizerModule);
begin
  inherited Create;
  FLocalizerModule := ALocalizerModule;
end;

// -----------------------------------------------------------------------------

procedure TModuleResourceProcessor.Execute(ReadStream, WriteStream: TStream; LocaleID: LCID);
begin
  Execute(ReadStream, WriteStream, LocaleID, TProjectResourceProcessor.DefaultTranslator);
end;


// -----------------------------------------------------------------------------
//
// TModuleDFMResourceProcessor
//
// -----------------------------------------------------------------------------
type
  TModuleDFMResourceProcessor = class(TModuleResourceProcessor)
  private
    FReader: TReader;
    FWriter: TWriter;
    FLastCopied: integer;
  protected
    procedure ReadCollection(LocaleID: LCID; Translator: TTranslateProc; LocalizerItem: TLocalizerItem; const Path: string);
    procedure ReadProperty(LocaleID: LCID; Translator: TTranslateProc; LocalizerItem: TLocalizerItem; const Path: string);
    procedure ReadComponent(LocaleID: LCID; Translator: TTranslateProc; const Path: string);

    procedure CopyToHere(CopyPos: int64);
  public
    procedure Execute(ReadStream, WriteStream: TStream; LocaleID: LCID; Translator: TTranslateProc); override;
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

procedure TModuleDFMResourceProcessor.Execute(ReadStream, WriteStream: TStream; LocaleID: LCID; Translator: TTranslateProc);
begin
  Assert(ReadStream <> nil);
  Assert(LocalizerModule.Kind = mkForm);

  FReader := TReader.Create(ReadStream, 1024);
  try
    if (WriteStream <> nil) then
      FWriter := TWriter.Create(WriteStream, 1024)
    else
      FWriter := nil;
    try

      FReader.ReadSignature;

      ReadComponent(LocaleID, Translator, '');

      CopyToHere(ReadStream.Size);

    finally
      FreeAndNil(FWriter);
    end;
  finally
    FreeAndNil(FReader);
  end;
end;

// -----------------------------------------------------------------------------

procedure TModuleDFMResourceProcessor.ReadCollection(LocaleID: LCID; Translator: TTranslateProc; LocalizerItem: TLocalizerItem; const Path: string);
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
      ReadProperty(LocaleID, Translator, CollectionLocalizerItem, Name);
    FReader.ReadListEnd;
    Inc(Index);
  end;
  FReader.ReadListEnd;
end;

// -----------------------------------------------------------------------------

procedure TModuleDFMResourceProcessor.ReadComponent(LocaleID: LCID; Translator: TTranslateProc; const Path: string);
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
    ReadProperty(LocaleID, Translator, LocalizerItem, Name);
  FReader.ReadListEnd;

  while (not FReader.EndOfList) do
    ReadComponent(LocaleID, Translator, Name);
  FReader.ReadListEnd;
end;

// -----------------------------------------------------------------------------

procedure TModuleDFMResourceProcessor.ReadProperty(LocaleID: LCID; Translator: TTranslateProc; LocalizerItem: TLocalizerItem; const Path: string);
var
  PropertyName: string;
  LocalizerProperty: TLocalizerProperty;
  ValueType: TValueType;
  StartPos, EndPos: int64;
  NewValue: string;
begin
  // TReader.ReadProperty
  PropertyName := FReader.ReadStr;

  ValueType := FReader.NextValue;

  case ValueType of
  vaCollection:
    begin
      FReader.ReadValue;
      ReadCollection(LocaleID, Translator, LocalizerItem, Path+'.'+PropertyName);
    end;

  vaWString, vaUTF8String, vaString, vaLString:
    begin
      LocalizerProperty := LocalizerItem.AddProperty(PropertyName);

      StartPos := FReader.Position;

      // TReader.ReadPropValue
      LocalizerProperty.Value := FReader.ReadString;

      EndPos := FReader.Position;

      // Perform translation
      if (Assigned(Translator)) then
      begin
        NewValue := LocalizerProperty.Value;

        if (Translator(LocaleID, LocalizerProperty, NewValue)) and (NewValue <> LocalizerProperty.Value) and (FWriter <> nil) then
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
  public
    constructor Create(ALocalizerModule: TLocalizerModule; ASymbolMap: TResourceStringSymbolMap);

    procedure Execute(ReadStream, WriteStream: TStream; LocaleID: LCID; Translator: TTranslateProc); override;
  end;

// -----------------------------------------------------------------------------

constructor TModuleStringResourceProcessor.Create(ALocalizerModule: TLocalizerModule; ASymbolMap: TResourceStringSymbolMap);
begin
  inherited Create(ALocalizerModule);
  FSymbolMap := ASymbolMap;
end;

// -----------------------------------------------------------------------------

procedure TModuleStringResourceProcessor.Execute(ReadStream, WriteStream: TStream; LocaleID: LCID; Translator: TTranslateProc);
var
  i: integer;
  Size: Word;
  Value: string;
  Item: TLocalizerItem;
  ResourceID: Word;
  Name: string;
  LocalizerProperty: TLocalizerProperty;
begin
  Assert(ReadStream <> nil);
  Assert(LocalizerModule.Kind = mkString);

  i := 0;
  ResourceID := (LocalizerModule.ResourceID-1) * 16;
  while (i < 16) do
  begin
    ReadStream.Read(Size, SizeOf(Size));
    SetLength(Value, Size);

    if (Size > 0) then
      ReadStream.Read(PChar(Value)^, Size*SizeOf(Char));

    // Skip string if value is empty and ID is unknown
    Name := '';
    if (FSymbolMap.TryLookupID(ResourceID, Name)) or (not Value.IsEmpty) then
    begin
      Item := LocalizerModule.AddItem(ResourceID, '');

      if (not Name.IsEmpty) then
        Item.Name := Name;

      if (Item.Status <> lItemStatusDontTranslate) then
      begin
        LocalizerProperty := Item.AddProperty('', Value);

        // Perform translation
        if (Assigned(Translator)) then
        begin
          Value := LocalizerProperty.Value;

          if (not Translator(LocaleID, LocalizerProperty, Value)) then
            Value := LocalizerProperty.Value;
        end;
      end;
    end;

    if (WriteStream <> nil) then
    begin
      Size := Length(Value);
      WriteStream.Write(Size, SizeOf(Size));

      if (Size > 0) then
        WriteStream.Write(PChar(Value)^, Size*SizeOf(Char));
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
constructor TProjectResourceProcessor.Create;
begin
  inherited Create;
  FSymbolMap := TResourceStringSymbolMap.Create;
end;

destructor TProjectResourceProcessor.Destroy;
begin
  FSymbolMap.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

class function TProjectResourceProcessor.DefaultTranslator(LocaleID: LCID; LocalizerProperty: TLocalizerProperty; var NewValue: string): boolean;
var
  Translation: TLocalizerTranslation;
begin
  if (LocalizerProperty.Status <> lItemStatusTranslate) or
    (LocalizerProperty.Item.Status <> lItemStatusTranslate) or
    (LocalizerProperty.Item.Module.Status <> lItemStatusTranslate) then
    Exit(False);

  if (not LocalizerProperty.Translations.TryGetTranslation(LocaleID, Translation)) or (not (Translation.Status in [tStatusProposed, tStatusTranslated])) then
    Exit(False);

  NewValue := Translation.Value;
  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TProjectResourceProcessor.Execute(LocalizerProject: TLocalizerProject; const Filename: string; LocaleID: LCID; const ResourceWriter: IResourceWriter; Translator: TTranslateProc);
var
  Module: HModule;
begin
  Module := LoadLibraryEx(PChar(FileName), 0, LOAD_LIBRARY_AS_DATAFILE);
  if (Module = 0) then
    RaiseLastOSError;
  try

    Execute(LocalizerProject, Module, LocaleID, ResourceWriter, Translator);

  finally
    FreeLibrary(Module);
  end;
end;

// -----------------------------------------------------------------------------

procedure TProjectResourceProcessor.ScanProject(LocalizerProject: TLocalizerProject; const Filename: string);
begin
  Execute(LocalizerProject, Filename, 0, nil);
end;

procedure TProjectResourceProcessor.ScanProject(LocalizerProject: TLocalizerProject; Instance: HINST);
begin
  Execute(LocalizerProject, Instance, 0, nil);
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

    LocalizerModule := LocalizerProject.AddModule(IntToStr(ResourceID));
    LocalizerModule.Kind := mkString;
    LocalizerModule.ResourceID := ResourceID;
  end;

  Result := True;
end;

procedure TProjectResourceProcessor.Execute(LocalizerProject: TLocalizerProject; Instance: HINST; LocaleID: LCID; const ResourceWriter: IResourceWriter; Translator: TTranslateProc);
var
  LocalizerModule: TLocalizerModule;
  Stream: TStream;
  TranslatedStream: TMemoryStream;
  Signature: UInt32;
  ModuleProcessor: TModuleResourceProcessor;
  Size: integer;
  Filename: string;
const
  FilerSignature: UInt32 = $30465054; // ($54, $50, $46, $30) 'TPF0'
begin
  LocalizerProject.BeginLoad;
  try

    EnumResourceNames(Instance, RT_RCDATA, @EnumResourceNamesProc, integer(LocalizerProject));

    EnumResourceNames(Instance, RT_STRING, @EnumResourceNamesProc, integer(LocalizerProject));

    SetLength(Filename, MAX_PATH);
    Size := GetModuleFileName(Instance, PChar(Filename), Length(Filename)+1);
    SetLength(Filename, Size);

    Filename := TPath.ChangeExtension(Filename, '.drc');
    if (TFile.Exists(Filename)) then
      FSymbolMap.LoadFromFile(Filename)
    else
      FSymbolMap.Clear;

    if (ResourceWriter <> nil) then
      ResourceWriter.BeginWrite;
    try

      if (not Assigned(Translator)) then
        Translator := TProjectResourceProcessor.DefaultTranslator;

      for LocalizerModule in LocalizerProject.Modules.Values.ToArray do
      begin
        if (LocalizerModule.Kind = mkForm) then
        begin
          if (FindResource(Instance, PChar(LocalizerModule.Name), RT_RCDATA) = 0) then
          begin
            LocalizerModule.State := lItemStateUnused;
            continue;
          end;
          Stream := TResourceStream.Create(Instance, LocalizerModule.Name, RT_RCDATA);
        end else
        if (LocalizerModule.Kind = mkString) then
        begin
          if (LocalizerModule.ResourceID = 0) then
            continue;
          if (FindResource(Instance, PChar(LocalizerModule.ResourceID), RT_STRING) = 0) then
          begin
            LocalizerModule.State := lItemStateUnused;
            continue;
          end;
          Stream := TResourceStream.CreateFromID(Instance, LocalizerModule.ResourceID, RT_STRING);
        end else
          continue;
        try

          if (LocalizerModule.Kind = mkForm) then
          begin
            Stream.Read(Signature, SizeOf(Signature));
            if (Signature <> FilerSignature) then
            begin
              LocalizerModule.Kind := mkOther;
              LocalizerModule.Status := lItemStatusDontTranslate;
              continue;
            end;
          end;

          if (LocalizerModule.Kind = mkForm) then
            ModuleProcessor := TModuleDFMResourceProcessor.Create(LocalizerModule)
          else
          if (LocalizerModule.Kind = mkString) then
            ModuleProcessor := TModuleStringResourceProcessor.Create(LocalizerModule, FSymbolMap)
          else
            continue;
          try

            if (ResourceWriter <> nil) then
              TranslatedStream := TMemoryStream.Create
            else
              TranslatedStream := nil;
            try

              Stream.Position := 0;
              if (LocalizerModule.Status = lItemStatusTranslate) then
                ModuleProcessor.Execute(Stream, TranslatedStream, LocaleID, Translator)
              else
              if (ResourceWriter <> nil) and (TranslatedStream <> nil) then
                TranslatedStream.CopyFrom(Stream, 0);

              if (ResourceWriter <> nil) and (TranslatedStream <> nil) then
              begin
                // Write translated resource data
                TranslatedStream.Position := 0;
                ResourceWriter.WriteModule(LocalizerModule, TranslatedStream);
              end;

            finally
              TranslatedStream.Free;
            end;

          finally
            ModuleProcessor.Free;
          end;

        finally
          Stream.Free;
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
end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------


end.
