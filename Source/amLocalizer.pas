unit amLocalizer;

interface

uses
  Generics.Collections,
  Generics.Defaults,
  Winapi.Windows, System.Classes;

type
  TLocalizerModule = class;
  TLocalizerItem = class;
  TLocalizerProperty = class;

  TLocalizerModules = TObjectDictionary<string, TLocalizerModule>;
  TLocalizerItems = TObjectDictionary<string, TLocalizerItem>;
  TLocalizerProperties = TObjectDictionary<string, TLocalizerProperty>;

// -----------------------------------------------------------------------------
//
// TLocalizerProject
//
// -----------------------------------------------------------------------------
  TLocalizerProject = class
  private
    FName: string;
    FModules: TLocalizerModules;
    FBaseLocaleID: LCID;
  public
    constructor Create(const AName: string; ABaseLocaleID: LCID);
    destructor Destroy; override;

    property Name: string read FName write FName;
    property BaseLocaleID: LCID read FBaseLocaleID write FBaseLocaleID;
    property Modules: TLocalizerModules read FModules;
    function AddModule(const AName: string): TLocalizerModule;
  end;

  TLocalizerItemState = (lItemStateNew, lItemStateExisting, lItemStateUnused);
  TLocalizerItemStatus = (lItemStatusNormal, lItemStatusDontTranslate, lItemStatusHold);

// -----------------------------------------------------------------------------
//
// TLocalizerModule
//
// -----------------------------------------------------------------------------
  TLocalizerModule = class
  private
    FProject: TLocalizerProject;
    FName: string;
    FItems: TLocalizerItems;
    FState: TLocalizerItemState;
    FStatus: TLocalizerItemStatus;
  protected
    procedure SetName(const Value: string);
  public
    constructor Create(AProject: TLocalizerProject; const AName: string);
    destructor Destroy; override;

    property Project: TLocalizerProject read FProject;
    property Name: string read FName write SetName;
    property Items: TLocalizerItems read FItems;
    function AddItem(const AName, ATypeName: string): TLocalizerItem;

    property State: TLocalizerItemState read FState write FState;
    property Status: TLocalizerItemStatus read FStatus write FStatus;
  end;

// -----------------------------------------------------------------------------
//
// TLocalizerItem
//
// -----------------------------------------------------------------------------
  TLocalizerItem = class
  private
    FModule: TLocalizerModule;
    FName: string;
    FTypeName: string;
    FProperties: TLocalizerProperties;
    FState: TLocalizerItemState;
    FStatus: TLocalizerItemStatus;
  public
    constructor Create(AModule: TLocalizerModule; const AName, ATypeName: string);
    destructor Destroy; override;

    property Module: TLocalizerModule read FModule;
    property Name: string read FName;
    property TypeName: string read FTypeName write FTypeName;
    property Properties: TLocalizerProperties read FProperties;
    function AddProperty(const AName: string; const AValue: string = ''): TLocalizerProperty;

    property State: TLocalizerItemState read FState write FState;
    property Status: TLocalizerItemStatus read FStatus write FStatus;
  end;

// -----------------------------------------------------------------------------
//
// TLocalizerTranslations
//
// -----------------------------------------------------------------------------
  TTranslationStatus = (tStatusOld, tStatusPending, tStatusProposed, pStatusTranslated);

  TLocalizerTranslation = class
  private
    FValue: string;
    FStatus: TTranslationStatus;
  public
    property Value: string read FValue write FValue;
    property Status: TTranslationStatus read FStatus;
  end;

  TLocalizerTranslations = class
  private
    FTranslations: TDictionary<LCID, TLocalizerTranslation>;
  protected
    function GetItem(LocaleID: LCID): TLocalizerTranslation;
  public
    constructor Create;
    destructor Destroy; override;
    function TryGetTranslation(LocaleID: LCID; var Value: TLocalizerTranslation): boolean;
    function FindTranslation(LocaleID: LCID): TLocalizerTranslation;
    function AddOrUpdateTranslation(LocaleID: LCID; const Value: string): TLocalizerTranslation;
    property Items[LocaleID: LCID]: TLocalizerTranslation read GetItem; default;
  end;

// -----------------------------------------------------------------------------
//
// TLocalizerProperty
//
// -----------------------------------------------------------------------------
  TLocalizerProperty = class
  private
    FItem: TLocalizerItem;
    FName: string;
    FValue: string;
    FTranslations: TLocalizerTranslations;
    FState: TLocalizerItemState;
    FStatus: TLocalizerItemStatus;
  protected
    procedure SetValue(const Value: string);
  public
    constructor Create(AItem: TLocalizerItem; const AName: string);
    destructor Destroy; override;
    property Item: TLocalizerItem read FItem;
    property Name: string read FName;
    property Value: string read FValue write SetValue;

    property State: TLocalizerItemState read FState write FState;
    property Status: TLocalizerItemStatus read FStatus write FStatus;
    property Translations: TLocalizerTranslations read FTranslations;
  end;

// -----------------------------------------------------------------------------
//
// TTranslateProc
//
// -----------------------------------------------------------------------------
type
  TTranslateProc = reference to function(Prop: TLocalizerProperty; var NewValue: string): boolean;

// -----------------------------------------------------------------------------
//
// TModuleProcessorDFMResource
//
// -----------------------------------------------------------------------------
type
  TModuleProcessorDFMResource = class
  private
    FLocalizerModule: TLocalizerModule;
    FReader: TReader;
    FWriter: TWriter;
    FLastCopied: integer;
    FTranslator: TTranslateProc;
  protected
    procedure ReadCollection(LocalizerItem: TLocalizerItem; const Path: string);
    procedure ReadProperty(LocalizerItem: TLocalizerItem; const Path: string);
    procedure ReadComponent(const Path: string);

    procedure CopyToHere(CopyPos: int64);
  public
    constructor Create(ALocalizerModule: TLocalizerModule);

    procedure Execute(ReadStream, WriteStream: TStream; Translator: TTranslateProc);
  end;

// -----------------------------------------------------------------------------
//
// TProjectProcessorDFMResource
//
// -----------------------------------------------------------------------------
type
  TProjectProcessorDFMResource = class
  private
  protected
  public
    procedure Execute(LocalizerProject: TLocalizerProject; Instance: HINST; Translator: TTranslateProc = nil); overload;
    procedure Execute(LocalizerProject: TLocalizerProject; const Filename: string; Translator: TTranslateProc = nil); overload;
  end;


// -----------------------------------------------------------------------------
//
// TModuleImporterXLIFF
//
// -----------------------------------------------------------------------------
type
  TModuleImporterXLIFF = class
  private
  protected
  public
    function LoadFromStream(LocalizerProject: TLocalizerProject; Stream: TStream; const ModuleName: string = ''): TLocalizerModule; overload;
    procedure LoadFromStream(LocalizerModule: TLocalizerModule; Stream: TStream; const ModuleName: string = ''); overload;
    function LoadFromFile(LocalizerProject: TLocalizerProject; const Filename: string): TLocalizerModule; overload;
    procedure LoadFromFile(LocalizerModule: TLocalizerModule; const Filename: string); overload;
  end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  IOUtils,
  Variants,
  System.SysUtils,
  XMLDoc, XMLIntf,
  msxmldom,
  amLocale;

// -----------------------------------------------------------------------------
//
// TTextComparer
//
// -----------------------------------------------------------------------------
type
  TTextComparer = class(TEqualityComparer<string>)
  public
    function GetHashCode(const Value: String): Integer; override;
    function Equals(const Left, Right: string): Boolean; override;
  end;

// -----------------------------------------------------------------------------

function TTextComparer.Equals(const Left, Right: string): Boolean;
begin
  Result := (Length(Left) = Length(Right)) and (AnsiSameText(Left, Right));
end;

function TTextComparer.GetHashCode(const Value: String): Integer;
var
  s: string;
begin
  s := AnsiUppercase(Value);
  Result := BobJenkinsHash(PChar(s)^, SizeOf(Char) * Length(s), 0);
end;


// -----------------------------------------------------------------------------
//
// TLocalizerProject
//
// -----------------------------------------------------------------------------
constructor TLocalizerProject.Create(const AName: string; ABaseLocaleID: LCID);
begin
  inherited Create;
  FModules := TLocalizerModules.Create([doOwnsValues], TTextComparer.Create);
  FName := AName;
  FBaseLocaleID := ABaseLocaleID;
end;

destructor TLocalizerProject.Destroy;
begin
  FModules.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

function TLocalizerProject.AddModule(const AName: string): TLocalizerModule;
begin
  if (not FModules.TryGetValue(AName, Result)) then
    Result := TLocalizerModule.Create(Self, AName);
end;


// -----------------------------------------------------------------------------
//
// TLocalizerModule
//
// -----------------------------------------------------------------------------
constructor TLocalizerModule.Create(AProject: TLocalizerProject; const AName: string);
begin
  inherited Create;
  FItems := TLocalizerItems.Create([doOwnsValues], TTextComparer.Create);
  FProject := AProject;
  FName := AName;
  FProject.Modules.Add(FName, Self);
end;

destructor TLocalizerModule.Destroy;
begin
  FProject.Modules.ExtractPair(FName);
  FItems.Free;
  inherited;
end;

procedure TLocalizerModule.SetName(const Value: string);
begin
  if (FName = Value) then
    exit;

  FProject.Modules.ExtractPair(FName);
  FName := Value;
  FProject.Modules.Add(FName, Self);
end;

// -----------------------------------------------------------------------------

function TLocalizerModule.AddItem(const AName, ATypeName: string): TLocalizerItem;
begin
  if (not FItems.TryGetValue(AName, Result)) then
    Result := TLocalizerItem.Create(Self, AName, ATypeName);
end;


// -----------------------------------------------------------------------------
//
// TLocalizerItem
//
// -----------------------------------------------------------------------------
constructor TLocalizerItem.Create(AModule: TLocalizerModule; const AName, ATypeName: string);
begin
  inherited Create;
  FProperties := TLocalizerProperties.Create([doOwnsValues], TTextComparer.Create);
  FModule := AModule;
  FName := AName;
  FTypeName := ATypeName;
  FModule.Items.Add(FName, Self);
end;

destructor TLocalizerItem.Destroy;
begin
  FModule.Items.ExtractPair(FName);
  FProperties.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

function TLocalizerItem.AddProperty(const AName, AValue: string): TLocalizerProperty;
begin
  if (not FProperties.TryGetValue(AName, Result)) then
    Result := TLocalizerProperty.Create(Self, AName);
  Result.Value := AValue;
end;


// -----------------------------------------------------------------------------
//
// TLocalizerProperty
//
// -----------------------------------------------------------------------------
constructor TLocalizerProperty.Create(AItem: TLocalizerItem; const AName: string);
begin
  inherited Create;
  FItem := AItem;
  FName := AName;
  FItem.Properties.Add(FName, Self);
  FTranslations := TLocalizerTranslations.Create;
end;

destructor TLocalizerProperty.Destroy;
begin
  FItem.Properties.ExtractPair(FName);
  FTranslations.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerProperty.SetValue(const Value: string);
begin
  if (FValue = Value) then
    exit;
  FValue := Value;
end;


// -----------------------------------------------------------------------------
//
// TLocalizerTranslations
//
// -----------------------------------------------------------------------------
function TLocalizerTranslations.AddOrUpdateTranslation(LocaleID: LCID; const Value: string): TLocalizerTranslation;
begin
  if (not FTranslations.TryGetValue(LocaleID, Result)) then
  begin
    Result := TLocalizerTranslation.Create;
    FTranslations.Add(LocaleID, Result);
    Result.Value := Value;
  end;
end;

constructor TLocalizerTranslations.Create;
begin
  inherited Create;
  FTranslations := TObjectDictionary<LCID, TLocalizerTranslation>.Create([doOwnsValues]);
end;

destructor TLocalizerTranslations.Destroy;
begin
  FTranslations.Free;
  inherited;
end;

function TLocalizerTranslations.FindTranslation(LocaleID: LCID): TLocalizerTranslation;
begin
  if (not FTranslations.TryGetValue(LocaleID, Result)) then
    Result := nil;
end;

function TLocalizerTranslations.GetItem(LocaleID: LCID): TLocalizerTranslation;
begin
  Result := FTranslations[LocaleID];
end;

function TLocalizerTranslations.TryGetTranslation(LocaleID: LCID; var Value: TLocalizerTranslation): boolean;
begin
  Result := FTranslations.TryGetValue(LocaleID, Value);
end;

// -----------------------------------------------------------------------------
//
// TProjectProcessorDFMResource
//
// -----------------------------------------------------------------------------
procedure TProjectProcessorDFMResource.Execute(LocalizerProject: TLocalizerProject; const Filename: string; Translator: TTranslateProc);
var
  Module: HModule;
begin
  Module := LoadLibraryEx(PChar(FileName), 0, LOAD_LIBRARY_AS_DATAFILE);
  if (Module = 0) then
    RaiseLastOSError;
  try

    Execute(LocalizerProject, Module, Translator);

  finally
    FreeLibrary(Module);
  end;
end;

function EnumResourceNamesProc(Module: HMODULE; ResType, ResName: PChar; LocalizerProject: TLocalizerProject): BOOL; stdcall;
begin
  LocalizerProject.AddModule(ResName);

  Result := True;
end;

procedure TProjectProcessorDFMResource.Execute(LocalizerProject: TLocalizerProject; Instance: HINST; Translator: TTranslateProc);
var
  LocalizerModule: TLocalizerModule;
  Stream: TStream;
  TranslatedStream: TStream;
  DFMStream: TStream;
  Signature: UInt32;
  ModuleProcessor: TModuleProcessorDFMResource;
const
  FilerSignature: UInt32 = $30465054; // ($54, $50, $46, $30) 'TPF0'
begin
  EnumResourceNames(Instance, RT_RCDATA, @EnumResourceNamesProc, integer(LocalizerProject));

  for LocalizerModule in LocalizerProject.Modules.Values.ToArray do
  begin
    Stream := TResourceStream.Create(Instance, LocalizerModule.Name, RT_RCDATA);
    try

      Stream.Read(Signature, SizeOf(Signature));
      if (Signature <> FilerSignature) then
        continue;

      Stream.Position := 0;

      ModuleProcessor := TModuleProcessorDFMResource.Create(LocalizerModule);
      try

        TranslatedStream := TMemoryStream.Create;
        try

          ModuleProcessor.Execute(Stream, TranslatedStream, Translator);

          DFMStream := TFileStream.Create(Format('localized\%s.dfm', [LocalizerModule.Name]), fmCreate);
          try

            TranslatedStream.Position := 0;

            ObjectBinaryToText(TranslatedStream, DFMStream);

          finally
            DFMStream.Free;
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
end;


// -----------------------------------------------------------------------------
//
// TModuleProcessorDFMResource
//
// -----------------------------------------------------------------------------
constructor TModuleProcessorDFMResource.Create(ALocalizerModule: TLocalizerModule);
begin
  inherited Create;
  FLocalizerModule := ALocalizerModule;
end;

// -----------------------------------------------------------------------------

procedure TModuleProcessorDFMResource.CopyToHere(CopyPos: int64);
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

procedure TModuleProcessorDFMResource.Execute(ReadStream, WriteStream: TStream; Translator: TTranslateProc);
begin
  Assert(ReadStream <> nil);

  FReader := TReader.Create(ReadStream, 1024);
  try
    if (WriteStream <> nil) then
      FWriter := TWriter.Create(WriteStream, 1024)
    else
      FWriter := nil;
    try

      FReader.ReadSignature;

      FTranslator := Translator;
      try

        ReadComponent('');

      finally
        FTranslator := nil;
      end;

      CopyToHere(ReadStream.Size);

    finally
      FreeAndNil(FWriter);
    end;
  finally
    FreeAndNil(FReader);
  end;
end;

// -----------------------------------------------------------------------------

procedure TModuleProcessorDFMResource.ReadCollection(LocalizerItem: TLocalizerItem; const Path: string);
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
    CollectionLocalizerItem := FLocalizerModule.AddItem(Name, LocalizerItem.TypeName);

    FReader.ReadListBegin;
    while (not FReader.EndOfList) do
      ReadProperty(CollectionLocalizerItem, Name);
    FReader.ReadListEnd;
    Inc(Index);
  end;
  FReader.ReadListEnd;
end;

// -----------------------------------------------------------------------------

procedure TModuleProcessorDFMResource.ReadComponent(const Path: string);
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

  LocalizerItem := FLocalizerModule.AddItem(Name, ClassName);

  // TReader.ReadDataInner
  while (not FReader.EndOfList) do
    ReadProperty(LocalizerItem, Name);
  FReader.ReadListEnd;

  while (not FReader.EndOfList) do
    ReadComponent(Name);
  FReader.ReadListEnd;
end;

// -----------------------------------------------------------------------------

procedure TModuleProcessorDFMResource.ReadProperty(LocalizerItem: TLocalizerItem; const Path: string);
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
      ReadCollection(LocalizerItem, Path+'.'+PropertyName);
    end;

  vaWString, vaUTF8String, vaString, vaLString:
    begin
      LocalizerProperty := LocalizerItem.AddProperty(PropertyName);

      StartPos := FReader.Position;

      // TReader.ReadPropValue
      LocalizerProperty.Value := FReader.ReadString;

      EndPos := FReader.Position;

      // Perform translation
      if (Assigned(FTranslator)) then
      begin
        NewValue := LocalizerProperty.Value;
        if (FTranslator(LocalizerProperty, NewValue)) and (NewValue <> LocalizerProperty.Value) and (FWriter <> nil) then
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
//              TFixedStreamAdapter
//
// -----------------------------------------------------------------------------
(*
type
  TFixedStreamAdapter = class(TStreamAdapter)
  public
    function Write(pv: Pointer; cb: Longint; pcbWritten: PLongint): HResult; override; stdcall;
    function Read(pv: Pointer; cb: Longint; pcbRead: PLongint): HResult; override; stdcall;
  end;

// -----------------------------------------------------------------------------

function TFixedStreamAdapter.Read(pv: Pointer; cb: Integer; pcbRead: PLongint): HResult;
var
  NumRead: Longint;
begin
  try
    if (pv = Nil) then
      Exit(STG_E_INVALIDPOINTER);

    NumRead := Stream.Read(pv^, cb);

    if (pcbRead <> Nil) then
      pcbRead^ := NumRead;

    Result := S_OK;
  except
    on E: EOutOfMemory do
      Result := E_OUTOFMEMORY;
    on E: EAbort do
      Result := E_ABORT;
  else
    Result := S_FALSE;
  end;
end;

// -----------------------------------------------------------------------------

function TFixedStreamAdapter.Write(pv: Pointer; cb: Integer; pcbWritten: PLongint): HResult;
var
  NumWritten: Longint;
begin
  try
    if (pv = Nil) then
      Exit(STG_E_INVALIDPOINTER);

    NumWritten := Stream.Write(pv^, cb);

    if (pcbWritten <> Nil) then
      pcbWritten^ := NumWritten;

    Result := S_OK;
  except
    on E: EOutOfMemory do
      Result := E_OUTOFMEMORY;

    on E: EAbort do
      Result := E_ABORT;
  else
    Result := STG_E_CANTSAVE;
  end;
end;
*)

// -----------------------------------------------------------------------------
//
// TModuleImporterXLIFF
//
// -----------------------------------------------------------------------------
function TModuleImporterXLIFF.LoadFromFile(LocalizerProject: TLocalizerProject; const Filename: string): TLocalizerModule;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(LocalizerProject, Stream, TPath.GetFileNameWithoutExtension(Filename));
  finally
    Stream.Free;
  end;
end;

procedure TModuleImporterXLIFF.LoadFromFile(LocalizerModule: TLocalizerModule; const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(LocalizerModule, Stream);
  finally
    Stream.Free;
  end;
end;

function TModuleImporterXLIFF.LoadFromStream(LocalizerProject: TLocalizerProject; Stream: TStream; const ModuleName: string): TLocalizerModule;
begin
  Result := LocalizerProject.AddModule(ModuleName);
  try

    LoadFromStream(Result, Stream);

  except
    Result.Free;
    raise;
  end;
end;

  (*
  ** status:
  ** 0          Untranslated
  ** 1          Translated
  ** 2          Auto translated
  ** 3          Unused
  ** 4          Newly translated
  ** 5          Hold
  ** 6          Don't translate
  ** 7          Egged
  *)
type
  TETMTranslationStatus = (tsUntranslated, tsTranslated, tsAutoTranslated, tsUnused, tsNewlyTranslated, tsHold, tsDontTranslate, tsEgged, tsFinal, tsApproved, tsUnknown);

function EscapeChar(c: Char; IsUnicode: boolean): string;
begin
//  if (TCharacter.GetUnicodeCategory(c) in [TUnicodeCategory.ucControl, TUnicodeCategory.ucUnassigned, TUnicodeCategory.ucPrivateUse]) then
    Result := Format('\x%.4X', [Ord(c)]);
//  else
//    Result := c;
end;

const
  UnicodePrefix = 'L';

type
  SetOfChar = set of char;

function Escape(const s: string; Quote: boolean = False; DontEscape: SetOfChar = []; ForceUnicode: boolean = False): string;
var
  i: integer;
  IsUnicode: boolean;
  p: PChar;
  Lines: integer;
  Chunk: string;
begin
  IsUnicode := ForceUnicode;
  p := PChar(s);
  for i := 1 to Length(s) do
  begin
    if (Ord(p^) > 127) then
    begin
      IsUnicode := True;
      break;
    end;
    inc(p);
  end;

  Result := '';
  if (IsUnicode) then
    Result := Result + UnicodePrefix;

  if (Quote) then
    Result := Result + '"';

  Lines := 1;
  p := PChar(s);
  for i := 1 to Length(s) do
  begin
    if (p^ in DontEscape) then
      Chunk := p^
    else
    case integer(p^) of
      0: Chunk := '\0';
      8: Chunk := '\a'; // Not standard
      9: Chunk := '\t';
      10: Chunk := '\n';
      13: Chunk := '\r';
      34: Chunk := '""'; // "
      92: Chunk := '\\'; // \
      32..33, 35..91, 93..127: Chunk := p^;
    else
      Chunk := EscapeChar(p^, IsUnicode);
    end;
    inc(p);

    // Max length of single string must be 4097
    // Split long strings with continuation character: \
    // We split at 4000 to make room for margin and end delimiter. Not the best
    // solution but "good enough".
    if ((Length(Result) + Length(Chunk)) div Lines > 4000) then
    begin
      Result := Result + '\'+#13#10;
      inc(Lines);
    end;
    Result := Result + Chunk;
  end;

  if (Quote) then
    Result := Result + '"';
end;

function Unescape(const Value: string): string;
var
  p: PChar;
  LastChar: char;
  Escaped: boolean;
  s: string;
  First: boolean;

  procedure Push(c: char);
  begin
    Result := Result + c;
    LastChar := #0;
    Escaped := False;
  end;

  procedure Flush;
  begin
    Result := Result + LastChar;
    LastChar := #0;
    Escaped := False;
  end;

  procedure Purge;
  begin
    LastChar := #0;
    Escaped := False;
  end;

  function Fetch: char;
  begin
    if (p^ <> #0) then
      inc(p);
    Result := p^;
  end;

begin
  Result := '';
  p := PChar(Value);

  // Skip initial 'L'
  if (p^ <> #0) and (p^ = 'L') then
    inc(p);

  Purge;
  First := True;

  while (p^ <> #0) do
  begin
    if (Escaped) then
    begin
      if (p^ = 'x') then
      begin
        s := '$'+Fetch;
        s := s + Fetch;
        s := s + Fetch;
        s := s + Fetch;
        Push(Char(StrToInt(s)));
      end else
      if (p^ = 'r') then
        Push(#13)
      else
      if (p^ = 'n') then
        Push(#10)
      else
      if (p^ = 't') then
        Push(#9)
      else
        Push(p^);
    end else
    if (p^ = '"') then
    begin
      if (First) then
        Purge // just ignore it
      else
      if (LastChar = '"') then
        Flush
      else
        LastChar := p^;
    end else
    if (p^ = '\') then
      Escaped := True
    else
      Push(p^);

    First := False;
    inc(p);
  end;
end;

procedure TModuleImporterXLIFF.LoadFromStream(LocalizerModule: TLocalizerModule; Stream: TStream; const ModuleName: string);
var
  LocalizerItem: TLocalizerItem;
  LocalizerProperty: TLocalizerProperty;
  XML: IXMLDocument;
  Header, HeaderProps: IXMLNode;
  Body: IXMLNode;
  Node, NextNode: IXMLNode;
  Child: IXMLNode;
  TargetNode: IXMLNode;
  PropChild, NextPropChild: IXMLNode;
  s: string;
  i: integer;
  SourceLanguage, TargetLanguage: string;
  SourceLocaleID, TargetLocaleID: LCID;
  LocaleItem: TLocaleItem;
  SourceValue, TargetValue: string;
  Value: string;
  NewValue: string;
  FixCount: integer;
  OldTranslationStatus, TranslationStatus: TETMTranslationStatus;
  CountTranslated, CountUntranslated: integer;
  Localize: boolean;
  RemoveNode: boolean;
  ItemName, ItemType, PropertyName, PropertyValue: string;
begin
  XML := TXMLDocument.Create(nil);
  XML.Options := [doNodeAutoIndent];
  XML.Active := True;

  XML.LoadFromStream(Stream);

  FixCount := 0;
  CountTranslated := 0;
  CountUntranslated := 0;

  if (XML.DocumentElement.NodeName <> 'xliff') then
    raise Exception.CreateFmt('XML document root node is not named "xliff": %s', [XML.DocumentElement.NodeName]);

  Node := XML.DocumentElement.ChildNodes.FindNode('file');
  if (Node = nil) then
    raise Exception.Create('xliff node not found: xliff\file');

  Value := Node.Attributes['original'];
  if (Value <> '') then
  begin
    // Remove path from filename
    NewValue := TPath.GetFileName(Value);
    if (Value <> NewValue) then
    begin
      Node.Attributes['original'] := NewValue;
      Inc(FixCount);
    end;
    NewValue := TPath.GetFileNameWithoutExtension(NewValue);
    if (not AnsiSameText(NewValue, LocalizerModule.Name)) then
      LocalizerModule.Name := NewValue;
  end else
  if (LocalizerModule.Name <> '') then
  begin
    // Fix empty filename (caused by ETM)
    Node.Attributes['original'] := TPath.ChangeExtension(LocalizerModule.Name, '.xliff');
    Inc(FixCount);
  end;

  Value := Node.Attributes['ts'];
  if (Value <> '') then
  begin
    // Remove path from filename
    NewValue := TPath.GetFileName(Value);
    if (Value <> NewValue) then
    begin
      Node.Attributes['ts'] := NewValue;
      Inc(FixCount);
    end;

    if (LocalizerModule.Name = '') then
      LocalizerModule.Name := TPath.GetFileNameWithoutExtension(NewValue);
  end else
  if (LocalizerModule.Name <> '') then
  begin
    // Fix empty filename (caused by ETM)
    Node.Attributes['ts'] := TPath.ChangeExtension(LocalizerModule.Name, '.xliff');
    Inc(FixCount);
  end;

  SourceLanguage := Node.Attributes['source-language'];
  TargetLanguage := Node.Attributes['target-language'];

  LocaleItem := TLocaleItems.FindISO639_1Name(SourceLanguage);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindISO3166Name(SourceLanguage);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindLanguageShortName(SourceLanguage);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindLanguageName(SourceLanguage);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindCountry(SourceLanguage);

  if (LocaleItem <> nil) then
    SourceLocaleID := LocaleItem.Locale
  else
    SourceLocaleID := 0;

  LocaleItem := TLocaleItems.FindISO639_1Name(TargetLanguage);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindISO3166Name(TargetLanguage);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindLanguageShortName(TargetLanguage);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindLanguageName(TargetLanguage);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindCountry(TargetLanguage);

  if (LocaleItem <> nil) then
    TargetLocaleID := LocaleItem.Locale
  else
    TargetLocaleID := 0;

  if (LocalizerModule.Project.BaseLocaleID = 0) then
    LocalizerModule.Project.BaseLocaleID := SourceLocaleID
  else
  if (SourceLanguage = '') then
    Node.Attributes['source-language'] := TLocaleItems.FindLCID(LocalizerModule.Project.BaseLocaleID).LanguageShortName; // TODO

(*
  if (LocalizerModule.Project.TargetLanguage = '') then
    LocalizerModule.Project.TargetLanguage := TargetLanguage
  else
  if (TargetLanguage = '') then
    Node.Attributes['target-language'] := LocalizerModule.Project.TargetLanguage;
*)

  // TODO : Validation that module languages matches project

  Header := Node.ChildNodes.FindNode('header');

  Body := Node.ChildNodes.FindNode('body');
  if (Body = nil) then
    raise Exception.Create('xliff node not found: xliff\file\body');

  Node := Body.ChildNodes.First;
  while (Node <> nil) do
  begin
    RemoveNode := False;
    Localize := True;
    if (Node.NodeName = 'trans-unit') then
    begin
      if (Node.Attributes['translate'] = 'no') then
        Localize := False;

      s := VarToStr(Node.Attributes['resname']);
      if (s <> '') then
      begin
        // Build a property path from the resname value.
        // Old: I:FormSearch.I:PanelMain.O:PageControl.O:TabSheetTextSearch.O:TopPanel.O:Bevel1.Properties.LineOptions.Visible
        // New: FormSearch\PanelMain\PageControl\TabSheetTextSearch\TopPanel\Bevel1.Properties.LineOptions.Visible
        // Item name: FormSearch\PanelMain\PageControl\TabSheetTextSearch\TopPanel\Bevel1
        // Property name: Properties.LineOptions.Visible
        i := 1;
        while (i < Length(s)-1) do
        begin
          if (s[i] = 'I') or (s[i] = 'O') then
          begin
            if (s[i+1] = ':') then
            begin
              if (i > 1) and (s[i-1] = '.') then
              begin
                Dec(i);
                Delete(s, i, 1);
              end;
              Delete(s, i, 1);
              s[i] := '\';
            end;
          end;
          inc(i);
        end;
        Node.Attributes['resname'] := s;

        // Property name starts at first '.'
        i := Pos('.', s);
        if (i > 0) then
        begin
          ItemName := Copy(s, 1, i-1);
          PropertyName := Copy(s, i+1);
        end else
        begin
          ItemName := s;
          PropertyName := '';
        end;
        if (ItemName = '') then
          ItemName := 'item';
        if (propertyName = '') then
          propertyName := 'value';

        ItemType := '';


        // Remove <alt-trans>: Old translation
        Child := Node.ChildNodes.FindNode('alt-trans');
        while (Child <> nil) do // There can be more than one
        begin
          Inc(FixCount);
          Node.ChildNodes.Remove(Child);
          Child := Node.ChildNodes.FindNode('alt-trans');
        end;

        // Get Source value
        Child := Node.ChildNodes.FindNode('source');
        if (Child <> nil) then
        begin
          SourceValue := Unescape(Child.Text);
          // Set source language
          s := Child.Attributes['xml:lang'];
          if (s = '') and (SourceLanguage <> '') then
            Child.Attributes['xml:lang'] := SourceLanguage;
        end else
          SourceValue := '';

        // Get Target value
        TranslationStatus := tsUnknown;
        TargetNode := Node.ChildNodes.FindNode('target');
        if (TargetNode <> nil) then
        begin
          TargetValue := Unescape(TargetNode.Text);

          // Set target language
          s := TargetNode.Attributes['xml:lang'];
          if (s = '') and (TargetLanguage <> '') then
            TargetNode.Attributes['xml:lang'] := TargetLanguage;

          Value := TargetNode.Attributes['state'];
          if (Value = 'final') then
            TranslationStatus := tsFinal
          else
          if (Value = 'translated') then
            TranslationStatus := tsTranslated
          else
          if (Value = 'signed-off') then
            TranslationStatus := tsApproved
          else
          if (Value = '') or (Value = 'new') then
            TranslationStatus := tsUntranslated
          else
          if (Value = 'x-ignore') then
            TranslationStatus := tsDontTranslate
          else
          if (Value = 'x-unused') then
            TranslationStatus := tsUnused
          else
          if (Value = 'x-hold') then
            TranslationStatus := tsHold
          else
            TranslationStatus := tsUnknown;
        end else
          TargetValue := '';

        Child := Node.ChildNodes.FindNode('prop-group');

        if (Child <> nil) then
        begin
          PropChild := Child.ChildNodes.First;
          while (PropChild <> nil) do
          begin
            NextPropChild := PropChild.NextSibling;
            Value := PropChild.Attributes['prop-type'];
            if (Value = 'Created') then
            begin
              // Use static timestamp
              if (PropChild.Text <> '41356') then
              begin
                PropChild.Text := '41356';
                Inc(FixCount);
              end;
            end else
            if (Value = 'Order') then
            begin
              // Use static order
              if (PropChild.Text <> '0') then
              begin
                PropChild.Text := '0';
                Inc(FixCount);
              end;
              // Child.ChildNodes.Remove(PropChild);
            end else
            if (Value = 'Type') then
            begin
              ItemType := PropChild.Text;
            end else
            if (Value = 'Localize') then
            begin
              // Ignore item if Localize=0
              if (PropChild.Text = '0') then
                Localize := False;
            end else
            if (Value = 'Status') and (TranslationStatus = tsUnknown) then
            begin
              OldTranslationStatus := TETMTranslationStatus(StrToIntDef(PropChild.Text, 0));
              TranslationStatus := OldTranslationStatus;

              // Change variations of Translated
              if (TranslationStatus in [tsAutoTranslated, tsNewlyTranslated]) then
              begin
                TranslationStatus := tsTranslated;
              end else
              // Change "Untranslated" to "Don't translate" for non-string values
              // String in .dfn are ' delimited. Strings in .rcn are " delimited.
              if (TranslationStatus = tsUntranslated) then
              begin
                if (Length(SourceValue) < 2) or (not(SourceValue[1] in ['"', ''''])) then
                  TranslationStatus := tsDontTranslate;
              end;

(*
  Unfortunately removing properties from the DFN causes the Localization Editor to crash.

  Update Localized Projects can reconstruct the original (verbose) DFN by merging the DFN and the unlocalized DFM, so
  if the Update Localized Projects step is performed first then the DFN can be opened in the Localization Editor.
  Unfortunately this risks corrupting the localization strings as we need SigmaLocalizer to escape singlequotes before
  Localization Editor is used. Catch 22. Game over.

              if (TranslationStatus = tsDontTranslate) then
              begin
                if (Length(SourceValue) < 2) or (not(SourceValue[1] in ['"', ''''])) then
                begin
                  RemoveNode := True;
                  TranslationStatus := tsEgged; // So we don't count it below
                  break;
                end;
              end;
*)

              if (PropChild <> nil) and (TranslationStatus <> OldTranslationStatus) then
              begin
                Inc(FixCount);
                PropChild.Text := IntToStr(Ord(TranslationStatus));
              end;
            end;
            PropChild := NextPropChild;
          end;

          // Count status so we can update counters
          if (not RemoveNode) and (Localize) then
          begin
            if (TargetNode <> nil) and (TranslationStatus in [tsUntranslated, tsDontTranslate]) then
            begin
              Inc(FixCount);
              Node.ChildNodes.Remove(TargetNode);
            end;

            if (TranslationStatus in [tsTranslated, tsAutoTranslated, tsNewlyTranslated, tsDontTranslate]) then
              Inc(CountTranslated)
            else
            if (TranslationStatus in [tsUntranslated, tsHold]) then
              Inc(CountUntranslated);
          end;
        end;

      end else
        Localize := False;


      if (Localize) then
      begin
        LocalizerItem := LocalizerModule.AddItem(ItemName, ItemType);
        LocalizerProperty := LocalizerItem.AddProperty(PropertyName, SourceValue);
        if (TargetNode <> nil) then
          LocalizerProperty.Translations.AddOrUpdateTranslation(TargetLocaleID, TargetValue);
      end;
    end;

    NextNode := Node.NextSibling;

    if (RemoveNode) and (Localize) then
    begin
      Inc(FixCount);
      Body.ChildNodes.Remove(Node);
    end;

    Node := NextNode;
  end;

  if (Header <> nil) then
  begin
    HeaderProps := Header.ChildNodes.FindNode('prop-group');

    if (HeaderProps <> nil) then
    begin
      PropChild := HeaderProps.ChildNodes.First;
      while (PropChild <> nil) do
      begin
        NextPropChild := PropChild.NextSibling;
        Value := PropChild.Attributes['prop-type'];
        if (Value = 'SelfDateTime') or (Value = 'OrigDateTime') or (Value = 'XlatDateTime') then
        begin
          // Use static timestamp
          if (PropChild.Text <> '41356') then
          begin
            PropChild.Text := '41356';
            Inc(FixCount);
          end;
        end else
        if (Value = 'Translated') then
        begin
          // Update status counters
          NewValue := IntToStr(CountTranslated);
          if (PropChild.Text <> NewValue) then
          begin
            PropChild.Text := NewValue;
            Inc(FixCount);
          end;
        end else
        if (Value = 'UnTranslated') then
        begin
          // Update status counters
          NewValue := IntToStr(CountUntranslated);
          if (PropChild.Text <> NewValue) then
          begin
            PropChild.Text := NewValue;
            Inc(FixCount);
          end;
        end;
        PropChild := NextPropChild;
      end;
    end;
  end;

//  if (FixCount > 0) then
//    LogLine(Format('%s: Fixed properties: %d', [ExtractFilename(Filename), FixCount]));

//  if (FForceSave) then
//    LogLine(Format('Saving %s', [ExtractFilename(Filename)]));

//  if ((FixCount > 0) or (FForceSave)) and (not FTest) then
//    WriteDocumentToFile(TMSDOMDocument(XML.DOMDocument).MSDocument, Filename);
end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

end.
