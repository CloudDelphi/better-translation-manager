unit amLocalization.Model;

interface

uses
  Generics.Collections,
  Generics.Defaults,
  Winapi.Windows, System.Classes;

type
  TLocalizerModule = class;
  TLocalizerItem = class;
  TLocalizerProperty = class;
  TLocalizerTranslation = class;

  TLocalizerModules = TObjectDictionary<string, TLocalizerModule>;
  TLocalizerItems = TObjectDictionary<string, TLocalizerItem>;
  TLocalizerProperties = TObjectDictionary<string, TLocalizerProperty>;

  TLocalizerModuleDelegate = reference to function(Item: TLocalizerModule): boolean;
  TLocalizerItemDelegate = reference to function(Item: TLocalizerItem): boolean;
  TLocalizerPropertyDelegate = reference to function(Prop: TLocalizerProperty): boolean;
  TLocalizerTranslationDelegate = reference to function(Prop: TLocalizerProperty; LocaleID: LCID; Translation: TLocalizerTranslation): boolean;

  TLocalizerItemState = (lItemStateNew, lItemStateExisting, lItemStateUnused);
  TLocalizerItemStatus = (lItemStatusTranslate, lItemStatusDontTranslate, lItemStatusHold);

  TLocalizerModuleKind = (mkOther, mkForm, mkString);
  TLocalizerModuleKinds = set of TLocalizerModuleKind;

// -----------------------------------------------------------------------------
//
// TLocalizerProject
//
// -----------------------------------------------------------------------------
  TLocalizerProjectState = (ProjectStateLoading);
  TLocalizerProjectStates = set of TLocalizerProjectState;

  TLocalizerProject = class
  private
    FName: string;
    FModules: TLocalizerModules;
    FBaseLocaleID: LCID;
    FState: TLocalizerProjectStates;
    FLoadCount: integer;
    FSourceFilename: string;
  protected
    procedure SetItemState(State: TLocalizerItemState);
  public
    constructor Create(const AName: string; ABaseLocaleID: LCID);
    destructor Destroy; override;

    property State: TLocalizerProjectStates read FState;

    property Name: string read FName write FName;
    property SourceFilename: string read FSourceFilename write FSourceFilename;
    property BaseLocaleID: LCID read FBaseLocaleID write FBaseLocaleID;
    property Modules: TLocalizerModules read FModules;

    procedure Clear;
    function AddModule(const AName: string): TLocalizerModule;

    procedure BeginLoad;
    procedure EndLoad;

    function Traverse(Delegate: TLocalizerModuleDelegate): boolean; overload;
    function Traverse(Delegate: TLocalizerItemDelegate): boolean; overload;
    function Traverse(Delegate: TLocalizerPropertyDelegate; Kinds: TLocalizerModuleKinds = [mkForm, mkString]): boolean; overload;
  end;

// -----------------------------------------------------------------------------
//
// TCustomLocalizerItem
//
// -----------------------------------------------------------------------------
  TCustomLocalizerItem = class abstract
  private
    FName: string;
    FState: TLocalizerItemState;
    FStatus: TLocalizerItemStatus;
  protected
    procedure SetName(const Value: string); virtual;
    function GetState: TLocalizerItemState; virtual;
    function GetStatus: TLocalizerItemStatus; virtual;
    function GetInheritParentState: boolean; virtual;
    function GetInheritParentStatus: boolean; virtual;
  public
    constructor Create(const AName: string);

    property Name: string read FName write SetName;

    procedure Clear; virtual; abstract;

    function Traverse(Delegate: TLocalizerPropertyDelegate): boolean; virtual; abstract;

    property State: TLocalizerItemState read GetState write FState;
    property Status: TLocalizerItemStatus read GetStatus write FStatus;
    property InheritParentState: boolean read GetInheritParentState;
    property InheritParentStatus: boolean read GetInheritParentStatus;
  end;

// -----------------------------------------------------------------------------
//
// TCustomLocalizerChildItem
//
// -----------------------------------------------------------------------------
  TCustomLocalizerChildItem<TParentClass: TCustomLocalizerItem> = class abstract(TCustomLocalizerItem)
  private
    FParent: TParentClass;
  protected
    function GetParent: TParentClass;
  protected
    function GetState: TLocalizerItemState; override;
    function GetStatus: TLocalizerItemStatus; override;
    function GetInheritParentState: boolean; override;
    function GetInheritParentStatus: boolean; override;
    property Parent: TParentClass read GetParent;
  public
    constructor Create(AParent: TParentClass; const AName: string);
  end;

// -----------------------------------------------------------------------------
//
// TLocalizerModule
//
// -----------------------------------------------------------------------------
  TLocalizerModule = class(TCustomLocalizerItem)
  private
    FProject: TLocalizerProject;
    FKind: TLocalizerModuleKind;
    FItems: TLocalizerItems;
    FResourceGroups: TList<Word>;
  protected
    procedure SetName(const Value: string); override;
  public
    constructor Create(AProject: TLocalizerProject; const AName: string);
    destructor Destroy; override;

    property Project: TLocalizerProject read FProject;
    property Kind: TLocalizerModuleKind read FKind write FKind;
    property Items: TLocalizerItems read FItems;

    procedure Clear; override;

    function AddItem(const AName, ATypeName: string): TLocalizerItem; overload;
    function AddItem(AResourceID: Word; const ATypeName: string): TLocalizerItem; overload;

    property ResourceGroups: TList<Word> read FResourceGroups;

    function Traverse(Delegate: TLocalizerItemDelegate): boolean; reintroduce; overload;
    function Traverse(Delegate: TLocalizerPropertyDelegate): boolean; overload; override;
  end;

// -----------------------------------------------------------------------------
//
// TLocalizerItem
//
// -----------------------------------------------------------------------------
  TLocalizerItem = class(TCustomLocalizerChildItem<TLocalizerModule>)
  private
    FResourceID: WORD;
    FTypeName: string;
    FProperties: TLocalizerProperties;
  protected
    procedure SetName(const Value: string); override;
  public
    constructor Create(AModule: TLocalizerModule; const AName, ATypeName: string);
    destructor Destroy; override;

    property Module: TLocalizerModule read GetParent;
    property ResourceID: WORD read FResourceID write FResourceID;
    property TypeName: string read FTypeName write FTypeName;
    property Properties: TLocalizerProperties read FProperties;

    procedure Clear; override;

    function AddProperty(const AName: string; const AValue: string = ''): TLocalizerProperty;

    function Traverse(Delegate: TLocalizerPropertyDelegate): boolean; override;
  end;


// -----------------------------------------------------------------------------
//
// TLocalizerTranslations
//
// -----------------------------------------------------------------------------
  TTranslationStatus = (
    tStatusObsolete,            // Source value has changed. Translation is obsolete.
    tStatusPending,             // Translation has not been performed.
    tStatusProposed,            // Translation has been proposed
    tStatusTranslated           // Translation complete
    );

  TLocalizerTranslation = class
  private
    FValue: string;
    FStatus: TTranslationStatus;
  public
    property Value: string read FValue write FValue;
    property Status: TTranslationStatus read FStatus write FStatus;
  end;

  TLocalizerTranslations = class
  private
    FTranslations: TDictionary<LCID, TLocalizerTranslation>;
  protected
    function GetItem(LocaleID: LCID): TLocalizerTranslation;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function TryGetTranslation(LocaleID: LCID; var Value: TLocalizerTranslation): boolean;
    function FindTranslation(LocaleID: LCID): TLocalizerTranslation;
    function AddOrUpdateTranslation(LocaleID: LCID; const Value: string; Status: TTranslationStatus = tStatusProposed): TLocalizerTranslation;
    procedure Remove(LocaleID: LCID);

    property Items[LocaleID: LCID]: TLocalizerTranslation read GetItem; default;
  end;

// -----------------------------------------------------------------------------
//
// TLocalizerProperty
//
// -----------------------------------------------------------------------------
  TLocalizerProperty = class(TCustomLocalizerChildItem<TLocalizerItem>)
  private
    FValue: string;
    FTranslations: TLocalizerTranslations;
  protected
    procedure SetValue(const Value: string);
    function GetTranslatedValue(LocaleID: LCID): string;
    procedure SetTranslatedValue(LocaleID: LCID; const Value: string);
  public
    constructor Create(AItem: TLocalizerItem; const AName: string);
    destructor Destroy; override;

    procedure Clear; override;

    function Traverse(Delegate: TLocalizerPropertyDelegate): boolean; overload; override;
    function Traverse(Delegate: TLocalizerTranslationDelegate): boolean; reintroduce; overload;

    property Item: TLocalizerItem read GetParent;
    property Value: string read FValue write SetValue;
    property TranslatedValue[LocaleID: LCID]: string read GetTranslatedValue write SetTranslatedValue;

    property Translations: TLocalizerTranslations read FTranslations;
  end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  IOUtils,
  Variants,
  System.SysUtils,
  TypInfo,
  XMLDoc, XMLIntf,
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
// TCustomLocalizerItem
//
// -----------------------------------------------------------------------------
constructor TCustomLocalizerItem.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

function TCustomLocalizerItem.GetInheritParentState: boolean;
begin
  Result := False;
end;

function TCustomLocalizerItem.GetInheritParentStatus: boolean;
begin
  Result := False;
end;

function TCustomLocalizerItem.GetState: TLocalizerItemState;
begin
  Result := FState;
end;

function TCustomLocalizerItem.GetStatus: TLocalizerItemStatus;
begin
  Result := FStatus;
end;

procedure TCustomLocalizerItem.SetName(const Value: string);
begin
  FName := Value;
end;


// -----------------------------------------------------------------------------
//
// TCustomLocalizerChildItem
//
// -----------------------------------------------------------------------------
constructor TCustomLocalizerChildItem<TParentClass>.Create(AParent: TParentClass; const AName: string);
begin
  inherited Create(AName);
  FParent := AParent;
end;

function TCustomLocalizerChildItem<TParentClass>.GetInheritParentState: boolean;
begin
  Result := (Parent.State <> lItemStateExisting);
end;

function TCustomLocalizerChildItem<TParentClass>.GetInheritParentStatus: boolean;
begin
  Result := (Parent.Status <> lItemStatusTranslate);
end;

function TCustomLocalizerChildItem<TParentClass>.GetParent: TParentClass;
begin
  Result := FParent;
end;

function TCustomLocalizerChildItem<TParentClass>.GetState: TLocalizerItemState;
begin
  if (InheritParentState) then
    Result := Parent.State
  else
    Result := inherited GetState;
end;

function TCustomLocalizerChildItem<TParentClass>.GetStatus: TLocalizerItemStatus;
begin
  if (InheritParentStatus) then
    Result := Parent.Status
  else
    Result := inherited GetStatus;
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

procedure TLocalizerProject.Clear;
begin
  FModules.Clear;
end;

procedure TLocalizerProject.BeginLoad;
begin
  Inc(FLoadCount);
  if (FLoadCount = 1) then
  begin
    Include(FState, ProjectStateLoading);
    SetItemState(lItemStateUnused);
  end;
end;

procedure TLocalizerProject.EndLoad;
begin
  Dec(FLoadCount);
  if (FLoadCount = 0) then
    Exclude(FState, ProjectStateLoading);
end;

procedure TLocalizerProject.SetItemState(State: TLocalizerItemState);
var
  Module: TLocalizerModule;
  Item: TLocalizerItem;
  Prop: TLocalizerProperty;
begin
  for Module in Modules.Values do
  begin
    Module.State := State;
    for Item in Module.Items.Values do
    begin
      Item.State:= State;
      for Prop in Item.Properties.Values do
        Prop.State := State;
    end;
  end;
end;

function TLocalizerProject.Traverse(Delegate: TLocalizerModuleDelegate): boolean;
var
  SortedModules: TArray<TLocalizerModule>;
  Module: TLocalizerModule;
begin
  Result := True;

  SortedModules := Modules.Values.ToArray;

  TArray.Sort<TLocalizerModule>(SortedModules, TComparer<TLocalizerModule>.Construct(
    function(const Left, Right: TLocalizerModule): Integer
    begin
      Result := (Ord(Left.Kind) - Ord(Right.Kind));
      if (Result = 0) then
        Result := AnsiCompareText(Left.Name, Right.Name);
    end));

  for Module in SortedModules do
    if (not Delegate(Module)) then
      Exit(False);
end;

function TLocalizerProject.Traverse(Delegate: TLocalizerItemDelegate): boolean;
begin
  Result := Traverse(
    function(Module: TLocalizerModule): boolean
    begin
      Result := Module.Traverse(Delegate);
    end);
end;

function TLocalizerProject.Traverse(Delegate: TLocalizerPropertyDelegate; Kinds: TLocalizerModuleKinds): boolean;
begin
  Result := Traverse(
    function(Module: TLocalizerModule): boolean
    begin
      if (Module.Kind in Kinds) then
        Result := Module.Traverse(Delegate)
      else
        Result := True;
    end);
end;

// -----------------------------------------------------------------------------

function TLocalizerProject.AddModule(const AName: string): TLocalizerModule;
begin
  if (not FModules.TryGetValue(AName, Result)) then
    Result := TLocalizerModule.Create(Self, AName)
  else
  if (ProjectStateLoading in State) then
    Result.State := lItemStateExisting;
end;


// -----------------------------------------------------------------------------
//
// TLocalizerModule
//
// -----------------------------------------------------------------------------
constructor TLocalizerModule.Create(AProject: TLocalizerProject; const AName: string);
begin
  inherited Create(AName);
  FItems := TLocalizerItems.Create([doOwnsValues], TTextComparer.Create);
  FResourceGroups := TList<Word>.Create;
  FProject := AProject;
  FProject.Modules.Add(Name, Self);
end;

destructor TLocalizerModule.Destroy;
begin
  FProject.Modules.ExtractPair(FName);
  FItems.Free;
  FResourceGroups.Free;
  inherited;
end;

procedure TLocalizerModule.Clear;
begin
  FItems.Clear;
  FResourceGroups.Clear;
end;

procedure TLocalizerModule.SetName(const Value: string);
begin
  if (Name = Value) then
    exit;

  FProject.Modules.ExtractPair(Name);
  inherited SetName(Value);
  FProject.Modules.Add(Name, Self);
end;

function TLocalizerModule.Traverse(Delegate: TLocalizerPropertyDelegate): boolean;
begin
  Result := Traverse(
    function(Item: TLocalizerItem): boolean
    begin
      Result := Item.Traverse(Delegate);
    end);
end;

function TLocalizerModule.Traverse(Delegate: TLocalizerItemDelegate): boolean;
var
  SortedItems: TArray<TLocalizerItem>;
  Item: TLocalizerItem;
begin
  Result := True;

  SortedItems := Items.Values.ToArray;

  TArray.Sort<TLocalizerItem>(SortedItems, TComparer<TLocalizerItem>.Construct(
    function(const Left, Right: TLocalizerItem): Integer
    begin
      Result := (Ord(Left.Module.Kind) - Ord(Right.Module.Kind));
      if (Result = 0) then
      begin
        if (Left.Module.Kind = mkForm) or ((Left.ResourceID = 0) and  (Right.ResourceID = 0)) then
          Result := CompareText(Left.Name, Right.Name)
        else
        begin
          if (Left.ResourceID = 0) then
            Result := -1
          else
          if (Right.ResourceID = 0) then
            Result := 1
          else
          if (Left.ResourceID > Right.ResourceID) then
            Result := 1
          else
          if (Left.ResourceID < Right.ResourceID) then
            Result := -1
          else
            Result := 0;
        end;
      end;
    end));

  for Item in SortedItems do
    if (not Delegate(Item)) then
      Exit(False);
end;

// -----------------------------------------------------------------------------

function TLocalizerModule.AddItem(const AName, ATypeName: string): TLocalizerItem;
begin
  if (not FItems.TryGetValue(AName, Result)) then
    Result := TLocalizerItem.Create(Self, AName, ATypeName)
  else
  if (ProjectStateLoading in Project.State) then
    Result.State := lItemStateExisting;
end;

function TLocalizerModule.AddItem(AResourceID: Word; const ATypeName: string): TLocalizerItem;
var
  Item: TPair<string, TLocalizerItem>;
begin
  Result := nil;

  for Item in Items do
    if (Item.Value.ResourceID = AResourceID) then
    begin
      Result := Item.Value;
      break;
    end;

  if (Result = nil) and (not FItems.TryGetValue(IntToStr(AResourceID), Result)) then
  begin
    Result := TLocalizerItem.Create(Self, IntToStr(AResourceID), ATypeName);
    Result.ResourceID := AResourceID;
  end else
  if (ProjectStateLoading in Project.State) then
    Result.State := lItemStateExisting;
end;


// -----------------------------------------------------------------------------
//
// TLocalizerItem
//
// -----------------------------------------------------------------------------
constructor TLocalizerItem.Create(AModule: TLocalizerModule; const AName, ATypeName: string);
begin
  inherited Create(AModule, AName);
  FProperties := TLocalizerProperties.Create([doOwnsValues], TTextComparer.Create);
  FTypeName := ATypeName;
  Module.Items.Add(FName, Self);
end;

destructor TLocalizerItem.Destroy;
begin
  Module.Items.ExtractPair(Name);
  FProperties.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerItem.Clear;
begin
  FProperties.Clear;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerItem.SetName(const Value: string);
begin
  if (Name = Value) then
    exit;

  Module.Items.ExtractPair(Name);
  inherited SetName(Value);
  Module.Items.Add(Name, Self);
end;

// -----------------------------------------------------------------------------

function TLocalizerItem.Traverse(Delegate: TLocalizerPropertyDelegate): boolean;
var
  SortedProps: TArray<TLocalizerProperty>;
  Prop: TLocalizerProperty;
begin
  Result := True;

  SortedProps := Properties.Values.ToArray;

  TArray.Sort<TLocalizerProperty>(SortedProps, TComparer<TLocalizerProperty>.Construct(
    function(const Left, Right: TLocalizerProperty): Integer
    begin
      Result := AnsiCompareText(Left.Name, Right.Name);
    end));

  for Prop in SortedProps do
    if (not Delegate(Prop)) then
      Exit(False);
end;

// -----------------------------------------------------------------------------

function TLocalizerItem.AddProperty(const AName, AValue: string): TLocalizerProperty;
begin
  if (not FProperties.TryGetValue(AName, Result)) then
    Result := TLocalizerProperty.Create(Self, AName)
  else
  if (ProjectStateLoading in Module.Project.State) then
    Result.State := lItemStateExisting;

  Result.Value := AValue;
end;


// -----------------------------------------------------------------------------
//
// TLocalizerProperty
//
// -----------------------------------------------------------------------------
constructor TLocalizerProperty.Create(AItem: TLocalizerItem; const AName: string);
begin
  inherited Create(AItem, AName);
  Item.Properties.Add(Name, Self);
  FTranslations := TLocalizerTranslations.Create;
end;

destructor TLocalizerProperty.Destroy;
begin
  Item.Properties.ExtractPair(Name);
  FTranslations.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerProperty.Clear;
begin
  FTranslations.Clear;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerProperty.SetTranslatedValue(LocaleID: LCID; const Value: string);
begin
  Translations.AddOrUpdateTranslation(LocaleID, Value);
end;

function TLocalizerProperty.GetTranslatedValue(LocaleID: LCID): string;
var
  Translation: TLocalizerTranslation;
begin
  if (Translations.TryGetTranslation(LocaleID, Translation)) and (Translation.Status <> tStatusObsolete) then
    Result := Translation.Value
  else
    Result := Value;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerProperty.SetValue(const Value: string);
begin
  if (FValue = Value) then
    exit;
  FValue := Value;
end;


// -----------------------------------------------------------------------------

function TLocalizerProperty.Traverse(Delegate: TLocalizerPropertyDelegate): boolean;
begin
  Result := Delegate(Self);
end;

function TLocalizerProperty.Traverse(Delegate: TLocalizerTranslationDelegate): boolean;
var
  Translation: TPair<LCID, TLocalizerTranslation>;
begin
  Result := True;

  for Translation in FTranslations.FTranslations do
    if (not Delegate(Self, Translation.Key, Translation.Value)) then
      Exit(False);
end;


// -----------------------------------------------------------------------------
//
// TLocalizerTranslations
//
// -----------------------------------------------------------------------------
function TLocalizerTranslations.AddOrUpdateTranslation(LocaleID: LCID; const Value: string; Status: TTranslationStatus): TLocalizerTranslation;
begin
  if (not FTranslations.TryGetValue(LocaleID, Result)) then
  begin
    Result := TLocalizerTranslation.Create;
    FTranslations.Add(LocaleID, Result);
  end;

  Result.Value := Value;
  Result.Status := Status;
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

procedure TLocalizerTranslations.Clear;
begin
  FTranslations.Clear;
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

procedure TLocalizerTranslations.Remove(LocaleID: LCID);
begin
  FTranslations.Remove(LocaleID);
end;

function TLocalizerTranslations.TryGetTranslation(LocaleID: LCID; var Value: TLocalizerTranslation): boolean;
begin
  Result := FTranslations.TryGetValue(LocaleID, Value);
end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

end.
