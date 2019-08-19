unit amLocalization.Model;

interface

uses
  Generics.Collections,
  Generics.Defaults,
  Winapi.Windows, System.Classes;

const
  sModuleNameResourcestrings = 'resourcestrings';

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
  TLocalizerTranslationDelegate = reference to function(Prop: TLocalizerProperty; Translation: TLocalizerTranslation): boolean;

  TLocalizerItemState = (ItemStateNew, ItemStateExisting, ItemStateUnused);
  TLocalizerItemStatus = (ItemStatusTranslate, ItemStatusHold, ItemStatusDontTranslate);

  TLocalizerModuleKind = (mkOther, mkForm, mkString);
  TLocalizerModuleKinds = set of TLocalizerModuleKind;


// -----------------------------------------------------------------------------
//
// TTargetLanguage
//
// -----------------------------------------------------------------------------
  TTargetLanguage = class
  private
    FLanguageID: LCID;
    FTranslatedCount: integer;
  public
    constructor Create(ALanguageID: LCID);

    property LanguageID: LCID read FLanguageID;
    property TranslatedCount: integer read FTranslatedCount write FTranslatedCount;
  end;


// -----------------------------------------------------------------------------
//
// TTargetLanguageList
//
// -----------------------------------------------------------------------------
  TTargetLanguageList = class
  private
    FLanguages: TObjectDictionary<LCID, TTargetLanguage>;
  protected
    function GetCount: integer;
    function GetItem(Index: integer): TTargetLanguage;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(LanguageID: LCID): TTargetLanguage;
    procedure Remove(LanguageID: LCID);
    procedure Delete(Index: integer);
    procedure Clear;
    function Find(LanguageID: LCID): TTargetLanguage;
    function Contains(LanguageID: LCID): boolean;

    property Count: integer read GetCount;
    property Languages[Index: integer]: TTargetLanguage read GetItem; default;
  end;


// -----------------------------------------------------------------------------
//
// TLocalizerProject
//
// -----------------------------------------------------------------------------
  TLocalizerProjectState = (ProjectStateLoading);
  TLocalizerProjectStates = set of TLocalizerProjectState;

  TLocalizerModuleEvent = procedure(Module: TLocalizerModule) of object;


  TLocalizerProject = class
  strict private
    FName: string;
    FSourceFilename: string;
    FModules: TLocalizerModules;
    FBaseLocaleID: LCID;
    FTargetLanguages: TTargetLanguageList;
    FState: TLocalizerProjectStates;
    FLoadCount: integer;
    FPropertyCount: integer;
    FStatusCount: array[TLocalizerItemStatus] of integer;
    FModified: boolean;
    FUpdateCount: integer;
    FUpdatePending: boolean;
    FOnChanged: TNotifyEvent;
    FOnModuleChanged: TLocalizerModuleEvent;
  strict protected
    function GetStatusCount(Status: TLocalizerItemStatus): integer;
    procedure SetItemStateRecursive(State: TLocalizerItemState);
  protected
    procedure UpdateStatusCount(Status: TLocalizerItemStatus; Delta: integer);
    procedure ModuleChanged(Module: TLocalizerModule);
  public
    constructor Create(const AName: string; ABaseLocaleID: LCID);
    destructor Destroy; override;

    procedure Changed;

    procedure Clear;
    function Purge: boolean;

    function AddModule(const AName: string; Kind: TLocalizerModuleKind = mkOther): TLocalizerModule;
    function FindModule(const AName: string): TLocalizerModule;

    procedure BeginLoad(MarkUnused: boolean = False);
    procedure EndLoad;

    procedure BeginUpdate;
    procedure EndUpdate;

    function Traverse(Delegate: TLocalizerModuleDelegate; Sorted: boolean = True): boolean; overload;
    function Traverse(Delegate: TLocalizerItemDelegate; Sorted: boolean = True): boolean; overload;
    function Traverse(Delegate: TLocalizerPropertyDelegate; Sorted: boolean = True; Kinds: TLocalizerModuleKinds = [mkForm, mkString]): boolean; overload;

    property State: TLocalizerProjectStates read FState;

    property Name: string read FName write FName;
    property SourceFilename: string read FSourceFilename write FSourceFilename;
    property BaseLocaleID: LCID read FBaseLocaleID write FBaseLocaleID;
    property TargetLanguages: TTargetLanguageList read FTargetLanguages;
    property Modules: TLocalizerModules read FModules;
    property Modified: boolean read FModified write FModified;

    // Total number of properties with State<>lItemStateUnused
    property PropertyCount: integer read FPropertyCount;
    // Property count by status, State<>lItemStateUnused
    property StatusCount[Status: TLocalizerItemStatus]: integer read GetStatusCount;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnModuleChanged: TLocalizerModuleEvent read FOnModuleChanged write FOnModuleChanged;
  end;


// -----------------------------------------------------------------------------
//
// TCustomLocalizerItem
//
// -----------------------------------------------------------------------------
  TCustomLocalizerItem = class abstract
  strict private
    FName: string;
    FState: TLocalizerItemState;
    FStatus: TLocalizerItemStatus;
    // FStatusCount holds the accumulated sum of the *actual* status of the item.
    // For parent items the sum is that of its children's FStatusCount
    FStatusCount: array[TLocalizerItemStatus] of integer;
    FUpdateCount: integer;
    FChanged: boolean;
  strict protected
    procedure UpdateParentStatusCount(AStatus: TLocalizerItemStatus; Delta: integer); virtual; abstract;
    procedure DoChanged; virtual; abstract;

    procedure SetName(const Value: string); virtual;
    procedure SetState(const Value: TLocalizerItemState);
    procedure SetStatus(const Value: TLocalizerItemStatus);
    procedure DoSetStatus(const Value: TLocalizerItemStatus); virtual;
    procedure ApplyParentStatusChange(const Value: TLocalizerItemStatus);
    procedure ApplyDirectStatusChange(const Value: TLocalizerItemStatus);
    function GetState: TLocalizerItemState; virtual;
    function GetInheritParentState: boolean; virtual;
    function GetEffectiveStatus: TLocalizerItemStatus; virtual;
    function GetStatusCount(AStatus: TLocalizerItemStatus): integer;
    function GetPropertyCount: integer;
  protected
    procedure Changed;
    procedure UpdateStatusCount(AStatus: TLocalizerItemStatus; Delta: integer);
    function CalculateEffectiveStatus(ChildStatus: TLocalizerItemStatus): TLocalizerItemStatus;
  public
    constructor Create(const AName: string);

    property Name: string read FName write SetName;

    procedure Clear; virtual; abstract;
    procedure BeginUpdate;
    procedure EndUpdate;

    function Traverse(Delegate: TLocalizerPropertyDelegate; Sorted: boolean = True): boolean; virtual; abstract;

    property State: TLocalizerItemState read GetState write SetState;
    property Status: TLocalizerItemStatus read FStatus write SetStatus;
    property InheritParentState: boolean read GetInheritParentState;
    property EffectiveStatus: TLocalizerItemStatus read GetEffectiveStatus;

    property StatusCount[AStatus: TLocalizerItemStatus]: integer read GetStatusCount;
    property PropertyCount: integer read GetPropertyCount;
  end;


// -----------------------------------------------------------------------------
//
// TCustomLocalizerChildItem
//
// -----------------------------------------------------------------------------
  TCustomLocalizerChildItem<TParentClass: TCustomLocalizerItem> = class abstract(TCustomLocalizerItem)
  strict private
    FParent: TParentClass;
  strict protected
    procedure UpdateParentStatusCount(AStatus: TLocalizerItemStatus; Delta: integer); override;
    procedure DoChanged; override;

    function GetState: TLocalizerItemState; override;
    function GetInheritParentState: boolean; override;
    function GetEffectiveStatus: TLocalizerItemStatus; override;
  protected
    function GetParent: TParentClass;
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
  strict private
    FProject: TLocalizerProject;
    FKind: TLocalizerModuleKind;
    FItems: TLocalizerItems;
    FResourceGroups: TList<Word>;
  strict protected
    procedure SetName(const Value: string); override;
    procedure UpdateParentStatusCount(AStatus: TLocalizerItemStatus; Delta: integer); override;
    procedure DoChanged; override;
  protected
  public
    constructor Create(AProject: TLocalizerProject; const AName: string);
    destructor Destroy; override;

    property Project: TLocalizerProject read FProject;
    property Kind: TLocalizerModuleKind read FKind write FKind;
    property Items: TLocalizerItems read FItems;

    procedure Clear; override;
    function Purge: boolean;

    function FindItem(const AName: string): TLocalizerItem;
    function AddItem(const AName, ATypeName: string): TLocalizerItem; overload;
    function AddItem(AResourceID: Word; const ATypeName: string): TLocalizerItem; overload;

    property ResourceGroups: TList<Word> read FResourceGroups;

    function Traverse(Delegate: TLocalizerItemDelegate; Sorted: boolean = True): boolean; reintroduce; overload;
    function Traverse(Delegate: TLocalizerPropertyDelegate; Sorted: boolean = True): boolean; overload; override;
  end;


// -----------------------------------------------------------------------------
//
// TLocalizerItem
//
// -----------------------------------------------------------------------------
  TLocalizerItem = class(TCustomLocalizerChildItem<TLocalizerModule>)
  strict private
    FResourceID: WORD;
    FTypeName: string;
    FProperties: TLocalizerProperties;
  strict protected
    procedure SetName(const Value: string); override;
  public
    constructor Create(AModule: TLocalizerModule; const AName, ATypeName: string);
    destructor Destroy; override;

    procedure Clear; override;
    function Purge: boolean;

    function FindProperty(const AName: string): TLocalizerProperty;
    function AddProperty(const AName: string): TLocalizerProperty; overload;
    function AddProperty(const AName: string; const AValue: string): TLocalizerProperty; overload;

    function Traverse(Delegate: TLocalizerPropertyDelegate; Sorted: boolean = True): boolean; override;

    property Module: TLocalizerModule read GetParent;
    property ResourceID: WORD read FResourceID write FResourceID;
    property TypeName: string read FTypeName write FTypeName;
    property Properties: TLocalizerProperties read FProperties;
  end;


// -----------------------------------------------------------------------------
//
// TLocalizerTranslation
//
// -----------------------------------------------------------------------------
  TTranslationStatus = (
    tStatusObsolete,            // Source value has changed. Translation is obsolete.
    tStatusPending,             // Translation has not been performed.
    tStatusProposed,            // Translation has been proposed
    tStatusTranslated           // Translation complete
    );
    // Note:
    // Status > tStatusPending is considered "translated".
    // Take this into consideration if adding values or changing the order.
    // See GetIsTranslated and SetStatus.

  TTranslationFlag = (
    tFlagWhatEver
  );
  TTranslationFlags = set of TTranslationFlag;

  TTranslationWarning = (
    tWarningAccelerator,        // Translation has incorrect number of accelerators
    tWarningFormatSpecifier,    // Translation has incorrect number of format specifiers
    tWarningLineBreak           // Translation has incorrect number of linebreaks
  );
  TTranslationWarnings = set of TTranslationWarning;

  TLocalizerTranslation = class
  strict private
    FValue: string;
    FLanguage: TTargetLanguage;
    FStatus: TTranslationStatus;
    FOwner: TLocalizerProperty;
    FUpdateCount: integer;
    FChanged: boolean;
    FWarnings: TTranslationWarnings;
  strict protected
    function GetIsTranslated: boolean;
    procedure SetStatus(const Value: TTranslationStatus);
    procedure SetValue(const Value: string);
    procedure Changed;
  public
    constructor Create(AOwner: TLocalizerProperty; ALanguage: TTargetLanguage);
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    property Owner: TLocalizerProperty read FOwner;

    procedure Update(const AValue: string; AStatus: TTranslationStatus);
    procedure UpdateWarnings;

    property Value: string read FValue write SetValue;
    property Language: TTargetLanguage read FLanguage;
    property Status: TTranslationStatus read FStatus write SetStatus;
    property IsTranslated: boolean read GetIsTranslated;
    property Warnings: TTranslationWarnings read FWarnings write FWarnings;
  end;

  TLocalizerTranslations = class
  strict private
    FTranslations: TDictionary<TTargetLanguage, TLocalizerTranslation>;
    FOwner: TLocalizerProperty;
  strict protected
    function GetItem(Language: TTargetLanguage): TLocalizerTranslation;
  protected
    property Translations: TDictionary<TTargetLanguage, TLocalizerTranslation> read FTranslations;
  public
    constructor Create(AOwner: TLocalizerProperty);
    destructor Destroy; override;

    procedure Clear;

    function TryGetTranslation(Language: TTargetLanguage; var Value: TLocalizerTranslation): boolean;
    function FindTranslation(Language: TTargetLanguage): TLocalizerTranslation;
    function AddOrUpdateTranslation(Language: TTargetLanguage; const Value: string; Status: TTranslationStatus = tStatusProposed): TLocalizerTranslation;
    procedure Remove(Language: TTargetLanguage);

    property Owner: TLocalizerProperty read FOwner;
    property Items[Language: TTargetLanguage]: TLocalizerTranslation read GetItem; default;
  end;


// -----------------------------------------------------------------------------
//
// TLocalizerProperty
//
// -----------------------------------------------------------------------------
  TLocalizerProperty = class(TCustomLocalizerChildItem<TLocalizerItem>)
  strict private
    FValue: string;
    FTranslations: TLocalizerTranslations;
  strict protected
    procedure SetValue(const Value: string);
    function GetTranslatedValue(Language: TTargetLanguage): string;
    procedure SetTranslatedValue(Language: TTargetLanguage; const Value: string);
    procedure DoSetStatus(const Value: TLocalizerItemStatus); override;
  public
    constructor Create(AItem: TLocalizerItem; const AName: string);
    destructor Destroy; override;

    procedure Clear; override;

    function Traverse(Delegate: TLocalizerPropertyDelegate; Sorted: boolean = True): boolean; overload; override;
    function Traverse(Delegate: TLocalizerTranslationDelegate): boolean; reintroduce; overload;

    function HasTranslation(Language: TTargetLanguage): boolean;

    property Item: TLocalizerItem read GetParent;
    property Value: string read FValue write SetValue;
    property TranslatedValue[Language: TTargetLanguage]: string read GetTranslatedValue write SetTranslatedValue;

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
  System.Hash,
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
  Result := THashBobJenkins.GetHashValue(s);
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

// -----------------------------------------------------------------------------

procedure TCustomLocalizerItem.SetName(const Value: string);
begin
  FName := Value;
end;

// -----------------------------------------------------------------------------

procedure TCustomLocalizerItem.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCustomLocalizerItem.EndUpdate;
begin
  Dec(FUpdateCount);
  if (FChanged) and (FUpdateCount = 0) then
  begin
    DoChanged;
    FChanged := False;
  end;
end;

procedure TCustomLocalizerItem.Changed;
begin
  BeginUpdate;
  FChanged := True;
  EndUpdate;
end;

// -----------------------------------------------------------------------------

function TCustomLocalizerItem.GetInheritParentState: boolean;
begin
  Result := False;
end;

function TCustomLocalizerItem.GetState: TLocalizerItemState;
begin
  Result := FState;
end;

procedure TCustomLocalizerItem.SetState(const Value: TLocalizerItemState);
begin
  if (FState = Value) then
    Exit;

  if (FState = ItemStateUnused) then
  begin
    UpdateParentStatusCount(ItemStatusTranslate, FStatusCount[ItemStatusTranslate]);
    UpdateParentStatusCount(ItemStatusDontTranslate, FStatusCount[ItemStatusDontTranslate]);
    UpdateParentStatusCount(ItemStatusHold, FStatusCount[ItemStatusHold]);
  end;

  FState := Value;

  if (FState = ItemStateUnused) then
  begin
    UpdateParentStatusCount(ItemStatusTranslate, -FStatusCount[ItemStatusTranslate]);
    UpdateParentStatusCount(ItemStatusDontTranslate, -FStatusCount[ItemStatusDontTranslate]);
    UpdateParentStatusCount(ItemStatusHold, -FStatusCount[ItemStatusHold]);
  end;

  Changed;
end;

procedure TCustomLocalizerItem.DoSetStatus(const Value: TLocalizerItemStatus);
begin
  ApplyParentStatusChange(Value);
end;

procedure TCustomLocalizerItem.ApplyDirectStatusChange(const Value: TLocalizerItemStatus);
begin
  // Apply change in effective status count - Only used by property
  UpdateStatusCount(FStatus, -1);
  FStatus := Value;
  UpdateStatusCount(FStatus, 1);
end;

procedure TCustomLocalizerItem.ApplyParentStatusChange(const Value: TLocalizerItemStatus);
var
  DeltaCount: array[TLocalizerItemStatus] of integer;
begin
  if (FState = ItemStateUnused) then
    Exit;

  // Apply change in logical status count
  DeltaCount[ItemStatusTranslate] := 0;
  DeltaCount[ItemStatusDontTranslate] := 0;
  DeltaCount[ItemStatusHold] := 0;
  // Update counts as if we are performing a temporary status change TO lItemStatusTranslate
  case FStatus of
    ItemStatusTranslate:
      ;

    ItemStatusDontTranslate:
      begin
        DeltaCount[ItemStatusTranslate] := FStatusCount[ItemStatusTranslate];
        DeltaCount[ItemStatusDontTranslate] := -FStatusCount[ItemStatusTranslate]-FStatusCount[ItemStatusHold];
        DeltaCount[ItemStatusHold] := FStatusCount[ItemStatusHold];
      end;

    ItemStatusHold:
      begin
        DeltaCount[ItemStatusTranslate] := FStatusCount[ItemStatusTranslate];
        DeltaCount[ItemStatusDontTranslate] := 0;
        DeltaCount[ItemStatusHold] := -FStatusCount[ItemStatusTranslate];
      end;
  end;

  FStatus := Value;

  // Update counts as if we are performing a temporary status change FROM lItemStatusTranslate
  case FStatus of
    ItemStatusTranslate:
      ;

    ItemStatusDontTranslate:
      begin
        Inc(DeltaCount[ItemStatusTranslate], -FStatusCount[ItemStatusTranslate]);
        Inc(DeltaCount[ItemStatusDontTranslate], FStatusCount[ItemStatusTranslate]+FStatusCount[ItemStatusHold]);
        Inc(DeltaCount[ItemStatusHold], -FStatusCount[ItemStatusHold]);
      end;

    ItemStatusHold:
      begin
        Inc(DeltaCount[ItemStatusTranslate], -FStatusCount[ItemStatusTranslate]);
        Inc(DeltaCount[ItemStatusHold], FStatusCount[ItemStatusTranslate]);
      end;
  end;

  // Apply change in count
  if (DeltaCount[ItemStatusTranslate] <> 0) then
    UpdateParentStatusCount(ItemStatusTranslate, DeltaCount[ItemStatusTranslate]);
  if (DeltaCount[ItemStatusDontTranslate] <> 0) then
    UpdateParentStatusCount(ItemStatusDontTranslate, DeltaCount[ItemStatusDontTranslate]);
  if (DeltaCount[ItemStatusHold] <> 0) then
    UpdateParentStatusCount(ItemStatusHold, DeltaCount[ItemStatusHold]);

(* Another way to do the above - perhaps easier to understand:

  // Remove all counts belonging to this item from parent
  UpdateParentStatusCount(ItemStatusTranslate, -FStatusCount[ItemStatusTranslate]);
  UpdateParentStatusCount(ItemStatusHold, -FStatusCount[ItemStatusHold]);
  UpdateParentStatusCount(ItemStatusDontTranslate, -FStatusCount[ItemStatusDontTranslate]);

  FStatus := Value;

  // Reapply all counts belonging to this item to parent
  case FStatus of
    ItemStatusTranslate:
      begin
        UpdateParentStatusCount(ItemStatusTranslate, FStatusCount[ItemStatusTranslate]);
        UpdateParentStatusCount(ItemStatusHold, FStatusCount[ItemStatusHold]);
        UpdateParentStatusCount(ItemStatusDontTranslate, FStatusCount[ItemStatusDontTranslate]);
      end;

    ItemStatusHold:
      begin
        UpdateParentStatusCount(ItemStatusHold, FStatusCount[ItemStatusTranslate]+FStatusCount[ItemStatusHold]);
        UpdateParentStatusCount(ItemStatusDontTranslate, FStatusCount[ItemStatusDontTranslate]);
      end;

    ItemStatusDontTranslate:
      begin
        UpdateParentStatusCount(ItemStatusDontTranslate, FStatusCount[ItemStatusTranslate]+FStatusCount[ItemStatusHold]+FStatusCount[ItemStatusDontTranslate]);
      end;
  end;
*)

(* Another way - even more verbose:

  // Remove all counts belonging to this item from parent
  UpdateParentStatusCount(CalculateEffectiveStatus(ItemStatusTranslate), -FStatusCount[ItemStatusTranslate]);
  UpdateParentStatusCount(CalculateEffectiveStatus(ItemStatusHold), -FStatusCount[ItemStatusHold]);
  UpdateParentStatusCount(CalculateEffectiveStatus(ItemStatusDontTranslate), -FStatusCount[ItemStatusDontTranslate]);

  FStatus := Value;

  // Reapply all counts belonging to this item to parent
  UpdateParentStatusCount(CalculateEffectiveStatus(ItemStatusTranslate), FStatusCount[ItemStatusTranslate]);
  UpdateParentStatusCount(CalculateEffectiveStatus(ItemStatusHold), FStatusCount[ItemStatusHold]);
  UpdateParentStatusCount(CalculateEffectiveStatus(ItemStatusDontTranslate), FStatusCount[ItemStatusDontTranslate]);
*)
end;

procedure TCustomLocalizerItem.SetStatus(const Value: TLocalizerItemStatus);
begin
  if (FStatus = Value) then
    Exit;

  BeginUpdate;
  try
    DoSetStatus(Value);

    Changed;
  finally
    EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

function TCustomLocalizerItem.GetPropertyCount: integer;
begin
  Result := FStatusCount[ItemStatusTranslate];
end;

function TCustomLocalizerItem.GetStatusCount(AStatus: TLocalizerItemStatus): integer;
begin
  if (FState <> ItemStateUnused) then
    Result := FStatusCount[AStatus]
  else
    Result := 0;
end;

procedure TCustomLocalizerItem.UpdateStatusCount(AStatus: TLocalizerItemStatus; Delta: integer);
begin
  if (Delta = 0) then
    Exit;

  Inc(FStatusCount[AStatus], Delta);

  Assert(FStatusCount[AStatus] >= 0);

  if (FState <> ItemStateUnused) then
    UpdateParentStatusCount(CalculateEffectiveStatus(AStatus), Delta);
end;

function TCustomLocalizerItem.GetEffectiveStatus: TLocalizerItemStatus;
begin
  Result := Status;
end;

function TCustomLocalizerItem.CalculateEffectiveStatus(ChildStatus: TLocalizerItemStatus): TLocalizerItemStatus;
begin
  if (ChildStatus >= Status) then
    Result := ChildStatus
  else
    Result := Status;
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

// -----------------------------------------------------------------------------

procedure TCustomLocalizerChildItem<TParentClass>.DoChanged;
begin
  FParent.Changed;
end;

// -----------------------------------------------------------------------------

function TCustomLocalizerChildItem<TParentClass>.GetEffectiveStatus: TLocalizerItemStatus;
begin
  Result := Parent.CalculateEffectiveStatus(Status);
end;

function TCustomLocalizerChildItem<TParentClass>.GetInheritParentState: boolean;
begin
  //  If the parent is unused, the children are also unused
  Result := (Parent.State = ItemStateUnused);
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

procedure TCustomLocalizerChildItem<TParentClass>.UpdateParentStatusCount(AStatus: TLocalizerItemStatus; Delta: integer);
begin
  if (Delta = 0) then
    Exit;

  FParent.UpdateStatusCount(AStatus, Delta);
end;


// -----------------------------------------------------------------------------
//
// TTargetLanguage
//
// -----------------------------------------------------------------------------
constructor TTargetLanguage.Create(ALanguageID: LCID);
begin
  inherited Create;

  FLanguageID := ALanguageID;
end;

// -----------------------------------------------------------------------------
//
// TTargetLanguageList
//
// -----------------------------------------------------------------------------
constructor TTargetLanguageList.Create;
begin
  inherited Create;

  FLanguages := TObjectDictionary<LCID, TTargetLanguage>.Create([doOwnsValues]);
end;

destructor TTargetLanguageList.Destroy;
begin
  FLanguages.Free;
  inherited;
end;

function TTargetLanguageList.Add(LanguageID: LCID): TTargetLanguage;
begin
  if (not FLanguages.TryGetValue(LanguageID, Result)) then
  begin
    Result := TTargetLanguage.Create(LanguageID);
    FLanguages.Add(LanguageID, Result);
  end;
end;

procedure TTargetLanguageList.Clear;
begin
  FLanguages.Clear;
end;

function TTargetLanguageList.Contains(LanguageID: LCID): boolean;
begin
  Result := FLanguages.ContainsKey(LanguageID);
end;

procedure TTargetLanguageList.Delete(Index: integer);
begin
  FLanguages.Remove(FLanguages.Keys.ToArray[Index]);
end;

function TTargetLanguageList.Find(LanguageID: LCID): TTargetLanguage;
begin
  if (not FLanguages.TryGetValue(LanguageID, Result)) then
    Result := nil;
end;

function TTargetLanguageList.GetCount: integer;
begin
  Result := FLanguages.Count;
end;

function TTargetLanguageList.GetItem(Index: integer): TTargetLanguage;
begin
  Result := FLanguages.Values.ToArray[Index];
end;

procedure TTargetLanguageList.Remove(LanguageID: LCID);
begin
  FLanguages.Remove(LanguageID);
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
  FTargetLanguages := TTargetLanguageList.Create;
  FName := AName;
  FBaseLocaleID := ABaseLocaleID;
end;

destructor TLocalizerProject.Destroy;
begin
  FModules.Free;
  FTargetLanguages.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerProject.Changed;
begin
  FModified := True;

  BeginUpdate;
  FUpdatePending := True;
  EndUpdate;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerProject.Clear;
var
  Status: TLocalizerItemStatus;
begin
  FModules.Clear;
  FTargetLanguages.Clear;
  FPropertyCount := 0;
  for Status := Low(TLocalizerItemStatus) to High(TLocalizerItemStatus) do
    FStatusCount[Status] := 0;
end;

// -----------------------------------------------------------------------------

function TLocalizerProject.Purge: boolean;
var
  Module: TLocalizerModule;
begin
  Result := False;
  for Module in Modules.Values.ToArray do // ToArray for stability since we delete from dictionary
  begin
    if (Module.Kind = mkOther) or (Module.State = ItemStateUnused) then
    begin
      Module.Free;
      Result := True;
      continue;
    end;

    if (Module.Purge) then
      Result := True;

    if (Module.Items.Count = 0) then
    begin
      Module.Free;
      Result := True;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerProject.BeginLoad(MarkUnused: boolean);
begin
  Inc(FLoadCount);
  if (FLoadCount = 1) then
  begin
    Include(FState, ProjectStateLoading);
    if (MarkUnused) then
      SetItemStateRecursive(ItemStateUnused);
  end;
end;

procedure TLocalizerProject.EndLoad;
begin
  Dec(FLoadCount);
  if (FLoadCount = 0) then
    Exclude(FState, ProjectStateLoading);
end;

// -----------------------------------------------------------------------------

procedure TLocalizerProject.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TLocalizerProject.EndUpdate;
begin
  Dec(FUpdateCount);
  if (FUpdateCount = 0) then
  begin
    if (FUpdatePending) then
    begin
      if (Assigned(FOnChanged)) then
        FOnChanged(Self);

      FUpdatePending := False;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerProject.ModuleChanged(Module: TLocalizerModule);
begin
  if (FUpdateCount = 0) and (Assigned(FOnModuleChanged)) then
    FOnModuleChanged(Module);

  Changed;
end;

// -----------------------------------------------------------------------------

function TLocalizerProject.GetStatusCount(Status: TLocalizerItemStatus): integer;
begin
  Result := FStatusCount[Status];
end;

procedure TLocalizerProject.UpdateStatusCount(Status: TLocalizerItemStatus; Delta: integer);
begin
  if (Delta = 0) then
    Exit;

  Inc(FPropertyCount, Delta);
  Assert(FPropertyCount >= 0);

  Inc(FStatusCount[Status], Delta);
  Assert(FStatusCount[Status] >= 0);
end;

// -----------------------------------------------------------------------------

procedure TLocalizerProject.SetItemStateRecursive(State: TLocalizerItemState);
var
  Module: TLocalizerModule;
  Item: TLocalizerItem;
  Prop: TLocalizerProperty;
begin
  BeginUpdate;
  try
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
  finally
    EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

function TLocalizerProject.Traverse(Delegate: TLocalizerModuleDelegate; Sorted: boolean): boolean;
var
  SortedModules: TArray<TLocalizerModule>;
  Module: TLocalizerModule;
begin
  Result := True;

  SortedModules := Modules.Values.ToArray;

  if (Sorted) then
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

function TLocalizerProject.Traverse(Delegate: TLocalizerItemDelegate; Sorted: boolean): boolean;
begin
  Result := Traverse(
    function(Module: TLocalizerModule): boolean
    begin
      Result := Module.Traverse(Delegate, Sorted);
    end, Sorted);
end;

function TLocalizerProject.Traverse(Delegate: TLocalizerPropertyDelegate; Sorted: boolean; Kinds: TLocalizerModuleKinds): boolean;
begin
  Result := Traverse(
    function(Module: TLocalizerModule): boolean
    begin
      if (Module.Kind in Kinds) then
        Result := Module.Traverse(Delegate, Sorted)
      else
        Result := True;
    end, Sorted);
end;

// -----------------------------------------------------------------------------

function TLocalizerProject.AddModule(const AName: string; Kind: TLocalizerModuleKind): TLocalizerModule;
begin
  if (not FModules.TryGetValue(AName, Result)) then
  begin
    Result := TLocalizerModule.Create(Self, AName);
    Result.Kind := Kind;
  end else
  if (ProjectStateLoading in State) then
    Result.State := ItemStateExisting;
end;

function TLocalizerProject.FindModule(const AName: string): TLocalizerModule;
begin
  if (not FModules.TryGetValue(AName, Result)) then
    Result := nil;
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
  BeginUpdate;
  try
    FProject := AProject;
    FProject.Modules.Add(Name, Self);
    Changed;
  finally
    EndUpdate;
  end;
end;

destructor TLocalizerModule.Destroy;
begin
  BeginUpdate;
  try
    Clear;
    FProject.Modules.ExtractPair(Name);
    FItems.Free;
    FResourceGroups.Free;
    Changed;
  finally
    EndUpdate;
  end;
  inherited;
end;

// -----------------------------------------------------------------------------

function TLocalizerModule.Purge: boolean;
var
  Item: TLocalizerItem;
begin
  Result := False;

  BeginUpdate;
  try
    for Item in FItems.Values.ToArray do // ToArray for stability since we delete from dictionary
    begin
      if (Item.State = ItemStateUnused) then
      begin
        Item.Free;
        Result := True;
        continue;
      end;

      if (Item.Purge) then
        Result := True;

      if (Item.Properties.Count = 0) then
      begin
        Item.Free;
        Result := True;
      end;
    end;

    if (Result) then
      Changed;
  finally
    EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerModule.Clear;
begin
  BeginUpdate;
  try
    FItems.Clear;
    FResourceGroups.Clear;
  finally
    EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerModule.DoChanged;
begin
  FProject.ModuleChanged(Self);
end;

// -----------------------------------------------------------------------------

procedure TLocalizerModule.SetName(const Value: string);
begin
  if (Name = Value) then
    exit;

  BeginUpdate;
  try
    FProject.Modules.ExtractPair(Name);

    inherited SetName(Value);

    FProject.Modules.Add(Name, Self);

    Changed;
  finally
    EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerModule.UpdateParentStatusCount(AStatus: TLocalizerItemStatus; Delta: integer);
begin
  if (Delta = 0) then
    Exit;

  if (State <> ItemStateUnused) then
    FProject.UpdateStatusCount(AStatus, Delta);
end;

// -----------------------------------------------------------------------------

function TLocalizerModule.Traverse(Delegate: TLocalizerPropertyDelegate; Sorted: boolean): boolean;
begin
  Result := Traverse(
    function(Item: TLocalizerItem): boolean
    begin
      Result := Item.Traverse(Delegate, Sorted);
    end, Sorted);
end;

function TLocalizerModule.Traverse(Delegate: TLocalizerItemDelegate; Sorted: boolean): boolean;
var
  SortedItems: TArray<TLocalizerItem>;
  Item: TLocalizerItem;
begin
  Result := True;

  SortedItems := Items.Values.ToArray;

  if (Sorted) then
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

function TLocalizerModule.FindItem(const AName: string): TLocalizerItem;
begin
  if (not FItems.TryGetValue(AName, Result)) then
    Result := nil;
end;

// -----------------------------------------------------------------------------

function TLocalizerModule.AddItem(const AName, ATypeName: string): TLocalizerItem;
begin
  if (not FItems.TryGetValue(AName, Result)) then
    Result := TLocalizerItem.Create(Self, AName, ATypeName)
  else
  if (ProjectStateLoading in Project.State) then
    Result.State := ItemStateExisting;
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
    Result.State := ItemStateExisting;
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
  BeginUpdate;
  try
    FTypeName := ATypeName;
    Module.Items.Add(Name, Self);
    Changed;
  finally
    EndUpdate;
  end;
end;

destructor TLocalizerItem.Destroy;
begin
  BeginUpdate;
  try
    Clear;
    Module.Items.ExtractPair(Name);
    FProperties.Free;
    Changed;
  finally
    EndUpdate;
  end;
  inherited;
end;

function TLocalizerItem.Purge: boolean;
var
  Prop: TLocalizerProperty;
begin
  Result := False;
  BeginUpdate;
  try

    for Prop in Properties.Values.ToArray do // ToArray for stability since we delete from dictionary
      if (Prop.State = ItemStateUnused) then
      begin
        Prop.Free;
        Result := True;
      end;

    if (Result) then
      Changed;
  finally
    EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerItem.Clear;
begin
  BeginUpdate;
  try

    FProperties.Clear;

  finally
    EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerItem.SetName(const Value: string);
begin
  if (Name = Value) then
    exit;

  BeginUpdate;
  try
    Module.Items.ExtractPair(Name);

    inherited SetName(Value);

    Module.Items.Add(Name, Self);

    Changed;
  finally
    EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

function TLocalizerItem.Traverse(Delegate: TLocalizerPropertyDelegate; Sorted: boolean): boolean;
var
  SortedProps: TArray<TLocalizerProperty>;
  Prop: TLocalizerProperty;
begin
  Result := True;

  SortedProps := Properties.Values.ToArray;

  if (Sorted) then
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

function TLocalizerItem.FindProperty(const AName: string): TLocalizerProperty;
begin
  if (not FProperties.TryGetValue(AName, Result)) then
    Result := nil;
end;

// -----------------------------------------------------------------------------

function TLocalizerItem.AddProperty(const AName: string): TLocalizerProperty;
begin
  if (not FProperties.TryGetValue(AName, Result)) then
    Result := TLocalizerProperty.Create(Self, AName)
  else
  if (ProjectStateLoading in Module.Project.State) then
    Result.State := ItemStateExisting;
end;

function TLocalizerItem.AddProperty(const AName, AValue: string): TLocalizerProperty;
begin
  BeginUpdate;
  try
    Result := AddProperty(AName);
    Result.Value := AValue;
  finally
    EndUpdate;
  end;
end;


// -----------------------------------------------------------------------------
//
// TLocalizerProperty
//
// -----------------------------------------------------------------------------
constructor TLocalizerProperty.Create(AItem: TLocalizerItem; const AName: string);
begin
  inherited Create(AItem, AName);
  BeginUpdate;
  try
    Item.Properties.Add(Name, Self);
    FTranslations := TLocalizerTranslations.Create(Self);
    UpdateStatusCount(Status, 1);
    Changed;
  finally
    EndUpdate;
  end;
end;

destructor TLocalizerProperty.Destroy;
begin
  BeginUpdate;
  try
    UpdateStatusCount(Status, -1);
    Clear;
    Item.Properties.ExtractPair(Name);
    FTranslations.Free;
    Changed;
  finally
    EndUpdate;
  end;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerProperty.Clear;
begin
  BeginUpdate;
  try

    FTranslations.Clear;

  finally
    EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerProperty.DoSetStatus(const Value: TLocalizerItemStatus);
begin
  ApplyDirectStatusChange(Value);
end;

// -----------------------------------------------------------------------------

procedure TLocalizerProperty.SetTranslatedValue(Language: TTargetLanguage; const Value: string);
begin
  Translations.AddOrUpdateTranslation(Language, Value);
end;

function TLocalizerProperty.GetTranslatedValue(Language: TTargetLanguage): string;
var
  Translation: TLocalizerTranslation;
begin
  if (Translations.TryGetTranslation(Language, Translation)) and (Translation.Status <> tStatusObsolete) then
    Result := Translation.Value
  else
    Result := Value;
end;

// -----------------------------------------------------------------------------

function TLocalizerProperty.HasTranslation(Language: TTargetLanguage): boolean;
var
  Translation: TLocalizerTranslation;
begin
  Result := (Translations.TryGetTranslation(Language, Translation)) and (Translation.Status <> tStatusObsolete);
end;

// -----------------------------------------------------------------------------

procedure TLocalizerProperty.SetValue(const Value: string);
var
  Translation: TPair<TTargetLanguage, TLocalizerTranslation>;
begin
  if (FValue = Value) then
    exit;

  BeginUpdate;
  try

    // If value changes then all existing translations are obsolete
    for Translation in FTranslations.Translations do
      if (Translation.Value.IsTranslated) then
        Translation.Value.Status := tStatusObsolete;

    FValue := Value;

    Changed;

  finally
    EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

function TLocalizerProperty.Traverse(Delegate: TLocalizerPropertyDelegate; Sorted: boolean): boolean;
begin
  Result := Delegate(Self);
end;

function TLocalizerProperty.Traverse(Delegate: TLocalizerTranslationDelegate): boolean;
var
  Translation: TPair<TTargetLanguage, TLocalizerTranslation>;
begin
  Result := True;

  for Translation in FTranslations.Translations do
    if (not Delegate(Self, Translation.Value)) then
      Exit(False);
end;


// -----------------------------------------------------------------------------
//
// TLocalizerTranslation
//
// -----------------------------------------------------------------------------
constructor TLocalizerTranslation.Create(AOwner: TLocalizerProperty; ALanguage: TTargetLanguage);
begin
  inherited Create;
  FOwner := AOwner;
  FLanguage := ALanguage;
//  FLanguage.Translations.Add(Self);
end;

destructor TLocalizerTranslation.Destroy;
begin
//  FLanguage.Translations.Remove(Self);

  if (IsTranslated) then
    Dec(FLanguage.FTranslatedCount);

  inherited;
end;

// -----------------------------------------------------------------------------

function TLocalizerTranslation.GetIsTranslated: boolean;
begin
  Result := (FStatus > tStatusPending);
end;

// -----------------------------------------------------------------------------

procedure TLocalizerTranslation.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TLocalizerTranslation.EndUpdate;
begin
  Dec(FUpdateCount);
  if (FChanged) and (FUpdateCount = 0) then
  begin
    Owner.Changed;
    FChanged := False;
  end;
end;

procedure TLocalizerTranslation.Changed;
begin
  BeginUpdate;
  FChanged := True;
  EndUpdate;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerTranslation.SetStatus(const Value: TTranslationStatus);
var
  OldStatus: TTranslationStatus;
begin
  if (FStatus = Value) then
    Exit;

  BeginUpdate;
  try

    OldStatus := FStatus;

    FStatus := Value;

    if (OldStatus <= tStatusPending) and (Value > tStatusPending) then
      Inc(FLanguage.FTranslatedCount)
    else
    if (OldStatus > tStatusPending) and (Value <= tStatusPending) then
    begin
      Dec(FLanguage.FTranslatedCount);
      FWarnings := [];
    end;

    Changed;

  finally
    EndUpdate;
  end;
end;

procedure TLocalizerTranslation.SetValue(const Value: string);
begin
  if (FValue = Value) then
    Exit;

  FValue := Value;
  UpdateWarnings;

  Changed;
end;

procedure TLocalizerTranslation.Update(const AValue: string; AStatus: TTranslationStatus);
begin
  if (FValue = AValue) and (FStatus = AStatus) then
    Exit;

  BeginUpdate;
  try

    FValue := AValue;
    SetStatus(AStatus);

    Changed;

  finally
    EndUpdate;
  end;
end;


type
  TAcceleratorState = (asNone, asStart);
  TFormatState = (fsNone, fsStart, fsNumber, fsIndex, fsJustification, fsWidth, fsDot, fsPrecision, fsType);

procedure TLocalizerTranslation.UpdateWarnings;

  procedure Count(const Value: string; var CountAccelerator, CountFormat, CountLineBreak: integer);
  const
    cHotkeyPrefix = '&'; // From menus.pas
  var
    i: integer;
    c, LastChar: Char;
    SkipAccelerator, SkipFormat: boolean;
    AcceleratorState: TAcceleratorState;
    FormatState: TFormatState;
  begin
    CountAccelerator := 0;
    CountFormat := 0;
    CountLineBreak := 0;

    LastChar := #0;
    SkipAccelerator := False;
    SkipFormat := False;
    AcceleratorState := asNone;
    FormatState := fsNone;

    for i := 1 to Length(Value) do
    begin
      c := Value[i];
      if (IsLeadChar(c)) then
      begin
        LastChar := c;
        continue;
      end;

      if (c = #13) then
      begin
        Inc(CountLineBreak);
        SkipAccelerator := True;
      end else
      if (c = #10) then
      begin
        if (LastChar <> #13) then
          Inc(CountLineBreak);
        SkipAccelerator := True;
      end;

      if (not SkipAccelerator) then
      begin
        if (c = cHotkeyPrefix) then
        begin
          case AcceleratorState of
            asNone:
              AcceleratorState := asStart;
            asStart:
              AcceleratorState := asNone; // Escaped &&
          end;
        end else
        if (AcceleratorState = asStart) then
        begin
          AcceleratorState := asNone;
          if (c = ' ') then
            SkipAccelerator := True // Very common to have a space after & but highly unlikely that space is an accelerator key
          else
          if (CountAccelerator = 0) then
            Inc(CountAccelerator) // Only one allowed per caption
          else
            SkipAccelerator := True;
        end;
      end;

      if (not SkipFormat) then
      begin
        // Format string state machine
        case c of
          '%':
            case FormatState of
              fsNone:
                FormatState := fsStart;
              fsStart:
                FormatState := fsNone;          // Escaped: %%
            else
              SkipFormat := True;
            end;

          '0'..'9':
            case FormatState of
              fsNone:
                ; // skip
              fsStart,
              fsNumber:
                FormatState := fsNumber;
              fsIndex,
              fsWidth,
              fsJustification:
                FormatState := fsWidth;
              fsDot:
                FormatState := fsPrecision;
            else
              SkipFormat := True;
            end;

          ':':
            case FormatState of
              fsNone:
                ; // skip
              fsNumber:
                FormatState := fsIndex;
            else
              SkipFormat := True; // Invalid
            end;

          '-':
            case FormatState of
              fsNone:
                ; // skip
              fsStart,
              fsIndex:
                FormatState := fsJustification;
            else
              SkipFormat := True; // Invalid
            end;

          '.':
            case FormatState of
              fsNone:
                ; // skip
              fsStart,
              fsIndex,
              fsJustification,
              fsWidth:
                FormatState := fsDot;
            else
              SkipFormat := True; // Invalid
            end;

          'd', 'u', 'e', 'f', 'g', 'n', 'm', 'p', 's', 'x':
            case FormatState of
              fsNone:
                ; // skip
              fsStart,
              fsIndex,
              fsJustification,
              fsWidth,
              fsPrecision:
                begin
                  Inc(CountFormat);
                  FormatState := fsNone; // Got one - Start over
                end
            else
              SkipFormat := True; // Invalid
            end;
        else
          case FormatState of
            fsNone:
              ;
          else
            SkipFormat := True;
          end;
        end;
      end;

      LastChar := c;
    end;

    if (AcceleratorState <> asNone) then
      // & but no chars left for accelerator key
      SkipAccelerator := True;

    if (FormatState <> fsNone) then
      SkipFormat := True;

    if (SkipAccelerator) then
      CountAccelerator := 0;

    if (SkipFormat) then
      CountFormat := 0;
  end;

var
  SourceCountAccelerator, TargetCountAccelerator: integer;
  SourceCountFormat, TargetCountFormat: integer;
  SourceCountLineBreak, TargetCountLineBreak: integer;
begin
  FWarnings := [];

  if (Value = Owner.Value) then
    Exit;

  Count(Value, TargetCountAccelerator, TargetCountFormat, TargetCountLineBreak);
  Count(Owner.Value, SourceCountAccelerator, SourceCountFormat, SourceCountLineBreak);

  if (SourceCountAccelerator <> TargetCountAccelerator) then
    Include(FWarnings, tWarningAccelerator);
  if (SourceCountFormat <> TargetCountFormat) then
    Include(FWarnings, tWarningFormatSpecifier);
  if (SourceCountLineBreak <> TargetCountLineBreak) then
    Include(FWarnings, tWarningLineBreak);
end;

// -----------------------------------------------------------------------------
//
// TLocalizerTranslations
//
// -----------------------------------------------------------------------------
constructor TLocalizerTranslations.Create(AOwner: TLocalizerProperty);
begin
  inherited Create;
  FTranslations := TObjectDictionary<TTargetLanguage, TLocalizerTranslation>.Create([doOwnsValues]);
  FOwner := AOwner;
end;

destructor TLocalizerTranslations.Destroy;
begin
  FTranslations.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

function TLocalizerTranslations.AddOrUpdateTranslation(Language: TTargetLanguage; const Value: string; Status: TTranslationStatus): TLocalizerTranslation;
begin
  if (not FTranslations.TryGetValue(Language, Result)) then
  begin
    Result := TLocalizerTranslation.Create(Owner, Language);
    FTranslations.Add(Language, Result);
  end;

  Result.Update(Value, Status);

  // TLocalizerTranslation.Update() will call Changed
  // Owner.Changed;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerTranslations.Clear;
begin
  FTranslations.Clear;
  Owner.Changed;
end;

// -----------------------------------------------------------------------------

function TLocalizerTranslations.FindTranslation(Language: TTargetLanguage): TLocalizerTranslation;
begin
  if (not FTranslations.TryGetValue(Language, Result)) then
    Result := nil;
end;

// -----------------------------------------------------------------------------

function TLocalizerTranslations.GetItem(Language: TTargetLanguage): TLocalizerTranslation;
begin
  Result := FTranslations[Language];
end;

// -----------------------------------------------------------------------------

procedure TLocalizerTranslations.Remove(Language: TTargetLanguage);
begin
  FTranslations.Remove(Language);

  Owner.Changed;
end;

// -----------------------------------------------------------------------------

function TLocalizerTranslations.TryGetTranslation(Language: TTargetLanguage; var Value: TLocalizerTranslation): boolean;
begin
  Result := FTranslations.TryGetValue(Language, Value);
end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

end.
