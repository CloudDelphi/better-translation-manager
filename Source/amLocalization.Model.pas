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
  TLocalizerTranslationDelegate = reference to function(Prop: TLocalizerProperty; Translation: TLocalizerTranslation): boolean;

  TLocalizerItemState = (lItemStateNew, lItemStateExisting, lItemStateUnused);
  TLocalizerItemStatus = (lItemStatusTranslate, lItemStatusDontTranslate, lItemStatusHold);

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
    FModules: TLocalizerModules;
    FBaseLocaleID: LCID;
    FTargetLanguages: TTargetLanguageList;
    FState: TLocalizerProjectStates;
    FLoadCount: integer;
    FSourceFilename: string;
    FPropertyCount: integer;
    FTranslatedCount: integer;
    FStatusCount: array[TLocalizerItemStatus] of integer;
    FModified: boolean;
    FUpdateCount: integer;
    FUpdatePending: boolean;
    FOnChanged: TNotifyEvent;
    FOnModuleCompleteChanged: TLocalizerModuleEvent;
  strict protected
    function GetStatusCount(Status: TLocalizerItemStatus): integer;
    procedure SetItemState(State: TLocalizerItemState);
  protected
    procedure UpdateStatusCount(Status: TLocalizerItemStatus; Delta: integer);
    procedure UpdateTranslatedCount(Delta: integer);
    procedure ModuleCompleteChanged(Module: TLocalizerModule);
  public
    constructor Create(const AName: string; ABaseLocaleID: LCID);
    destructor Destroy; override;

    procedure Changed;

    procedure Clear;
    function Purge: boolean;

    function AddModule(const AName: string): TLocalizerModule;

    procedure BeginLoad;
    procedure EndLoad;

    procedure BeginUpdate;
    procedure EndUpdate;

    function Traverse(Delegate: TLocalizerModuleDelegate): boolean; overload;
    function Traverse(Delegate: TLocalizerItemDelegate): boolean; overload;
    function Traverse(Delegate: TLocalizerPropertyDelegate; Kinds: TLocalizerModuleKinds = [mkForm, mkString]): boolean; overload;

    property State: TLocalizerProjectStates read FState;

    property Name: string read FName write FName;
    property SourceFilename: string read FSourceFilename write FSourceFilename;
    property BaseLocaleID: LCID read FBaseLocaleID write FBaseLocaleID;
    property TargetLanguages: TTargetLanguageList read FTargetLanguages;
    property Modules: TLocalizerModules read FModules;
    property Modified: boolean read FModified write FModified;

    // Total number of properties with State<>lItemStateUnused
    property PropertyCount: integer read FPropertyCount;
    // Total number of translated properties with State<>lItemStateUnused
    property TranslatedCount: integer read FTranslatedCount;
    // Property count by status, State<>lItemStateUnused
    property StatusCount[Status: TLocalizerItemStatus]: integer read GetStatusCount;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnModuleCompleteChanged: TLocalizerModuleEvent read FOnModuleCompleteChanged write FOnModuleCompleteChanged;
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
    FStatusCount: array[TLocalizerItemStatus] of integer;
    FTranslatedCount: integer;
  strict
  private
    function GetPropertyCount: integer; protected
    procedure UpdateStatusCount(Status: TLocalizerItemStatus; Delta: integer);
    procedure UpdateParentStatusCount(Status: TLocalizerItemStatus; Delta: integer); virtual; abstract;
  private
  protected
    procedure SetName(const Value: string); virtual;
    procedure SetState(const Value: TLocalizerItemState);
    procedure SetStatus(const Value: TLocalizerItemStatus);
    function GetState: TLocalizerItemState; virtual;
    function GetStatus: TLocalizerItemStatus; virtual;
    function GetInheritParentState: boolean; virtual;
    function GetInheritParentStatus: boolean; virtual;
    function GetStatusCount(Status: TLocalizerItemStatus): integer;
    function GetComplete: boolean;

    procedure Changed; virtual; abstract;
    procedure UpdateStatusCountFromChild(Status: TLocalizerItemStatus; Delta: integer);
    procedure UpdateParentTranslatedCount(Delta: integer); virtual; abstract;
    procedure UpdateTranslatedCount(Delta: integer);
    function TranslateChildStatus(Status: TLocalizerItemStatus): TLocalizerItemStatus;
  public
    constructor Create(const AName: string);

    property Name: string read FName write SetName;

    procedure Clear; virtual; abstract;

    function Traverse(Delegate: TLocalizerPropertyDelegate): boolean; virtual; abstract;

    property State: TLocalizerItemState read GetState write SetState;
    property Status: TLocalizerItemStatus read GetStatus write SetStatus;
    property InheritParentState: boolean read GetInheritParentState;
    property InheritParentStatus: boolean read GetInheritParentStatus;

    property StatusCount[Status: TLocalizerItemStatus]: integer read GetStatusCount;
    property PropertyCount: integer read GetPropertyCount;
    property TranslatedCount: integer read FTranslatedCount;

    property Complete: boolean read GetComplete;
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
    procedure UpdateParentStatusCount(Status: TLocalizerItemStatus; Delta: integer); override;
  protected
    procedure Changed; override;
    procedure UpdateParentTranslatedCount(Delta: integer); override;
    function GetState: TLocalizerItemState; override;
    function GetStatus: TLocalizerItemStatus; override;
    function GetInheritParentState: boolean; override;
    function GetInheritParentStatus: boolean; override;

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
    FLastCompleteState: boolean;
  strict protected
    procedure SetName(const Value: string); override;
    procedure UpdateParentStatusCount(Status: TLocalizerItemStatus; Delta: integer); override;
    procedure NotifyComplete;
  protected
    procedure Changed; override;
    procedure UpdateParentTranslatedCount(Delta: integer); override;
  public
    constructor Create(AProject: TLocalizerProject; const AName: string);
    destructor Destroy; override;

    property Project: TLocalizerProject read FProject;
    property Kind: TLocalizerModuleKind read FKind write FKind;
    property Items: TLocalizerItems read FItems;

    procedure Clear; override;
    function Purge: boolean;

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
  strict private
    FResourceID: WORD;
    FTypeName: string;
    FProperties: TLocalizerProperties;
  strict protected
    procedure SetName(const Value: string); override;
  public
    constructor Create(AModule: TLocalizerModule; const AName, ATypeName: string);
    destructor Destroy; override;

    property Module: TLocalizerModule read GetParent;
    property ResourceID: WORD read FResourceID write FResourceID;
    property TypeName: string read FTypeName write FTypeName;
    property Properties: TLocalizerProperties read FProperties;

    procedure Clear; override;
    function Purge: boolean;

    function AddProperty(const AName: string): TLocalizerProperty; overload;
    function AddProperty(const AName: string; const AValue: string): TLocalizerProperty; overload;

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
    // Note:
    // Status > tStatusPending is considered "translated". Take this into consideration if changing the order.

  TLocalizerTranslation = class
  strict private
    FValue: string;
    FLanguage: TTargetLanguage;
    FStatus: TTranslationStatus;
    FOwner: TLocalizerProperty;
  strict protected
    procedure SetStatus(const Value: TTranslationStatus);
    procedure SetValue(const Value: string);
  public
    constructor Create(AOwner: TLocalizerProperty; ALanguage: TTargetLanguage);
    destructor Destroy; override;

    procedure Changed;

    property Owner: TLocalizerProperty read FOwner;

    property Value: string read FValue write SetValue;
    property Language: TTargetLanguage read FLanguage;
    property Status: TTranslationStatus read FStatus write SetStatus;
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
  public
    constructor Create(AItem: TLocalizerItem; const AName: string);
    destructor Destroy; override;

    procedure Clear; override;

    function Traverse(Delegate: TLocalizerPropertyDelegate): boolean; overload; override;
    function Traverse(Delegate: TLocalizerTranslationDelegate): boolean; reintroduce; overload;

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

function TCustomLocalizerItem.GetComplete: boolean;
begin
  Result := (FTranslatedCount = FStatusCount[lItemStatusTranslate]);
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

procedure TCustomLocalizerItem.SetState(const Value: TLocalizerItemState);
begin
  if (FState = Value) then
    Exit;

  if (FState = lItemStateUnused) then
  begin
    UpdateParentStatusCount(lItemStatusTranslate, FStatusCount[lItemStatusTranslate]);
    UpdateParentStatusCount(lItemStatusDontTranslate, FStatusCount[lItemStatusDontTranslate]);
    UpdateParentStatusCount(lItemStatusHold, FStatusCount[lItemStatusHold]);
    if (FStatus = lItemStatusTranslate) then
      UpdateParentTranslatedCount(FTranslatedCount);
  end;

  FState := Value;

  if (FState = lItemStateUnused) then
  begin
    UpdateParentStatusCount(lItemStatusTranslate, -FStatusCount[lItemStatusTranslate]);
    UpdateParentStatusCount(lItemStatusDontTranslate, -FStatusCount[lItemStatusDontTranslate]);
    UpdateParentStatusCount(lItemStatusHold, -FStatusCount[lItemStatusHold]);
    if (FStatus = lItemStatusTranslate) then
      UpdateParentTranslatedCount(-FTranslatedCount);
  end;

  Changed;
end;

procedure TCustomLocalizerItem.SetStatus(const Value: TLocalizerItemStatus);
var
  DeltaCount: array[TLocalizerItemStatus] of integer;
begin
  if (FStatus = Value) then
    Exit;

  if (FState <> lItemStateUnused) then
  begin
    if (FStatus = lItemStatusTranslate) then
      UpdateParentTranslatedCount(-FTranslatedCount);

    DeltaCount[lItemStatusTranslate] := 0;
    DeltaCount[lItemStatusDontTranslate] := 0;
    DeltaCount[lItemStatusHold] := 0;
    // Update counts as if we are performing a temporary status change TO lItemStatusTranslate
    case FStatus of
      lItemStatusTranslate:
        begin
          DeltaCount[lItemStatusTranslate] := 0;
        end;

      lItemStatusDontTranslate:
        begin
          DeltaCount[lItemStatusTranslate] := FStatusCount[lItemStatusTranslate];
          DeltaCount[lItemStatusDontTranslate] := -FStatusCount[lItemStatusTranslate]-FStatusCount[lItemStatusHold];
          DeltaCount[lItemStatusHold] := FStatusCount[lItemStatusHold];
        end;

      lItemStatusHold:
        begin
          DeltaCount[lItemStatusTranslate] := FStatusCount[lItemStatusTranslate];
          DeltaCount[lItemStatusDontTranslate] := 0;
          DeltaCount[lItemStatusHold] := -FStatusCount[lItemStatusTranslate];
        end;
    end;
  end;

  FStatus := Value;

  if (FState <> lItemStateUnused) then
  begin
    if (FStatus = lItemStatusTranslate) then
      UpdateParentTranslatedCount(FTranslatedCount);

    // Update counts as if we are performing a temporary status change FROM lItemStatusTranslate
    case FStatus of
      lItemStatusTranslate:
        ;

      lItemStatusDontTranslate:
        begin
          Inc(DeltaCount[lItemStatusTranslate], -FStatusCount[lItemStatusTranslate]);
          Inc(DeltaCount[lItemStatusDontTranslate], FStatusCount[lItemStatusTranslate]+FStatusCount[lItemStatusHold]);
          Inc(DeltaCount[lItemStatusHold], -FStatusCount[lItemStatusHold]);
        end;

      lItemStatusHold:
        begin
          Inc(DeltaCount[lItemStatusTranslate], -FStatusCount[lItemStatusTranslate]);
          Inc(DeltaCount[lItemStatusHold], FStatusCount[lItemStatusTranslate]);
        end;
    end;

    // Apply change in count
    if (DeltaCount[lItemStatusTranslate] <> 0) then
      UpdateParentStatusCount(lItemStatusTranslate, DeltaCount[lItemStatusTranslate]);
    if (DeltaCount[lItemStatusDontTranslate] <> 0) then
      UpdateParentStatusCount(lItemStatusDontTranslate, DeltaCount[lItemStatusDontTranslate]);
    if (DeltaCount[lItemStatusHold] <> 0) then
      UpdateParentStatusCount(lItemStatusHold, DeltaCount[lItemStatusHold]);
  end;

  Changed;
end;

// -----------------------------------------------------------------------------

function TCustomLocalizerItem.GetPropertyCount: integer;
begin
  Result := FStatusCount[lItemStatusTranslate];
end;

function TCustomLocalizerItem.GetStatusCount(Status: TLocalizerItemStatus): integer;
begin
  if (FState <> lItemStateUnused) then
    Result := FStatusCount[Status]
  else
    Result := 0;
end;

procedure TCustomLocalizerItem.UpdateStatusCount(Status: TLocalizerItemStatus; Delta: integer);
begin
  if (Delta = 0) then
    Exit;

  Inc(FStatusCount[Status], Delta);

  Assert(FStatusCount[Status] >= 0);

  if (FState <> lItemStateUnused) then
    UpdateParentStatusCount(Status, Delta);
end;


procedure TCustomLocalizerItem.UpdateStatusCountFromChild(Status: TLocalizerItemStatus; Delta: integer);
begin
  if (Delta = 0) then
    Exit;

  UpdateStatusCount(TranslateChildStatus(Status), Delta);
end;

procedure TCustomLocalizerItem.UpdateTranslatedCount(Delta: integer);
begin
  Inc(FTranslatedCount, Delta);
  Assert(FTranslatedCount >= 0);

  if (FState <> lItemStateUnused) and (Status = lItemStatusTranslate) then
    UpdateParentTranslatedCount(Delta);
end;

function TCustomLocalizerItem.TranslateChildStatus(Status: TLocalizerItemStatus): TLocalizerItemStatus;
begin
  // Translate status
  case FStatus of
    lItemStatusDontTranslate:
      Result := lItemStatusDontTranslate;

    lItemStatusHold:
      if (Status = lItemStatusDontTranslate) then
        Result := lItemStatusDontTranslate
      else
        Result := lItemStatusHold;
  else // i.e. lItemStatusTranslate
    Result := Status;
  end;
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

procedure TCustomLocalizerChildItem<TParentClass>.Changed;
begin
  FParent.Changed;
end;

// -----------------------------------------------------------------------------

function TCustomLocalizerChildItem<TParentClass>.GetInheritParentState: boolean;
begin
  Result := (Parent.State <> lItemStateExisting);
end;

function TCustomLocalizerChildItem<TParentClass>.GetInheritParentStatus: boolean;
begin
  Result := (inherited GetStatus <> lItemStatusDontTranslate) and (Parent.Status <> lItemStatusTranslate);
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


procedure TCustomLocalizerChildItem<TParentClass>.UpdateParentStatusCount(Status: TLocalizerItemStatus; Delta: integer);
begin
  if (Delta = 0) then
    Exit;

  FParent.UpdateStatusCountFromChild(Status, Delta);
end;


procedure TCustomLocalizerChildItem<TParentClass>.UpdateParentTranslatedCount(Delta: integer);
begin
  if (Delta = 0) then
    Exit;

  FParent.UpdateTranslatedCount(Delta);
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
    if (Module.Kind = mkOther) or (Module.State = lItemStateUnused) then
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

procedure TLocalizerProject.ModuleCompleteChanged(Module: TLocalizerModule);
begin
  if (Assigned(FOnModuleCompleteChanged)) then
    FOnModuleCompleteChanged(Module);
end;

// -----------------------------------------------------------------------------

function TLocalizerProject.GetStatusCount(Status: TLocalizerItemStatus): integer;
begin
  Result := FStatusCount[Status];
end;

procedure TLocalizerProject.UpdateStatusCount(Status: TLocalizerItemStatus; Delta: integer);
begin
  Inc(FPropertyCount, Delta);
  Assert(FPropertyCount >= 0);

  Inc(FStatusCount[Status], Delta);
  Assert(FStatusCount[Status] >= 0);
end;

procedure TLocalizerProject.UpdateTranslatedCount(Delta: integer);
begin
  Inc(FTranslatedCount, Delta);
  Assert(FTranslatedCount >= 0);
end;

// -----------------------------------------------------------------------------

procedure TLocalizerProject.SetItemState(State: TLocalizerItemState);
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
  Changed;
end;

destructor TLocalizerModule.Destroy;
begin
  FProject.Modules.ExtractPair(Name);
  FItems.Free;
  FResourceGroups.Free;
  Changed;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerModule.NotifyComplete;
var
  NewComplete: boolean;
begin
  // Notify if complete state changes
  NewComplete := Complete;

  if (NewComplete <> FLastCompleteState) then
  begin
    FLastCompleteState := NewComplete;
    FProject.ModuleCompleteChanged(Self);
  end;
end;

// -----------------------------------------------------------------------------

function TLocalizerModule.Purge: boolean;
var
  Item: TLocalizerItem;
begin
  Result := False;
  for Item in FItems.Values.ToArray do // ToArray for stability since we delete from dictionary
  begin
    if (Item.State = lItemStateUnused) then
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
end;

// -----------------------------------------------------------------------------

procedure TLocalizerModule.Clear;
begin
  FItems.Clear;
  FResourceGroups.Clear;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerModule.Changed;
begin
  FProject.Changed;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerModule.SetName(const Value: string);
begin
  if (Name = Value) then
    exit;

  FProject.Modules.ExtractPair(Name);

  inherited SetName(Value);

  FProject.Modules.Add(Name, Self);
end;

// -----------------------------------------------------------------------------

procedure TLocalizerModule.UpdateParentStatusCount(Status: TLocalizerItemStatus; Delta: integer);
begin
  if (Delta = 0) then
    Exit;

  FProject.UpdateStatusCount(Status, Delta);

  NotifyComplete;
end;

procedure TLocalizerModule.UpdateParentTranslatedCount(Delta: integer);
begin
  if (Delta = 0) then
    Exit;

  FProject.UpdateTranslatedCount(Delta);

  NotifyComplete;
end;

// -----------------------------------------------------------------------------

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
  Module.Items.Add(Name, Self);
  Changed;
end;

destructor TLocalizerItem.Destroy;
begin
  Module.Items.ExtractPair(Name);
  FProperties.Free;
  Changed;
  inherited;
end;

function TLocalizerItem.Purge: boolean;
var
  Prop: TLocalizerProperty;
begin
  Result := True;
  for Prop in Properties.Values.ToArray do // ToArray for stability since we delete from dictionary
    if (Prop.State = lItemStateUnused) then
    begin
      Prop.Free;
      Result := True;
    end;
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

function TLocalizerItem.AddProperty(const AName: string): TLocalizerProperty;
begin
  if (not FProperties.TryGetValue(AName, Result)) then
    Result := TLocalizerProperty.Create(Self, AName)
  else
  if (ProjectStateLoading in Module.Project.State) then
    Result.State := lItemStateExisting;
end;

function TLocalizerItem.AddProperty(const AName, AValue: string): TLocalizerProperty;
begin
  Result := AddProperty(AName);
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
  FTranslations := TLocalizerTranslations.Create(Self);
  UpdateStatusCount(Status, 1);
  Changed;
end;

destructor TLocalizerProperty.Destroy;
begin
  if (State <> lItemStateUnused) then
    UpdateParentStatusCount(Status, -1);
  Item.Properties.ExtractPair(Name);
  FTranslations.Free;
  Changed;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerProperty.Clear;
begin
  FTranslations.Clear;
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

procedure TLocalizerProperty.SetValue(const Value: string);
var
  Translation: TPair<TTargetLanguage, TLocalizerTranslation>;
begin
  if (FValue = Value) then
    exit;

  FValue := Value;

  // If value changes then all existing translations are obsolete
  for Translation in FTranslations.Translations do
    if (Translation.Value.Status in [tStatusProposed, tStatusTranslated]) then
      Translation.Value.Status := tStatusObsolete;

  Changed;
end;

// -----------------------------------------------------------------------------

function TLocalizerProperty.Traverse(Delegate: TLocalizerPropertyDelegate): boolean;
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

  if (FStatus > tStatusPending) then
  begin
    Dec(FLanguage.FTranslatedCount);
    FOwner.UpdateTranslatedCount(-1);
  end;

  inherited;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerTranslation.Changed;
begin
  Owner.Changed;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerTranslation.SetStatus(const Value: TTranslationStatus);
var
  OldStatus: TTranslationStatus;
begin
  if (FStatus = Value) then
    Exit;

  OldStatus := FStatus;

  FStatus := Value;

  if (OldStatus <= tStatusPending) and (Value > tStatusPending) then
  begin
    Inc(FLanguage.FTranslatedCount);
    FOwner.UpdateTranslatedCount(1);
  end else
  if (OldStatus > tStatusPending) and (Value <= tStatusPending) then
  begin
    Dec(FLanguage.FTranslatedCount);
    FOwner.UpdateTranslatedCount(-1);
  end;

  Changed;
end;

procedure TLocalizerTranslation.SetValue(const Value: string);
begin
  if (FValue = Value) then
    Exit;

  FValue := Value;
  Changed;
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

  Result.Value := Value;
  Result.Status := Status;

  Owner.Changed;
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
