unit amLocalization.Settings;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

// -----------------------------------------------------------------------------
//
// This unit contains settings related stuff.
//
// -----------------------------------------------------------------------------

uses
  Generics.Collections,
  Windows,
  SysUtils,
  Classes,
  Forms,
  Graphics,
  dxSpellChecker,
  cxCustomData,
  amRegConfig,
  amLocalization.ResourceWriter,
  amLocalization.Filters;


//------------------------------------------------------------------------------
//
//      Configuration Persistence
//
//------------------------------------------------------------------------------
var
  TranslationManagerRegistryKey: HKEY = HKEY_CURRENT_USER;
  TranslationManagerRegistryRoot: string = '\Software\Melander\TranslationManager\';

//------------------------------------------------------------------------------
//
//      Configuration
//
//------------------------------------------------------------------------------

type
  TCustomFormSettings = class(TConfigurationSection)
  strict private
//    FForm: TCustomForm;
    FValid: boolean;
    FMaximized: boolean;
    FVisible: boolean;
    FLeft: integer;
    FHeight: integer;
    FWidth: integer;
    FTop: integer;
  protected
    property Visible: boolean read FVisible write FVisible default True;
    property Height: integer read FHeight write FHeight default -1;
    property Width: integer read FWidth write FWidth default -1;
    property Maximized: boolean read FMaximized write FMaximized default False;
  protected
    procedure ReadSection(const Key: string); override;
    procedure WriteSection(const Key: string); override;
  public
    function ApplySettings(Form: TCustomForm): boolean;
    function PrepareSettings(Form: TCustomForm): boolean;
    procedure ResetSettings;
  published
    property Valid: boolean read FValid write FValid default False;
    property Top: integer read FTop write FTop;
    property Left: integer read FLeft write FLeft;
  end;

  TTranslationManagerFormMainSettings = class(TCustomFormSettings)
  private
  protected
  public
  published
    property Width;
    property Height;
    property Maximized;
  end;

  TTranslationManagerFormsSettings = class(TConfigurationSection)
  private
    FMain: TTranslationManagerFormMainSettings;
  public
    constructor Create(AOwner: TConfigurationSection); override;
    destructor Destroy; override;
    procedure ResetSettings;
  published
    property Main: TTranslationManagerFormMainSettings read FMain;
  end;


type
  TTranslationManagerFolder = (tmFolderAppData, tmFolderDocuments, tmFolderSkins, tmFolderUserSkins, tmFolderSpellCheck, tmFolderUserSpellCheck);

type
  TTranslationManagerFolderSettings = class(TConfigurationSection)
  strict private
    FValid: boolean;
    FRecentFiles: TConfigurationStringList;
    FRecentApplications: TConfigurationStringList;
    FFolders: array[TTranslationManagerFolder] of string;
  strict private const
    sFolderDisplayName: array[TTranslationManagerFolder] of string = ( // TODO : Localization
      'Application data',
      'Project default',
      'Skins (system)',
      'Skins (user)',
      'Spell Check dictionaries (system)',
      'Spell Check dictionaries (user)'
      );
    cFolderReadOnly: array[TTranslationManagerFolder] of boolean = (
      True,
      False,
      True,
      False,
      True,
      False
      );
  private
    function GetFolder(Index: TTranslationManagerFolder): string;
    procedure SetFolder(Index: TTranslationManagerFolder; const Value: string);
    function GetFolderName(Index: TTranslationManagerFolder): string;
    function GetFolderReadOnly(Index: TTranslationManagerFolder): boolean;
  protected
    procedure ApplyDefault; override;
    procedure ReadSection(const Key: string); override;
    procedure WriteSection(const Key: string); override;
  public
    constructor Create(AOwner: TConfigurationSection); override;
    destructor Destroy; override;

    procedure ResetSettings;

    property Folder[Index: TTranslationManagerFolder]: string read GetFolder write SetFolder;
    property FolderName[Index: TTranslationManagerFolder]: string read GetFolderName;
    property FolderReadOnly[Index: TTranslationManagerFolder]: boolean read GetFolderReadOnly;
  published
    property Valid: boolean read FValid write FValid default False;

    property FolderAppData: string index tmFolderAppData read GetFolder write SetFolder;
    property FolderDocuments: string index tmFolderDocuments read GetFolder write SetFolder;
    property FolderSkins: string index tmFolderSkins read GetFolder write SetFolder;
    property FolderUserSkins: string index tmFolderUserSkins read GetFolder write SetFolder;
    property FolderSpellCheck: string index tmFolderSpellCheck read GetFolder write SetFolder;
    property FolderUserSpellCheck: string index tmFolderUserSpellCheck read GetFolder write SetFolder;

    property RecentFiles: TConfigurationStringList read FRecentFiles;
    property RecentApplications: TConfigurationStringList read FRecentApplications;
  end;

type
  TTranslationManagerProviderMicrosoftTranslatorV3Settings = class(TConfigurationSection)
  private
    FAPIKey: string;
    FAPIKeyValidated: boolean;
  public
  published
    property APIKey: string read FAPIKey write FAPIKey;
    property APIKeyValidated: boolean read FAPIKeyValidated write FAPIKeyValidated;
  end;

  TTranslationManagerProviderMicrosoftTerminologySettings = class(TConfigurationSection)
  private
    FMaxResult: integer;
  public
  published
    property MaxResult: integer read FMaxResult write FMaxResult default 10;
  end;

  TTranslationManagerProviderTM = class(TConfigurationSection)
  private
    FFilename: string;
    FLoadOnDemand: boolean;
    FBackgroundQuery: boolean;
    FPromptToSave: boolean;
  protected
    procedure ApplyDefault; override;
  public
  published
    property Filename: string read FFilename write FFilename;
    property LoadOnDemand: boolean read FLoadOnDemand write FLoadOnDemand default True;
    property PromptToSave: boolean read FPromptToSave write FPromptToSave default False;
    property BackgroundQuery: boolean read FBackgroundQuery write FBackgroundQuery default True;
  end;

  TTranslationManagerProviderSettings = class(TConfigurationSection)
  private
    FMicrosoftV3: TTranslationManagerProviderMicrosoftTranslatorV3Settings;
    FTranslationMemory: TTranslationManagerProviderTM;
    FMicrosoftTerminology: TTranslationManagerProviderMicrosoftTerminologySettings;
  public
    constructor Create(AOwner: TConfigurationSection); override;
    destructor Destroy; override;
    procedure ResetSettings;
  published
    property MicrosoftTranslatorV3: TTranslationManagerProviderMicrosoftTranslatorV3Settings read FMicrosoftV3;
    property MicrosoftTerminology: TTranslationManagerProviderMicrosoftTerminologySettings read FMicrosoftTerminology;
    property TranslationMemory: TTranslationManagerProviderTM read FTranslationMemory;
  end;

  TTranslationManagerProofingSettings = class(TConfigurationSection)
  private
    FValid: boolean;
  public
    procedure SaveFrom(SpellChecker: TdxSpellChecker);
    procedure ApplyTo(SpellChecker: TdxSpellChecker);
  published
    property Valid: boolean read FValid write FValid;
  end;

  TTranslationManagerLayoutTreeSettings = class(TConfigurationSection)
  private
    FValid: boolean;
  public
    procedure WriteFilter(Filter: TcxDataFilterCriteria);
    procedure ReadFilter(Filter: TcxDataFilterCriteria);
  published
    property Valid: boolean read FValid write FValid;
  end;

  TTranslationManagerLayoutGridSettings = class(TConfigurationSection)
  private
    FValid: boolean;
  public
  published
    property Valid: boolean read FValid write FValid;
  end;

  TTranslationManagerLayoutSettings = class(TConfigurationSection)
  private
    FItemTree: TTranslationManagerLayoutTreeSettings;
    FModuleTree: TTranslationManagerLayoutTreeSettings;
    FTranslationMemory: TTranslationManagerLayoutGridSettings;
    FBlackList: TTranslationManagerLayoutGridSettings;
  public
    constructor Create(AOwner: TConfigurationSection); override;
    destructor Destroy; override;
  published
    property ModuleTree: TTranslationManagerLayoutTreeSettings read FModuleTree;
    property ItemTree: TTranslationManagerLayoutTreeSettings read FItemTree;
    property TranslationMemory: TTranslationManagerLayoutGridSettings read FTranslationMemory;
    property BlackList: TTranslationManagerLayoutGridSettings read FBlackList;
  end;


  TTranslationManagerBackupSettings = class(TConfigurationSection)
  private
    FAutoRecoverInterval: integer;
    FMaxCount: integer;
    FMaxSize: int64;
    FAutoRecover: boolean;
    FSaveBackups: boolean;
  public
  published
    property SaveBackups: boolean read FSaveBackups write FSaveBackups default True;
    property MaxCount: integer read FMaxCount write FMaxCount default 5;
    property MaxSize: int64 read FMaxSize write FMaxSize default 10; // In Mb
    property AutoRecover: boolean read FAutoRecover write FAutoRecover;
    property AutoRecoverInterval: integer read FAutoRecoverInterval write FAutoRecoverInterval;
  end;

  TListStyle = (ListStyleDefault, ListStyleSelected, ListStyleInactive, ListStyleFocused, ListStyleNotTranslated, ListStyleProposed, ListStyleTranslated, ListStyleHold, ListStyleDontTranslate);

  TTranslationManagerListStyleSettings = class(TConfigurationSection)
  private
    FColorText: TColor;
    FColorBackground: TColor;
    FBold: integer;
    FListStyle: TListStyle;
  protected
    function GetStyleName: string;
  public
    constructor Create(AOwner: TConfigurationSection; AListStyle: TListStyle); reintroduce;
    property Name: string read GetStyleName;
  published
    property ColorText: TColor read FColorText write FColorText default clDefault;
    property ColorBackground: TColor read FColorBackground write FColorBackground default clDefault;
    property Bold: integer read FBold write FBold default -1;
  end;

  TTranslationManagerStyleSettings = class(TConfigurationSection)
  private
    FStyles: array[TListStyle] of TTranslationManagerListStyleSettings;
    FValid: boolean;
  protected
    procedure ReadSection(const Key: string); override;
    procedure ApplyDefault; override;
    function GetStyle(Index: TListStyle): TTranslationManagerListStyleSettings;
  public
    constructor Create(AOwner: TConfigurationSection); override;
    destructor Destroy; override;

    procedure ResetSettings;

    property Styles[Style: TListStyle]: TTranslationManagerListStyleSettings read GetStyle; default;
  published
    property Valid: boolean read FValid write FValid;
    property StyleDefault: TTranslationManagerListStyleSettings index ListStyleDefault read GetStyle;
    property StyleSelected: TTranslationManagerListStyleSettings index ListStyleSelected read GetStyle;
    property StyleInactive: TTranslationManagerListStyleSettings index ListStyleInactive read GetStyle;
    property StyleFocused: TTranslationManagerListStyleSettings index ListStyleFocused read GetStyle;
    property StyleNotTranslated: TTranslationManagerListStyleSettings index ListStyleNotTranslated read GetStyle;
    property StyleProposed: TTranslationManagerListStyleSettings index ListStyleProposed read GetStyle;
    property StyleTranslated: TTranslationManagerListStyleSettings index ListStyleTranslated read GetStyle;
    property StyleHold: TTranslationManagerListStyleSettings index ListStyleHold read GetStyle;
    property StyleDontTranslate: TTranslationManagerListStyleSettings index ListStyleDontTranslate read GetStyle;
  end;

  TTranslationManagerEditorSettings = class(TConfigurationSection)
  private
    FStyle: TTranslationManagerStyleSettings;
    FDisplayStatusGlyphs: boolean;
    FStatusGlyphHints: boolean;
    FUseProposedStatus: boolean;
    FEditBiDiMode: boolean;
    FAutoApplyTranslations: boolean;
    FAutoApplyTranslationsSimilar: boolean;
  public
    constructor Create(AOwner: TConfigurationSection); override;
    destructor Destroy; override;
  published
    property Style: TTranslationManagerStyleSettings read FStyle;
    property DisplayStatusGlyphs: boolean read FDisplayStatusGlyphs write FDisplayStatusGlyphs default True;
    property StatusGlyphHints: boolean read FStatusGlyphHints write FStatusGlyphHints default False;

    property UseProposedStatus: boolean read FUseProposedStatus write FUseProposedStatus default True;
    property EditBiDiMode: boolean read FEditBiDiMode write FEditBiDiMode default True;
    property AutoApplyTranslations: boolean read FAutoApplyTranslations write FAutoApplyTranslations default True;
    property AutoApplyTranslationsSimilar: boolean read FAutoApplyTranslationsSimilar write FAutoApplyTranslationsSimilar default True;
  end;

  TTranslationManagerFilterGroupSettings = class(TConfigurationStringList)
  private
    FExpanded: boolean;
  published
    property Expanded: boolean read FExpanded write FExpanded default False;
  end;

  TTranslationManagerFiltersSettings = class(TConfigurationSectionValues<TTranslationManagerFilterGroupSettings>)
  private
    FValid: boolean;
    FFilters: TFilterItemList;
    FExpandedState: TDictionary<string, boolean>;
  protected
    function GetGroupExpanded(const Name: string): boolean;
    procedure SetGroupExpanded(const Name: string; const Value: boolean);
    procedure WriteSection(const Key: string); override;
    procedure ReadSection(const Key: string); override;
  public
    constructor Create(AOwner: TConfigurationSection); override;
    destructor Destroy; override;

    property Filters: TFilterItemList read FFilters;
    property Expanded[const Name: string]: boolean read GetGroupExpanded write SetGroupExpanded;
  published
    property Valid: boolean read FValid write FValid;
  end;

  TTranslationManagerSystemSettings = class(TConfigurationSection)
  private
    FBooting: boolean;
    FBootCount: integer;
    FFirstRun: boolean;
    FFirstRunThisVersion: boolean;
    FLastBootCompleted: boolean;
  private
    FHideFeedback: boolean;
  private
    FSingleInstance: boolean;
    FSkin: string;
    FIncludeVersionInfo: boolean;
    FModuleNameScheme: TModuleNameScheme;
    FDefaultTargetLanguage: LCID;
    FDefaultSourceLanguage: LCID;
    FApplicationLanguage: LCID;
    FSafeMode: boolean;
    FAutoApplyStopList: boolean;
    FPortable: boolean;
    FPortablePurge: boolean;
  protected
    procedure WriteSection(const Key: string); override;
    procedure ReadSection(const Key: string); override;
  public
    constructor Create(AOwner: TConfigurationSection); override;

    procedure BeginBoot;
    procedure EndBoot;

    property Booting: boolean read FBooting;
    property LastBootCompleted: boolean read FLastBootCompleted;

    property SafeMode: boolean read FSafeMode;
    property Portable: boolean read FPortable write FPortable;

    /// <summary>FirstRun is True if the application has never run before.</summary>
    property FirstRun: boolean read FFirstRun;

    /// <summary>FirstRunThisVersion is True if the current major version of the application has never run before.</summary>
    property FirstRunThisVersion: boolean read FFirstRunThisVersion;

    procedure SetSafeMode;

    // HideFeedback: Experimental. Not yet persisted
    property HideFeedback: boolean read FHideFeedback write FHideFeedback;
  published
    property SingleInstance: boolean read FSingleInstance write FSingleInstance default False;

    property Skin: string read FSkin write FSkin;

    property AutoApplyStopList: boolean read FAutoApplyStopList write FAutoApplyStopList default True;

    property IncludeVersionInfo: boolean read FIncludeVersionInfo write FIncludeVersionInfo default True;
    property ModuleNameScheme: TModuleNameScheme read FModuleNameScheme write FModuleNameScheme;

    property ApplicationLanguage: LCID read FApplicationLanguage write FApplicationLanguage;
    property DefaultSourceLanguage: LCID read FDefaultSourceLanguage write FDefaultSourceLanguage;
    property DefaultTargetLanguage: LCID read FDefaultTargetLanguage write FDefaultTargetLanguage;

    property PortablePurge: boolean read FPortablePurge write FPortablePurge;
  end;


type
  TTranslationManagerSettings = class;
  TTranslationManagerSettingsEvent = reference to procedure(Settings: TTranslationManagerSettings);

  TTranslationManagerSettings = class(TConfiguration)
  strict private class var
    FOnSettingsCreating: TTranslationManagerSettingsEvent;
    FOnSettingsDestroying: TTranslationManagerSettingsEvent;
  strict private
    FValid: boolean;
    FVersion: string;
    FSystem: TTranslationManagerSystemSettings;
    FForms: TTranslationManagerFormsSettings;
    FFolders: TTranslationManagerFolderSettings;
    FProviders: TTranslationManagerProviderSettings;
    FProofing: TTranslationManagerProofingSettings;
    FLayout: TTranslationManagerLayoutSettings;
    FBackup: TTranslationManagerBackupSettings;
    FEditor: TTranslationManagerEditorSettings;
    FFilters: TTranslationManagerFiltersSettings;
  private
    class function GetFolderInstall: string; static;
  protected
  public
    constructor Create(Root: HKEY; const APath: string; AAccess: LongWord = KEY_ALL_ACCESS); override;
    destructor Destroy; override;

    procedure ResetSettings;

    class property FolderInstall: string read GetFolderInstall;
    class property OnSettingsCreating: TTranslationManagerSettingsEvent read FOnSettingsCreating write FOnSettingsCreating;
    class property OnSettingsDestroying: TTranslationManagerSettingsEvent read FOnSettingsDestroying write FOnSettingsDestroying;
  published
    property Valid: boolean read FValid write FValid default False;
    property Version: string read FVersion write FVersion;

    property System: TTranslationManagerSystemSettings read FSystem;
    property Folders: TTranslationManagerFolderSettings read FFolders;
    property Forms: TTranslationManagerFormsSettings read FForms;
    property Providers: TTranslationManagerProviderSettings read FProviders;
    property Proofing: TTranslationManagerProofingSettings read FProofing;
    property Layout: TTranslationManagerLayoutSettings read FLayout;
    property Backup: TTranslationManagerBackupSettings read FBackup;
    property Editor: TTranslationManagerEditorSettings read FEditor;
    property Filters: TTranslationManagerFiltersSettings read FFilters;
  end;

function TranslationManagerSettings: TTranslationManagerSettings;
function TranslationManagerSettingsLoaded: boolean;


implementation

uses
  IOUtils,
  StrUtils,
  Math,
  Types,
  cxPropertiesStore,
  amVersionInfo,
  amLocalization.TranslationMemory.Data;

//------------------------------------------------------------------------------
//
//      TCustomFormSettings
//
//------------------------------------------------------------------------------
type
  TFormCracker = class(TCustomForm);

function TCustomFormSettings.ApplySettings(Form: TCustomForm): boolean;
var
  Monitor: TMonitor;
  WorkareaRect: TRect;
  NewTop, NewLeft, NewWidth, NewHeight: integer;
begin
  Result := Valid;

  if (not Result) then
    exit;

  TFormCracker(Form).SetDesigning(True, False); // To avoid RecreateWnd
  try

    TFormCracker(Form).Position := poDesigned;
    TFormCracker(Form).DefaultMonitor := dmDesktop;

  finally
    TFormCracker(Form).SetDesigning(False, False);
  end;

  // Find the monitor containing the top/left corner.
  // If the point is outside available monitors then the nearest monitor is used.
  Monitor := Screen.MonitorFromPoint(Point(Left, Top), mdNearest);

  WorkareaRect := Monitor.WorkareaRect;

  if (Height <> -1) then
    NewHeight := Min(Height, WorkareaRect.Height)
  else
    NewHeight := TFormCracker(Form).Height;
  if (Width <> -1) then
    NewWidth := Min(Width, WorkareaRect.Width)
  else
    NewWidth := TFormCracker(Form).Width;

  if (PtInRect(WorkareaRect, Point(Left, Top))) then
  begin
    NewTop := Top;
    NewLeft := Left;
  end else
  begin
    // Center on monitor if top/left is outside screen (e.g. if a monitor has been removed)
    NewTop := WorkareaRect.Top + (WorkareaRect.Height - NewHeight) div 2;
    NewLeft := WorkareaRect.Left + (WorkareaRect.Width - NewWidth) div 2;
  end;

  Form.SetBounds(NewLeft, NewTop, NewWidth, NewHeight);

(* Altering WindowState of a DevExpress ribbon form before form has been shown breaks it (RecreateWnd called).
  if (Maximized) then
    TFormCracker(Form).WindowState := wsMaximized
  else
    TFormCracker(Form).WindowState := wsNormal;
*)
end;

function TCustomFormSettings.PrepareSettings(Form: TCustomForm): boolean;
var
  wp: TWindowPlacement;
begin
  Valid := True;

  wp.length := Sizeof(wp);
  GetWindowPlacement(Form.Handle, @wp);
  Left := wp.rcNormalPosition.Left;
  Top := wp.rcNormalPosition.Top;
  Height := wp.rcNormalPosition.Bottom-Top;
  Width := wp.rcNormalPosition.Right-Left;
  Maximized := (Form.WindowState = wsMaximized);
  Visible := Form.Visible;

  Result := True;
end;

procedure TCustomFormSettings.ReadSection(const Key: string);
begin
  inherited;
  if (not Valid) then
    ResetSettings;
end;

procedure TCustomFormSettings.WriteSection(const Key: string);
begin
  if (Valid) then
    inherited;
end;

procedure TCustomFormSettings.ResetSettings;
begin
  ApplyDefault;

  Valid := False;
//  FLeft := FForm.ExplicitLeft;
//  FTop := FForm.ExplicitTop;
  FWidth := -1;
  FHeight := -1;
  FMaximized := False;
end;

//------------------------------------------------------------------------------
//
//      TTranslationManagerFolderSettings
//
//------------------------------------------------------------------------------
procedure TTranslationManagerFolderSettings.ApplyDefault;
const
  sSkinFolder = 'Skins\';
  sSpellCheckFolder = 'Dictionaries\';
begin
  inherited;

  FolderDocuments := '%DOCUMENTS%\';
  FolderAppData := '%DATA%\';

  FolderSkins := '%INSTALL%\' + sSkinFolder;
  FolderUserSkins := FolderAppData + sSkinFolder;
  FolderSpellCheck := '%INSTALL%\' + sSpellCheckFolder;
  FolderUserSpellCheck := FolderAppData + sSpellCheckFolder;
end;

constructor TTranslationManagerFolderSettings.Create(AOwner: TConfigurationSection);
begin
  inherited Create(AOwner);
  FRecentFiles := TConfigurationStringList.Create(Self);
  FRecentApplications := TConfigurationStringList.Create(Self);
end;

destructor TTranslationManagerFolderSettings.Destroy;
begin
  FRecentFiles.Free;
  FRecentApplications.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TTranslationManagerFolderSettings.ResetSettings;
begin
  ApplyDefault;

  FRecentFiles.Clear;
  FRecentApplications.Clear;
end;

procedure TTranslationManagerFolderSettings.ReadSection(const Key: string);
begin
  inherited;
  if (not Valid) then
    ResetSettings;
end;

procedure TTranslationManagerFolderSettings.WriteSection(const Key: string);
begin
  Valid := True;
  inherited;
end;

//------------------------------------------------------------------------------

function TTranslationManagerFolderSettings.GetFolder(Index: TTranslationManagerFolder): string;
begin
  Result := FFolders[Index];
  if (Result <>'') then
    Result := IncludeTrailingPathDelimiter(Result);
end;

function TTranslationManagerFolderSettings.GetFolderName(Index: TTranslationManagerFolder): string;
begin
  Result := sFolderDisplayName[Index];
end;

function TTranslationManagerFolderSettings.GetFolderReadOnly(Index: TTranslationManagerFolder): boolean;
begin
  Result := cFolderReadOnly[Index];
end;

procedure TTranslationManagerFolderSettings.SetFolder(Index: TTranslationManagerFolder; const Value: string);
begin
  FFolders[Index] := Value;
end;

//------------------------------------------------------------------------------
//
//      TTranslationManagerFormsSettings
//
//------------------------------------------------------------------------------
constructor TTranslationManagerFormsSettings.Create(AOwner: TConfigurationSection);
begin
  inherited Create(AOwner);
  FMain := TTranslationManagerFormMainSettings.Create(Self);
end;

destructor TTranslationManagerFormsSettings.Destroy;
begin
  FMain.Free;
  inherited;
end;

procedure TTranslationManagerFormsSettings.ResetSettings;
begin
  ApplyDefault;

  FMain.ResetSettings;
end;

//------------------------------------------------------------------------------
//
//      TTranslationManagerSettings
//
//------------------------------------------------------------------------------
constructor TTranslationManagerSettings.Create(Root: HKEY; const APath: string; AAccess: LongWord = KEY_ALL_ACCESS);
begin
  inherited Create(Root, APath, AAccess);

  FSystem := TTranslationManagerSystemSettings.Create(Self);
  FForms := TTranslationManagerFormsSettings.Create(Self);
  FFolders := TTranslationManagerFolderSettings.Create(Self);
  FProviders := TTranslationManagerProviderSettings.Create(Self);
  FProofing := TTranslationManagerProofingSettings.Create(Self);
  FLayout := TTranslationManagerLayoutSettings.Create(Self);
  FBackup := TTranslationManagerBackupSettings.Create(Self);
  FEditor := TTranslationManagerEditorSettings.Create(Self);
  FFilters := TTranslationManagerFiltersSettings.Create(Self);

  if (Assigned(FOnSettingsCreating)) then
    FOnSettingsCreating(Self);
end;

destructor TTranslationManagerSettings.Destroy;
begin
  if (Assigned(FOnSettingsDestroying)) then
    FOnSettingsDestroying(Self);

  FSystem.Free;
  FForms.Free;
  FFolders.Free;
  FProviders.Free;
  FProofing.Free;
  FLayout.Free;
  FBackup.Free;
  FEditor.Free;
  FFilters.Free;

  inherited;
end;


//------------------------------------------------------------------------------

class function TTranslationManagerSettings.GetFolderInstall: string;
begin
  Result := IncludeTrailingPathDelimiter(TPath.GetDirectoryName(ParamStr(0)));
end;

//------------------------------------------------------------------------------

procedure TTranslationManagerSettings.ResetSettings;
begin
  ApplyDefault;

  FForms.ResetSettings;
  FFolders.ResetSettings;
end;


//------------------------------------------------------------------------------
//
//      TranslationManagerSettings
//
//------------------------------------------------------------------------------
var
  FTranslationManagerSettings: TTranslationManagerSettings = nil;

function TranslationManagerSettingsLoaded: boolean;
begin
  Result := (FTranslationManagerSettings <> nil);
end;

function TranslationManagerSettings: TTranslationManagerSettings;
begin
  if (FTranslationManagerSettings = nil) then
  begin
    FTranslationManagerSettings := TTranslationManagerSettings.Create(TranslationManagerRegistryKey, TranslationManagerRegistryRoot);

    FTranslationManagerSettings.ReadConfig;
    if (not FTranslationManagerSettings.Valid) then
      FTranslationManagerSettings.ResetSettings;
  end;
  Result := FTranslationManagerSettings;
end;

//------------------------------------------------------------------------------

{ TTranslationManagerProviderSettings }

constructor TTranslationManagerProviderSettings.Create(AOwner: TConfigurationSection);
begin
  inherited;
  FMicrosoftV3 := TTranslationManagerProviderMicrosoftTranslatorV3Settings.Create(Self);
  FMicrosoftTerminology := TTranslationManagerProviderMicrosoftTerminologySettings.Create(Self);
  FTranslationMemory := TTranslationManagerProviderTM.Create(Self);
end;

destructor TTranslationManagerProviderSettings.Destroy;
begin
  FMicrosoftV3.Free;
  FMicrosoftTerminology.Free;
  FTranslationMemory.Free;
  inherited;
end;

procedure TTranslationManagerProviderSettings.ResetSettings;
begin
  ApplyDefault;

  FMicrosoftV3.ApplyDefault;
  FMicrosoftTerminology.ApplyDefault;
  FTranslationMemory.ApplyDefault;
end;

{ TTranslationManagerProviderTM }

procedure TTranslationManagerProviderTM.ApplyDefault;
begin
  inherited;

  FFilename := '%DATA%\' + sTranslationMemoryFilename;
end;

{ TTranslationManagerProofingSettings }

procedure TTranslationManagerProofingSettings.ApplyTo(SpellChecker: TdxSpellChecker);
var
  PropertiesStore: TcxPropertiesStore;
  PropertiesStoreComponent: TcxPropertiesStoreComponent;
  Stream: TMemoryStream;
begin
  PropertiesStore := TcxPropertiesStore.Create(nil);
  try
    PropertiesStoreComponent := PropertiesStore.Components.Add;
    PropertiesStoreComponent.Component := SpellChecker;

    PropertiesStoreComponent.Properties.Add('AutoCorrectOptions');
    PropertiesStoreComponent.Properties.Add('CheckAsYouTypeOptions');
    PropertiesStoreComponent.Properties.Add('SpellingOptions');

    PropertiesStoreComponent.RestoreFromRegistry(KeyPath);
  finally
    PropertiesStore.Free;
  end;


  if (not Valid) then
    Exit;
  Stream := TMemoryStream.Create;
  try
    ReadStream(KeyPath+'AutoCorrectOptions', 'FirstLetterExceptions', Stream);
    Stream.Position := 0;
    SpellChecker.AutoCorrectOptions.FirstLetterExceptions.LoadFromStream(Stream);
    Stream.Size := 0;

    ReadStream(KeyPath+'AutoCorrectOptions', 'InitialCapsExceptions', Stream);
    Stream.Position := 0;
    SpellChecker.AutoCorrectOptions.InitialCapsExceptions.LoadFromStream(Stream);
    Stream.Size := 0;

    ReadStream(KeyPath+'AutoCorrectOptions', 'Replacements', Stream);
    Stream.Position := 0;
    SpellChecker.AutoCorrectOptions.Replacements.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TTranslationManagerProofingSettings.SaveFrom(SpellChecker: TdxSpellChecker);
var
  PropertiesStore: TcxPropertiesStore;
  PropertiesStoreComponent: TcxPropertiesStoreComponent;
  Stream: TMemoryStream;
begin
  PropertiesStore := TcxPropertiesStore.Create(nil);
  try
    PropertiesStoreComponent := PropertiesStore.Components.Add;
    PropertiesStoreComponent.Component := SpellChecker;

    PropertiesStoreComponent.Properties.Add('AutoCorrectOptions');
    PropertiesStoreComponent.Properties.Add('CheckAsYouTypeOptions');
    PropertiesStoreComponent.Properties.Add('SpellingOptions');

    PropertiesStoreComponent.StoreToRegistry(KeyPath, False);
  finally
    PropertiesStore.Free;
  end;

  Stream := TMemoryStream.Create;
  try
    SpellChecker.AutoCorrectOptions.FirstLetterExceptions.SaveToStream(Stream);
    Stream.Position := 0;
    WriteStream(KeyPath+'AutoCorrectOptions', 'FirstLetterExceptions', Stream);
    Stream.Size := 0;

    SpellChecker.AutoCorrectOptions.InitialCapsExceptions.SaveToStream(Stream);
    Stream.Position := 0;
    WriteStream(KeyPath+'AutoCorrectOptions', 'InitialCapsExceptions', Stream);
    Stream.Size := 0;

    SpellChecker.AutoCorrectOptions.Replacements.SaveToStream(Stream);
    Stream.Position := 0;
    WriteStream(KeyPath+'AutoCorrectOptions', 'Replacements', Stream);
    Stream.Size := 0;
  finally
    Stream.Free;
  end;

  Valid := True;
end;

{ TTranslationManagerLayoutSettings }

constructor TTranslationManagerLayoutSettings.Create(AOwner: TConfigurationSection);
begin
  inherited;

  FItemTree := TTranslationManagerLayoutTreeSettings.Create(Self);
  FModuleTree := TTranslationManagerLayoutTreeSettings.Create(Self);
  FTranslationMemory := TTranslationManagerLayoutGridSettings.Create(Self);
  FBlackList := TTranslationManagerLayoutGridSettings.Create(Self);
end;

destructor TTranslationManagerLayoutSettings.Destroy;
begin
  FItemTree.Free;
  FModuleTree.Free;
  FTranslationMemory.Free;
  BlackList.Free;

  inherited;
end;

{ TTranslationManagerLayoutTreeSettings }

procedure TTranslationManagerLayoutTreeSettings.ReadFilter(Filter: TcxDataFilterCriteria);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    ReadStream(KeyPath+'Filter', '', Stream);
    Stream.Position := 0;
    if (Stream.Size > 0) then
      Filter.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TTranslationManagerLayoutTreeSettings.WriteFilter(Filter: TcxDataFilterCriteria);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Filter.SaveToStream(Stream);
    Stream.Position := 0;
    WriteStream(KeyPath+'Filter', '', Stream);
  finally
    Stream.Free;
  end;
end;

{ TTranslationManagerSystemSettings }

constructor TTranslationManagerSystemSettings.Create(AOwner: TConfigurationSection);
begin
  inherited;

  FFirstRun := True;
  FFirstRunThisVersion := True;
  FHideFeedback := True;
end;

procedure TTranslationManagerSystemSettings.BeginBoot;
begin
  Inc(FBootCount);

  if (FBootCount = 1) then
  begin
    FBooting := True;

    // Set boot marker
    Registry.WriteBoolean(Key, 'Booting', True);
  end;
end;

procedure TTranslationManagerSystemSettings.EndBoot;
begin
  Dec(FBootCount);

  if (FBootCount = 0) then
  begin
    FBooting := False;

    // Clear boot marker
    Registry.WriteBool(Key, 'Booting', False);

    // Boot complete - clear safe mode
    Registry.WriteBoolean(Key, 'SafeMode', False);
    // Set first run marker
    Registry.WriteBoolean(Key, 'HasBeenRun', True);
  end;
end;

procedure TTranslationManagerSystemSettings.ReadSection(const Key: string);
var
  SavedVersionMajor: Word;
  ThisVersionMajor: Word;
  VersionInfo: TVersionInfo;
begin
  inherited ReadSection(Key);

  FSafeMode := Registry.ReadBoolean(Key, 'SafeMode', FSafeMode);
  FLastBootCompleted := not Registry.ReadBoolean(Key, 'Booting', False);
  FFirstRun := not Registry.ReadBoolean(Key, 'HasBeenRun', False);

  SavedVersionMajor := TVersionInfo.VersionMajor(TVersionInfo.StringToVersion(TTranslationManagerSettings(Owner).Version));
  VersionInfo := TVersionInfo.Create(Application.ExeName);
  try
    ThisVersionMajor := TVersionInfo.VersionMajor(VersionInfo.FileVersion);
  finally
    VersionInfo.Free;
  end;
  FFirstRunThisVersion := (FFirstRunThisVersion) or (FFirstRun) or (ThisVersionMajor <> SavedVersionMajor);
end;

procedure TTranslationManagerSystemSettings.WriteSection(const Key: string);
begin
  inherited WriteSection(Key);

  Registry.WriteBoolean(Key, 'SafeMode', False);
  Registry.WriteBoolean(Key, 'HasBeenRun', True);
end;

procedure TTranslationManagerSystemSettings.SetSafeMode;
begin
  FSafeMode := True;
  Registry.WriteBoolean(Key, 'SafeMode', True);
end;

{ TTranslationManagerStyleSettings }

procedure TTranslationManagerStyleSettings.ApplyDefault;
begin
  inherited;

  StyleDefault.ColorText := clBlack;
  StyleDefault.ColorBackground := clWhite;
  StyleDefault.Bold := 0;

  StyleSelected.ColorText := clHighlightText;
  StyleSelected.ColorBackground := clHighlight;
  StyleSelected.Bold := -1;

  StyleInactive.ColorText := clDefault;
  StyleInactive.ColorBackground := $00C4996F;
  StyleInactive.Bold := -1;

  StyleFocused.ColorText := clHighlightText;
  StyleFocused.ColorBackground := clHighlight;
  StyleFocused.Bold := 1;

  StyleNotTranslated.ColorText := clDefault;
  StyleNotTranslated.ColorBackground := clDefault;
  StyleNotTranslated.Bold := -1;

  StyleProposed.ColorText := $00FF7900;
  StyleProposed.ColorBackground := clDefault;
  StyleProposed.Bold := -1;

  StyleTranslated.ColorText := $00FF7900;
  StyleTranslated.ColorBackground := $00FFF8F0;
  StyleTranslated.Bold := -1;

  StyleHold.ColorText := clDefault;
  StyleHold.ColorBackground := $00F4F4FF;
  StyleHold.Bold := -1;

  StyleDontTranslate.ColorText := clGray;
  StyleDontTranslate.ColorBackground := $00F4F4FF;
  StyleDontTranslate.Bold := -1;
end;

constructor TTranslationManagerStyleSettings.Create(AOwner: TConfigurationSection);
var
  ListStyle: TListStyle;
begin
  inherited Create(AOwner);

  for ListStyle := Low(TListStyle) to High(TListStyle) do
    FStyles[ListStyle] := TTranslationManagerListStyleSettings.Create(Self, ListStyle);
end;

destructor TTranslationManagerStyleSettings.Destroy;
var
  ListStyle: TListStyle;
begin
  for ListStyle := Low(TListStyle) to High(TListStyle) do
    FStyles[ListStyle].Free;

  inherited;
end;

function TTranslationManagerStyleSettings.GetStyle(Index: TListStyle): TTranslationManagerListStyleSettings;
begin
  Result := FStyles[Index];
end;

procedure TTranslationManagerStyleSettings.ReadSection(const Key: string);
begin
  inherited ReadSection(Key);

  if (not Valid) then
    ApplyDefault;

  Valid := True;
end;

procedure TTranslationManagerStyleSettings.ResetSettings;
begin
  ApplyDefault;
end;

{ TTranslationManagerEditorSettings }

constructor TTranslationManagerEditorSettings.Create(AOwner: TConfigurationSection);
begin
  inherited Create(AOwner);
  FStyle := TTranslationManagerStyleSettings.Create(Self);
end;

destructor TTranslationManagerEditorSettings.Destroy;
begin
  FStyle.Free;
  inherited;
end;

{ TTranslationManagerListStyleSettings }

constructor TTranslationManagerListStyleSettings.Create(AOwner: TConfigurationSection; AListStyle: TListStyle);
begin
  inherited Create(AOwner);
  FListStyle := AListStyle;
end;

function TTranslationManagerListStyleSettings.GetStyleName: string;
const
  // TODO : Localization
  sStyleNames: array[TListStyle] of string = (
    'Default',
    'Selected',
    'Inactive',
    'Focused',
    'Not translated',
    'Proposed',
    'Translated',
    'Hold',
    'Don''t translate'
  );
begin
  Result := sStyleNames[FListStyle];
end;

{ TTranslationManagerFiltersSettings }

constructor TTranslationManagerFiltersSettings.Create(AOwner: TConfigurationSection);
begin
  inherited Create(AOwner);
  PurgeOnWrite := True;
  FFilters := TFilterItemList.Create;
  FExpandedState := TDictionary<string, boolean>.Create;
end;

destructor TTranslationManagerFiltersSettings.Destroy;
begin
  FFilters.Free;
  FExpandedState.Free;
  inherited;
end;

function TTranslationManagerFiltersSettings.GetGroupExpanded(const Name: string): boolean;
var
  Group: string;
begin
  Group := Name;
  if (Group = '') then
    Group := sFilterGroupGeneral;
  if (not FExpandedState.TryGetValue(Group, Result)) then
    Result := False;
end;

procedure TTranslationManagerFiltersSettings.ReadSection(const Key: string);

  procedure StringToFilter(const Group, Value: string);
  var
    Values: TArray<string>;
    s: string;
    Filter: TFilterItem;
    n, Start, Next: integer;
  begin
    Filter := TFilterItem.Create;
    FFilters.Add(Filter);

    s := Group;
    if (s = sFilterGroupGeneral) then
      s := '';
    Filter.Group := s;

    SetLength(Values, 4);
    n := 0;
    Start := 1;
    while (n < Length(Values)) do
    begin
      if (n < Length(Values)-1) then
        Next := PosEx('/', Value, Start)
      else
        Next := MaxInt-1;

      if (Next > Start) then
        Values[n] := Copy(Value, Start, Next-Start)
      else
        Values[n] := '';

      Inc(n);
      Start := Next + 1;
    end;

    Filter.Enabled := boolean(StrToInt(Values[0]));
    Filter.Field := TFilterField(StrToInt(Values[1]));
    Filter.FilterOperator := TFilterOperator(StrToInt(Values[2]));
    Filter.Value := Values[3];
  end;

var
  i, j: integer;
  Group: string;
  GroupSection: TTranslationManagerFilterGroupSettings;
begin
  inherited ReadSection(Key);

  FFilters.Clear;
  FExpandedState.Clear;

  if (not Valid) then
  begin
    // Add a few filters just to get us going
    // Don't translate
    StringToFilter('', '1/3/0/Font.Name');
    StringToFilter('', '1/4/0/TAction.Category');
    StringToFilter('', '1/5/1/Lorem ipsum');
    StringToFilter('DevExpress', '1/2/0/TdxLayoutEmptySpaceItem');
    StringToFilter('DevExpress', '1/2/0/TdxLayoutSeparatorItem');
    StringToFilter('DevExpress', '1/2/0/TcxImageList');
    StringToFilter('DevExpress', '1/3/0/Properties.KeyFieldNames');
    StringToFilter('DevExpress', '1/3/0/DataBinding.ValueType');
    Exit;
  end;

  for i := 0 to Count-1 do
  begin
    Group := Names[i];
    GroupSection := Items[Group];

    for j := 0 to GroupSection.Count-1 do
      StringToFilter(Group, GroupSection[j]);

    if (Group = sFilterGroupGeneral) then
      Group := '';
    FExpandedState.AddOrSetValue(Group, GroupSection.Expanded);
  end;
end;

procedure TTranslationManagerFiltersSettings.SetGroupExpanded(const Name: string; const Value: boolean);
var
  Group: string;
begin
  Group := Name;
  if (Group = '') then
    Group := sFilterGroupGeneral;
  FExpandedState.AddOrSetValue(Group, Value);
end;

procedure TTranslationManagerFiltersSettings.WriteSection(const Key: string);

  function FilterToString(Filter: TFilterItem): string;
  begin
    Result := Format('%d/%d/%d/%s', [Ord(Filter.Enabled), Ord(Filter.Field), Ord(Filter.FilterOperator), Filter.Value]);
  end;

var
  Filter: TFilterItem;
  Group: string;
  GroupSection: TTranslationManagerFilterGroupSettings;
  i: integer;
  Expanded: boolean;
begin
  Clear;

  for Filter in FFilters do
  begin
    Group := Filter.Group;
    if (Group = '') then
      Group := sFilterGroupGeneral;
    GroupSection := FindOrAdd(Group);
    GroupSection.Add(FilterToString(Filter));
  end;

  for i := 0 to Count-1 do
  begin
    Group := Names[i];
    if (Group = sFilterGroupGeneral) then
      Group := '';
    if (not FExpandedState.TryGetValue(Group, Expanded)) then
      Expanded := False;

    Items[i].Expanded := Expanded;
  end;

  FValid := True;

  inherited WriteSection(Key);
end;


initialization
//  ConfigurationServiceRegistryKey := TranslationManagerRegistryKey;
//  ConfigurationServiceRegistryRoot := TranslationManagerRegistryRoot;
finalization
  FreeAndNil(FTranslationManagerSettings);
end.