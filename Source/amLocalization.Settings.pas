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
  Windows,
  SysUtils,
  Classes,
  Forms,
  dxSpellChecker,
  cxCustomData,
  amRegConfig;


//------------------------------------------------------------------------------
//
//      Configuration Persistence
//
//------------------------------------------------------------------------------
var
  TranslationManagerRegistryKey: HKEY = HKEY_CURRENT_USER;
  TranslationManagerRegistryRoot: string = '\Software\Melander\TranslationManager\';

const
  TranslationManagerSettingsKeyLayout: string = '\Layout';
  TranslationManagerSettingsKeyFolder: string = '\Folders';
  TranslationManagerSettingsKeyFormMain: string = '\Forms\Debugger';
  TranslationManagerSettingsKeyDialogSupression: string = '\Dialogs\Supress';
  TranslationManagerSettingsKeyLicense: string = '\License';

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
  TTranslationManagerFolder = (tmFolderSkins, tmFolderUserSkins, tmFolderSpellCheck, tmFolderUserSpellCheck);

  TTranslationManagerFolderSettings = class(TConfigurationSection)
  strict private
    FValid: boolean;
    FRecentFiles: TConfigurationStringList;
    FRecentApplications: TConfigurationStringList;
    FFolders: array[TTranslationManagerFolder] of string;
  private
    function GetFolder(Index: TTranslationManagerFolder): string;
    procedure SetFolder(Index: TTranslationManagerFolder; const Value: string);
    class function GetDocumentFolder: string; static;
  protected
    procedure ApplyDefault; override;
    procedure ReadSection(const Key: string); override;
    procedure WriteSection(const Key: string); override;
  public
    constructor Create(AOwner: TConfigurationSection); override;
    destructor Destroy; override;

    procedure ResetSettings;

    property Folder[Index: TTranslationManagerFolder]: string read GetFolder write SetFolder;

    class property DocumentFolder: string read GetDocumentFolder;
  published
    property Valid: boolean read FValid write FValid default False;

    property FolderSkins: string index tmFolderSkins read GetFolder write SetFolder;
    property FolderUserSkins: string index tmFolderUserSkins read GetFolder write SetFolder;
    property FolderSpellCheck: string index tmFolderSpellCheck read GetFolder write SetFolder;
    property FolderUserSpellCheck: string index tmFolderUserSpellCheck read GetFolder write SetFolder;

    property RecentFiles: TConfigurationStringList read FRecentFiles;
    property RecentApplications: TConfigurationStringList read FRecentApplications;
  end;

const
  // TODO : Localization
  sFolderDisplayName: array[TTranslationManagerFolder] of string = (
    'Skins (user)',
    'Skins (system)',
    'Spell Check dictionaries (user)',
    'Spell Check dictionaries (system)'
    );

type
  TTranslationManagerTranslatorMicrosoftV3Settings = class(TConfigurationSection)
  private
    FAPIKey: string;
    FAPIKeyValidated: boolean;
  public
  published
    property APIKey: string read FAPIKey write FAPIKey;
    property APIKeyValidated: boolean read FAPIKeyValidated write FAPIKeyValidated;
  end;

  TTranslationManagerTranslatorTMX = class(TConfigurationSection)
  private
    FFilename: string;
    FLoadOnDemand: boolean;
  protected
    procedure ApplyDefault; override;
  public
  published
    property Filename: string read FFilename write FFilename;
    property LoadOnDemand: boolean read FLoadOnDemand write FLoadOnDemand;
  end;

  TTranslationManagerTranslatorSettings = class(TConfigurationSection)
  private
    FMicrosoftV3: TTranslationManagerTranslatorMicrosoftV3Settings;
    FTranslationMemory: TTranslationManagerTranslatorTMX;
  public
    constructor Create(AOwner: TConfigurationSection); override;
    destructor Destroy; override;
    procedure ResetSettings;
  published
    property MicrosoftV3: TTranslationManagerTranslatorMicrosoftV3Settings read FMicrosoftV3;
    property TranslationMemory: TTranslationManagerTranslatorTMX read FTranslationMemory;
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

  TTranslationManagerLayoutSettings = class(TConfigurationSection)
  private
    FItemTree: TTranslationManagerLayoutTreeSettings;
    FModuleTree: TTranslationManagerLayoutTreeSettings;
  public
    constructor Create(AOwner: TConfigurationSection); override;
    destructor Destroy; override;
  published
    property ModuleTree: TTranslationManagerLayoutTreeSettings read FModuleTree;
    property ItemTree: TTranslationManagerLayoutTreeSettings read FItemTree;
  end;


  TTranslationManagerSystemSettings = class(TConfigurationSection)
  private
    FSingleInstance: boolean;
    FSkin: string;
    FUseProposedStatus: boolean;
    FIncludeVersionInfo: boolean;
  published
    property SingleInstance: boolean read FSingleInstance write FSingleInstance default False;
    property Skin: string read FSkin write FSkin;
    property UseProposedStatus: boolean read FUseProposedStatus write FUseProposedStatus default True;
    property IncludeVersionInfo: boolean read FIncludeVersionInfo write FIncludeVersionInfo default True;
  end;


type
  TTranslationManagerSettings = class(TConfiguration)
  strict private
    FValid: boolean;
    FVersion: string;
    FSystem: TTranslationManagerSystemSettings;
    FForms: TTranslationManagerFormsSettings;
    FFolders: TTranslationManagerFolderSettings;
    FTranslators: TTranslationManagerTranslatorSettings;
    FProofing: TTranslationManagerProofingSettings;
    FLayout: TTranslationManagerLayoutSettings;
  private
  protected
  public
    constructor Create(Root: HKEY; const APath: string; AAccess: LongWord = KEY_ALL_ACCESS); override;
    destructor Destroy; override;
    procedure ResetSettings;
  published
    property Valid: boolean read FValid write FValid default False;
    property Version: string read FVersion write FVersion;

    property System: TTranslationManagerSystemSettings read FSystem;
    property Folders: TTranslationManagerFolderSettings read FFolders;
    property Forms: TTranslationManagerFormsSettings read FForms;
    property Translators: TTranslationManagerTranslatorSettings read FTranslators;
    property Proofing: TTranslationManagerProofingSettings read FProofing;
    property Layout: TTranslationManagerLayoutSettings read FLayout;
  end;

function TranslationManagerSettings: TTranslationManagerSettings;

implementation

uses
  IOUtils,
  Math,
  Types,
  cxPropertiesStore;

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

  FolderSkins := IncludeTrailingPathDelimiter(TPath.GetDirectoryName(Application.ExeName)) + sSkinFolder;
  FolderUserSkins := DocumentFolder + sSkinFolder;
  FolderSpellCheck := IncludeTrailingPathDelimiter(TPath.GetDirectoryName(Application.ExeName)) + sSpellCheckFolder;
  FolderUserSpellCheck := DocumentFolder + sSpellCheckFolder;
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

class function TTranslationManagerFolderSettings.GetDocumentFolder: string;
const
  sApplicationFolder = 'TranslationManager\';
begin
{$ifdef DEBUG}
  // Place stuff with .exe during development so we don't have to hunt for it
  Result := IncludeTrailingPathDelimiter(TPath.GetDirectoryName(Application.ExeName));
{$else DEBUG}
  // We should use AppData but for now Documents will do and is easier to locate
  Result := IncludeTrailingPathDelimiter(TPath.GetDocumentsPath) + sApplicationFolder;
{$endif DEBUG}
end;

function TTranslationManagerFolderSettings.GetFolder(Index: TTranslationManagerFolder): string;
begin
  Result := IncludeTrailingPathDelimiter(FFolders[Index]);
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
  FTranslators := TTranslationManagerTranslatorSettings.Create(Self);
  FProofing := TTranslationManagerProofingSettings.Create(Self);
  FLayout := TTranslationManagerLayoutSettings.Create(Self);
end;

destructor TTranslationManagerSettings.Destroy;
begin
  FSystem.Free;
  FForms.Free;
  FFolders.Free;
  FTranslators.Free;
  FProofing.Free;
  FLayout.Free;

  inherited;
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
  FScriptDebuggerSettings: TTranslationManagerSettings = nil;

function TranslationManagerSettings: TTranslationManagerSettings;
begin
  if (FScriptDebuggerSettings = nil) then
    FScriptDebuggerSettings := TTranslationManagerSettings.Create(TranslationManagerRegistryKey, TranslationManagerRegistryRoot);
  Result := FScriptDebuggerSettings;
end;

//------------------------------------------------------------------------------

{ TTranslationManagerTranslatorSettings }

constructor TTranslationManagerTranslatorSettings.Create(AOwner: TConfigurationSection);
begin
  inherited;
  FMicrosoftV3 := TTranslationManagerTranslatorMicrosoftV3Settings.Create(Self);
  FTranslationMemory := TTranslationManagerTranslatorTMX.Create(Self);
end;

destructor TTranslationManagerTranslatorSettings.Destroy;
begin
  FMicrosoftV3.Free;
  FTranslationMemory.Free;
  inherited;
end;

procedure TTranslationManagerTranslatorSettings.ResetSettings;
begin
  ApplyDefault;

  FMicrosoftV3.ApplyDefault;
  FTranslationMemory.ApplyDefault;
end;

{ TTranslationManagerTranslatorTMX }

procedure TTranslationManagerTranslatorTMX.ApplyDefault;
begin
  inherited;
  FFilename := TTranslationManagerFolderSettings.DocumentFolder + TPath.GetFileNameWithoutExtension(Application.ExeName) + '.tmx';
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
end;

destructor TTranslationManagerLayoutSettings.Destroy;
begin
  FItemTree.Free;
  FModuleTree.Free;

  inherited;
end;

{ TTranslationManagerLayoutTreeSettings }


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

initialization
//  ConfigurationServiceRegistryKey := TranslationManagerRegistryKey;
//  ConfigurationServiceRegistryRoot := TranslationManagerRegistryRoot;
finalization
  FreeAndNil(FScriptDebuggerSettings);
end.