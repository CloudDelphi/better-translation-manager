unit amLocalization.Dialog.Main;


interface

uses
  Generics.Collections,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, System.Actions,
  Vcl.ActnList, System.ImageList, Vcl.ImgList, Datasnap.DBClient, UITypes,
  amLocalization.Model,
  dxRibbonForm,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxRibbonSkins, dxSkinsCore,
  dxRibbonCustomizationForm, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator, dxDateRanges, Data.DB,
  cxDBData, cxGridLevel, cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, dxBar, dxSkinsForm,
  cxClasses, dxStatusBar, dxRibbonStatusBar, dxRibbon, cxTL, cxTLdxBarBuiltInMenu, cxInplaceContainer, cxLabel, cxMemo,
  cxImageComboBox, cxSplitter, cxContainer, cxTreeView, cxTextEdit, cxBlobEdit, cxImageList, cxDBExtLookupComboBox, cxMaskEdit, cxDropDownEdit, cxLookupEdit, cxDBLookupEdit,
  cxBarEditItem, cxDataControllerConditionalFormattingRulesManagerDialog, cxButtonEdit, dxSpellCheckerCore, dxSpellChecker, cxTLData;


const
  MSG_SOURCE_CHANGED = WM_USER;
  MSG_TARGET_CHANGED = WM_USER+1;

// -----------------------------------------------------------------------------
//
// TLocalizerDataSource
//
// -----------------------------------------------------------------------------
// TcxVirtualTreeList data provider for module properties
// -----------------------------------------------------------------------------
type
  TLocalizerDataSource = class(TcxTreeListCustomDataSource)
  private
    FModule: TLocalizerModule;
    FTargetLanguage: TTargetLanguage;
  protected
    procedure SetModule(const Value: TLocalizerModule);
    procedure SetTargetLanguage(const Value: TTargetLanguage);

    function GetRootRecordHandle: TcxDataRecordHandle; override;
    function GetParentRecordHandle(ARecordHandle: TcxDataRecordHandle): TcxDataRecordHandle; override;
    function GetRecordCount: Integer; override;
    function GetRecordHandle(ARecordIndex: Integer): TcxDataRecordHandle; override;

    function GetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant; override;
  public
    constructor Create(AModule: TLocalizerModule);

    property Module: TLocalizerModule read FModule write SetModule;
    property TargetLanguage: TTargetLanguage read FTargetLanguage write SetTargetLanguage;
  end;

// -----------------------------------------------------------------------------
//
// TFormMain
//
// -----------------------------------------------------------------------------
type
  TFormMain = class(TdxRibbonForm)
    OpenDialogXLIFF: TOpenDialog;
    BarManager: TdxBarManager;
    RibbonMain: TdxRibbon;
    RibbonTabMain: TdxRibbonTab;
    StatusBar: TdxRibbonStatusBar;
    SkinController: TdxSkinController;
    BarManagerBarFile: TdxBar;
    dxBarLargeButton1: TdxBarLargeButton;
    dxBarLargeButton2: TdxBarLargeButton;
    dxBarButton1: TdxBarButton;
    BarManagerBarProject: TdxBar;
    dxBarLargeButton3: TdxBarLargeButton;
    dxBarLargeButton4: TdxBarLargeButton;
    BarManagerBarImport: TdxBar;
    dxBarButton2: TdxBarButton;
    RibbonTabTranslation: TdxRibbonTab;
    TreeListItems: TcxVirtualTreeList;
    TreeListColumnItemName: TcxTreeListColumn;
    TreeListColumnValueName: TcxTreeListColumn;
    TreeListColumnID: TcxTreeListColumn;
    TreeListColumnSource: TcxTreeListColumn;
    TreeListColumnTarget: TcxTreeListColumn;
    TreeListColumnType: TcxTreeListColumn;
    TreeListColumnStatus: TcxTreeListColumn;
    TreeListColumnState: TcxTreeListColumn;
    ImageListSmall: TcxImageList;
    ImageListLarge: TcxImageList;
    ActionList: TActionList;
    ActionProjectOpen: TAction;
    ActionProjectNew: TAction;
    ActionProjectSave: TAction;
    ActionProjectUpdate: TAction;
    ActionBuild: TAction;
    ActionImportXLIFF: TAction;
    BarManagerBarLanguage: TdxBar;
    BarEditItemSourceLanguage: TcxBarEditItem;
    BarEditItemTargetLanguage: TcxBarEditItem;
    OpenDialogProject: TOpenDialog;
    dxBarButton3: TdxBarButton;
    ActionProjectPurge: TAction;
    ImageListTree: TcxImageList;
    BarManagetBarTranslationStatus: TdxBar;
    BarButton4: TdxBarButton;
    dxBarButton4: TdxBarButton;
    dxBarButton5: TdxBarButton;
    BarManagetBarTranslationState: TdxBar;
    dxBarButton6: TdxBarButton;
    dxBarButton7: TdxBarButton;
    dxBarButton8: TdxBarButton;
    ActionTranslationStatePropose: TAction;
    ActionTranslationStateAccept: TAction;
    ActionTranslationStateReject: TAction;
    ActionStatusTranslate: TAction;
    ActionStatusDontTranslate: TAction;
    ActionStatusHold: TAction;
    SpellChecker: TdxSpellChecker;
    BarManagerBarProofing: TdxBar;
    BarButtonSpellCheck: TdxBarLargeButton;
    ActionProofingCheck: TAction;
    ActionProofingLiveCheck: TAction;
    dxBarButton9: TdxBarButton;
    dxBarButton10: TdxBarButton;
    ActionProofingCheckSelected: TAction;
    RibbonTabTools: TdxRibbonTab;
    RibbonTabEdit: TdxRibbonTab;
    BarManagerBarClipboard: TdxBar;
    dxBarLargeButton5: TdxBarLargeButton;
    dxBarButton11: TdxBarButton;
    dxBarButton12: TdxBarButton;
    ActionEditPaste: TAction;
    ActionEditCopy: TAction;
    ActionEditCut: TAction;
    BarManagerBarFind: TdxBar;
    ActionFindSearch: TAction;
    ActionFindReplace: TAction;
    dxBarButton13: TdxBarButton;
    dxBarButton14: TdxBarButton;
    FindDialog: TFindDialog;
    ReplaceDialog: TReplaceDialog;
    PopupMenuTree: TdxRibbonPopupMenu;
    SplitterTreeLists: TcxSplitter;
    TreeListModules: TcxTreeList;
    TreeListColumnModuleName: TcxTreeListColumn;
    TreeListColumnModuleStatus: TcxTreeListColumn;
    BarManagerBarLookup: TdxBar;
    dxBarLargeButton6: TdxBarLargeButton;
    dxBarButton15: TdxBarButton;
    dxBarButton16: TdxBarButton;
    ActionMain: TAction;
    ActionFindNextUntranslated: TAction;
    OpenDialogEXE: TOpenDialog;
    ActionImportFile: TAction;
    StyleRepository: TcxStyleRepository;
    StyleNormal: TcxStyle;
    StyleComplete: TcxStyle;
    StyleNeedTranslation: TcxStyle;
    StyleDontTranslate: TcxStyle;
    StyleHold: TcxStyle;
    dxBarSubItem1: TdxBarSubItem;
    dxBarButton18: TdxBarButton;
    ActionImportFileSource: TAction;
    ActionImportFileTarget: TAction;
    dxBarButton19: TdxBarButton;
    BarManagerBarExport: TdxBar;
    dxBarButton17: TdxBarButton;
    dxBarButton20: TdxBarButton;
    dxBarButton21: TdxBarButton;
    dxBarButton22: TdxBarButton;
    dxBarButton23: TdxBarButton;
    dxBarButton24: TdxBarButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeListColumnStatusPropertiesEditValueChanged(Sender: TObject);
    procedure TreeListItemsEditValueChanged(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
    procedure TreeListColumnStatePropertiesEditValueChanged(Sender: TObject);
    procedure ActionProjectUpdateExecute(Sender: TObject);
    procedure ActionProjectSaveExecute(Sender: TObject);
    procedure ActionProjectNewExecute(Sender: TObject);
    procedure ActionImportXLIFFExecute(Sender: TObject);
    procedure ActionBuildExecute(Sender: TObject);
    procedure BarEditItemTargetLanguagePropertiesEditValueChanged(Sender: TObject);
    procedure BarEditItemSourceLanguagePropertiesEditValueChanged(Sender: TObject);
    procedure ActionProjectOpenExecute(Sender: TObject);
    procedure ActionProjectPurgeExecute(Sender: TObject);
    procedure ActionTranslationStateUpdate(Sender: TObject);
    procedure ActionTranslationStateProposeExecute(Sender: TObject);
    procedure ActionTranslationStateAcceptExecute(Sender: TObject);
    procedure ActionTranslationStateRejectExecute(Sender: TObject);
    procedure ActionStatusTranslateUpdate(Sender: TObject);
    procedure ActionStatusTranslateExecute(Sender: TObject);
    procedure ActionStatusDontTranslateExecute(Sender: TObject);
    procedure ActionStatusDontTranslateUpdate(Sender: TObject);
    procedure ActionStatusHoldExecute(Sender: TObject);
    procedure ActionStatusHoldUpdate(Sender: TObject);
    procedure TreeListColumnTargetPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure ActionProofingLiveCheckExecute(Sender: TObject);
    procedure ActionProofingLiveCheckUpdate(Sender: TObject);
    procedure ActionProofingCheckExecute(Sender: TObject);
    procedure SpellCheckerCheckWord(Sender: TdxCustomSpellChecker; const AWord: WideString; out AValid: Boolean; var AHandled: Boolean);
    procedure SpellCheckerSpellingComplete(Sender: TdxCustomSpellChecker; var AHandled: Boolean);
    procedure TreeListItemsEditing(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; var Allow: Boolean);
    procedure ActionProofingCheckSelectedExecute(Sender: TObject);
    procedure ActionHasItemFocusedUpdate(Sender: TObject);
    procedure BarManagerBarProofingCaptionButtons0Click(Sender: TObject);
    procedure ActionFindSearchExecute(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure ActionProjectSaveUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ActionHasProjectUpdate(Sender: TObject);
    procedure ActionHasModulesUpdate(Sender: TObject);
    procedure TreeListColumnModuleStatusPropertiesEditValueChanged(Sender: TObject);
    procedure TreeListItemsGetNodeImageIndex(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AIndexType: TcxTreeListImageIndexType; var AIndex: TImageIndex);
    procedure TreeListModulesFocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
    procedure TreeListModulesEnter(Sender: TObject);
    procedure TreeListModulesExit(Sender: TObject);
    procedure BarManagerBarLanguageCaptionButtons0Click(Sender: TObject);
    procedure BarEditItemTargetLanguagePropertiesInitPopup(Sender: TObject);
    procedure ActionMainExecute(Sender: TObject);
    procedure ActionMainUpdate(Sender: TObject);
    procedure ActionImportFileExecute(Sender: TObject);
    procedure ActionFindNextUntranslatedExecute(Sender: TObject);
    procedure TreeListModulesStylesGetContentStyle(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; var AStyle: TcxStyle);
    procedure SpellCheckerCheckAsYouTypeStart(Sender: TdxCustomSpellChecker; AControl: TWinControl; var AAllow: Boolean);
    procedure ActionImportFileSourceExecute(Sender: TObject);
    procedure ActionImportFileTargetExecute(Sender: TObject);
    procedure TreeListModulesGetNodeImageIndex(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AIndexType: TcxTreeListImageIndexType; var AIndex: TImageIndex);
  private
    FLocalizerProject: TLocalizerProject;
    FTargetLanguage: TTargetLanguage;
    FUpdateLockCount: integer;
    FSpellCheckProp: TLocalizerProperty;
    FSpellCheckingWord: boolean;
    FSpellCheckingString: boolean;
    FSpellCheckingStringResult: boolean;
    FFindFirst: boolean;
    FLocalizerDataSource: TLocalizerDataSource;
    FActiveTreeList: TcxCustomTreeList;
    FFilterTargetLanguages: boolean;
    FTranslationCounts: TDictionary<TLocalizerModule, integer>;
  protected
    function GetSourceLanguageID: Word;
    function GetTargetLanguageID: Word;
    procedure SetSourceLanguageID(const Value: Word);
    procedure SetTargetLanguageID(const Value: Word);
    function GetTargetLanguage: TTargetLanguage;
    procedure ClearTargetLanguage;

    function GetFocusedNode: TcxTreeListNode;
    function GetFocusedItem: TCustomLocalizerItem;
    function GetFocusedModule: TLocalizerModule;
    function GetFocusedProperty: TLocalizerProperty;

    property FocusedNode: TcxTreeListNode read GetFocusedNode;
    property FocusedItem: TCustomLocalizerItem read GetFocusedItem;
    property FocusedModule: TLocalizerModule read GetFocusedModule;
    property FocusedProperty: TLocalizerProperty read GetFocusedProperty;

    function NodeToItem(Node: TcxTreeListNode): TCustomLocalizerItem;
  protected
    procedure LoadProject(Project: TLocalizerProject; Clear: boolean = True);
    procedure LoadItem(Item: TCustomLocalizerItem; Recurse: boolean = False);
    procedure LoadFocusedItem(Recurse: boolean = False);
    procedure LoadModuleNode(Node: TcxTreeListNode; Recurse: boolean); overload;
    procedure LoadModuleNode(Node: TcxTreeListNode; Module: TLocalizerModule; Recurse: boolean); overload;
    procedure LoadFocusedPropertyNode;
    procedure ReloadNode(Node: TcxTreeListNode); overload;
  protected
    procedure MsgSourceChanged(var Msg: TMessage); message MSG_SOURCE_CHANGED;
    procedure MsgTargetChanged(var Msg: TMessage); message MSG_TARGET_CHANGED;
    procedure OnProjectChanged(Sender: TObject);
    procedure OnModuleChanged(Module: TLocalizerModule);
    procedure InitializeProject(const SourceFilename: string; SourceLocaleID: Word);
    procedure LockUpdates;
    procedure UnlockUpdates;
    function PerformSpellCheck(Prop: TLocalizerProperty): boolean;
    function CheckSave: boolean;
  protected
    function GetTranslatedCount(Module: TLocalizerModule): integer;
    procedure InvalidateTranslatedCount(Module: TLocalizerModule);
    procedure RemoveTranslatedCount(Module: TLocalizerModule);
  public
    property SourceLanguageID: Word read GetSourceLanguageID write SetSourceLanguageID;
    property TargetLanguageID: Word read GetTargetLanguageID write SetTargetLanguageID;
    property TargetLanguage: TTargetLanguage read GetTargetLanguage;
  end;

var
  FormMain: TFormMain;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  IOUtils,
  StrUtils,
  Generics.Defaults,
  System.Character,
  Menus,
  msxmldom,

  // DevExpress skins
  dxSkinOffice2016Colorful,

  dxHunspellDictionary,
  dxSpellCheckerDialogs,

  amCursorService,

  amLocale,
  amLocalization.Engine,
  amLocalization.ResourceWriter,
  amLocalization.Persistence,
  amLocalization.Import.XLIFF,
  amLocalization.Data.Main,
  amLocalization.Dialog.TextEdit,
  amLocalization.Dialog.NewProject,
  amLocalization.Dialog.Languages;

// -----------------------------------------------------------------------------

type
  TdxSpellCheckerCracker = class(TdxCustomSpellChecker);

// -----------------------------------------------------------------------------

function SanitizeSpellCheckText(const Value: string): string;
var
  n: integer;
begin
  // Handle & accelerator chars
  Result := StripHotkey(Value);

  // Handle Format strings
  if (Result = Value) then
  begin
    // Find first format specifier
    n := PosEx('%', Result, 1);

    while (n > 0) and (n < Length(Result)) do
    begin
      Inc(n);

      if (Result[n] = '%') then
      begin
        // Escaped % - ignore
        Delete(Result, n, 1);
      end else
      if (Result[n] in ['0'..'9', '-', '.', 'd', 'u', 'e', 'f', 'g', 'n', 'm', 'p', 's', 'x']) then
      begin
        Result[n-1] := ' '; // Replace %... with space

        // Remove chars until end of format specifier
        while (Result[n] in ['0'..'9']) do
          Delete(Result, n, 1);

        if (Result[n] = ':') then
          Delete(Result, n, 1);

        if (Result[n] = '-') then
          Delete(Result, n, 1);

        while (Result[n] in ['0'..'9']) do
          Delete(Result, n, 1);

        if (Result[n] = '.') then
          Delete(Result, n, 1);

        while (Result[n] in ['0'..'9']) do
          Delete(Result, n, 1);

        if (Result[n] in ['d', 'u', 'e', 'f', 'g', 'n', 'm', 'p', 's', 'x']) then
          Delete(Result, n, 1)
        else
          // Not a format string - undo
          Exit(Value);
      end else
        // Not a format string - undo
        Exit(Value);

      // Find next format specifier
      n := PosEx('%', Result, n);
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TreeListFindFilter(ANode: TcxTreeListNode; AData: Pointer): Boolean;
begin
  Result := (ANode.Data = AData);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.LockUpdates;
begin
  Inc(FUpdateLockCount);
  TreeListModules.BeginUpdate;
  TreeListItems.BeginUpdate;
end;

procedure TFormMain.UnlockUpdates;
begin
  ASSERT(FUpdateLockCount > 0);
  TreeListItems.EndUpdate;
  TreeListModules.EndUpdate;
  Dec(FUpdateLockCount);
end;

procedure TFormMain.ActionBuildExecute(Sender: TObject);
var
  ProjectProcessor: TProjectResourceProcessor;
  ResourceWriter: IResourceWriter;
  Filename, Path: string;
  LocaleItem: TLocaleItem;
begin
  Filename := FLocalizerProject.SourceFilename;
  LocaleItem := TLocaleItems.FindLCID(TargetLanguageID);

  if (LocaleItem <> nil) then
    Filename := TPath.ChangeExtension(Filename, '.'+LocaleItem.LanguageShortName)
  else
    Filename := TPath.ChangeExtension(Filename, '.dll');

  Path := TPath.GetDirectoryName(Filename);
  Filename := TPath.GetFileName(Filename);

  if (not PromptForFileName(Filename, '', '', 'Enter filename of resource module', Path)) then
    Exit;

  SaveCursor(crHourGlass);

  ProjectProcessor := TProjectResourceProcessor.Create;
  try
    FLocalizerProject.BeginLoad;
    try

      ResourceWriter := TResourceModuleWriter.Create(Filename);
      try

        ProjectProcessor.Execute(liaTranslate, FLocalizerProject, FLocalizerProject.SourceFilename, TargetLanguage, ResourceWriter);

      finally
        ResourceWriter := nil;
      end;

    finally
      FLocalizerProject.EndLoad;
    end;
  finally
    ProjectProcessor.Free;
  end;

  ShowMessage(Format('Resource module built:'#13'%s', [Filename]));

  LoadProject(FLocalizerProject, False);
end;

procedure TFormMain.FindDialogFind(Sender: TObject);
var
  Node: TcxTreeListNode;
  Value: string;
  StringSearchOptions: TStringSearchOptions;
begin
  SaveCursor(crAppStart);

(*
  if (FFindFirst) or (TreeList.FocusedNode = nil) then
    Node := TreeList.Root
  else
    // Skip current node
    Node := TreeList.FocusedNode.GetNext;

  while (Node <> nil) do
  begin
    if (Node.Data <> nil) and (TObject(Node.Data) is TLocalizerProperty) then
    begin
      Value := TLocalizerProperty(Node.Data).Value;

      StringSearchOptions := [soDown];
      if (frMatchCase in FindDialog.Options) then
        Include(StringSearchOptions, soMatchCase);
      if (frHideWholeWord in FindDialog.Options) then
        Include(StringSearchOptions, soWholeWord);

      if (SearchBuf(PChar(Value), Length(Value), 0, 0, FindDialog.FindText, StringSearchOptions) <> nil) then
        break;
    end;

    Node := Node.GetNext;
  end;

  if (Node <> nil) then
  begin
    Node.MakeVisible;
    Node.Focused := True;
  end else
  begin
    if (FFindFirst) then
      ShowMessage('Not found')
    else
      ShowMessage('No more found');
  end;

  FFindFirst := False;
  FindDialog.Options := FindDialog.Options + [frfindNext];
*)
end;

procedure TFormMain.ActionImportFileExecute(Sender: TObject);
begin
//
end;

procedure TFormMain.ActionImportFileSourceExecute(Sender: TObject);
var
  ProjectProcessor: TProjectResourceProcessor;
  SaveFilename: string;
begin
  if (not OpenDialogEXE.Execute(Handle)) then
    exit;

  SaveCursor(crHourGlass);

  OpenDialogEXE.InitialDir := TPath.GetDirectoryName(OpenDialogEXE.FileName);

  // Temporarily switch to new file or we will not be able to find the companion files (*.drc)
  SaveFilename := FLocalizerProject.SourceFilename;
  try
    FLocalizerProject.SourceFilename := OpenDialogEXE.FileName;

    ProjectProcessor := TProjectResourceProcessor.Create;
    try

      ProjectProcessor.ScanProject(FLocalizerProject, OpenDialogEXE.FileName);

    finally
      ProjectProcessor.Free;
    end;

    LoadProject(FLocalizerProject, False);

    StatusBar.Panels[0].Text := 'Updated';

    if (TaskMessageDlg('Update project?', 'Do you want to update the project to use this module as the source file?',
      mtConfirmation, [mbYes, mbNo], 0, mbNo) = mrYes) then
    begin
      FLocalizerProject.Name := TPath.GetFileNameWithoutExtension(FLocalizerProject.SourceFilename);
      SaveFilename := '';
      RibbonMain.DocumentName := FLocalizerProject.Name;
    end;

  finally
    if (SaveFilename <> '') then
      FLocalizerProject.SourceFilename := SaveFilename;
  end;
end;

procedure TFormMain.ActionImportFileTargetExecute(Sender: TObject);
var
  ProjectProcessor: TProjectResourceProcessor;
begin
  if (not OpenDialogEXE.Execute(Handle)) then
    exit;

  SaveCursor(crHourGlass);

  OpenDialogEXE.InitialDir := TPath.GetDirectoryName(OpenDialogEXE.FileName);

  ProjectProcessor := TProjectResourceProcessor.Create;
  try

    ProjectProcessor.Execute(liaUpdateTarget, FLocalizerProject, OpenDialogEXE.FileName, TargetLanguage, nil);

  finally
    ProjectProcessor.Free;
  end;

  LoadProject(FLocalizerProject, False);

  StatusBar.Panels[0].Text := 'Imported';
end;

procedure TFormMain.ActionFindNextUntranslatedExecute(Sender: TObject);
var
  CurrentModule: TLocalizerModule;
  CurrentProp: TLocalizerProperty;
  Found: boolean;
begin
  SaveCursor(crHourGlass);

  CurrentModule := FocusedModule;
  CurrentProp := FocusedProperty;

  Found := False;

  FLocalizerProject.Traverse(
    function(Prop: TLocalizerProperty): boolean
    var
      Translation: TLocalizerTranslation;
      ModuleNode, PropNode: TcxTreeListNode;
    begin
      // Skip until we reach the current module
      if (CurrentModule <> nil) then
      begin
        if (Prop.Item.Module = CurrentModule) then
          CurrentModule := nil
        else
          Exit(True);
      end;

      // Skip until we reach the current property
      if (CurrentProp <> nil) then
      begin
        if (Prop = CurrentProp) then
          CurrentProp := nil; // Skip current one but check next one
        Exit(True);
      end;

      if (Prop.Status <> ItemStatusTranslate) or (Prop.State = ItemStateUnused) then
        Exit(True);

      if (not Prop.Translations.TryGetTranslation(TargetLanguage, Translation)) or (Translation.Status = tStatusPending) then
      begin
        // Found - find treelist nodes
        // Select module tree node
        ModuleNode := TreeListModules.Find(Prop.Item.Module, TreeListModules.Root, False, True, TreeListFindFilter);
        if (ModuleNode = nil) or (ModuleNode.IsHidden) then
          Exit(True); // ModuleNode might have been hidden by a filter

        // Select property tree node
        PropNode := TreeListItems.NodeFromHandle(Prop);
        if (PropNode = nil) or (PropNode.IsHidden) then
          Exit(True); // PropNode might have been hidden by a filter

        ModuleNode.MakeVisible;
        ModuleNode.Focused := True;

        PropNode.MakeVisible;
        PropNode.Focused := True;

        // Done
        Found := True;
        Exit(False);
      end;

      Result := True;
    end, False);

  if (not Found) then
    ShowMessage('No more untranslated items found');
end;

procedure TFormMain.ActionFindSearchExecute(Sender: TObject);
begin
  FFindFirst := True;
  FindDialog.Options := FindDialog.Options - [frfindNext]; // Bug: frfindNext always visible regardless of option
  FindDialog.Execute(Handle);
end;

procedure TFormMain.ActionImportXLIFFExecute(Sender: TObject);
var
  Importer: TModuleImporterXLIFF;
begin
  if (not OpenDialogXLIFF.Execute(Handle)) then
    exit;

  SaveCursor(crHourGlass);

  OpenDialogXLIFF.InitialDir := TPath.GetDirectoryName(OpenDialogXLIFF.FileName);

  Importer := TModuleImporterXLIFF.Create;
  try
    Importer.LoadFromFile(FLocalizerProject, OpenDialogXLIFF.FileName);
  finally
    Importer.Free;
  end;

  FLocalizerProject.Modified := True;

  LoadProject(FLocalizerProject, False);
end;

procedure TFormMain.ActionMainExecute(Sender: TObject);
begin
//
end;

procedure TFormMain.ActionMainUpdate(Sender: TObject);
begin
  BarEditItemSourceLanguage.Enabled := (not FLocalizerProject.SourceFilename.IsEmpty);
  BarEditItemTargetLanguage.Enabled := (not FLocalizerProject.SourceFilename.IsEmpty);
  BarManagerBarLanguage.CaptionButtons[0].Enabled := (not FLocalizerProject.SourceFilename.IsEmpty);
end;

procedure TFormMain.ActionProjectNewExecute(Sender: TObject);
var
  FormNewProject: TFormNewProject;
begin
  if (not CheckSave) then
    exit;

  FormNewProject := TFormNewProject.Create(nil);
  try
    FormNewProject.SetLanguageView(DataModuleMain.GridTableViewLanguages, DataModuleMain.GridTableViewLanguagesColumnLanguage);

    if (FLocalizerProject.SourceFilename <> '') then
      FormNewProject.SourceApplication := FLocalizerProject.SourceFilename
    else
      FormNewProject.SourceApplication := Application.ExeName;

    if (FLocalizerProject.BaseLocaleID <> 0) then
      FormNewProject.SourceLanguageID := FLocalizerProject.BaseLocaleID
    else
      FormNewProject.SourceLanguageID := GetUserDefaultUILanguage;

    if (not FormNewProject.Execute) then
      exit;

    InitializeProject(FormNewProject.SourceApplication, FormNewProject.SourceLanguageID);

  finally
    FormNewProject.Free;
  end;

  LoadProject(FLocalizerProject);
end;

procedure TFormMain.ActionProjectOpenExecute(Sender: TObject);
begin
  if (not CheckSave) then
    exit;

  if (not OpenDialogProject.Execute(Handle)) then
    exit;

  SaveCursor(crHourGlass);

  OpenDialogProject.InitialDir := TPath.GetDirectoryName(OpenDialogProject.FileName);

  ClearTargetLanguage;

  TLocalizationProjectFiler.LoadFromFile(FLocalizerProject, OpenDialogProject.FileName);
  FLocalizerProject.Modified := False;

  RibbonMain.DocumentName := FLocalizerProject.Name;

  LoadProject(FLocalizerProject);
end;

procedure TFormMain.ActionProjectPurgeExecute(Sender: TObject);
var
  CurrentModule: TLocalizerModule;
  Module, LoopModule: TLocalizerModule;
  Item, LoopItem: TLocalizerItem;
  Prop: TLocalizerProperty;
  NeedReload: boolean;
  CountModule, CountItem, CountProp: integer;
  Node: TcxTreeListNode;
begin
  SaveCursor(crHourGlass);

  NeedReload := False;
  CurrentModule := FocusedModule;

  CountModule := 0;
  CountItem := 0;
  CountProp := 0;

  TreeListItems.BeginUpdate;
  try
    try

      for LoopModule in FLocalizerProject.Modules.Values.ToArray do // ToArray for stability since we delete from dictionary
      begin
        Module := LoopModule;
        Module.BeginUpdate;
        try
          if (Module.Kind = mkOther) or (Module.State = ItemStateUnused) then
          begin
            NeedReload := True;
            if (Module = CurrentModule) then
            begin
              FLocalizerDataSource.Module := nil;
  //            TreeListItems.Clear;
              CurrentModule := nil;
            end;
            Node := TreeListModules.Find(Module, TreeListModules.Root, False, True, TreeListFindFilter);
            Node.Free;
            RemoveTranslatedCount(Module);
            FreeAndNil(Module);
            Inc(CountModule);
            continue;
          end;

          for LoopItem in Module.Items.Values.ToArray do // ToArray for stability since we delete from dictionary
          begin
            Item := LoopItem;
            Item.BeginUpdate;
            try
              if (Item.State = ItemStateUnused) then
              begin
                NeedReload := True;
                if (Module = CurrentModule) then
                begin
                  FLocalizerDataSource.Module := nil;
    //              TreeListItems.Clear;
                  CurrentModule := nil;
                end;
                FreeAndNil(Item);
                Inc(CountItem);
                continue;
              end;

              // TODO : Purge obsolete translations?
              for Prop in Item.Properties.Values.ToArray do // ToArray for stability since we delete from dictionary
                if (Prop.State = ItemStateUnused) then
                begin
                  NeedReload := True;
                  if (Module = CurrentModule) then
                  begin
                    Node := TreeListItems.NodeFromHandle(Prop);
                    if (Node <> nil) then
                      Node.Free
                    else
                    begin
                      FLocalizerDataSource.Module := nil;
    //                  TreeListItems.Clear;
                      CurrentModule := nil;
                    end;
                  end;
                  Inc(CountProp);
                  Prop.Free;
                end;

              if (Item.Properties.Count = 0) then
              begin
                NeedReload := True;
                if (Module = CurrentModule) then
                begin
                  FLocalizerDataSource.Module := nil;
    //              TreeListItems.Clear;
                  CurrentModule := nil;
                end;
                FreeAndNil(Item);
                Inc(CountItem);
              end;

            finally
              if (Item <> nil) then
                Item.EndUpdate;
            end;
          end;

          if (Module.Items.Count = 0) then
          begin
            NeedReload := True;
            if (Module = CurrentModule) then
            begin
              FLocalizerDataSource.Module := nil;
  //            TreeListItems.Clear;
              CurrentModule := nil;
            end;
            Node := TreeListModules.Find(Module, TreeListModules.Root, False, True, TreeListFindFilter);
            Node.Free;
            RemoveTranslatedCount(Module);
            FreeAndNil(Module);
            Inc(CountModule);
          end;

        finally
          if (Module <> nil) then
            Module.EndUpdate;
        end;
      end;

      if (NeedReload) then
      begin
        FLocalizerProject.Modified := True;

        LoadProject(FLocalizerProject, False);
      end;

    finally
      if (TreeListItems.CustomDataSource = nil) then
        TreeListItems.CustomDataSource := FLocalizerDataSource;
    end;
  finally
    TreeListItems.EndUpdate;
  end;

  StatusBar.Panels[1].Text := Format('Purged %d modules, %d items, %d properties', [CountModule, CountItem, CountProp]);
end;

procedure TFormMain.ActionHasModulesUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FLocalizerProject.Modules.Count > 0);
end;

procedure TFormMain.ActionProjectSaveExecute(Sender: TObject);
begin
  SaveCursor(crHourGlass);

  TLocalizationProjectFiler.SaveToFile(FLocalizerProject, TPath.ChangeExtension(FLocalizerProject.SourceFilename, '.xml'));

  FLocalizerProject.Modified := False;

  StatusBar.Panels[0].Text := 'Saved';
end;

procedure TFormMain.ActionProjectSaveUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (not FLocalizerProject.Name.IsEmpty) and (FLocalizerProject.Modified);
end;

procedure TFormMain.ActionProjectUpdateExecute(Sender: TObject);
var
  ProjectProcessor: TProjectResourceProcessor;
begin
  SaveCursor(crHourGlass);

  ProjectProcessor := TProjectResourceProcessor.Create;
  try

    ProjectProcessor.ScanProject(FLocalizerProject, FLocalizerProject.SourceFilename);

  finally
    ProjectProcessor.Free;
  end;

  LoadProject(FLocalizerProject, False);

  StatusBar.Panels[0].Text := 'Updated';
end;

procedure TFormMain.ActionHasProjectUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (not FLocalizerProject.SourceFilename.IsEmpty);
end;

procedure TFormMain.ActionProofingLiveCheckExecute(Sender: TObject);
begin
  SpellChecker.CheckAsYouTypeOptions.Active := TAction(Sender).Checked;
end;

procedure TFormMain.ActionProofingLiveCheckUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := SpellChecker.CheckAsYouTypeOptions.Active;
end;

procedure TFormMain.ActionProofingCheckExecute(Sender: TObject);
begin
  SaveCursor(crAppStart);

  FLocalizerProject.Traverse(
    function(Prop: TLocalizerProperty): boolean
    begin
      Result := PerformSpellCheck(Prop);
    end, False);

  SpellChecker.ShowSpellingCompleteMessage;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionStatusDontTranslateExecute(Sender: TObject);
var
  i: integer;
  Item: TCustomLocalizerItem;
begin
  for i := 0 to FocusedNode.TreeList.SelectionCount-1 do
  begin
    Item := NodeToItem(FocusedNode.TreeList.Selections[i]);
    if (Item.Status = ItemStatusDontTranslate) then
      continue;
    Item.Status := ItemStatusDontTranslate;
    LoadItem(Item, True);
  end;
end;

procedure TFormMain.ActionStatusDontTranslateUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FocusedNode <> nil) and (FocusedItem <> nil) and (FocusedItem.State <> ItemStateUnused);

  TAction(Sender).Checked := (TAction(Sender).Enabled) and (FocusedItem.Status = ItemStatusDontTranslate);
end;

procedure TFormMain.ActionStatusHoldExecute(Sender: TObject);
var
  i: integer;
  Item: TCustomLocalizerItem;
begin
  for i := 0 to FocusedNode.TreeList.SelectionCount-1 do
  begin
    Item := NodeToItem(FocusedNode.TreeList.Selections[i]);
    if (Item.Status = ItemStatusHold) then
      continue;
    Item.Status := ItemStatusHold;
    LoadItem(Item, True);
  end;
end;

procedure TFormMain.ActionStatusHoldUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FocusedNode <> nil) and (FocusedItem <> nil) and (FocusedItem.State <> ItemStateUnused);

  TAction(Sender).Checked := (TAction(Sender).Enabled) and (FocusedItem.Status = ItemStatusHold);
end;

procedure TFormMain.ActionStatusTranslateExecute(Sender: TObject);
var
  i: integer;
  Item: TCustomLocalizerItem;
begin
  for i := 0 to FocusedNode.TreeList.SelectionCount-1 do
  begin
    Item := NodeToItem(FocusedNode.TreeList.Selections[i]);
    if (Item.Status = ItemStatusTranslate) then
      continue;
    Item.Status := ItemStatusTranslate;
    LoadItem(Item, True);
  end;
end;

procedure TFormMain.ActionStatusTranslateUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FocusedNode <> nil) and (FocusedItem <> nil) and (FocusedItem.State <> ItemStateUnused);

  TAction(Sender).Checked := (TAction(Sender).Enabled) and (FocusedItem.Status = ItemStatusTranslate);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionTranslationStateUpdate(Sender: TObject);
var
  Translation: TLocalizerTranslation;
  Prop: TLocalizerProperty;
begin
  Prop := FocusedProperty;

  TAction(Sender).Enabled := (Prop <> nil) and (Prop.State <> ItemStateUnused) and (Prop.Status <> ItemStatusDontTranslate) and
    ((not Prop.Translations.TryGetTranslation(TargetLanguage, Translation)) or
     (Translation.Status <> tStatusObsolete));
end;

procedure TFormMain.ActionTranslationStateAcceptExecute(Sender: TObject);
var
  i: integer;
  Prop: TLocalizerProperty;
  Translation: TLocalizerTranslation;
begin
  for i := 0 to TreeListItems.SelectionCount-1 do
  begin
    Prop := TLocalizerProperty(NodeToItem(TreeListItems.Selections[i]));

    if (not Prop.Translations.TryGetTranslation(TargetLanguage, Translation)) then
      Translation := Prop.Translations.AddOrUpdateTranslation(TargetLanguage, Prop.Value);

    Translation.Status := tStatusTranslated;

    LoadItem(Prop);
  end;
end;

procedure TFormMain.ActionTranslationStateProposeExecute(Sender: TObject);
var
  i: integer;
  Prop: TLocalizerProperty;
  Translation: TLocalizerTranslation;
begin
  for i := 0 to TreeListItems.SelectionCount-1 do
  begin
    Prop := TLocalizerProperty(NodeToItem(TreeListItems.Selections[i]));

    if (not Prop.Translations.TryGetTranslation(TargetLanguage, Translation)) then
      Translation := Prop.Translations.AddOrUpdateTranslation(TargetLanguage, Prop.Value);

    Translation.Status := tStatusProposed;

    LoadItem(Prop);
  end;
end;

procedure TFormMain.ActionTranslationStateRejectExecute(Sender: TObject);
var
  i: integer;
  Prop: TLocalizerProperty;
begin
  for i := 0 to TreeListItems.SelectionCount-1 do
  begin
    Prop := TLocalizerProperty(NodeToItem(TreeListItems.Selections[i]));

    Prop.Translations.Remove(TargetLanguage);

    LoadItem(Prop);
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.BarEditItemSourceLanguagePropertiesEditValueChanged(Sender: TObject);
begin
  FLocalizerProject.Modified := True;
  StatusBar.Panels[0].Text := 'Modified';
  PostMessage(Handle, MSG_SOURCE_CHANGED, 0, 0);
end;

procedure TFormMain.BarEditItemTargetLanguagePropertiesEditValueChanged(Sender: TObject);
begin
  PostMessage(Handle, MSG_TARGET_CHANGED, 0, 0);
end;

procedure TFormMain.BarEditItemTargetLanguagePropertiesInitPopup(Sender: TObject);
begin
  DataModuleMain.FilterTargetLanguages := FFilterTargetLanguages and
    ((FLocalizerProject.TargetLanguages.Count > 1) or
     ((FLocalizerProject.TargetLanguages.Count = 1) and (FLocalizerProject.TargetLanguages[0].LanguageID <> SourceLanguageID)));

  DataModuleMain.LocalizerProject := FLocalizerProject;
end;

procedure TFormMain.BarManagerBarLanguageCaptionButtons0Click(Sender: TObject);
var
  FormLanguages: TFormLanguages;
  i: integer;
  Languages: TList<integer>;
  DeleteLanguages: TList<TTargetLanguage>;
  Language: TTargetLanguage;
  PromptCount: integer;
  LocaleItem: TLocaleItem;
  Res: integer;
  Buttons: TMsgDlgButtons;
resourcestring
  sDeleteLanguageTranslationsTitle = 'Delete translations?';
  sDeleteLanguageTranslations = 'If you remove the %0:s target language you will also delete all translations made in this language from the project.'+#13#13+
    'There are currently %1:d translations in the %0:s language.'+#13#13+
    'Do you want to remove the language?';
begin
  FormLanguages := TFormLanguages.Create(nil);
  try

    FormLanguages.SourceLanguageID := SourceLanguageID;

    for i := 0 to FLocalizerProject.TargetLanguages.Count-1 do
      FormLanguages.SelectTargetLanguage(FLocalizerProject.TargetLanguages[i].LanguageID);

    FormLanguages.ApplyFilter := FFilterTargetLanguages;

    if (not FormLanguages.Execute) then
      Exit;

    FFilterTargetLanguages := FormLanguages.ApplyFilter;

    TreeListItems.BeginUpdate;
    try
      DeleteLanguages := TList<TTargetLanguage>.Create;
      try
        Languages := TList<integer>.Create;
        try

          // Build list of selected languages
          for i := 0 to FormLanguages.TargetLanguageCount-1 do
            Languages.Add(FormLanguages.TargetLanguage[i]);

          // Loop though list of current languages and build list of languages to delete
          PromptCount := 0;
          for i := FLocalizerProject.TargetLanguages.Count-1 downto 0 do
            if (not Languages.Contains(FLocalizerProject.TargetLanguages[i].LanguageID)) then
            begin
              DeleteLanguages.Add(FLocalizerProject.TargetLanguages[i]);

              if (FLocalizerProject.TargetLanguages[i].TranslatedCount > 0) then
                Inc(PromptCount);
            end;

        finally
          Languages.Free;
        end;

        if (PromptCount > 0) then
        begin
          Buttons := [mbYes, mbNo, mbCancel];
          if (PromptCount > 1) then
            Include(Buttons, mbYesToAll);
          for i := DeleteLanguages.Count-1 downto 0 do
          begin
            Language := DeleteLanguages[i];
            // Prompt user if language contains translations
            if (Language.TranslatedCount > 0) then
            begin
              LocaleItem := TLocaleItems.FindLCID(Language.LanguageID);

              Res := TaskMessageDlg(sDeleteLanguageTranslationsTitle,
                Format(sDeleteLanguageTranslations, [LocaleItem.LanguageName, Language.TranslatedCount]),
                mtConfirmation, Buttons, 0, mbNo);

              if (Res = mrCancel) then
                Exit;

              if (Res = mrYesToAll) then
                break;

              if (Res = mrNo) then
                DeleteLanguages.Delete(i);
            end;
          end;
        end;

        // Remove languages no longer in use
        SaveCursor(crHourGlass);
        for Language in DeleteLanguages do
        begin
          // If we delete current target language we must clear references to it in the GUI
          if (Language = FTargetLanguage) then
            ClearTargetLanguage;

          // Delete translations
          FLocalizerProject.Traverse(
            function(Prop: TLocalizerProperty): boolean
            begin
              Prop.Translations.Remove(Language);
              Result := True;
            end, False);

          Assert(Language.TranslatedCount = 0);

          FLocalizerProject.TargetLanguages.Remove(Language.LanguageID);
        end;

      finally
        DeleteLanguages.Free;
      end;

      // Add selected languages
      for i := 0 to FormLanguages.TargetLanguageCount-1 do
        FLocalizerProject.TargetLanguages.Add(FormLanguages.TargetLanguage[i]);

      // If we deleted current target language we must select a new one - default to source language
      if (FTargetLanguage = nil) then
        TargetLanguageID := SourceLanguageID;

    finally
      TreeListItems.EndUpdate;
    end;

  finally
    FormLanguages.Free;
  end;
end;

procedure TFormMain.BarManagerBarProofingCaptionButtons0Click(Sender: TObject);
begin
  dxShowSpellingOptionsDialog(SpellChecker);
end;

// -----------------------------------------------------------------------------

function TFormMain.CheckSave: boolean;
var
  Res: integer;
begin
  if (FLocalizerProject.Modified) then
  begin
    Res := TaskMessageDlg('Project has not been saved', 'Your changes has not been saved.'#13#13'Do you want to save them now?',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0, mbCancel);

    if (Res = mrCancel) then
      Exit(False);

    if (Res = mrYes) then
    begin
      ActionProjectSave.Execute;
      Result := (not FLocalizerProject.Modified);
    end else
      Result := True;
  end else
    Result := True;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckSave;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.FormCreate(Sender: TObject);
begin
  DisableAero := True;

  msxmldom.MSXMLDOMDocumentFactory.AddDOMProperty('ProhibitDTD', False);

  OpenDialogXLIFF.InitialDir := TPath.GetDirectoryName(Application.ExeName);
  OpenDialogProject.InitialDir := TPath.GetDirectoryName(Application.ExeName);

  FLocalizerProject := TLocalizerProject.Create('', GetUserDefaultUILanguage);
  FLocalizerProject.OnChanged := OnProjectChanged;
  FLocalizerProject.OnModuleChanged := OnModuleChanged;

  FTranslationCounts := TDictionary<TLocalizerModule, integer>.Create;

  FLocalizerDataSource := TLocalizerDataSource.Create(nil);
  TreeListItems.DataController.CustomDataSource := FLocalizerDataSource;

  DataModuleMain := TDataModuleMain.Create(Self);

  RibbonTabMain.Active := True;

  InitializeProject('', GetUserDefaultUILanguage);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  // Block notifications
  FLocalizerProject.BeginUpdate;

  FTranslationCounts.Clear;
  FLocalizerProject.Clear;

  FLocalizerDataSource.Free;
  FLocalizerProject.Free;
  FTranslationCounts.Free;
end;

// -----------------------------------------------------------------------------

function TFormMain.NodeToItem(Node: TcxTreeListNode): TCustomLocalizerItem;
begin
  if (Node.TreeList = TreeListItems) then
    Result := TLocalizerProperty(TreeListItems.HandleFromNode(Node))
  else
    Result := TLocalizerModule(Node.Data);
end;

function TFormMain.GetFocusedItem: TCustomLocalizerItem;
begin
  Result := FocusedProperty;

  if (Result = nil) then
    Result := FocusedModule;
end;

function TFormMain.GetFocusedModule: TLocalizerModule;
begin
  if (TreeListModules.FocusedNode <> nil) then
    Result := TLocalizerModule(TreeListModules.FocusedNode.Data)
  else
    Result := nil;
end;

function TFormMain.GetFocusedNode: TcxTreeListNode;
begin
  if (FActiveTreeList = TreeListItems) then
    Result := TreeListItems.FocusedNode
  else
    Result := TreeListModules.FocusedNode;
end;

function TFormMain.GetFocusedProperty: TLocalizerProperty;
begin
  if (FActiveTreeList = TreeListItems) and (TreeListItems.FocusedNode <> nil) then
    Result := TLocalizerProperty(TreeListItems.HandleFromNode(TreeListItems.FocusedNode))
  else
    Result := nil;
end;

// -----------------------------------------------------------------------------

function TFormMain.GetSourceLanguageID: Word;
begin
  Result := BarEditItemSourceLanguage.EditValue;
end;

procedure TFormMain.SetSourceLanguageID(const Value: Word);
begin
  BarEditItemSourceLanguage.EditValue := Value;

  Perform(MSG_SOURCE_CHANGED, 0, 0);
end;

function TFormMain.GetTargetLanguage: TTargetLanguage;
begin
  if (FTargetLanguage = nil) then
  begin
    if (TargetLanguageID <> 0) then
      FTargetLanguage := FLocalizerProject.TargetLanguages.Add(TargetLanguageID)
    else
      FTargetLanguage := FLocalizerProject.TargetLanguages.Add(SourceLanguageID);
  end;

  Result := FTargetLanguage;
end;

function TFormMain.GetTargetLanguageID: Word;
begin
  Result := BarEditItemTargetLanguage.EditValue;
end;

procedure TFormMain.ClearTargetLanguage;
begin
  FTargetLanguage := nil;
  FLocalizerDataSource.TargetLanguage := nil;
end;

procedure TFormMain.SetTargetLanguageID(const Value: Word);
begin
  ClearTargetLanguage;

  BarEditItemTargetLanguage.EditValue := Value;

  Perform(MSG_TARGET_CHANGED, 0, 0);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.InitializeProject(const SourceFilename: string; SourceLocaleID: Word);
begin
  TreeListModules.Clear;
  ClearTargetLanguage;

  FLocalizerProject.BeginUpdate;
  try

    FTranslationCounts.Clear;

    FLocalizerProject.Clear;

    FLocalizerProject.SourceFilename := SourceFilename;
    FLocalizerProject.Name := TPath.GetFileNameWithoutExtension(SourceFilename);
    FLocalizerProject.BaseLocaleID := SourceLocaleID;

    FLocalizerProject.Modified := False;

  finally
    FLocalizerProject.EndUpdate;
  end;

  RibbonMain.DocumentName := FLocalizerProject.Name;

  SourceLanguageID := FLocalizerProject.BaseLocaleID;
  TargetLanguageID := FLocalizerProject.BaseLocaleID;

  TreeListColumnSource.Caption.Text := TLocaleItems.FindLCID(FLocalizerProject.BaseLocaleID).LanguageName;
  TreeListColumnTarget.Caption.Text := TLocaleItems.FindLCID(FLocalizerProject.BaseLocaleID).LanguageName;
end;

procedure TFormMain.LoadProject(Project: TLocalizerProject; Clear: boolean);
var
  Module: TLocalizerModule;
  Modules: TArray<TLocalizerModule>;
  ModuleNode: TcxTreeListNode;
  SelectedModuleFound: boolean;
  PrimeTranslatedCountCache: boolean;
begin
  SaveCursor(crHourGlass);

  LockUpdates;
  try
    try
      if (Clear) then
      begin
        FLocalizerDataSource.Module := nil;
        TreeListModules.Clear;
        TreeListItems.Clear;
      end;

      Modules := Project.Modules.Values.ToArray;

      TArray.Sort<TLocalizerModule>(Modules, TComparer<TLocalizerModule>.Construct(
        function(const Left, Right: TLocalizerModule): Integer
        begin
          Result := (Ord(Left.Kind) - Ord(Right.Kind));
          if (Result = 0) then
            Result := CompareText(Left.Name, Right.Name);
        end));

      SelectedModuleFound := False;

      PrimeTranslatedCountCache := (FTranslationCounts.Count = 0);

      for Module in Modules do
      begin
        if (Module.Kind = mkOther) then
          continue;

        // Prime translated count cache to avoid having to grow the cache on demand.
        // We will still calculate the actual counts on demand.
        if (PrimeTranslatedCountCache) then
          FTranslationCounts.Add(Module, -1);

        if (Clear) then
          ModuleNode := nil
        else
        begin
          if (Module = FLocalizerDataSource.Module) then
            SelectedModuleFound := True;

          // Look for existing node
          ModuleNode := TreeListModules.Find(Module, TreeListModules.Root, False, True, TreeListFindFilter);
        end;

        if (ModuleNode = nil) then
          // Create node if it didn't already exist
          ModuleNode := TreeListModules.Add(nil, Module);

        LoadModuleNode(ModuleNode, Module, True);
      end;

      if (not Clear) and (not SelectedModuleFound) then
        // Selected module no longer exist
        FLocalizerDataSource.Module := nil;

    finally
      if (Clear) then
        // Reassign TreeListItems.CustomDataSource if it was cleared by TreeListItems.Clear
        TreeListItems.CustomDataSource := FLocalizerDataSource;
    end;
  finally
    UnlockUpdates;
  end;
  if (FLocalizerProject.Modified) then
    StatusBar.Panels[0].Text := 'Modified'
  else
    StatusBar.Panels[0].Text := '';
end;

// -----------------------------------------------------------------------------

procedure TFormMain.LoadFocusedItem;
begin
  LoadItem(FocusedItem);
end;

procedure TFormMain.LoadItem(Item: TCustomLocalizerItem; Recurse: boolean);
var
  Node: TcxTreeListNode;
begin
  if (Item is TLocalizerProperty) then
  begin
    Node := TreeListItems.NodeFromHandle(Item);
    if (Node <> nil) then
      ReloadNode(Node);
  end else
  if (Item is TLocalizerModule) then
  begin
    Node := TreeListModules.Find(Item, TreeListModules.Root, False, True, TreeListFindFilter);
    if (Node <> nil) then
      LoadModuleNode(Node, TLocalizerModule(Item), Recurse);
  end;
end;

procedure TFormMain.LoadModuleNode(Node: TcxTreeListNode; Module: TLocalizerModule; Recurse: boolean);
begin
  Assert(Node <> nil);
  Assert(Node.Data <> nil);
  Assert(Node.Data = Module);

  LockUpdates;
  try

    Node.Texts[TreeListColumnModuleName.ItemIndex] := Module.Name;
    Node.Values[TreeListColumnModuleStatus.ItemIndex] := Ord(Module.Status);

    if (Recurse) and (Node.Focused) then
      TreeListItems.FullRefresh;

  finally
    UnlockUpdates;
  end;
end;

procedure TFormMain.LoadModuleNode(Node: TcxTreeListNode; Recurse: boolean);
begin
  Assert(Node <> nil);
  Assert(Node.Data <> nil);
  Assert(TObject(Node.Data) is TLocalizerModule);

  LoadModuleNode(Node, TLocalizerModule(Node.Data), Recurse);
end;

procedure TFormMain.LoadFocusedPropertyNode;
var
  Node: TcxTreeListNode;
begin
  Node := TreeListItems.FocusedNode;
  Assert(Node <> nil);

  ReloadNode(Node);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.MsgSourceChanged(var Msg: TMessage);
begin
  FLocalizerProject.BaseLocaleID := BarEditItemSourceLanguage.EditValue;
  TreeListColumnSource.Caption.Text := TLocaleItems.FindLCID(FLocalizerProject.BaseLocaleID).LanguageName;
end;

procedure TFormMain.MsgTargetChanged(var Msg: TMessage);
var
  LocaleItem: TLocaleItem;
  i: integer;
  Found: boolean;
  AnyFound: boolean;
  FilenameDic, FilenameAff: string;
  SpellCheckerDictionaryItem: TdxSpellCheckerDictionaryItem;
begin
  ClearTargetLanguage;

  LocaleItem := TLocaleItems.FindLCID(TargetLanguageID);

  TreeListColumnTarget.Caption.Text := LocaleItem.LanguageName;

  (*
  ** Load spell check dictionary for new language
  *)

  // Unload old custom dictionary
  Assert(SpellChecker.Dictionaries[0] is TdxUserSpellCheckerDictionary);
  SpellChecker.Dictionaries[0].Enabled := False;
  SpellChecker.Dictionaries[0].Unload;
  // Load new custom dictionary
  TdxUserSpellCheckerDictionary(SpellChecker.Dictionaries[0]).DictionaryPath := Format('.\dictionaries\user-%s.dic', [LocaleItem.LanguageShortName]);
  SpellChecker.Dictionaries[0].Enabled := True;
  SpellChecker.Dictionaries[0].Load;
  SpellChecker.Dictionaries[0].Language := LocaleItem.Locale;

  // Deactivate all existing dictionaries except ones that match new language
  AnyFound := False;
  for i := 1 to SpellChecker.DictionaryCount-1 do
  begin
    Found := (SpellChecker.Dictionaries[i].Language = LocaleItem.Locale);

//    if (SpellChecker.Dictionaries[i].Enabled) and (not Found) then
//      SpellChecker.Dictionaries[i].Unload;

    SpellChecker.Dictionaries[i].Enabled := Found;
    AnyFound := AnyFound or Found;
  end;

  // Add and load  new dictionary
  if (not AnyFound) then
  begin
    FilenameDic := Format('.\dictionaries\%s.dic', [LocaleItem.LanguageShortName]);
    FilenameAff := Format('.\dictionaries\%s.aff', [LocaleItem.LanguageShortName]);
    if (TFile.Exists(FilenameDic)) and (TFile.Exists(FilenameAff)) then
    begin
      // AnyFound := True;
      SpellCheckerDictionaryItem := SpellChecker.DictionaryItems.Add;
      SpellCheckerDictionaryItem.DictionaryTypeClass := TdxHunspellDictionary;
      TdxHunspellDictionary(SpellCheckerDictionaryItem.DictionaryType).Language := LocaleItem.Locale;
      TdxHunspellDictionary(SpellCheckerDictionaryItem.DictionaryType).DictionaryPath := FilenameDic;
      TdxHunspellDictionary(SpellCheckerDictionaryItem.DictionaryType).GrammarPath := FilenameAff;
      TdxHunspellDictionary(SpellCheckerDictionaryItem.DictionaryType).Enabled := True;
      SpellCheckerDictionaryItem.DictionaryType.Load;
    end;

    // TODO : Add support for other formats here...
  end;

  // Reload project
  FLocalizerDataSource.TargetLanguage := TargetLanguage;
//  LoadProject(FLocalizerProject, False);
end;

procedure TFormMain.OnProjectChanged(Sender: TObject);
begin
  StatusBar.Panels[0].Text := 'Modified';
  StatusBar.Panels[2].Text := Format('Translate: %d, Ignore: %d, Hold: %d',
    [FLocalizerProject.StatusCount[ItemStatusTranslate], FLocalizerProject.StatusCount[ItemStatusDontTranslate], FLocalizerProject.StatusCount[ItemStatusHold]]);
end;

procedure TFormMain.OnModuleChanged(Module: TLocalizerModule);
var
  Node: TcxTreeListNode;
begin
  InvalidateTranslatedCount(Module);

  Node := TreeListModules.Find(Module, TreeListModules.Root, False, True, TreeListFindFilter);

  if (Node <> nil) then
    // Refresh node colors and images
    Node.Repaint(True);
end;

procedure TFormMain.SpellCheckerCheckAsYouTypeStart(Sender: TdxCustomSpellChecker; AControl: TWinControl; var AAllow: Boolean);
begin
  // Only spell check inplace editor and memo in text dialog
  AAllow := ((AControl.Parent = TreeListItems) and (AControl is TcxCustomButtonEdit)) or (AControl is TcxCustomMemo);
end;

procedure TFormMain.SpellCheckerCheckWord(Sender: TdxCustomSpellChecker; const AWord: WideString; out AValid: Boolean; var AHandled: Boolean);
var
  SanitizedWord: string;
  Node: TcxTreeListNode;
  i: integer;
  HasLetters: boolean;
begin
  if (FSpellCheckingWord) then
    Exit;

  if (FSpellCheckingString) then
    SanitizedWord := AWord
  else
    SanitizedWord := SanitizeSpellCheckText(AWord);

  FSpellCheckingWord := True;
  try

    HasLetters := False;
    for i := 1 to Length(SanitizedWord) do
      if (SanitizedWord[i].IsLetter) then
      begin
        HasLetters := True;
        break;
      end;

    if (HasLetters) then
      // Call IsValidWord() to check the word against the dictionary.
      // The FSpellCheckingWord flag guards against recursion (IsValidWord() calls this event).
      AValid := Sender.IsValidWord(SanitizedWord)
    else
      // Ignore words without letters
      AValid := True;

  finally
    FSpellCheckingWord := False;
  end;

  if (FSpellCheckingString) then
  begin
    // If we're pre checking the sanitized string we save the result and
    // clear the returned result to avoid having the UI displayed at this time.
    FSpellCheckingStringResult := FSpellCheckingStringResult and AValid;
    AValid := True;
  end;

  AHandled := True;

  if (not AValid) and (FSpellCheckProp <> nil) then
  begin
    // Find and select module
    Node := TreeListModules.Find(FSpellCheckProp.Item.Module, nil, False, True, TreeListFindFilter);

    if (Node <> nil) then
    begin
      Node.MakeVisible;
      Node.Focused := True;

      // Find and select property node
      Node := TreeListItems.NodeFromHandle(FSpellCheckProp);
      if (Node <> nil) then
      begin
        Node.MakeVisible;
        Node.Focused := True;
      end;
      Application.ProcessMessages;
    end;
  end;
end;

procedure TFormMain.SpellCheckerSpellingComplete(Sender: TdxCustomSpellChecker; var AHandled: Boolean);
begin
  // Supress "Check complete" message
  AHandled := True;
end;

function TFormMain.PerformSpellCheck(Prop: TLocalizerProperty): boolean;
var
  Text, CheckedText: string;
begin
  if (Prop.State = ItemStateUnused) then
    Exit(True);

  if (Prop.Status <> ItemStatusTranslate) then
    Exit(True);

  Text := Prop.TranslatedValue[TargetLanguage];

  // Display spell check dialog
  FSpellCheckProp := Prop;
  try

    // Perform pre check on sanitized string.
    // FSpellCheckingStringResult will indicate if string has errors.
    FSpellCheckingStringResult := True;
    FSpellCheckingString := True;
    try
      CheckedText := SanitizeSpellCheckText(Text);
      SpellChecker.Check(CheckedText);
    finally
      FSpellCheckingString := False;
    end;

    CheckedText := Text;
    // If sanitized string had errors we check the unsanitized string to display the UI
    if (not FSpellCheckingStringResult) then
      SpellChecker.Check(CheckedText);

  finally
    FSpellCheckProp := nil;
  end;

  if (CheckedText <> Text) then
  begin
    // Save spell corrected value
    Prop.TranslatedValue[TargetLanguage] := CheckedText;

    // Update treenode
    LoadFocusedPropertyNode;
  end;

  Result := (TdxSpellCheckerCracker(SpellChecker).LastDialogResult = mrOK);
end;

type
  TcxTreeListNodeCracker = class(TcxTreeListNode);

procedure TFormMain.ReloadNode(Node: TcxTreeListNode);
begin
  // Hack to reload a single tree node
  Exclude(TcxTreeListNodeCracker(Node).State, nsValuesAssigned);
  Node.TreeList.LayoutChanged;
end;

procedure TFormMain.TreeListColumnModuleStatusPropertiesEditValueChanged(Sender: TObject);
begin
  // Module status edited inline
  TCustomLocalizerItem(TreeListModules.FocusedNode.Data).Status := TLocalizerItemStatus(TcxImageComboBox(Sender).EditValue);

  LoadModuleNode(TreeListModules.FocusedNode, True);
end;

procedure TFormMain.TreeListColumnStatePropertiesEditValueChanged(Sender: TObject);
var
  Translation: TLocalizerTranslation;
  TranslationStatus: TTranslationStatus;
begin
  TranslationStatus := TTranslationStatus(TcxImageComboBox(Sender).EditValue);

  if (TranslationStatus = tStatusPending) then
  begin
    // Remove translation
    FocusedProperty.Translations.Remove(TargetLanguage);
  end else
  begin
    if (not FocusedProperty.Translations.TryGetTranslation(TargetLanguage, Translation)) then
      Translation := FocusedProperty.Translations.AddOrUpdateTranslation(TargetLanguage, FocusedProperty.Value);
    Translation.Status := TranslationStatus;
  end;

  LoadFocusedPropertyNode;
end;

procedure TFormMain.TreeListColumnStatusPropertiesEditValueChanged(Sender: TObject);
begin
  // Item status edited inline
  FocusedProperty.Status := TLocalizerItemStatus(TcxImageComboBox(Sender).EditValue);

  LoadFocusedPropertyNode;
end;

type
  TcxCustomEditCracker = class(TcxCustomEdit);

procedure TFormMain.TreeListColumnTargetPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
var
  TextEditor: TFormTextEditor;
begin
  TextEditor := TFormTextEditor.Create(nil);
  try
    TextEditor.SourceText := FocusedProperty.Value;
    TextEditor.Text := TcxButtonEdit(Sender).EditingText;// FocusedProperty.TranslatedValue[TargetLanguage];

    if (TextEditor.Execute) then
    begin
      // Write new value back to inner edit control. The OnChange event will occur as normally when the user exits the cell.
      TcxCustomEditCracker(Sender).InnerEdit.EditValue := TextEditor.Text;
      TcxCustomEdit(Sender).ModifiedAfterEnter := True;
    end;
  finally
    TextEditor.Free;
  end;
end;

procedure TFormMain.TreeListItemsEditing(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; var Allow: Boolean);
begin
  // Only allow inline editing of property nodes
  Allow := (FocusedProperty <> nil);
end;

procedure TFormMain.TreeListItemsEditValueChanged(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
begin
  if (AColumn = TreeListColumnTarget) then
  begin
    if (FUpdateLockCount > 0) then
      exit;

    LockUpdates;
    try

      FocusedProperty.TranslatedValue[TargetLanguage] := VarToStr(Sender.InplaceEditor.EditValue);

      LoadFocusedPropertyNode;
    finally
      UnlockUpdates;
    end;
  end;
end;

procedure TFormMain.TreeListItemsGetNodeImageIndex(Sender: TcxCustomTreeList; ANode: TcxTreeListNode;
  AIndexType: TcxTreeListImageIndexType; var AIndex: TImageIndex);
var
  Prop: TLocalizerProperty;
  Translation: TLocalizerTranslation;
begin
  AIndex := -1;

  if (not (AIndexType in [tlitImageIndex, tlitSelectedIndex])) then
    Exit;

  Prop := TLocalizerProperty((Sender as TcxVirtualTreeList).HandleFromNode(ANode));

  if (not Prop.Translations.TryGetTranslation(TargetLanguage, Translation)) then
    Translation := nil;

  if (Prop.State = ItemStateUnused) then
    AIndex := 1
  else
  if (Prop.State = ItemStateNew) and (Prop.Status = ItemStatusTranslate) and (Translation = nil) then
    AIndex := 0
  else
  if (Prop.Status = ItemStatusDontTranslate) then
    AIndex := 2
  else
  if (Prop.Status = ItemStatusHold) then
    AIndex := 5
  else
  if (Translation <> nil) and (Translation.Status <> tStatusPending) then
  begin
    if (Translation.Status = tStatusProposed) then
      AIndex := 3
    else
    if (Translation.Status = tStatusTranslated) then
      AIndex := 4
    else
    if (Translation.Status = tStatusObsolete) then
      AIndex := 7
    else
      AIndex := -1; // Should never happen
  end else
    AIndex := 6;
end;

procedure TFormMain.TreeListModulesEnter(Sender: TObject);
begin
  FActiveTreeList := TcxCustomTreeList(Sender);
end;

procedure TFormMain.TreeListModulesExit(Sender: TObject);
begin
  FActiveTreeList := nil;
end;

procedure TFormMain.TreeListModulesFocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
var
  OldNameVisible, NewNameVisible: boolean;
begin
  TreeListItems.BeginUpdate;
  try

    OldNameVisible := TreeListColumnValueName.Visible;

    FLocalizerDataSource.Module := FocusedModule;

    NewNameVisible := (FLocalizerDataSource.Module = nil) or (FLocalizerDataSource.Module.Kind = mkForm);
    TreeListColumnValueName.Visible := NewNameVisible;

    if (OldNameVisible <> NewNameVisible) then
    begin
      if (NewNameVisible) then
        TreeListColumnItemName.Width := TreeListColumnItemName.Width - TreeListColumnValueName.Width
      else
        TreeListColumnItemName.Width := TreeListColumnItemName.Width + TreeListColumnValueName.Width;
    end;

  finally
    TreeListItems.EndUpdate;
  end;

  if (TreeListItems.TopNode <> nil) then
    TreeListItems.TopNode.MakeVisible;
end;

function TFormMain.GetTranslatedCount(Module: TLocalizerModule): integer;
var
  Language: TTargetLanguage;
  Count: integer;
begin
  if (FTranslationCounts.TryGetValue(Module, Result)) and (Result <> -1) then
    Exit;

  // Calculate translated count
  Count := 0;
  Language := TargetLanguage; // Cache costly conversion
  Module.Traverse(
    function(Prop: TLocalizerProperty): boolean
    begin
      if (Prop.State <> ItemStateUnused) and (Prop.Status = ItemStatusTranslate) and (Prop.HasTranslation(Language)) then
        Inc(Count);
      Result := True;
    end, False);

  Result := Count;

  // Update cache
  FTranslationCounts.AddOrSetValue(Module, Result);
end;

procedure TFormMain.InvalidateTranslatedCount(Module: TLocalizerModule);
begin
  if (FTranslationCounts.ContainsKey(Module)) then
    FTranslationCounts.AddOrSetValue(Module, -1);
end;

procedure TFormMain.RemoveTranslatedCount(Module: TLocalizerModule);
begin
  FTranslationCounts.Remove(Module);
end;

procedure TFormMain.TreeListModulesGetNodeImageIndex(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AIndexType: TcxTreeListImageIndexType; var AIndex: TImageIndex);
var
  Module: TLocalizerModule;
  TranslatedCount: integer;
  Completeness: integer;
begin
  AIndex := -1;

  if (not (AIndexType in [tlitImageIndex, tlitSelectedIndex])) then
    Exit;

  Module := TLocalizerModule(ANode.Data);

  if (Module.State = ItemStateUnused) then
    AIndex := 1
  else
  if (Module.Status = ItemStatusTranslate) then
  begin
    TranslatedCount := GetTranslatedCount(Module);
    // Calculate completeness in %
    Completeness := MulDiv(TranslatedCount, 100, Module.PropertyCount);
    if (Completeness = 100) then
      AIndex := 4 // 100% complete
    else
    if (Completeness >= 75) then
      AIndex := 10 // 75%..99% complete
    else
    if (Completeness >= 50) then
      AIndex := 9 // 50%..74% complete
    else
    if (Completeness >= 25) then
      AIndex := 8 // 25%..49% complete
    else
    if (Module.State = ItemStateNew) then
      AIndex := 0 // 0%..24% complete
    else
      AIndex := 6;
  end else
  if (Module.Status = ItemStatusDontTranslate) then
    AIndex := 2
  else
  if (Module.Status = ItemStatusHold) then
    AIndex := 5
  else
    AIndex := -1;
end;

procedure TFormMain.TreeListModulesStylesGetContentStyle(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; var AStyle: TcxStyle);
var
  Module: TLocalizerModule;
begin
  Module := TLocalizerModule(ANode.Data);

  if (Module.State = ItemStateUnused) or (Module.Status = ItemStatusDontTranslate) then
  begin
    AStyle := StyleDontTranslate;
    Exit;
  end;

  if (Module.Status = ItemStatusHold) then
  begin
    AStyle := StyleHold;
    Exit;
  end;

  if (GetTranslatedCount(Module) = Module.PropertyCount) then
    AStyle := StyleComplete
  else
    AStyle := StyleNeedTranslation;
end;

procedure TFormMain.ActionProofingCheckSelectedExecute(Sender: TObject);
var
  i: integer;
  Item: TCustomLocalizerItem;
  Items: TArray<TCustomLocalizerItem>;
begin
  // TreeList selection will change during spell check so save a static copy before we start
  SetLength(Items, FocusedNode.TreeList.SelectionCount);
  for i := 0 to FocusedNode.TreeList.SelectionCount-1 do
    Items[i] := NodeToItem(FocusedNode.TreeList.Selections[i]);

  for Item in Items do
  begin
    if (not Item.Traverse(
      function(Prop: TLocalizerProperty): boolean
      begin
        Result := PerformSpellCheck(Prop);
      end, False)) then
      break;
  end;

  SpellChecker.ShowSpellingCompleteMessage;
end;

procedure TFormMain.ActionHasItemFocusedUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FocusedItem <> nil);
end;

// -----------------------------------------------------------------------------
//
// TLocalizerDataSource
//
// -----------------------------------------------------------------------------
constructor TLocalizerDataSource.Create(AModule: TLocalizerModule);
begin
  inherited Create;
  FModule := AModule;
end;

function TLocalizerDataSource.GetParentRecordHandle(ARecordHandle: TcxDataRecordHandle): TcxDataRecordHandle;
begin
  Result := nil;
end;

function TLocalizerDataSource.GetRecordCount: Integer;
var
  i: integer;
begin
  Result := 0;

  if (FModule = nil) then
    Exit;

  for i := 0 to FModule.Items.Count-1 do
    Inc(Result, FModule.Items.Values.ToArray[i].Properties.Count);
end;

function TLocalizerDataSource.GetRecordHandle(ARecordIndex: Integer): TcxDataRecordHandle;
var
  Item: TLocalizerItem;
  Props: TArray<TLocalizerProperty>;
begin
  Result := nil;

  if (FModule = nil) then
    Exit;

  for Item in FModule.Items.Values.ToArray do
  begin
    Props := Item.Properties.Values.ToArray;
    if (ARecordIndex > Length(Props)-1) then
    begin
      Dec(ARecordIndex, Length(Props));
      continue;
    end;

    Exit(Props[ARecordIndex]);
  end;
end;

function TLocalizerDataSource.GetRootRecordHandle: TcxDataRecordHandle;
begin
  Result := nil;
end;

function TLocalizerDataSource.GetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant;
const
  TreeItemIndexName             = 0;
  TreeItemIndexType             = 1;
  TreeItemIndexValueName        = 2;
  TreeItemIndexID               = 3;
  TreeItemIndexStatus           = 4;
  TreeItemIndexState            = 5;
  TreeItemIndexSourceValue      = 6;
  TreeItemIndexTargetValue      = 7;
var
  ItemIndex: integer;
  Prop: TLocalizerProperty;
  Translation: TLocalizerTranslation;
begin
  Prop := TLocalizerProperty(ARecordHandle);
  ItemIndex := integer(AItemHandle);

  case ItemIndex of
    TreeItemIndexName:
      Result := Prop.Item.Name;

    TreeItemIndexType:
      Result := Prop.Item.TypeName;

    TreeItemIndexValueName:
      Result := Prop.Name;

    TreeItemIndexID:
      Result := Prop.Item.ResourceID;

    TreeItemIndexStatus:
      Result := Ord(Prop.Status);

    TreeItemIndexState:
      if (Prop.Translations.TryGetTranslation(TargetLanguage, Translation)) then
        Result := Ord(Translation.Status)
      else
        Result := Ord(tStatusPending);

    TreeItemIndexSourceValue:
      Result := Prop.Value;

    TreeItemIndexTargetValue:
      if (Prop.Translations.TryGetTranslation(TargetLanguage, Translation)) then
        Result := Translation.Value
      else
        Result := Prop.Value;
  else
    Result := Null;
  end;
end;

procedure TLocalizerDataSource.SetModule(const Value: TLocalizerModule);
begin
  if (FModule = Value) then
    Exit;

  FModule := Value;

  DataChanged;
end;

procedure TLocalizerDataSource.SetTargetLanguage(const Value: TTargetLanguage);
begin
  if (FTargetLanguage = Value) then
    Exit;

  FTargetLanguage := Value;

  DataChanged;
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

end.
