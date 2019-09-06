unit amLocalization.Dialog.Main;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  Generics.Collections,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Forms, Vcl.Controls, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, System.Actions,
  Vcl.ActnList, System.ImageList, Vcl.ImgList, Datasnap.DBClient, UITypes, Data.DB,
  SyncObjs,

  dxRibbonForm,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxRibbonSkins, dxSkinsCore,
  dxRibbonCustomizationForm, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator, dxDateRanges,
  cxDBData, cxGridLevel, cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, dxBar, dxSkinsForm,
  cxClasses, dxStatusBar, dxRibbonStatusBar, dxRibbon, cxTL, cxTLdxBarBuiltInMenu, cxInplaceContainer, cxLabel, cxMemo,
  cxImageComboBox, cxSplitter, cxContainer, cxTreeView, cxTextEdit, cxBlobEdit, cxImageList, cxDBExtLookupComboBox, cxMaskEdit, cxDropDownEdit, cxLookupEdit, cxDBLookupEdit,
  cxBarEditItem, cxDataControllerConditionalFormattingRulesManagerDialog, cxButtonEdit, dxSpellCheckerCore, dxSpellChecker, cxTLData,
  dxLayoutcxEditAdapters, dxLayoutLookAndFeels, dxLayoutContainer, dxLayoutControl,

  amLocale,
  amProgressForm,
  amLocalization.Model,
  amLocalization.Translator,
  amLocalization.Dialog.Search,
  amLocalization.Translator.TM;


const
  MSG_SOURCE_CHANGED = WM_USER;
  MSG_TARGET_CHANGED = WM_USER+1;
  MSG_FORM_MAXIMIZE = WM_USER+2;
  MSG_RESTART = WM_USER+3;
  MSG_FILE_OPEN = WM_USER+4;
  MSG_AFTER_SHOW = WM_USER+5;

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
  TFormMain = class(TdxRibbonForm, ILocalizerSearchHost)
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
    ReplaceDialog: TReplaceDialog;
    PopupMenuTree: TdxRibbonPopupMenu;
    SplitterTreeLists: TcxSplitter;
    TreeListModules: TcxTreeList;
    TreeListColumnModuleName: TcxTreeListColumn;
    TreeListColumnModuleStatus: TcxTreeListColumn;
    BarManagerBarLookup: TdxBar;
    dxBarLargeButton6: TdxBarLargeButton;
    dxBarButton15: TdxBarButton;
    BarButtonGotoNext: TdxBarSubItem;
    ActionMain: TAction;
    ActionGotoNextUntranslated: TAction;
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
    StyleSelected: TcxStyle;
    PanelModules: TPanel;
    LayoutControlModulesGroup_Root: TdxLayoutGroup;
    LayoutControlModules: TdxLayoutControl;
    dxLayoutItem2: TdxLayoutItem;
    LabelCountTranslated: TcxLabel;
    dxLayoutItem1: TdxLayoutItem;
    LabelCountPending: TcxLabel;
    ActionAutomationWebLookup: TAction;
    ImageListState: TcxImageList;
    ActionAutomationMemory: TAction;
    ActionAutomationMemoryAdd: TAction;
    ActionAutomationMemoryTranslate: TAction;
    dxBarButton25: TdxBarButton;
    ActionFindNext: TAction;
    dxBarButton26: TdxBarButton;
    ActionGotoNext: TAction;
    ActionGotoNextWarning: TAction;
    ActionGotoNextBookmark: TAction;
    dxBarButton16: TdxBarButton;
    dxBarButton27: TdxBarButton;
    ButtonGotoBookmark: TdxBarButton;
    PopupMenuBookmark: TdxRibbonPopupMenu;
    ButtonItemBookmarkAny: TdxBarButton;
    ActionGotoBookmarkAny: TAction;
    ActionEditMark: TAction;
    BarManagerBarMark: TdxBar;
    ButtonItemBookmark: TdxBarButton;
    StyleFocused: TcxStyle;
    dxLayoutItem3: TdxLayoutItem;
    LabelCountTranslatedPercent: TcxLabel;
    dxLayoutGroup1: TdxLayoutGroup;
    ActionValidate: TAction;
    ActionGotoNextStatus: TAction;
    ActionGotoNextState: TAction;
    dxBarSubItem2: TdxBarSubItem;
    dxBarSubItem3: TdxBarSubItem;
    dxBarButton28: TdxBarButton;
    dxBarButton29: TdxBarButton;
    dxBarButton30: TdxBarButton;
    ActionGotoNextStatusTranslate: TAction;
    ActionGotoNextStatusHold: TAction;
    ActionGotoNextStatusDontTranslate: TAction;
    ActionGotoNextStateNew: TAction;
    ActionGotoNextStateExisting: TAction;
    ActionGotoNextStateUnused: TAction;
    dxBarButton31: TdxBarButton;
    dxBarButton32: TdxBarButton;
    dxBarButton33: TdxBarButton;
    PopupMenuRecentFiles: TdxRibbonPopupMenu;
    BarManagerBarQuickAccess: TdxBar;
    ActionSettings: TAction;
    dxBarButton34: TdxBarButton;
    TreeListColumnEffectiveStatus: TcxTreeListColumn;
    OpenDialogDRC: TOpenDialog;
    SaveDialogProject: TSaveDialog;
    SaveDialogEXE: TOpenDialog;
    BarManagerBarFeedback: TdxBar;
    BarButtonFeedbackPositive: TdxBarButton;
    BarButtonFeedbackNegative: TdxBarButton;
    BarButtonFeedbackHide: TdxBarButton;
    BarButtonFeedback: TdxBarSubItem;
    ActionFeedback: TAction;
    ActionFeedbackPositive: TAction;
    ActionFeedbackNegative: TAction;
    ActionFeedbackHide: TAction;
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
    procedure ActionGotoNextUntranslatedExecute(Sender: TObject);
    procedure TreeListModulesStylesGetContentStyle(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; var AStyle: TcxStyle);
    procedure SpellCheckerCheckAsYouTypeStart(Sender: TdxCustomSpellChecker; AControl: TWinControl; var AAllow: Boolean);
    procedure ActionImportFileSourceExecute(Sender: TObject);
    procedure ActionImportFileTargetExecute(Sender: TObject);
    procedure TreeListModulesGetNodeImageIndex(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AIndexType: TcxTreeListImageIndexType; var AIndex: TImageIndex);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ActionAutomationWebLookupExecute(Sender: TObject);
    procedure ActionAutomationWebLookupUpdate(Sender: TObject);
    procedure TreeListItemsStylesGetContentStyle(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; var AStyle: TcxStyle);
    procedure ActionFindNextExecute(Sender: TObject);
    procedure ActionFindNextUpdate(Sender: TObject);
    procedure ActionAutomationMemoryExecute(Sender: TObject);
    procedure TreeListItemsGetCellHint(Sender: TcxCustomTreeList; ACell: TObject; var AText: string; var ANeedShow: Boolean);
    procedure TreeListItemsClick(Sender: TObject);
    procedure ActionDummyExecute(Sender: TObject);
    procedure ActionGotoNextWarningExecute(Sender: TObject);
    procedure TreeListItemsCustomDrawIndicatorCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas; AViewInfo: TcxTreeListIndicatorCellViewInfo; var ADone: Boolean);
    procedure ActionBookmarkExecute(Sender: TObject);
    procedure PopupMenuBookmarkPopup(Sender: TObject);
    procedure ActionGotoBookmarkAnyExecute(Sender: TObject);
    procedure ActionEditMarkExecute(Sender: TObject);
    procedure ActionEditMarkUpdate(Sender: TObject);
    procedure ActionGotoNextBookmarkExecute(Sender: TObject);
    procedure StatusBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure StatusBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ActionValidateExecute(Sender: TObject);
    procedure ActionAutomationMemoryAddExecute(Sender: TObject);
    procedure ActionHasPropertyFocusedUpdate(Sender: TObject);
    procedure ActionAutomationMemoryTranslateExecute(Sender: TObject);
    procedure ActionGotoNextStatusExecute(Sender: TObject);
    procedure ActionGotoNextStateExecute(Sender: TObject);
    procedure ActionAutomationMemoryTranslateUpdate(Sender: TObject);
    procedure ActionAutomationMemoryAddUpdate(Sender: TObject);
    procedure ButtonOpenRecentClick(Sender: TObject);
    procedure ActionSettingsExecute(Sender: TObject);
    procedure ActionImportFileTargetUpdate(Sender: TObject);
    procedure BarEditItemTargetLanguageEnter(Sender: TObject);
    procedure BarEditItemTargetLanguageExit(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ActionFeedbackHideExecute(Sender: TObject);
    procedure ActionFeedbackPositiveExecute(Sender: TObject);
    procedure ActionFeedbackNegativeExecute(Sender: TObject);
  private
    FProject: TLocalizerProject;
    FProjectFilename: string;
    FTargetLanguage: TTargetLanguage;
    FUpdateLockCount: integer;
    FSpellCheckProp: TLocalizerProperty;
    FSpellCheckingWord: boolean;
    FSpellCheckingString: boolean;
    FSpellCheckingStringResult: boolean;
    FLocalizerDataSource: TLocalizerDataSource;
    FActiveTreeList: TcxCustomTreeList;
    FFilterTargetLanguages: boolean;
    FTranslationCounts: TDictionary<TLocalizerModule, integer>;
    FSearchProvider: ILocalizerSearchProvider;
    FDataModuleTranslationMemory: TDataModuleTranslationMemory;
    FLastBookmark: integer;
  private
    procedure SaveSettings;
    procedure LoadSettings;
    function QueueRestart(Immediately: boolean = False): boolean;
  private
    // Hints
    // Status bar panel hint needs custom handling.
    // See: DevPress ticket DS31900
    // http://www.devexpress.com/Support/Center/Question/Details/DS31900
    FStatusBarPanel: TdxStatusBarPanel;
    FStatusBarPanelHint: array of string;
    procedure DoShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    procedure ShowHint(Sender: TObject); { updates statusbar with hints }
    procedure SetInfoText(const Msg: string);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoCreate; override;
    procedure WndProc(var Msg: TMessage); override;
  private
    type
      TMsgFileOpen = packed record
        Msg: Cardinal;
        case Integer of
        0: (
          WParam: WPARAM;
          LParam: LPARAM;
          Result: LRESULT);
        1: (
          CommandLine: boolean;
          WParamPadding0: byte;
          WParamPadding1: byte;
          WParamPadding2: byte;
          Unused: LPARAM;
          _Result: LRESULT);
      end;
    // Message handlers
    procedure MsgSourceChanged(var Msg: TMessage); message MSG_SOURCE_CHANGED;
    procedure MsgTargetChanged(var Msg: TMessage); message MSG_TARGET_CHANGED;
    procedure MsgFormMaximize(var Msg: TMessage); message MSG_FORM_MAXIMIZE;
    procedure MsgRestart(var Msg: TMessage); message MSG_RESTART;
    procedure MsgAfterShow(var Msg: TMessage); message MSG_AFTER_SHOW;
    procedure MsgFileOpen(var Msg: TMsgFileOpen); message MSG_FILE_OPEN;
  private
    // Event handlers
    procedure OnProjectChanged(Sender: TObject);
    procedure OnModuleChanged(Module: TLocalizerModule);
  private
    // Recent files
    procedure LoadRecentFiles;
    procedure SaveRecentFiles;
    procedure AddRecentFile(const Filename: string);
  protected
    function GetLanguageID(Value: LCID): LCID;
    function GetSourceLanguageID: Word;
    function GetTargetLanguageID: Word;
    procedure SetSourceLanguageID(const Value: Word);
    procedure SetTargetLanguageID(const Value: Word);
    function GetTargetLanguage: TTargetLanguage;
    procedure ClearTargetLanguage;
    procedure UpdateTargetLanguage;

    function GetFocusedNode: TcxTreeListNode;
    function GetFocusedItem: TCustomLocalizerItem;
    function GetFocusedModule: TLocalizerModule;
    function GetFocusedProperty: TLocalizerProperty;

    property FocusedNode: TcxTreeListNode read GetFocusedNode;
    property FocusedItem: TCustomLocalizerItem read GetFocusedItem;
    property FocusedModule: TLocalizerModule read GetFocusedModule;
    property FocusedProperty: TLocalizerProperty read GetFocusedProperty;

    function NodeToItem(Node: TcxTreeListNode): TCustomLocalizerItem;
    function GetNodeValidationMessage(Node: TcxTreeListNode): string;
  protected
    FPendingFileOpen: TStrings;
    FPendingFileOpenLock: TCriticalSection;
    procedure LoadFromFile(const Filename: string);
    procedure LoadFromSingleInstance(const Param: string);
  protected
    procedure LoadProject(Project: TLocalizerProject; Clear: boolean = True);
    procedure LoadItem(Item: TCustomLocalizerItem; Recurse: boolean = False);
    procedure LoadFocusedItem(Recurse: boolean = False);
    procedure LoadModuleNode(Node: TcxTreeListNode; Recurse: boolean); overload;
    procedure LoadModuleNode(Node: TcxTreeListNode; Module: TLocalizerModule; Recurse: boolean); overload;
    procedure LoadFocusedPropertyNode;
    procedure ReloadNode(Node: TcxTreeListNode);
    procedure ReloadProperty(Prop: TLocalizerProperty);
    procedure DisplayModuleStats;
    procedure ViewProperty(Prop: TLocalizerProperty);
  protected
    procedure InitializeProject(const SourceFilename: string; SourceLocaleID: Word);
    procedure LockUpdates;
    procedure UnlockUpdates;
    function PerformSpellCheck(Prop: TLocalizerProperty): boolean;
    function CheckSave: boolean;
    procedure ClearDependents;
    function GotoNext(Predicate: TLocalizerPropertyDelegate; FromStart: boolean = False): boolean;
    procedure UpdateProjectModifiedIndicator;
    function CheckSourceFile: boolean;
    function CheckStringsSymbolFile: boolean;
  private
    procedure TranslateSelected(const TranslationService: ITranslationService);
  private type
    TCounts = record
      CountModule, CountItem, CountProperty: integer;
      UnusedModule, UnusedItem, UnusedProperty: integer;
      Translated: integer;
      ObsoleteTranslation: integer;
    end;
  private
    function CountStuff: TCounts;
  protected
    function GetTranslatedCount(Module: TLocalizerModule): integer;
    procedure InvalidateTranslatedCount(Module: TLocalizerModule);
    procedure InvalidateTranslatedCounts;
    procedure RemoveTranslatedCount(Module: TLocalizerModule);
  protected
    // ILocalizerSearchHost
    function ILocalizerSearchHost.GetSelectedModule = GetFocusedModule;
    function ILocalizerSearchHost.GetTargetLanguage = GetTargetLanguage;
    procedure ILocalizerSearchHost.ViewItem = ViewProperty;
  private
    FSkin: string;
    FColorSchemeAccent: integer;
  protected
    procedure SetSkin(const Value: string);
    property Skin: string read FSkin write SetSkin;
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
  Math,
  IOUtils,
  StrUtils,
  Generics.Defaults,
  System.Character,
  RegularExpressions,

  // DevExpress skins
  dxSkinOffice2016Colorful,

  // Skin utils
  dxSkinsDefaultPainters,
  dxSkinsdxRibbonPainter,

  dxHunspellDictionary,
  dxSpellCheckerDialogs,

  DelphiDabbler.SingleInstance,

  amCursorService,
  amVersionInfo,
  amShell,

  amLocalization.Engine,
  amLocalization.ResourceWriter,
  amLocalization.Persistence,
  amLocalization.Import.XLIFF,
  amLocalization.Data.Main,
  amLocalization.Utils,
  amLocalization.Shell,
  amLocalization.Settings,
  amLocalization.Translator.Microsoft.Version3,
  amLocalization.Dialog.TextEdit,
  amLocalization.Dialog.NewProject,
  amLocalization.Dialog.TranslationMemory,
  amLocalization.Dialog.Languages,
  amLocalization.Dialog.Settings,
  amLocalization.Dialog.Feedback;

resourcestring
  sLocalizerFindNoMore = 'No more found';

resourcestring
  sTranslateEligibleWarning = 'Note: %d of the selected values are not elegible for translation.';

resourcestring
  sProjectUpdatedTitle = 'Project updated';
  sProjectUpdated = 'Update completed with the following changes:'#13#13+
    'Modules: %.0n added, %.0n unused'#13+
    'Items: %.0n added, %.0n unused'#13+
    'Properties: %.0n added, %.0n unused'#13+
    'Translations obsoleted: %.0n';
  sProjectUpdatedNothing = 'Update completed without any changes.';

resourcestring
  sTranslationsUpdatedTitle = 'Translations updated';
  sTranslationsUpdated = 'Translations has been updated with the following changes:'#13#13+
    'Added: %.0n'#13+
    'Updated: %.0n'#13+
    'Skipped: %.0n';

resourcestring
  sResourceModuleFilter = '%s resource modules (*.%1:s)|*.%1:s|';

const
  ImageIndexBookmark0 = 27;
  ImageIndexBookmarkA = 37;
  ImageIndexModified = 44;
  ImageIndexNotModified = -1;

const
  StatusBarPanelHint = 0;
  StatusBarPanelModified = 1;
  StatusBarPanelStats = 2;

const
  sDefaultSkinName = 'Office2016Colorful';

// -----------------------------------------------------------------------------

type
  TdxSpellCheckerCracker = class(TdxCustomSpellChecker);

// -----------------------------------------------------------------------------

function TreeListFindFilter(ANode: TcxTreeListNode; AData: Pointer): Boolean;
begin
  Result := (ANode.Data = AData);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  SingleInstance.CreateParams(Params);
end;

procedure TFormMain.WndProc(var Msg: TMessage);
begin
  if (not HandleAllocated) or (not SingleInstance.HandleMessages(Handle, Msg)) then
    inherited;
end;

procedure TFormMain.DoCreate;
begin
  inherited;

  (*
  ** Restore main window state
  **
  ** We must do this after TdxCustomRibbonForm.DoCreate has been executed
  ** in order to avoid the "growing form syndrome". The growing form is caused
  ** by TdxCustomRibbonForm.AdjustLayout (called from DoCreate).
  *)
  TranslationManagerSettings.Forms.Main.ApplySettings(Self);
  if (TranslationManagerSettings.Forms.Main.Maximized) then
    PostMessage(Handle, MSG_FORM_MAXIMIZE, 0, 0);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.FormCreate(Sender: TObject);

  procedure CreateBookmarkMenu;
  var
    Flag: TPropertyFlag;
    Action: TAction;
    ItemLink: TdxBarItemLink;
    Separator: TdxBarSeparator;
  resourcestring
    sMenuCaptionBookmark = 'Bookmark &%.1X';
  begin
    FLastBookmark := -1;
    for Flag := FlagBookmark0 to FlagBookmarkF do
    begin
      if (Flag = FlagBookmarkA) then
      begin
        ItemLink := PopupMenuBookmark.ItemLinks.AddItem(TdxBarSeparator);
        Separator := TdxBarSeparator(ItemLink.Item);
        Separator.ShowCaption := False;
        // Hide item from customization
        ItemLink.Item.Visible := ivNotInCustomizing;
      end;

      Action := TAction.Create(ActionList);
      Action.Tag := Ord(Flag);
      Action.Caption := Format(sMenuCaptionBookmark, [Action.Tag - Ord(FlagBookmark0)]);
      Action.ImageIndex := ImageIndexBookmark0 + Ord(Flag) - Ord(FlagBookmark0);
      Action.OnExecute := ActionBookmarkExecute;

      ItemLink := PopupMenuBookmark.ItemLinks.AddButton;
      ItemLink.Item.Action := Action;
      TdxBarButton(ItemLink.Item).ButtonStyle := bsChecked;
      // Hide item from customization
      ItemLink.Item.Visible := ivNotInCustomizing;
    end;
  end;

var
  i: integer;
begin
  DisableAero := True;

  FPendingFileOpenLock := TCriticalSection.Create;

  FProject := TLocalizerProject.Create('', GetLanguageID(TranslationManagerSettings.System.DefaultSourceLanguage));
  FProject.OnChanged := OnProjectChanged;
  FProject.OnModuleChanged := OnModuleChanged;

  FTranslationCounts := TDictionary<TLocalizerModule, integer>.Create;

  FLocalizerDataSource := TLocalizerDataSource.Create(nil);
  TreeListItems.DataController.CustomDataSource := FLocalizerDataSource;

  DataModuleMain := TDataModuleMain.Create(Self);
  FDataModuleTranslationMemory := TDataModuleTranslationMemory.Create(Self);

  Application.OnHint := ShowHint;
  Application.OnShowHint := DoShowHint;

  RibbonTabMain.Active := True;

  for i := 0 to StatusBar.Panels.Count-1 do
    StatusBar.Panels[i].Text := '';
  SetLength(FStatusBarPanelHint, StatusBar.Panels.Count);

  CreateBookmarkMenu;

  FSkin := '';
  FColorSchemeAccent := Ord(RibbonMain.ColorSchemeAccent);

  if (TranslationManagerSettings.System.SafeMode) then
    Caption := Caption + ' [SAFE MODE]';

  LoadSettings;

  SingleInstance.OnProcessParam := LoadFromSingleInstance;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  SaveSettings;

  TDirectory.CreateDirectory(TranslationManagerSettings.Folders.FolderUserSpellCheck);
  SpellChecker.Dictionaries[0].Unload;

  Application.OnHint := nil;
  Application.OnShowHint := nil;

  // Block notifications
  FProject.BeginUpdate;

  FTranslationCounts.Clear;
  FProject.Clear;

  FLocalizerDataSource.Free;
  FProject.Free;
  FTranslationCounts.Free;

  FreeAndNil(FPendingFileOpen);
  FreeAndNil(FPendingFileOpenLock);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.LoadSettings;
begin
  TranslationManagerSettings.Proofing.ApplyTo(SpellChecker);
  SpellChecker.UseThreadedLoad := (not TranslationManagerSettings.System.SafeMode);
  SpellChecker.CheckAsYouTypeOptions.Active := SpellChecker.CheckAsYouTypeOptions.Active and (not TranslationManagerSettings.System.SafeMode);

  LoadRecentFiles;

  if (TranslationManagerSettings.System.UseProposedStatus) then
    TLocalizerTranslations.DefaultStatus := tStatusProposed
  else
    TLocalizerTranslations.DefaultStatus := tStatusTranslated;

  OpenDialogXLIFF.InitialDir := TranslationManagerSettings.Folders.DocumentFolder;
  OpenDialogProject.InitialDir := TranslationManagerSettings.Folders.DocumentFolder;

  if (TranslationManagerSettings.Layout.ModuleTree.Valid) then
  begin
    // Tree.RestoreFromRegistry fails if data doesn't exist
    TreeListModules.RestoreFromRegistry(TranslationManagerSettings.Layout.ModuleTree.Owner.KeyPath, False, False, TranslationManagerSettings.Layout.ModuleTree.Name);
    TranslationManagerSettings.Layout.ModuleTree.ReadFilter(TreeListModules.Filter);
  end;

  if (TranslationManagerSettings.Layout.ItemTree.Valid) then
  begin
    TreeListItems.RestoreFromRegistry(TranslationManagerSettings.Layout.ItemTree.Owner.KeyPath, False, False, TranslationManagerSettings.Layout.ItemTree.Name);
    TranslationManagerSettings.Layout.ItemTree.ReadFilter(TreeListItems.Filter);
  end;

  RibbonMain.TabAreaToolbar.Visible := not TranslationManagerSettings.System.HideFeedback;

  SetSkin(TranslationManagerSettings.System.Skin);
end;

procedure TFormMain.SaveSettings;
begin
  TranslationManagerSettings.Version := TVersionInfo.FileVersionString(ParamStr(0));

  SaveRecentFiles;

  TranslationManagerSettings.Forms.Main.PrepareSettings(Self);

  TreeListModules.StoreToRegistry(TranslationManagerSettings.Layout.ModuleTree.Owner.KeyPath, False, TranslationManagerSettings.Layout.ModuleTree.Name);
  TranslationManagerSettings.Layout.ModuleTree.WriteFilter(TreeListModules.Filter);
  TranslationManagerSettings.Layout.ModuleTree.Valid := True;

  TreeListItems.StoreToRegistry(TranslationManagerSettings.Layout.ItemTree.Owner.KeyPath, False, TranslationManagerSettings.Layout.ItemTree.Name);
  TranslationManagerSettings.Layout.ItemTree.WriteFilter(TreeListItems.Filter);
  TranslationManagerSettings.Layout.ItemTree.Valid := True;

  if (not TranslationManagerSettings.System.SafeMode) then // Spell checker setting are not complete in safe mode
    TranslationManagerSettings.Proofing.SaveFrom(SpellChecker);

  TranslationManagerSettings.System.HideFeedback := not RibbonMain.TabAreaToolbar.Visible;

  TranslationManagerSettings.WriteConfig;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.MsgFormMaximize(var Msg: TMessage);
begin
  WindowState := wsMaximized;
end;

procedure TFormMain.MsgRestart(var Msg: TMessage);
begin
  if (Application.Terminated) or (csDestroying in ComponentState) then
    exit;

  QueueRestart(True);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.MsgSourceChanged(var Msg: TMessage);
begin
  FProject.SourceLanguageID := SourceLanguageID;
  TreeListColumnSource.Caption.Text := TLocaleItems.FindLCID(FProject.SourceLanguageID).LanguageName;
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
  ClearDependents;
  ClearTargetLanguage;
  InvalidateTranslatedCounts;

  LocaleItem := TLocaleItems.FindLCID(TargetLanguageID);

  TreeListColumnTarget.Caption.Text := LocaleItem.LanguageName;

  (*
  ** Load spell check dictionary for new language
  *)

  // Unload old custom dictionary
  Assert(SpellChecker.Dictionaries[0] is TdxUserSpellCheckerDictionary);
  SpellChecker.Dictionaries[0].Enabled := False;
  // Make sure folder exist before we save
  TDirectory.CreateDirectory(TranslationManagerSettings.Folders.FolderUserSpellCheck);
  SpellChecker.Dictionaries[0].Unload;
  // Load new custom dictionary
  TdxUserSpellCheckerDictionary(SpellChecker.Dictionaries[0]).DictionaryPath := TranslationManagerSettings.Folders.FolderUserSpellCheck+Format('user-%s.dic', [LocaleItem.LanguageShortName]);
  SpellChecker.Dictionaries[0].Enabled := True;
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

  // Add and load new dictionary
  if (not AnyFound) then
  begin
    FilenameDic := TranslationManagerSettings.Folders.FolderSpellCheck+Format('%s.dic', [LocaleItem.LanguageShortName]);
    FilenameAff := TranslationManagerSettings.Folders.FolderSpellCheck+Format('%s.aff', [LocaleItem.LanguageShortName]);
    if (TFile.Exists(FilenameDic)) and (TFile.Exists(FilenameAff)) then
    begin
      // AnyFound := True;
      SpellCheckerDictionaryItem := SpellChecker.DictionaryItems.Add;
      SpellCheckerDictionaryItem.DictionaryTypeClass := TdxHunspellDictionary;
      TdxHunspellDictionary(SpellCheckerDictionaryItem.DictionaryType).Language := LocaleItem.Locale;
      TdxHunspellDictionary(SpellCheckerDictionaryItem.DictionaryType).DictionaryPath := FilenameDic;
      TdxHunspellDictionary(SpellCheckerDictionaryItem.DictionaryType).GrammarPath := FilenameAff;
      TdxHunspellDictionary(SpellCheckerDictionaryItem.DictionaryType).Enabled := True;
    end;

    // TODO : Add support for other formats here...
  end;

  SpellChecker.LoadDictionaries;

  // Reload project
  UpdateTargetLanguage;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_F12) and (ssAlt in Shift) then
  begin
    Key := 0;
    if (not CheckSave) then
      exit;

    InitializeProject('test.xxx', SourceLanguageID);

    FProject.AddModule('ONE', mkForm).AddItem('Item1', 'TFooBar').AddProperty('Test1', 'value1');
    FProject.AddModule('TWO', mkForm).AddItem('Item2', 'TFooBar').AddProperty('Test2', 'value2');

    LoadProject(FProject, True);
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.FormShow(Sender: TObject);
begin
  PostMessage(Handle, MSG_AFTER_SHOW, 0, 0);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.LoadRecentFiles;
var
  ShortCut: Char;

  procedure AddRecentFile(const Filename: string);
  var
    Item: TdxBarItem;
  begin
    Item := PopupMenuRecentFiles.ItemLinks.AddButton.Item;
    Item.Caption := Format('&%s %s', [ShortCut, cxGetStringAdjustedToWidth(0, 0, Filename, 300, mstPathEllipsis)]);
    Item.Description := Filename;
    Item.Hint := Filename;
    Item.OnClick := ButtonOpenRecentClick;
    // Hide item from customization
    Item.Visible := ivNotInCustomizing;
  end;

var
  s: string;
  i: integer;
begin
  for i := PopupMenuRecentFiles.ItemLinks.Count-1 downto 0 do
    PopupMenuRecentFiles.ItemLinks[i].Item.Free;

  ShortCut := '0';

  for s in TranslationManagerSettings.Folders.RecentFiles do
    if (s <> '') then
    begin
      AddRecentFile(s);

      if (ShortCut = '9') then
        ShortCut := 'A'
      else
        Inc(ShortCut);
    end;
end;

procedure TFormMain.SaveRecentFiles;
begin
end;

procedure TFormMain.AddRecentFile(const Filename: string);
var
  i: integer;
begin
  if (Filename = '') or (TPath.GetDirectoryName(Filename) = '') or (not TFile.Exists(Filename)) then
    exit;

  try

    for i := 0 to TranslationManagerSettings.Folders.RecentFiles.Count-1 do
      if (AnsiSameText(Filename, TranslationManagerSettings.Folders.RecentFiles[i])) then
      begin
        TranslationManagerSettings.Folders.RecentFiles.Move(i, 0);
        Exit;
      end;

    TranslationManagerSettings.Folders.RecentFiles.Insert(0, Filename);

    // Prune to at most 10 items
    for i := TranslationManagerSettings.Folders.RecentFiles.Count-1 downto 10 do
      TranslationManagerSettings.Folders.RecentFiles.Delete(i);

  finally
    LoadRecentFiles;
  end;
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

// -----------------------------------------------------------------------------

procedure TFormMain.ActionMainExecute(Sender: TObject);
begin
//
end;

procedure TFormMain.ActionMainUpdate(Sender: TObject);
begin
  BarEditItemSourceLanguage.Enabled := (not FProject.SourceFilename.IsEmpty);
  BarEditItemTargetLanguage.Enabled := (not FProject.SourceFilename.IsEmpty);
  BarManagerBarLanguage.CaptionButtons[0].Enabled := (not FProject.SourceFilename.IsEmpty);
end;

procedure TFormMain.ActionDummyExecute(Sender: TObject);
begin
//
end;

procedure TFormMain.ActionHasProjectUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (not FProject.SourceFilename.IsEmpty);
end;

procedure TFormMain.ActionHasModulesUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FProject.Modules.Count > 0);
end;

procedure TFormMain.ActionHasItemFocusedUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FocusedItem <> nil);
end;

procedure TFormMain.ActionHasPropertyFocusedUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FocusedProperty <> nil);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionAutomationMemoryAddExecute(Sender: TObject);
var
  i: integer;
  Item: TCustomLocalizerItem;
  Count, ElegibleCount: integer;
  ElegibleWarning: string;
resourcestring
  sAddToDictionaryPromptTitle = 'Add to Translation Memory?';
  sAddToDictionaryPrompt = 'Do you want to add the selected %d values to the Translation Memory?%s';
begin
  Count := 0;
  for i := 0 to FocusedNode.TreeList.SelectionCount-1 do
  begin
    Item := NodeToItem(FocusedNode.TreeList.Selections[i]);
    Item.Traverse(
      function(Prop: TLocalizerProperty): boolean
      begin
        Inc(Count);
        if (Prop.EffectiveStatus = ItemStatusTranslate) then
          Inc(ElegibleCount);
        Result := True;
      end, False);
  end;

  if (ElegibleCount < Count) then
    ElegibleWarning :=  #13#13 + Format(sTranslateEligibleWarning, [Count-ElegibleCount])
  else
    ElegibleWarning :=  '';

  if (TaskMessageDlg(sAddToDictionaryPromptTitle, Format(sAddToDictionaryPrompt, [ElegibleCount, ElegibleWarning]),
    mtConfirmation, [mbYes, mbNo], 0, mbNo) <> mrYes) then
    Exit;

  for i := 0 to FocusedNode.TreeList.SelectionCount-1 do
  begin
    Item := NodeToItem(FocusedNode.TreeList.Selections[i]);
    Item.Traverse(
      function(Prop: TLocalizerProperty): boolean
      begin
        if (Prop.EffectiveStatus = ItemStatusTranslate) then
          FDataModuleTranslationMemory.Add(SourceLanguageID, Prop.Value, TargetLanguageID, Prop.TranslatedValue[TargetLanguage]);
        Result := True;
      end, False);
  end;
end;

procedure TFormMain.ActionAutomationMemoryAddUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FocusedItem <> nil) and (SourceLanguageID <> TargetLanguageID) and (FDataModuleTranslationMemory.IsAvailable);
end;

procedure TFormMain.ActionAutomationMemoryExecute(Sender: TObject);
var
  FormTranslationMemory: TFormTranslationMemory;
begin
  FormTranslationMemory := TFormTranslationMemory.Create(nil);
  try

    FormTranslationMemory.Execute(FDataModuleTranslationMemory);

  finally
    FormTranslationMemory.Free;
  end;
end;

procedure TFormMain.TranslateSelected(const TranslationService: ITranslationService);
var
  Progress: IProgress;
  SourceLocaleItem, TargetLocaleItem: TLocaleItem;
  i: integer;
  Item: TCustomLocalizerItem;
  Count, ElegibleCount: integer;
  TranslatedCount, UpdatedCount: integer;
  ElegibleWarning: string;
resourcestring
  sTranslateAutoProgress = 'Translating using %s...';
  sTranslateAutoPromptTitle = 'Translate using %s?';
  sTranslateAutoPrompt = 'Do you want to perform automated translation on the selected %d values?%s';
  sTranslateAutoResultTitle = 'Automated translation completed.';
  sTranslateAutoResult = 'Translated: %d'#13'Updated: %d'#13'Not found: %d';
begin
  SourceLocaleItem := TLocaleItems.FindLCID(SourceLanguageID);
  TargetLocaleItem := TLocaleItems.FindLCID(TargetLanguageID);

  if (SourceLocaleItem = TargetLocaleItem) then
    Exit;

  Count := 0;
  for i := 0 to FocusedNode.TreeList.SelectionCount-1 do
  begin
    Item := NodeToItem(FocusedNode.TreeList.Selections[i]);
    Item.Traverse(
      function(Prop: TLocalizerProperty): boolean
      begin
        Inc(Count);
        if (Prop.EffectiveStatus = ItemStatusTranslate) and (not Prop.IsUnused) then
          Inc(ElegibleCount);
        Result := True;
      end, False);
  end;

  if (ElegibleCount < Count) then
    ElegibleWarning :=  Format(sTranslateEligibleWarning, [Count-ElegibleCount])
  else
    ElegibleWarning :=  '';

  if (TaskMessageDlg(Format(sTranslateAutoPromptTitle, [TranslationService.ServiceName]), Format(sTranslateAutoPrompt, [ElegibleCount, ElegibleWarning]),
    mtConfirmation, [mbYes, mbNo], 0, mbNo) <> mrYes) then
    Exit;

  if (not TranslationService.BeginLookup(SourceLocaleItem, TargetLocaleItem)) then
    Exit;
  try

    TranslatedCount := 0;
    UpdatedCount := 0;
    Count := 0;

    SaveCursor(crHourGlass);
    Progress := ShowProgress(Format(sTranslateAutoProgress, [TranslationService.ServiceName]));
    Progress.EnableAbort := True;

    FProject.BeginUpdate;
    try

      for i := 0 to FocusedNode.TreeList.SelectionCount-1 do
      begin
        Item := NodeToItem(FocusedNode.TreeList.Selections[i]);

        Item.Traverse(
          function(Prop: TLocalizerProperty): boolean
          var
            Value, SourceValue, TranslatedValue: string;
          begin
            if (Prop.EffectiveStatus <> ItemStatusTranslate) or (Prop.IsUnused) then
              Exit(True);

            Inc(Count);

            SourceValue := SanitizeText(Prop.Value);
            if (Prop.HasTranslation(TargetLanguage)) then
              TranslatedValue := SanitizeText(Prop.TranslatedValue[TargetLanguage])
            else
              TranslatedValue := SourceValue;

            Progress.Progress(psProgress, Count, ElegibleCount, SourceValue);
            if (Progress.Aborted) then
              Exit(False);

            // Perform translation
            // Note: It is the responsibility of the translation service to return True/False to indicate
            // if SourceValue=TargetValue is in fact a translation.
            if (TranslationService.Lookup(Prop, SourceLocaleItem, TargetLocaleItem, SourceValue, Value)) then
            begin
              if (not Value.IsEmpty) then
              begin
                // If source is all UPPERCASE the target should also be so
                if (IsUppercase(SourceValue)) then
                  Value := AnsiUpperCase(Value)
                else
                // If source starts with an Uppercase letter the target should also do so
                if (StartsWithUppercase(SourceValue)) then
                  Value := Value[1].ToUpper+Copy(Value, 2, MaxInt);

                // Handle accelerator keys
                if (HasAccelerator(Prop.Value)) then
                begin
                  if (not HasAccelerator(Value)) then
                    // If source had an accelerator then make sure the target also has one
                    Value := AddAccelerator(EscapeAccelerators(Value));
                end else
                begin
                  if (HasAccelerator(Value)) then
                    // If source doesn't have an accelerator then make sure the target also doesn't have one
                    Value := SanitizeText(Value, [skAccelerator]);
                end;
              end;

              Inc(TranslatedCount);

              if (Prop.HasTranslation(TargetLanguage)) and (Prop.TranslatedValue[TargetLanguage] <> Value) then
                Inc(UpdatedCount);

              // Set value regardless of current value so we get the correct Status set
              Prop.TranslatedValue[TargetLanguage] := Value;

              ReloadProperty(Prop);
            end;
            Result := True;
          end, False);
      end;

      Progress.Progress(psEnd, Count, ElegibleCount);

    finally
      FProject.EndUpdate;
    end;

  finally
    TranslationService.EndLookup;
  end;

  Progress.Hide;
  Progress := nil;

  TaskMessageDlg(sTranslateAutoResultTitle, Format(sTranslateAutoResult, [TranslatedCount, UpdatedCount, ElegibleCount-TranslatedCount]),
    mtInformation, [mbOK], 0);
end;

procedure TFormMain.ActionAutomationMemoryTranslateExecute(Sender: TObject);
var
  TranslationService: ITranslationService;
begin
  TranslationService := FDataModuleTranslationMemory as ITranslationService;

  TranslateSelected(TranslationService);
end;

procedure TFormMain.ActionAutomationMemoryTranslateUpdate(Sender: TObject);
var
  Item: TCustomLocalizerItem;
begin
  Item := FocusedItem;

  TAction(Sender).Enabled := (Item <> nil) and (not Item.IsUnused) and (Item.EffectiveStatus <> ItemStatusDontTranslate) and
    (SourceLanguageID <> TargetLanguageID) and (FDataModuleTranslationMemory.IsAvailable);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionAutomationWebLookupExecute(Sender: TObject);
var
  Translator: TDataModuleTranslatorMicrosoftV3;
begin
  Translator := TDataModuleTranslatorMicrosoftV3.Create(nil);
  try

    TranslateSelected(Translator);

  finally
    Translator.Free;
  end;
end;

procedure TFormMain.ActionAutomationWebLookupUpdate(Sender: TObject);
var
  Item: TCustomLocalizerItem;
begin
  Item := FocusedItem;

  TAction(Sender).Enabled := (Item <> nil) and (not Item.IsUnused) and (Item.EffectiveStatus <> ItemStatusDontTranslate) and
    (SourceLanguageID <> TargetLanguageID);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionBookmarkExecute(Sender: TObject);
var
  Node: TcxTreeListNode;
  Prop: TLocalizerProperty;
  Flag: TPropertyFlag;
  Props: TArray<TLocalizerProperty>;
  i: integer;
  DoSet: boolean;
begin
  if (FocusedProperty = nil) then
    exit;

  Flag := TPropertyFlag(TAction(Sender).Tag);

  FLastBookmark := Ord(Flag);

  // If the any bookmark action is visible then we have been invoked from a Goto bookmark action.
  // Otherwise it must be from a Set boomark action.
  if (ActionGotoBookmarkAny.Visible) then
  begin
    if (not GotoNext(
      function(Prop: TLocalizerProperty): boolean
      begin
        Result := (Flag in Prop.Flags);
      end, Flag in [FlagBookmark0..FlagBookmark9])) then
      ShowMessage(sLocalizerFindNoMore);
  end else
  begin
    // Change the generic set boomark action to indicate what kind of bookmark it will set
    ActionEditMark.ImageIndex := TAction(Sender).ImageIndex;
    ActionEditMark.Tag := TAction(Sender).Tag;

    if (Flag in [FlagBookmark0..FlagBookmark9]) then
    begin
      // Operate on focused node
      Prop := FocusedProperty;

      // Setting bookmark on (single) item that already has same bookmark, clears the bookmark
      if (Flag in Prop.Flags) then
      begin
        Prop.ClearFlag(Flag);
        TreeListItems.FocusedNode.Repaint(True);
        Exit;
      end;

      // Clear existing bookmark...
      FProject.Traverse(
        function(Prop: TLocalizerProperty): boolean
        begin
          Result := True;
          if (Flag in Prop.Flags) then
          begin
            Prop.ClearFlag(Flag);
            Result := False;
            Node := TreeListItems.NodeFromHandle(Prop);
            if (Node <> nil) then
              Node.Repaint(True);
          end;
        end, False);

      // ...Then set new (on single item)
      Prop.SetFlag(Flag);
      TreeListItems.FocusedNode.Repaint(True);
    end else
    begin
      // Operate on selected nodes
      SetLength(Props, TreeListItems.SelectionCount);
      for i := 0 to TreeListItems.SelectionCount-1 do
        Props[i] := TLocalizerProperty(TreeListItems.HandleFromNode(TreeListItems.Selections[i]));

      // Clear if ALL already has bookmark, otherwise Set
      DoSet := False;
      for Prop in Props do
      begin
        if (not (Flag in Prop.Flags)) then
        begin
          DoSet := True;
          break;
        end;
      end;

      // Set on selected items
      for Prop in Props do
      begin
        if (DoSet) then
          Prop.SetFlag(Flag)
        else
          Prop.ClearFlag(Flag);
      end;

      for i := 0 to TreeListItems.SelectionCount-1 do
        TreeListItems.Selections[i].Repaint(True);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionBuildExecute(Sender: TObject);
var
  ProjectProcessor: TProjectResourceProcessor;
  ResourceWriter: IResourceWriter;
  Filename, Path: string;
  LocaleItem: TLocaleItem;
  Filter: string;
resourcestring
  sLocalizerResourceModuleFilenamePrompt = 'Enter filename of resource module';
  sLocalizerResourceModuleBuilt = 'Resource module built:'#13'%s';
begin
  if (not CheckSourceFile) then
    Exit;

  CheckStringsSymbolFile;

  LocaleItem := TLocaleItems.FindLCID(TargetLanguageID);

  if (LocaleItem <> nil) then
    Filename := TPath.ChangeExtension(FProject.SourceFilename, '.'+LocaleItem.LanguageShortName)
  else
    Filename := TPath.ChangeExtension(FProject.SourceFilename, '.dll');

  Path := TPath.GetDirectoryName(Filename);
  Filename := TPath.GetFileName(Filename);

  Filter := SaveDialogEXE.Filter;
  if (LocaleItem <> nil) then
    Filter := Format(sResourceModuleFilter, [LocaleItem.LanguageName, LocaleItem.LanguageShortName]) + Filter;

  if (not PromptForFileName(Filename, Filter, '', sLocalizerResourceModuleFilenamePrompt, Path, True)) then
    Exit;

  SaveCursor(crHourGlass);

  ProjectProcessor := TProjectResourceProcessor.Create;
  try
    ProjectProcessor.IncludeVersionInfo := TranslationManagerSettings.System.IncludeVersionInfo;

    FProject.BeginLoad;
    try

      ResourceWriter := TResourceModuleWriter.Create(Filename);
      try

        ProjectProcessor.Execute(liaTranslate, FProject, FProject.SourceFilename, TargetLanguage, ResourceWriter);

      finally
        ResourceWriter := nil;
      end;

    finally
      FProject.EndLoad;
    end;
  finally
    ProjectProcessor.Free;
  end;

  ShowMessageFmt(sLocalizerResourceModuleBuilt, [Filename]);

  LoadProject(FProject, False);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionImportFileExecute(Sender: TObject);
begin
//
end;

procedure TFormMain.ActionImportFileSourceExecute(Sender: TObject);
var
  Filename: string;
  SaveFilename: string;
  ProjectProcessor: TProjectResourceProcessor;
  CountBefore, CountAfter: TCounts;
  Msg: string;
resourcestring
  sSwitchSourceModuleTitle = 'Update project?';
  sSwitchSourceModule = 'Do you want to alter the project to use this file as the source file?';
begin
  if (not OpenDialogEXE.Execute(Handle)) then
    exit;

  Filename := OpenDialogEXE.FileName;
  OpenDialogEXE.InitialDir := TPath.GetDirectoryName(Filename);

  // Temporarily switch to new file or we will not be able to find the companion files (*.drc)
  SaveFilename := FProject.SourceFilename;
  try
    FProject.SourceFilename := Filename;

    CheckStringsSymbolFile;

    SaveCursor(crHourGlass);

    CountBefore := CountStuff;

    ProjectProcessor := TProjectResourceProcessor.Create;
    try

      ProjectProcessor.ScanProject(FProject, Filename);

    finally
      ProjectProcessor.Free;
    end;
  finally
    FProject.SourceFilename := SaveFilename;
  end;

  CountAfter := CountStuff;

  LoadProject(FProject, False);

  // Display update statistics
  if (FProject.Modified) then
    Msg := Format(sProjectUpdated,
      [
      1.0*(CountAfter.CountModule-CountBefore.CountModule), 1.0*(CountAfter.UnusedModule-CountBefore.UnusedModule),
      1.0*(CountAfter.CountItem-CountBefore.CountItem), 1.0*(CountAfter.UnusedItem-CountBefore.UnusedItem),
      1.0*(CountAfter.CountProperty-CountBefore.CountProperty), 1.0*(CountAfter.UnusedProperty-CountBefore.UnusedProperty),
      1.0*(CountAfter.ObsoleteTranslation-CountBefore.ObsoleteTranslation)
      ])
  else
    Msg := sProjectUpdatedNothing;

  Msg := Msg + #13#13 +sSwitchSourceModule;

  if (TaskMessageDlg(sProjectUpdatedTitle, Msg, mtConfirmation, [mbYes, mbNo], 0, mbNo) = mrYes) then
  begin
    FProject.SourceFilename := Filename;
    FProject.Name := TPath.GetFileNameWithoutExtension(Filename);
    RibbonMain.DocumentName := FProject.Name;

    FProject.Changed;
  end;
end;

procedure TFormMain.ActionImportFileTargetExecute(Sender: TObject);
var
  ProjectProcessor: TProjectResourceProcessor;
  Stats: TTranslationCounts;
  SaveFilter: string;
  LocaleItem: TLocaleItem;
begin
  SaveFilter := OpenDialogEXE.Filter;
  try

    LocaleItem := TLocaleItems.FindLCID(TargetLanguageID);
    OpenDialogEXE.Filter := Format(sResourceModuleFilter, [LocaleItem.LanguageName, LocaleItem.LanguageShortName]) + SaveFilter;
    OpenDialogEXE.FilterIndex := 1;

    if (not OpenDialogEXE.Execute(Handle)) then
      exit;

  finally
    OpenDialogEXE.Filter := SaveFilter;
  end;

  SaveCursor(crHourGlass);

  OpenDialogEXE.InitialDir := TPath.GetDirectoryName(OpenDialogEXE.FileName);

  ProjectProcessor := TProjectResourceProcessor.Create;
  try

    ProjectProcessor.Execute(liaUpdateTarget, FProject, OpenDialogEXE.FileName, TargetLanguage, nil);

    Stats := ProjectProcessor.TranslationCount;

  finally
    ProjectProcessor.Free;
  end;

  LoadProject(FProject, False);

  TaskMessageDlg(sTranslationsUpdatedTitle,
    Format(sTranslationsUpdated, [1.0 * Stats.CountAdded, 1.0 * Stats.CountUpdated, 1.0 * Stats.CountSkipped]),
    mtInformation, [mbOK], 0);
end;

procedure TFormMain.ActionImportFileTargetUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FProject.Modules.Count > 0) and (SourceLanguageID <> TargetLanguageID);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionEditMarkExecute(Sender: TObject);
var
  p: TPoint;
begin
  // Default to BookmarkA
  if (TAction(Sender).Tag = -1) then
    TAction(Sender).Tag := Ord(FlagBookmarkA);

  // Disable ActionGotoBookmarkAny
  // This also signals OnActionBookmarkExecute that we are setting a bookmark as opposed to navigating to one.
  ActionGotoBookmarkAny.Visible := False;
  ActionGotoBookmarkAny.Enabled := ActionGotoBookmarkAny.Visible;

  // Dropdown if we were invoked from toolbar, otherwise popup at cursor.
  // This solves the problem that the dropdown menu is inaccessible unless the tab it's on is active.
  if (ButtonItemBookmark.ClickItemLink <> nil) then
  begin
    if (GetKeyState(VK_SHIFT) and $8000 = 0) then
      // Use generic bookmark handler to set to last used bookmark
      ActionBookmarkExecute(Sender)
    else
      ButtonItemBookmark.DropDown
  end else
  begin
    if (GetKeyState(VK_SHIFT) and $8000 = 0) then
      // Use generic bookmark handler to set to last used bookmark
      ActionBookmarkExecute(Sender)
    else
    begin
      p.X := Left + Width div 2;
      p.Y := Top + Height div 2;
      ButtonItemBookmark.DropDownMenu.Popup(p.X, p.Y);
    end;
  end;
end;

procedure TFormMain.ActionEditMarkUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FocusedProperty <> nil);

  TAction(Sender).Checked := TAction(Sender).Enabled and (FocusedProperty.Flags * [FlagBookmark0..FlagBookmarkF] <> []);
  // We have to manually sync the button because bsChecked always behave like AutoCheck=True (and DevExpress won't admit they've got it wrong)
  ButtonItemBookmark.Down := TAction(Sender).Checked;
end;

procedure TFormMain.ActionGotoBookmarkAnyExecute(Sender: TObject);
begin
  FLastBookmark := -1;
  if (not GotoNext(
    function(Prop: TLocalizerProperty): boolean
    begin
      Result := (Prop.Flags * [FlagBookmark0..FlagBookmarkF] <> []);
    end)) then
    ShowMessage(sLocalizerFindNoMore);
end;

procedure TFormMain.ActionGotoNextBookmarkExecute(Sender: TObject);
var
  p: TPoint;
begin
  // Enable ActionGotoBookmarkAny
  // This also signals OnActionBookmarkExecute that we are setting a bookmark as opposed to navigating to one.
  ActionGotoBookmarkAny.Visible := True;
  ActionGotoBookmarkAny.Enabled := True;

  // Dropdown if we were invoked from toolbar, otherwise popup at cursor.
  // This solves the problem that the dropdown menu is inaccessible unless the tab it's on is active.
  if (ButtonGotoBookmark.ClickItemLink <> nil) then
  begin
    ButtonGotoBookmark.DropDown
  end else
  begin
    p.X := Left + Width div 2;
    p.Y := Top + Height div 2;
    ButtonGotoBookmark.DropDownMenu.Popup(p.X, p.Y);
  end;
end;

procedure TFormMain.ActionGotoNextStateExecute(Sender: TObject);
begin
  if (not GotoNext(
    function(Prop: TLocalizerProperty): boolean
    begin
      Result := (TLocalizerItemState(TAction(Sender).Tag) in Prop.State);
    end)) then
    ShowMessage(sLocalizerFindNoMore);
end;

procedure TFormMain.ActionGotoNextStatusExecute(Sender: TObject);
begin
  if (not GotoNext(
    function(Prop: TLocalizerProperty): boolean
    begin
      Result := (Prop.EffectiveStatus = TLocalizerItemStatus(TAction(Sender).Tag));
    end)) then
    ShowMessage(sLocalizerFindNoMore);
end;

procedure TFormMain.ActionGotoNextUntranslatedExecute(Sender: TObject);
begin
  if (not GotoNext(
    function(Prop: TLocalizerProperty): boolean
    var
      Translation: TLocalizerTranslation;
    begin
      if (Prop.EffectiveStatus <> ItemStatusTranslate) or (Prop.IsUnused) then
        Exit(False);

      Result := (not Prop.Translations.TryGetTranslation(TargetLanguage, Translation)) or (Translation.Status = tStatusPending);
    end)) then
    ShowMessage(sLocalizerFindNoMore);
end;

procedure TFormMain.ActionGotoNextWarningExecute(Sender: TObject);
begin
  if (not GotoNext(
    function(Prop: TLocalizerProperty): boolean
    var
      Translation: TLocalizerTranslation;
    begin
      if (Prop.EffectiveStatus <> ItemStatusTranslate) or (Prop.IsUnused) then
        Exit(False);

      Result := (Prop.Translations.TryGetTranslation(TargetLanguage, Translation)) and (Translation.Warnings <> []);
    end)) then
    ShowMessage(sLocalizerFindNoMore);
end;

procedure TFormMain.ActionFindSearchExecute(Sender: TObject);
begin
  if (FSearchProvider = nil) then
    FSearchProvider := TFormSearch.Create(Self);

  // Make sure we have a module selected or the search dialog will burn
  if (FocusedModule = nil) then
    TreeListModules.Root.GetFirstChildVisible.Focused := True;

  FSearchProvider.Show;
end;

procedure TFormMain.ActionFeedbackHideExecute(Sender: TObject);
begin
  RibbonMain.TabAreaToolbar.Visible := False;
end;

procedure TFormMain.ActionFeedbackNegativeExecute(Sender: TObject);
var
  FormFeedback: TFormFeedback;
begin
  FormFeedback := TFormFeedback.Create(nil);
  try
    FormFeedback.Execute(FeedbackNegative);
  finally
    FormFeedback.Free;
  end;
end;

procedure TFormMain.ActionFeedbackPositiveExecute(Sender: TObject);
var
  FormFeedback: TFormFeedback;
begin
  FormFeedback := TFormFeedback.Create(nil);
  try
    FormFeedback.Execute(FeedbackPositive);
  finally
    FormFeedback.Free;
  end;
end;

procedure TFormMain.ActionFindNextExecute(Sender: TObject);
begin
  FSearchProvider.SelectNextResult;
end;

procedure TFormMain.ActionFindNextUpdate(Sender: TObject);
begin
  TAction(sender).Enabled := (FSearchProvider <> nil) and (FSearchProvider.CanSelectNextResult);
end;

procedure TFormMain.ActionImportXLIFFExecute(Sender: TObject);
var
  Importer: TModuleImporterXLIFF;
  Stats: TTranslationCounts;
begin
  if (not OpenDialogXLIFF.Execute(Handle)) then
    exit;

  OpenDialogXLIFF.InitialDir := TPath.GetDirectoryName(OpenDialogXLIFF.FileName);

  SaveCursor(crHourGlass);

  Importer := TModuleImporterXLIFF.Create;
  try

    Importer.LoadFromFile(FProject, OpenDialogXLIFF.FileName);

    Stats := Importer.TranslationCounts;

  finally
    Importer.Free;
  end;

  LoadProject(FProject, False);

  TaskMessageDlg(sTranslationsUpdatedTitle,
    Format(sTranslationsUpdated, [1.0 * Stats.CountAdded, 1.0 * Stats.CountUpdated, 1.0 * Stats.CountSkipped]),
    mtInformation, [mbOK], 0);
end;

// -----------------------------------------------------------------------------

function TFormMain.CheckStringsSymbolFile: boolean;
var
  Res: Word;
  Filename: string;
resourcestring
  sStringSymbolsNotFoundTitle = 'Symbol file not found';
  sStringSymbolsNotFound = 'The resourcestring symbol file does not exist in the same folder as the source application file.'+#13#13+
    'The name of the file is expected to be: %s'+#13#13+
    'The file is required for support of resourcestrings.'+#13#13+
    'Do you want to locate the file now?';
begin
  Result := True;

  if (FProject.StringSymbolFilename = '') then
    FProject.StringSymbolFilename := TPath.ChangeExtension(FProject.SourceFilename, '.drc');

  Filename := FProject.StringSymbolFilename;
  while (not TFile.Exists(Filename)) do
  begin
    Result := False;

    // Symbol file does not exist. Try to locate it.
    Res := TaskMessageDlg(sStringSymbolsNotFoundTitle, Format(sStringSymbolsNotFound, [FProject.StringSymbolFilename]),
      mtWarning, [mbYes, mbNo], 0, mbYes);
    if (Res <> mrYes) then
      Exit(False);

    OpenDialogDRC.InitialDir := TPath.GetDirectoryName(Filename);
    OpenDialogDRC.FileName := TPath.GetFileName(Filename);

    if (not OpenDialogDRC.Execute(Handle)) then
      Exit(False);

    Filename := OpenDialogDRC.FileName;
  end;

  if (not Result) then
    FProject.StringSymbolFilename := Filename;

  Result := True;
end;

function TFormMain.CheckSourceFile: boolean;
var
  Res: Word;
  Filename: string;
resourcestring
  sSourceFileNotFoundTitle = 'Source file not found';
  sSourceFileNotFound = 'The source application file specified in the project does not exist.'+#13#13+
    'Filename: %s'+#13#13+
    'Do you want to locate the file now?';
begin
  Result := True;

  Filename := FProject.SourceFilename;
  while (not TFile.Exists(Filename)) do
  begin
    Result := False;

    // Symbol file does not exist. Try to locate it.
    Res := TaskMessageDlg(sSourceFileNotFoundTitle, Format(sSourceFileNotFound, [FProject.SourceFilename]),
      mtWarning, [mbYes, mbNo], 0, mbYes);
    if (Res <> mrYes) then
      Exit(False);

    OpenDialogEXE.InitialDir := TPath.GetDirectoryName(Filename);
    OpenDialogEXE.FileName := TPath.GetFileName(Filename);

    if (not OpenDialogEXE.Execute(Handle)) then
      Exit(False);

    Filename := OpenDialogEXE.FileName;
  end;

  if (not Result) then
    FProject.SourceFilename := Filename;

  Result := True;
end;

procedure TFormMain.ActionProjectNewExecute(Sender: TObject);
var
  FormNewProject: TFormNewProject;
  ProjectProcessor: TProjectResourceProcessor;
  Count: TCounts;
resourcestring
  sProjectInitializedTitle = 'Project initialized';
  sProjectInitialized = 'Project has been initialized. The following resources was read from the source file:'#13#13+
    'Modules: %.0n'#13+
    'Items: %.0n'#13+
    'Properties: %.0n';
begin
  if (not CheckSave) then
    exit;

  FormNewProject := TFormNewProject.Create(nil);
  try
    FormNewProject.SetLanguageView(DataModuleMain.GridTableViewLanguages, DataModuleMain.GridTableViewLanguagesColumnLanguage);

    if (FProject.SourceFilename <> '') then
      FormNewProject.SourceApplication := FProject.SourceFilename
    else
    if (TranslationManagerSettings.Folders.RecentApplications.Count > 0) then
      FormNewProject.SourceApplication := TranslationManagerSettings.Folders.RecentApplications[0]
    else
      FormNewProject.SourceApplication := Application.ExeName;

    FormNewProject.SourceLanguageID := GetLanguageID(TranslationManagerSettings.System.DefaultSourceLanguage);

    if (not FormNewProject.Execute) then
      exit;

    InitializeProject(FormNewProject.SourceApplication, FormNewProject.SourceLanguageID);
    FProjectFilename := '';

  finally
    FormNewProject.Free;
  end;

  if (not CheckSourceFile) then
    Exit;
  CheckStringsSymbolFile;

  // Initial scan
  SaveCursor(crHourGlass);

  ProjectProcessor := TProjectResourceProcessor.Create;
  try

    ProjectProcessor.ScanProject(FProject, FProject.SourceFilename);

  finally
    ProjectProcessor.Free;
  end;

  Count := CountStuff;

  // Display update statistics
  TaskMessageDlg(sProjectInitializedTitle,
    Format(sProjectInitialized, [1.0*Count.CountModule, 1.0*Count.CountItem, 1.0*Count.CountProperty]),
    mtInformation, [mbOK], 0);

  LoadProject(FProject);
end;

procedure TFormMain.LoadFromFile(const Filename: string);
var
  BestLanguage: TTargetLanguage;
  i: integer;
  Progress: IProgress;
resourcestring
  sProgressProjectLoading = 'Loading project...';
begin
  SaveCursor(crHourGlass);

  ClearDependents;
  ClearTargetLanguage;
  FTranslationCounts.Clear;
  TreeListItems.Clear;
  TreeListModules.Clear;

  Progress := ShowProgress(sProgressProjectLoading);
  try
    Progress.Marquee := True;

    FProject.BeginUpdate;
    try

      FProjectFilename := Filename;
      try

        TLocalizationProjectFiler.LoadFromFile(FProject, FProjectFilename);

      except
        FProjectFilename := '';
        raise;
      end;

      FProject.Modified := False;

    finally
      FProject.EndUpdate;
    end;

    UpdateProjectModifiedIndicator;

    RibbonMain.DocumentName := FProject.Name;

    // Find language with most translations
    BestLanguage := nil;
    for i := 0 to FProject.TargetLanguages.Count-1 do
      if (BestLanguage = nil) or (FProject.TargetLanguages[i].TranslatedCount >= BestLanguage.TranslatedCount) then
        BestLanguage := FProject.TargetLanguages[i];

    if (BestLanguage <> nil) then
    begin
      TargetLanguageID := BestLanguage.LanguageID;
      FFilterTargetLanguages := True;
    end else
    begin
      TargetLanguageID := SourceLanguageID;
      FFilterTargetLanguages := False;
    end;

    LoadProject(FProject);

  finally
    Progress.Hide;
    Progress := nil;
  end;

  AddRecentFile(FProjectFilename);

  if (CheckSourceFile) then
    CheckStringsSymbolFile;
end;

procedure TFormMain.LoadFromSingleInstance(const Param: string);
begin
  // Called by SingleInst.
  // Continue processing param out of band to avoid blocking this call; If we are too slow the shell will spawn another
  // instance to handle the request.
  FPendingFileOpenLock.Enter;
  try
    if (FPendingFileOpen = nil) then
      FPendingFileOpen := TStringList.Create;

    FPendingFileOpen.Add(Param);
    PostMessage(Handle, MSG_FILE_OPEN, Ord(True), 0);
  finally
    FPendingFileOpenLock.Leave;
  end;
end;

procedure TFormMain.MsgAfterShow(var Msg: TMessage);
begin
  TranslationManagerSettings.System.EndBoot;

  // Release semaphore once SingleInstance handling has been set up and the boot marker has been cleared
  ReleaseRestartSemaphore;

  // Register shell integration on first run
  if (TranslationManagerSettings.System.FirstRun) then
    TranslationManagerShell.RegisterShellIntegration;


  InitializeProject('', GetLanguageID(TranslationManagerSettings.System.DefaultSourceLanguage));

  // We could just as well have called LoadFromFile directly here but what the hell...
  if (ParamCount > 0) then
  begin
    FPendingFileOpenLock.Enter;
    try
      if (FPendingFileOpen = nil) then
        FPendingFileOpen := TStringList.Create;

      FPendingFileOpen.Add(ParamStr(1));
      PostMessage(Handle, MSG_FILE_OPEN, Ord(True), 0);
    finally
      FPendingFileOpenLock.Leave;
    end;
  end;
end;

procedure TFormMain.MsgFileOpen(var Msg: TMsgFileOpen);
var
  Filename: string;
begin
  ASSERT(FPendingFileOpen <> nil);
  ASSERT(FPendingFileOpenLock <> nil);

  FPendingFileOpenLock.Enter;
  try
    if (FPendingFileOpen.Count = 0) then
      exit;

    Filename := FPendingFileOpen[0];
    FPendingFileOpen.Delete(0);

    if (FPendingFileOpen.Count > 0) then
      // Repost the message to handle the next pending file
      PostMessage(Handle, MSG_FILE_OPEN, Msg.WParam, Msg.LParam);
  finally
    FPendingFileOpenLock.Leave;
  end;

  if (not CheckSave) then
    exit;

  if (Msg.CommandLine) then
    // Filename is a command line argument, shell execute document, url or the like
    LoadFromFile(Filename)
  else
    // Filename is a physical file path (from drag/drop)
    LoadFromFile(Filename);
end;

procedure TFormMain.ActionProjectOpenExecute(Sender: TObject);
begin
  if (not CheckSave) then
    exit;

  if (not OpenDialogProject.Execute(Handle)) then
    exit;

  SaveCursor(crHourGlass);

  OpenDialogProject.InitialDir := TPath.GetDirectoryName(OpenDialogProject.FileName);

  LoadFromFile(OpenDialogProject.FileName);
end;

procedure TFormMain.ActionProjectPurgeExecute(Sender: TObject);
var
  CurrentModule: TLocalizerModule;
  Module, LoopModule: TLocalizerModule;
  Item, LoopItem: TLocalizerItem;
  Prop: TLocalizerProperty;
  NeedReload: boolean;
  CountBefore, CountAfter: TCounts;
  Node: TcxTreeListNode;
  Msg: string;
resourcestring
  sLocalizerPurgeStatusTitle = 'Purge completed.';
  sLocalizerPurgeStatus = 'The following was removed from the project:'#13#13+
    'Modules: %.0n'#13'Items: %.0n'#13'Properties: %.0n';
begin
  SaveCursor(crHourGlass);

  NeedReload := False;
  CurrentModule := FocusedModule;

  CountBefore := CountStuff;

  TreeListItems.BeginUpdate;
  try
    try

      for LoopModule in FProject.Modules.Values.ToArray do // ToArray for stability since we delete from dictionary
      begin
        Module := LoopModule;
        Module.BeginUpdate;
        try
          if (Module.Kind = mkOther) or (Module.IsUnused) then
          begin
            NeedReload := True;
            if (Module = CurrentModule) then
            begin
              FLocalizerDataSource.Module := nil;
              CurrentModule := nil;
            end;
            Node := TreeListModules.Find(Module, TreeListModules.Root, False, True, TreeListFindFilter);
            Node.Free;
            RemoveTranslatedCount(Module);
            FreeAndNil(Module);
            continue;
          end;

          for LoopItem in Module.Items.Values.ToArray do // ToArray for stability since we delete from dictionary
          begin
            Item := LoopItem;
            Item.BeginUpdate;
            try
              if (Item.IsUnused) then
              begin
                NeedReload := True;
                if (Module = CurrentModule) then
                begin
                  FLocalizerDataSource.Module := nil;
                  CurrentModule := nil;
                end;
                FreeAndNil(Item);
                continue;
              end;

              // TODO : Purge obsolete translations?
              for Prop in Item.Properties.Values.ToArray do // ToArray for stability since we delete from dictionary
                if (Prop.IsUnused) then
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
                      CurrentModule := nil;
                    end;
                  end;
                  Prop.Free;
                end;

              if (Item.Properties.Count = 0) then
              begin
                NeedReload := True;
                if (Module = CurrentModule) then
                begin
                  FLocalizerDataSource.Module := nil;
                  CurrentModule := nil;
                end;
                FreeAndNil(Item);
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
          end;

        finally
          if (Module <> nil) then
            Module.EndUpdate;
        end;
      end;

      if (NeedReload) then
      begin
        FProject.Modified := True;

        LoadProject(FProject, False);
      end;

    finally
      if (TreeListItems.CustomDataSource = nil) then
        TreeListItems.CustomDataSource := FLocalizerDataSource;
    end;
  finally
    TreeListItems.EndUpdate;
  end;

  CountAfter := CountStuff;

  Msg := Format(sLocalizerPurgeStatus,
    [
    1.0*(CountBefore.CountModule-CountAfter.CountModule),
    1.0*(CountBefore.CountItem-CountAfter.CountItem),
    1.0*(CountBefore.CountProperty-CountAfter.CountProperty)
    ]);
  TaskMessageDlg(sLocalizerPurgeStatusTitle, Msg, mtInformation, [mbOK], 0);
end;

procedure TFormMain.ActionProjectSaveExecute(Sender: TObject);
var
  Filename, TempFilename, BackupFilename: string;
  i: integer;
  Progress: IProgress;
resourcestring
  sProgressProjectLoading = 'Loading project...';
  sProgressProjectSaving = 'Saving project...';
begin
  SaveCursor(crHourGlass);

  Filename := FProjectFilename;

  if (Filename = '') then
    Filename := TPath.ChangeExtension(FProject.SourceFilename, TranslationManagerShell.sProjectFileType);

  SaveDialogProject.InitialDir := TPath.GetDirectoryName(Filename);
  SaveDialogProject.FileName := TPath.GetFileName(Filename);

  if (not SaveDialogProject.Execute(Handle)) then
    Exit;

  Filename := SaveDialogProject.FileName;

  Progress := ShowProgress(sProgressProjectSaving);
  try
    Progress.Marquee := True;

    TempFilename := Filename;

    // Save to temporary file if destination file already exist
    if (TFile.Exists(Filename)) then
    begin
      i := 0;
      repeat
        TempFilename := Format('%s\savefile%.4X%s', [TPath.GetDirectoryName(Filename), i, TranslationManagerShell.sProjectFileType]);
        Inc(i);
      until (not TFile.Exists(TempFilename));
    end;

    // Save file
    TLocalizationProjectFiler.SaveToFile(FProject, TempFilename);

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

    FProjectFilename := Filename;
    FProject.Modified := False;

    SetInfoText('Saved');
    UpdateProjectModifiedIndicator;
  finally
    Progress := nil;
  end;

  AddRecentFile(Filename);
end;

procedure TFormMain.ActionProjectSaveUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (not FProject.Name.IsEmpty) and (FProject.Modified);
end;

procedure TFormMain.ActionProjectUpdateExecute(Sender: TObject);
var
  ProjectProcessor: TProjectResourceProcessor;
  CountBefore, CountAfter: TCounts;
  Msg: string;
  SaveModified: boolean;
  WasModified: boolean;
begin
  if (not CheckSourceFile) then
    Exit;

  CheckStringsSymbolFile;

  SaveCursor(crHourGlass);

  CountBefore := CountStuff;

  SaveModified := FProject.Modified;
  FProject.Modified := False;

  ProjectProcessor := TProjectResourceProcessor.Create;
  try

    ProjectProcessor.ScanProject(FProject, FProject.SourceFilename);

  finally
    ProjectProcessor.Free;
  end;

  // Doesn't work:
  // Project will always be modified as some of the modules that are created during
  // the resource scan will be found to not be DFMs and thus deleted before completion.
  // Creation of these modules causes change notification.
  WasModified := FProject.Modified;
  FProject.Modified := WasModified or SaveModified;

  CountAfter := CountStuff;

  LoadProject(FProject, False);

  // Display update statistics
  if (WasModified) then
  begin
    Msg := Format(sProjectUpdated,
      [
      1.0*(CountAfter.CountModule-CountBefore.CountModule), 1.0*(CountAfter.UnusedModule-CountBefore.UnusedModule),
      1.0*(CountAfter.CountItem-CountBefore.CountItem), 1.0*(CountAfter.UnusedItem-CountBefore.UnusedItem),
      1.0*(CountAfter.CountProperty-CountBefore.CountProperty), 1.0*(CountAfter.UnusedProperty-CountBefore.UnusedProperty),
      1.0*(CountAfter.ObsoleteTranslation-CountBefore.ObsoleteTranslation)
      ]);
    TaskMessageDlg(sProjectUpdatedTitle, Msg, mtInformation, [mbOK], 0);
  end else
    TaskMessageDlg(sProjectUpdatedTitle, sProjectUpdatedNothing, mtInformation, [mbOK], 0);
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

  FProject.Traverse(
    function(Prop: TLocalizerProperty): boolean
    begin
      Result := PerformSpellCheck(Prop);
    end, False);

  SpellChecker.ShowSpellingCompleteMessage;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionSettingsExecute(Sender: TObject);
var
  FormSettings: TFormSettings;
begin
  FormSettings := TFormSettings.Create(nil);
  try
    FormSettings.RibbonStyle := RibbonMain.Style;
    FormSettings.SpellChecker := SpellChecker;

    if (not FormSettings.Execute) then
      Exit;

    Skin := TranslationManagerSettings.System.Skin;

    if (FormSettings.RestartRequired) then
    begin
      if (not QueueRestart(True)) then
        exit;
    end;
  finally
    FormSettings.Free;
  end;
end;

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
  TAction(Sender).Enabled := (FocusedNode <> nil) and (FocusedItem <> nil) and (not FocusedItem.IsUnused);

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
  TAction(Sender).Enabled := (FocusedNode <> nil) and (FocusedItem <> nil) and (not FocusedItem.IsUnused);

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
  TAction(Sender).Enabled := (FocusedNode <> nil) and (FocusedItem <> nil) and (not FocusedItem.IsUnused);

  TAction(Sender).Checked := (TAction(Sender).Enabled) and (FocusedItem.Status = ItemStatusTranslate);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionValidateExecute(Sender: TObject);
var
  WarningCount, NewWarningCount: integer;
  NeedRefresh: boolean;
  CurrentModule: TLocalizerModule;
resourcestring
  sLocalizerWarningsNone = 'No validation problems found';
  sLocalizerWarnings = 'Validation found %d problems in the project. Of these %d are new';
begin
  SaveCursor(crHourGlass);

  CurrentModule := FocusedModule;
  NeedRefresh := False;
  WarningCount := 0;
  NewWarningCount := 0;

  FProject.Traverse(
    function(Prop: TLocalizerProperty): boolean
    var
      Translation: TLocalizerTranslation;
      OldWarnings: TTranslationWarnings;
      Warning: TTranslationWarning;
    begin
      if (Prop.EffectiveStatus = ItemStatusTranslate) and (not Prop.IsUnused) then
      begin
        if (Prop.Translations.TryGetTranslation(TargetLanguage, Translation)) then
        begin
          OldWarnings := Translation.Warnings;
          Translation.UpdateWarnings;

          for Warning in Translation.Warnings do
            Inc(WarningCount);

          if (Translation.Warnings <> OldWarnings) then
          begin
            for Warning in Translation.Warnings-OldWarnings do
              Inc(NewWarningCount);

            // Refresh if warnings changed and translation belongs to current module
            if (Prop.Item.Module = CurrentModule) then
              NeedRefresh := True;
          end;
        end;
      end;

      Result := True;
    end, False);

  // Update node state images
  if (NeedRefresh) then
    TreeListItems.FullRefresh;

  if (WarningCount = 0) then
    MessageDlg(sLocalizerWarningsNone, mtInformation, [mbOK], 0)
  else
    MessageDlg(Format(sLocalizerWarnings, [WarningCount, NewWarningCount]), mtWarning, [mbOK], 0);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionTranslationStateUpdate(Sender: TObject);
var
  Prop: TLocalizerProperty;
begin
  Prop := FocusedProperty;

  TAction(Sender).Enabled := (Prop <> nil) and (not Prop.IsUnused) and (Prop.EffectiveStatus <> ItemStatusDontTranslate);
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
  FProject.Modified := True;
  UpdateProjectModifiedIndicator;
  PostMessage(Handle, MSG_SOURCE_CHANGED, 0, 0);
end;

procedure TFormMain.BarEditItemTargetLanguageEnter(Sender: TObject);
begin
  DataModuleMain.FilterTargetLanguages := FFilterTargetLanguages and
    ((FProject.TargetLanguages.Count > 1) or
     ((FProject.TargetLanguages.Count = 1) and (FProject.TargetLanguages[0].LanguageID <> SourceLanguageID)));

  DataModuleMain.Project := FProject;
end;

procedure TFormMain.BarEditItemTargetLanguageExit(Sender: TObject);
begin
  DataModuleMain.FilterTargetLanguages := False;
  DataModuleMain.Project := nil;
end;

procedure TFormMain.BarEditItemTargetLanguagePropertiesEditValueChanged(Sender: TObject);
begin
  PostMessage(Handle, MSG_TARGET_CHANGED, 0, 0);
end;

procedure TFormMain.BarEditItemTargetLanguagePropertiesInitPopup(Sender: TObject);
begin
  // BUG: The Properties.OnInitPoup isn't fired when a reposity item is assigned.
  // Instead we use the OnEnter and OnExit events
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

    for i := 0 to FProject.TargetLanguages.Count-1 do
      FormLanguages.SelectTargetLanguage(FProject.TargetLanguages[i].LanguageID);

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
          for i := FProject.TargetLanguages.Count-1 downto 0 do
            if (not Languages.Contains(FProject.TargetLanguages[i].LanguageID)) then
            begin
              DeleteLanguages.Add(FProject.TargetLanguages[i]);

              if (FProject.TargetLanguages[i].TranslatedCount > 0) then
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
          begin
            ClearDependents;
            ClearTargetLanguage;
          end;

          // Delete translations
          FProject.Traverse(
            function(Prop: TLocalizerProperty): boolean
            begin
              Prop.Translations.Remove(Language);
              Result := True;
            end, False);

          Assert(Language.TranslatedCount = 0);

          FProject.TargetLanguages.Remove(Language.LanguageID);
        end;

      finally
        DeleteLanguages.Free;
      end;

      // Add selected languages
      for i := 0 to FormLanguages.TargetLanguageCount-1 do
        FProject.TargetLanguages.Add(FormLanguages.TargetLanguage[i]);

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

procedure TFormMain.ButtonOpenRecentClick(Sender: TObject);
begin
  if (not CheckSave) then
    exit;

  LoadFromFile(TdxBarItem(Sender).Hint);
end;

// -----------------------------------------------------------------------------

function TFormMain.CheckSave: boolean;
var
  Res: integer;
resourcestring
  sLocalizerSavePromptTitle = 'Project has not been saved';
  sLocalizerSavePrompt = 'Your changes has not been saved.'#13#13'Do you want to save them now?';
begin
  if (FProject.Modified) then
  begin
    Res := TaskMessageDlg(sLocalizerSavePromptTitle, sLocalizerSavePrompt,
      mtConfirmation, [mbYes, mbNo, mbCancel], 0, mbCancel);

    if (Res = mrCancel) then
      Exit(False);

    if (Res = mrYes) then
    begin
      ActionProjectSave.Execute;
      Result := (not FProject.Modified);
    end else
      Result := True;
  end else
    Result := True;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckSave and FDataModuleTranslationMemory.CheckSave;
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

function TFormMain.GetNodeValidationMessage(Node: TcxTreeListNode): string;
var
  Prop: TLocalizerProperty;
  Translation: TLocalizerTranslation;
  Warning: TTranslationWarning;
const
  sTranslationValidationWarnings: array[TTranslationWarning] of string = (
    'Source or translation is empty and the other is not',
    'Accelerator count mismatch',
    'Format specifier count mismatch',
    'Linebreak count mismatch',
    'Leading space count mismatch',
    'Trailing space count mismatch',
    'Translation is terminated differently than source');
begin
  Result := '';

  Prop := TLocalizerProperty(TreeListItems.HandleFromNode(Node));

  if (not Prop.Translations.TryGetTranslation(TargetLanguage, Translation)) then
    Exit;

  if (Translation <> nil) and (Translation.Warnings <> []) then
  begin
    Result := 'Translation has validation warnings:';

    for Warning in Translation.Warnings do
      Result := Result + #13 + '- ' + sTranslationValidationWarnings[Warning];
  end;
end;

// -----------------------------------------------------------------------------

const
  RemoteSession = False;

procedure TFormMain.SetSkin(const Value: string);
var
  SkinName, SkinFilename: string;
  SkinIndex: integer;
  OriginalSkinName: string;
  SkinLoaded: boolean;
  Retry: boolean;
  ColorSchemeName: string;
  RibbonSkin: TdxCustomRibbonSkin;
  SkinDetails: TdxSkinDetails;
  i: integer;
begin
  if (AnsiSameText(FSkin, Value)) then
    exit;

  if (TranslationManagerSettings.System.SafeMode) then
  begin
    // In safe mode we just store the skin value, but doesn't act on it. This enables the user to modify the skin
    // setting without any immediate consequences.
    FSkin := Value;
    exit;
  end;

  DecomposeSkinName(Value, SkinName, SkinFilename, SkinIndex);
  OriginalSkinName := SkinName;

  SkinLoaded := False;
  if (SkinFilename <> '') then
  begin
    if (TFile.Exists(SkinFilename)) then
    begin
      SkinLoaded := dxSkinsUserSkinLoadFromFile(SkinFilename, SkinName);
      if (not SkinLoaded) and (SkinIndex <> -1) then
        SkinLoaded := dxSkinsUserSkinLoadFromFileByIndex(SkinFilename, SkinIndex);
    end;
    if (not SkinLoaded) then
    begin
      SkinFilename := '';
      SkinIndex := -1;
    end else
      SkinName := 'UserSkin'; // Custom skin must be named "UserSkin"
  end;

  Retry := True;
  while (True) do
  begin

    // Verify that skin is valid
    // RibbonSkin := dxRibbonSkinsManager.Find(SkinName, dxRibbonMain.Style);
    // DevExpress 17.1.1
    RibbonSkin := dxRibbonSkinsManager.GetMostSituable(SkinName, RibbonMain.Style, Screen.PixelsPerInch);

    if (RibbonSkin <> nil) or (SkinLoaded) or (not Retry) then
      break;

    Retry := False;

    // Map old skins to new skins
    if (ContainsText(SkinName, 'Silver')) then
      SkinName := 'Office2013White'
    else
    if (ContainsText(SkinName, 'Blue')) then
      SkinName := 'Office2013LightGray'
    else
    if (ContainsText(SkinName, 'Black')) then
      SkinName := 'Office2013DarkGray'
    else
      break;

    OriginalSkinName := SkinName;
  end;

  // If value was invalid. Try default.
  if (RibbonSkin = nil) then
  begin
    SkinFilename := '';
    SkinIndex := -1;
    SkinName := sDefaultSkinName;
    OriginalSkinName := SkinName;

    if (not AnsiSameText(SkinName, Value)) then
      // RibbonSkin := dxRibbonSkinsManager.Find(SkinName, RibbonMain.Style);
      // DevExpress 17.1.1
      RibbonSkin := dxRibbonSkinsManager.GetMostSituable(SkinName, RibbonMain.Style, Screen.PixelsPerInch);
  end;

  // If value was invalid. Find first valid skin.
  if (RibbonSkin = nil) then
  begin
    SkinName := '';
    for i := 0 to dxRibbonSkinsManager.SkinCount-1 do
    begin
      RibbonSkin := dxRibbonSkinsManager.Skins[I];

      if (RibbonSkin.Style <> RibbonMain.Style) then
        continue;

      if not(RibbonSkin is TdxSkinRibbonPainter) then
        continue;

      if (TdxSkinRibbonPainter(RibbonSkin).Painter.IsInternalPainter) then
        continue;

      if (TdxSkinRibbonPainter(RibbonSkin).Painter.GetPainterDetails(SkinDetails)) then
      begin
        SkinName := SkinDetails.Name;
        break;
      end;
    end;
    OriginalSkinName := SkinName;
  end;


  RibbonMain.BeginUpdate;
  try
    RibbonMain.ColorSchemeAccent := TdxRibbonColorSchemeAccent(FColorSchemeAccent);

{
    if (SkinName = '') or (ContainsText(SkinName, 'Office')) then
    begin
      ColorSchemeName := 'Colorful';
      if (dxRibbonSkinsManager.Find(ColorSchemeName, RibbonMain.Style) = nil) then
        ColorSchemeName := SkinName;
    end else
}
      ColorSchemeName := SkinName;

    // Ribbon skin (ColorSchemeName specifies the skin/painter to use for the ribbon)
    RibbonMain.ColorSchemeName := ColorSchemeName;

    // "The rest" skin (i.e. not ribbon)
    if (SkinName <> '') then
    begin
      if (cxLookAndFeelPaintersManager.GetPainter(SkinName) = nil) then
      begin
        // Ribbon Color Schemes only skins the ribbon. Use a static skin for rest of application.
        if (RibbonMain.ColorScheme.Style = rs2013) then
          SkinName := 'Office2013White'
        else
        if (RibbonMain.ColorScheme.Style = rs2016) then
          SkinName := 'Office2016Colorful'
        else
          SkinName := sDefaultSkinName;
      end;

      FSkin := ComposeSkinName(OriginalSkinName, SkinFilename, SkinIndex);
    end else
    begin
      FSkin := '';
    end;

    SkinController.SkinName := SkinName;

    // Switch to Alternate skin mode (use Fills instead of BitBlts) if remote session
    if (RemoteSession) then
      SkinController.UseImageSet := imsAlternate;

    SkinController.UseSkins := (FSkin <> ''); // Causes splash to flicker if skinned

    // SkinPainter is nil if we tried to use a non-existing skin
    if (RootLookAndFeel.SkinPainter = nil) then
    begin
      FSkin := '';
      SkinController.UseImageSet := imsAlternate;
      SkinController.UseSkins := False;
    end;

  finally
    RibbonMain.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

function TFormMain.GetLanguageID(Value: LCID): LCID;
begin
  if (Value > 0) then
    Result := Value
  else
    Result := GetUserDefaultUILanguage;
end;

function TFormMain.GetSourceLanguageID: Word;
begin
  if (VarIsOrdinal(BarEditItemSourceLanguage.EditValue)) then
    Result := BarEditItemSourceLanguage.EditValue
  else
    Result := 0;
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
      FTargetLanguage := FProject.TargetLanguages.Add(TargetLanguageID)
    else
      FTargetLanguage := FProject.TargetLanguages.Add(SourceLanguageID);
  end;

  Result := FTargetLanguage;
end;

function TFormMain.GetTargetLanguageID: Word;
begin
  if (VarIsOrdinal(BarEditItemTargetLanguage.EditValue)) then
    Result := BarEditItemTargetLanguage.EditValue
  else
    Result := 0;
end;

procedure TFormMain.SetTargetLanguageID(const Value: Word);
begin
  // Always process setting the target. We need to have the dependents cleared and reloaded.
  // if (TargetLanguageID = Value) then
  //  Exit;

  ClearDependents;
  ClearTargetLanguage;

  BarEditItemTargetLanguage.EditValue := Value;

  Perform(MSG_TARGET_CHANGED, 0, 0);
end;

procedure TFormMain.ClearTargetLanguage;
begin
  FTargetLanguage := nil;
  FLocalizerDataSource.TargetLanguage := nil;
end;

procedure TFormMain.UpdateTargetLanguage;
begin
  FLocalizerDataSource.TargetLanguage := TargetLanguage;
  TreeListModules.FullRefresh;
  DisplayModuleStats;
end;

// -----------------------------------------------------------------------------

function TFormMain.CountStuff: TCounts;
var
  Counts: TCounts;
begin
  ZeroMemory(@Counts, SizeOf(Counts));

  FProject.Traverse(
    function(Module: TLocalizerModule): boolean
    begin
      Inc(Counts.CountModule);

      if (Module.IsUnused) then
        Inc(Counts.UnusedModule);

      Module.Traverse(
        function(Item: TLocalizerItem): boolean
        begin
          Inc(Counts.CountItem);

          if (Item.IsUnused) then
            Inc(Counts.UnusedItem);

          Item.Traverse(
            function(Prop: TLocalizerProperty): boolean
            var
              i: integer;
            begin
              Inc(Counts.CountProperty);

              if (Prop.IsUnused) then
                Inc(Counts.UnusedProperty);

              for i := 0 to Prop.Translations.Count-1 do
                case Prop.Translations[i].Status of
                  tStatusProposed,
                  tStatusTranslated:
                    Inc(Counts.Translated);
                  tStatusObsolete:
                    Inc(Counts.ObsoleteTranslation);
                end;

              Result := True;
            end, False);

          Result := True;
        end, False);

      Result := True;
    end, False);

  Result := Counts;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ClearDependents;
begin
  if (FSearchProvider <> nil) then
    FSearchProvider.Clear;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.InitializeProject(const SourceFilename: string; SourceLocaleID: Word);
begin
  ClearDependents;
  TreeListModules.Clear;
  ClearTargetLanguage;

  FProject.BeginUpdate;
  try

    FTranslationCounts.Clear;

    FProject.Clear;

    FProject.SourceFilename := SourceFilename;
    FProject.StringSymbolFilename := TPath.ChangeExtension(SourceFilename, '.drc');

    FProject.Name := TPath.GetFileNameWithoutExtension(SourceFilename);
    FProject.SourceLanguageID := SourceLocaleID;

    FProject.Modified := False;

  finally
    FProject.EndUpdate;
  end;

  UpdateProjectModifiedIndicator;

  RibbonMain.DocumentName := FProject.Name;

  SourceLanguageID := FProject.SourceLanguageID;
  TargetLanguageID := GetLanguageID(TranslationManagerSettings.System.DefaultTargetLanguage);
end;

// -----------------------------------------------------------------------------

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

  UpdateProjectModifiedIndicator;
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
    ReloadProperty(TLocalizerProperty(Item));
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
    Node.Values[TreeListColumnModuleStatus.ItemIndex] := Ord(Module.EffectiveStatus);

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

procedure TFormMain.OnProjectChanged(Sender: TObject);
begin
  UpdateProjectModifiedIndicator;
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

// -----------------------------------------------------------------------------

procedure TFormMain.SetInfoText(const Msg: string);
var
  s: string;
  Sep: integer;
begin
  s := Msg;

  Sep := Pos('|', s);

  // Remove ImageIndex from BallonHint s string
  if (Sep <> 0) then
    s := Copy(s, 1, Sep-1);

  StatusBar.Panels[StatusBarPanelHint].Text := s;
  StatusBar.Update;
end;

procedure TFormMain.ShowHint(Sender: TObject);
begin
  SetInfoText(Application.Hint);
end;

procedure TFormMain.DoShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
var
  s: string;
begin
  if (HintInfo.HintControl = StatusBar) and (FStatusBarPanel <> nil) then
  begin
    s := FStatusBarPanelHint[FStatusBarPanel.Index];
    if (s <> '') then
      HintStr := s;
  end;
end;

procedure TFormMain.StatusBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
(*
var
  Panel: TdxStatusBarPanel;
  p: TPoint;
*)
begin
(*
  if (Button <> mbRight) or  (ssDouble in Shift) then
    exit;

  Panel := StatusBar.GetPanelAt(X, Y);

  if (Panel = StatusBar.Panels[StatusBarPanelModified]) then
  begin
    p := StatusBar.ClientToScreen(Point(X, Y));
    PopupMenuModified.Popup(p.X, p.Y);
  end;
*)
end;

procedure TFormMain.StatusBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  // Determine which status bar panel the mouse is over so we can display a hint for the panel
  // See: DevPress ticket DS31900
  // http://www.devexpress.com/Support/Center/Question/Details/DS31900
  FStatusBarPanel := StatusBar.GetPanelAt(X, Y);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.UpdateProjectModifiedIndicator;
resourcestring
  sLocalizerStatusCount = 'Translate: %.0n, Ignore: %.0n, Hold: %.0n';
  sLocalizerProjectModified = 'Project has been modified since it was last saved';
  sLocalizerProjectBackupTimestamp = 'Last autosave: %s';
  sLocalizerProjectBackupTimestampModified = 'Last autosave: %s'+#13+'Project has been modified since last autosave.';
  sLocalizerProjectBackupNone = 'Project has not yet been autosaved.';
  sLocalizerProjectBackupAutoSaveDisabled = 'Autosave has been temporarily disabled.';
  sLocalizerProjectBackupAutoSaveDisabledProject = 'Autosave has been temporarily disabled for this project.';
  sLocalizerProjectBackupAutoSaveDisabledGlobal = 'Autosave is not enabled.';
var
//  s: string;
  ImageIndex: integer;
begin
  if (FProject.Modified) then
  begin
    FStatusBarPanelHint[StatusBarPanelModified] := sLocalizerProjectModified;
    ImageIndex := ImageIndexModified;

(*
    if (not FSettings.Backup.IntervalBackup) then
      FStatusBarPanelHint[StatusBarPanelModified] := sLocalizerProjectBackupAutoSaveDisabledGlobal
    else
    if (not FAutoSaveEnabled) then
      FStatusBarPanelHint[StatusBarPanelModified] := sLocalizerProjectBackupAutoSaveDisabled
    else
      FStatusBarPanelHint[cStatusBarPanelModified] := sLocalizerProjectBackupAutoSaveDisabledProject
    else
    if (FProject.BackupTimestamp <> 0) then
    begin
      if (FProject.NeedBackup) then
        s := sLocalizerProjectBackupTimestampModified
      else
      begin
        s := sLocalizerProjectBackupTimestamp;
        ImageIndex := ImageIndexFileSaveProtected;
      end;

      FStatusBarPanelHint[StatusBarPanelModified] := Format(s, [TimeToStr(FProject.BackupTimestamp)]);
    end else
      FStatusBarPanelHint[StatusBarPanelModified] := sLocalizerProjectBackupNone;

    if (psBackup in FProject.State) then
      ImageIndex := ImageIndexFileSaveWorking;
*)
  end else
  begin
    ImageIndex := ImageIndexNotModified;
    FStatusBarPanelHint[StatusBarPanelModified] := '';
  end;
  TdxStatusBarTextPanelStyle(StatusBar.Panels[StatusBarPanelModified].PanelStyle).ImageIndex := ImageIndex;

  StatusBar.Panels[StatusBarPanelStats].Text := Format(sLocalizerStatusCount,
    [1.0*FProject.StatusCount[ItemStatusTranslate], 1.0*FProject.StatusCount[ItemStatusDontTranslate], 1.0*FProject.StatusCount[ItemStatusHold]]);

  StatusBar.Update;
end;

// -----------------------------------------------------------------------------

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
      if (not Prop.IsUnused) and (Prop.EffectiveStatus = ItemStatusTranslate) and (Prop.HasTranslation(Language)) then
        Inc(Count);
      Result := True;
    end, False);

  Result := Count;

  // Update cache
  FTranslationCounts.AddOrSetValue(Module, Result);

  // Current module stats has updated - refresh display
  if (Module = FLocalizerDataSource.Module) then
    DisplayModuleStats;
end;

procedure TFormMain.InvalidateTranslatedCount(Module: TLocalizerModule);
begin
  if (FTranslationCounts.ContainsKey(Module)) then
    FTranslationCounts.AddOrSetValue(Module, -1);
end;

procedure TFormMain.InvalidateTranslatedCounts;
var
  i: integer;
begin
  for i := 0 to FTranslationCounts.Count-1 do
    FTranslationCounts.AddOrSetValue(FTranslationCounts.Keys.ToArray[i], -1);
end;

procedure TFormMain.RemoveTranslatedCount(Module: TLocalizerModule);
begin
  FTranslationCounts.Remove(Module);
end;

// -----------------------------------------------------------------------------

function TFormMain.GotoNext(Predicate: TLocalizerPropertyDelegate; FromStart: boolean): boolean;
var
  Module: TLocalizerModule;
  Prop: TLocalizerProperty;
  ModuleNode: TcxTreeListNode;
  PropNode: TcxTreeListNode;
begin
  SaveCursor(crHourGlass);

  Result := False;

  if (FromStart) then
    ModuleNode := nil
  else
    ModuleNode := TreeListModules.FocusedNode;

  if (ModuleNode <> nil) then
    PropNode := TreeListItems.FocusedNode
  else
  begin
    ModuleNode := TreeListModules.Root.GetFirstChildVisible;
    PropNode := nil;
  end;

  while (ModuleNode <> nil) do
  begin
    Module := TLocalizerModule(ModuleNode.Data);

    if (Module.Status = ItemStatusTranslate) then
    begin
      if (PropNode <> nil) then
      begin
        // Check next prop
        PropNode := PropNode.GetNextSiblingVisible;
      end else
      begin
        // Determine if module has any valid properties.
        // If so we load the property list and find the first node
        if (not Module.Traverse(
          function(Prop: TLocalizerProperty): boolean
          begin
            Result := not Predicate(Prop);
          end, False)) then
        begin
          // Select module node - this loads the property nodes
          ModuleNode.MakeVisible;
          ModuleNode.Focused := True;

          // Start with the top item
          PropNode := TreeListItems.Root.GetFirstChildVisible;
        end;
      end;

      while (PropNode <> nil) do
      begin
        Prop := TLocalizerProperty(TreeListItems.HandleFromNode(PropNode));

        if (Predicate(Prop)) then
        begin
          // Select property tree node
          PropNode.MakeVisible;
          PropNode.Focused := True;

          TreeListItems.SetFocus;

          Exit(True);
        end;

        // Check next prop
        PropNode := PropNode.GetNextSiblingVisible;
      end;
    end;

    ModuleNode := ModuleNode.GetNextSiblingVisible;
    PropNode := nil;
  end;

  // The following finds the next untranslated in physical order.
  // Since this probably differ from the logical order due to the trees being sorted,
  // it makes the search order appear random to the user.
(*
  CurrentModule := FocusedModule;
  CurrentProp := FocusedProperty;

  Found := False;

  FProject.Traverse(
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

      if (Prop.EffectiveStatus <> ItemStatusTranslate) or (Prop.State = ItemStateUnused) then
        Exit(True);

      if (not Prop.Translations.TryGetTranslation(TargetLanguage, Translation)) or (Translation.Status = tStatusPending) then
      begin
        // Found - find treelist nodes
        // Select module tree node
        ModuleNode := TreeListModules.Find(Prop.Item.Module, TreeListModules.Root, False, True, TreeListFindFilter);
        if (ModuleNode = nil) or (ModuleNode.IsHidden) then
          Exit(True); // ModuleNode might have been hidden by a filter

        // Select module node - this loads the property nodes
        ModuleNode.MakeVisible;
        ModuleNode.Focused := True;

        // Select property tree node
        PropNode := TreeListItems.NodeFromHandle(Prop);
        if (PropNode = nil) or (PropNode.IsHidden) then
          Exit(True); // PropNode might have been hidden by a filter

        PropNode.MakeVisible;
        PropNode.Focused := True;

        TreeListItems.SetFocus;

        // Done
        Found := True;
        Exit(False);
      end;

      Result := True;
    end, False);

  if (not Found) then
*)
end;

// -----------------------------------------------------------------------------

procedure TFormMain.SpellCheckerCheckAsYouTypeStart(Sender: TdxCustomSpellChecker; AControl: TWinControl; var AAllow: Boolean);
begin
  // Only spell check inplace editor and memo in text dialog
  AAllow := ((AControl.Parent = TreeListItems) and (AControl is TcxCustomButtonEdit)) or (AControl is TcxCustomMemo);
end;

procedure TFormMain.SpellCheckerCheckWord(Sender: TdxCustomSpellChecker; const AWord: WideString; out AValid: Boolean; var AHandled: Boolean);
var
  SanitizedWord: string;
  i: integer;
  HasLetters: boolean;
begin
  if (FSpellCheckingWord) then
    Exit;

  if (FSpellCheckingString) then
    SanitizedWord := AWord
  else
    SanitizedWord := SanitizeText(AWord);

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
    ViewProperty(FSpellCheckProp);
end;

procedure TFormMain.SpellCheckerSpellingComplete(Sender: TdxCustomSpellChecker; var AHandled: Boolean);
begin
  // Supress "Check complete" message
  AHandled := True;
end;

function TFormMain.PerformSpellCheck(Prop: TLocalizerProperty): boolean;
var
  Translation: TLocalizerTranslation;
  Text, CheckedText: string;
begin
  if (Prop.IsUnused) then
    Exit(True);

  if (Prop.EffectiveStatus <> ItemStatusTranslate) then
    Exit(True);

  Translation := Prop.Translations.FindTranslation(TargetLanguage);

  // Do not check values that have not been translated
  if (Translation = nil) or (not Translation.IsTranslated) then
    Exit(True);

  Text := Translation.Value;

  CheckedText := SanitizeText(Text);

  // Remove file filters
  if (Pos('|', CheckedText) > 0) then
    CheckedText := TRegEx.Replace(CheckedText, '\(?\*\.[a-zA-Z*]+\)?[,;]?\|?', '', []);

  (*
  i := 0;
  while (True) do
  begin
    i := FindDelimiter('|*()\', CheckedText, i+1);
    if (i = 0) then
      break;
    CheckedText[i] := ' ';
  end;
  *)

  CheckedText := CheckedText.Trim;
  if (CheckedText.IsEmpty) then
    Exit(True);

  // Display spell check dialog
  FSpellCheckProp := Prop;
  try

    // Perform pre check on sanitized string.
    // FSpellCheckingStringResult will indicate if string has errors.
    FSpellCheckingStringResult := True;
    FSpellCheckingString := True;
    try

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

// -----------------------------------------------------------------------------

type
  TCustomdxBarControlCracker = class(TCustomdxBarControl);
  TdxBarItemCracker = class(TdxBarItem);

procedure TFormMain.PopupMenuBookmarkPopup(Sender: TObject);
var
  BarControl: TCustomdxBarControl;
  BarItemLink: TdxBarItemLink;
  i, j: integer;
  Flag: TPropertyFlag;
  AllSet: boolean;
  Prop: TLocalizerProperty;
  Props: TArray<TLocalizerProperty>;
begin
  // If menu is dropped by a click on a dropdown button, we can determine the kind of action goto/set bookmark here.
  // Otherwise it is determined on the action that performs the drop (ActionEditMarkExecute & ActionGotoBookmarkExecute).
  BarControl := ActiveBarControl;
  if (BarControl <> nil) then
  begin
    BarItemLink := TCustomdxBarControlCracker(BarControl).SelectedLink;
    if (BarItemLink <> nil) then
    begin
      ActionGotoBookmarkAny.Visible := (BarItemLink.Item = ButtonGotoBookmark);
      ActionGotoBookmarkAny.Enabled := ActionGotoBookmarkAny.Visible;
    end;
  end;

  // Work around for "Any bookmark" item losing shortcut
  if (ActionGotoBookmarkAny.Visible) then
    ButtonItemBookmarkAny.Caption := ActionGotoBookmarkAny.Caption;

  for i := 0 to PopupMenuBookmark.ItemLinks.Count-1 do
    if (PopupMenuBookmark.ItemLinks[i].Item.Action <> nil) and (PopupMenuBookmark.ItemLinks[i].Item.Action.Tag = FLastBookmark) then
    begin
      TCustomdxBarControlCracker(PopupMenuBookmark.SubMenuControl).SelectedLink := PopupMenuBookmark.ItemLinks[i];
      break;
    end;

  for i := 0 to PopupMenuBookmark.ItemLinks.Count-1 do
    if (PopupMenuBookmark.ItemLinks[i].Item.Action <> nil) then
    begin
      Flag := TPropertyFlag(TAction(PopupMenuBookmark.ItemLinks[i].Item.Action).Tag);

      // Preselect last used bookmark
      if (Ord(Flag) = FLastBookmark) then
        TCustomdxBarControlCracker(PopupMenuBookmark.SubMenuControl).SelectedLink := PopupMenuBookmark.ItemLinks[i];

      // Indicate the current bookmark state of the selected items if we are setting a bookmark.
      SetLength(Props, TreeListItems.SelectionCount);
      for j := 0 to TreeListItems.SelectionCount-1 do
        Props[j] := TLocalizerProperty(TreeListItems.HandleFromNode(TreeListItems.Selections[j]));
      AllSet := True;
      for Prop in Props do
      begin
        if (not (Flag in Prop.Flags)) then
        begin
          AllSet := False;
          break;
        end;
      end;

      TAction(PopupMenuBookmark.ItemLinks[i].Item.Action).Checked := (not ActionGotoBookmarkAny.Visible) and (AllSet);
      // We need to sync button with action manually in order to work around for button always behaving like AutoCheck=True (http://www.devexpress.com/Support/Center/Question/Details/Q488436)
      TdxBarButton(PopupMenuBookmark.ItemLinks[i].Item).Down := TAction(PopupMenuBookmark.ItemLinks[i].Item.Action).Checked;
    end;
end;

function TFormMain.QueueRestart(Immediately: boolean): boolean;
resourcestring
  sApplicationRestartPrompt = 'The application must be restarted in order to complete the operation.'+#13#13+'Do you want to restart the application now?';
begin
  Result := False;

  if (not Immediately) then
  begin
    PostMessage(Handle, MSG_RESTART, 0, 0);
    Exit;
  end;

  if (MessageDlg(sApplicationRestartPrompt, mtConfirmation, [mbYes, mbNo], -1, mbNo) <> mrYes) then
    exit;

  // Prompt to save changed files etc.
  if (CheckSave) then
  begin
    SaveCursor(crHourGlass);

    // Grab restart semaphore.
    // Nobody should be holding the semaphore at this point, so no need to wait very long.
    while (not AcquireRestartSemaphore(5000)) do
    begin
      if (TaskMessageDlg('Failed to prepare for restart', 'A previous instance of the application might have failed to terminate.'+#13#13+
        'You can use Task Manager to terminate it.', mtWarning, [mbAbort, mbRetry], 0, mbRetry) <> mrRetry) then
        Exit(False);
    end;

    // Launch new instance of application
    if (not Shell.Execute(Application.ExeName, FProjectFilename, Self)) then
      ReleaseRestartSemaphore;

    // Force close of all projects (we have already prompted to save them) to avoid user being prompted again.
    FProject.Modified := False;

    // Invoke normal logic to close application
    Close;
    //ActionExit.Execute;

    Result := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ViewProperty(Prop: TLocalizerProperty);
var
  Node: TcxTreeListNode;
begin
  // Find and select module
  Node := TreeListModules.Find(Prop.Item.Module, nil, False, True, TreeListFindFilter);

  if (Node <> nil) then
  begin
    Node.MakeVisible;
    Node.Focused := True;

    // Find and select property node
    Node := TreeListItems.NodeFromHandle(Prop);

    if (Node <> nil) then
    begin
      Node.MakeVisible;
      Node.Focused := True;
      TreeListItems.SetFocus;
    end;
  end;
end;

// -----------------------------------------------------------------------------

type
  TcxTreeListNodeCracker = class(TcxTreeListNode);

procedure TFormMain.ReloadNode(Node: TcxTreeListNode);
begin
  // Hack to reload a single tree node
  Exclude(TcxTreeListNodeCracker(Node).State, nsValuesAssigned);
  Node.TreeList.LayoutChanged;
end;

procedure TFormMain.ReloadProperty(Prop: TLocalizerProperty);
var
  Node: TcxTreeListNode;
begin
  if (Prop.Item.Module <> FocusedModule) then
    Exit;
  Node := TreeListItems.NodeFromHandle(Prop);
  if (Node <> nil) then
    ReloadNode(Node);
end;

// -----------------------------------------------------------------------------

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

procedure TFormMain.TreeListItemsClick(Sender: TObject);
var
  Msg: string;
begin
  if (not TreeListItems.HitTest.HitAtStateImage) then
    Exit;

  Msg := GetNodeValidationMessage(TreeListItems.HitTest.HitNode);
  if (Msg <> '') then
    MessageDlg(Msg, mtWarning, [mbOK], 0);
end;

procedure TFormMain.TreeListItemsCustomDrawIndicatorCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas;
  AViewInfo: TcxTreeListIndicatorCellViewInfo; var ADone: Boolean);
var
  r: TRect;
  Prop: TLocalizerProperty;
  Flag: TPropertyFlag;
  ImageIndex: integer;
  s: string;
const
  OffsetNumeric = 10; // Note: Each node can have only one numeric bookmark ATM
  OffsetFlag = 8;
begin
  if (AViewInfo.Node = nil) then
    Exit;

  if (AViewInfo.Node.Focused) or (AViewInfo.Node.Selected) then
    ACanvas.FillRect(AViewInfo.BoundsRect, StyleSelected.Color)
  else
    ACanvas.FillRect(AViewInfo.BoundsRect, AViewInfo.ViewParams.Color);

  AViewInfo.Painter.DrawHeaderBorder(ACanvas, AViewInfo.BoundsRect, [], []);

  s := string.Create('9', Trunc(Log10(AViewInfo.Node.Parent.Count)+1));
  r := AViewInfo.BoundsRect;
  r.Right := r.Left+ACanvas.TextWidth(s)+4;
  ACanvas.Font.Color := AViewInfo.ViewParams.TextColor;
  ACanvas.Brush.Style := bsClear;
  s := IntToStr(AViewInfo.Node.Index + 1);
  ACanvas.DrawTexT(s, r, TAlignment.taRightJustify, TcxAlignmentVert.vaCenter, False, False);

  Prop := TLocalizerProperty(TreeListItems.HandleFromNode(AViewInfo.Node));

  if (Prop <> nil) then
  begin

    r.Top := (AViewInfo.BoundsRect.Top + AViewInfo.BoundsRect.Bottom - ImageListSmall.Height) div 2;
    r.Left := AViewInfo.BoundsRect.Right - ImageListSmall.Width;

    for Flag := FlagBookmark9 downto FlagBookmark0 do
    begin
      // Draw indicator for numeric bookmarks
      if (r.Left < AViewInfo.BoundsRect.Left) then
        break;

      if (Flag in Prop.Flags) then
      begin
        ImageIndex := ImageIndexBookmark0 + Ord(Flag)-Ord(FlagBookmark0);

        ACanvas.DrawImage(ImageListSmall, r.Left, r.Top, ImageIndex);

        Dec(r.Left, OffsetNumeric);
      end;
    end;

    for Flag := FlagBookmarkA to FlagBookmarkF do
    begin
      // Draw indicator for flag bookmarks
      if (r.Left < AViewInfo.BoundsRect.Left) then
        break;

      if (Flag in Prop.Flags) then
      begin
        ImageIndex := ImageIndexBookmarkA + Ord(Flag)-Ord(FlagBookmarkA);

        ACanvas.DrawImage(ImageListSmall, r.Left, r.Top, ImageIndex);

        Dec(r.Left, OffsetFlag);
      end;
    end;

  end;

  ADone := True;
end;

procedure TFormMain.TreeListItemsEditing(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; var Allow: Boolean);
begin
  // Only allow inline editing of property nodes
  Allow := (FocusedProperty <> nil);
end;

procedure TFormMain.TreeListItemsEditValueChanged(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
var
  Translation: TLocalizerTranslation;
begin
  if (AColumn = TreeListColumnTarget) then
  begin
    if (FUpdateLockCount > 0) then
      exit;

    LockUpdates;
    try

      Translation := FocusedProperty.Translations.AddOrUpdateTranslation(TargetLanguage, VarToStr(Sender.InplaceEditor.EditValue));
      Translation.UpdateWarnings;

      LoadFocusedPropertyNode;
    finally
      UnlockUpdates;
    end;
  end;
end;

procedure TFormMain.TreeListItemsGetCellHint(Sender: TcxCustomTreeList; ACell: TObject; var AText: string; var ANeedShow: Boolean);
begin
  if (not (ACell is TcxTreeListIndentCellViewInfo)) or (TcxTreeListIndentCellViewInfo(ACell).Kind <> nikState) then
    Exit;

  AText := GetNodeValidationMessage(TcxTreeListIndentCellViewInfo(ACell).Node);
  ANeedShow := (AText <> '');
end;

procedure TFormMain.TreeListItemsGetNodeImageIndex(Sender: TcxCustomTreeList; ANode: TcxTreeListNode;
  AIndexType: TcxTreeListImageIndexType; var AIndex: TImageIndex);
var
  Prop: TLocalizerProperty;
  Translation: TLocalizerTranslation;
begin
  AIndex := -1;

  if (not (AIndexType in [tlitImageIndex, tlitSelectedIndex, tlitStateIndex])) then
    Exit;

  Prop := TLocalizerProperty(TcxVirtualTreeList(Sender).HandleFromNode(ANode));
  Assert(Prop <> nil);

  if (not Prop.Translations.TryGetTranslation(TargetLanguage, Translation)) then
    Translation := nil;

  if (AIndexType = tlitStateIndex) then
  begin
    if (Translation <> nil) and (Translation.Warnings <> []) then
      AIndex := 0;

    Exit;
  end;

  // Note: Image indicates effective status

  if (Prop.IsUnused) then
    AIndex := 1
  else
  if (ItemStateNew in Prop.State) and (Prop.EffectiveStatus = ItemStatusTranslate) and (Translation = nil) then
    AIndex := 0
  else
  if (Prop.EffectiveStatus = ItemStatusDontTranslate) then
    AIndex := 2
  else
  if (Prop.EffectiveStatus = ItemStatusHold) then
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

procedure TFormMain.TreeListItemsStylesGetContentStyle(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; var AStyle: TcxStyle);
var
  Prop: TLocalizerProperty;
  Translation: TLocalizerTranslation;
begin
  if (ANode.Selected) and ((AColumn = nil) or (not AColumn.Focused) or ((AColumn.Focused) and (not AColumn.TreeList.Focused))) then
  begin
    AStyle := StyleSelected;
    Exit;
  end else
  if (ANode.Focused) and (AColumn <> nil) and (AColumn.Focused) and (AColumn.TreeList.Focused) and (not AColumn.Editing) then
  begin
    AStyle := StyleFocused;
    Exit;
  end;

  Prop := TLocalizerProperty((Sender as TcxVirtualTreeList).HandleFromNode(ANode));

  if (Prop.IsUnused) or (Prop.EffectiveStatus = ItemStatusDontTranslate) then
  begin
    AStyle := StyleDontTranslate;
    Exit;
  end;

  if (Prop.EffectiveStatus = ItemStatusHold) then
  begin
    AStyle := StyleHold;
    Exit;
  end;

  if (not Prop.Translations.TryGetTranslation(TargetLanguage, Translation)) then
    Translation := nil;

  if (Translation <> nil) and (Translation.IsTranslated) then
    AStyle := StyleComplete
  else
    AStyle := StyleNeedTranslation;
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

    if (TreeListModules.SelectionCount = 1) then
      FLocalizerDataSource.Module := FocusedModule
    else
      // Clear item treelist if more than one module is selected
      FLocalizerDataSource.Module := nil;

    // Hide property name column if module is resourcestrings
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
  begin
    TreeListItems.TopNode.MakeVisible;
    TreeListItems.TopNode.Focused := True;
  end;

  DisplayModuleStats;
end;

procedure TFormMain.DisplayModuleStats;
var
  TranslatedCount: integer;
  TranslatableCount: integer;
  PendingCount: integer;
begin
  if (FLocalizerDataSource.Module <> nil) then
  begin
    TranslatedCount := GetTranslatedCount(FLocalizerDataSource.Module);
    TranslatableCount := FLocalizerDataSource.Module.StatusCount[ItemStatusTranslate];
  end else
  begin
    TranslatedCount := 0;
    TranslatableCount := 0;
  end;
  PendingCount := TranslatableCount - TranslatedCount;

  LabelCountTranslated.Caption := Format('%.0n', [1.0 * TranslatedCount]);
  LabelCountPending.Caption := Format('%.0n', [1.0 * PendingCount]);
  if (TranslatedCount <> 0) and (TranslatableCount <> 0) then
    LabelCountTranslatedPercent.Caption := Format('(%.1n%%)', [TranslatedCount/TranslatableCount*100])
  else
    LabelCountTranslatedPercent.Caption := '';
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

  if (Module.IsUnused) then
    AIndex := 1
  else
  if (Module.EffectiveStatus = ItemStatusTranslate) then
  begin
    TranslatedCount := GetTranslatedCount(Module);
    // Calculate completeness in %
    if (Module.StatusCount[ItemStatusTranslate] <> 0) then
      Completeness := MulDiv(TranslatedCount, 100, Module.StatusCount[ItemStatusTranslate])
    else
      Completeness := 100;

    if (Completeness = 100) then        // 100% complete
      AIndex := 4
    else
    if (Completeness >= 66) then        // 66%..99% complete
      AIndex := 10
    else
    if (Completeness >= 33) then        // 33%..65% complete
      AIndex := 9
    else
    if (TranslatedCount > 0) then       // >0%..32% complete
      AIndex := 8
    else
    if (ItemStateNew in Module.State) then
      AIndex := 0
    else
      AIndex := 6;
  end else
  if (Module.EffectiveStatus = ItemStatusDontTranslate) then
    AIndex := 2
  else
  if (Module.EffectiveStatus = ItemStatusHold) then
    AIndex := 5
  else
    AIndex := -1;
end;

procedure TFormMain.TreeListModulesStylesGetContentStyle(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; var AStyle: TcxStyle);
var
  Module: TLocalizerModule;
begin
  if (ANode.Selected) and ((AColumn = nil) or (not AColumn.Focused) or ((AColumn.Focused) and (not AColumn.TreeList.Focused))) then
  begin
    AStyle := StyleSelected;
    Exit;
  end else
  if (ANode.Focused) and (AColumn <> nil) and (AColumn.Focused) and (AColumn.TreeList.Focused) and (not AColumn.Editing) then
  begin
    AStyle := StyleFocused;
    Exit;
  end;

  Module := TLocalizerModule(ANode.Data);

  if (Module.IsUnused) or (Module.EffectiveStatus = ItemStatusDontTranslate) then
  begin
    AStyle := StyleDontTranslate;
    Exit;
  end;

  if (Module.EffectiveStatus = ItemStatusHold) then
  begin
    AStyle := StyleHold;
    Exit;
  end;

  if (GetTranslatedCount(Module) = Module.StatusCount[ItemStatusTranslate]) then
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
  Item: TLocalizerItem;
begin
  Result := 0;

  if (FModule = nil) then
    Exit;

  // We can not just sum the status counts because these exclude items that are in the Unused state
  // Result := FModule.StatusCount[ItemStatusTranslate]+FModule.StatusCount[ItemStatusHold]+FModule.StatusCount[ItemStatusDontTranslate];

  for Item in FModule.Items.Values.ToArray do
    Inc(Result, Item.Properties.Count);
end;

function TLocalizerDataSource.GetRecordHandle(ARecordIndex: Integer): TcxDataRecordHandle;
var
  Item: TLocalizerItem;
begin
  Result := nil;

  if (FModule = nil) then
    Exit;

  for Item in FModule.Items.Values.ToArray do
  begin
    if (ARecordIndex > Item.Properties.Count-1) then
    begin
      Dec(ARecordIndex, Item.Properties.Count);
      continue;
    end;

    Exit(Item.Properties.Values.ToArray[ARecordIndex]);
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
  TreeItemIndexEffectiveStatus  = 5;
  TreeItemIndexState            = 6;
  TreeItemIndexSourceValue      = 7;
  TreeItemIndexTargetValue      = 8;
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

    TreeItemIndexEffectiveStatus:
      Result := Ord(Prop.EffectiveStatus);

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
