﻿unit amLocalization.Dialog.Main;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

{$WARN SYMBOL_PLATFORM OFF}

uses
  Generics.Collections,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Forms, Vcl.Controls, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, System.Actions,
  Vcl.ActnList, System.ImageList, Vcl.ImgList, UITypes, Data.DB, Vcl.Menus,
  SyncObjs,

  dxRibbonForm,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxRibbonSkins, dxSkinsCore,
  dxRibbonCustomizationForm, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator, dxDateRanges,
  cxDBData, cxGridLevel, cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, dxBar, dxSkinsForm,
  cxClasses, dxStatusBar, dxRibbonStatusBar, dxRibbon, cxTL, cxTLdxBarBuiltInMenu, cxInplaceContainer, cxLabel, cxMemo,
  cxImageComboBox, cxSplitter, cxContainer, cxTreeView, cxTextEdit, cxBlobEdit, cxImageList, cxDBExtLookupComboBox, cxMaskEdit,
  cxDropDownEdit, cxLookupEdit, cxDBLookupEdit, cxBarEditItem, cxDataControllerConditionalFormattingRulesManagerDialog, cxButtonEdit,
  dxSpellCheckerCore, dxSpellChecker, cxTLData,
  dxLayoutcxEditAdapters, dxLayoutLookAndFeels, dxLayoutContainer, dxLayoutControl, dxOfficeSearchBox, dxScreenTip, dxCustomHint, cxHint,
  dxGallery, dxRibbonGallery, dxRibbonMiniToolbar, cxRichEdit, cxButtons,

  amLocale,
  amProgress.API,
  amLocalization.Model,
  amLocalization.Provider,
  amLocalization.Dialog.Search,
  amLocalization.Integration.Tracker,
  amLocalization.TranslationMemory,
  amLocalization.StopList,
  amLocalization.Index,
  amLocalization.Settings,
  amLocalization.Data.ModuleItemsDataSource,
  amLocalization.ExceptionHandler.API, dxCore, dxScrollbarAnnotations;


const
  MSG_SOURCE_CHANGED = WM_USER;
  MSG_TARGET_CHANGED = WM_USER+1;
  MSG_FORM_MAXIMIZE = WM_USER+2;
  MSG_RESTART = WM_USER+3;
  MSG_FILE_OPEN = WM_USER+4;
  MSG_AFTER_SHOW = WM_USER+5;
  MSG_REFRESH_MODULE_STATS = WM_USER+6;


// -----------------------------------------------------------------------------
//
// TcxGridTableView redirect
//
// -----------------------------------------------------------------------------
// Modified to display crHandPoint cursor when mouse is over a hint marker
// -----------------------------------------------------------------------------
type
  TcxGridTableView = class(cxGridTableView.TcxGridTableView)
  protected
    function GetControllerClass: TcxCustomGridControllerClass; override;
  end;



// -----------------------------------------------------------------------------
//
// TFormMain
//
// -----------------------------------------------------------------------------
type
  TFormMain = class(TdxRibbonForm, ILocalizerSearchHost, IExceptionInfoProvider)
    OpenDialogXLIFF: TOpenDialog;
    BarManager: TdxBarManager;
    RibbonMain: TdxRibbon;
    RibbonTabMain: TdxRibbonTab;
    StatusBar: TdxRibbonStatusBar;
    SkinController: TdxSkinController;
    BarManagerBarFile: TdxBar;
    BarButtonOpenProject: TdxBarLargeButton;
    BarButtonNewProject: TdxBarLargeButton;
    BarButtonSaveProject: TdxBarButton;
    BarManagerBarProject: TdxBar;
    BarButtonUpdateProject: TdxBarLargeButton;
    BarButtonBuildProject: TdxBarLargeButton;
    BarManagerBarMigrate: TdxBar;
    dxBarButton2: TdxBarButton;
    RibbonTabTranslation: TdxRibbonTab;
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
    BarButtonPurgeProject: TdxBarButton;
    ActionProjectPurge: TAction;
    BarManagetBarTranslationStatus: TdxBar;
    BarButtonStatusTranslate: TdxBarButton;
    BarButtonStatusDontTranslate: TdxBarButton;
    BarButtonStatusHold: TdxBarButton;
    BarManagetBarTranslationState: TdxBar;
    BarButtonStatePropose: TdxBarButton;
    BarButtonStateAccept: TdxBarButton;
    BarButtonStateReject: TdxBarButton;
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
    PopupMenuTree: TdxRibbonPopupMenu;
    SplitterTreeLists: TcxSplitter;
    TreeListModules: TcxTreeList;
    TreeListColumnModuleName: TcxTreeListColumn;
    TreeListColumnModuleStatus: TcxTreeListColumn;
    BarManagerBarMachineTranslation: TdxBar;
    BarButtonAutoTranslate: TdxBarLargeButton;
    BarButtonTM: TdxBarButton;
    BarButtonGotoNext: TdxBarSubItem;
    ActionMain: TAction;
    ActionGotoNextUntranslated: TAction;
    OpenDialogEXE: TOpenDialog;
    ActionImportFile: TAction;
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
    BarButtonTMAdd: TdxBarButton;
    BarButtonTMLookup: TdxBarButton;
    PanelModules: TPanel;
    LayoutControlModulesGroup_Root: TdxLayoutGroup;
    LayoutControlModules: TdxLayoutControl;
    dxLayoutItem2: TdxLayoutItem;
    LabelCountTranslated: TcxLabel;
    dxLayoutItem1: TdxLayoutItem;
    LabelCountPending: TcxLabel;
    ActionAutomationTranslate: TAction;
    ActionTranslationMemory: TAction;
    ActionTranslationMemoryAdd: TAction;
    ActionTranslationMemoryTranslate: TAction;
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
    dxLayoutItem3: TdxLayoutItem;
    LabelCountTranslatedPercent: TcxLabel;
    LayoutGroupStatsPercent: TdxLayoutGroup;
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
    BarEditItemSearch: TcxBarEditItem;
    TimerHint: TTimer;
    HintStyleController: TcxHintStyleController;
    ScreenTipRepository: TdxScreenTipRepository;
    ScreenTipTranslationMemory: TdxScreenTip;
    dxBarButton35: TdxBarButton;
    ActionAbout: TAction;
    TaskDialogTranslate: TTaskDialog;
    PopupMenuTranslateProviders: TdxRibbonPopupMenu;
    ActionImportCSV: TAction;
    TimerToast: TTimer;
    BarManagerBarStopList: TdxBar;
    BarButtonStopList: TdxBarButton;
    ActionStopList: TAction;
    ActionStopListAdd: TAction;
    ActionStopListAddModule: TAction;
    ActionStopListAddElement: TAction;
    ActionStopListAddType: TAction;
    ActionStopListAddName: TAction;
    ActionStopListAddValue: TAction;
    ActionStopListApply: TAction;
    dxBarButton7: TdxBarButton;
    ActionStopListAddTypeAndName: TAction;
    RibbonGalleryItemStopList: TdxRibbonGalleryItem;
    RibbonGalleryItemGroup: TdxRibbonGalleryGroup;
    dxRibbonGalleryItem1Group1Item1: TdxRibbonGalleryGroupItem;
    dxRibbonGalleryItem1Group1Item2: TdxRibbonGalleryGroupItem;
    dxRibbonGalleryItem1Group1Item3: TdxRibbonGalleryGroupItem;
    dxRibbonGalleryItem1Group1Item4: TdxRibbonGalleryGroupItem;
    dxRibbonGalleryItem1Group1Item5: TdxRibbonGalleryGroupItem;
    dxRibbonGalleryItem1Group1Item6: TdxRibbonGalleryGroupItem;
    PopupMenuBuild: TdxRibbonPopupMenu;
    ButtonBuildAll: TdxBarButton;
    ButtonSeparatorBuild: TdxBarSeparator;
    ActionEditTranslationText: TAction;
    ActionTranslationSuggestionList: TAction;
    ActionProjectRecover: TAction;
    dxBarButton1: TdxBarButton;
    ActionClearBookmarks: TAction;
    dxBarButton3: TdxBarButton;
    ActionTranslationMemoryLocate: TAction;
    dxBarButton4: TdxBarButton;
    ActionExportCSV: TAction;
    ActionExportExcel: TAction;
    ActionImportPO: TAction;
    dxBarButton5: TdxBarButton;
    TaskDialogImportUpdate: TTaskDialog;
    ActionValidationWarningDismiss: TAction;
    dxBarButton6: TdxBarButton;
    ActionValidationWarningResolve: TAction;
    dxBarButton8: TdxBarButton;
    GridItemsLevel: TcxGridLevel;
    GridItems: TcxGrid;
    GridItemsTableView: TcxGridTableView;
    GridItemsTableViewColumnItemName: TcxGridColumn;
    GridItemsTableViewColumnType: TcxGridColumn;
    GridItemsTableViewColumnValueName: TcxGridColumn;
    GridItemsTableViewColumnID: TcxGridColumn;
    GridItemsTableViewColumnStatus: TcxGridColumn;
    GridItemsTableViewColumnEffectiveStatus: TcxGridColumn;
    GridItemsTableViewColumnState: TcxGridColumn;
    GridItemsTableViewColumnSource: TcxGridColumn;
    GridItemsTableViewColumnTarget: TcxGridColumn;
    PanelMain: TPanel;
    PanelEditors: TPanel;
    SplitterEditors: TcxSplitter;
    PanelSource: TPanel;
    EditSourceText: TcxRichEdit;
    LabelSourceName: TcxLabel;
    PanelText: TPanel;
    EditTargetText: TcxRichEdit;
    LabelTargetName: TcxLabel;
    SplitterMainEditors: TcxSplitter;
    PanelTextEditButtons: TPanel;
    ButtonTextEditApply: TcxButton;
    ButtonTextEditCancel: TcxButton;
    ButtonTextEditPrev: TcxButton;
    ButtonTextEditNext: TcxButton;
    ActionTextEditApply: TAction;
    ActionTextEditCancel: TAction;
    ActionTextEditPrevious: TAction;
    ActionTextEditNext: TAction;
    dxLayoutItem4: TdxLayoutItem;
    LabelCountModuleTranslated: TcxLabel;
    dxLayoutItem5: TdxLayoutItem;
    LabelCountModuleTranslatedPercent: TcxLabel;
    dxLayoutItem6: TdxLayoutItem;
    LabelCountModulePending: TcxLabel;
    LayoutGroupStatsTranslated: TdxLayoutGroup;
    LayoutGroupStatsPending: TdxLayoutGroup;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem;
    RibbonGalleryItemGroupItem7: TdxRibbonGalleryGroupItem;
    ActionStopListAddTypeNameValue: TAction;
    PopupMenuValidationWarning: TdxRibbonPopupMenu;
    BarSeparatorValidationWarningHeader: TdxBarSeparator;
    dxBarSeparator2: TdxBarSeparator;
    dxBarButton15: TdxBarButton;
    ActionGotoAgain: TAction;
    ButtonTranslationMemoryUpdate: TdxBarButton;
    ActionTranslationMemoryUpdate: TAction;
    ActionProjectLocateSource: TAction;
    ButtonProjectLocateSource: TdxBarButton;
    BarManagerBarIntegration: TdxBar;
    dxBarButton23: TdxBarButton;
    ActionIntegrationTracking: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
    procedure ActionProofingLiveCheckExecute(Sender: TObject);
    procedure ActionProofingLiveCheckUpdate(Sender: TObject);
    procedure ActionProofingCheckExecute(Sender: TObject);
    procedure SpellCheckerCheckWord(Sender: TdxCustomSpellChecker; const AWord: WideString; out AValid: Boolean; var AHandled: Boolean);
    procedure SpellCheckerSpellingComplete(Sender: TdxCustomSpellChecker; var AHandled: Boolean);
    procedure ActionProofingCheckSelectedExecute(Sender: TObject);
    procedure ActionHasItemFocusedUpdate(Sender: TObject);
    procedure BarManagerBarProofingCaptionButtons0Click(Sender: TObject);
    procedure ActionFindSearchExecute(Sender: TObject);
    procedure ActionProjectSaveUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ActionHasProjectUpdate(Sender: TObject);
    procedure ActionHasModulesUpdate(Sender: TObject);
    procedure TreeListColumnModuleStatusPropertiesEditValueChanged(Sender: TObject);
    procedure BarManagerBarLanguageCaptionButtons0Click(Sender: TObject);
    procedure BarEditItemTargetLanguagePropertiesInitPopup(Sender: TObject);
    procedure ActionMainExecute(Sender: TObject);
    procedure ActionMainUpdate(Sender: TObject);
    procedure ActionImportFileExecute(Sender: TObject);
    procedure ActionGotoNextUntranslatedExecute(Sender: TObject);
    procedure TreeListModulesStylesGetContentStyle(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; var AStyle: TcxStyle);
    procedure SpellCheckerCheckStart(Sender: TdxCustomSpellChecker; AControl: TWinControl; var AAllow: Boolean);
    procedure ActionImportFileSourceExecute(Sender: TObject);
    procedure ActionImportFileTargetExecute(Sender: TObject);
    procedure TreeListModulesGetNodeImageIndex(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AIndexType: TcxTreeListImageIndexType; var AIndex: TImageIndex);
    procedure ActionAutomationTranslateExecute(Sender: TObject);
    procedure ActionAutomationTranslateUpdate(Sender: TObject);
    procedure ActionFindNextExecute(Sender: TObject);
    procedure ActionFindNextUpdate(Sender: TObject);
    procedure ActionTranslationMemoryExecute(Sender: TObject);
    procedure ActionDummyExecute(Sender: TObject);
    procedure ActionGotoNextWarningExecute(Sender: TObject);
    procedure ActionBookmarkExecute(Sender: TObject);
    procedure PopupMenuBookmarkPopup(Sender: TObject);
    procedure ActionGotoBookmarkAnyExecute(Sender: TObject);
    procedure ActionEditMarkExecute(Sender: TObject);
    procedure ActionEditMarkUpdate(Sender: TObject);
    procedure ActionGotoNextBookmarkExecute(Sender: TObject);
    procedure StatusBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure StatusBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ActionValidateExecute(Sender: TObject);
    procedure ActionTranslationMemoryAddExecute(Sender: TObject);
    procedure ActionHasActivePropertyUpdate(Sender: TObject);
    procedure ActionTranslationMemoryTranslateExecute(Sender: TObject);
    procedure ActionGotoNextStatusExecute(Sender: TObject);
    procedure ActionGotoNextStateNewExecute(Sender: TObject);
    procedure ActionTranslationMemoryTranslateUpdate(Sender: TObject);
    procedure ActionTranslationMemoryAddUpdate(Sender: TObject);
    procedure ButtonOpenRecentClick(Sender: TObject);
    procedure ActionSettingsExecute(Sender: TObject);
    procedure ActionImportFileTargetUpdate(Sender: TObject);
    procedure BarEditItemTargetLanguageEnter(Sender: TObject);
    procedure BarEditItemTargetLanguageExit(Sender: TObject);
    procedure ActionFeedbackHideExecute(Sender: TObject);
    procedure ActionFeedbackPositiveExecute(Sender: TObject);
    procedure ActionFeedbackNegativeExecute(Sender: TObject);
    procedure TimerHintTimer(Sender: TObject);
    procedure ActionAboutExecute(Sender: TObject);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure TreeListModulesSelectionChanged(Sender: TObject);
    procedure StatusBarPanels1Click(Sender: TObject);
    procedure PopupMenuTranslateProvidersPopup(Sender: TObject);
    procedure TimerToastTimer(Sender: TObject);
    procedure StatusBarHint(Sender: TObject);
    procedure StatusBarPanels2Click(Sender: TObject);
    procedure ActionStopListExecute(Sender: TObject);
    procedure ActionStopListAddExecute(Sender: TObject);
    procedure ActionStopListApplyExecute(Sender: TObject);
    procedure ActionStopListApplyUpdate(Sender: TObject);
    procedure RibbonGalleryItemStopListPopup(Sender: TObject);
    procedure PopupMenuBuildPopup(Sender: TObject);
    procedure ButtonBuildAllClick(Sender: TObject);
    procedure ActionProofingCheckUpdate(Sender: TObject);
    procedure ActionProofingCheckSelectedUpdate(Sender: TObject);
    procedure ActionEditTranslationTextExecute(Sender: TObject);
    procedure ActionTranslationSuggestionListExecute(Sender: TObject);
    procedure ActionProjectRecoverExecute(Sender: TObject);
    procedure ActionTranslationSuggestionListUpdate(Sender: TObject);
    procedure ActionEditTranslationTextUpdate(Sender: TObject);
    procedure ActionClearBookmarksExecute(Sender: TObject);
    procedure ActionTranslationMemoryLocateUpdate(Sender: TObject);
    procedure ActionTranslationMemoryLocateExecute(Sender: TObject);
    procedure ActionEditCopyUpdate(Sender: TObject);
    procedure ActionEditCopyExecute(Sender: TObject);
    procedure ActionEditPasteExecute(Sender: TObject);
    procedure ActionExportCSVExecute(Sender: TObject);
    procedure ActionImportPOExecute(Sender: TObject);
    procedure ActionValidationWarningResolveExecute(Sender: TObject);
    procedure ActionValidationWarningDismissExecute(Sender: TObject);
    procedure ActionValidationWarningDismissUpdate(Sender: TObject);
    procedure ActionValidationWarningResolveUpdate(Sender: TObject);
    procedure GridItemsTableViewCustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
      AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
    procedure GridItemsTableViewCustomDrawIndicatorCell(Sender: TcxGridTableView; ACanvas: TcxCanvas;
      AViewInfo: TcxCustomGridIndicatorItemViewInfo; var ADone: Boolean);
    procedure GridItemsTableViewCellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo;
      AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
    procedure GridItemsTableViewInitEdit(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit);
    procedure GridItemsTableViewStylesGetContentStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
      AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
    procedure GridItemsTableViewDataControllerRecordChanged(ADataController: TcxCustomDataController; ARecordIndex,
      AItemIndex: Integer);
    procedure GridItemsTableViewColumnGetCellHint(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
      ACellViewInfo: TcxGridTableDataCellViewInfo; const AMousePos: TPoint; var AHintText: TCaption; var AIsHintMultiLine: Boolean;
      var AHintTextRect: TRect);
    procedure GridItemsTableViewColumnTargetValidateDrawValue(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
      const AValue: Variant; AData: TcxEditValidateInfo);
    procedure GridItemsTableViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GridItemsTableViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GridItemsTableViewFocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord,
      AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
    procedure EditTargetTextExit(Sender: TObject);
    procedure EditTargetTextEnter(Sender: TObject);
    procedure ActionTextEditFocusedUpdate(Sender: TObject);
    procedure ActionTextEditCancelExecute(Sender: TObject);
    procedure ActionTextEditPreviousExecute(Sender: TObject);
    procedure ActionTextEditNextExecute(Sender: TObject);
    procedure ActionTextEditPreviousUpdate(Sender: TObject);
    procedure ActionTextEditNextUpdate(Sender: TObject);
    procedure GridItemsTableViewCanFocusRecord(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; var AAllow: Boolean);
    procedure EditTargetTextPropertiesEditValueChanged(Sender: TObject);
    procedure GridItemsTableViewColumnStatusStateGetFilterValues(Sender: TcxCustomGridTableItem; AValueList: TcxDataFilterValueList);
    procedure GridItemsTableViewEditing(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem; var AAllow: Boolean);
    procedure ActionImportCSVExecute(Sender: TObject);
    procedure PopupMenuValidationWarningPopup(Sender: TObject);
    procedure ActionGotoAgainUpdate(Sender: TObject);
    procedure ActionGotoAgainExecute(Sender: TObject);
    procedure ActionGotoNextStateExistingExecute(Sender: TObject);
    procedure ActionGotoNextStateUnusedExecute(Sender: TObject);
    procedure ActionTranslationMemoryUpdateUpdate(Sender: TObject);
    procedure ActionTranslationMemoryUpdateExecute(Sender: TObject);
    procedure TreeListModulesFocusedColumnChanged(Sender: TcxCustomTreeList;
      APrevFocusedColumn, AFocusedColumn: TcxTreeListColumn);
    procedure GridItemsTableViewFocusedItemChanged(
      Sender: TcxCustomGridTableView; APrevFocusedItem,
      AFocusedItem: TcxCustomGridTableItem);
    procedure ActionProjectLocateSourceExecute(Sender: TObject);
    procedure ActionIntegrationTrackingExecute(Sender: TObject);
  private
    FProject: TLocalizerProject;
    FProjectFilename: string;
    FUpdateCount: integer;
    FUpdateLockCount: integer;
    FModuleItemsDataSource: TLocalizerModuleItemsDataSource;
    FProjectIndex: ILocalizerProjectPropertyLookup;
    FFilterTargetLanguages: boolean;
    FRefreshModuleStatsQueued: boolean;
    FSearchProvider: ILocalizerSearchProvider;
    FLastBookmark: integer;
    FLastGotoAction: TAction;
  private
    // Focus tracking
    FTracker: ITranslationManagerTracker;
    FTrackingDismissed: boolean;
    procedure ResetTracker;
  private
    // Language selection
    FSourceLanguage: TLocaleItem;
    FTargetLanguage: TLocaleItem;
    FTranslationLanguage: TTranslationLanguage;
    function GetLanguageID(Value: LCID): LCID;
    function GetSourceLanguageID: Word;
    function GetTargetLanguageID: Word;
    procedure SetSourceLanguageID(const Value: Word);
    procedure SetTargetLanguageID(const Value: Word);
    function GetTranslationLanguage: TTranslationLanguage;
    procedure ClearTargetLanguage;
    procedure UpdateTargetLanguage(Clear: boolean = False);
  private
    // Spell check
    FCanSpellCheck: boolean;
    FSpellCheckProp: TLocalizerProperty;
    FSpellCheckingWord: boolean;
    FSpellCheckingString: boolean;
    FSpellCheckingStringResult: boolean;
    function PerformSpellCheck(Prop: TLocalizerProperty): boolean;
  private
    FTranslationMemory: ITranslationMemory;
    FTranslationMemoryPeek: ITranslationMemoryPeek;
    procedure CreateTranslationMemoryPeeker(Force: boolean);
    procedure TranslationMemoryPeekHandler(Sender: TObject);
    procedure QueueTranslationMemoryPeek(Prop: TLocalizerProperty; Force: boolean = False); overload; // Specified row
    procedure QueueTranslationMemoryPeek(RecordIndex: integer; Force: boolean = False); overload; // Specified row
    procedure QueueTranslationMemoryPeek; overload; // All nodes
    procedure ClearTranslationMemoryPeekResult;
  private
    // Custom hint management
    FHintRect: TRect;
    FHintVisible: boolean;
    FShowHint: boolean;
    FHintProp: TLocalizerProperty;
    procedure HideHint;
  private
    // Toast messages
    FToastMessage: string;
    procedure QueueToast(const Msg: string);
    procedure DismissToast; overload;
    procedure DismissToast(const Msg: string); overload;
  private
    // Warnings
    FWarningCount: integer;
  private
    procedure SaveSettings;
    procedure ApplySettings;
    procedure ApplyCustomSettings;
    procedure ApplyListStyles;
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
    procedure MsgRefreshModuleStats(var Msg: TMessage); message MSG_REFRESH_MODULE_STATS;
    procedure MsgCopyData(var Msg: TWMCopyData); message WM_COPYDATA;
  protected
    procedure ScaleFactorChanged(M: Integer; D: Integer); override;
  private
    // Translation project event handlers
    procedure OnProjectChanged(Sender: TObject);
    procedure OnModuleChanged(Module: TLocalizerModule);
    procedure OnTranslationWarning(Translation: TLocalizerTranslation);
  private
    // Recent files
    procedure LoadRecentFiles;
    procedure SaveRecentFiles;
    procedure AddRecentFile(const Filename: string);
  protected
    function GetProject: TLocalizerProject;
    function GetFocusedItem: TCustomLocalizerItem;
    function GetIsModuleActive: boolean;
    function GetFocusedModule: TLocalizerModule;
    function GetActiveModule: TLocalizerModule;
    function GetIsPropertyActive: boolean;
    function GetFocusedProperty: TLocalizerProperty;
    function GetActiveProperty: TLocalizerProperty;

    property FocusedItem: TCustomLocalizerItem read GetFocusedItem;

    // IsModuleActive=True if module grid has focus. Otherwise False.
    property IsModuleActive: boolean read GetIsModuleActive;
    property FocusedModule: TLocalizerModule read GetFocusedModule;
    // ActiveModule = FocusedModule if IsModuleActive=True. Otherwise nil.
    property ActiveModule: TLocalizerModule read GetActiveModule;

    // IsPropertyActive=True if property grid has focus. Otherwise False.
    property IsPropertyActive: boolean read GetIsPropertyActive;
    property FocusedProperty: TLocalizerProperty read GetFocusedProperty;
    // ActiveProperty = FocusedProperty if IsPropertyActive=True. Otherwise nil.
    property ActiveProperty: TLocalizerProperty read GetActiveProperty;
  protected
    function GetSelectionCount: integer;
    function GetSelection(Index: integer): TCustomLocalizerItem;
    property SelectionCount: integer read GetSelectionCount;
    property Selection[Index: integer]: TCustomLocalizerItem read GetSelection;
    function HasSelection: boolean;
  protected
    FPendingFileOpen: TStrings;
    FPendingFileOpenLock: TCriticalSection;
    function LoadFromFile(const Filename: string): boolean;
    procedure LoadFromSingleInstance(const Param: string);
  protected
    procedure LoadProject(Project: TLocalizerProject; Clear: boolean = True);
    procedure LoadItem(Item: TCustomLocalizerItem; Recurse: boolean = False);
    procedure LoadFocusedItem(Recurse: boolean = False);
    procedure LoadModuleNode(Node: TcxTreeListNode; Recurse: boolean); overload;
    procedure LoadModuleNode(Node: TcxTreeListNode; Module: TLocalizerModule; Recurse: boolean); overload;
    procedure LoadFocusedProperty;
    procedure ReloadNode(Node: TcxTreeListNode);
    procedure ReloadProperty(Prop: TLocalizerProperty);
    procedure RefreshModuleStats;
    procedure DoRefreshModuleStats;
    function ViewProperty(Prop: TLocalizerProperty): boolean;
    function ViewItem(Item: TLocalizerItem): boolean;
    function ViewModule(Module: TLocalizerModule): boolean;
    procedure TranslationAdded(AProp: TLocalizerProperty); overload;
    procedure TranslationAdded(AProp: TLocalizerProperty; var AutoApplyTranslations, AutoApplyTranslationsSimilar: TTranslationAutoApply; var UpdatedSame, UpdatedSimilar: integer); overload;
    procedure DisplayAutoApplyTranslationsStats(UpdatedSame, UpdatedSimilar: integer);
    function ApplyStopList(const Progress: IProgress = nil): TStopListItemList.TStopListStats;
    function RecoverUnusedTranslations(OnlyNew: boolean): integer;
  protected
    procedure InitializeProject(const SourceFilename: string; SourceLocaleID: Word);
    procedure LockUpdates;
    procedure UnlockUpdates;
    procedure BeginUpdate;
    procedure EndUpdate;
    function CheckSave: boolean;
    procedure ClearDependents;
    function GotoNext(Predicate: TLocalizerPropertyDelegate; DisplayNotFound: boolean = True; FromStart: boolean = False; AutoWrap: boolean = True): boolean;
    procedure UpdateProjectModifiedIndicator;
    function LocateSourceFile(ForcePrompt: boolean): boolean;
    function LocateStringsSymbolFile(CheckForOutOfDate, ForcePrompt: boolean): boolean;
    function CheckSourceFile: boolean;
    function CheckStringsSymbolFile(CheckForOutOfDate: boolean): boolean;
  protected
    FTextEditProperty: TLocalizerProperty; // Property being edited
    FTextEditing: boolean; // True if text editor has focus
    FTextEditModified: boolean;
    procedure PostTranslationTextEdit;
    procedure ApplyTranslationTextEdit(EndEdit: boolean);
  private
    // Machine Translation
    procedure TranslateSelected(const TranslationService: ITranslationService; const TranslationProvider: ITranslationProvider);
    procedure OnTranslationProviderHandler(Sender: TObject);
  private type
    TCounts = record
      CountModule, CountItem, CountProperty, CountSynthesized: integer;
      UnusedModule, UnusedItem, UnusedProperty, UnusedTranslation: integer;
      Translated: integer;
      ObsoleteTranslation: integer;
    end;
  private
    function CountStuff: TCounts;
  protected
    // Build
    function BuildLanguageModule(LocaleItem: TLocaleItem; const Filename: string): boolean;
    procedure OnBuildSingleLanguageHandler(Sender: TObject);
  protected
    // ILocalizerSearchHost
    function ILocalizerSearchHost.GetProject = GetProject;
    function ILocalizerSearchHost.GetSelectedModule = GetFocusedModule;
    function ILocalizerSearchHost.GetTranslationLanguage = GetTranslationLanguage;
    function ILocalizerSearchHost.ViewItem = ViewProperty;
    procedure ILocalizerSearchHost.InvalidateItem = ReloadProperty;
  protected
    // IExceptionInfoProvider
    procedure GetExceptionInfo(const ExceptIntf: IUnknown; const ExceptionInfoConsumer: IExceptionInfoConsumer);
  private
    // Skin
    FSkin: string;
    FColorSchemeAccent: integer;
  protected
    procedure SetSkin(const Value: string);
    property Skin: string read FSkin write SetSkin;
  public
    constructor Create(AOwner: TComponent); override;

    property SourceLanguage: TLocaleItem read FSourceLanguage;
    property SourceLanguageID: Word read GetSourceLanguageID write SetSourceLanguageID;

    property TargetLanguage: TLocaleItem read FTargetLanguage;
    property TargetLanguageID: Word read GetTargetLanguageID write SetTargetLanguageID;
    property TranslationLanguage: TTranslationLanguage read GetTranslationLanguage;
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
  Types,
  IOUtils,
  StrUtils,
  Generics.Defaults,
  System.Character,
  RegularExpressions,
  CommCtrl,
  ClipBrd,
{$ifdef DEBUG}
  Diagnostics,
{$endif DEBUG}

  // DevExpress skins
  dxSkinOffice2016Colorful,
  dxSkinOffice2019Colorful,

  // Skin utils
  dxSkinsDefaultPainters,
  dxSkinsdxRibbonPainter,

  dxHunspellDictionary,
  dxSpellCheckerDialogs,
  cxDrawTextUtils,
  dxBarExtItems,

  DelphiDabbler.SingleInstance,

  amCursorService,
  amVersionInfo,
  amShell,
  amPath,
  amSplash,
  amFileUtils,
  amProgress.Stream,

  amLocalization.System.Restart,
  amLocalization.Utils,
  amLocalization.Engine,
  amLocalization.ResourceWriter,
  amLocalization.Persistence,
  amLocalization.Import.XLIFF,
  amLocalization.Data.Main,
  amLocalization.Skin,
  amLocalization.Normalization,
  amLocalization.Shell,
  amLocalization.Environment,
  amLocalization.Common,
  amLocalization.Export.CSV,
  amLocalization.Import.PO,
  amLocalization.Settings.SpellChecker,
  amLocalization.Settings.Layout.Tree,
  amLocalization.Integration.Tracker.API,
  amLocalization.Dialog.TextEdit,
  amLocalization.Dialog.NewProject,
  amLocalization.Dialog.TranslationMemory,
  amLocalization.Dialog.Languages,
  amLocalization.Dialog.Settings,
{$ifdef MADEXCEPT}
  amLocalization.Dialog.Feedback,
{$endif MADEXCEPT}
  amLocalization.Dialog.StopList,
  amLocalization.Dialog.Import.CSV;

resourcestring
  sLocalizerFindNone = 'None found';
  sLocalizerFindNoMore = 'No more found';
  sLocalizerFindWrapAround = 'Reached end, continued from start';

resourcestring
  sProjectUpdatedTitle = 'Project updated';
  sProjectUpdated = 'Update completed with the following changes:'#13#13+
    'Modules: %.0n added, %.0n unused'#13+
    'Items: %.0n added, %.0n unused'#13+
    'Properties: %.0n added, %.0n unused, %.0n synthesized'#13+
    'Translations obsoleted: %.0n';
  sProjectUpdatedNothing = 'Update completed without any changes.';

resourcestring
  sTranslationsUpdatedTitle = 'Translations updated';
  sTranslationsUpdated = 'Translations have been updated with the following changes:'#13#13+
    'Added: %.0n'#13+
    'Updated: %.0n'#13+
    'Skipped: %.0n';

resourcestring
  sRecoverUnusedTranslationsTitle = 'Recover translations';
  sRecoverUnusedTranslationsUpdate = '%d translations have been marked unused.';
  sRecoverUnusedTranslationsAction = '%d translations are currently marked unused.';
  sRecoverUnusedTranslations = '%s'#13+
    'This is likely because components have been deleted, moved or renamed.'#13#13+
    'For components that have been moved or renamed their translations can often be recovered and applied to the new components.'#13#13+
    'Do you want to attempt an automatic recovery of these translations?';
  sRecoverUnusedStatusTitle = 'Recover completed';
  sRecoverUnusedStatus = '%d of %d unused translations were recovered.';
  sRecoverUnusedStatusNone = 'No translations recovered.';

resourcestring
  sDone = 'Done!';

const
  StatusBarPanelHint = 0;
  StatusBarPanelWarning = 1;
  StatusBarPanelModified = 2;
  StatusBarPanelStats = 3;

const
  sDefaultSkinName = 'Office2016Colorful';

const
  HintCornerSize = 8;

// -----------------------------------------------------------------------------
//
// Class crackers
//
// -----------------------------------------------------------------------------
type
  TCustomdxBarControlCracker = class(TCustomdxBarControl);
  TdxBarItemCracker = class(TdxBarItem);

type
  TcxTreeListNodeCracker = class(TcxTreeListNode);
  TcxTreeListCracker = class(TcxTreeList);

type
  TcxCustomEditCracker = class(TcxCustomEdit);

type
  TdxSpellCheckerCracker = class(TdxCustomSpellChecker);

// -----------------------------------------------------------------------------

function TreeListFindFilter(ANode: TcxTreeListNode; AData: Pointer): Boolean;
begin
  Result := (ANode.Data = AData);
end;


// -----------------------------------------------------------------------------
//
// TFormMain
//
// -----------------------------------------------------------------------------
type
  TcxSplitterCracker = class(TcxSplitter);

constructor TFormMain.Create(AOwner: TComponent);
begin
  SaveCursor(crAppStart, True);

  inherited;

  // Only way to avoid skinning splitter
  TcxSplitterCracker(SplitterTreeLists).LookAndFeel.SkinName := '';
  TcxSplitterCracker(SplitterMainEditors).LookAndFeel.SkinName := '';
  TcxSplitterCracker(SplitterEditors).LookAndFeel.SkinName := '';
end;

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

  procedure LoadBanner;
  var
    Filename: string;
  begin
    // Look for custom ribbon background image
    Filename := TranslationManagerSettings.FolderInstall + 'banner.png';
    if (not FileExists(Filename)) then
    begin
      Filename := TranslationManagerSettings.FolderInstall + 'banner.bmp';
      if (not FileExists(Filename)) then
      begin
        Filename := TranslationManagerSettings.FolderInstall + 'banner.jpg';
        if (not FileExists(Filename)) then
          Filename := '';
      end;
    end;

    if (Filename <> '') then
      RibbonMain.BackgroundImage.LoadFromFile(Filename);
  end;

var
  i: integer;
begin
  DisableAero := True;

  FPendingFileOpenLock := TCriticalSection.Create;

  FProject := TLocalizerProject.Create('', GetLanguageID(TranslationManagerSettings.System.DefaultSourceLanguage));
  FProject.OnChanged := OnProjectChanged;
  FProject.OnModuleChanged := OnModuleChanged;
  FProject.OnTranslationWarning := OnTranslationWarning;

  FModuleItemsDataSource := TLocalizerModuleItemsDataSource.Create(nil);
  GridItemsTableView.DataController.CustomDataSource := FModuleItemsDataSource;

  DataModuleMain := TDataModuleMain.Create(Self);
  FTranslationMemory := TranslationMemory.PrimaryProvider;

  Application.OnHint := ShowHint;
  Application.OnShowHint := DoShowHint;

  RibbonTabMain.Active := True;
  SplitterMainEditors.CloseSplitter;

  for i := 0 to StatusBar.Panels.Count-1 do
    StatusBar.Panels[i].Text := '';
  SetLength(FStatusBarPanelHint, StatusBar.Panels.Count);

  CreateBookmarkMenu;

  FSkin := '';
  FColorSchemeAccent := Ord(RibbonMain.ColorSchemeAccent);

  if (TranslationManagerSettings.System.SafeMode) then
    Caption := Caption + ' [SAFE MODE]';

  // Make sure folders exist and are writable
  if (not TranslationManagerSettings.Folders.ValidateFolders) then
  begin
    Application.ShowMainForm := False; // Prevent flash as form is shown and then immediately destroyed
    Application.Terminate;
    Exit;
  end;

  ApplySettings;
  ApplyCustomSettings;

  // Load ribbon banner
  LoadBanner;

  ExceptionHandler.RegisterExceptionInfoProvider(Self);

  SingleInstance.OnProcessParam := LoadFromSingleInstance;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  ExceptionHandler.UnregisterExceptionInfoProvider(Self);

  FTranslationMemoryPeek := nil;
  FTranslationMemory := nil;

  SaveSettings;

  SpellChecker.Dictionaries[0].Unload;

  Application.OnHint := nil;
  Application.OnShowHint := nil;

  // Block notifications
  FProject.BeginUpdate;

  FProjectIndex := nil;
  FProject.Clear;

  FModuleItemsDataSource.Free;
  FProject.Free;

  FreeAndNil(FPendingFileOpen);
  FreeAndNil(FPendingFileOpenLock);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.GetExceptionInfo(const ExceptIntf: IInterface; const ExceptionInfoConsumer: IExceptionInfoConsumer);
begin
  ExceptionInfoConsumer.AddExceptionInfo('Application', 'Portable', TranslationManagerSettings.System.Portable.ToString(TUseBoolStrs.True));
  ExceptionInfoConsumer.AddExceptionInfo('Application', 'SingleInstance', TranslationManagerSettings.System.SingleInstance.ToString(TUseBoolStrs.True));
  ExceptionInfoConsumer.AddExceptionInfo('Application', 'Skin', FSkin);
  ExceptionInfoConsumer.AddExceptionInfo('Application', 'ApplicationLanguage', TranslationManagerSettings.System.ApplicationLanguage.ToHexString(8));
  ExceptionInfoConsumer.AddExceptionInfo('Application', 'Safe Mode', TranslationManagerSettings.System.SafeMode.ToString(TUseBoolStrs.True));
  ExceptionInfoConsumer.AddExceptionInfo('Application', 'Last boot completed', TranslationManagerSettings.System.LastBootCompleted.ToString(TUseBoolStrs.True));
  ExceptionInfoConsumer.AddExceptionInfo('Application', 'First run', TranslationManagerSettings.System.FirstRun.ToString(TUseBoolStrs.True));
  ExceptionInfoConsumer.AddExceptionInfo('Application', 'First this version', TranslationManagerSettings.System.FirstRunThisVersion.ToString(TUseBoolStrs.True));

  ExceptionInfoConsumer.AddExceptionInfo('Folders', 'Install', TranslationManagerSettings.FolderInstall);
  for var Folder := Low(TTranslationManagerFolder) to High(TTranslationManagerFolder) do
    ExceptionInfoConsumer.AddExceptionInfo('Folders', TranslationManagerSettings.Folders.FolderName[Folder], TranslationManagerSettings.Folders[Folder]);

  ExceptionInfoConsumer.AddExceptionInfo('Project', 'Source Language ID', SourceLanguageID.ToHexString(8));
  ExceptionInfoConsumer.AddExceptionInfo('Project', 'Source Language', SourceLanguage.LocaleName);
  ExceptionInfoConsumer.AddExceptionInfo('Project', 'Target Language ID', TargetLanguageID.ToHexString(8));
  ExceptionInfoConsumer.AddExceptionInfo('Project', 'Target Language', TargetLanguage.LocaleName);
  ExceptionInfoConsumer.AddExceptionInfo('Project', 'Filename', FProjectFilename);
  ExceptionInfoConsumer.AddExceptionInfo('Project', 'Source filename', FProject.SourceFilename);
  ExceptionInfoConsumer.AddExceptionInfo('Project', 'Symbol Filename', FProject.StringSymbolFilename);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ApplyListStyles;

  procedure ApplyListStyle(AListStyle: TListStyle; Style: TcxStyle);
  var
    ListStyle: TTranslationManagerListStyleSettings;
  begin
    ListStyle := TranslationManagerSettings.Editor.Style[AListStyle];

    if (ListStyle.ColorBackground <> clDefault) then
      Style.Color := ListStyle.ColorBackground
    else
    if (TranslationManagerSettings.Editor.Style[ListStyleDefault].ColorBackground <> clDefault) then
      Style.Color := TranslationManagerSettings.Editor.Style[ListStyleDefault].ColorBackground
    else
      Style.Color := clWhite;

    if (ListStyle.ColorText <> clDefault) then
      Style.TextColor := ListStyle.ColorText
    else
    if (TranslationManagerSettings.Editor.Style[ListStyleDefault].ColorText <> clDefault) then
      Style.TextColor := TranslationManagerSettings.Editor.Style[ListStyleDefault].ColorText
    else
      Style.TextColor := clBlack;

    if (ListStyle.Bold <> -1) then
    begin
      if (ListStyle.Bold = 1) then
        Style.Font.Style := Style.Font.Style + [fsBold]
      else
        Style.Font.Style := Style.Font.Style - [fsBold];
    end else
    if (TranslationManagerSettings.Editor.Style[ListStyleDefault].Bold = 1) then
      Style.Font.Style := Style.Font.Style + [fsBold]
    else
      Style.Font.Style := Style.Font.Style - [fsBold];

    // Make sure we're using the correct font
    if (TcxStyleValue.svFont in Style.AssignedValues) then
      Style.Font.Name := Font.Name;
  end;

begin
  ApplyListStyle(ListStyleDefault, DataModuleMain.StyleDefault);
  ApplyListStyle(ListStyleSelected, DataModuleMain.StyleSelected);
  ApplyListStyle(ListStyleInactive, DataModuleMain.StyleInactive);
  ApplyListStyle(ListStyleFocused, DataModuleMain.StyleFocused);
  ApplyListStyle(ListStyleNotTranslated, DataModuleMain.StyleNeedTranslation);
  ApplyListStyle(ListStyleProposed, DataModuleMain.StyleProposed);
  ApplyListStyle(ListStyleTranslated, DataModuleMain.StyleComplete);
  ApplyListStyle(ListStyleHold, DataModuleMain.StyleHold);
  ApplyListStyle(ListStyleDontTranslate, DataModuleMain.StyleDontTranslate);
  ApplyListStyle(ListStyleSynthesized, DataModuleMain.StyleSynthesized);
end;

procedure TFormMain.ApplySettings;
begin
  (*
  ** Settings that can NOT be modified via GUI or which require restart
  *)
  TranslationManagerProofingSettingsAdapter.ApplyTo(TranslationManagerSettings.Proofing, SpellChecker);
  SpellChecker.UseThreadedLoad := (not TranslationManagerSettings.System.SafeMode);
  SpellChecker.CheckAsYouTypeOptions.Active := SpellChecker.CheckAsYouTypeOptions.Active and (not TranslationManagerSettings.System.SafeMode);

  LoadRecentFiles;

  OpenDialogXLIFF.InitialDir := EnvironmentVars.ExpandString(TranslationManagerSettings.Folders.FolderDocuments);
  OpenDialogProject.InitialDir := OpenDialogXLIFF.InitialDir;

  if (TranslationManagerSettings.Layout.ModuleTree.Valid) then
  begin
    // Tree.RestoreFromRegistry fails if data doesn't exist
    TreeListModules.RestoreFromRegistry(TranslationManagerSettings.Layout.KeyPath, False, False, TranslationManagerSettings.Layout.ModuleTree.Name);
    TranslationManagerLayoutTreeSettingsAdapter.ReadFilter(TranslationManagerSettings.Layout.ModuleTree, TreeListModules.Filter);
  end;

  if (TranslationManagerSettings.Layout.ItemGrid.Valid) then
  begin
    GridItemsTableView.RestoreFromRegistry(TranslationManagerSettings.Layout.KeyPath, False, False, [gsoUseFilter], TranslationManagerSettings.Layout.ItemGrid.Name);
    // TranslationManagerSettings.Layout.ItemGrid.ReadFilter(GridItemsTableView.Filtering);
  end;

{$ifdef MADEXCEPT}
  if (TranslationManagerSettings.System.HideFeedback) then
    BarButtonFeedback.Visible := ivInCustomizing
  else
    BarButtonFeedback.Visible := ivAlways;
{$else MADEXCEPT}
  ActionFeedback.Visible := False;
  BarButtonFeedback.Visible := ivNever;
{$endif MADEXCEPT}
end;

procedure TFormMain.ApplyCustomSettings;
begin
  (*
  ** Settings that can be modified via GUI
  *)
  SetSkin(TranslationManagerSettings.System.Skin);

  ApplyListStyles;

  if (TranslationManagerSettings.Editor.UseProposedStatus) then
    TLocalizerTranslations.DefaultStatus := tStatusProposed
  else
    TLocalizerTranslations.DefaultStatus := tStatusTranslated;

  if (TranslationManagerSettings.Editor.DisplayStatusGlyphs) then
  begin
    TreeListModules.Images := DataModuleMain.ImageListTree;
  end else
  begin
    TreeListModules.Images := nil;
  end;
  // Force redraw
  TreeListModules.LayoutChanged;
end;

procedure TFormMain.SaveSettings;
begin
  TranslationManagerSettings.Version := TVersionInfo.FileVersionString(ParamStr(0));

  SaveRecentFiles;

  TranslationManagerSettings.Forms.Main.PrepareSettings(Self);

  // TreeList layout and filters
  TreeListModules.StoreToRegistry(TranslationManagerSettings.Layout.KeyPath, False, TranslationManagerSettings.Layout.ModuleTree.Name);
  TranslationManagerLayoutTreeSettingsAdapter.WriteFilter(TranslationManagerSettings.Layout.ModuleTree, TreeListModules.Filter);
  TranslationManagerSettings.Layout.ModuleTree.Valid := True;

  GridItemsTableView.StoreToRegistry(TranslationManagerSettings.Layout.KeyPath, False, [gsoUseFilter], TranslationManagerSettings.Layout.ItemGrid.Name);
  TranslationManagerSettings.Layout.ItemGrid.Valid := True;

  if (not TranslationManagerSettings.System.SafeMode) then // Spell checker setting are not complete in safe mode
    TranslationManagerProofingSettingsAdapter.SaveFrom(TranslationManagerSettings.Proofing, SpellChecker);

  TranslationManagerSettings.System.HideFeedback := (BarButtonFeedback.Visible <> ivAlways);

  TranslationManagerSettings.Valid := True;
  TranslationManagerSettings.WriteConfig;
end;

procedure TFormMain.ScaleFactorChanged(M, D: Integer);
begin
  inherited ScaleFactorChanged(M, D);
  DataModuleMain.FontSizeChanged(Font.Size);
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

procedure TFormMain.MsgRefreshModuleStats(var Msg: TMessage);
begin
  DoRefreshModuleStats;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.MsgSourceChanged(var Msg: TMessage);
begin
  SourceLanguageID := BarEditItemSourceLanguage.EditValue;
end;

procedure TFormMain.MsgTargetChanged(var Msg: TMessage);
begin
  TargetLanguageID := BarEditItemTargetLanguage.EditValue;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
var
  TheShortCut: TShortCut;
begin
  TheShortCut := ShortCutFromMessage(Msg);

  if (TheShortCut = ShortCut(VK_F12, [ssAlt])) then
  begin
    if (not CheckSave) then
      exit;

    InitializeProject('test.xxx', SourceLanguageID);

    FProject.AddModule('ONE', mkForm).AddItem('Item1', 'TFooBar').AddProperty('Test1', 'value1');
    FProject.AddModule('TWO', mkForm).AddItem('Item2', 'TFooBar').AddProperty('Test2', 'value2');

    LoadProject(FProject, True);

    Handled := True;
  end else
  if (TheShortCut = ShortCut(Ord('A'), [ssCtrl])) then
  begin
    if (IsPropertyActive) then
    begin
      GridItemsTableView.Controller.SelectAll;
      Handled := True;
    end else
    if (IsModuleActive) then
    begin
      TreeListModules.SelectAll;
      Handled := True;
    end;
  end;

  if (not Handled) then
    inherited;
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

procedure TFormMain.BeginUpdate;
begin
  Inc(FUpdateCount);
  TreeListModules.BeginUpdate;
  GridItemsTableView.BeginUpdate;
end;

procedure TFormMain.EndUpdate;
begin
  ASSERT(FUpdateCount > 0);
  Dec(FUpdateCount);
  GridItemsTableView.EndUpdate;
  TreeListModules.EndUpdate;
end;

procedure TFormMain.LockUpdates;
begin
  Inc(FUpdateLockCount);
  BeginUpdate;
end;

procedure TFormMain.UnlockUpdates;
begin
  ASSERT(FUpdateLockCount > 0);
  Dec(FUpdateLockCount);
  EndUpdate;
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

procedure TFormMain.ActionHasActivePropertyUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (IsPropertyActive);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionAboutExecute(Sender: TObject);
var
  FormSplash: TFormSplash;
begin
  FormSplash := TFormSplash.Create(nil);
  try
    FormSplash.Version := TVersionInfo.FileVersionString(Application.ExeName);
    FormSplash.DisplayBannerResource('CREDITS', 'TEXT', sbStaticFade);
    FormSplash.BannerOffset := Abs(FormSplash.Font.Height); // TODO : HighDPI

    FormSplash.Execute(False);
  except
    FormSplash.Free;
    raise;
  end;
end;

procedure TFormMain.ActionTranslationMemoryAddExecute(Sender: TObject);
var
  i: integer;
  Item: TCustomLocalizerItem;
  Count, ElegibleCount: integer;
  ElegibleWarning: string;
  Stats, OneStats: TTranslationMemoryMergeStats;
  DuplicateAction: TTranslationMemoryDuplicateAction;
  Progress: IProgress;
resourcestring
  sAddToDictionaryPromptTitle = 'Add to Translation Memory?';
  sAddToDictionaryPrompt = 'Do you want to add the selected %d values to the Translation Memory?%s';
  sAddToDictionaryEligibleWarning = 'Note: %d of the selected values are not elegible for translation and have been excluded.';
  sProgressAddingTranslationMemory = 'Adding to Translation Memory...';
begin
  if (not HasSelection) then
    Exit;

  Count := 0;
  for i := 0 to SelectionCount-1 do
  begin
    Item := Selection[i];
    Item.Traverse(
      function(Prop: TLocalizerProperty): boolean
      begin
        Inc(Count);
        if (Prop.EffectiveStatus = ItemStatusTranslate) and (Prop.HasTranslation(TranslationLanguage)) then
          Inc(ElegibleCount);
        Result := True;
      end);
  end;

  if (ElegibleCount = 0) then
    Exit;

  if (ElegibleCount < Count) then
    ElegibleWarning :=  #13#13 + Format(sAddToDictionaryEligibleWarning, [Count-ElegibleCount])
  else
    ElegibleWarning :=  '';

  if (TaskMessageDlg(sAddToDictionaryPromptTitle, Format(sAddToDictionaryPrompt, [ElegibleCount, ElegibleWarning]),
    mtConfirmation, [mbYes, mbNo], 0, mbYes) <> mrYes) then
    Exit;

  if (FTranslationMemoryPeek <> nil) then
    FTranslationMemoryPeek.Cancel;

  Stats := Default(TTranslationMemoryMergeStats);
  Progress := ShowProgress(sProgressAddingTranslationMemory);
  Progress.EnableAbort := True;
  Progress.Progress(psProgress, 0, ElegibleCount);

  for i := 0 to SelectionCount-1 do
  begin
    Item := Selection[i];
    Item.Traverse(
      function(Prop: TLocalizerProperty): boolean
      begin
        if (Prop.EffectiveStatus = ItemStatusTranslate) and (Prop.HasTranslation(TranslationLanguage)) then
        begin
          DuplicateAction := FTranslationMemory.Add(SourceLanguage, Prop.Value, TargetLanguage, Prop.TranslatedValue[TranslationLanguage], OneStats, DuplicateAction);

          Inc(Stats.Added, OneStats.Added);
          Inc(Stats.Merged, OneStats.Merged);
          Inc(Stats.Skipped, OneStats.Skipped);
          Inc(Stats.Duplicate, OneStats.Duplicate);

          Progress.AdvanceProgress;
        end;

        Result := (DuplicateAction <> tmDupActionAbort) and (not Progress.Aborted);
      end);

    if (DuplicateAction = tmDupActionAbort) or (Progress.Aborted) then
      break;
  end;

  Progress.Hide;

  QueueTranslationMemoryPeek;

  if (DuplicateAction = tmDupActionAbort) and (ElegibleCount = 1) then
    Exit; // Nothing to report

  TaskMessageDlg(sTranslationMemoryAddCompleteTitle,
    Format(sTranslationMemoryMergeComplete, [Stats.Added * 1.0, Stats.Merged * 1.0, Stats.Skipped * 1.0, Stats.Duplicate * 1.0]),
    mtInformation, [mbOK], 0);
end;

procedure TFormMain.ActionTranslationMemoryAddUpdate(Sender: TObject);
var
  Enabled: boolean;
  Count: integer;
  i: integer;
  Item: TCustomLocalizerItem;
begin
  Enabled := (HasSelection) and (SourceLanguage <> TargetLanguage) and (FTranslationMemory.IsAvailable);

  if (Enabled) and (SelectionCount < 100) then
  begin
    // Require that at least one property is translated
    // Only check first 100 properties.
    // After that give up and assume one is translated. Execute handler will ignore properties that are not translated.
    Count := 0;
    for i := 0 to SelectionCount-1 do
    begin
      Item := Selection[i];
      Enabled := not Item.Traverse(
        function(Prop: TLocalizerProperty): boolean
        begin
          Inc(Count);
          Result := (Count < 100) and (not Prop.HasTranslation(TranslationLanguage));
        end);
      if (Enabled) then
        break;
    end;
  end;
  TAction(Sender).Enabled := Enabled;
end;

procedure TFormMain.ActionTranslationMemoryExecute(Sender: TObject);
var
  FormTranslationMemory: TFormTranslationMemory;
begin
  // Kill thread. We might delete fields and other nasty stuff which it can't handle.
  FTranslationMemoryPeek := nil;

  FormTranslationMemory := TFormTranslationMemory.Create(nil);
  try

    FormTranslationMemory.Execute(FTranslationMemory);

  finally
    FormTranslationMemory.Free;
  end;

  ClearTranslationMemoryPeekResult;
  CreateTranslationMemoryPeeker(True);
end;

procedure TFormMain.ActionTranslationMemoryLocateExecute(Sender: TObject);
var
  FormTranslationMemory: TFormTranslationMemory;
resourcestring
  sTMLocateNotFound = 'Source/Target pair not found in TM';
begin
  SaveCursor(crHourGlass);

  FormTranslationMemory := TFormTranslationMemory.Create(nil);
  try

    FormTranslationMemory.Execute(FTranslationMemory,
      function(FormTranslationMemory: TFormTranslationMemory): boolean
      var
        TranslationMemoryFormTools: ITranslationMemoryFormTools;
        Prop: TLocalizerProperty;
      begin
        if (Supports(FormTranslationMemory, ITranslationMemoryFormTools, TranslationMemoryFormTools)) then
        begin
          Prop := FocusedProperty;
          Result := TranslationMemoryFormTools.LocatePair(SourceLanguage, Prop.Value, TargetLanguage, Prop.TranslatedValue[TranslationLanguage]);
          if (not Result) then
            ShowMessage(sTMLocateNotFound);
        end else
          Result := False;

        if (Result) then
          // Kill thread. We might delete fields and other nasty stuff which it can't handle.
          FTranslationMemoryPeek := nil;
      end);

  finally
    FormTranslationMemory.Free;
  end;

  if (FTranslationMemoryPeek = nil) then
  begin
    ClearTranslationMemoryPeekResult;
    CreateTranslationMemoryPeeker(True);
  end;
end;

procedure TFormMain.ActionTranslationMemoryLocateUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (ActiveProperty <> nil) and (SourceLanguage <> TargetLanguage) and (FTranslationMemory.IsAvailable) and
    (FocusedProperty.HasTranslation(TranslationLanguage));
end;

procedure TFormMain.TranslateSelected(const TranslationService: ITranslationService; const TranslationProvider: ITranslationProvider);
type
  TAutoTranslateCounts = record
    Count: integer;
    ElegibleCount: integer;
    TranslatedCount: integer;
    UpdatedCount: integer;
  end;
var
  Progress: IProgress;
  i: integer;
  Item: TCustomLocalizerItem;
  Counts: TAutoTranslateCounts;
  Translation: TLocalizerTranslation;
  Warning: string;
  TranslateTranslated: boolean;
resourcestring
  sTranslateAutoProgress = 'Translating using %s...';
  sTranslateAutoPromptTitle = 'Translate using %s?';
  sTranslateAutoPrompt = 'Do you want to perform machine translation on the selected %d values?';
  sTranslateAutoPromptCheck = 'Only translate strings that have not already been translated';
  sTranslateAutoEligibleWarning = '%d of the selected values are not elegible for translation and have been excluded.';
  sTranslateAutoTranslatedWarning = '%d of the selected values have already been translated.';
  sTranslateAutoNoneTitle = 'Nothing to translate';
  sTranslateAutoNone = 'None of the %d selected values are elegible for translation.';
  sTranslateAutoResultTitle = 'Machine translation completed.';
  sTranslateAutoResult = 'Translated: %d'#13'Updated: %d'#13'Not found: %d';
begin
  if (SourceLanguage = TargetLanguage) then
    Exit;

  Counts := Default(TAutoTranslateCounts);
  for i := 0 to SelectionCount-1 do
  begin
    Item := Selection[i];
    Item.Traverse(
      function(Prop: TLocalizerProperty): boolean
      begin
        Inc(Counts.Count);
        if (not Prop.Value.Trim.IsEmpty) and (Prop.EffectiveStatus = ItemStatusTranslate) and (not Prop.IsUnused) then
        begin
          Inc(Counts.ElegibleCount);
          if (Prop.HasTranslation(TranslationLanguage)) then
            Inc(Counts.TranslatedCount);
        end;
        Result := True;
      end);
  end;

  TaskDialogTranslate.Title := Format(sTranslateAutoPromptTitle, [TranslationProvider.ProviderName]);

  if (Counts.TranslatedCount > 0) then
    TaskDialogTranslate.VerificationText := sTranslateAutoPromptCheck
  else
    TaskDialogTranslate.VerificationText := '';

  if (Counts.TranslatedCount > 0) then
  begin
    if (Warning <> '') then
      Warning := Warning + #13;
    Warning := Warning + Format(sTranslateAutoTranslatedWarning, [Counts.TranslatedCount]);
  end;

  if (Counts.ElegibleCount > 0) then
  begin
    TaskDialogTranslate.CommonButtons := [tcbYes, tcbNo];
    TaskDialogTranslate.Text := Format(sTranslateAutoPrompt, [Counts.ElegibleCount]);

    if (Counts.Count <> Counts.ElegibleCount) and (Counts.ElegibleCount <> 0) then
      Warning := Format(sTranslateAutoEligibleWarning, [Counts.Count-Counts.ElegibleCount])
    else
      Warning := '';

    if (Counts.TranslatedCount > 0) then
    begin
      if (Warning <> '') then
        Warning := Warning + #13;
      Warning := Warning + Format(sTranslateAutoTranslatedWarning, [Counts.TranslatedCount]);
    end;

    TaskDialogTranslate.FooterText := Warning;
  end else
  begin
    TaskDialogTranslate.CommonButtons := [tcbOK];
    TaskDialogTranslate.Title := sTranslateAutoNone;
    TaskDialogTranslate.Text := Format(sTranslateAutoNone, [Counts.Count]);
    TaskDialogTranslate.FooterText := '';
  end;

  if (not TaskDialogTranslate.Execute) or (TaskDialogTranslate.ModalResult <> mrYes) then
    Exit;

  TranslateTranslated := not(tfVerificationFlagChecked in TaskDialogTranslate.Flags);

  SaveCursor(crAppStart);

  if (not TranslationService.BeginLookup(SourceLanguage, TargetLanguage)) then
    Exit;

  if (FTranslationMemoryPeek <> nil) then
    // Abort any pending TM peek - we will requeue them all when we're done
    FTranslationMemoryPeek.Cancel;
  try
    try

      if (not TranslateTranslated) then
        Dec(Counts.ElegibleCount, Counts.TranslatedCount);
      Counts.TranslatedCount := 0;
      Counts.UpdatedCount := 0;
      Counts.Count := 0;

      Progress := ShowProgress(Format(sTranslateAutoProgress, [TranslationProvider.ProviderName]));
      Progress.EnableAbort := True;

      FProject.BeginUpdate;
      try
        var TranslatedProps := TList<TLocalizerProperty>.Create;
        try

          for i := 0 to SelectionCount-1 do
          begin
            Item := Selection[i];

            Item.Traverse(
              function(Prop: TLocalizerProperty): boolean
              var
                Value: string;
              begin
                if (Prop.Value.Trim.IsEmpty) or (Prop.EffectiveStatus <> ItemStatusTranslate) or (Prop.IsUnused) then
                  Exit(True);

                if (not TranslateTranslated) and (Prop.HasTranslation(TranslationLanguage)) then
                  Exit(True);

                Inc(Counts.Count);

                Progress.Progress(psProgress, Counts.Count, Counts.ElegibleCount, Prop.Value);
                if (Progress.Aborted) then
                  Exit(False);

                // Perform translation
                // Note: It is the responsibility of the translation service to return True/False to indicate
                // if SourceValue=TargetValue is in fact a translation.
                if (TranslationService.Lookup(Prop, SourceLanguage, TargetLanguage, Value)) then
                begin
                  Inc(Counts.TranslatedCount);

                  if (Prop.Translations.TryGetTranslation(TranslationLanguage, Translation)) and (Translation.Value <> Value) then
                    Inc(Counts.UpdatedCount);

                  // Set value regardless of current value so we get the correct Status set
                  if (Translation <> nil) then
                    Translation.Update(Value, TLocalizerTranslations.DefaultStatus)
                  else
                    Translation := Prop.Translations.AddOrUpdateTranslation(TranslationLanguage, Value);

                  Translation.UpdateWarnings;

                  // Defer calling TranslationAdded() to after we have translated all properties
                  // so user isn't interrupted with prompts to auto-apply.
                  TranslatedProps.Add(Prop);
                end;
                Result := True;
              end);
          end;

          // Call TranslationAdded() for all properties that were translated
          var AutoApplyTranslations := TranslationManagerSettings.Editor.AutoApplyTranslations;
          var AutoApplyTranslationsSimilar := TranslationManagerSettings.Editor.AutoApplyTranslationsSimilar;
          var UpdatedSame := 0;
          var UpdatedSimilar := 0;
          for var Prop in TranslatedProps do
            // Optionally apply translation to rest of project, reload property
            TranslationAdded(Prop, AutoApplyTranslations, AutoApplyTranslationsSimilar, UpdatedSame, UpdatedSimilar);

          DisplayAutoApplyTranslationsStats(UpdatedSame, UpdatedSimilar);

        finally
          TranslatedProps.Free;
        end;

        Progress.Progress(psEnd, Counts.Count, Counts.ElegibleCount);

      finally
        FProject.EndUpdate;
      end;

    finally
      TranslationService.EndLookup;
    end;

  finally
    QueueTranslationMemoryPeek;
  end;

  Progress.Hide;
  Progress := nil;

  RefreshModuleStats;

  TaskMessageDlg(sTranslateAutoResultTitle, Format(sTranslateAutoResult, [Counts.TranslatedCount, Counts.UpdatedCount, Counts.ElegibleCount-Counts.TranslatedCount]),
    mtInformation, [mbOK], 0);
end;

procedure TFormMain.ActionTranslationMemoryTranslateExecute(Sender: TObject);
var
  TranslationProvider: ITranslationProvider;
  TranslationService: ITranslationService;
begin
  TranslationProvider := FTranslationMemory as ITranslationProvider;
  TranslationService := CreateTranslationService(TranslationProvider);

  TranslateSelected(TranslationService, TranslationProvider);
end;

procedure TFormMain.ActionTranslationMemoryTranslateUpdate(Sender: TObject);
var
  Item: TCustomLocalizerItem;
begin
  Item := FocusedItem;

  TAction(Sender).Enabled := (Item <> nil) and (not Item.IsUnused) and (Item.EffectiveStatus <> ItemStatusDontTranslate) and
    (SourceLanguage <> TargetLanguage) and (FTranslationMemory.IsAvailable);
end;

procedure TFormMain.ActionTranslationMemoryUpdateExecute(Sender: TObject);
var
  ElegibleWarning: string;
resourcestring
  sUpdateDictionaryPromptTitle = 'Update Translation Memory?';
  sUpdateDictionaryPrompt = 'Do you want to update the Translation Memory with the selected %.0n values?%s';
  sUpdateDictionaryEligibleWarning = 'Note: %.0n of the selected values are not elegible for translation and have been excluded.';
  sProgressUpdateTranslationMemory = 'Updating Translation Memory...';
begin
  if (not HasSelection) then
    Exit;

  // Count eligible values so we can prompt the user
  var Count := 0;
  var ElegibleCount := 0;
  for var i := 0 to SelectionCount-1 do
  begin
    var Item := Selection[i];
    Item.Traverse(
      function(Prop: TLocalizerProperty): boolean
      begin
        Inc(Count);
        if (Prop.EffectiveStatus = ItemStatusTranslate) and (Prop.HasTranslation(TranslationLanguage)) then
          Inc(ElegibleCount);
        Result := True;
      end);
  end;

  if (ElegibleCount = 0) then
    Exit;

  if (ElegibleCount < Count) then
    ElegibleWarning :=  #13#13 + Format(sUpdateDictionaryEligibleWarning, [(Count-ElegibleCount) * 1.0])
  else
    ElegibleWarning :=  '';

  if (TaskMessageDlg(sUpdateDictionaryPromptTitle, Format(sUpdateDictionaryPrompt, [ElegibleCount * 1.0, ElegibleWarning]),
    mtConfirmation, [mbYes, mbNo], 0, mbYes) <> mrYes) then
    Exit;

  var Stats := Default(TTranslationMemoryMergeStats);
  var DuplicateAction: TTranslationMemoryDuplicateAction := tmDupActionPrompt;

  // Create list of unique translated values
  var Translations := TDictionary<string, TLocalizerProperty>.Create(ElegibleCount);
  try

    for var i := 0 to SelectionCount-1 do
    begin
      var Item := Selection[i];
      Item.Traverse(
        function(Prop: TLocalizerProperty): boolean
        var
          Translation: string;
        begin
          Inc(Count);
          if (Prop.EffectiveStatus = ItemStatusTranslate) and (Prop.HasTranslation(TranslationLanguage)) then
          begin
            Translation := Prop.TranslatedValue[TranslationLanguage];
            if (not Translations.TryAdd(Translation, Prop)) then // Ignore duplicates
              Inc(Stats.Duplicate);
          end;
          Result := True;
        end);
    end;

    if (FTranslationMemoryPeek <> nil) then
      FTranslationMemoryPeek.Cancel;

    var Progress := ShowProgress(sProgressUpdateTranslationMemory);
    Progress.EnableAbort := True;
    Progress.Progress(psProgress, 0, Translations.Count);


    var LookupResult := TTranslationLookupResult.Create;
    try
      // Find source values in TM
      for var Translation in Translations do
      begin

        Progress.AdvanceProgress;

        // Search TM for source terms
        LookupResult.Clear;
        if (not FTranslationMemory.FindTerms(SourceLanguage, Translation.Value.Value, LookupResult, True)) then
        begin
          Inc(Stats.Skipped);
          continue;
        end;

        // Add best match
        for var Result in LookupResult do
        begin
          var OneStats: TTranslationMemoryMergeStats;
          DuplicateAction := FTranslationMemory.Add(SourceLanguage, Result.SourceValue, TargetLanguage, Translation.Key, OneStats, DuplicateAction);

          Inc(Stats.Added, OneStats.Added);
          Inc(Stats.Merged, OneStats.Merged);
          Inc(Stats.Skipped, OneStats.Skipped);
          Inc(Stats.Duplicate, OneStats.Duplicate);

          // We're only interested in the first match
          break;
        end;

        if (DuplicateAction = tmDupActionAbort) or (Progress.Aborted) then
          break;
      end;

    finally
      LookupResult.Free;
    end;

    Progress.Hide;
  finally
    Translations.Free;
  end;


  QueueTranslationMemoryPeek;

  if (DuplicateAction = tmDupActionAbort) and (ElegibleCount = 1) then
    Exit; // Nothing to report

  TaskMessageDlg(sTranslationMemoryUpdateCompleteTitle,
    Format(sTranslationMemoryMergeComplete, [Stats.Added * 1.0, Stats.Merged * 1.0, Stats.Skipped * 1.0, Stats.Duplicate * 1.0]),
    mtInformation, [mbOK], 0);
end;

procedure TFormMain.ActionTranslationMemoryUpdateUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (HasSelection) and (SourceLanguage <> TargetLanguage) and (FTranslationMemory.IsAvailable);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.QueueToast(const Msg: string);
begin
  TimerToast.Enabled := False;
  FToastMessage := Msg;
  StatusBar.Panels[StatusBarPanelHint].Text := FToastMessage;
  TdxStatusBarTextPanelStyle(StatusBar.Panels[StatusBarPanelHint].PanelStyle).ImageIndex := ImageIndexInfo;
  TimerToast.Enabled := True;
end;

procedure TFormMain.DismissToast;
begin
  DismissToast(FToastMessage);
end;

procedure TFormMain.DismissToast(const Msg: string);
begin
  if (Msg = '') then
    Exit;

  if (StatusBar.Panels[StatusBarPanelHint].Text = Msg) then
    StatusBar.Panels[StatusBarPanelHint].Text := '';

  if (FToastMessage = Msg) then
  begin
    TdxStatusBarTextPanelStyle(StatusBar.Panels[StatusBarPanelHint].PanelStyle).ImageIndex := -1;
    FToastMessage := '';
    TimerToast.Enabled := False;
  end;
end;


procedure TFormMain.TimerToastTimer(Sender: TObject);
begin
  DismissToast;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.CreateTranslationMemoryPeeker(Force: boolean);
begin
  if (not TranslationManagerSettings.Providers.TranslationMemory.BackgroundQuery) or (TranslationManagerSettings.System.SafeMode) then
  begin
    FTranslationMemoryPeek := nil;
    Exit;
  end;

  if (FProject.Modules.Count > 0) and ((FTranslationMemoryPeek = nil) or (Force)) then
  begin
    FTranslationMemoryPeek := nil;
    FTranslationMemoryPeek := FTranslationMemory.CreateBackgroundLookup(SourceLanguageID, TargetLanguageID, TranslationMemoryPeekHandler);
    QueueTranslationMemoryPeek;
  end;
end;

procedure TFormMain.DisplayAutoApplyTranslationsStats(UpdatedSame, UpdatedSimilar: integer);
resourcestring
  sApplyTranslationResultIdentical = 'Applied translation to %.0n identical values';
  sApplyTranslationResultSimilar = 'Applied translation to %.0n similar values';
  sApplyTranslationResultBoth = 'Applied translation to %.0n identical and %.0n similar values';
begin
  if (UpdatedSame > 0) or (UpdatedSimilar > 0) then
  begin
    var Msg: string;
    if (UpdatedSame > 0) and (UpdatedSimilar = 0) then
      Msg := Format(sApplyTranslationResultIdentical, [UpdatedSame*1.0])
    else
    if (UpdatedSame = 0) and (UpdatedSimilar > 0) then
      Msg := Format(sApplyTranslationResultSimilar, [UpdatedSimilar*1.0])
    else
      Msg := Format(sApplyTranslationResultBoth, [UpdatedSame*1.0, UpdatedSimilar*1.0]);

    QueueToast(Msg); // TODO : Localization
  end;
end;

procedure TFormMain.TranslationAdded(AProp: TLocalizerProperty);
begin
  var AutoApplyTranslations := TranslationManagerSettings.Editor.AutoApplyTranslations;
  var AutoApplyTranslationsSimilar := TranslationManagerSettings.Editor.AutoApplyTranslationsSimilar;
  var UpdatedSame := 0;
  var UpdatedSimilar := 0;

  TranslationAdded(AProp, AutoApplyTranslations, AutoApplyTranslationsSimilar, UpdatedSame, UpdatedSimilar);

  DisplayAutoApplyTranslationsStats(UpdatedSame, UpdatedSimilar);
end;

procedure TFormMain.TranslationAdded(AProp: TLocalizerProperty; var AutoApplyTranslations, AutoApplyTranslationsSimilar: TTranslationAutoApply; var UpdatedSame, UpdatedSimilar: integer);
var
  SourceValue, TranslatedValue: string;
  PropertyList: TLocalizerPropertyList;
  Prop: TLocalizerProperty;
resourcestring
  sApplyTranslationCaption = 'Apply translation?';
  sApplyTranslationTitle = 'Apply the same translation to the following identical value?';
  sApplySimilarTranslationTitle = 'Apply the same translation to the following similar value?';
  sApplyTranslationText =
    '  Module: %s'#13+
    '  Element: %s'#13+
    '  Property: %s'#13+
    '  Value: %s'#13#13+
    '  Old translation: %s'#13+
    '  New translation: %s'#13#13+
    '(%.0n of %.0n)';
  sApplyTranslationButtonAll = 'Use this choice for all';
begin
  (*
  ** TODO :
  ** 1. Check if translation doesn't exist in TM and
  **    a) Other identical translations of same term exist.
  **    b) Term exist in TM but with other translation.
  ** 2. Suggest to user that translation be added to TM:
  **    "The translation you just made appears to be common.
  **     It is suggested that you add this translation to the Translation Memory
  **     so it can be reused for future translations."
  **
  *)

  if (AutoApplyTranslations = aaNever) or (not AProp.HasTranslation(TranslationLanguage)) then
    Exit;

  SourceValue := AProp.Value;
  TranslatedValue := AProp.TranslatedValue[TranslationLanguage];

  // Ignore empty source or target
  if (SourceValue.Trim.IsEmpty) or (TranslatedValue.Trim.IsEmpty) then
    Exit;

  SaveCursor(crAppStart);

  // Get a list of properties with "similar" source values
  PropertyList := FProjectIndex.Lookup(AProp);

  if (PropertyList = nil) then
    Exit;

  // Count props so we can prompt
  var CountSame := 0;
  var CountSimilar := 0;
  for Prop in PropertyList do
  begin
    if (Prop = AProp) then
      continue;

    if (Prop.EffectiveStatus <> ItemStatusTranslate) then
      continue;

    if (TranslationManagerSettings.Editor.AutoApplyTranslationsExisting) or (not Prop.HasTranslation(TranslationLanguage)) then
    begin
      if (SourceValue = Prop.Value) then
        Inc(CountSame)
      else
        Inc(CountSimilar);
    end;
  end;

  var IndexSame := 0;
  var IndexSimilar := 0;

  for Prop in PropertyList do
  begin
    if (Prop = AProp) then
      continue;

    if (Prop.EffectiveStatus <> ItemStatusTranslate) then
      continue;

    var HasTranslation := Prop.HasTranslation(TranslationLanguage);
    if (TranslationManagerSettings.Editor.AutoApplyTranslationsExisting) or (not HasTranslation) then
    begin
      if (SourceValue = Prop.Value) then
      begin
        Inc(IndexSame);

        var OldTranslation := '';
        if (HasTranslation) then
          OldTranslation := Prop.TranslatedValue[TranslationLanguage];

        var Apply := (not HasTranslation) or (OldTranslation <> TranslatedValue);

        // Prompt to translate those that already has a (maybe incorrect) translation
        if (Apply) and (AutoApplyTranslations = aaPrompt) then
        begin
          var TaskDialog := TTaskDialog.Create(nil);
          try
            TaskDialog.Caption := sApplyTranslationCaption;
            TaskDialog.Title := sApplyTranslationTitle;
            TaskDialog.Text := Format(sApplyTranslationText, [Prop.Item.Module.Name, Prop.Item.Name, Prop.Name, Prop.Value, OldTranslation, TranslatedValue, IndexSame*1.0, CountSame*1.0]);
            TaskDialog.CommonButtons := [tcbYes, tcbNo, tcbCancel];
            TaskDialog.VerificationText := sApplyTranslationButtonAll;

            TaskDialog.Execute;

            case TaskDialog.ModalResult of
              mrYes:
                begin
                  Apply := True;
                  if (tfVerificationFlagChecked in TaskDialog.Flags) then
                    AutoApplyTranslations := aaAlways
                end;

              mrNo:
                begin
                  Apply := False;
                  if (tfVerificationFlagChecked in TaskDialog.Flags) then
                    AutoApplyTranslations := aaNever;
                end;
            else
              AutoApplyTranslations := aaNever;
              AutoApplyTranslationsSimilar := aaNever;
              break;
            end;
          finally
            TaskDialog.Free;
          end;
        end;

        if (Apply) then
        begin
          Prop.TranslatedValue[TranslationLanguage] := TranslatedValue;
          Inc(UpdatedSame);
        end;
      end else
      if (AutoApplyTranslationsSimilar <> aaNever) then
      begin
        Inc(IndexSimilar);

        var OldTranslation := '';
        if (HasTranslation) then
          OldTranslation := Prop.TranslatedValue[TranslationLanguage];
        var NewTranslation := MakeAlike(Prop.Value, TranslatedValue);

        var Apply := (not HasTranslation) or (OldTranslation <> NewTranslation);

        // Prompt to translate those that already has a (maybe incorrect) translation
        if (Apply) and (AutoApplyTranslationsSimilar = aaPrompt) then
        begin

          var TaskDialog := TTaskDialog.Create(nil);
          try
            TaskDialog.Caption := sApplyTranslationCaption;
            TaskDialog.Title := sApplySimilarTranslationTitle;
            TaskDialog.Text := Format(sApplyTranslationText, [Prop.Item.Module.Name, Prop.Item.Name, Prop.Name, Prop.Value, OldTranslation, NewTranslation, IndexSimilar*1.0, CountSimilar*1.0]);
            TaskDialog.CommonButtons := [tcbYes, tcbNo, tcbCancel];
            TaskDialog.VerificationText := sApplyTranslationButtonAll;

            TaskDialog.Execute;

            case TaskDialog.ModalResult of
              mrYes:
                begin
                  Apply := True;
                  if (tfVerificationFlagChecked in TaskDialog.Flags) then
                    AutoApplyTranslationsSimilar := aaAlways
                end;

              mrNo:
                begin
                  Apply := False;
                  if (tfVerificationFlagChecked in TaskDialog.Flags) then
                    AutoApplyTranslationsSimilar := aaNever;
                end;
            else
              AutoApplyTranslations := aaNever;
              AutoApplyTranslationsSimilar := aaNever;
              break;
            end;
          finally
            TaskDialog.Free;
          end;
        end;

        if (Apply) then
        begin
          Prop.TranslatedValue[TranslationLanguage] := NewTranslation;
          Inc(UpdatedSimilar);
        end;
      end else
        continue;

      ReloadProperty(Prop);
    end;
  end;

  ReloadProperty(AProp);
end;

procedure TFormMain.TranslationMemoryPeekHandler(Sender: TObject);
var
  RecordIndex, RowIndex: integer;
begin
  RecordIndex := FModuleItemsDataSource.IndexOfProperty(TLocalizerProperty(Sender));
  if (RecordIndex = -1) then
    Exit;

  FModuleItemsDataSource.PeekResult[RecordIndex] := TTranslationMemoryPeekResult.prFound;

  RowIndex := GridItemsTableView.DataController.GetRowIndexByRecordIndex(RecordIndex, False);
  if (RowIndex <> -1) then
    GridItemsTableView.ViewData.Rows[RowIndex].Invalidate(GridItemsTableViewColumnTarget);
end;

procedure TFormMain.QueueTranslationMemoryPeek;
var
  i: integer;
begin
  if (FTranslationMemoryPeek = nil) then
    Exit;

  // Reset state of all - queue those that are visible right now

  ClearTranslationMemoryPeekResult;

  for i := 0 to GridItemsTableView.ViewData.RowCount-1 do
    QueueTranslationMemoryPeek(GridItemsTableView.ViewData.Rows[i].RecordIndex, True);
end;

procedure TFormMain.QueueTranslationMemoryPeek(RecordIndex: integer; Force: boolean);
var
  Prop: TLocalizerProperty;
  RowIndex: integer;
begin
  if (FTranslationMemoryPeek = nil) then
    Exit;

  if (not Force) and (FModuleItemsDataSource.PeekResult[RecordIndex] <> TTranslationMemoryPeekResult.prNone) then
    Exit;

  Prop := FModuleItemsDataSource.Properties[RecordIndex];

  if (Prop = nil) or (Prop.IsUnused) or (Prop.EffectiveStatus <> ItemStatusTranslate) or (Prop.HasTranslation(TranslationLanguage)) then
    Exit;

  if (FModuleItemsDataSource.PeekResult[RecordIndex] = TTranslationMemoryPeekResult.prFound) then
  begin
    RowIndex := GridItemsTableView.DataController.GetRowIndexByRecordIndex(RecordIndex, False);
    if (RowIndex <> -1) then
      GridItemsTableView.ViewData.Rows[RowIndex].Invalidate(GridItemsTableViewColumnTarget);
  end;

  FModuleItemsDataSource.PeekResult[RecordIndex] := TTranslationMemoryPeekResult.prQueued;
  FTranslationMemoryPeek.EnqueueQuery(Prop);
end;

procedure TFormMain.QueueTranslationMemoryPeek(Prop: TLocalizerProperty; Force: boolean);
var
  RecordIndex: integer;
begin
  if (FTranslationMemoryPeek = nil) then
    Exit;

  RecordIndex := FModuleItemsDataSource.IndexOfProperty(Prop);

  if (RecordIndex <> -1) then
    QueueTranslationMemoryPeek(RecordIndex, Force);
end;

procedure TFormMain.ClearTranslationMemoryPeekResult;
var
  i: integer;
begin
  // Reset state of all
  for i := 0 to FModuleItemsDataSource.RecordCount-1 do
    FModuleItemsDataSource.PeekResult[i] := TTranslationMemoryPeekResult.prNone;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionAutomationTranslateExecute(Sender: TObject);
begin
  BarButtonAutoTranslate.DropDown(True);
end;

procedure TFormMain.ActionAutomationTranslateUpdate(Sender: TObject);
var
  Item: TCustomLocalizerItem;
begin
  Item := FocusedItem;

  TAction(Sender).Enabled := (Item <> nil) and (not Item.IsUnused) and (Item.EffectiveStatus <> ItemStatusDontTranslate) and
    (SourceLanguage <> TargetLanguage);
end;

procedure TFormMain.OnTranslationProviderHandler(Sender: TObject);
var
  TranslationProvider: ITranslationProvider;
  TranslationService: ITranslationService;
begin
  TranslationProvider := TranslationProviderRegistry.CreateProvider(TdxBarButton(Sender).Tag);
  try
    TranslationService := CreateTranslationService(TranslationProvider);
    try

      // It would be better if we delegated the task of providing a name to the service but for
      // now we have to pass the provider along so the dialogs and progress can display the
      // name of the provider.
      // When we implement support for multi-provider lookup this will have to change.

      TranslateSelected(TranslationService, TranslationProvider);

    finally
      TranslationService := nil;
    end;
  finally
    TranslationProvider := nil;
  end;
end;

procedure TFormMain.PopupMenuTranslateProvidersPopup(Sender: TObject);
var
  i: integer;
  Provider: TranslationProviderRegistry.TProvider;
  ItemLink: TdxBarItemLink;
begin
  // Clear all existing items and repopulate
  for i := TdxRibbonPopupMenu(Sender).ItemLinks.Count-1 downto 0 do
    TdxRibbonPopupMenu(Sender).ItemLinks[i].Item.Free;

  for Provider in TranslationProviderRegistry(nil) do // Hack! enumerator not allowed on meta types
  begin
    ItemLink := TdxRibbonPopupMenu(Sender).ItemLinks.AddButton;
    TdxBarButton(ItemLink.Item).Caption := Provider.ProviderName;
    TdxBarButton(ItemLink.Item).Tag := Provider.Handle;
    TdxBarButton(ItemLink.Item).OnClick := OnTranslationProviderHandler;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.PopupMenuValidationWarningPopup(Sender: TObject);
begin
  for var i := PopupMenuValidationWarning.ItemLinks.Count-1 downto 0 do
    if (PopupMenuValidationWarning.ItemLinks[i].Item is TdxBarStatic) then
      PopupMenuValidationWarning.ItemLinks[i].Item.Free;

  var Index := 1;
  for var Warning in FocusedProperty.Translations[TranslationLanguage].Warnings do
  begin
    var ItemLink := PopupMenuValidationWarning.ItemLinks.AddItem(TdxBarStatic);
    var Item := ItemLink.Item as TdxBarStatic;
    Item.Caption := LoadResString(sTranslationValidationWarnings[Warning]);
    ItemLink.Index := Index;
    Inc(Index);
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionBookmarkExecute(Sender: TObject);
var
  Prop: TLocalizerProperty;
  Flag: TPropertyFlag;
  Props: TArray<TLocalizerProperty>;
  i: integer;
  DoSet: boolean;
begin
  FLastGotoAction := TAction(Sender);

  Flag := TPropertyFlag(TAction(Sender).Tag);

  FLastBookmark := Ord(Flag);

  // If the any bookmark action is visible then we have been invoked from a Goto bookmark action.
  // Otherwise it must be from a Set boomark action.
  if (ActionGotoBookmarkAny.Visible) then
  begin
    GotoNext(
      function(Prop: TLocalizerProperty): boolean
      begin
        Result := (Flag in Prop.Flags);
      end, True, Flag in [FlagBookmark0..FlagBookmark9]);
  end else
  begin
    if (ActiveProperty = nil) then
      exit;

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
        LoadFocusedProperty;
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
            ReloadProperty(Prop);
          end;
        end);

      // ...Then set new (on single item)
      Prop.SetFlag(Flag);
      LoadFocusedProperty;
    end else
    begin
      // Operate on selected nodes
      SetLength(Props, GridItemsTableView.Controller.SelectedRecordCount);
      for i := 0 to GridItemsTableView.Controller.SelectedRecordCount-1 do
        Props[i] := FModuleItemsDataSource.Properties[GridItemsTableView.Controller.SelectedRecords[i].RecordIndex];

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

      for i := 0 to GridItemsTableView.Controller.SelectedRowCount -1 do
        GridItemsTableView.Controller.SelectedRows[i].Invalidate;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionBuildExecute(Sender: TObject);
begin
  BarButtonBuildProject.DropDown(True);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionImportCSVExecute(Sender: TObject);
begin
  var FormCSVImport := TFormCSVImport.Create(nil);
  try

    if (TranslationLanguage.TranslatedCount > 0) then
    begin
      var s := TaskDialogImportUpdate.Text;
      try
        TaskDialogImportUpdate.Text := Format(s, [TranslationLanguage.TranslatedCount*1.0, TargetLanguage.LanguageName]);

        if (not TaskDialogImportUpdate.Execute) then
          Exit;
      finally
        TaskDialogImportUpdate.Text := s;
      end;
      FormCSVImport.UpdateExisting := (TaskDialogImportUpdate.ModalResult = 101);
    end else
      FormCSVImport.UpdateExisting := True;

    FormCSVImport.Execute(FProject);
  finally
    FormCSVImport.Free;
  end;
end;

procedure TFormMain.ActionImportFileExecute(Sender: TObject);
begin
//
end;

procedure TFormMain.ActionImportFileSourceExecute(Sender: TObject);
var
  Filename: string;
  SaveSourceFilename, SaveSymbolFilename: string;
  SymbolFilename: string;
  ProjectProcessor: TProjectResourceProcessor;
  CountBefore, CountAfter: TCounts;
  Msg: string;
resourcestring
  sSwitchSourceModuleTitle = 'Update project?';
  sSwitchSourceModule = 'Do you want to alter the project to use this file as the source file?';
begin
  SaveCursor(crAppStart); // Open dialog can take a while to appear

  Filename := OpenDialogEXE.FileName;
  if (Filename <> '') then
  begin
    OpenDialogEXE.InitialDir := TPath.GetDirectoryName(Filename);
    OpenDialogEXE.FileName := TPath.ChangeExtension(TPath.GetFileName(Filename), '.exe');
  end;

  if (not OpenDialogEXE.Execute(Handle)) then
    Exit;

  Filename := OpenDialogEXE.FileName;

  // Temporarily switch to new file or we will not be able to find the companion files (*.drc)
  SaveSourceFilename := FProject.SourceFilename;
  SaveSymbolFilename := FProject.StringSymbolFilename;
  try
    FProject.SourceFilename := Filename;
    FProject.StringSymbolFilename := TPath.ChangeExtension(Filename, TranslationManagerShell.sFileTypeStringSymbols);

    if (not CheckStringsSymbolFile(True)) then
      Exit;

    SaveCursor(crHourGlass);

    CountBefore := CountStuff;

    ProjectProcessor := TProjectResourceProcessor.Create;
    try
      try

        ProjectProcessor.ScanProject(FProject, Filename);

      except
        on E: EResourceProcessor do
        begin
          TaskMessageDlg(sErrorLoadingModuleTitle, E.Message, mtWarning, [mbOK], 0);
          Exit;
        end;
      end;
    finally
      ProjectProcessor.Free;
    end;

    SymbolFilename := FProject.StringSymbolFilename;

  finally
    FProject.StringSymbolFilename := SaveSymbolFilename;
    FProject.SourceFilename := SaveSourceFilename;
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
      1.0*(CountAfter.CountSynthesized-CountBefore.CountSynthesized),
      1.0*(CountAfter.ObsoleteTranslation-CountBefore.ObsoleteTranslation)
      ])
  else
    Msg := sProjectUpdatedNothing;

  if (not AnsiSameText(PathUtil.Canonicalise(FProject.SourceFilename), PathUtil.Canonicalise(Filename))) then
  begin
    Msg := Msg + #13#13 + sSwitchSourceModule;

    if (TaskMessageDlg(sProjectUpdatedTitle, Msg, mtConfirmation, [mbYes, mbNo], 0, mbNo) = mrYes) then
    begin
      FProject.SourceFilename := Filename;
      FProject.StringSymbolFilename := SymbolFilename;

      RibbonMain.DocumentName := TPath.GetFileNameWithoutExtension(Filename);
      ResetTracker;

      FProject.Changed;
    end;
  end else
    TaskMessageDlg(sProjectUpdatedTitle, Msg, mtInformation, [mbOK], 0);
end;

procedure TFormMain.ActionImportFileTargetExecute(Sender: TObject);
var
  Filename: string;
  ProjectProcessor: TProjectResourceProcessor;
  Stats: TTranslationCounts;
  SaveFilter: string;
begin
  SaveCursor(crAppStart); // Open dialog can take a while to appear

  SaveFilter := OpenDialogEXE.Filter;
  try

    OpenDialogEXE.Filter := Format(sResourceModuleFilter, [TargetLanguage.LanguageName, TargetLanguage.ISO639_1Name+'*']) + SaveFilter;
    OpenDialogEXE.FilterIndex := 1;

    Filename := OpenDialogEXE.FileName;
    if (Filename <> '') then
    begin
      OpenDialogEXE.InitialDir := TPath.GetDirectoryName(Filename);
      OpenDialogEXE.FileName := LocalizationTools.BuildModuleFilename(TPath.GetFileName(Filename), TargetLanguage.Locale, TranslationManagerSettings.System.ModuleNameScheme);
    end;

    if (not OpenDialogEXE.Execute(Handle)) then
      Exit;

  finally
    OpenDialogEXE.Filter := SaveFilter;
  end;

  SaveCursor(crHourGlass);

  ProjectProcessor := TProjectResourceProcessor.Create;
  try
    try

      ProjectProcessor.Execute(liaUpdateTarget, FProject, OpenDialogEXE.FileName, TranslationLanguage, nil);

    except
      on E: EResourceProcessor do
      begin
        TaskMessageDlg(sErrorLoadingModuleTitle, E.Message, mtWarning, [mbOK], 0);
        Exit;
      end;
    end;

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
  TAction(Sender).Enabled := (FProject.Modules.Count > 0) and (SourceLanguage <> TargetLanguage);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionImportPOExecute(Sender: TObject);
var
  Stream: TStream;
  ProgressStream: TStream;
  Progress: IProgress;
  POImport: TLocalizerPOImport;
  Filename, Folder: string;
  s: string;
  UpdateExisting: boolean;
resourcestring
  sPOImportProgress = 'Importing GNU GetText translations';
  sPOImportCompleteTitle = 'Import completed';
  sPOImportComplete = '%.0n translations were applied to the current project.'#13#13+
    'Exact matches: %.0n'#13+
    'Fuzzy matches: %.0n'#13+
    'Skipped values: %.0n'#13+
    'Not found: %.0n'#13+
    'Invalid values: %.0n';
begin
  Filename := TPath.ChangeExtension(FProject.SourceFilename, '.po');
  Folder := TPath.GetDirectoryName(Filename);
  Filename := TPath.GetFileName(Filename);

  if (not PromptForFileName(Filename, sFileFilterPO, 'po', '', Folder)) then
    Exit;

  if (TranslationLanguage.TranslatedCount > 0) then
  begin
    s := TaskDialogImportUpdate.Text;
    try
      TaskDialogImportUpdate.Text := Format(s, [TranslationLanguage.TranslatedCount*1.0, TargetLanguage.LanguageName]);

      if (not TaskDialogImportUpdate.Execute) then
        Exit;
    finally
      TaskDialogImportUpdate.Text := s;
    end;
    UpdateExisting := (TaskDialogImportUpdate.ModalResult = 101);
  end else
    UpdateExisting := True;

  POImport := TLocalizerPOImport.Create(FProject);
  try
    POImport.UpdateExisting := UpdateExisting;
    FProject.BeginUpdate;
    try

      Stream := TFileStream.Create(Filename, fmOpenRead);
      try
        Progress := ShowProgress(sPOImportProgress);
        Progress.EnableAbort := True;
        Progress.RaiseOnAbort := True;

        ProgressStream := TProgressStream.Create(Stream, Progress);
        try
          try

            POImport.ImportFromStream(ProgressStream, TranslationLanguage);

          except
            on E: EAbort do
              ; // Ignore
          end;
        finally
          ProgressStream.Free;
        end;

        Progress.Hide;
        Progress := nil;
      finally
        Stream.Free;
      end;

    finally
      FProject.EndUpdate;
    end;

    TaskMessageDlg(sPOImportCompleteTitle,
      Format(sPOImportComplete, [POImport.CountImported * 1.0, POImport.CountExact * 1.0, POImport.CountFuzzy * 1.0,
        POImport.CountSkipped * 1.0, POImport.CountNotFound * 1.0, POImport.CountIgnored * 1.0]),
      mtInformation, [mbOK], 0);

  finally
    POImport.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionTranslationSuggestionListExecute(Sender: TObject);
begin
  // We must drop down combo manually if we're invoked via the shortcut.
  // The action has a shortcut because we would like to display it in the hint.
  if (TAction(Sender).ActionComponent = nil) then
  begin
    if (not GridItemsTableViewColumnTarget.Editing) then
      GridItemsTableViewColumnTarget.Editing := True;

    if (GridItemsTableView.Controller.EditingController.Edit = nil) then
      Exit;

    TcxCustomDropDownEdit(GridItemsTableView.Controller.EditingController.Edit).DroppedDown := not TcxCustomDropDownEdit(GridItemsTableView.Controller.EditingController.Edit).DroppedDown;
  end;

  // ActionComponent is only set when we are fired via the dropdown button but it
  // is never reset, so we do that here.
  TAction(Sender).ActionComponent := nil;
end;

procedure TFormMain.ActionTranslationSuggestionListUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (IsPropertyActive);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ApplyTranslationTextEdit(EndEdit: boolean);
begin
  if (not FTextEditing) then
    Exit;

  FTextEditing := False;

  PostTranslationTextEdit;

  // Moving focus out of edit control will also apply the edit (via the OnExit handler)
  if (EndEdit) and (EditTargetText.Focused) then
    TreeListModules.SetFocus;
end;

procedure TFormMain.PostTranslationTextEdit;
var
  RecordIndex: integer;
begin
  if (not FTextEditModified) then
    Exit;

  Assert(FTextEditProperty <> nil);

  // Edit without explicit post - write value directly back to property and refresh row

  FTextEditModified := False;

  RecordIndex := FModuleItemsDataSource.IndexOfProperty(FTextEditProperty);

  // If we are posting because the focus has moved to another module, then we will not find
  // the property in the data source and will have to update it directly.
  if (RecordIndex <> -1) then
    GridItemsTableView.DataController.Values[RecordIndex, GridItemsTableViewColumnTarget.Index] := EditTargetText.Lines.Text
  else
  begin
    FTextEditProperty.TranslatedValue[TranslationLanguage] := EditTargetText.Lines.Text;
    TranslationAdded(FTextEditProperty);
  end;
end;

procedure TFormMain.EditTargetTextEnter(Sender: TObject);
begin
  FTextEditProperty := FocusedProperty;
  FTextEditing := True;
end;

procedure TFormMain.EditTargetTextExit(Sender: TObject);
begin
  FTextEditing := False;
  PostTranslationTextEdit;
  FTextEditProperty := nil;
end;

procedure TFormMain.EditTargetTextPropertiesEditValueChanged(Sender: TObject);
begin
  if (FTextEditing) then
    FTextEditModified := True;
end;

procedure TFormMain.ActionEditTranslationTextExecute(Sender: TObject);
(*
var
  TextEditor: TFormTextEditor;
*)
var
  SelStart: integer;
begin
  // Place caret at end by default
  SelStart := MaxInt;

  // Open text editor if grid has focus. Otherwise post changes and close text editor.
  if (IsPropertyActive) then
  begin
    if (GridItemsTableViewColumnTarget.Editing) then
    begin
      // Get current caret position if in-place editor is active
      SelStart := TcxTextEdit(GridItemsTableView.Controller.EditingController.Edit).SelStart;

      // Save current edit text and then cancel edit. We will continue the edit in the text editor instead.
      // The modified state is transferred to the text editor.
      // This enables the user to cancel the edit even if they invoke the text editor.
      EditTargetText.Text := TcxCustomTextEdit(GridItemsTableView.Controller.EditingController.Edit).EditingText;
      FTextEditModified := TcxCustomTextEdit(GridItemsTableView.Controller.EditingController.Edit).ModifiedAfterEnter;
      GridItemsTableView.Controller.EditingController.HideEdit(False);
    end;

    // Restore caret position
    EditTargetText.SelStart := SelStart;

    SplitterMainEditors.OpenSplitter;
    EditTargetText.SetFocus;
  end else
  if (EditTargetText.Focused) then
  begin
    // Save caret position
    SelStart := EditTargetText.SelStart;

    // Clear modified flag to avoid OnExit posting value
    FTextEditModified := False;
    FTextEditProperty := nil;

    // Move focus to grid and continue edit in in-place editor
    GridItems.SetFocus;
    GridItemsTableViewColumnTarget.Editing := True;

    if (GridItemsTableView.Controller.EditingController.Edit = nil) then
      Exit;

    // Write new value back to inner edit control. The OnChange event will occur as normally when the user exits the cell.
    // Note that the following three methods must be called in this exact order.
    GridItemsTableView.Controller.EditingController.Edit.DoEditing;
    TcxCustomEditCracker(GridItemsTableView.Controller.EditingController.Edit).InnerEdit.EditValue := EditTargetText.Text;
    GridItemsTableView.Controller.EditingController.Edit.ModifiedAfterEnter := True;

    // Restore caret position
    TcxTextEdit(GridItemsTableView.Controller.EditingController.Edit).SelStart := SelStart;

    SplitterMainEditors.CloseSplitter;
  end;

(*
  TextEditor := TFormTextEditor.Create(nil);
  try
    TextEditor.SourceText := FocusedProperty.Value;
    TextEditor.Text := TcxCustomTextEdit(GridItemsTableView.Controller.EditingController.Edit).EditingText;
    TextEditor.SourceLanguage := SourceLanguage;
    TextEditor.TargetLanguage := TargetLanguage;

    if (TextEditor.Execute) then
    begin
      // Write new value back to inner edit control. The OnChange event will occur as normally when the user exits the cell.
      // Note that the following three metyhods must be called in this exact order.
      GridItemsTableView.Controller.EditingController.Edit.DoEditing;
      TcxCustomEditCracker(GridItemsTableView.Controller.EditingController.Edit).InnerEdit.EditValue := TextEditor.Text;
      GridItemsTableView.Controller.EditingController.Edit.ModifiedAfterEnter := True;
    end;
  finally
    TextEditor.Free;
  end;
*)
end;

procedure TFormMain.ActionEditTranslationTextUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FTextEditing) or (IsPropertyActive);
end;

procedure TFormMain.ActionTextEditFocusedUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FTextEditing);
end;

procedure TFormMain.ActionTextEditCancelExecute(Sender: TObject);
begin
  if (FTextEditModified) then
  begin
    // Restore original value and clear modified flag
    EditTargetText.Lines.Text := GridItemsTableView.DataController.Values[GridItemsTableView.DataController.FocusedRecordIndex, GridItemsTableViewColumnTarget.Index];
    FTextEditModified := False;
  end else
    // Escape with no modifications closes the editor
    ActionEditTranslationText.Execute;
end;

procedure TFormMain.ActionTextEditNextExecute(Sender: TObject);
begin
  GridItemsTableView.Controller.ClearSelection;
  GridItemsTableView.Controller.FocusedRowIndex := GridItemsTableView.Controller.FocusedRowIndex + 1;
  GridItemsTableView.Controller.FocusedRow.Selected := True;
end;

procedure TFormMain.ActionTextEditNextUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FTextEditing) and (GridItemsTableView.Controller.FocusedRowIndex < GridItemsTableView.DataController.RowCount-1);
end;

procedure TFormMain.ActionTextEditPreviousExecute(Sender: TObject);
begin
  GridItemsTableView.Controller.ClearSelection;
  GridItemsTableView.Controller.FocusedRowIndex := GridItemsTableView.Controller.FocusedRowIndex - 1;
  GridItemsTableView.Controller.FocusedRow.Selected := True;
end;

procedure TFormMain.ActionTextEditPreviousUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FTextEditing) and (GridItemsTableView.Controller.FocusedRowIndex > 0);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionExportCSVExecute(Sender: TObject);
var
  Filename: string;
  Writer: TTextWriter;
  CsvWriter: TLocalizerCsvWriter;
begin
  if (FProjectFilename <> '') then
    Filename := FProjectFilename
  else
    Filename := FProject.SourceFilename;

  Filename := TPath.ChangeExtension(TPath.GetFileName(Filename), '.csv');

  if (not PromptForFileName(Filename, sFileFilterCSV, 'csv', '', TranslationManagerSettings.Folders.FolderDocuments, True)) then
    Exit;

  Writer := TStreamWriter.Create(Filename, False, TEncoding.UTF8);
  try
    CsvWriter := TLocalizerCsvWriter.Create(Writer, True);
    try

      CsvWriter.Write(FProject);

    finally
      CsvWriter.Free;
    end;
  finally
    Writer.Free;
  end;
end;

procedure TFormMain.ActionEditCopyExecute(Sender: TObject);
var
  Writer: TTextWriter;
  CsvWriter: TLocalizerCsvWriter;
  i: integer;
  Item: TCustomLocalizerItem;
begin
  Writer := TStringWriter.Create;
  try
    CsvWriter := TLocalizerCsvWriter.Create(Writer, True, TranslationLanguage);
    try
      // Use [Tab] as delimiter for the clipboard so Excel can understand the format
      // without the user having to go through the Text Import sttings.
      CsvWriter.Delimiter := #9;

      for i := 0 to SelectionCount-1 do
      begin
        Item := Selection[i];
        if (Item is TLocalizerModule) then
          CsvWriter.Write(TLocalizerModule(Item))
        else
        if (Item is TLocalizerProperty) then
          CsvWriter.Write(TLocalizerProperty(Item));
      end;

    finally
      CsvWriter.Free;
    end;

    Clipboard.AsText := Writer.ToString;
  finally
    Writer.Free;
  end;
end;

procedure TFormMain.ActionEditCopyUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FProject.Modules.Count > 0) and (HasSelection);
end;

procedure TFormMain.ActionEditPasteExecute(Sender: TObject);
begin
  // TODO
  // Paste translations from clipboard
  // Format is assumed to match Copy format
  // Module/Item/Prop is located using names. Translated value is imported.
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionClearBookmarksExecute(Sender: TObject);
resourcestring
  sClearBookmarks = 'Clear all bookmarks in project?';
begin
  if (MessageDlg(sClearBookmarks, mtConfirmation, [mbYes, mbNo], 0, mbNo) <> mrYes) then
    Exit;

  FProject.BeginUpdate;
  try
    FProject.Traverse(
      function(Prop: TLocalizerProperty): boolean
      begin
        if (Prop.Flags * [FlagBookmark0..FlagBookmarkF] <> []) then
        begin
          Prop.Flags := Prop.Flags - [FlagBookmark0..FlagBookmarkF];
          ReloadProperty(Prop);
        end;

        Result := True;
      end);
  finally
    FProject.EndUpdate;
  end;
end;

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
  TAction(Sender).Enabled := (ActiveProperty <> nil);

  TAction(Sender).Checked := TAction(Sender).Enabled and (FocusedProperty.Flags * [FlagBookmark0..FlagBookmarkF] <> []);
  // We have to manually sync the button because bsChecked always behave like AutoCheck=True (and DevExpress won't admit they've got it wrong)
  ButtonItemBookmark.Down := TAction(Sender).Checked;
end;

procedure TFormMain.ActionGotoAgainExecute(Sender: TObject);
begin
  FLastGotoAction.Execute;
end;

procedure TFormMain.ActionGotoAgainUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FLastGotoAction <> nil) and (FLastGotoAction.Enabled);
end;

procedure TFormMain.ActionGotoBookmarkAnyExecute(Sender: TObject);
begin
  FLastGotoAction := TAction(Sender);
  FLastBookmark := -1;
  GotoNext(
    function(Prop: TLocalizerProperty): boolean
    begin
      Result := (Prop.Flags * [FlagBookmark0..FlagBookmarkF] <> []);
    end);
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

procedure TFormMain.ActionGotoNextStateExistingExecute(Sender: TObject);
begin
  FLastGotoAction := TAction(Sender);
  GotoNext(
    function(Prop: TLocalizerProperty): boolean
    begin
      Result := (Prop.State * [ItemStateNew, ItemStateUnused] = []);
    end);
end;

procedure TFormMain.ActionGotoNextStateNewExecute(Sender: TObject);
begin
  FLastGotoAction := TAction(Sender);
  GotoNext(
    function(Prop: TLocalizerProperty): boolean
    begin
      Result := (ItemStateNew in Prop.State);
    end);
end;

procedure TFormMain.ActionGotoNextStateUnusedExecute(Sender: TObject);
begin
  FLastGotoAction := TAction(Sender);
  GotoNext(
    function(Prop: TLocalizerProperty): boolean
    begin
      Result := (ItemStateUnused in Prop.State);
    end);
end;

procedure TFormMain.ActionGotoNextStatusExecute(Sender: TObject);
begin
  FLastGotoAction := TAction(Sender);
  GotoNext(
    function(Prop: TLocalizerProperty): boolean
    begin
      Result := (Prop.EffectiveStatus = TLocalizerItemStatus(TAction(Sender).Tag));
    end);
end;

procedure TFormMain.ActionGotoNextUntranslatedExecute(Sender: TObject);
begin
  FLastGotoAction := TAction(Sender);
  GotoNext(
    function(Prop: TLocalizerProperty): boolean
    var
      Translation: TLocalizerTranslation;
    begin
      if (Prop.EffectiveStatus <> ItemStatusTranslate) or (Prop.IsUnused) then
        Exit(False);

      Result := (not Prop.Translations.TryGetTranslation(TranslationLanguage, Translation)) or (Translation.Status = tStatusPending);
    end);
end;

procedure TFormMain.ActionGotoNextWarningExecute(Sender: TObject);
begin
  FLastGotoAction := TAction(Sender);
  GotoNext(
    function(Prop: TLocalizerProperty): boolean
    var
      Translation: TLocalizerTranslation;
    begin
      if (Prop.EffectiveStatus <> ItemStatusTranslate) or (Prop.IsUnused) then
        Exit(False);

      Result := (Prop.Translations.TryGetTranslation(TranslationLanguage, Translation)) and (Translation.Warnings <> []);
    end, True, (Sender = nil)); // Goto first if we're invoked from the status bar
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionFeedbackHideExecute(Sender: TObject);
begin
  BarButtonFeedback.Visible := ivInCustomizing;
end;

procedure TFormMain.ActionFeedbackNegativeExecute(Sender: TObject);
{$ifdef MADEXCEPT}
var
  FormFeedback: TFormFeedback;
{$endif MADEXCEPT}
begin
{$ifdef MADEXCEPT}
  FormFeedback := TFormFeedback.Create(nil);
  try
    FormFeedback.Execute(FeedbackNegative);
  finally
    FormFeedback.Free;
  end;
{$endif MADEXCEPT}
end;

procedure TFormMain.ActionFeedbackPositiveExecute(Sender: TObject);
{$ifdef MADEXCEPT}
var
  FormFeedback: TFormFeedback;
{$endif MADEXCEPT}
begin
{$ifdef MADEXCEPT}
  FormFeedback := TFormFeedback.Create(nil);
  try

    FormFeedback.Execute(FeedbackPositive);

  finally
    FormFeedback.Free;
  end;
{$endif MADEXCEPT}
end;

// -----------------------------------------------------------------------------

procedure TFormMain.RibbonGalleryItemStopListPopup(Sender: TObject);
var
  i: integer;
  StopListField: TStopListField;
  Value: string;
begin
  for i := 0 to RibbonGalleryItemGroup.Items.Count-1 do
  begin
    RibbonGalleryItemGroup.Items[i].Selected := False;

    StopListField := TStopListField(RibbonGalleryItemGroup.Items[i].Action.Tag);

    if (IsPropertyActive) then
      Value := TStopListItem.ExtractValue(StopListField, FocusedProperty)
    else
    if (StopListField = slFieldModule) then
      Value := FocusedModule.Name // Module name
    else
      Value := '-';

    RibbonGalleryItemGroup.Items[i].Hint := Value;
    RibbonGalleryItemGroup.Items[i].Description := cxGetStringAdjustedToWidth(0, 0, Value, 200, mstEndEllipsis);
  end;
end;

procedure TFormMain.ActionStopListAddExecute(Sender: TObject);
var
  i: integer;
  Item: TCustomLocalizerItem;
  FormStopList: TFormStopList;
  StopListField: TStopListField;
  Value: string;
begin
  FormStopList := TFormStopList.Create(nil);
  try
    StopListField := TStopListField(TAction(Sender).Tag);

    FormStopList.BeginAddStopList;

    for i := 0 to SelectionCount-1 do
    begin
      Item := Selection[i];

      if (Item is TLocalizerProperty) then
        Value := TStopListItem.ExtractValue(StopListField, TLocalizerProperty(Item))
      else
        Value := Item.Name; // Module name

      FormStopList.ContinueAddStopList('', StopListField, slOpEquals, Value);
    end;

    if (FormStopList.EndAddStopList) then
      ActionStopListApply.Execute;
  finally
    FormStopList.Free;
  end;
end;

function TFormMain.ApplyStopList(const Progress: IProgress): TStopListItemList.TStopListStats;
begin
  Result := TranslationManagerSettings.StopList.StopList.Apply(FProject,
    procedure(Item: TCustomLocalizerItem)
    begin
      if (Item is TLocalizerProperty) then
      begin
        if (TLocalizerProperty(Item).Item.Module = FocusedModule) then
          ReloadProperty(TLocalizerProperty(Item));
      end else
      if (Item is TLocalizerModule) then
        LoadItem(TLocalizerModule(Item));
    end, Progress);
end;

procedure TFormMain.ActionStopListApplyExecute(Sender: TObject);
resourcestring
  sStopListApply = 'Applying Stop List';
  sStopListApplyPromptTitle = 'Apply Stop List';
  sStopListApplyPrompt = 'Applying the Stop List will mark all properties which match any stop list criteria as "Don''t translate".'#13#13+
    'Do you want to apply the stop list to your project now?';
  sStopListResultTitle = 'Stop list applied';
  sStopListResult = '%.0n modules, %.0n properties were marked "Don''t Translate".';
var
  Progress: IProgress;
  Stats: TStopListItemList.TStopListStats;
begin
  if (TaskMessageDlg(sStopListApplyPromptTitle, sStopListApplyPrompt, mtConfirmation, [mbYes, mbNo], 0, mbYes) <> mrYes) then
    Exit;

  SaveCursor(crHourGlass);
  Progress := ShowProgress(sStopListApply);

  Stats := ApplyStopList(Progress);


  Progress.Hide;
  Progress := nil;

  RefreshModuleStats;

  TaskMessageDlg(sStopListResultTitle, Format(sStopListResult, [Stats.ModuleCount*1.0, Stats.PropertyCount*1.0]), mtInformation, [mbOK], 0);
end;

procedure TFormMain.ActionStopListApplyUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FProject.Modules.Count > 0) and (TranslationManagerSettings.StopList.StopList.Count > 0);
end;

procedure TFormMain.ActionStopListExecute(Sender: TObject);
var
  FormStopList: TFormStopList;
begin
  FormStopList := TFormStopList.Create(nil);
  try

    FormStopList.Execute;

  finally
    FormStopList.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionFindSearchExecute(Sender: TObject);
begin
  if (FSearchProvider = nil) then
    FSearchProvider := TFormSearch.Create(Self);

  // Make sure we have a module selected or the search dialog will burn
  if (FocusedModule = nil) then
    TreeListModules.Root.GetFirstChildVisible.Focused := True;

  FSearchProvider.Show;
end;

procedure TFormMain.ActionFindNextExecute(Sender: TObject);
begin
  FSearchProvider.SelectNextResult;
end;

procedure TFormMain.ActionFindNextUpdate(Sender: TObject);
begin
  TAction(sender).Enabled := (FSearchProvider <> nil) and (FSearchProvider.CanSelectNextResult);
end;

// -----------------------------------------------------------------------------

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

procedure TFormMain.ActionIntegrationTrackingExecute(Sender: TObject);
begin
  ResetTracker;
  FTrackingDismissed := True;
end;

// -----------------------------------------------------------------------------

function TFormMain.LocateStringsSymbolFile(CheckForOutOfDate, ForcePrompt: boolean): boolean;
var
  SymbolFilename: string;
resourcestring
  sStringSymbolsNotFoundTitle = 'String Symbol file not found';
  sStringSymbolsNotFound = 'The resourcestring symbol file does not exist in the same folder as the source application file.'+#13#13+
    'The name of the file is expected to be: %s'+#13#13+
    'The file is required for support of resourcestrings. Delphi will generate this file automatically if '+
    'the "Output resourcestring .drc file" linker option is enabled.'+#13#13+
    'Do you want to locate the file now?';
  sStringSymbolsOutOfDateTitle = 'String Symbols file out of date';
  sStringSymbolsOutOfDate = 'The DRC file appears to be out of date based on its timestamps compared to that of the EXE file.'#13#13+
    'EXE was last modified: %s'#13+
    'DRC was last modified: %s'#13#13+
    'Beware that using an out of date DRC file can cause resourcestring translations to be lost or become unusable.'#13#13+
    'Are you sure you want to use the specified DRC file?';
const
  MaxTimeDifference = 10; // seconds
begin
  Result := True;
  var UpdateFilename := False;

  SaveCursor(crAppStart);

  if (FProject.StringSymbolFilename = '') then
  begin
    SymbolFilename := TPath.ChangeExtension(FProject.SourceFilename, TranslationManagerShell.sFileTypeStringSymbols);
    UpdateFilename := True;
  end else
    // Path might be relative - Get absolute path
    SymbolFilename := PathUtil.PathCombinePath(TPath.GetDirectoryName(FProject.SourceFilename), FProject.StringSymbolFilename);

  while (ForcePrompt) or (not TFile.Exists(SymbolFilename)) do
  begin
    UpdateFilename := True;

    if (not ForcePrompt) then
    begin
      // Symbol file does not exist. Try to locate it.
      var Res := TaskMessageDlg(sStringSymbolsNotFoundTitle,
        Format(sStringSymbolsNotFound, [FProject.StringSymbolFilename]),
        mtWarning, [mbYes, mbNo, mbCancel], 0, mbYes);

      if (Res <> mrYes) then
      begin
        Result := (Res <> mrCancel);
        Exit;
      end;
    end;

    if (SymbolFilename <> '') then
    begin
      OpenDialogDRC.InitialDir := TPath.GetDirectoryName(SymbolFilename);
      OpenDialogDRC.Filename := TPath.GetFileName(SymbolFilename);
    end;

    ForcePrompt := False;

    if (not OpenDialogDRC.Execute(Handle)) then
      continue;

    SymbolFilename := OpenDialogDRC.Filename;
  end;

  // Check for out-of-date DRC file
  if (CheckForOutOfDate) then
  begin
    var TimeStampEXE := TFile.GetLastWriteTime(FProject.SourceFilename);
    var TimeStampDRC := TFile.GetLastWriteTime(SymbolFilename);

    var TimeDifference := Abs(TimeStampEXE - TimeStampDRC);

    if (TimeDifference * 24*60*60 > MaxTimeDifference) then
    begin
      var Res := TaskMessageDlg(sStringSymbolsOutOfDateTitle,
        Format(sStringSymbolsOutOfDate, [DateTimeToStr(TimeStampEXE), DateTimeToStr(TimeStampDRC)]),
        mtWarning, [mbYes, mbNo], 0, mbNo);

      if (Res = mrNo) then
        Exit(False);
    end;
  end;

  if (UpdateFilename) then
  begin
    FProject.StringSymbolFilename := SymbolFilename;
    FProject.Modified := True;
  end;
end;

function TFormMain.CheckStringsSymbolFile(CheckForOutOfDate: boolean): boolean;
begin
  Result := LocateStringsSymbolFile(CheckForOutOfDate, False);
end;

function TFormMain.LocateSourceFile(ForcePrompt: boolean): boolean;
resourcestring
  sSourceFileNotFoundTitle = 'Source file not found';
  sSourceFileNotFound = 'The source application file specified in the project does not exist.'+#13#13+
    'Filename: %s'+#13#13+
    'Do you want to locate the file now?';
begin
  var WarnIfNotFound := not ForcePrompt;
  var SourceFilename := FProject.SourceFilename;

  while (ForcePrompt) or (not TFile.Exists(SourceFilename)) do
  begin
    // Source file does not exist - Try to locate it.
    if (WarnIfNotFound) then
    begin
      var Res := TaskMessageDlg(sSourceFileNotFoundTitle, Format(sSourceFileNotFound, [SourceFilename]), mtWarning, [mbYes, mbNo], 0, mbYes);

      if (Res <> mrYes) then
        Exit(False);
    end;

    if (SourceFilename <> '') then
    begin
      OpenDialogEXE.InitialDir := TPath.GetDirectoryName(SourceFilename);
      OpenDialogEXE.FileName := TPath.GetFileName(SourceFilename);
    end;

    if (not OpenDialogEXE.Execute(Handle)) then
      Exit(False);

    SourceFilename := OpenDialogEXE.FileName;

    ForcePrompt := False;
  end;

  if (FProject.SourceFilename <> SourceFilename) then
  begin
    FProject.SourceFilename := SourceFilename;
    FProject.Modified := True;
  end;

  // If symbol file also doesn't exist we try to fix that without involving the user
  // but only if the file is found at the expected location.
  if (not TFile.Exists(FProject.StringSymbolFilename)) then
  begin
    SourceFilename := TPath.ChangeExtension(SourceFilename, TranslationManagerShell.sFileTypeStringSymbols);
    if (TFile.Exists(SourceFilename)) then
    begin
      FProject.StringSymbolFilename := SourceFilename;
      FProject.Modified := True;
    end;
  end;

  Result := True;
end;

function TFormMain.CheckSourceFile: boolean;
begin
  SaveCursor(crAppStart);
  Result := LocateSourceFile(False);
end;

procedure TFormMain.ActionProjectLocateSourceExecute(Sender: TObject);
begin
  SaveCursor(crAppStart);
  if (LocateSourceFile(True)) then
    // Also verify DRC file if we got a valid source file
    LocateStringsSymbolFile(True, False);
end;

procedure TFormMain.ActionProjectNewExecute(Sender: TObject);
var
  FormNewProject: TFormNewProject;
  ProjectProcessor: TProjectResourceProcessor;
  Count: TCounts;
resourcestring
  sProjectInitializedTitle = 'Project initialized';
  sProjectInitialized = 'Project has been initialized.'+#13#13+'The following resources were read from the source file:'#13#13+
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
      FormNewProject.SourceApplication := '';

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

  if (not CheckStringsSymbolFile(True)) then
    Exit;

  // Initial scan
  SaveCursor(crHourGlass);

  ClearTargetLanguage;
  FProjectIndex := nil;
  FProject.Clear;

  ProjectProcessor := TProjectResourceProcessor.Create;
  try
    try

      ProjectProcessor.ScanProject(FProject, FProject.SourceFilename);

    except
      on E: EResourceProcessor do
      begin
        TaskMessageDlg(sErrorLoadingModuleTitle, E.Message, mtWarning, [mbOK], 0);
        Exit;
      end;
    end;
  finally
    ProjectProcessor.Free;
  end;

  UpdateTargetLanguage(True);

  FProjectIndex := FProject.CreatePropertyLookup(TranslationManagerSettings.Editor.SanitizeRules);

  if (TranslationManagerSettings.System.AutoApplyStopList) then
    ApplyStopList;

  CreateTranslationMemoryPeeker(False);

  Count := CountStuff;

  // Display update statistics
  TaskMessageDlg(sProjectInitializedTitle,
    Format(sProjectInitialized, [1.0*Count.CountModule, 1.0*Count.CountItem, 1.0*Count.CountProperty]),
    mtInformation, [mbOK], 0);

  LoadProject(FProject);
end;

function TFormMain.LoadFromFile(const Filename: string): boolean;
var
  Stream: TStream;
  ProgressStream: TStream;
  BestLanguage: TTranslationLanguage;
  i: integer;
  Progress: IProgress;
resourcestring
  sFileNotFound = 'File not found: %s';
begin
  Result := True;

  if (not TFile.Exists(Filename)) then
  begin
    MessageDlg(Format(sFileNotFound, [Filename]), mtWarning, [mbOK], 0);
    Exit(False);
  end;

  SaveCursor(crHourGlass);

  ClearDependents;
  ClearTargetLanguage;
  FModuleItemsDataSource.Clear;
  TreeListModules.Clear;

  Progress := ShowProgress(sProgressProjectLoading);
  try
//    Progress.Marquee := True;

    FProject.BeginUpdate;
    try

      FProjectFilename := Filename;
      try

        Stream := TFileStream.Create(FProjectFilename, fmOpenRead);
        try
          ProgressStream := TProgressStream.Create(Stream, Progress);
          try

            TLocalizationProjectFiler.LoadFromStream(FProject, ProgressStream, Progress);

          finally
            ProgressStream.Free;
          end;
        finally
          Stream.Free;
        end;

      except
        FProjectFilename := '';
        raise;
      end;

      // Source path is relative to project file - Make it absolute
      FProject.SourceFilename := PathUtil.PathCombinePath(TPath.GetDirectoryName(Filename), FProject.SourceFilename);
      // Symbol path is relative to source file - Make it absolute
      FProject.StringSymbolFilename := PathUtil.PathCombinePath(TPath.GetDirectoryName(FProject.SourceFilename), FProject.StringSymbolFilename);

      FProject.Modified := False;

    finally
      FProject.EndUpdate;
    end;

    FProjectIndex := FProject.CreatePropertyLookup(TranslationManagerSettings.Editor.SanitizeRules);

    UpdateProjectModifiedIndicator;

    RibbonMain.DocumentName := TPath.GetFileNameWithoutExtension(FProject.SourceFilename);
    ResetTracker;

    // Find language with most translations
    BestLanguage := nil;
    for i := 0 to FProject.TranslationLanguages.Count-1 do
      if (BestLanguage = nil) or (FProject.TranslationLanguages[i].TranslatedCount >= BestLanguage.TranslatedCount) then
        BestLanguage := FProject.TranslationLanguages[i];

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

  (* This must be optional or not done at all. We don't need access to the EXE in order to edit translations.
  if (CheckSourceFile) then
    CheckStringsSymbolFile(False);
  *)
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
var
  i: integer;
  Param: string;
begin
  TranslationManagerSettings.System.EndBoot;

  // Release semaphore once SingleInstance handling has been set up and the boot marker has been cleared
  RestartSemaphore.Release;

  if (not TranslationManagerSettings.Providers.TranslationMemory.LoadOnDemand) and (not TranslationManagerSettings.System.SafeMode) then
  begin
    try

      FTranslationMemory.CheckLoaded(True);

    except
      on E: ETranslationMemory do
        MessageDlg(E.Message, mtWarning, [mbOK], 0);
    end;
  end;

  // Register shell integration on first run
  if (TranslationManagerSettings.System.FirstRun) and (not TranslationManagerSettings.System.SafeMode) then
    try
      TranslationManagerShell.RegisterShellIntegration;
    except
      // Fail silently
    end;


  InitializeProject('', GetLanguageID(TranslationManagerSettings.System.DefaultSourceLanguage));

  // We could just as well have called LoadFromFile directly here but what the hell...
  if (ParamCount > 0) then
  begin
    FPendingFileOpenLock.Enter;
    try
      i := 1;
      while (i <= ParamCount) do
      begin
        Param := ParamStr(i);
        // Ignore parameters
        if (Param <> '') and (IsAnsi(Param[1])) and (not (AnsiChar(Param[1]) in SwitchChars)) then
        begin
          if (FPendingFileOpen = nil) then
            FPendingFileOpen := TStringList.Create;

          FPendingFileOpen.Add(Param);
          break; // We only handle a single file at a time
        end;

        inc(i);
      end;
      if (FPendingFileOpen <> nil) and (FPendingFileOpen.Count > 0) then
        PostMessage(Handle, MSG_FILE_OPEN, Ord(True), 0);
    finally
      FPendingFileOpenLock.Leave;
    end;
  end;
end;

procedure TFormMain.MsgCopyData(var Msg: TWMCopyData);
resourcestring
  sFocusTrackTitle = 'Focus tracking requested';
  sFocusTrackMsg = 'The application being translated has requested that focus tracking be enabled.'#13#13+
    'When focus tracking is enabled the translation manager will automatically select the module and item corresponding to the '+
    'control currently focused in the application being translated.'#13#13+
    'Do you want to enable focus tracking now?';
begin
  if (not ActionIntegrationTracking.Checked) then
  begin
    if (not FTrackingDismissed) then
    begin
      FTrackingDismissed := True;
      var Res := TaskMessageDlg(sFocusTrackTitle, sFocusTrackMsg, mtConfirmation, [mbYes, mbNo], 0, mbNo);
      if (Res <> mrYes) then
        exit;
      ActionIntegrationTracking.Checked := True;
    end else
      exit;
  end;

  case TTranslationManagerTrackKind(Msg.CopyDataStruct.dwData) of
    tkForm:
      begin
        var Name: string := PChar(Msg.CopyDataStruct.lpData);
        for var Module in FProject.Modules.Values do
          if (AnsiSameText(Name, Module.Name)) then
          begin
            ViewModule(Module);
            Msg.Result := 1;
            break;
          end;
      end;

    tkControl:
      begin
        if (FocusedModule = nil) then
          exit;

        var Name: string := PChar(Msg.CopyDataStruct.lpData);

        var BestMatchItem: TLocalizerItem := nil;
        var OverSize := MaxInt;
        for var Item in FocusedModule.Items.Values do
          if (Item.Name.StartsWith(Name, True)) then
          begin
            var NewOverSize := Length(Item.Name) - Length(Name);
            if (NewOverSize < OverSize) then
            begin
              OverSize := NewOverSize;
              BestMatchItem := Item;

              if (OverSize = 0) then
                break;
            end;
          end;

        if (BestMatchItem <> nil) then
        begin
          ViewItem(BestMatchItem);
          Msg.Result := 1;
        end;
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
var
  Filename: string;
begin
  if (not CheckSave) then
    Exit;

  Filename := OpenDialogProject.FileName;
  if (Filename <> '') then
  begin
    OpenDialogProject.FileName := TPath.GetFileName(Filename);
    OpenDialogProject.InitialDir := TPath.GetDirectoryName(Filename);
  end;

  if (not OpenDialogProject.Execute(Handle)) then
    Exit;

  SaveCursor(crHourGlass);

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
  sPurgeUnusedTranslationsTitle = 'Purge unused translations';
  sPurgeUnusedTranslationsNone = 'No translations are marked as unused.'#13#13+
    'Nothing to purge';
  sPurgeUnusedTranslations = '%d translations are marked as unused.'#13+
    'This is likely because components were deleted, moved or renamed since the previous update.'#13#13+
    'Do you want to delete all unused translations?';
  sPurgeUnusedStatusTitle = 'Purge completed';
  sPurgeUnusedStatus = 'The following was removed from the project:'#13#13+
    'Modules: %.0n'#13'Items: %.0n'#13'Properties: %.0n';
begin
  CountBefore := CountStuff;

  if (CountBefore.UnusedTranslation = 0) then
  begin
    TaskMessageDlg(sPurgeUnusedTranslationsTitle, sPurgeUnusedTranslationsNone, mtInformation, [mbOK], 0);
    Exit;
  end;

  if (TaskMessageDlg(sPurgeUnusedTranslationsTitle, Format(sPurgeUnusedTranslations, [CountBefore.UnusedTranslation]), mtConfirmation, [mbYes, mbNo], 0, mbNo) <> mrYes) then
    Exit;

  SaveCursor(crHourGlass);

  NeedReload := False;
  CurrentModule := FocusedModule;

  GridItemsTableView.BeginUpdate;
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
            FModuleItemsDataSource.Module := nil;
            CurrentModule := nil;
          end;
          Node := TreeListModules.Find(Module, TreeListModules.Root, False, True, TreeListFindFilter);
          Node.Free;
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
                FModuleItemsDataSource.Module := nil;
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
                  FModuleItemsDataSource.Module := nil;
                  CurrentModule := nil;
                end;
                Prop.Free;
              end;

            if (Item.Properties.Count = 0) then
            begin
              NeedReload := True;
              if (Module = CurrentModule) then
              begin
                FModuleItemsDataSource.Module := nil;
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
            FModuleItemsDataSource.Module := nil;
            CurrentModule := nil;
          end;
          Node := TreeListModules.Find(Module, TreeListModules.Root, False, True, TreeListFindFilter);
          Node.Free;
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
    GridItemsTableView.EndUpdate;
  end;

  CountAfter := CountStuff;

  Msg := Format(sPurgeUnusedStatus,
    [
    1.0*(CountBefore.CountModule-CountAfter.CountModule),
    1.0*(CountBefore.CountItem-CountAfter.CountItem),
    1.0*(CountBefore.CountProperty-CountAfter.CountProperty)
    ]);
  TaskMessageDlg(sPurgeUnusedStatusTitle, Msg, mtInformation, [mbOK], 0);
end;

procedure TFormMain.ActionProjectRecoverExecute(Sender: TObject);
var
  Count: TCounts;
  CountRecovered: integer;
  Msg: string;
resourcestring
  sRecoverUnusedNothing = 'Nothing to recover.';
begin
  Count := CountStuff;

  if (Count.UnusedTranslation = 0) then
  begin
    ShowMessage(sRecoverUnusedNothing);
    Exit;
  end;

  ApplyTranslationTextEdit(False);

  Msg := Format(sRecoverUnusedTranslationsAction, [Count.UnusedTranslation]);
  Msg := Format(sRecoverUnusedTranslations, [Msg]);
  if (TaskMessageDlg(sRecoverUnusedTranslationsTitle, Msg, mtConfirmation, [mbYes, mbNo], 0, mbYes) <> mrYes) then
    Exit;

  CountRecovered := RecoverUnusedTranslations(False);

  if (CountRecovered > 0) then
  begin
    LoadProject(FProject, False);
    TaskMessageDlg(sRecoverUnusedStatusTitle, Format(sRecoverUnusedStatus, [CountRecovered, Count.UnusedTranslation]), mtInformation, [mbOK], 0);
  end else
    TaskMessageDlg(sRecoverUnusedStatusTitle, sRecoverUnusedStatusNone, mtInformation, [mbOK], 0);
end;

procedure TFormMain.ActionProjectSaveExecute(Sender: TObject);
var
  ProjectFilename, SourceFilename, SymbolFilename: string;
  SaveSourceFilename, SaveSymbolFilename: string;
  Progress: IProgress;
resourcestring
  sProgressProjectLoading = 'Loading project...';
  sProgressProjectSaving = 'Saving project...';
begin
  ApplyTranslationTextEdit(False);

  SaveCursor(crHourGlass);

  // Save current paths so we can restore them if anything goes wrong
  SaveSourceFilename := FProject.SourceFilename;
  SaveSymbolFilename := FProject.StringSymbolFilename;

  SourceFilename := SaveSourceFilename;
  SymbolFilename := SaveSymbolFilename;
  if (FProjectFilename = '') then
    FProjectFilename := TPath.ChangeExtension(SourceFilename, TranslationManagerShell.sFileTypeProject);

  // Make sure we have the absolute paths before the base path is changed
  if (FProjectFilename <> '') then
  begin
    SourceFilename := PathUtil.PathCombinePath(TPath.GetDirectoryName(FProjectFilename), SourceFilename);
    SymbolFilename := PathUtil.PathCombinePath(TPath.GetDirectoryName(SourceFilename), SymbolFilename);

    SaveDialogProject.InitialDir := TPath.GetDirectoryName(FProjectFilename);
    SaveDialogProject.FileName := TPath.GetFileName(FProjectFilename);
  end;


  // Prompt user for project filename
  if (not SaveDialogProject.Execute(Handle)) then
    Exit;


  ProjectFilename := SaveDialogProject.FileName;

  Progress := ShowProgress(sProgressProjectSaving);
  try
    Progress.Marquee := True;

    // Save relative paths in project
    FProject.SourceFilename := PathUtil.FilenameMakeRelative(TPath.GetDirectoryName(ProjectFilename), SourceFilename);
    FProject.StringSymbolFilename := PathUtil.FilenameMakeRelative(TPath.GetDirectoryName(SourceFilename), SymbolFilename);

    try

      if (not SafeReplaceFile(ProjectFilename,
        function(const Filename: string): boolean
        begin
          var Options: TLocalizationSaveOptions := [];
          if (not TranslationManagerSettings.Project.SaveDontTranslate) then
            Include(Options, soOmitDontTranslateItems);
          if (TranslationManagerSettings.Project.SaveSorted) then
            Include(Options, soSort);

          TLocalizationProjectFiler.SaveToFile(FProject, Filename, Options, Progress);

          Result := True;
        end, TranslationManagerSettings.Backup.SaveBackups)) then
      begin
        // Something went wrong - restore original absolute paths
        FProject.SourceFilename := SaveSourceFilename;
        FProject.StringSymbolFilename := SaveSymbolFilename;
        Exit;
      end;

    except
      // Something went wrong - restore original absolute paths
      FProject.SourceFilename := SaveSourceFilename;
      FProject.StringSymbolFilename := SaveSymbolFilename;
      raise;
    end;

    FProjectFilename := ProjectFilename;

    // Make paths absolute again
    FProject.SourceFilename := PathUtil.PathCombinePath(TPath.GetDirectoryName(FProjectFilename), FProject.SourceFilename);
    FProject.StringSymbolFilename := PathUtil.PathCombinePath(TPath.GetDirectoryName(FProject.SourceFilename), FProject.StringSymbolFilename);

    FProject.Modified := False;

    SetInfoText('Saved');
    UpdateProjectModifiedIndicator;
  finally
    Progress := nil;
  end;

  AddRecentFile(ProjectFilename);
end;

procedure TFormMain.ActionProjectSaveUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (not FProject.SourceFilename.IsEmpty);
end;

procedure TFormMain.ActionProjectUpdateExecute(Sender: TObject);
var
  ProjectProcessor: TProjectResourceProcessor;
  CountBefore, CountAfter: TCounts;
  Msg: string;
  SaveModified: boolean;
  WasModified: boolean;
  CountRecovered: integer;
begin
  if (not CheckSourceFile) then
    Exit;

  CheckStringsSymbolFile(True);

  ApplyTranslationTextEdit(True);

  SaveCursor(crHourGlass);

  CountBefore := CountStuff;

  // Clear New state
  FProject.Traverse(
    function(Module: TLocalizerModule): boolean
    begin
      Module.ClearState(ItemStateNew);

      Module.Traverse(
        function(Item: TLocalizerItem): boolean
        begin
          Item.ClearState(ItemStateNew);

          Item.Traverse(
            function(Prop: TLocalizerProperty): boolean
            begin
              Prop.ClearState(ItemStateNew);
              Result := True;
            end);
          Result := True;
        end);
      Result := True;
    end);


  SaveModified := FProject.Modified;
  FProject.Modified := False;

  FProjectIndex := nil;

  ProjectProcessor := TProjectResourceProcessor.Create;
  try
    try

      ProjectProcessor.ScanProject(FProject, FProject.SourceFilename);

    except
      on E: EResourceProcessor do
      begin
        TaskMessageDlg(sErrorLoadingModuleTitle, E.Message, mtWarning, [mbOK], 0);
        Exit;
      end;
      on E: EOSError do
      begin
        TaskMessageDlg(sErrorLoadingModuleTitle, E.Message, mtWarning, [mbOK], 0);
        Exit;
      end;
    end;
  finally
    ProjectProcessor.Free;
  end;

  FProjectIndex := FProject.CreatePropertyLookup(TranslationManagerSettings.Editor.SanitizeRules);

  if (TranslationManagerSettings.System.AutoApplyStopList) then
    ApplyStopList;

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
      1.0*(CountAfter.CountSynthesized-CountBefore.CountSynthesized),
      1.0*(CountAfter.ObsoleteTranslation-CountBefore.ObsoleteTranslation)
      ]);
    TaskMessageDlg(sProjectUpdatedTitle, Msg, mtInformation, [mbOK], 0);

    if (CountAfter.UnusedTranslation > CountBefore.UnusedTranslation) then
    begin
      Msg := Format(sRecoverUnusedTranslationsUpdate, [CountAfter.UnusedTranslation-CountBefore.UnusedTranslation]);
      Msg := Format(sRecoverUnusedTranslations, [Msg]);
      if (TaskMessageDlg(sRecoverUnusedTranslationsTitle, Msg, mtConfirmation, [mbYes, mbNo], 0, mbYes) <> mrYes) then
        Exit;

      CountRecovered := RecoverUnusedTranslations(True);

      if (CountRecovered > 0) then
      begin
        LoadProject(FProject, False);
        TaskMessageDlg(sRecoverUnusedStatusTitle, Format(sRecoverUnusedStatus, [CountRecovered, CountAfter.UnusedTranslation-CountBefore.UnusedTranslation]), mtInformation, [mbOK], 0);
      end else
        TaskMessageDlg(sRecoverUnusedStatusTitle, sRecoverUnusedStatusNone, mtInformation, [mbOK], 0);
    end;
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
    end);

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

    CreateTranslationMemoryPeeker(False);

    ApplyCustomSettings;


    // Make sure folders exist and are writable
    TranslationManagerSettings.Folders.ValidateFolders;

    if (FormSettings.RestartRequired) then
    begin
      if (not QueueRestart(True)) then
        exit;
    end;
  finally
    FormSettings.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionStatusDontTranslateExecute(Sender: TObject);
var
  i: integer;
  Item: TCustomLocalizerItem;
begin
  SaveCursor(crAppStart);
  BeginUpdate;
  try
    for i := 0 to SelectionCount-1 do
    begin
      Item := Selection[i];

      if (Item.IsUnused) or (Item.Status = ItemStatusDontTranslate) then
        continue;

      Item.Status := ItemStatusDontTranslate;

      // TODO : This should be done in LoadItem()
      if (Item = FocusedModule) then
        QueueTranslationMemoryPeek
      else
      if (Item is TLocalizerProperty) then
        QueueTranslationMemoryPeek(TLocalizerProperty(Item), True);

      LoadItem(Item, True);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TFormMain.ActionStatusDontTranslateUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FocusedItem <> nil) and (not FocusedItem.IsUnused);

  TAction(Sender).Checked := (TAction(Sender).Enabled) and (FocusedItem.Status = ItemStatusDontTranslate);
end;

procedure TFormMain.ActionStatusHoldExecute(Sender: TObject);
var
  i: integer;
  Item: TCustomLocalizerItem;
begin
  SaveCursor(crAppStart);
  BeginUpdate;
  try
    for i := 0 to SelectionCount-1 do
    begin
      Item := Selection[i];

      if (Item.IsUnused) or (Item.Status = ItemStatusHold) then
        continue;

      Item.Status := ItemStatusHold;

      // TODO : This should be done in LoadItem()
      if (Item = FocusedModule) then
        QueueTranslationMemoryPeek
      else
      if (Item is TLocalizerProperty) then
        QueueTranslationMemoryPeek(TLocalizerProperty(Item), True);

      LoadItem(Item, True);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TFormMain.ActionStatusHoldUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FocusedItem <> nil) and (not FocusedItem.IsUnused);

  TAction(Sender).Checked := (TAction(Sender).Enabled) and (FocusedItem.Status = ItemStatusHold);
end;

procedure TFormMain.ActionStatusTranslateExecute(Sender: TObject);
var
  i: integer;
  Item: TCustomLocalizerItem;
begin
  SaveCursor(crAppStart);
  BeginUpdate;
  try
    for i := 0 to SelectionCount-1 do
    begin
      Item := Selection[i];

      if (Item.IsUnused) or (Item.Status = ItemStatusTranslate) then
        continue;

      Item.Status := ItemStatusTranslate;

      // TODO : This should be done in LoadItem()
      if (Item = FocusedModule) then
        QueueTranslationMemoryPeek
      else
      if (Item is TLocalizerProperty) then
        QueueTranslationMemoryPeek(TLocalizerProperty(Item), True);

      LoadItem(Item, True);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TFormMain.ActionStatusTranslateUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FocusedItem <> nil) and (not FocusedItem.IsUnused);

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
  sLocalizerWarnings = 'Validation found %.0n problems in the project. Of these %.0n are new';
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
        if (Prop.Translations.TryGetTranslation(TranslationLanguage, Translation)) then
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
    end);

  // Update node state images
  if (NeedRefresh) then
    GridItemsTableView.Invalidate(True);

  if (WarningCount = 0) then
    MessageDlg(sLocalizerWarningsNone, mtInformation, [mbOK], 0)
  else
    MessageDlg(Format(sLocalizerWarnings, [WarningCount*1.0, NewWarningCount*1.0]), mtWarning, [mbOK], 0);
end;

procedure TFormMain.ActionValidationWarningDismissExecute(Sender: TObject);
begin
  FocusedProperty.Translations[TranslationLanguage].Warnings := [];
  LoadFocusedProperty;
end;

procedure TFormMain.ActionValidationWarningResolveExecute(Sender: TObject);
var
  Warnings: TTranslationWarnings;
  RemainingWarnings: TTranslationWarnings;
  Warning: TTranslationWarning;
  EqualizeRules: TMakeAlikeRules;
  Translation: TLocalizerTranslation;
  SourceValue, Value: string;
begin
  Translation := FocusedProperty.Translations[TranslationLanguage];
  Warnings := Translation.Warnings;

  EqualizeRules := [];
  RemainingWarnings := [];
  for Warning in Warnings do
    case Warning of
      tWarningAccelerator:
        Include(EqualizeRules, EqualizeAccelerators);
      tWarningTerminator:
        Include(EqualizeRules, EqualizeEnding);
      tWarningSurround:
        Include(EqualizeRules, EqualizeSurround);
    else
      Include(RemainingWarnings, Warning);
    end;

  Value := Translation.Value;
  SourceValue := FocusedProperty.Value;

  if (EqualizeRules <> []) then
    Value := MakeAlike(SourceValue, Value, EqualizeRules);

  if (tWarningLeadSpace in RemainingWarnings) then
    Value := EqualizeLeadingSpace(SourceValue, Value);

  if (tWarningTrailSpace in RemainingWarnings) then
    Value := EqualizeTrailingSpace(SourceValue, Value);

  Translation.Value := Value;
  LoadFocusedProperty;
end;

procedure TFormMain.ActionValidationWarningResolveUpdate(Sender: TObject);
var
  Prop: TLocalizerProperty;
  Translation: TLocalizerTranslation;
const
  ResolvableWarnings: TTranslationWarnings = [tWarningAccelerator,tWarningLeadSpace,tWarningTrailSpace,tWarningTerminator,tWarningSurround];
begin
  Prop := ActiveProperty;
  TAction(Sender).Enabled := (Prop <> nil) and (Prop.Translations.TryGetTranslation(TranslationLanguage, Translation)) and
    (Translation.Warnings * ResolvableWarnings <> []);
end;

procedure TFormMain.ActionValidationWarningDismissUpdate(Sender: TObject);
var
  Prop: TLocalizerProperty;
  Translation: TLocalizerTranslation;
begin
  Prop := ActiveProperty;
  TAction(Sender).Enabled := (Prop <> nil) and (Prop.Translations.TryGetTranslation(TranslationLanguage, Translation)) and
    (Translation.Warnings <> []);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionTranslationStateUpdate(Sender: TObject);
var
  Prop: TLocalizerProperty;
begin
  Prop := ActiveProperty;

  TAction(Sender).Enabled := (Prop <> nil) and (not Prop.IsUnused) and (Prop.EffectiveStatus <> ItemStatusDontTranslate);
end;

procedure TFormMain.ActionTranslationStateAcceptExecute(Sender: TObject);
var
  i: integer;
  RecordIndex: integer;
  Prop: TLocalizerProperty;
  Translation: TLocalizerTranslation;
begin
  SaveCursor(crAppStart);
  GridItemsTableView.BeginUpdate;
  try
    var AutoApplyTranslations := TranslationManagerSettings.Editor.AutoApplyTranslations;
    var AutoApplyTranslationsSimilar := TranslationManagerSettings.Editor.AutoApplyTranslationsSimilar;
    var UpdatedSame := 0;
    var UpdatedSimilar := 0;

    for i := 0 to GridItemsTableView.Controller.SelectedRecordCount-1 do
    begin
      RecordIndex := GridItemsTableView.Controller.SelectedRecords[i].RecordIndex;
      Prop := FModuleItemsDataSource.Properties[RecordIndex];

      if (not Prop.Translations.TryGetTranslation(TranslationLanguage, Translation)) then
        Translation := Prop.Translations.AddOrUpdateTranslation(TranslationLanguage, Prop.Value);

      Translation.Status := tStatusTranslated;

      LoadItem(Prop);

      TranslationAdded(Prop, AutoApplyTranslations, AutoApplyTranslationsSimilar, UpdatedSame, UpdatedSimilar);
    end;

    DisplayAutoApplyTranslationsStats(UpdatedSame, UpdatedSimilar);
  finally
    GridItemsTableView.EndUpdate;
  end;
end;

procedure TFormMain.ActionTranslationStateProposeExecute(Sender: TObject);
var
  i: integer;
  RecordIndex: integer;
  Prop: TLocalizerProperty;
  Translation: TLocalizerTranslation;
begin
  SaveCursor(crAppStart);
  GridItemsTableView.BeginUpdate;
  try
    var AutoApplyTranslations := TranslationManagerSettings.Editor.AutoApplyTranslations;
    var AutoApplyTranslationsSimilar := TranslationManagerSettings.Editor.AutoApplyTranslationsSimilar;
    var UpdatedSame := 0;
    var UpdatedSimilar := 0;

    for i := 0 to GridItemsTableView.Controller.SelectedRecordCount-1 do
    begin
      RecordIndex := GridItemsTableView.Controller.SelectedRecords[i].RecordIndex;
      Prop := FModuleItemsDataSource.Properties[RecordIndex];

      if (not Prop.Translations.TryGetTranslation(TranslationLanguage, Translation)) then
        Translation := Prop.Translations.AddOrUpdateTranslation(TranslationLanguage, Prop.Value);

      Translation.Status := tStatusProposed;

      TranslationAdded(Prop, AutoApplyTranslations, AutoApplyTranslationsSimilar, UpdatedSame, UpdatedSimilar);
    end;

    DisplayAutoApplyTranslationsStats(UpdatedSame, UpdatedSimilar);
  finally
    GridItemsTableView.EndUpdate;
  end;
end;

procedure TFormMain.ActionTranslationStateRejectExecute(Sender: TObject);
var
  i: integer;
  RecordIndex: integer;
  Prop: TLocalizerProperty;
begin
  SaveCursor(crAppStart);
  GridItemsTableView.BeginUpdate;
  try
    for i := 0 to GridItemsTableView.Controller.SelectedRecordCount-1 do
    begin
      RecordIndex := GridItemsTableView.Controller.SelectedRecords[i].RecordIndex;
      Prop := FModuleItemsDataSource.Properties[RecordIndex];

      Prop.Translations.Remove(TranslationLanguage);

      LoadItem(Prop);

      // Prop is now translatable again - check for match in TM
      QueueTranslationMemoryPeek(Prop, True);
    end;
  finally
    GridItemsTableView.EndUpdate;
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
    ((FProject.TranslationLanguages.Count > 1) or
     ((FProject.TranslationLanguages.Count = 1) and (FProject.TranslationLanguages[0].LanguageID <> SourceLanguageID)));

  DataModuleMain.Project := FProject;
end;

procedure TFormMain.BarEditItemTargetLanguageExit(Sender: TObject);
begin
  DataModuleMain.FilterTargetLanguages := False;
  DataModuleMain.Project := nil;
end;

procedure TFormMain.BarEditItemTargetLanguagePropertiesEditValueChanged(Sender: TObject);
begin
  // BarEditItemTargetLanguage.EditValue isn't updated at this point.
  // When MSG_TARGET_CHANGED arrives it will be.
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
  DeleteLanguages: TList<TTranslationLanguage>;
  Language: TTranslationLanguage;
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

    for i := 0 to FProject.TranslationLanguages.Count-1 do
      FormLanguages.SelectTargetLanguage(FProject.TranslationLanguages[i].LanguageID);

    FormLanguages.ApplyFilter := FFilterTargetLanguages;

    if (not FormLanguages.Execute) then
      Exit;

    FFilterTargetLanguages := FormLanguages.ApplyFilter;

    GridItemsTableView.BeginUpdate;
    try
      DeleteLanguages := TList<TTranslationLanguage>.Create;
      try
        Languages := TList<integer>.Create;
        try

          // Build list of selected languages
          for i := 0 to FormLanguages.TargetLanguageCount-1 do
            Languages.Add(FormLanguages.TargetLanguage[i]);

          // Loop though list of current languages and build list of languages to delete
          PromptCount := 0;
          for i := FProject.TranslationLanguages.Count-1 downto 0 do
            if (not Languages.Contains(FProject.TranslationLanguages[i].LanguageID)) then
            begin
              DeleteLanguages.Add(FProject.TranslationLanguages[i]);

              if (FProject.TranslationLanguages[i].TranslatedCount > 0) then
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
          if (Language = FTranslationLanguage) then
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
            end);

          Assert(Language.TranslatedCount = 0);

          FProject.TranslationLanguages.Remove(Language.LanguageID);
        end;

      finally
        DeleteLanguages.Free;
      end;

      // Add selected languages
      for i := 0 to FormLanguages.TargetLanguageCount-1 do
        FProject.TranslationLanguages.Add(FormLanguages.TargetLanguage[i]);

      // If we deleted current target language we must select a new one - default to source language
      if (FTranslationLanguage = nil) then
        TargetLanguageID := SourceLanguageID;

    finally
      GridItemsTableView.EndUpdate;
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

  var Filename := TdxBarItem(Sender).Hint;

  if (not LoadFromFile(Filename)) then
  begin
    // File could not be opened - remove from recent files list
    var n := TranslationManagerSettings.Folders.RecentFiles.IndexOf(Filename);
    if (n <> -1) then
      TranslationManagerSettings.Folders.RecentFiles.Delete(n);
    Sender.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TFormMain.CheckSave: boolean;
var
  Res: integer;
resourcestring
  sLocalizerSavePromptTitle = 'Project has not been saved';
  sLocalizerSavePrompt = 'Your changes has not been saved.'#13#13'Do you want to save them now?';
begin
  // Apply any pending edits
  ApplyTranslationTextEdit(True);

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
  CanClose := CheckSave and FTranslationMemory.CheckSave;
end;

// -----------------------------------------------------------------------------

function TFormMain.GetProject: TLocalizerProject;
begin
  Result := FProject;
end;

function TFormMain.GetFocusedItem: TCustomLocalizerItem;
begin
  if (IsPropertyActive) then
    Result := FocusedProperty
  else
    Result := FocusedModule;
end;

function TFormMain.GetIsModuleActive: boolean;
begin
  Result := (TreeListModules.Focused) and (TreeListModules.FocusedNode <> nil);
end;

function TFormMain.GetActiveModule: TLocalizerModule;
begin
  if (IsModuleActive) then
    Result := FocusedModule
  else
    Result := nil;
end;

function TFormMain.GetFocusedModule: TLocalizerModule;
begin
  if (TreeListModules.FocusedNode <> nil) then
    Result := TLocalizerModule(TreeListModules.FocusedNode.Data)
  else
    Result := nil;
end;

function TFormMain.GetIsPropertyActive: boolean;
begin
  Result := (GridItemsTableView.IsControlFocused) and (GridItemsTableView.Controller.FocusedRowIndex <> -1);
end;

function TFormMain.GetActiveProperty: TLocalizerProperty;
begin
  if (GetIsPropertyActive) then
    Result := FocusedProperty
  else
    Result := nil;
end;

function TFormMain.GetFocusedProperty: TLocalizerProperty;
begin
  if (GridItemsTableView.Controller.FocusedRowIndex <> -1) then//and (GridItemsTableView.Controller.FocusedItem <> nil) then
    Result := FModuleItemsDataSource.Properties[GridItemsTableView.Controller.FocusedRecord.RecordIndex]
  else
    Result := nil;
end;

// -----------------------------------------------------------------------------

function TFormMain.HasSelection: boolean;
begin
  Result := (SelectionCount > 0);
end;

function TFormMain.GetSelection(Index: integer): TCustomLocalizerItem;
begin
  if (IsPropertyActive) then
    Result := FModuleItemsDataSource.Properties[GridItemsTableView.Controller.SelectedRecords[Index].RecordIndex]
  else
    Result := TLocalizerModule(TreeListModules.Selections[Index].Data);
end;

function TFormMain.GetSelectionCount: integer;
begin
  if (IsPropertyActive) then
    Result := GridItemsTableView.Controller.SelectedRowCount
  else
    Result := TreeListModules.SelectionCount;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.SetSkin(const Value: string);

  function DetectRemoteSession: boolean;
  const
    SM_REMOTECONTROL      = $2001; // This system metric is used in a Terminal
                                   // Services environment. Its value is nonzero
                                   // if the current session is remotely
                                   // controlled; otherwise, 0.

    SM_REMOTESESSION      = $1000; // This system metric is used in a Terminal
                                   // Services environment. If the calling process
                                   // is associated with a Terminal Services
                                   // client session, the return value is nonzero.
                                   // If the calling process is associated with
                                   // the Terminal Server console session, the
                                   // return value is 0. The console session is
                                   // not necessarily the physical console.
  var
    Mode: string;
  begin
    Result := (GetSystemMetrics(SM_REMOTESESSION) <> 0) or (GetSystemMetrics(SM_REMOTECONTROL) <> 0);

    // Test for emulated local/remote mode
    if (FindCmdLineSwitch('Session', Mode, True)) then
    begin
      if (SameText(Mode, 'Remote')) then
        Result := True
      else
      if (SameText(Mode, 'Local')) then
        Result := False;
    end;
  end;

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

  SkinFilename := EnvironmentVars.ExpandString(SkinFilename);

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
    RibbonSkin := dxRibbonSkinsManager.GetMostSuitable(SkinName, RibbonMain.Style, Screen.PixelsPerInch);

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
      RibbonSkin := dxRibbonSkinsManager.GetMostSuitable(SkinName, RibbonMain.Style, Screen.PixelsPerInch);
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
    if (DetectRemoteSession) then
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
    Exit(Value);

  Result := GetUserDefaultLCID;

  // If the user locale isn't a real locale we fall back to the system default locale
  // Note: The following values denotes custom locales (which we do not support):
  //       $1000, $2000, $2400, $2800, $2C00, $3000, $3400, $3800, $3C00, $4000, $4400, $4800, $4C00

  if (TLocaleItems.FindLCID(Result) = nil) then
    Result := GetSystemDefaultLCID;
end;

function TFormMain.GetSourceLanguageID: Word;
begin
  Result := FSourceLanguage.Locale;
(*
  if (VarIsOrdinal(BarEditItemSourceLanguage.EditValue)) then
    Result := BarEditItemSourceLanguage.EditValue
  else
    Result := 0;
*)
end;

procedure TFormMain.SetSourceLanguageID(const Value: Word);
var
  LocaleItem: TLocaleItem;
begin
  // TODO : Should broadcast notification
  LocaleItem := TLocaleItems.FindLCID(Value);
  if (LocaleItem = nil) then
    raise Exception.CreateFmt('Invalid source language ID: %.4X', [Value]);

  ClearTranslationMemoryPeekResult;

  FSourceLanguage := LocaleItem;
  BarEditItemSourceLanguage.EditValue := FSourceLanguage.Locale;
  FProject.SourceLanguageID := FSourceLanguage.Locale;
  GridItemsTableViewColumnSource.Caption := FSourceLanguage.LanguageName;
  LabelSourceName.Caption := FSourceLanguage.LanguageName;
  if (FSourceLanguage.IsRightToLeft <> IsRightToLeft) and (TranslationManagerSettings.Editor.EditBiDiMode) then
  begin
    // Source language is Right-to-Left but rest of UI isn't - or vice versa
    if (FSourceLanguage.IsRightToLeft) then
      EditSourceText.BiDiMode := bdRightToLeft
    else
      EditSourceText.BiDiMode := bdLeftToRight;
  end else
    EditSourceText.BiDiMode := BiDiMode;

  CreateTranslationMemoryPeeker(True);
end;

function TFormMain.GetTranslationLanguage: TTranslationLanguage;
begin
  if (FTranslationLanguage = nil) then
    FTranslationLanguage := FProject.TranslationLanguages.Add(TargetLanguageID);

  Result := FTranslationLanguage;
end;

function TFormMain.GetTargetLanguageID: Word;
begin
  Result := FTargetLanguage.Locale;
end;

procedure TFormMain.SetTargetLanguageID(const Value: Word);

  function FindDictionaryFile(LocaleItem: TLocaleItem; const FileType: string): string;

    function FindFile(const Folder: string): string;
    begin
      Result := Format('%s%s.%s', [Folder, LocaleItem.LanguageShortName, FileType]); // DAN
      if (TFile.Exists(Result)) then
        Exit;

      Result := Format('%s%s.%s', [Folder, LocaleItem.LocaleName, FileType]); // da-DK
      if (TFile.Exists(Result)) then
        Exit;

      Result := Format('%s%s_%s.%s', [Folder, LocaleItem.ISO639_1Name, LocaleItem.ISO3166Name, FileType]); // DA_DK
      if (TFile.Exists(Result)) then
        Exit;

      Result := Format('%s%s.%s', [Folder, LocaleItem.ISO639_1Name, FileType]); // DA
      if (TFile.Exists(Result)) then
        Exit;

      Result := '';
    end;

  begin
    Result := FindFile(EnvironmentVars.ExpandString(TranslationManagerSettings.Folders.Folder[tmFolderUserSpellCheck]));
    if (Result = '') then
      Result := FindFile(EnvironmentVars.ExpandString(TranslationManagerSettings.Folders.Folder[tmFolderSpellCheck]));
  end;

var
  LocaleItem: TLocaleItem;
  i: integer;
  Found: boolean;
  AnyFound: boolean;
  FilenameDic, FilenameAff: string;
  SpellCheckerDictionaryItem: TdxSpellCheckerDictionaryItem;
begin
  // TODO : Should broadcast notification

  // Note: Always process setting the target regardless of current value.
  // We need to have the dependents refreshed.

  LocaleItem := TLocaleItems.FindLCID(Value);
  if (LocaleItem = nil) then
    raise Exception.CreateFmt('Invalid target language ID: %.4X', [Value]);

  ApplyTranslationTextEdit(False);

  BeginUpdate;
  try
    ClearDependents;
    ClearTargetLanguage;
    ClearTranslationMemoryPeekResult;

    FTargetLanguage := LocaleItem;

    BarEditItemTargetLanguage.EditValue := FTargetLanguage.Locale;

    CreateTranslationMemoryPeeker(True);

    GridItemsTableViewColumnTarget.Caption := FTargetLanguage.LanguageName;
    LabelTargetName.Caption := FTargetLanguage.LanguageName;
    if (FTargetLanguage.IsRightToLeft <> IsRightToLeft) and (TranslationManagerSettings.Editor.EditBiDiMode) then
    begin
      // Target language is Right-to-Left but rest of UI isn't - or vice versa
      if (FTargetLanguage.IsRightToLeft) then
        EditTargetText.BiDiMode := bdRightToLeft
      else
        EditTargetText.BiDiMode := bdLeftToRight;
    end else
      EditTargetText.BiDiMode := BiDiMode;


    (*
    ** Load spell check dictionary for new language
    *)

    // Unload old custom dictionary
    Assert(SpellChecker.Dictionaries[0] is TdxUserSpellCheckerDictionary);
    SpellChecker.Dictionaries[0].Enabled := False;
    SpellChecker.Dictionaries[0].Unload; // This saves the custom dictionary
    // Load new custom dictionary
    TdxUserSpellCheckerDictionary(SpellChecker.Dictionaries[0]).DictionaryPath := Format('%suser-%s.dic', [EnvironmentVars.ExpandString(TranslationManagerSettings.Folders.FolderUserSpellCheck), FTargetLanguage.LanguageShortName]);
    SpellChecker.Dictionaries[0].Enabled := True;
    SpellChecker.Dictionaries[0].Language := FTargetLanguage.Locale;

    // Deactivate all existing dictionaries except ones that match new language
    AnyFound := False;
    for i := 1 to SpellChecker.DictionaryCount-1 do
    begin
      Found := (SpellChecker.Dictionaries[i].Language = FTargetLanguage.Locale);

  //    if (SpellChecker.Dictionaries[i].Enabled) and (not Found) then
  //      SpellChecker.Dictionaries[i].Unload;

      SpellChecker.Dictionaries[i].Enabled := Found;
      AnyFound := AnyFound or Found;
    end;

    FCanSpellCheck := AnyFound or (TdxUserSpellCheckerDictionary(SpellChecker.Dictionaries[0]).WordCount > 0);

    // Add and load new dictionary
    if (not AnyFound) then
    begin
      FilenameDic := FindDictionaryFile(FTargetLanguage, 'dic');
      if (FilenameDic <> '') then
        FilenameAff := FindDictionaryFile(FTargetLanguage, 'aff')
      else
        FilenameAff := '';

      if (FilenameDic <> '') and (FilenameAff <> '') then
      begin
        // AnyFound := True;
        SpellCheckerDictionaryItem := SpellChecker.DictionaryItems.Add;
        SpellCheckerDictionaryItem.DictionaryTypeClass := TdxHunspellDictionary;
        TdxHunspellDictionary(SpellCheckerDictionaryItem.DictionaryType).Language := FTargetLanguage.Locale;
        TdxHunspellDictionary(SpellCheckerDictionaryItem.DictionaryType).DictionaryPath := FilenameDic;
        TdxHunspellDictionary(SpellCheckerDictionaryItem.DictionaryType).GrammarPath := FilenameAff;
        TdxHunspellDictionary(SpellCheckerDictionaryItem.DictionaryType).Enabled := True;

        FCanSpellCheck := True;
      end;

      // TODO : Add support for other formats here...
    end;

    SpellChecker.LoadDictionaries;

    // Reload project
    UpdateTargetLanguage(True);

  finally
    EndUpdate;
  end;
end;

procedure TFormMain.ClearTargetLanguage;
begin
  FTranslationLanguage := nil;
  FModuleItemsDataSource.TranslationLanguage := nil;
end;

procedure TFormMain.UpdateTargetLanguage(Clear: boolean);
begin
  if (Clear) then
    FTranslationLanguage := nil;
  FModuleItemsDataSource.TranslationLanguage := GetTranslationLanguage; // Must use getter
  TreeListModules.FullRefresh;
  RefreshModuleStats;
end;

// -----------------------------------------------------------------------------

function TFormMain.CountStuff: TCounts;
var
  Counts: TCounts;
begin
  Counts := Default(TCounts);

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
              HasTranslations: boolean;
            begin
              Inc(Counts.CountProperty);
              if (Prop.Synthesized) then
                Inc(Counts.CountSynthesized);

              HasTranslations := False;
              for i := 0 to Prop.Translations.Count-1 do
                case Prop.Translations[i].Status of
                  tStatusProposed,
                  tStatusTranslated:
                    begin
                      HasTranslations := True;
                      Inc(Counts.Translated);
                    end;
                  tStatusObsolete:
                    begin
                      HasTranslations := True;
                      Inc(Counts.ObsoleteTranslation);
                    end;
                end;

              if (Prop.IsUnused) then
              begin
                Inc(Counts.UnusedProperty);
                if (HasTranslations) and (Prop.EffectiveStatus = ItemStatusTranslate) then
                  Inc(Counts.UnusedTranslation);
              end;

              Result := True;
            end);

          Result := True;
        end);

      Result := True;
    end);

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
  FTranslationMemoryPeek := nil;
  ClearDependents;
  TreeListModules.Clear;
  ClearTargetLanguage;

  FProject.BeginUpdate;
  try

    FProject.Clear;

    FProject.SourceFilename := SourceFilename;
    FProject.StringSymbolFilename := TPath.ChangeExtension(SourceFilename, TranslationManagerShell.sFileTypeStringSymbols);

    FProject.SourceLanguageID := SourceLocaleID;

    FProject.Modified := False;

  finally
    FProject.EndUpdate;
  end;

  FWarningCount := 0;
  StatusBar.Panels[StatusBarPanelWarning].Visible := False;

  UpdateProjectModifiedIndicator;

  RibbonMain.DocumentName := TPath.GetFileNameWithoutExtension(FProject.SourceFilename);
  ResetTracker;

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
begin
  SaveCursor(crHourGlass);

  LockUpdates;
  try
    if (Clear) then
    begin
      GridItemsTableView.Controller.ClearSelection;
      GridItemsTableView.Controller.FocusedRowIndex := -1;
      GridItemsTableView.Controller.TopRowIndex := 0;
      FModuleItemsDataSource.Module := nil;
    end;

    Modules := Project.Modules.Values.ToArray;

    TArray.Sort<TLocalizerModule>(Modules, TComparer<TLocalizerModule>.Construct(
      function(const Left, Right: TLocalizerModule): Integer
      begin
        Result := (Ord(Right.Kind) - Ord(Left.Kind)); // Reversed to order resourcestrings before forms (doesn't really matter here)
        if (Result = 0) then
          Result := AnsiCompareText(Left.Name, Right.Name);
      end));

    SelectedModuleFound := False;

    for Module in Modules do
    begin
      if (Module.Kind = mkOther) then
        continue;

      if (Clear) then
        ModuleNode := nil
      else
      begin
        if (Module = FModuleItemsDataSource.Module) then
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
    begin
      // Selected module no longer exist
      FModuleItemsDataSource.Module := nil;
    end else
    begin
      FModuleItemsDataSource.Refresh; // Project has changed - make sure data source reloads
      GridItemsTableView.Controller.ClearSelection;
      GridItemsTableView.Controller.TopRowIndex := 0;
      GridItemsTableView.Controller.FocusedRowIndex := -1;
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
      GridItemsTableView.Invalidate(True);

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

procedure TFormMain.LoadFocusedProperty;
begin
  ReloadProperty(FocusedProperty);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.OnProjectChanged(Sender: TObject);
begin
  UpdateProjectModifiedIndicator;
end;

procedure TFormMain.OnTranslationWarning(Translation: TLocalizerTranslation);
resourcestring
  sValidationWarningHint = 'Project contains %.0n validation warnings';
begin
  if (Translation.Warnings <> []) then
    Inc(FWarningCount)
  else
    Dec(FWarningCount);

  if (FWarningCount > 0) then
  begin
    StatusBar.Panels[StatusBarPanelWarning].Visible := True;
    FStatusBarPanelHint[StatusBarPanelWarning] := Format(sValidationWarningHint, [FWarningCount*1.0]);
  end else
    StatusBar.Panels[StatusBarPanelWarning].Visible := False;

  // TODO : Maintain list of properties with warnings so we can display them in a list. Limit the size to something reasonable.
end;

procedure TFormMain.OnModuleChanged(Module: TLocalizerModule);
var
  Node: TcxTreeListNode;
begin
  // Refresh stats for current module
  if (Module = FocusedModule) then
    RefreshModuleStats;

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

  DismissToast;
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

procedure TFormMain.StatusBarHint(Sender: TObject);
begin
  DismissToast;
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

procedure TFormMain.StatusBarPanels1Click(Sender: TObject);
begin
  // TODO : Display validation warning overview
  ActionGotoNextWarningExecute(nil); // The nil parameter signals that the status bar button is the caller
end;

procedure TFormMain.StatusBarPanels2Click(Sender: TObject);
begin
  ActionProjectSave.Execute;
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

  var s := Format(sLocalizerStatusCount,
    [1.0*FProject.StatusCount[ItemStatusTranslate], 1.0*FProject.StatusCount[ItemStatusDontTranslate], 1.0*FProject.StatusCount[ItemStatusHold]]);

  StatusBar.Panels[StatusBarPanelStats].Width := (StatusBar.Canvas.TextWidth(s) + 15) and $FFFFFFF8; // 8 pixels extra and round up to 8 pixels
  StatusBar.Panels[StatusBarPanelStats].Text := s;

  StatusBar.Update;
end;

// -----------------------------------------------------------------------------

function TFormMain.GotoNext(Predicate: TLocalizerPropertyDelegate; DisplayNotFound, FromStart, AutoWrap: boolean): boolean;
var
  Module: TLocalizerModule;
  Prop: TLocalizerProperty;
  ModuleNode: TcxTreeListNode;
  RowIndex, RecordIndex: integer;
begin
  SaveCursor(crHourGlass);
  DismissToast(sLocalizerFindWrapAround);

  Result := False;

  if (FromStart) then
    ModuleNode := nil
  else
    ModuleNode := TreeListModules.FocusedNode;

  if (ModuleNode <> nil) then
  begin
    if (GridItemsTableView.Controller.FocusedRecordIndex <> -1) then
      RowIndex := GridItemsTableView.Controller.FocusedRecord.Index + 1 // Note: Visible index
    else
      RowIndex := -1;
  end else
  begin
    ModuleNode := TreeListModules.Root.GetFirstChildVisible;
    RowIndex := -1;
  end;

  while (ModuleNode <> nil) do
  begin
    Module := TLocalizerModule(ModuleNode.Data);

    if (Module.Status = ItemStatusTranslate) then
    begin
      if (RowIndex = -1) then
      begin
        // Determine if module has any valid properties.
        // If so we load the property list and start with the top row
        if (not Module.Traverse(
          function(Prop: TLocalizerProperty): boolean
          begin
            Result := not Predicate(Prop);
          end)) then
        begin
          // Select module node - this loads the property grid
          ModuleNode.MakeVisible;
          ModuleNode.Focused := True;

          // Start with top row
          RowIndex := 0;
        end;
      end;

      while (RowIndex <> -1) and (RowIndex < GridItemsTableView.ViewData.RowCount) do
      begin
        // First verify that item is visible - and get the record index
        RecordIndex := GridItemsTableView.ViewData.Rows[RowIndex].RecordIndex;
        if (RecordIndex <> -1) then
        begin
          Prop := FModuleItemsDataSource.Properties[RecordIndex];

          if (Predicate(Prop)) then
          begin
            ViewProperty(Prop);
            GridItems.SetFocus;

            Exit(True);
          end;
        end;

        // Check next row
        Inc(RowIndex);
      end;
    end;

    ModuleNode := ModuleNode.GetNextSiblingVisible;
    RowIndex := -1;

    if (ModuleNode = nil) and (AutoWrap) and (not FromStart) then
    begin
      AutoWrap := False;
      QueueToast(sLocalizerFindWrapAround);
      ModuleNode := TreeListModules.Root.GetFirstChildVisible;
    end;
  end;

  if (DisplayNotFound) then
  begin
    if (FromStart) then
      ShowMessage(sLocalizerFindNone)
    else
      ShowMessage(sLocalizerFindNoMore);
  end;
end;

procedure TFormMain.GridItemsTableViewCanFocusRecord(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; var AAllow: Boolean);
begin
  // Focused record is changing - Post any text editor changes
  PostTranslationTextEdit;
end;

procedure TFormMain.GridItemsTableViewCellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo;
  AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
begin
  // Double click starts edit mode.
  // This works around DevExpress' unintuitive "very slow double click to edit"
  if (ACellViewInfo.Item.Options.Editing) then
  begin
    Sender.Controller.EditingController.ShowEdit;
    AHandled := True;
  end;
end;

procedure TFormMain.GridItemsTableViewColumnGetCellHint(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
  ACellViewInfo: TcxGridTableDataCellViewInfo; const AMousePos: TPoint; var AHintText: TCaption; var AIsHintMultiLine: Boolean; var AHintTextRect: TRect);
var
  Prop: TLocalizerProperty;
  ImageIndex: integer;
  r: TRect;
begin
  if (TranslationManagerSettings.Editor.DisplayStatusGlyphs) and (TranslationManagerSettings.Editor.StatusGlyphHints) and (Sender.VisibleIndex = 0) then
  begin
    r := ACellViewInfo.Bounds;
    r.Right := r.Left + 1 + DataModuleMain.ImageListTree.Width;
    if (not r.Contains(AMousePos)) then
      Exit;

    Prop := FModuleItemsDataSource.Properties[ARecord.RecordIndex];

    ImageIndex := DataModuleMain.GetImageIndex(Prop, TranslationLanguage);
    if (ImageIndex = -1) then
      Exit;

    AHintText := DataModuleMain.GetImageHint(ImageIndex);
    AHintTextRect := ACellViewInfo.Bounds;
    AHintTextRect.Left := r.Right + 8;
(* TODO : I would like to reposition the normal hint a bit so the hint text perfectly aligns with the cell text
  end else
  begin
    AHintTextRect := ACellViewInfo.Bounds;
    AHintTextRect.Left := ACellViewInfo.TextAreaBounds.Left - 2;
*)
  end;
end;

procedure TFormMain.GridItemsTableViewColumnStatusStateGetFilterValues(Sender: TcxCustomGridTableItem; AValueList: TcxDataFilterValueList);
begin
  var Properties := TcxImageComboBoxProperties(Sender.GetProperties);
  for var i := 0 to Properties.Items.Count-1 do
    AValueList.Add(fviValue, Properties.Items[i].Value, Properties.Items[i].Description, True);
end;

procedure TFormMain.GridItemsTableViewColumnTargetValidateDrawValue(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
  const AValue: Variant; AData: TcxEditValidateInfo);
var
  Prop: TLocalizerProperty;
  Translation: TLocalizerTranslation;
  Warning: TTranslationWarning;
  s: string;
  Bitmap: TBitmap;
resourcestring
  sValidationWarning = 'Translation has validation warnings:%s';
begin
  Prop := FModuleItemsDataSource.Properties[ARecord.RecordIndex];
  Assert(Prop <> nil);

  if (not Prop.Translations.TryGetTranslation(TranslationLanguage, Translation)) then
    Exit;

  if (Translation.Warnings <> []) then
  begin
    AData.ErrorType := eetCustom;
    s := '';

    for Warning in Translation.Warnings do
      s := s + #13 + '- ' + LoadResString(sTranslationValidationWarnings[Warning]);

    AData.ErrorText := Format(sValidationWarning, [s]);

    Bitmap := TBitmap.Create;
    try
      DataModuleMain.ImageListState.GetImage(NodeImageIndexStateWarning, Bitmap);
      AData.ErrorIcon.Assign(Bitmap);
    finally
      Bitmap.Free;
    end;
  end;
end;

procedure TFormMain.GridItemsTableViewCustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);

  procedure PrepareBidi(Language: TLocaleItem);
  var
    Flags: DWORD;
  begin
    if (Language.IsRightToLeft <> IsRightToLeft) then
    begin
      // Target language is Right-to-Left but rest of UI isn't - or vice versa

      (* None of these work:
      AViewInfo.EditViewInfo.UseRightToLeftAlignment := True;
      TcxCustomTextEditViewInfo(AViewInfo.EditViewInfo).TextOutData.TextParams.RTLReading := True;
      ACanvas.TextFlags := ACanvas.TextFlags or CXTO_RTLREADING;
      *)
      Flags := TcxCustomTextEditViewInfo(AViewInfo.EditViewInfo).DrawTextFlags;
      if (Language.IsRightToLeft) then
      begin
        // Set RTL
        Flags := Flags or CXTO_RTLREADING;
        // Swap left/right alignment
        if (TranslationManagerSettings.Editor.EditBiDiMode) then
        begin
          if ((Flags and $0000000F) = CXTO_LEFT) then
            Flags := (Flags and $FFFFFFF0) or CXTO_RIGHT
          else
          if ((Flags and $0000000F) =  CXTO_RIGHT) then
            Flags := (Flags and $FFFFFFF0) or CXTO_LEFT;
        end;
      end else
        // Clear RTL
        Flags := Flags and (not CXTO_RTLREADING);

      TcxCustomTextEditViewInfo(AViewInfo.EditViewInfo).DrawTextFlags := Flags;
    end;
  end;

  procedure DrawTargetValue;
  var
    Prop: TLocalizerProperty;
    Triangle: array[0..2] of TPoint;
  begin
    // Draw indicator if source value is found in Translation Memory
    if (FModuleItemsDataSource.PeekResult[AViewInfo.GridRecord.RecordIndex] <> TTranslationMemoryPeekResult.prFound) then
      Exit;

    // Don't draw indicator if we're editing cell
    if (AViewInfo.Editing) and (AViewInfo.Focused) then
      Exit;

    Prop := FModuleItemsDataSource.Properties[AViewInfo.GridRecord.RecordIndex];

    if (Prop.IsUnused) or (Prop.EffectiveStatus <> ItemStatusTranslate) or (Prop.HasTranslation(TranslationLanguage)) then
    begin
      // Status has changed since the flag was set
      FModuleItemsDataSource.PeekResult[AViewInfo.GridRecord.RecordIndex] := TTranslationMemoryPeekResult.prQueued;
      Exit;
    end;

    // Draw the default content
    if (not ADone) then
      AViewInfo.EditViewInfo.Paint(ACanvas);

    // Draw a rectangle in the top right corner
    ACanvas.SaveDC;
    try

      ACanvas.Pen.Handle := 0; // Work around for messed up pen handle sporadically causing line not to be drawn
      ACanvas.Pen.Color := $00E39C5B;
      ACanvas.Pen.Style := psSolid;

      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := clSkyBlue;

      var CornerSize := ScaleFactor.Apply(HintCornerSize);
      if (UseRightToLeftReading) or (TargetLanguage.IsRightToLeft and TranslationManagerSettings.Editor.EditBiDiMode) then
      begin
        // Top left corner
        Triangle[0].X := AViewInfo.Bounds.Left+1;
        Triangle[1].X := Triangle[0].X + CornerSize;
      end else
      begin
        // Top right corner
        Triangle[0].X := AViewInfo.Bounds.Right-2;
        Triangle[1].X := Triangle[0].X - CornerSize;
      end;
      Triangle[0].Y := AViewInfo.Bounds.Top;
      Triangle[1].Y := Triangle[0].Y;
      Triangle[2].X := Triangle[0].X;
      Triangle[2].Y := Triangle[0].Y + CornerSize;

      ACanvas.Polygon(Triangle);

    finally
      ACanvas.RestoreDC;
    end;

    ADone := True;
  end;

  procedure DrawStatusIndicator;
  var
    Prop: TLocalizerProperty;
    ImageIndex: integer;
    r: TRect;
  const
    ImageOffset = 1;
    ImageMargin = 4;
  begin
    Prop := FModuleItemsDataSource.Properties[AViewInfo.GridRecord.RecordIndex];

    ImageIndex := DataModuleMain.GetImageIndex(Prop, TranslationLanguage);
    if (ImageIndex = -1) then
      Exit;

    r := AViewInfo.Bounds;

    // ACanvas.FillRect(r, AViewInfo.EditViewInfo.BackgroundColor);
    AViewInfo.EditViewInfo.DrawCustomEdit(ACanvas, True, True);

    DataModuleMain.ImageListTree.Draw(ACanvas.Canvas, r.Left + ImageOffset, r.Top + (r.Height-DataModuleMain.ImageListTree.Height) div 2, ImageIndex);

    TcxCustomTextEditViewInfo(AViewInfo.EditViewInfo).TextRect.Left :=
      AViewInfo.TextAreaBounds.Left + DataModuleMain.ImageListTree.Width + ImageOffset + ImageMargin;

    TcxCustomTextEditViewInfo(AViewInfo.EditViewInfo).DrawText(ACanvas);

    ADone := True;
  end;

begin
  if (AViewInfo.Item = GridItemsTableViewColumnTarget) then
    PrepareBidi(TargetLanguage)
  else
  if (AViewInfo.Item = GridItemsTableViewColumnSource) then
    PrepareBidi(SourceLanguage);

  if (TranslationManagerSettings.Editor.DisplayStatusGlyphs) and (AViewInfo.Item.VisibleIndex = 0) then
    DrawStatusIndicator;

  if (AViewInfo.Item = GridItemsTableViewColumnTarget) then
    DrawTargetValue;
end;

procedure TFormMain.GridItemsTableViewCustomDrawIndicatorCell(Sender: TcxGridTableView; ACanvas: TcxCanvas; AViewInfo: TcxCustomGridIndicatorItemViewInfo; var ADone: Boolean);
var
  r: TRect;
  Prop: TLocalizerProperty;
  Flag: TPropertyFlag;
  ImageIndex: integer;
  s: string;
begin
  if (not (AViewInfo is TcxGridIndicatorRowItemViewInfo)) then
    Exit;

  var OffsetNumeric := ScaleFactor.Apply(10); // Note: Each node can have only one numeric bookmark ATM
  var OffsetFlag := ScaleFactor.Apply(8);

  if (TcxGridIndicatorRowItemViewInfo(AViewInfo).RowViewInfo.Focused) or (TcxGridIndicatorRowItemViewInfo(AViewInfo).RowViewInfo.Selected) then
    ACanvas.FillRect(AViewInfo.Bounds, DataModuleMain.StyleSelected.Color)
  else
    ACanvas.FillRect(AViewInfo.Bounds, AViewInfo.Params.Color);

  AViewInfo.LookAndFeelPainter.DrawHeaderBorder(ACanvas, AViewInfo.Bounds, [], []);

  // Find width of longest number
  s := string.Create('9', Trunc(Log10(AViewInfo.GridView.ViewData.RowCount)+1));
  r := AViewInfo.Bounds;
  // Draw row number right-aligned
  r.Right := r.Left+ACanvas.TextWidth(s)+ScaleFactor.Apply(4);
  ACanvas.Font.Color := AViewInfo.Params.TextColor;
  ACanvas.Brush.Style := bsClear;
  s := IntToStr(TcxGridIndicatorRowItemViewInfo(AViewInfo).RowViewInfo.GridRecord.Index + 1);
  ACanvas.DrawTexT(s, r, TAlignment.taRightJustify, TcxAlignmentVert.vaCenter, False, False);

  Prop := FModuleItemsDataSource.Properties[TcxGridIndicatorRowItemViewInfo(AViewInfo).GridRecord.RecordIndex];

  if (Prop <> nil) then
  begin
    // Get TM status for row once row becomes visible. If we're drawing the indicator, then the row must be visible
    QueueTranslationMemoryPeek(TcxGridIndicatorRowItemViewInfo(AViewInfo).GridRecord.RecordIndex);

    r.Top := (AViewInfo.Bounds.Top + AViewInfo.Bounds.Bottom - DataModuleMain.ImageListSmall.Height) div 2;
    r.Left := AViewInfo.Bounds.Right - DataModuleMain.ImageListSmall.Width;

    for Flag := FlagBookmark9 downto FlagBookmark0 do
    begin
      // Draw indicator for numeric bookmarks
      if (r.Left < AViewInfo.Bounds.Left) then
        break;

      if (Flag in Prop.Flags) then
      begin
        ImageIndex := ImageIndexBookmark0 + Ord(Flag)-Ord(FlagBookmark0);

        ACanvas.DrawImage(DataModuleMain.ImageListSmall, r.Left, r.Top, ImageIndex);

        Dec(r.Left, OffsetNumeric);
      end;
    end;

    for Flag := FlagBookmarkA to FlagBookmarkF do
    begin
      // Draw indicator for flag bookmarks
      if (r.Left < AViewInfo.Bounds.Left) then
        break;

      if (Flag in Prop.Flags) then
      begin
        ImageIndex := ImageIndexBookmarkA + Ord(Flag)-Ord(FlagBookmarkA);

        ACanvas.DrawImage(DataModuleMain.ImageListSmall, r.Left, r.Top, ImageIndex);

        Dec(r.Left, OffsetFlag);
      end;
    end;

  end;

  ADone := True;
end;

procedure TFormMain.GridItemsTableViewDataControllerRecordChanged(ADataController: TcxCustomDataController; ARecordIndex, AItemIndex: Integer);
var
  Prop: TLocalizerProperty;
  s: string;
begin
  if (AItemIndex <> GridItemsTableViewColumnTarget.Index) then
    Exit;

  if (FUpdateLockCount > 0) then // TODO : Why?
    Exit;

  Prop := FModuleItemsDataSource.Properties[ARecordIndex];
  if (Prop.HasTranslation(TranslationLanguage)) then
    TranslationAdded(Prop);

  // Update text edit in case change was made via in-place edit or Record.Values
  s := VarToStr(ADataController.Values[ARecordIndex, AItemIndex]);
  if (EditTargetText.Lines.Text <> s) then
  begin
    EditTargetText.Lines.Text := s;
    EditTargetText.SelStart := MaxInt;
    FTextEditModified := False;
  end;
end;

procedure TFormMain.GridItemsTableViewEditing(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem; var AAllow: Boolean);
begin
  // TODO : If *source* text contains CRs and editor is being invoked by user (i.e. not from text editor)
  // then automatically open and focus text editor instead of in-place editor.
  // This behavior should be controlled by an option..
end;

procedure TFormMain.GridItemsTableViewFocusedItemChanged(
  Sender: TcxCustomGridTableView; APrevFocusedItem,
  AFocusedItem: TcxCustomGridTableItem);
begin
  // Only enable incremental search when Element, Name or Value column is focused
  Sender.OptionsBehavior.IncSearchItem := AFocusedItem;
  Sender.OptionsBehavior.IncSearch :=
    (AFocusedItem = GridItemsTableViewColumnItemName) or
    (AFocusedItem = GridItemsTableViewColumnValueName) or
    (AFocusedItem = GridItemsTableViewColumnSource);
end;

procedure TFormMain.GridItemsTableViewFocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord,
  AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
begin
  PostTranslationTextEdit;

  FTextEditProperty := FocusedProperty;

  // Load text edit values
  if (AFocusedRecord <> nil) and (FTextEditProperty <> nil) then
  begin
    EditSourceText.Lines.Text := VarToStr(AFocusedRecord.Values[GridItemsTableViewColumnSource.Index]);
    EditTargetText.Lines.Text := VarToStr(AFocusedRecord.Values[GridItemsTableViewColumnTarget.Index]);
    EditTargetText.SelStart := MaxInt;
    FTextEditModified := False;
    EditTargetText.Properties.ReadOnly := False;
  end else
  begin
    EditSourceText.Text := '';
    EditTargetText.Text := '';
    FTextEditModified := False;
    EditTargetText.Properties.ReadOnly := True;
  end;
end;

procedure TFormMain.GridItemsTableViewInitEdit(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit);
var
  SourceValue: string;
  Button: TcxEditButton;
  PropertyList: TLocalizerPropertyList;
  Prop: TLocalizerProperty;
  LookupResult: TTranslationLookupResult;
{$ifdef DEBUG}
  StopWatch: TStopWatch;
{$endif DEBUG}
begin
  HideHint;

  if (AItem <> GridItemsTableViewColumnTarget) then
    Exit;

  (*
  ** BiDi mode
  *)
  if (TargetLanguage.IsRightToLeft <> IsRightToLeft) and (TranslationManagerSettings.Editor.EditBiDiMode) then
  begin
    // Target language is Right-to-Left but rest of UI isn't - or vice versa
    if (TargetLanguage.IsRightToLeft) then
      AEdit.BiDiMode := bdRightToLeft
    else
      AEdit.BiDiMode := bdLeftToRight;
  end else
    AEdit.BiDiMode := BiDiMode;

{$ifdef DEBUG}
  StopWatch := TStopWatch.StartNew;
{$endif DEBUG}

  (*
  ** Populate lookup list with existing translations of same term - and translations from TM
  *)
  QueueTranslationMemoryPeek(GridItemsTableView.Controller.FocusedRecord.RecordIndex);

  TcxCustomDropDownEdit(AEdit).Properties.LookupItems.Clear;

  SourceValue := FocusedProperty.Value;

  LookupResult := TTranslationLookupResult.Create;
  try

    PropertyList := FProjectIndex.Lookup(FocusedProperty);

    // Add existing translations - disregard current property
    if (PropertyList <> nil) then
    begin
      for Prop in PropertyList do
        if (Prop <> FocusedProperty) and (Prop.EffectiveStatus = ItemStatusTranslate) and (Prop.HasTranslation(TranslationLanguage)) then
          LookupResult.Add(Prop.Value, Prop.TranslatedValue[TranslationLanguage]);
    end;

    // Add translations from TM
    if (FModuleItemsDataSource.PeekResult[GridItemsTableView.Controller.FocusedRecord.RecordIndex] = TTranslationMemoryPeekResult.prFound) then
      FTranslationMemory.FindTranslations(FocusedProperty, SourceLanguage, TargetLanguage, LookupResult);

    // Rank by source value similarity - this also eliminates duplicates
    LookupResult.RankTranslations(SourceValue);

    LookupResult.AddToStrings(TcxCustomComboBox(AEdit).Properties.Items);

  finally
    LookupResult.Free;
  end;

  // Display dropdown button if there are any translations
  ActionTranslationSuggestionList.Visible := (TcxCustomComboBox(AEdit).Properties.Items.Count > 0);
  TcxCustomComboBox(AEdit).Properties.Buttons[0].Action := ActionTranslationSuggestionList;

  if (TcxCustomComboBox(AEdit).Properties.Buttons.Count = 1) then
  begin
    Button := TcxCustomComboBox(AEdit).Properties.Buttons.Add;
    Button.Action := ActionEditTranslationText;
    Button.Kind := bkEllipsis;
  end;

{$ifdef DEBUG}
  StopWatch.Stop;
  OutputDebugString(PChar(Format('Lookup overhead: %.0n mS', [StopWatch.ElapsedMilliseconds * 1.0])));
{$endif DEBUG}
end;

type
  TcxEditViewInfoCracker = class(TcxCustomEditViewInfo);

procedure TFormMain.GridItemsTableViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
  r: TRect;
  Prop: TLocalizerProperty;
  HitTest: TcxGridRecordCellHitTest;
begin
  HideHint;

  p := Point(X, Y);

  HitTest := TcxGridRecordCellHitTest(GridItemsTableView.ViewInfo.GetHitTest(p));

  // Require click in target cell
  if (HitTest.HitTestCode <> htCell) or (HitTest.Item <> GridItemsTableViewColumnTarget) then
    Exit;

  if (HitTest.Item.Editing) then
    Exit;

  (*
  ** Display mini toolbar if user clicks warning indicator
  *)
  if (HitTest.Item.FocusedCellViewInfo <> nil) and (TcxEditViewInfoCracker(HitTest.Item.FocusedCellViewInfo.EditViewInfo).GetPart(HitTest.Pos) = ecpErrorIcon) then
  begin
    p := GridItems.ClientToScreen(Point(HitTest.Item.FocusedCellViewInfo.Bounds.Left, HitTest.Item.FocusedCellViewInfo.Bounds.Bottom));
    r := HitTest.Item.FocusedCellViewInfo.Bounds;
    PopupMenuValidationWarning.PopupEx(
      p.X, p.Y,
      0, HitTest.Item.FocusedCellViewInfo.Bounds.Height, // Horizontal non-overlap band
      True, @r);

    HitTest.Item.Focused := False; // Prevent click from starting edit mode
    Exit;
  end;

  (*
  ** Translate if user clicks TM indicator
  *)
  r := HitTest.ViewInfo.Bounds;
  if (UseRightToLeftReading) or (TargetLanguage.IsRightToLeft and TranslationManagerSettings.Editor.EditBiDiMode) then
    // Top left corner
    r.Right := r.Left + HintCornerSize
  else
    // Top right corner
    r.Left := r.Right - HintCornerSize;
  r.Bottom := r.Top + HintCornerSize;

  if (not r.Contains(p)) then
    Exit;

  Prop := FModuleItemsDataSource.Properties[HitTest.GridRecord.RecordIndex];
  if (Prop.IsUnused) or (Prop.EffectiveStatus <> ItemStatusTranslate) or (Prop.HasTranslation(TranslationLanguage)) then
    Exit;

  GridItemsTableView.Controller.ClearSelection;
  HitTest.GridRecord.Selected := True;

  ActionTranslationMemoryTranslate.Execute;
end;

procedure TFormMain.GridItemsTableViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
  r: TRect;
  Prop: TLocalizerProperty;
  HitTest: TcxGridRecordCellHitTest;
begin
  p := Point(X, Y);

  // Hide current hint if we've moved out of the hint rect
  if (FHintVisible) or (GridItemsTableView.Controller.IsEditing) then
  begin
    if (FHintRect.Contains(p)) then
      Exit; // We're still inside the hint rect

    FHintProp := nil;
    HideHint;
  end else
  if (not FHintRect.Contains(p)) then
    FHintProp := nil;

  HitTest := TcxGridRecordCellHitTest(GridItemsTableView.ViewInfo.GetHitTest(p));

  if (HitTest.HitTestCode <> htCell) or (HitTest.Item <> GridItemsTableViewColumnTarget) then
    Exit;

  if (HitTest.Item.Editing) then
    Exit;

  r := HitTest.ViewInfo.Bounds;

  (*
  ** Display TM lookup hint if mouse moves over TM indicator
  *)
  if (UseRightToLeftReading) or (TargetLanguage.IsRightToLeft and TranslationManagerSettings.Editor.EditBiDiMode) then
    // Top left corner
    r.Right := r.Left + HintCornerSize
  else
    // Top right corner
    r.Left := r.Right - HintCornerSize;
  r.Bottom := r.Top + HintCornerSize;

  if (not r.Contains(p)) then
    Exit;

  // Check if node has been marked by the Translation Memory peeker
  if (FModuleItemsDataSource.PeekResult[HitTest.GridRecord.RecordIndex] <> TTranslationMemoryPeekResult.prFound) then
    Exit;

  Prop := FModuleItemsDataSource.Properties[HitTest.GridRecord.RecordIndex];
  if (Prop.IsUnused) or (Prop.EffectiveStatus <> ItemStatusTranslate) or (Prop.HasTranslation(TranslationLanguage)) then
  begin
    // Status has changed since the flag was set
    FModuleItemsDataSource.PeekResult[HitTest.GridRecord.RecordIndex] := TTranslationMemoryPeekResult.prQueued;
    Exit;
  end;

  // Ignore if the hint timed out or we're already waiting for the hint
  if (FHintProp = Prop) then
    Exit;

  FHintProp := Prop;
  FHintRect := r;

  FShowHint := True;
  TimerHint.Enabled := False;
  TimerHint.Interval := HintStyleController.HintPause;
  TimerHint.Enabled := True;
end;

procedure TFormMain.GridItemsTableViewStylesGetContentStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
var
  Prop: TLocalizerProperty;
begin
  Prop := FModuleItemsDataSource.Properties[ARecord.RecordIndex];

  DataModuleMain.GetContentStyle(
    Sender.IsControlFocused,
    (ARecord.Focused) and (AItem <> nil) and (AItem.Focused),
    (ARecord.Selected),
    (AItem <> nil) and (AItem.Editing),
    TranslationLanguage, Prop, AStyle);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.SpellCheckerCheckStart(Sender: TdxCustomSpellChecker; AControl: TWinControl; var AAllow: Boolean);
begin
  // Only spell check:
  AAllow :=
    // Translation list in-place editor
    ((AControl.Parent = GridItems) and (AControl is TcxCustomButtonEdit)) or
    // Translation Memory in-place editor, if language matches
    ((AControl is TcxCustomButtonEdit) and (AControl.Tag = integer(TargetLanguage.Locale))) or
    // Text Editor memo
    (AControl = EditTargetText) or
    // Text Editor dialog memo, if language matches
    ((AControl.Owner is TFormTextEditor) and (TFormTextEditor(AControl.Owner).TargetLanguage = TargetLanguage) and (AControl is TcxCustomMemo));
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

  Translation := Prop.Translations.FindTranslation(TranslationLanguage);

  // Do not check values that have not been translated
  if (Translation = nil) or (not Translation.IsTranslated) then
    Exit(True);

  Text := Translation.Value;

  CheckedText := SanitizeSpellCheckText(Text);

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
    Prop.TranslatedValue[TranslationLanguage] := CheckedText;

    // Update treenode
    LoadFocusedProperty;
  end;

  Result := (TdxSpellCheckerCracker(SpellChecker).LastDialogResult = mrOK);
end;

// -----------------------------------------------------------------------------

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
      SetLength(Props, GridItemsTableView.Controller.SelectedRecordCount);
      for j := 0 to GridItemsTableView.Controller.SelectedRecordCount-1 do
        Props[j] := FModuleItemsDataSource.Properties[GridItemsTableView.Controller.SelectedRecords[j].RecordIndex];
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

// -----------------------------------------------------------------------------

function TFormMain.BuildLanguageModule(LocaleItem: TLocaleItem; const Filename: string): boolean;
var
  i: integer;
  TranslationLanguage: TTranslationLanguage;
  ProjectProcessor: TProjectResourceProcessor;
  ResourceWriter: IResourceWriter;
resourcestring
  sLocalizerResourceModuleBuilding = 'Building resource module for %s';
  sLocalizerResourceModuleBuilt = 'Resource module built: %s';
  sLocalizerFileLockedTitle = 'File can not be replaced';
  sLocalizerFileLocked = 'The file is currently in use and can not be overwritten.'#13#13+
    'Filename: %s'#13+
    'Error: %s';
begin
  Result := False;

  TranslationLanguage := nil;

  for i := 0 to FProject.TranslationLanguages.Count-1 do
    if (FProject.TranslationLanguages[i].LanguageID = LocaleItem.Locale) then
    begin
      TranslationLanguage := FProject.TranslationLanguages[i];
      break;
    end;

  if (TranslationLanguage = nil) then
    Exit; // TODO : Should never happen but an error message would be nice anyway.

  QueueToast(Format(sLocalizerResourceModuleBuilding, [LocaleItem.LanguageName]));

  ProjectProcessor := TProjectResourceProcessor.Create;
  try
    ProjectProcessor.IncludeVersionInfo := TranslationManagerSettings.System.IncludeVersionInfo;

    while (not Result) do
    begin
      try

        ResourceWriter := TResourceModuleWriter.Create(Filename);
        try

          ProjectProcessor.Execute(liaTranslate, FProject, FProject.SourceFilename, TranslationLanguage, ResourceWriter);

          QueueToast(Format(sLocalizerResourceModuleBuilt, [TPath.GetFileName(Filename)]));
          Result := True;

        finally
          ResourceWriter := nil;
        end;

      except
        on E: EResourceProcessor do
        begin
          TaskMessageDlg(sErrorLoadingModuleTitle, E.Message, mtWarning, [mbOK], 0);
          Exit;
        end;
        on E: EFCreateError do
        begin
          // Sharing violation - resource module is probably loaded
          if (TaskMessageDlg(sLocalizerFileLockedTitle, Format(sLocalizerFileLocked, [Filename, E.Message]), mtWarning, [mbRetry, mbAbort], 0, mbRetry) <> mrRetry) then
            break;
        end;
      end;
    end;

  finally
    ProjectProcessor.Free;
  end;

end;

procedure TFormMain.ButtonBuildAllClick(Sender: TObject);
var
  i: integer;
  LocaleItem: TLocaleItem;
  Filename: string;
begin
  if (not CheckSourceFile) then
    Exit;

  if (not CheckStringsSymbolFile(True)) then
    Exit;

  ApplyTranslationTextEdit(False);

  SaveCursor(crHourGlass);

  FProjectIndex := nil;

  FProject.BeginLoad;
  try

    for i := 0 to FProject.TranslationLanguages.Count-1 do
    begin
      LocaleItem := TLocaleItems.FindLCID(FProject.TranslationLanguages[i].LanguageID);
      Filename := LocalizationTools.BuildModuleFilename(FProject.SourceFilename, LocaleItem.Locale, TranslationManagerSettings.System.ModuleNameScheme);

      if (not BuildLanguageModule(LocaleItem, Filename)) then
        break;
    end;

  finally
    FProject.EndLoad;
  end;

  FProjectIndex := FProject.CreatePropertyLookup(TranslationManagerSettings.Editor.SanitizeRules);

  ShowMessage(sDone);

  LoadProject(FProject, False);
end;

procedure TFormMain.OnBuildSingleLanguageHandler(Sender: TObject);
var
  LocaleItem: TLocaleItem;
  TargetFilename, Path: string;
  Filter: string;
resourcestring
  sLocalizerResourceModuleFilenamePrompt = 'Enter filename of resource module';
begin
  if (not CheckSourceFile) then
    Exit;

  if (not CheckStringsSymbolFile(True)) then
    Exit;

  ApplyTranslationTextEdit(False);

  LocaleItem := TLocaleItems.FindLCID(TdxBarItem(Sender).Tag);

  TargetFilename := LocalizationTools.BuildModuleFilename(FProject.SourceFilename, LocaleItem.Locale, TranslationManagerSettings.System.ModuleNameScheme);

  Path := TPath.GetDirectoryName(TargetFilename);
  TargetFilename := TPath.GetFileName(TargetFilename);

  Filter := SaveDialogEXE.Filter;
  if (TargetLanguage <> nil) then
    Filter := Format(sResourceModuleFilter, [LocaleItem.LanguageName, LocaleItem.ISO639_1Name+'*']) + Filter;

  if (not PromptForFileName(TargetFilename, Filter, '', sLocalizerResourceModuleFilenamePrompt, Path, True)) then
    Exit;

  SaveCursor(crHourGlass);

  FProject.BeginLoad;
  try

    if (not BuildLanguageModule(LocaleItem, TargetFilename)) then
      Exit;

  finally
    FProject.EndLoad;
  end;

  ShowMessage(sDone);

  LoadProject(FProject, False);
end;

procedure TFormMain.PopupMenuBuildPopup(Sender: TObject);
var
  i: integer;
  BarItemLink: TdxBarItemLink;
  LocaleItem: TLocaleItem;
begin
  // Remove existing items
  for i := PopupMenuBuild.ItemLinks.Count-1 downto 0 do
    if (PopupMenuBuild.ItemLinks[i].Item.Tag <> 0) then
      PopupMenuBuild.ItemLinks[i].Item.Free;

  for i := 0 to FProject.TranslationLanguages.Count-1 do
  begin
    LocaleItem := TLocaleItems.FindLCID(FProject.TranslationLanguages[i].LanguageID);

    BarItemLink := PopupMenuBuild.ItemLinks.AddButton;
    BarItemLink.Item.Caption := Format('%s...', [LocaleItem.LanguageName]); // Add ellipsis since we're prompting for filename
    BarItemLink.Item.Tag := LocaleItem.Locale;
    BarItemLink.Item.OnClick := OnBuildSingleLanguageHandler;
    TdxBarButton(BarItemLink.Item).ButtonStyle := bsChecked;
    TdxBarButton(BarItemLink.Item).Down := (LocaleItem = TargetLanguage);
  end;
end;

// -----------------------------------------------------------------------------

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
    while (not RestartSemaphore.Acquire(5000)) do
    begin
      if (TaskMessageDlg('Failed to prepare for restart', 'A previous instance of the application might have failed to terminate.'+#13#13+
        'You can use Task Manager to terminate it.', mtWarning, [mbAbort, mbRetry], 0, mbRetry) <> mrRetry) then
        Exit(False);
    end;

    // Launch new instance of application
    if (not Shell.Execute(Application.ExeName, FProjectFilename, Self)) then
      RestartSemaphore.Release;

    // Force close of all projects (we have already prompted to save them) to avoid user being prompted again.
    FProject.Modified := False;

    // Invoke normal logic to close application
    Close;
    //ActionExit.Execute;

    Result := True;
  end;
end;

// -----------------------------------------------------------------------------

function TFormMain.ViewModule(Module: TLocalizerModule): boolean;
var
  Node: TcxTreeListNode;
resourcestring
  sNodeModuleHidden = 'Module has been hidden by a filter and can not be selected';
begin
  Result := False;

  // Find and select module
  Node := TreeListModules.Find(Module, nil, False, True, TreeListFindFilter);

  if (Node = nil) then
    Exit;

  if (Node.IsHidden) then
  begin
    QueueToast(sNodeModuleHidden);
    Exit;
  end;

  Node.MakeVisible;
  Node.Focused := True;

  Result := True;
end;

function TFormMain.ViewItem(Item: TLocalizerItem): boolean;
var
  RowIndex, RecordIndex: integer;
resourcestring
  sNodeItemHidden = 'Item has been hidden by a filter and can not be selected';
begin
  Result := False;

  if (not ViewModule(Item.Module)) then
    Exit;

  // Find and select item row - or any row.
  // Note: The following assumes that IndexOfItem does a backward search.
  RecordIndex := FModuleItemsDataSource.IndexOfItem(Item);
  while (RecordIndex >= 0) do
  begin
    RowIndex := GridItemsTableView.DataController.GetRowIndexByRecordIndex(RecordIndex, True);

    if (RowIndex <> -1) then
    begin
      GridItemsTableView.Controller.ClearSelection;
      GridItemsTableView.ViewData.Rows[RowIndex].Selected := True;
      GridItemsTableView.ViewData.Rows[RowIndex].Focused := True;
      GridItemsTableView.Control.SetFocus;

      Result := True;
      Exit;
    end;

    Dec(RecordIndex);
  end;
  QueueToast(sNodeItemHidden);
end;

function TFormMain.ViewProperty(Prop: TLocalizerProperty): boolean;
var
  RowIndex, RecordIndex: integer;
resourcestring
  sNodePropHidden = 'Property has been hidden by a filter and can not be selected';
begin
  Result := False;

  if (not ViewModule(Prop.Item.Module)) then
    Exit;

  // Find and select property row
  RecordIndex := FModuleItemsDataSource.IndexOfProperty(Prop);
  if (RecordIndex <> -1) then
  begin
    RowIndex := GridItemsTableView.DataController.GetRowIndexByRecordIndex(RecordIndex, True);

    if (RowIndex <> -1) then
    begin
      GridItemsTableView.Controller.ClearSelection;
      GridItemsTableView.ViewData.Rows[RowIndex].Selected := True;
      GridItemsTableView.ViewData.Rows[RowIndex].Focused := True;
      GridItemsTableView.Control.SetFocus;

      Result := True;
    end else
      QueueToast(sNodePropHidden);
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ReloadNode(Node: TcxTreeListNode);
begin
  // Hack to reload a single tree node
  Exclude(TcxTreeListNodeCracker(Node).State, nsValuesAssigned);
  Node.TreeList.LayoutChanged;
end;

procedure TFormMain.ReloadProperty(Prop: TLocalizerProperty);
var
  RecordIndex, RowIndex: integer;
  Translation: TLocalizerTranslation;
begin
  if (Prop.Item.Module <> FocusedModule) then
    Exit;

  RecordIndex := FModuleItemsDataSource.IndexOfProperty(Prop);
  if (RecordIndex <> -1) then
  begin
    // Discard old TM lookup result if property should be translated
    if (Prop.Status = ItemStatusTranslate) and (FModuleItemsDataSource.PeekResult[RecordIndex] <> TTranslationMemoryPeekResult.prNone) then
    begin
      if (not Prop.Translations.TryGetTranslation(TranslationLanguage, Translation)) or (not Translation.IsTranslated) then
        FModuleItemsDataSource.PeekResult[RecordIndex] := TTranslationMemoryPeekResult.prNone;
    end;

    RowIndex := GridItemsTableView.DataController.GetRowIndexByRecordIndex(RecordIndex, False);
    if (RowIndex <> -1) then
      GridItemsTableView.ViewData.Rows[RowIndex].Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ResetTracker;
begin
  if (RibbonMain.DocumentName <> '') then
    FTracker := CreateTranslationManagerTracker(Self, RibbonMain.DocumentName)
  else
    FTracker := nil;
  FTrackingDismissed := False;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.TreeListColumnModuleStatusPropertiesEditValueChanged(Sender: TObject);
begin
  // Module status edited inline
  FocusedModule.Status := TLocalizerItemStatus(TcxImageComboBox(Sender).EditValue);

  LoadModuleNode(TreeListModules.FocusedNode, True);
end;

procedure TFormMain.HideHint;
begin
  TimerHint.Enabled := False;
  HintStyleController.HideHint;
  FHintVisible := False;
end;

procedure TFormMain.TimerHintTimer(Sender: TObject);

  function UnicodeToRTF(const Value: string): string;
  var
    TruncatedValue: string;
    c, LastChar, NextLastChar: Char;
    ac: AnsiChar;
  begin
    // Truncate to 100 chars
    if (Length(Value) > 100) then
      TruncatedValue := Copy(Value, 1, 97)+'...'
    else
      TruncatedValue := Value;
    Result := '';
    LastChar := #0;
    for c in TruncatedValue do
    begin
      NextLastChar := c;
      case Ord(c) of
        10, 13:
          begin
            if (LastChar <> #13) then
              Result := Result + '\line ';
            NextLastChar := #13;
          end;

        0..9, 11..12, 14..32:
          begin
            if (LastChar <> ' ') then
              Result := Result + ' ';
            NextLastChar := ' ';
          end;

        Ord('\'), Ord('{'), Ord('}'):
          Result := Result + '\'+c;

        128..Ord(High(Char)):
          begin
            if (LocaleCharsFromUnicode(CP_THREAD_ACP, WC_COMPOSITECHECK, @c, 1, @ac, 1, nil, nil) <> 1) then
              ac := '?';
            Result := Result + Format('\u%d%s', [Ord(c), ac]);
          end;

      else
        Result := Result + c;
      end;
      LastChar := NextLastChar;
    end;
  end;

var
  p: TPoint;
  s: string;
  Translations: TStringList;
  HintList: string;
resourcestring
  sTranslationMemoryHintHeader = 'Translation suggestions:';
const
  sTranslationMemoryHintTemplate =
    '{\rtf1\ansi\ansicpg1252'+
    '\deff0'+           // Default font is #0
    // Font table
    '{\fonttbl'+
      '{\f0\f%5:s\fcharset0 Segoe UI;}'+        // Font #0: Native UI language, default char set
      '{\f1\f%6:s\fcharset%1:d Segoe UI;}'+     // Font #1: Target language, target charset
      '{\f2\ftech\fcharset2 Symbol;}'+          // Font #2: Bullets, symbol charset
    '}'+
    // Color table
    '{\colortbl '+
      ';'+
      '\red76\green76\blue76;'+                 // Color #1 (lead text)
      '\red0\green128\blue255;'+                // Color #2 (bullet text)
    '}'+
    '\viewkind4'+       // Normal view
    '\uc1'+             // 1 byte unicode fallback characters
    '\pard\cf1\f0\fs18'+// Paragraph, color #1, font #0, font size 9
    '\lang%0:d'+        // Native language ID (not sure that this is necessary)
    '\%4:spar'+         // Native LTR/RTL paragraph
    ' %2:s\par\pard'+   // Lead text

    '{\pntext\f2\''B7}'+ // Plain text bullet point, font #2
    '{\*\pn'+           // Paragraph numbering
      '\pnlvlblt'+      // Bulleted paragraph
      '\pnf2'+          // Font number 2
      '\pnindent0'+     // Minimum distance from margin to body text.
      '{\pntxtb\''B7}'+ // Text before: the bullet
    '}'+
    '\%4:spar'+         // Native LTR/RTL paragraph
    '\fi-200'+          // First line indent
    '\li200'+           // Left indent
    '\cf2'+             // Foreground color #2
    '\f1'+              // Font #1
    '%3:s'+             // Bullet list
    '}';
  sTranslationMemoryHintListItem =
    '\%1:sch'+          // Target LTR/RTL characters
    ' %0:s'+            // The bullet text
    '\%2:sch'+          // Native LTR/RTL characters
    '\par';
const
  sRtlLtr: array[boolean] of string = ('ltr', 'rtl');
  sRtlLtrFont: array[boolean] of string = ('nil', 'bidi');
begin
  TimerHint.Enabled := False;

  if (FShowHint) and (FHintProp <> nil) then
  begin
    // Only display hint if mouse is still in cell rect
    if (not FHintRect.Contains(GridItems.ScreenToClient(Mouse.CursorPos))) then
      Exit;

    Translations := TStringList.Create;
    try

      if (not FTranslationMemory.FindTranslations(FHintProp, SourceLanguage, TargetLanguage, Translations)) then
        Exit;

      HintList := '';
      for s in Translations do
        HintList := HintList + Format(sTranslationMemoryHintListItem, [UnicodeToRTF(s), sRtlLtr[TargetLanguage.IsRightToLeft], sRtlLtr[IsRightToLeft]]);

    finally
      Translations.Free;
    end;

    ScreenTipTranslationMemory.Description.Text := Format(sTranslationMemoryHintTemplate,
      [GetLanguageID(0), TargetLanguage.CharSet, UnicodeToRTF(sTranslationMemoryHintHeader), HintList,
      sRtlLtr[IsRightToLeft], sRtlLtrFont[IsRightToLeft], sRtlLtrFont[TargetLanguage.IsRightToLeft]]);

    p := GridItems.ClientToScreen(Point(FHintRect.Right+1, FHintRect.Top));

    TdxScreenTipStyle(HintStyleController.HintStyle).ShowScreenTip(p.X, p.Y, ScreenTipTranslationMemory);
    FHintVisible := True;

    // Hide hint again after a while
    FShowHint := False;
    TimerHint.Interval := HintStyleController.HintHidePause;
    TimerHint.Enabled := True;
  end else
    HideHint;
end;

function TFormMain.RecoverUnusedTranslations(OnlyNew: boolean): integer;
var
  CountRecovered: integer;
begin
  CountRecovered := 0;
  FProject.BeginUpdate;
  try
    FProject.Traverse(
      function(Prop: TLocalizerProperty): boolean
      begin
        if (Prop.IsUnused) and (Prop.Translations.Count > 0) and (Prop.EffectiveStatus = ItemStatusTranslate) then
        begin
          // Look for new property with same name and item type within same module
          Prop.Item.Module.Traverse(
            function(InnerProp: TLocalizerProperty): boolean
            var
              i: integer;
            begin
              Result := True;
              if (Prop <> InnerProp) and ((not OnlyNew) or (ItemStateNew in InnerProp.State)) and (InnerProp.EffectiveStatus = ItemStatusTranslate) and
                (InnerProp.Name = Prop.Name) and (InnerProp.Item.TypeName = Prop.Item.TypeName) and
                (Prop.Value = InnerProp.Value) and (InnerProp.Translations.Count = 0) then
              begin
                // Copy translations from unused prop to new prop
                for i := 0 to Prop.Translations.Count-1 do
                  if (Prop.Translations[i].Status <> tStatusPending) then
                  begin
                    Result := False;
                    InnerProp.Translations.AddOrUpdateTranslation(Prop.Translations[i].Language, Prop.Translations[i].Value, Prop.Translations[i].Status);
                  end;

                if (not Result) then
                  Inc(CountRecovered);
              end else
            end);
        end;
        Result := True;
      end);
  finally
    FProject.EndUpdate;
  end;
  Result := CountRecovered;
end;

procedure TFormMain.RefreshModuleStats;
begin
  // Prevent recursion
  if (FRefreshModuleStatsQueued) then
    Exit;

  FRefreshModuleStatsQueued := True;
  PostMessage(Handle, MSG_REFRESH_MODULE_STATS, 0, 0);
end;

procedure TFormMain.DoRefreshModuleStats;
var
  i: integer;
  Module: TLocalizerModule;
  TranslatedCount: integer;
  TranslatableCount: integer;
  PendingCount: integer;
begin
  try
    LayoutControlModules.BeginUpdate;
    try
      // Module stats
      TranslatedCount := 0;
      TranslatableCount := 0;
      for i := 0 to TreeListModules.SelectionCount-1 do
      begin
        Module := TLocalizerModule(TreeListModules.Selections[i].Data);
        Inc(TranslatedCount, Module.TranslatedCount[TranslationLanguage]);
        Inc(TranslatableCount, Module.StatusCount[ItemStatusTranslate]);
      end;
      PendingCount := TranslatableCount - TranslatedCount;

      LabelCountModuleTranslated.Caption := Format('%.0n', [1.0 * TranslatedCount]);
      LabelCountModulePending.Caption := Format('%.0n', [1.0 * PendingCount]);
      if (TranslatedCount <> 0) and (TranslatableCount <> 0) then
        LabelCountModuleTranslatedPercent.Caption := Format('(%.1n%%)', [TranslatedCount/TranslatableCount*100])
      else
        LabelCountModuleTranslatedPercent.Caption := '';

      // Project stats
      TranslatedCount := FProject.TranslatedCount[TranslationLanguage];
      TranslatableCount := FProject.StatusCount[ItemStatusTranslate];
      PendingCount := TranslatableCount - TranslatedCount;

      LabelCountTranslated.Caption := Format('%.0n', [1.0 * TranslatedCount]);
      LabelCountPending.Caption := Format('%.0n', [1.0 * PendingCount]);
      if (TranslatedCount <> 0) and (TranslatableCount <> 0) then
        LabelCountTranslatedPercent.Caption := Format('(%.1n%%)', [TranslatedCount/TranslatableCount*100])
      else
        LabelCountTranslatedPercent.Caption := '';

    finally
      LayoutControlModules.EndUpdate;
    end;
  finally
    FRefreshModuleStatsQueued := False;
  end;
end;

procedure TFormMain.TreeListModulesFocusedColumnChanged(
  Sender: TcxCustomTreeList; APrevFocusedColumn,
  AFocusedColumn: TcxTreeListColumn);
begin
  // Only enable incremental search when Module Name column has focus
  Sender.OptionsBehavior.IncSearch := (AFocusedColumn = TreeListColumnModuleName);
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

  if (not TranslationManagerSettings.Editor.DisplayStatusGlyphs) then
    Exit;

  Module := TLocalizerModule(ANode.Data);

  if (Module.IsUnused) then
    AIndex := NodeImageIndexUnused
  else
  if (Module.EffectiveStatus = ItemStatusTranslate) then
  begin
    TranslatedCount := Module.TranslatedCount[TranslationLanguage];
    // Calculate completeness in %
    if (Module.StatusCount[ItemStatusTranslate] <> 0) then
      Completeness := MulDiv(TranslatedCount, 100, Module.StatusCount[ItemStatusTranslate])
    else
      Completeness := 100;

    if (Completeness = 100) then        // 100% complete
      AIndex := NodeImageIndexTranslated
    else
    if (Completeness >= 66) then        // 66%..99% complete
      AIndex := NodeImageIndexComplete75
    else
    if (Completeness >= 33) then        // 33%..65% complete
      AIndex := NodeImageIndexComplete50
    else
    if (TranslatedCount > 0) then       // >0%..32% complete
      AIndex := NodeImageIndexComplete25
    else
    if (ItemStateNew in Module.State) then
      AIndex := NodeImageIndexNew
    else
      AIndex := NodeImageIndexNotTranslated;
  end else
  if (Module.EffectiveStatus = ItemStatusDontTranslate) then
    AIndex := NodeImageIndexDontTranslate
  else
  if (Module.EffectiveStatus = ItemStatusHold) then
    AIndex := NodeImageIndexHold
  else
    AIndex := -1;
end;

procedure TFormMain.TreeListModulesSelectionChanged(Sender: TObject);
var
  OldValueNameVisible, NewValueNameVisible: boolean;
begin
  if (FTranslationMemoryPeek <> nil) then
    FTranslationMemoryPeek.Cancel;

  SaveCursor(crAppStart);

  GridItemsTableView.BeginUpdate;
  try

    OldValueNameVisible := GridItemsTableViewColumnValueName.Visible;

    if (TreeListModules.SelectionCount = 1) then
      FModuleItemsDataSource.Module := FocusedModule
    else
      // Clear item grid if more than one module is selected
      FModuleItemsDataSource.Module := nil;

    // Hide property name column if module is resourcestrings
    NewValueNameVisible := (FModuleItemsDataSource.Module = nil) or (FModuleItemsDataSource.Module.Kind = mkForm);

    if (OldValueNameVisible <> NewValueNameVisible) then
    begin
      if (NewValueNameVisible) then
      begin
        GridItemsTableViewColumnItemName.Width := GridItemsTableViewColumnItemName.Width - GridItemsTableViewColumnValueName.Width;
        GridItemsTableViewColumnValueName.Visible := True;
      end else
      begin
        GridItemsTableViewColumnValueName.Visible := False;
        GridItemsTableViewColumnItemName.Width := GridItemsTableViewColumnItemName.Width + GridItemsTableViewColumnValueName.Width;
      end;
    end;

  finally
    GridItemsTableView.EndUpdate;
  end;

  GridItemsTableView.Controller.ClearSelection;
  GridItemsTableView.Controller.TopRowIndex := 0;
  GridItemsTableView.Controller.FocusedRowIndex := 0;
  if (GridItemsTableView.Controller.FocusedRecordIndex <> -1) then
    GridItemsTableView.Controller.FocusedRecord.Selected := True;

  RefreshModuleStats;
end;

procedure TFormMain.TreeListModulesStylesGetContentStyle(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; var AStyle: TcxStyle);
var
  Module: TLocalizerModule;
begin
  if (ANode.Selected) and (not Sender.Focused) then
  begin
    AStyle := DataModuleMain.StyleInactive;
    Exit;
  end else
  if (ANode.Selected) and ((AColumn = nil) or (not AColumn.Focused)) then
  begin
    AStyle := DataModuleMain.StyleSelected;
    Exit;
  end else
  if (Sender.Focused) and (ANode.Focused) and (AColumn <> nil) and (AColumn.Focused) and (not AColumn.Editing) then
  begin
    AStyle := DataModuleMain.StyleFocused;
    Exit;
  end;

  Module := TLocalizerModule(ANode.Data);

  if (Module.IsUnused) or (Module.EffectiveStatus = ItemStatusDontTranslate) then
  begin
    AStyle := DataModuleMain.StyleDontTranslate;
    Exit;
  end;

  if (Module.EffectiveStatus = ItemStatusHold) then
  begin
    AStyle := DataModuleMain.StyleHold;
    Exit;
  end;

  if (Module.TranslatedCount[TranslationLanguage] = Module.StatusCount[ItemStatusTranslate]) then
    AStyle := DataModuleMain.StyleComplete
  else
    AStyle := DataModuleMain.StyleNeedTranslation;
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionProofingCheckSelectedExecute(Sender: TObject);
var
  i: integer;
  Item: TCustomLocalizerItem;
  Items: TArray<TCustomLocalizerItem>;
begin
  // TreeList selection will change during spell check so save a static copy before we start
  SetLength(Items, SelectionCount);
  for i := 0 to SelectionCount-1 do
    Items[i] := Selection[i];

  for Item in Items do
  begin
    if (not Item.Traverse(
      function(Prop: TLocalizerProperty): boolean
      begin
        Result := PerformSpellCheck(Prop);
      end)) then
      break;
  end;

  SpellChecker.ShowSpellingCompleteMessage;
end;

procedure TFormMain.ActionProofingCheckSelectedUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FocusedItem <> nil) and
    ((FCanSpellCheck) or (TdxUserSpellCheckerDictionary(SpellChecker.Dictionaries[0]).WordCount > 0));
end;

procedure TFormMain.ActionProofingCheckUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FProject.Modules.Count > 0) and
    ((FCanSpellCheck) or (TdxUserSpellCheckerDictionary(SpellChecker.Dictionaries[0]).WordCount > 0));
end;

// -----------------------------------------------------------------------------
//
// TcxGridTableView redirect
//
// -----------------------------------------------------------------------------
type
  TamGridTableController = class(TcxGridTableController)
  public
    function GetCursor(X, Y: Integer): TCursor; override;
  end;

function TamGridTableController.GetCursor(X, Y: Integer): TCursor;
var
  HitTest: TcxGridRecordCellHitTest;
  r: TRect;
begin
  (*
  ** Display hand point if cursor hovers over TM or Validation indicators
  *)
  HitTest := TcxGridRecordCellHitTest(ViewInfo.GetHitTest(X, Y));

  if (HitTest.HitTestCode <> htCell) or (HitTest.Item <> TFormMain(GridView.Control.Owner).GridItemsTableViewColumnTarget) or (HitTest.Item.Editing) then
  begin
    Result := inherited GetCursor(X, Y);
    Exit;
  end;

  r := HitTest.ViewInfo.Bounds;

  if (TFormMain(GridView.Control.Owner).UseRightToLeftReading) or (TFormMain(GridView.Control.Owner).TargetLanguage.IsRightToLeft and TranslationManagerSettings.Editor.EditBiDiMode) then
    // Top left corner
    r.Right := r.Left + HintCornerSize
  else
    // Top right corner
    r.Left := r.Right - HintCornerSize;

  r.Bottom := r.Top + HintCornerSize;

  if (r.Contains(Point(X, Y))) and (TFormMain(GridView.Control.Owner).FModuleItemsDataSource.PeekResult[HitTest.GridRecord.RecordIndex] = TTranslationMemoryPeekResult.prFound) then
    Result := crHandPoint
  else
  if (HitTest.Item.FocusedCellViewInfo <> nil) and (TcxEditViewInfoCracker(HitTest.Item.FocusedCellViewInfo.EditViewInfo).GetPart(HitTest.Pos) = ecpErrorIcon) then
    Result := crHandPoint
  else
    Result := inherited GetCursor(X, Y);
end;

function TcxGridTableView.GetControllerClass: TcxCustomGridControllerClass;
begin
  Result := TamGridTableController;
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------


initialization
  InitializeExceptionHandler;
end.
