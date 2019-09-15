unit amLocalization.Dialog.Settings;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Generics.Collections,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, System.ImageList, Vcl.ImgList, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, Vcl.ComCtrls,

  dxSkinsCore, dxBarBuiltInMenu, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxLayoutContainer,
  cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxNavigator, dxDateRanges,
  cxDataControllerConditionalFormattingRulesManagerDialog, cxLabel, cxButtonEdit, cxCheckBox, cxMemo, cxImage,
  cxImageList, cxGridCustomPopupMenu, cxGridPopupMenu, dxLayoutLookAndFeels, cxClasses,
  dxGDIPlusClasses, cxGridViewLayoutContainer,
  cxGridLayoutView, cxGridCustomLayoutView, cxTextEdit, cxListView, cxCheckComboBox, cxSpinEdit, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridCustomView, cxGrid, cxDropDownEdit, Vcl.StdCtrls, cxButtons, cxMaskEdit, cxImageComboBox, dxLayoutControl,
  cxPC,

  dxRibbonSkins,
  dxSpellChecker,

  amLocalization.Dialog, cxLookupEdit, cxDBLookupEdit, cxDBExtLookupComboBox;

type
  TFormSettings = class(TFormDialog)
    ActionCategoryGeneral: TAction;
    ActionCategoryFiles: TAction;
    ActionCategoryProofing: TAction;
    ActionCategorySystem: TAction;
    ActionFoldersModify: TAction;
    ActionFolderReset: TAction;
    ActionFoldersExplorer: TAction;
    ActionFolderResetAll: TAction;
    ActionProofingAdd: TAction;
    ActionProofingReplace: TAction;
    ActionProofingDelete: TAction;
    ImageListColorSchemesGlyphsLarge: TcxImageList;
    ImageListColorSchemesGlyphsSmall: TcxImageList;
    ImageListSkin: TImageList;
    ImageListSkinLarge: TImageList;
    LayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    LayoutSkinLookAndFeel: TdxLayoutSkinLookAndFeel;
    LayoutSkinLookAndFeelURL: TdxLayoutSkinLookAndFeel;
    LayoutSkinLookAndFeelTitle: TdxLayoutSkinLookAndFeel;
    PageControl: TcxPageControl;
    TabSheetGeneral: TcxTabSheet;
    LayoutControlGeneral: TdxLayoutControl;
    LabelEditingHeader: TcxLabel;
    CheckUseProposed: TcxCheckBox;
    CheckBoxAtstart: TcxCheckBox;
    cxLabel2: TcxLabel;
    ImageComboBoxSkin: TcxImageComboBox;
    LabelApperance: TcxLabel;
    ButtonDialogsSuppressReset: TcxButton;
    dxLayoutGroup1: TdxLayoutGroup;
    LayoutControlGeneralItem1: TdxLayoutItem;
    LayoutControlGeneralItem2: TdxLayoutItem;
    LayoutControlGroupEditing: TdxLayoutGroup;
    LayoutControlGeneralItem12: TdxLayoutItem;
    LayoutControlGeneralItem13: TdxLayoutItem;
    LayoutControlGroupStartup: TdxLayoutGroup;
    LayoutControlGeneralItem14: TdxLayoutItem;
    LayoutControlGeneralItem16: TdxLayoutItem;
    LayoutControlGroupUserInterface: TdxLayoutGroup;
    LayoutControlGeneralGroup2: TdxLayoutGroup;
    LayoutControlGeneralGroup3: TdxLayoutGroup;
    LayoutControlGeneralItem11: TdxLayoutItem;
    LayoutControlGeneralGroup4: TdxLayoutGroup;
    TabSheetFileLocations: TcxTabSheet;
    TabSheetProofing: TcxTabSheet;
    LayoutControlProofing: TdxLayoutControl;
    CheckBoxProofingIgnoreNumbers: TcxCheckBox;
    cxLabel12: TcxLabel;
    CheckBoxProofingIgnoreRepeatWords: TcxCheckBox;
    ComboBoxProofingLanguages: TcxCheckComboBox;
    cxLabel13: TcxLabel;
    ButtonProofingEditCustomDictionary: TcxButton;
    CheckBoxProofingSpellCheck: TcxCheckBox;
    CheckBoxProofingIgnoreUppercase: TcxCheckBox;
    CheckBoxProofingIgnoreMixedCase: TcxCheckBox;
    CheckBoxProofingCorrectSentenceCaps: TcxCheckBox;
    CheckBoxProofingCorrectCapsLock: TcxCheckBox;
    CheckBoxProofingDisableCapsLock: TcxCheckBox;
    CheckBoxProofingCorrectAutoReplace: TcxCheckBox;
    ListViewProofingAutoCorrectReplacements: TcxListView;
    ButtonProofingAutoCorrectAdd: TcxButton;
    ButtonProofingAutoCorrectDelete: TcxButton;
    CheckBoxProofingCorrectAutomaticallyUseSuggestions: TcxCheckBox;
    EditProofingAutoCorrectReplacementFrom: TcxTextEdit;
    EditProofingAutoCorrectReplacementTo: TcxTextEdit;
    CheckBoxProofingAutoCorrect: TcxCheckBox;
    ButtonProofingAutoCorrectExceptions: TcxButton;
    CheckBoxProofingCorrectInitialCaps: TcxCheckBox;
    LayoutGroupProofing: TdxLayoutGroup;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutGroup6: TdxLayoutGroup;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutItem13: TdxLayoutItem;
    dxLayoutItem14: TdxLayoutItem;
    dxLayoutGroup7: TdxLayoutGroup;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutGroup5: TdxLayoutGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutItem18: TdxLayoutItem;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutItem19: TdxLayoutItem;
    dxLayoutItem20: TdxLayoutItem;
    dxLayoutItem21: TdxLayoutItem;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    EditProofingAutoCorrectReplace: TdxLayoutItem;
    dxLayoutItem17: TdxLayoutItem;
    dxLayoutSeparatorItem2: TdxLayoutSeparatorItem;
    dxLayoutItem22: TdxLayoutItem;
    LayoutGroupProofingAutoCorrect: TdxLayoutGroup;
    dxLayoutItem15: TdxLayoutItem;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    dxLayoutItem12: TdxLayoutItem;
    dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup;
    TabSheetSystem: TcxTabSheet;
    LayoutControlAdvanced: TdxLayoutControl;
    LabelAutoUpdateIntro: TcxLabel;
    CheckBoxSingleInstance: TcxCheckBox;
    cxLabel4: TcxLabel;
    ButtonRegisterFiletypes: TcxButton;
    cxLabel11: TcxLabel;
    CheckBoxAutoUpdateEnabled: TcxCheckBox;
    ButtonAutoUpdateNow: TcxButton;
    ButtonAutoUpdateReset: TcxButton;
    cxLabel14: TcxLabel;
    LayoutControlAdvancedGroup_Root: TdxLayoutGroup;
    LayoutControlAdvancedItem1: TdxLayoutItem;
    LayoutControlAdvancedItem2: TdxLayoutItem;
    LayoutControlAdvancedItem3: TdxLayoutItem;
    LayoutControlAdvancedGroup2: TdxLayoutGroup;
    LayoutControlAdvancedGroup4: TdxLayoutGroup;
    LayoutControlAdvancedItem6: TdxLayoutItem;
    LayoutControlAdvancedItem7: TdxLayoutItem;
    LayoutControlAdvancedItem8: TdxLayoutItem;
    LayoutControlAdvancedGroup5: TdxLayoutGroup;
    LayoutControlAdvancedGroup6: TdxLayoutGroup;
    LayoutControlAdvancedItem9: TdxLayoutItem;
    LayoutControlAdvancedItem10: TdxLayoutItem;
    LayoutControlAdvancedItem17: TdxLayoutItem;
    LayoutControlAdvancedGroup7: TdxLayoutGroup;
    LayoutControlAdvancedGroup9: TdxLayoutGroup;
    PanelCategory: TPanel;
    ButtonCategoryGeneral: TcxButton;
    ButtonCategoryFiles: TcxButton;
    ButtonCategoryAdvanced: TcxButton;
    ButtonCategoryProofing: TcxButton;
    PopupMenuFolderReset: TPopupMenu;
    Reset1: TMenuItem;
    Resetall1: TMenuItem;
    StyleRepository: TcxStyleRepository;
    StyleBackground: TcxStyle;
    StyleDisabled: TcxStyle;
    TabSheetTranslationServices: TcxTabSheet;
    ButtonCategoryTranslators: TcxButton;
    ActionCategoryTranslators: TAction;
    LayoutControlTranslatorsGroup_Root: TdxLayoutGroup;
    LayoutControlTranslators: TdxLayoutControl;
    dxLayoutItem16: TdxLayoutItem;
    LabelTranslatorTM: TcxLabel;
    LayoutGroupTranslatorTM: TdxLayoutGroup;
    dxLayoutItem23: TdxLayoutItem;
    CheckBoxTMLoadOnDemand: TcxCheckBox;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutItem24: TdxLayoutItem;
    LabelTransalatorMS: TcxLabel;
    LayoutGroupTranslatorMS: TdxLayoutGroup;
    dxLayoutItem25: TdxLayoutItem;
    EditTranslatorMSAPIKey: TcxButtonEdit;
    dxLayoutGroup8: TdxLayoutGroup;
    LayoutSkinLookAndFeelGroup: TdxLayoutSkinLookAndFeel;
    ImageList: TcxImageList;
    LayoutItemRestart: TdxLayoutLabeledItem;
    LayoutSkinLookAndFeelStandard: TdxLayoutSkinLookAndFeel;
    LayoutGroupRestart: TdxLayoutGroup;
    dxLayoutItem29: TdxLayoutItem;
    LabelLanguage: TcxLabel;
    LayoutGroupLanguage: TdxLayoutGroup;
    dxLayoutGroup10: TdxLayoutGroup;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    LayoutControlFilesGroup_Root: TdxLayoutGroup;
    LayoutControlFiles: TdxLayoutControl;
    dxLayoutItem31: TdxLayoutItem;
    cxLabel5: TcxLabel;
    dxLayoutItem32: TdxLayoutItem;
    GridFolders: TcxGrid;
    GridFoldersTableView: TcxGridTableView;
    GridFoldersTableViewColumnName: TcxGridColumn;
    GridFoldersTableViewColumnPath: TcxGridColumn;
    GridFoldersTableViewColumnReadOnly: TcxGridColumn;
    GridFoldersLevel: TcxGridLevel;
    dxLayoutItem33: TdxLayoutItem;
    ButtonFilesReset: TcxButton;
    dxLayoutItem34: TdxLayoutItem;
    ButtonFilesModify: TcxButton;
    dxLayoutGroup11: TdxLayoutGroup;
    dxLayoutGroup12: TdxLayoutGroup;
    dxLayoutGroup13: TdxLayoutGroup;
    dxLayoutItem35: TdxLayoutItem;
    cxLabel6: TcxLabel;
    dxLayoutItem36: TdxLayoutItem;
    CheckBoxAutoRecovery: TcxCheckBox;
    dxLayoutItem37: TdxLayoutItem;
    EditAutoRecoveryInterval: TcxSpinEdit;
    LayoutGroupRecovery: TdxLayoutGroup;
    dxLayoutGroup14: TdxLayoutGroup;
    dxLayoutItem39: TdxLayoutItem;
    cxLabel10: TcxLabel;
    dxLayoutItem40: TdxLayoutItem;
    CheckBoxHistoryBackup: TcxCheckBox;
    LayoutGroupBackup: TdxLayoutGroup;
    dxLayoutGroup15: TdxLayoutGroup;
    dxLayoutItem41: TdxLayoutItem;
    EditHistoryBackupMaxFiles: TcxSpinEdit;
    dxLayoutItem42: TdxLayoutItem;
    EditHistoryBackupMaxSize: TcxSpinEdit;
    dxLayoutGroup16: TdxLayoutGroup;
    dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem;
    dxLayoutSeparatorItem3: TdxLayoutSeparatorItem;
    LayoutItemCategories: TdxLayoutItem;
    LayoutItemPages: TdxLayoutItem;
    dxLayoutItem26: TdxLayoutItem;
    LabelResourceModuleHeader: TcxLabel;
    LayoutGroupResourceModules: TdxLayoutGroup;
    dxLayoutItem27: TdxLayoutItem;
    CheckBoxResourceModulesIncludeVersionInfo: TcxCheckBox;
    dxLayoutGroup9: TdxLayoutGroup;
    dxLayoutItem43: TdxLayoutItem;
    ComboBoxSourceLanguage: TcxExtLookupComboBox;
    dxLayoutSeparatorItem4: TdxLayoutSeparatorItem;
    dxLayoutItem28: TdxLayoutItem;
    ComboBoxTargetLanguage: TcxExtLookupComboBox;
    dxLayoutItem30: TdxLayoutItem;
    ComboBoxApplicationLanguage: TcxExtLookupComboBox;
    dxLayoutItem38: TdxLayoutItem;
    CheckBoxTMBackgroundQuery: TcxCheckBox;
    procedure TextEditTranslatorMSAPIKeyPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure TextEditTranslatorMSAPIKeyPropertiesChange(Sender: TObject);
    procedure ActionCategoryExecute(Sender: TObject);
    procedure ActionCategoryUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonCategoryEnter(Sender: TObject);
    procedure ButtonProofingAutoCorrectExceptionsClick(Sender: TObject);
    procedure ButtonProofingEditCustomDictionaryClick(Sender: TObject);
    procedure CheckBoxProofingAutoCorrectPropertiesChange(Sender: TObject);
    procedure EditProofingAutoCorrectReplacementFromPropertiesChange(Sender: TObject);
    procedure ActionProofingAddExecute(Sender: TObject);
    procedure ActionProofingAddUpdate(Sender: TObject);
    procedure ActionProofingDeleteExecute(Sender: TObject);
    procedure ActionProofingDeleteUpdate(Sender: TObject);
    procedure ListViewProofingAutoCorrectReplacementsClick(Sender: TObject);
    procedure ActionProofingReplaceExecute(Sender: TObject);
    procedure ActionProofingReplaceUpdate(Sender: TObject);
    procedure ActionFolderResetUpdate(Sender: TObject);
    procedure ActionFolderResetExecute(Sender: TObject);
    procedure ActionFolderResetAllExecute(Sender: TObject);
    procedure ActionFoldersExplorerExecute(Sender: TObject);
    procedure ActionFoldersModifyExecute(Sender: TObject);
    procedure ActionFoldersModifyUpdate(Sender: TObject);
    procedure GridFoldersTableViewColumnPathGetDisplayText(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
      var AText: string);
    procedure GridFoldersTableViewColumnPathGetCellHint(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
      ACellViewInfo: TcxGridTableDataCellViewInfo; const AMousePos: TPoint; var AHintText: TCaption; var AIsHintMultiLine: Boolean;
      var AHintTextRect: TRect);
    procedure GridFoldersTableViewCellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo;
      AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
    procedure GridFoldersTableViewColumnPathPropertiesEditValueChanged(Sender: TObject);
    procedure GridFoldersTableViewEditing(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem; var AAllow: Boolean);
    procedure GridFoldersTableViewStylesGetContentStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
      AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
    procedure ComboBoxApplicationLanguagePropertiesEditValueChanged(Sender: TObject);
    procedure CheckBoxSingleInstancePropertiesChange(Sender: TObject);
    procedure ButtonRegisterFiletypesClick(Sender: TObject);
  private
    type
      TSkinDetails = record
        Name: string;
        Group: string;
        DisplayName: string;
        Filename: string;
        Index: integer;
        GlyphSmall: TBitmap;
        GlyphLarge: TBitmap;
      end;
  private
    FSpellCheckerAutoCorrectOptions: TdxSpellCheckerAutoCorrectOptions;
    FSkinList: TList<TSkinDetails>;
    FRestartRequired: boolean;
    FSpellChecker: TdxSpellChecker;
    FRibbonStyle: TdxRibbonStyle;
  private
    procedure PopulateSkins;
    function GetSkin: string;
    procedure SetSkin(const value: string);
  protected
    procedure RequireRestart;
  protected
    procedure LoadProofing(SpellChecker: TdxSpellChecker);
    procedure ApplyProofing(SpellChecker: TdxSpellChecker);

    procedure LoadFolders;
    procedure ApplyFolders;
  protected
    procedure LoadSettings;
    procedure ApplySettings;
  public
    constructor Create(Awner: TComponent); override;
    destructor Destroy; override;

    function Execute: boolean;

    property SpellChecker: TdxSpellChecker read FSpellChecker write FSpellChecker;
    property RibbonStyle: TdxRibbonStyle read FRibbonStyle write FRibbonStyle;

    property RestartRequired: boolean read FRestartRequired;
  end;

implementation

{$R *.dfm}

uses
  Generics.Defaults,
  Types, UITypes,
  IOUtils,

(*
  dxSkinsDefaultPainters,
  dxSkinsLookAndFeelPainter,
*)
  dxSkinsdxRibbonPainter,

  dxSpellCheckerCore,
  dxSpellCheckerDialogs,
  dxSpellCheckerAutoCorrectOptionsDialog,

  cxStorage,

  amShell,
  amCursorService,
  amLocalization.Model,
  amLocalization.Settings,
  amLocalization.Persistence,
  amLocalization.Utils,
  amLocalization.Shell,
  amLocalization.Data.Main,
  amLocalization.Translator.Microsoft.Version3;

const
  FolderOrder: array[Ord(Low(TTranslationManagerFolder))..Ord(High(TTranslationManagerFolder))] of TTranslationManagerFolder =
    (tmFolderSkins, tmFolderUserSkins, tmFolderSpellCheck, tmFolderUserSpellCheck);

{ TFormSettings }

// -----------------------------------------------------------------------------

type
  TcxExtLookupComboBoxCracker = class(TcxExtLookupComboBox);

constructor TFormSettings.Create(Awner: TComponent);
resourcestring
  sLanguageSystemDefault = '(system default)';
begin
  inherited;

  FSkinList := TList<TSkinDetails>.Create;

  TcxExtLookupComboBoxCracker(ComboBoxSourceLanguage).TextHint := sLanguageSystemDefault;
  TcxExtLookupComboBoxCracker(ComboBoxTargetLanguage).TextHint := sLanguageSystemDefault;
  TcxExtLookupComboBoxCracker(ComboBoxApplicationLanguage).TextHint := sLanguageSystemDefault;
end;

destructor TFormSettings.Destroy;
begin
  FSpellCheckerAutoCorrectOptions.Free;
  FSkinList.Free;

  inherited;
end;

// -----------------------------------------------------------------------------

procedure TFormSettings.FormCreate(Sender: TObject);
var
  i: integer;
  Button: TcxButton;
  Action: TAction;
  Index: integer;
begin
  // Make sure Actions, Buttons and Tabsheet pages are in sync
  for i := 0 to PanelCategory.ComponentCount-1 do
    if (PanelCategory.Components[i] is TcxButton) then
    begin
      Button := TcxButton(PanelCategory.Components[i]);
      Action := TAction(Button.Action);

      // Button tab order must equal Tabsheet order.
      // - Button.TabOrder controls shortcut.
      // - Action.Visible controls visibility.
      Index := Button.TabOrder;
      Action.Tag := Index;
      Action.ShortCut := ShortCut(Ord('1')+Index, [ssCtrl]);
      Action.Enabled := Action.Visible;
      Button.Visible := Action.Visible;
    end;

  PageControl.HideTabs := True;
  PageControl.ActivePage := TabSheetGeneral;

  LayoutSkinLookAndFeelTitle.ItemOptions.CaptionOptions.Font.Assign(Font);
  LayoutSkinLookAndFeelTitle.ItemOptions.CaptionOptions.Font.Style := [fsBold];
  LayoutSkinLookAndFeelURL.ItemOptions.CaptionOptions.Font.Assign(LayoutSkinLookAndFeelTitle.ItemOptions.CaptionOptions.Font);

  PopulateSkins;

  GridFoldersTableView.DataController.RecordCount := Length(FolderOrder);
  GridFoldersTableView.DataController.BeginUpdate;
  try
    for i := Low(FolderOrder) to High(FolderOrder) do
    begin
      GridFoldersTableView.DataController.Values[i, 0] := sFolderDisplayName[FolderOrder[i]];
      GridFoldersTableView.DataController.Values[i, 2] := False; // Read-only
    end;
  finally
    GridFoldersTableView.DataController.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

function TFormSettings.Execute: boolean;
begin
  FSpellCheckerAutoCorrectOptions := TdxSpellCheckerAutoCorrectOptions.Create(FSpellChecker);

  LoadSettings;

  Result := (ShowModal = mrOK);

  if (Result) then
    ApplySettings;
end;

// -----------------------------------------------------------------------------

procedure TFormSettings.LoadSettings;
begin
  (*
  ** General section
  *)
  SetSkin(TranslationManagerSettings.System.Skin);
  CheckUseProposed.Checked := TranslationManagerSettings.System.UseProposedStatus;
  CheckBoxResourceModulesIncludeVersionInfo.Checked := TranslationManagerSettings.System.IncludeVersionInfo;
  ComboBoxSourceLanguage.EditValue := TranslationManagerSettings.System.DefaultSourceLanguage;
  ComboBoxTargetLanguage.EditValue := TranslationManagerSettings.System.DefaultTargetLanguage;
  ComboBoxApplicationLanguage.EditValue := TranslationManagerSettings.System.ApplicationLanguage;

  (*
  ** Translators section
  *)
  CheckBoxTMLoadOnDemand.Checked := TranslationManagerSettings.Translators.TranslationMemory.LoadOnDemand;
  CheckBoxTMBackgroundQuery.Checked := TranslationManagerSettings.Translators.TranslationMemory.BackgroundQuery;

  EditTranslatorMSAPIKey.Text := TranslationManagerSettings.Translators.MicrosoftV3.APIKey;
  if (TranslationManagerSettings.Translators.MicrosoftV3.APIKeyValidated) then
    EditTranslatorMSAPIKey.Properties.Buttons[0].ImageIndex := 1;

  (*
  ** Files section
  *)
  LoadFolders;

  (*
  ** Proofing section
  *)
  LoadProofing(FSpellChecker);

  (*
  ** Advanced section
  *)
  CheckBoxSingleInstance.Checked := TranslationManagerSettings.System.SingleInstance;

end;

procedure TFormSettings.ApplySettings;

  function VarToLCID(const Value: Variant): LCID;
  begin
    if (VarIsOrdinal(Value)) then
      Result := Value
    else
      Result := 0;
  end;

begin
  (*
  ** General section
  *)
  TranslationManagerSettings.System.Skin := GetSkin;
  TranslationManagerSettings.System.UseProposedStatus := CheckUseProposed.Checked;
  if (TranslationManagerSettings.System.UseProposedStatus) then
    TLocalizerTranslations.DefaultStatus := tStatusProposed
  else
    TLocalizerTranslations.DefaultStatus := tStatusTranslated;
  TranslationManagerSettings.System.IncludeVersionInfo := CheckBoxResourceModulesIncludeVersionInfo.Checked;
  TranslationManagerSettings.System.DefaultSourceLanguage := VarToLCID(ComboBoxSourceLanguage.EditValue);
  TranslationManagerSettings.System.DefaultTargetLanguage := VarToLCID(ComboBoxTargetLanguage.EditValue);
  TranslationManagerSettings.System.ApplicationLanguage := VarToLCID(ComboBoxApplicationLanguage.EditValue);

  (*
  ** Translators section
  *)
  TranslationManagerSettings.Translators.TranslationMemory.LoadOnDemand := CheckBoxTMLoadOnDemand.Checked;
  TranslationManagerSettings.Translators.TranslationMemory.BackgroundQuery := CheckBoxTMBackgroundQuery.Checked;

  TranslationManagerSettings.Translators.MicrosoftV3.APIKey := EditTranslatorMSAPIKey.Text;
  TranslationManagerSettings.Translators.MicrosoftV3.APIKeyValidated := (EditTranslatorMSAPIKey.Properties.Buttons[0].ImageIndex = 1);

  (*
  ** Files section
  *)
  ApplyFolders;

  (*
  ** Proofing section
  *)
  ApplyProofing(FSpellChecker);
  TranslationManagerSettings.Proofing.SaveFrom(FSpellChecker);

  (*
  ** Advanced section
  *)
  TranslationManagerSettings.System.SingleInstance := CheckBoxSingleInstance.Checked;
end;

// -----------------------------------------------------------------------------

function TFormSettings.GetSkin: string;
var
  SkinDetails: TSkinDetails;
begin
  if (ImageComboBoxSkin.ItemIndex <> -1) then
  begin
    SkinDetails := FSkinList[ImageComboBoxSkin.ItemIndex];
    Result := ComposeSkinName(SkinDetails.Name, SkinDetails.Filename, SkinDetails.Index);
  end else
    Result := '';
end;

procedure TFormSettings.SetSkin(const Value: string);
var
  Name, Filename: string;
  Index: integer;
  i: integer;
begin
  DecomposeSkinName(Value, Name, Filename, Index);

  if (Filename <> '') then
  begin
    // Look for exact filename and name match
    for i := 0 to FSkinList.Count-1 do
      if (AnsiSameText(Filename, FSkinList[i].Filename)) and (AnsiSameText(Name, FSkinList[i].Name)) then
      begin
        ImageComboBoxSkin.ItemIndex := i;
        exit;
      end;

    // Look for exact filename and index match
    if (Index <> -1) then
      for i := 0 to FSkinList.Count-1 do
        if (AnsiSameText(Filename, FSkinList[i].Filename)) and (Index = FSkinList[i].Index) then
        begin
          ImageComboBoxSkin.ItemIndex := i;
          exit;
        end;

    // A bit more expensive, but resilient to folder change
    Filename := TPath.GetFileName(Filename);
    for i := 0 to FSkinList.Count-1 do
      if (AnsiSameText(Filename, TPath.GetFileName(FSkinList[i].Filename))) and (AnsiSameText(Name, FSkinList[i].Name)) then
      begin
        ImageComboBoxSkin.ItemIndex := i;
        exit;
      end;
  end;

  // Compare name, ignore filename and index
  for i := 0 to FSkinList.Count-1 do
    if (AnsiSameText(Name, FSkinList[i].Name)) then
    begin
      ImageComboBoxSkin.ItemIndex := i;
      exit;
    end;
end;

// -----------------------------------------------------------------------------

procedure TFormSettings.GridFoldersTableViewCellDblClick(Sender: TcxCustomGridTableView;
  ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
begin
  if (ssCtrl in AShift) then
    ActionFoldersExplorer.Execute
  else
    ActionFoldersModify.Execute;
end;

procedure TFormSettings.GridFoldersTableViewColumnPathGetCellHint(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
  ACellViewInfo: TcxGridTableDataCellViewInfo; const AMousePos: TPoint; var AHintText: TCaption; var AIsHintMultiLine: Boolean;
  var AHintTextRect: TRect);
begin
  AHintText := VarToStr(ARecord.Values[Sender.Index]);
end;

procedure TFormSettings.GridFoldersTableViewColumnPathGetDisplayText(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
  var AText: string);
var
  r: TRect;
  Width: integer;
begin
  r := ARecord.ViewInfo.GetBoundsForItem(Sender);
  Width := R.Right - r.Left - GetSystemMetrics(SM_CXVSCROLL);

  AText := cxGetStringAdjustedToWidth(GridFolders.Canvas.Handle, GridFolders.Font.Handle, AText, Width, mstPathEllipsis);
end;

procedure TFormSettings.GridFoldersTableViewColumnPathPropertiesEditValueChanged(Sender: TObject);
begin
  if (GridFoldersTableView.DataController.FocusedRecordIndex = -1) then
    exit;
  if (TTranslationManagerFolder(GridFoldersTableView.DataController.FocusedRecordIndex) in [tmFolderSpellCheck]) then
    RequireRestart;
end;

procedure TFormSettings.GridFoldersTableViewEditing(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem;
  var AAllow: Boolean);
begin
  AAllow := not GridFoldersTableView.DataController.Values[GridFoldersTableView.DataController.FocusedRecordIndex, 2];
end;

procedure TFormSettings.GridFoldersTableViewStylesGetContentStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
begin
  if (AItem.Index = 1) and (not VarIsNull(ARecord.Values[2])) and (ARecord.Values[2]) then
    AStyle := StyleDisabled;
end;

// -----------------------------------------------------------------------------

procedure TFormSettings.ActionCategoryExecute(Sender: TObject);
begin
  PageControl.ActivePageIndex := TAction(Sender).Tag;
end;

procedure TFormSettings.ActionCategoryUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := (TAction(Sender).Tag = PageControl.ActivePageIndex);
end;

// -----------------------------------------------------------------------------

procedure TFormSettings.ActionFolderResetAllExecute(Sender: TObject);
var
  i: integer;
begin
  for i := Low(FolderOrder) to High(FolderOrder) do
    GridFoldersTableView.DataController.Values[i, 1] := TranslationManagerSettings.Folders.Folder[FolderOrder[i]];
end;

procedure TFormSettings.ActionFolderResetExecute(Sender: TObject);
begin
  GridFoldersTableView.Controller.FocusedRecord.Values[1] := TranslationManagerSettings.Folders.Folder[FolderOrder[GridFoldersTableView.Controller.FocusedRecord.Index]];
end;

procedure TFormSettings.ActionFolderResetUpdate(Sender: TObject);
begin
  // BUGBUGBUG: Short circuit boolean evaluation doesn't work when second value is a variant...
  // Above isn't relevant anymore, but keeping it for future reference

  TAction(Sender).Enabled := (GridFoldersTableView.Controller.FocusedRecord <> nil);
end;

// -----------------------------------------------------------------------------

procedure TFormSettings.ActionFoldersExplorerExecute(Sender: TObject);
var
  Folder: string;
resourcestring
  sCreateFileLocationPath = 'The specified folder does not exist.'+#13#13+'Would you like to create the folder now?';
begin
  Folder := VarToStr(GridFoldersTableView.Controller.FocusedRecord.Values[1]);

  if (Folder = '') then
    Exit;

  if (not TDirectory.Exists(Folder)) then
  begin
    if (MessageDlg(sCreateFileLocationPath, mtConfirmation, mbYesNo, -1) <> mrYes) then
      Exit;

    try

      TDirectory.CreateDirectory(Folder);

    except
      on E: Exception do
      begin
        ShowMessage(E.Message);
        Exit;
      end;
    end;
  end;

  Shell.DisplayFile(Folder, Self)
end;

// -----------------------------------------------------------------------------

procedure TFormSettings.ActionFoldersModifyExecute(Sender: TObject);
var
  Path: string;
  Title: string;
  FileOpenDialog: TFileOpenDialog;
begin
  Title := VarToStr(GridFoldersTableView.Controller.FocusedRecord.Values[0]);
  Path := VarToStr(GridFoldersTableView.Controller.FocusedRecord.Values[1]);

(*
  if (CheckWin32Version(6, 0)) then
  begin
*)
    // Select folder using IFileDialog on Vista+
    FileOpenDialog := TFileOpenDialog.Create(nil);
    try
      FileOpenDialog.Options := [fdoPickFolders, fdoPathMustExist, fdoForceFileSystem];
      FileOpenDialog.DefaultFolder := ExtractFilePath(ExcludeTrailingPathDelimiter(Path));
      FileOpenDialog.FileName := ExtractFileName(ExcludeTrailingPathDelimiter(Path));
      if (not FileOpenDialog.Execute(Handle)) then
        exit;
      Path := FileOpenDialog.FileName;
    finally
      FileOpenDialog.Free;
    end;
(*
  end else
  begin
    // Select folder using SHBrowseForFolder on XP
    if (not SelectDirectory(Title, '', Path, [sdNewUI, sdNewFolder, sdShowEdit, sdValidateDir], Self)) then
      exit;
  end;
*)

  GridFoldersTableView.Controller.FocusedRecord.Values[1] := Path;
end;

procedure TFormSettings.ActionFoldersModifyUpdate(Sender: TObject);
var
  b: boolean;
  v: Variant;
begin
  // BUGBUGBUG: Short circuit boolean evaluation doesn't work when second value is a variant...
  b := (GridFoldersTableView.Controller.FocusedRecord <> nil);
  if (b) then
  begin
    v := GridFoldersTableView.Controller.FocusedRecord.Values[2];
    if (not VarIsNull(v)) then
      b := (not v);
  end;
  TAction(Sender).Enabled := b;
end;

// -----------------------------------------------------------------------------

procedure TFormSettings.ButtonCategoryEnter(Sender: TObject);
begin
  PageControl.ActivePageIndex := TcxButton(Sender).TabOrder;
end;

// -----------------------------------------------------------------------------

procedure TFormSettings.ActionProofingAddExecute(Sender: TObject);
var
  Item: TListItem;
begin
  FSpellCheckerAutoCorrectOptions.Replacements.Add(EditProofingAutoCorrectReplacementFrom.Text, EditProofingAutoCorrectReplacementTo.Text);

  ListViewProofingAutoCorrectReplacements.Items.BeginUpdate;
  try
    Item := ListViewProofingAutoCorrectReplacements.Items.Add;
    Item.Caption := EditProofingAutoCorrectReplacementFrom.Text;
    Item.SubItems.Add(EditProofingAutoCorrectReplacementTo.Text);
    Item.Selected := True;
  finally
    ListViewProofingAutoCorrectReplacements.Items.EndUpdate;
  end;
end;

procedure TFormSettings.ActionProofingAddUpdate(Sender: TObject);
var
  Replacement: TdxSpellCheckerReplacement;
begin
  Replacement := FSpellCheckerAutoCorrectOptions.Replacements.FindReplacement(EditProofingAutoCorrectReplacementFrom.Text);
  TAction(Sender).Enabled := (EditProofingAutoCorrectReplacementFrom.Text <> '') and (Replacement = nil);
end;

procedure TFormSettings.ButtonProofingAutoCorrectExceptionsClick(Sender: TObject);
var
  Dialog: TdxCustomSpellCheckerAutoCorrectForm;
begin
  Dialog := dxSpellCheckerAutoCorrectExceptionsDialogClass.CreateEx(FSpellCheckerAutoCorrectOptions);
  try
    SetControlLookAndFeel(Dialog, nil);

    if (Dialog.ShowModal <> mrOK) then
      exit;

  finally
    Dialog.Free;
  end;
end;

procedure TFormSettings.ActionProofingDeleteExecute(Sender: TObject);
var
  Item: TListItem;
  Replacement: TdxSpellCheckerReplacement;
begin
  Replacement := FSpellCheckerAutoCorrectOptions.Replacements.FindReplacement(EditProofingAutoCorrectReplacementFrom.Text);
  ASSERT(Replacement <> nil);
  FSpellCheckerAutoCorrectOptions.Replacements.Remove(Replacement);

  ListViewProofingAutoCorrectReplacements.Items.BeginUpdate;
  try
    for Item in ListViewProofingAutoCorrectReplacements.Items do
      if (AnsiSameText(EditProofingAutoCorrectReplacementFrom.Text, Item.Caption)) then
      begin
        Item.Free;
        break;
      end;
  finally
    ListViewProofingAutoCorrectReplacements.Items.EndUpdate;
  end;
end;

procedure TFormSettings.ActionProofingDeleteUpdate(Sender: TObject);
var
  Replacement: TdxSpellCheckerReplacement;
begin
  Replacement := FSpellCheckerAutoCorrectOptions.Replacements.FindReplacement(EditProofingAutoCorrectReplacementFrom.Text);
  TAction(Sender).Enabled := (EditProofingAutoCorrectReplacementFrom.Text <> '') and (Replacement <> nil);
end;

procedure TFormSettings.ActionProofingReplaceExecute(Sender: TObject);
var
  Item: TListItem;
  Replacement: TdxSpellCheckerReplacement;
begin
  Replacement := FSpellCheckerAutoCorrectOptions.Replacements.FindReplacement(EditProofingAutoCorrectReplacementFrom.Text);
  ASSERT(Replacement <> nil);
  Replacement.ChangeReplacement(EditProofingAutoCorrectReplacementTo.Text);

  ListViewProofingAutoCorrectReplacements.Items.BeginUpdate;
  try
    for Item in ListViewProofingAutoCorrectReplacements.Items do
      if (AnsiSameText(EditProofingAutoCorrectReplacementFrom.Text, Item.Caption)) then
      begin
        Item.Caption := Replacement.Text;
        Item.SubItems[0] := Replacement.Replacement;
        break;
      end;
  finally
    ListViewProofingAutoCorrectReplacements.Items.EndUpdate;
  end;
end;

procedure TFormSettings.ActionProofingReplaceUpdate(Sender: TObject);
var
  Replacement: TdxSpellCheckerReplacement;
begin
  Replacement := FSpellCheckerAutoCorrectOptions.Replacements.FindReplacement(EditProofingAutoCorrectReplacementFrom.Text);
  TAction(Sender).Enabled := (EditProofingAutoCorrectReplacementFrom.Text <> '') and (Replacement <> nil) and (not AnsiSameText(Replacement.Replacement, EditProofingAutoCorrectReplacementTo.Text));
end;

procedure TFormSettings.ListViewProofingAutoCorrectReplacementsClick(Sender: TObject);
var
  Item: TListItem;
begin
  Item := ListViewProofingAutoCorrectReplacements.Selected;
  if (Item = nil) then
    exit;

  EditProofingAutoCorrectReplacementFrom.Text := Item.Caption;
  if (Item.SubItems.Count > 0) then
    EditProofingAutoCorrectReplacementTo.Text := Item.SubItems[0];
end;

procedure TFormSettings.ButtonProofingEditCustomDictionaryClick(Sender: TObject);
begin
  dxShowCustomDictionaryDialog(FSpellChecker.FindFirstEnabledUserDictionary);
end;

procedure TFormSettings.ButtonRegisterFiletypesClick(Sender: TObject);
resourcestring
  sFileTypeRegistrationSuccess = 'The translation manager file types are now registered.';
  sFileTypeRegistrationError = 'The translation manager file types could not be registered:'+#13+'%s'+#13#13+'Please run %s with administrative privileges and try again';
begin
  SaveCursor(crHourGlass);
  try
    TranslationManagerShell.RegisterShellIntegration;

    // Register COM server.
    // ComServer.UpdateRegistry(True);

    MessageDlg(sFileTypeRegistrationSuccess, mtInformation, [mbOk], 0);
  except
    on E: Exception do
      MessageDlg(Format(sFileTypeRegistrationError, [E.Message, TPath.GetFileNameWithoutExtension(Application.ExeName)]), mtError, [mbOK], 0);
  end;
end;

procedure TFormSettings.CheckBoxProofingAutoCorrectPropertiesChange(Sender: TObject);
begin
  LayoutGroupProofingAutoCorrect.Enabled := TcxCheckBox(Sender).Checked;
end;

procedure TFormSettings.CheckBoxSingleInstancePropertiesChange(Sender: TObject);
begin
  RequireRestart;
end;

procedure TFormSettings.ComboBoxApplicationLanguagePropertiesEditValueChanged(Sender: TObject);
begin
  RequireRestart;
end;

procedure TFormSettings.EditProofingAutoCorrectReplacementFromPropertiesChange(Sender: TObject);
var
  i: integer;
  Selected: integer;
begin
  Selected := -1;
  for i := 0 to ListViewProofingAutoCorrectReplacements.Items.Count-1 do
    if (AnsiSameText(ListViewProofingAutoCorrectReplacements.Items[i].Caption, EditProofingAutoCorrectReplacementFrom.Text)) then
    begin
      Selected := i;
      break;
    end;

  ListViewProofingAutoCorrectReplacements.ItemIndex := Selected;

  if (ListViewProofingAutoCorrectReplacements.Selected <> nil) then
    ButtonProofingAutoCorrectAdd.Action := ActionProofingReplace
  else
    ButtonProofingAutoCorrectAdd.Action := ActionProofingAdd;
end;

// -----------------------------------------------------------------------------

procedure TFormSettings.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_TAB) and (ssCtrl in Shift) then
  begin
    Key := 0;
    PageControl.ActivePage := PageControl.FindNextPageEx(PageControl.ActivePage, not(ssShift in Shift), True, True);
  end else
(*
  if (Key = VK_F1) and (Shift = [ssAlt]) then
  begin
    Key := 0;
    LayoutControlGroupDevelopment.Visible := True;
  end else
*)
    inherited;
end;

// -----------------------------------------------------------------------------

type
  TSkinFolder = (SkinFolderUser, SkinFolderSystem);

procedure TFormSettings.PopulateSkins;
const
  ColorSchemeNameMap: array[0..7] of string = (
    'Blue', 'Black', 'Silver', 'DarkGray', 'LightGray', 'White', 'MediumGray', 'Colorful' );
  ColorSchemeDisplayNameMap: array[0..7] of string = (
    'Blue', 'Black', 'Silver', 'Dark Gray', 'Light Gray', 'White', 'Medium Gray', 'Colorful');
  ColorSchemesGroupName = 'Ribbon Color Schemes';

  procedure LoadSkinDetails(dxSkinDetails: TdxSkinDetails; var SkinDetails: TSkinDetails);
  begin
    SkinDetails.Filename := '';
    SkinDetails.Index := -1;
    SkinDetails.Name := dxSkinDetails.Name;
    SkinDetails.DisplayName := dxSkinDetails.DisplayName;
    SkinDetails.Group := dxSkinDetails.GroupName;
    SkinDetails.GlyphSmall := dxSkinDetails.Icons[sis16].GetAsBitmap;
    SkinDetails.GlyphLarge := dxSkinDetails.Icons[sis48].GetAsBitmap;
  end;

  procedure LoadExternalSkinDetails(const Filename: string);
  var
    Reader: TdxSkinBinaryReader;
    i: Integer;
    FileStream: TFileStream;
    SkinDetails: TSkinDetails;
  begin
    FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      Reader := TdxSkinBinaryReader.Create(FileStream);
      try
        for i := 0 to Reader.Count - 1 do
        begin
          LoadSkinDetails(Reader.SkinDetails[i], SkinDetails);

          SkinDetails.Filename := Filename;
          SkinDetails.Index := i;

          FSkinList.Add(SkinDetails);
        end;
      finally
        Reader.Free;
      end;
    finally
      FileStream.Free;
    end;
  end;

  function SkinFolder(Folder: TSkinFolder): string;
  begin
    case Folder of
      SkinFolderUser:
        Result := TranslationManagerSettings.Folders.FolderUserSkins;
      SkinFolderSystem:
        Result := TranslationManagerSettings.Folders.FolderSkins;
    end;
  end;

  procedure LoadSkinFolder(const Folder: string);
  var
    Filename: string;
  begin
    if (TDirectory.Exists(Folder)) then
      for Filename in TDirectory.GetFiles(Folder, '*.SKINRES') do
        LoadExternalSkinDetails(Filename);
  end;

  function IsColorScheme(const ASkinName: string; out AIndex: Integer): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 0 to Length(ColorSchemeNameMap) - 1 do
      if (SameText(ASkinName, ColorSchemeNameMap[i])) then
      begin
        Result := True;
        AIndex := i;
        Break;
      end;
  end;

  procedure LoadColorSchemeGlyph(ABitmap: TBitmap; AImageList: TcxImageList; AGlyphIndex: Integer);
  var
    ABitmap32: TcxBitmap32;
  begin
    case AGlyphIndex of
      3: //DarkGray
        AGlyphIndex := 1;
      4: //LightGray
        AGlyphIndex := 2;
      6: //DarkGray
        AGlyphIndex := 3;
      7: //Colorful
        AGlyphIndex := 0;
    end;
    ABitmap32 := TcxBitmap32.CreateSize(AImageList.Width, AImageList.Height, True);
    try
      AImageList.Draw(ABitmap32.Canvas, 0, 0, AGlyphIndex);
      ABitmap.Assign(ABitmap32);
    finally
      ABitmap32.Free;
    end;
  end;

  procedure LoadColorSchemeDetails(ColorSchemeIndex: integer; var SkinDetails: TSkinDetails);
  begin
    SkinDetails.Filename := '';
    SkinDetails.Index := -1;
    SkinDetails.Name := ColorSchemeNameMap[ColorSchemeIndex];
    SkinDetails.DisplayName := ColorSchemeDisplayNameMap[ColorSchemeIndex];
    SkinDetails.Group := ColorSchemesGroupName;
    SkinDetails.GlyphSmall := TBitmap.Create;
    LoadColorSchemeGlyph(SkinDetails.GlyphSmall, ImageListColorSchemesGlyphsSmall, ColorSchemeIndex);
    SkinDetails.GlyphLarge := TBitmap.Create;
    LoadColorSchemeGlyph(SkinDetails.GlyphLarge, ImageListColorSchemesGlyphsLarge, ColorSchemeIndex);
  end;

var
  i: integer;
  Skin: TdxCustomRibbonSkin;
  SkinDetails: TSkinDetails;
  dxSkinDetails: TdxSkinDetails;
  ColorSchemeIndex: integer;
  Item: TcxImageComboBoxItem;
begin
  FSkinList.Clear;
  try

    for i := 0 to dxRibbonSkinsManager.SkinCount-1 do
    begin
      Skin := dxRibbonSkinsManager.Skins[I];

      if (Skin.Style <> FRibbonStyle) then
        continue;

      if (Skin is TdxSkinRibbonPainter) then
      begin
        if (TdxSkinRibbonPainter(Skin).Painter.IsInternalPainter) then
          continue;

        if (not TdxSkinRibbonPainter(Skin).Painter.GetPainterDetails(dxSkinDetails)) then
          continue;

        LoadSkinDetails(dxSkinDetails, SkinDetails);
      end else
      if (IsColorScheme(Skin.Name, ColorSchemeIndex)) then
      begin
        LoadColorSchemeDetails(ColorSchemeIndex, SkinDetails);
      end else
        continue;

      FSkinList.Add(SkinDetails);
    end;

    LoadSkinFolder(SkinFolder(SkinFolderUser));
    if (not AnsiSameText(SkinFolder(SkinFolderUser), SkinFolder(SkinFolderSystem))) then
      LoadSkinFolder(SkinFolder(SkinFolderSystem));

    FSkinList.Sort(TComparer<TSkinDetails>.Construct(function(const Left, Right: TSkinDetails): Integer
      begin
        Result := CompareText(Left.Group, Right.Group);
        if (Result = 0) then
          Result := CompareText(Left.DisplayName, Right.DisplayName);
      end));

    ImageComboBoxSkin.Properties.Items.Clear;
    ImageListSkin.Clear;
    ImageListSkinLarge.Clear;
    for i := 0 to FSkinList.Count-1 do
    begin
      SkinDetails := FSkinList[i];
      Item := ImageComboBoxSkin.Properties.Items.Add;
      Item.Value := SkinDetails.Name;
      Item.Description := SkinDetails.DisplayName;
      // Add small glyph (displayed in combo edit)
      ImageListSkin.Add(SkinDetails.GlyphSmall, nil);
      // Add large glyph (displayed in combo drop down)
      Item.ImageIndex := ImageListSkinLarge.Add(SkinDetails.GlyphLarge, nil);
    end;

  finally
    for i := 0 to FSkinList.Count-1 do
    begin
      SkinDetails := FSkinList[i];
      FreeAndNil(SkinDetails.GlyphSmall);
      FreeAndNil(SkinDetails.GlyphLarge);
    end;
  end;
end;

procedure TFormSettings.RequireRestart;
begin
  if (not Visible) then
    exit;

  FRestartRequired := True;
  LayoutGroupRestart.Visible := True;
end;

// -----------------------------------------------------------------------------

procedure TFormSettings.TextEditTranslatorMSAPIKeyPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
var
  Translator: TDataModuleTranslatorMicrosoftV3;
  ErrorMessage: string;
resourcestring
  sTranslatorMSAPIKeyValid = 'The API key has been validated.';
  sTranslatorMSAPIKeyInvalid = 'The API key could not be validated:'#13#13'%s';
begin
  EditTranslatorMSAPIKey.Properties.Buttons[AButtonIndex].ImageIndex := 0;

  Translator := TDataModuleTranslatorMicrosoftV3.Create(nil);
  try

    if ((Translator as ITranslationServiceMS).ValidateAPIKey(EditTranslatorMSAPIKey.Text, ErrorMessage)) then
    begin
      EditTranslatorMSAPIKey.Properties.Buttons[AButtonIndex].ImageIndex := 1;
      MessageDlg(sTranslatorMSAPIKeyValid, mtInformation, [mbOK], 0);
    end else
      MessageDlg(Format(sTranslatorMSAPIKeyInvalid, [ErrorMessage]), mtWarning, [mbOK], 0);

  finally
    Translator.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormSettings.TextEditTranslatorMSAPIKeyPropertiesChange(Sender: TObject);
begin
  // API key no longer validated
  EditTranslatorMSAPIKey.Properties.Buttons[0].ImageIndex := 0;
end;

// -----------------------------------------------------------------------------

procedure TFormSettings.LoadFolders;
var
  i: integer;
begin
  GridFoldersTableView.DataController.BeginUpdate;
  try
    for i := Low(FolderOrder) to High(FolderOrder) do
      GridFoldersTableView.DataController.Values[i, 1] := TranslationManagerSettings.Folders.Folder[FolderOrder[i]];
  finally
    GridFoldersTableView.DataController.EndUpdate;
  end;
end;

procedure TFormSettings.ApplyFolders;
var
  i: integer;
begin
  for i := Low(FolderOrder) to High(FolderOrder) do
    TranslationManagerSettings.Folders.Folder[FolderOrder[i]] := VarToStr(GridFoldersTableView.DataController.Values[i, 1]);
end;

// -----------------------------------------------------------------------------

procedure TFormSettings.LoadProofing(SpellChecker: TdxSpellChecker);

  function GetDictionaryLanguage(ADictionary: TdxCustomSpellCheckerDictionary): Integer;
  begin
    Result := ADictionary.Language;
    if (Result = 0) then
      Result := dxLanguages.GetDefaultLanguageLCID;
  end;

  procedure PopulateLanguages;
  var
    I, J: Integer;
    List: TStringList;
    Item: TcxCheckComboBoxItem;
    ADictionary: TdxCustomSpellCheckerDictionary;
  begin
    List := TStringList.Create;
    try
      SpellChecker.PopulateLanguages(List);

      ComboBoxProofingLanguages.Value := 0;
      ComboBoxProofingLanguages.Properties.Items.BeginUpdate;
      try
        ComboBoxProofingLanguages.Properties.Items.Clear;

        Item := ComboBoxProofingLanguages.Properties.Items.Add;
        Item.Description := 'Auto (use project language)';
        Item.Tag := -1;

        for I := 0 to List.Count - 1 do
        begin
          Item := ComboBoxProofingLanguages.Properties.Items.Add;
          Item.Description := List[I];
          Item.Tag := Cardinal(List.Objects[I]);

          for J := 0 to SpellChecker.DictionaryCount - 1 do
          begin
            ADictionary := SpellChecker.Dictionaries[J];
            if (ADictionary is TdxUserSpellCheckerDictionary) or (not ADictionary.Enabled) then
              Continue;
            if (GetDictionaryLanguage(ADictionary) = Item.Tag) then
            begin
              ComboBoxProofingLanguages.States[I] := cbsChecked;
              Break;
            end;
          end;
        end;
      finally
        ComboBoxProofingLanguages.Properties.Items.EndUpdate;
      end;
    finally
      List.Free;
    end;
  end;

  procedure PopulateReplaces;
  var
    I: Integer;
    Item: TListItem;
    Replacement: TdxSpellCheckerReplacement;
  begin
    FSpellCheckerAutoCorrectOptions.Replacements.Clear;

    ListViewProofingAutoCorrectReplacements.Items.BeginUpdate;
    try
      ListViewProofingAutoCorrectReplacements.Items.Clear;
      for I := 0 to SpellChecker.AutoCorrectOptions.Replacements.Count - 1 do
      begin
        Replacement := SpellChecker.AutoCorrectOptions.Replacements[i];
        FSpellCheckerAutoCorrectOptions.Replacements.Add(Replacement.Text, Replacement.Replacement);

        Item := ListViewProofingAutoCorrectReplacements.Items.Add;
        Item.Caption := Replacement.Text;
        Item.SubItems.Add(Replacement.Replacement);
      end;
    finally
      ListViewProofingAutoCorrectReplacements.Items.EndUpdate;
    end;
  end;

  procedure PopulateExceptions;
  var
    Stream: TStream;
  begin
    Stream := TMemoryStream.Create;
    try
      SpellChecker.AutoCorrectOptions.FirstLetterExceptions.SaveToStream(Stream);
      Stream.Position := 0;
      FSpellCheckerAutoCorrectOptions.FirstLetterExceptions.LoadFromStream(Stream);
      FSpellCheckerAutoCorrectOptions.FirstLetterExceptions.AutoInclude := SpellChecker.AutoCorrectOptions.FirstLetterExceptions.AutoInclude;
      Stream.Size := 0;
      SpellChecker.AutoCorrectOptions.InitialCapsExceptions.SaveToStream(Stream);
      Stream.Position := 0;
      FSpellCheckerAutoCorrectOptions.InitialCapsExceptions.LoadFromStream(Stream);
      FSpellCheckerAutoCorrectOptions.InitialCapsExceptions.AutoInclude := SpellChecker.AutoCorrectOptions.InitialCapsExceptions.AutoInclude;
    finally
      Stream.Free;
    end;
  end;

begin
  CheckBoxProofingSpellCheck.Checked := SpellChecker.CheckAsYouTypeOptions.Active;
  // Note: Assign does not copy member objects
  FSpellCheckerAutoCorrectOptions.Assign(SpellChecker.AutoCorrectOptions);

  CheckBoxProofingIgnoreUppercase.Checked := SpellChecker.SpellingOptions.IgnoreUpperCaseWords;
  CheckBoxProofingIgnoreMixedCase.Checked := SpellChecker.SpellingOptions.IgnoreMixedCaseWords;
  CheckBoxProofingIgnoreRepeatWords.Checked := SpellChecker.SpellingOptions.IgnoreRepeatedWords;
  CheckBoxProofingIgnoreNumbers.Checked := SpellChecker.SpellingOptions.IgnoreWordsWithNumbers;

  CheckBoxProofingAutoCorrect.Checked := FSpellCheckerAutoCorrectOptions.Active;
  CheckBoxProofingCorrectSentenceCaps.Checked := FSpellCheckerAutoCorrectOptions.CorrectSentenceCaps;
  CheckBoxProofingCorrectInitialCaps.Checked := FSpellCheckerAutoCorrectOptions.CorrectInitialCaps;
  CheckBoxProofingCorrectCapsLock.Checked := FSpellCheckerAutoCorrectOptions.CorrectCapsLock;
  CheckBoxProofingDisableCapsLock.Checked := FSpellCheckerAutoCorrectOptions.DisableCapsLock;

  CheckBoxProofingCorrectAutoReplace.Checked := FSpellCheckerAutoCorrectOptions.ReplaceTextAsYouType;
  CheckBoxProofingCorrectAutomaticallyUseSuggestions.Checked := FSpellCheckerAutoCorrectOptions.AutomaticallyUseSuggestions;

//  PopulateLanguages;
  PopulateReplaces;
  PopulateExceptions;
  ButtonProofingEditCustomDictionary.Enabled := (SpellChecker.FindFirstEnabledUserDictionary <> nil);
end;

procedure TFormSettings.ApplyProofing(SpellChecker: TdxSpellChecker);

  function GetDictionaryLanguage(ADictionary: TdxCustomSpellCheckerDictionary): Integer;
  begin
    Result := ADictionary.Language;
    if (Result = 0) then
      Result := dxLanguages.GetDefaultLanguageLCID;
  end;

  procedure SaveLanguages;
  var
    i, j: Integer;
    Dictionary: TdxCustomSpellCheckerDictionary;
    LCID: integer;
  begin
    for i := 0 to ComboBoxProofingLanguages.Properties.Items.Count-1 do
    begin
      for j := 0 to SpellChecker.DictionaryCount-1 do
      begin
        Dictionary := SpellChecker.Dictionaries[j];
        if (Dictionary is TdxUserSpellCheckerDictionary) then
          continue;

        LCID := ComboBoxProofingLanguages.Properties.Items[i].Tag;
        if (LCID = -1) then
          continue;

        if (GetDictionaryLanguage(Dictionary) = LCID) then
        begin
          Dictionary.Enabled := (ComboBoxProofingLanguages.States[i] = cbsChecked);
          Break;
        end;
      end;
    end;
  end;

  procedure SaveReplacements;
  begin
    // Note: TdxSpellCheckerAutoCorrectReplacementList is a bastard (poorly designed) object list. Calling Clear will
    // destroy the objects it contains but calling Delete() will not. We misuse this to transfer objects from one list
    // to another.
    SpellChecker.AutoCorrectOptions.Replacements.Clear;
    // Transfer ownership of replacement items
    SpellChecker.AutoCorrectOptions.Replacements.Assign(FSpellCheckerAutoCorrectOptions.Replacements);
    // Remove items without deleting them
    while (FSpellCheckerAutoCorrectOptions.Replacements.Count > 0) do
      FSpellCheckerAutoCorrectOptions.Replacements.Delete(0);
  end;

  procedure SaveExceptions;
  var
    Stream: TStream;
  begin
    Stream := TMemoryStream.Create;
    try
      FSpellCheckerAutoCorrectOptions.FirstLetterExceptions.SaveToStream(Stream);
      Stream.Position := 0;
      SpellChecker.AutoCorrectOptions.FirstLetterExceptions.LoadFromStream(Stream);
      SpellChecker.AutoCorrectOptions.FirstLetterExceptions.AutoInclude := FSpellCheckerAutoCorrectOptions.FirstLetterExceptions.AutoInclude;
      Stream.Size := 0;
      FSpellCheckerAutoCorrectOptions.InitialCapsExceptions.SaveToStream(Stream);
      Stream.Position := 0;
      SpellChecker.AutoCorrectOptions.InitialCapsExceptions.LoadFromStream(Stream);
      SpellChecker.AutoCorrectOptions.InitialCapsExceptions.AutoInclude := FSpellCheckerAutoCorrectOptions.InitialCapsExceptions.AutoInclude;
    finally
      Stream.Free;
    end;
  end;

begin
  SpellChecker.CheckAsYouTypeOptions.Active := CheckBoxProofingSpellCheck.Checked;

  SpellChecker.SpellingOptions.IgnoreUpperCaseWords := CheckBoxProofingIgnoreUppercase.Checked;
  SpellChecker.SpellingOptions.IgnoreMixedCaseWords := CheckBoxProofingIgnoreMixedCase.Checked;
  SpellChecker.SpellingOptions.IgnoreRepeatedWords := CheckBoxProofingIgnoreRepeatWords.Checked;
  SpellChecker.SpellingOptions.IgnoreWordsWithNumbers := CheckBoxProofingIgnoreNumbers.Checked;

  FSpellCheckerAutoCorrectOptions.Active := CheckBoxProofingAutoCorrect.Checked;
  FSpellCheckerAutoCorrectOptions.CorrectSentenceCaps := CheckBoxProofingCorrectSentenceCaps.Checked;
  FSpellCheckerAutoCorrectOptions.CorrectInitialCaps := CheckBoxProofingCorrectInitialCaps.Checked;
  FSpellCheckerAutoCorrectOptions.CorrectCapsLock := CheckBoxProofingCorrectCapsLock.Checked;
  FSpellCheckerAutoCorrectOptions.DisableCapsLock := CheckBoxProofingDisableCapsLock.Checked;

  FSpellCheckerAutoCorrectOptions.ReplaceTextAsYouType := CheckBoxProofingCorrectAutoReplace.Checked;
  FSpellCheckerAutoCorrectOptions.AutomaticallyUseSuggestions := CheckBoxProofingCorrectAutomaticallyUseSuggestions.Checked;

//  SaveLanguages;

  // Note: Assign does not copy member objects
  SpellChecker.AutoCorrectOptions.Assign(FSpellCheckerAutoCorrectOptions);

  SaveReplacements;
  SaveExceptions;

  SpellChecker.LoadDictionaries(True);
end;

// -----------------------------------------------------------------------------

end.
