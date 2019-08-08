unit amLocalization.Dialog.Main;


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, System.Actions,
  Vcl.ActnList, System.ImageList, Vcl.ImgList, Datasnap.DBClient,
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
    procedure ActionProofingCheckSelectedUpdate(Sender: TObject);
    procedure BarManagerBarProofingCaptionButtons0Click(Sender: TObject);
    procedure ActionFindSearchExecute(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure ActionProjectSaveUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ActionProjectUpdateUpdate(Sender: TObject);
    procedure ActionBuildUpdate(Sender: TObject);
    procedure ActionProjectPurgeUpdate(Sender: TObject);
    procedure TreeListColumnModuleStatusPropertiesEditValueChanged(Sender: TObject);
    procedure TreeListItemsGetNodeImageIndex(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AIndexType: TcxTreeListImageIndexType; var AIndex: TImageIndex);
    procedure TreeListModulesFocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
    procedure TreeListModulesEnter(Sender: TObject);
    procedure TreeListModulesExit(Sender: TObject);
    procedure BarManagerBarLanguageCaptionButtons0Click(Sender: TObject);
    procedure BarEditItemTargetLanguagePropertiesInitPopup(Sender: TObject);
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
    procedure InitializeProject(const SourceFilename: string; SourceLocaleID: Word);
    procedure LockUpdates;
    procedure UnlockUpdates;
    function PerformSpellCheck(Prop: TLocalizerProperty): boolean;
    procedure OnProjectChanged(Sender: TObject);
    function CheckSave: boolean;
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
  UITypes,
  Generics.Collections,
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
end;

procedure TFormMain.UnlockUpdates;
begin
  ASSERT(FUpdateLockCount > 0);
  TreeListModules.EndUpdate;
  Dec(FUpdateLockCount);
end;

procedure TFormMain.ActionBuildExecute(Sender: TObject);
var
  ProjectProcessor: TProjectResourceProcessor;
  ResourceWriter: IResourceWriter;
  Filename: string;
  LocaleItem: TLocaleItem;
begin
  SaveCursor(crHourGlass);

  ProjectProcessor := TProjectResourceProcessor.Create;
  try
    FLocalizerProject.BeginLoad;
    try

      Filename := FLocalizerProject.SourceFilename;
      LocaleItem := TLocaleItems.FindLCID(TargetLanguageID);

      if (LocaleItem <> nil) then
        Filename := TPath.ChangeExtension(Filename, '.'+LocaleItem.LanguageShortName)
      else
        Filename := TPath.ChangeExtension(Filename, '.dll');

      ResourceWriter := TResourceModuleWriter.Create(Filename);
      try

        ProjectProcessor.Execute(FLocalizerProject, FLocalizerProject.SourceFilename, TargetLanguage, ResourceWriter);

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

procedure TFormMain.ActionBuildUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (not FLocalizerProject.SourceFilename.IsEmpty);
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

procedure TFormMain.ActionProjectNewExecute(Sender: TObject);
var
  FormNewProject: TFormNewProject;
begin
  if (not CheckSave) then
    exit;

  FormNewProject := TFormNewProject.Create(nil);
  try
    FormNewProject.SetLanguageView(DataModuleMain.GridTableViewLanguages, DataModuleMain.GridTableViewLanguagesColumnLanguage);

    FormNewProject.SourceApplication := Application.ExeName;
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

  TLocalizationProjectFiler.LoadFromFile(FLocalizerProject, OpenDialogProject.FileName);
  FLocalizerProject.Modified := False;

  RibbonMain.DocumentName := FLocalizerProject.Name;

  LoadProject(FLocalizerProject);
end;

procedure TFormMain.ActionProjectPurgeExecute(Sender: TObject);
var
  Module: TLocalizerModule;
  Item: TLocalizerItem;
  Prop: TLocalizerProperty;
  NeedReload: boolean;
  CountModule, CountItem, CountProp: integer;
begin
  SaveCursor(crHourGlass);

  NeedReload := False;

  CountModule := 0;
  CountItem := 0;
  CountProp := 0;

  for Module in FLocalizerProject.Modules.Values.ToArray do // ToArray for stability since we delete from dictionary
  begin
    if (Module.Kind = mkOther) or (Module.State = lItemStateUnused) then
    begin
      NeedReload := True;
      Module.Free;
      Inc(CountModule);
      continue;
    end;

    for Item in Module.Items.Values.ToArray do // ToArray for stability since we delete from dictionary
    begin
      if (Item.State = lItemStateUnused) then
      begin
        NeedReload := True;
        Inc(CountItem);
        Item.Free;
        continue;
      end;

      // TODO : Purge obsolete translations?
      for Prop in Item.Properties.Values.ToArray do // ToArray for stability since we delete from dictionary
        if (Prop.State = lItemStateUnused) then
        begin
          NeedReload := True;
          Inc(CountProp);
          Prop.Free;
        end;

      if (Item.Properties.Count = 0) then
      begin
        NeedReload := True;
        Inc(CountItem);
        Item.Free;
      end;
    end;

    if (Module.Items.Count = 0) then
    begin
      NeedReload := True;
      Inc(CountModule);
      Module.Free;
    end;
  end;

  if (NeedReload) then
  begin
    FLocalizerProject.Modified := True;

    LoadProject(FLocalizerProject, False);
  end;

  StatusBar.SimplePanelStyle.Text := Format('Purged %d modules, %d items, %d properties', [CountModule, CountItem, CountProp]);
end;

procedure TFormMain.ActionProjectPurgeUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FLocalizerProject.Modules.Count > 0);
end;

procedure TFormMain.ActionProjectSaveExecute(Sender: TObject);
begin
  SaveCursor(crHourGlass);

  TLocalizationProjectFiler.SaveToFile(FLocalizerProject, TPath.ChangeExtension(FLocalizerProject.SourceFilename, '.xml'));

  FLocalizerProject.Modified := False;

  StatusBar.SimplePanelStyle.Text := 'Saved';
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

  StatusBar.SimplePanelStyle.Text := 'Updated';
end;

procedure TFormMain.ActionProjectUpdateUpdate(Sender: TObject);
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
    end);

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
    Item.Status := lItemStatusDontTranslate;
    LoadItem(Item, True);
  end;
end;

procedure TFormMain.ActionStatusDontTranslateUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FocusedNode <> nil) and (FocusedItem <> nil) and (FocusedItem.State <> lItemStateUnused);

  TAction(Sender).Checked := (TAction(Sender).Enabled) and (FocusedItem.Status = lItemStatusDontTranslate);
end;

procedure TFormMain.ActionStatusHoldExecute(Sender: TObject);
var
  i: integer;
  Item: TCustomLocalizerItem;
begin
  for i := 0 to FocusedNode.TreeList.SelectionCount-1 do
  begin
    Item := NodeToItem(FocusedNode.TreeList.Selections[i]);
    Item.Status := lItemStatusHold;
    LoadItem(Item, True);
  end;
end;

procedure TFormMain.ActionStatusHoldUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FocusedNode <> nil) and (FocusedItem <> nil) and (FocusedItem.State <> lItemStateUnused);

  TAction(Sender).Checked := (TAction(Sender).Enabled) and (FocusedItem.Status = lItemStatusHold);
end;

procedure TFormMain.ActionStatusTranslateExecute(Sender: TObject);
var
  i: integer;
  Item: TCustomLocalizerItem;
begin
  for i := 0 to FocusedNode.TreeList.SelectionCount-1 do
  begin
    Item := NodeToItem(FocusedNode.TreeList.Selections[i]);
    Item.Status := lItemStatusTranslate;
    LoadItem(Item, True);
  end;
end;

procedure TFormMain.ActionStatusTranslateUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FocusedNode <> nil) and (FocusedItem <> nil) and (FocusedItem.State <> lItemStateUnused);

  TAction(Sender).Checked := (TAction(Sender).Enabled) and (FocusedItem.Status = lItemStatusTranslate);
end;

// -----------------------------------------------------------------------------

procedure TFormMain.ActionTranslationStateUpdate(Sender: TObject);
var
  Translation: TLocalizerTranslation;
  Prop: TLocalizerProperty;
begin
  Prop := FocusedProperty;

  TAction(Sender).Enabled := (Prop <> nil) and (Prop.State <> lItemStateUnused) and (Prop.Status <> lItemStatusDontTranslate) and
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
            end);

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

  FLocalizerDataSource := TLocalizerDataSource.Create(nil);
  TreeListItems.DataController.CustomDataSource := FLocalizerDataSource;

  DataModuleMain := TDataModuleMain.Create(Self);

  RibbonTabMain.Active := True;

  InitializeProject('', GetUserDefaultUILanguage);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FLocalizerDataSource.Free;
  FLocalizerProject.Free;
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

  FLocalizerProject.Clear;

  FLocalizerProject.SourceFilename := SourceFilename;
  FLocalizerProject.Name := TPath.GetFileNameWithoutExtension(SourceFilename);
  FLocalizerProject.BaseLocaleID := SourceLocaleID;

  FLocalizerProject.Modified := False;

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
begin
  SaveCursor(crHourGlass);

  LockUpdates;
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

    for Module in Modules do
    begin
      if (Module.Kind = mkOther) then
        continue;

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

    if (Clear) then
      // Reassign TreeListItems.CustomDataSource if it was cleared by TreeListItems.Clear
      TreeListItems.CustomDataSource := FLocalizerDataSource;
  finally
    UnlockUpdates;
  end;
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

    if (Module.State = lItemStateUnused) then
      Node.ImageIndex := 1
    else
    if (Module.State = lItemStateNew) and (Module.Status = lItemStatusTranslate) then
      Node.ImageIndex := 0
    else
    if (Module.Status = lItemStatusDontTranslate) then
      Node.ImageIndex := 2
    else
    if (Module.Status = lItemStatusHold) then
      Node.ImageIndex := 5
    else
      Node.ImageIndex := 6;

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
  //
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
  if (Prop.State = lItemStateUnused) then
    Exit(True);

  if (Prop.Status <> lItemStatusTranslate) then
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
    TextEditor.Text := FocusedProperty.TranslatedValue[TargetLanguage];

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

  if (Sender is TcxVirtualTreeList) then
    Prop := TLocalizerProperty(TcxVirtualTreeList(Sender).HandleFromNode(ANode))
  else
    Prop := TLocalizerProperty(ANode.Data);

  if (not Prop.Translations.TryGetTranslation(TargetLanguage, Translation)) then
    Translation := nil;

  if (Prop.State = lItemStateUnused) then
    AIndex := 1
  else
  if (Prop.State = lItemStateNew) and (Prop.Status = lItemStatusTranslate) and (Translation = nil) then
    AIndex := 0
  else
  if (Prop.Status = lItemStatusDontTranslate) then
    AIndex := 2
  else
  if (Prop.Status = lItemStatusHold) then
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
begin
  FLocalizerDataSource.Module := FocusedModule;
end;

procedure TFormMain.ActionProofingCheckSelectedExecute(Sender: TObject);
var
  i: integer;
  Item: TCustomLocalizerItem;
begin
  for i := 0 to FocusedNode.TreeList.SelectionCount-1 do
  begin
    Item := NodeToItem(FocusedNode.TreeList.Selections[i]);

    Item.Traverse(
      function(Prop: TLocalizerProperty): boolean
      begin
        Result := PerformSpellCheck(Prop);
      end);
  end;

  SpellChecker.ShowSpellingCompleteMessage;
end;

procedure TFormMain.ActionProofingCheckSelectedUpdate(Sender: TObject);
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
  Prop: TLocalizerProperty;
begin
  Result := nil;

  if (FModule = nil) then
    Exit;

  for Item in FModule.Items.Values.ToArray do
    for Prop in Item.Properties.Values.ToArray do
    begin
      if (ARecordIndex = 0) then
        Exit(Prop);
      Dec(ARecordIndex);
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
