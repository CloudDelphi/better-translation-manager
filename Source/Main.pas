unit Main;


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
  cxBarEditItem, cxDataControllerConditionalFormattingRulesManagerDialog, cxButtonEdit, dxSpellCheckerCore, dxSpellChecker;


const
  MSG_SOURCE_CHANGED = WM_USER;
  MSG_TARGET_CHANGED = WM_USER+1;

type
  TFormMain = class(TdxRibbonForm)
    OpenDialogXLIFF: TOpenDialog;
    BarManager: TdxBarManager;
    RibbonMain: TdxRibbon;
    RibbonTabMain: TdxRibbonTab;
    dxRibbonStatusBar1: TdxRibbonStatusBar;
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
    TreeList: TcxTreeList;
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
    ClientDataSetLanguages: TClientDataSet;
    ClientDataSetLanguagesLocaleID: TWordField;
    ClientDataSetLanguagesLanguageName: TStringField;
    ClientDataSetLanguagesCountryName: TStringField;
    GridViewRepository: TcxGridViewRepository;
    GridTableViewLanguages: TcxGridDBTableView;
    GridTableViewLanguagesColumnLocaleID: TcxGridDBColumn;
    GridTableViewLanguagesColumnLanguage: TcxGridDBColumn;
    GridTableViewLanguagesColumnCountry: TcxGridDBColumn;
    DataSourceLanguages: TDataSource;
    ClientDataSetLanguagesLocaleName: TStringField;
    GridTableViewLanguagesColumnLocaleName: TcxGridDBColumn;
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
    ActionTranslationPropose: TAction;
    ActionTranslationAccept: TAction;
    ActionTranslationReject: TAction;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeListColumnStatusPropertiesEditValueChanged(Sender: TObject);
    procedure TreeListEditValueChanged(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
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
    procedure ActionTranslationProposeUpdate(Sender: TObject);
    procedure ActionTranslationProposeExecute(Sender: TObject);
    procedure ActionTranslationAcceptExecute(Sender: TObject);
    procedure ActionTranslationRejectExecute(Sender: TObject);
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
    procedure SpellCheckerCheckWord(Sender: TdxCustomSpellChecker; const AWord: WideString; out AValid: Boolean;
      var AHandled: Boolean);
    procedure SpellCheckerSpellingComplete(Sender: TdxCustomSpellChecker; var AHandled: Boolean);
    procedure TreeListEditing(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; var Allow: Boolean);
    procedure ActionProofingCheckSelectedExecute(Sender: TObject);
    procedure ActionProofingCheckSelectedUpdate(Sender: TObject);
    procedure BarManagerBarProofingCaptionButtons0Click(Sender: TObject);
    procedure ActionFindSearchExecute(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
  private
    FLocalizerProject: TLocalizerProject;
    FUpdateLockCount: integer;
    FSpellCheckProp: TLocalizerProperty;
    FSpellCheckingWord: boolean;
    FSpellCheckingString: boolean;
    FSpellCheckingStringResult: boolean;
    FFindFirst: boolean;
  protected
    function GetTargetLanguageID: Word;
  protected
    procedure MsgSourceChanged(var Msg: TMessage); message MSG_SOURCE_CHANGED;
    procedure MsgTargetChanged(var Msg: TMessage); message MSG_TARGET_CHANGED;
    procedure InitializeProject(const SourceFilename: string; SourceLocaleID: Word);
    procedure LoadProject(Project: TLocalizerProject);
    procedure LoadProjectPropertyNode(PropNode: TcxTreeListNode);
    procedure LockUpdates;
    procedure UnlockUpdates;
    function PerformSpellCheck(Prop: TLocalizerProperty): boolean;
  public
    property TargetLanguageID: Word read GetTargetLanguageID;
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
  Generics.Collections,
  Generics.Defaults,
  System.Character,
  Menus,
  msxmldom,

  // DevExpress skins
  dxSkinOffice2016Colorful,

  dxHunspellDictionary,
  dxSpellCheckerDialogs,

  amLocale,
  amLocalization.Engine,
  amLocalization.ResourceWriter,
  amLocalization.Persistence,
  amLocalization.Import.XLIFF,
  amLocalization.Dialog.TextEdit,
  amLocalization.Dialog.NewProject;


type
  TdxSpellCheckerCracker = class(TdxCustomSpellChecker);

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

procedure TFormMain.LockUpdates;
begin
  Inc(FUpdateLockCount);
  TreeList.BeginUpdate;
end;

procedure TFormMain.UnlockUpdates;
begin
  ASSERT(FUpdateLockCount > 0);
  TreeList.EndUpdate;
  Dec(FUpdateLockCount);
end;

procedure TFormMain.ActionBuildExecute(Sender: TObject);
var
  ProjectProcessor: TProjectResourceProcessor;
  ResourceWriter: IResourceWriter;
  Filename: string;
  LocaleItem: TLocaleItem;
begin
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

        ProjectProcessor.Execute(FLocalizerProject, FLocalizerProject.SourceFilename, TargetLanguageID, ResourceWriter);

      finally
        ResourceWriter := nil;
      end;

    finally
      FLocalizerProject.EndLoad;
    end;
  finally
    ProjectProcessor.Free;
  end;

  LoadProject(FLocalizerProject);
end;

procedure TFormMain.FindDialogFind(Sender: TObject);
var
  Node: TcxTreeListNode;
  Value: string;
  StringSearchOptions: TStringSearchOptions;
begin
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

  OpenDialogXLIFF.InitialDir := TPath.GetDirectoryName(OpenDialogXLIFF.FileName);

  Importer := TModuleImporterXLIFF.Create;
  try
    Importer.LoadFromFile(FLocalizerProject, OpenDialogXLIFF.FileName);
  finally
    Importer.Free;
  end;

  LoadProject(FLocalizerProject);
end;

procedure TFormMain.ActionProjectNewExecute(Sender: TObject);
var
  FormNewProject: TFormNewProject;
begin
  FormNewProject := TFormNewProject.Create(nil);
  try
    FormNewProject.SetLanguageView(GridTableViewLanguages, GridTableViewLanguagesColumnLanguage);

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
  if (not OpenDialogProject.Execute(Handle)) then
    exit;

  OpenDialogProject.InitialDir := TPath.GetDirectoryName(OpenDialogProject.FileName);

  TLocalizationProjectFiler.LoadFromFile(FLocalizerProject, OpenDialogProject.FileName);

  RibbonMain.DocumentName := FLocalizerProject.Name;

  LoadProject(FLocalizerProject);
end;

procedure TFormMain.ActionProjectPurgeExecute(Sender: TObject);
var
  Module: TLocalizerModule;
  Item: TLocalizerItem;
  Prop: TLocalizerProperty;
begin
  for Module in FLocalizerProject.Modules.Values.ToArray do
  begin
    if (Module.Kind = mkOther) or (Module.State = lItemStateUnused) then
    begin
      Module.Free;
      continue;
    end;

    for Item in Module.Items.Values.ToArray do
    begin
      if (Item.State = lItemStateUnused) then
      begin
        Item.Free;
        continue;
      end;

      for Prop in Item.Properties.Values.ToArray do
        if (Prop.State = lItemStateUnused) then
          Prop.Free;

      if (Item.Properties.Count = 0) then
        Item.Free;
    end;

    if (Module.Items.Count = 0) then
      Module.Free;
  end;

  LoadProject(FLocalizerProject);
end;

procedure TFormMain.ActionProjectSaveExecute(Sender: TObject);
begin
  TLocalizationProjectFiler.SaveToFile(FLocalizerProject, TPath.ChangeExtension(FLocalizerProject.SourceFilename, '.xml'));
end;

procedure TFormMain.ActionProjectUpdateExecute(Sender: TObject);
var
  ProjectProcessor: TProjectResourceProcessor;
begin
  ProjectProcessor := TProjectResourceProcessor.Create;
  try
    ProjectProcessor.ScanProject(FLocalizerProject, FLocalizerProject.SourceFilename);
  finally
    ProjectProcessor.Free;
  end;

  LoadProject(FLocalizerProject);
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
  FLocalizerProject.Traverse(
    function(Prop: TLocalizerProperty): boolean
    begin
      Result := PerformSpellCheck(Prop);
    end);

  SpellChecker.ShowSpellingCompleteMessage;
end;

procedure TFormMain.ActionStatusDontTranslateExecute(Sender: TObject);
begin
  TLocalizerProperty(TreeList.FocusedNode.Data).Status := lItemStatusDontTranslate;
  LoadProjectPropertyNode(TreeList.FocusedNode);
end;

procedure TFormMain.ActionStatusDontTranslateUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (TreeList.FocusedNode <> nil) and (TreeList.FocusedNode.Data <> nil) and
    (TObject(TreeList.FocusedNode.Data) is TLocalizerProperty) and
    (TLocalizerProperty(TreeList.FocusedNode.Data).State <> lItemStateUnused);
  TAction(Sender).Checked := (TAction(Sender).Enabled) and (TLocalizerProperty(TreeList.FocusedNode.Data).Status = lItemStatusDontTranslate);
end;

procedure TFormMain.ActionStatusHoldExecute(Sender: TObject);
begin
  TLocalizerProperty(TreeList.FocusedNode.Data).Status := lItemStatusHold;
  LoadProjectPropertyNode(TreeList.FocusedNode);
end;

procedure TFormMain.ActionStatusHoldUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (TreeList.FocusedNode <> nil) and (TreeList.FocusedNode.Data <> nil) and
    (TObject(TreeList.FocusedNode.Data) is TLocalizerProperty) and
    (TLocalizerProperty(TreeList.FocusedNode.Data).State <> lItemStateUnused);
  TAction(Sender).Checked := (TAction(Sender).Enabled) and (TLocalizerProperty(TreeList.FocusedNode.Data).Status = lItemStatusHold);
end;

procedure TFormMain.ActionStatusTranslateExecute(Sender: TObject);
begin
  TLocalizerProperty(TreeList.FocusedNode.Data).Status := lItemStatusTranslate;
  LoadProjectPropertyNode(TreeList.FocusedNode);
end;

procedure TFormMain.ActionStatusTranslateUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (TreeList.FocusedNode <> nil) and (TreeList.FocusedNode.Data <> nil) and
    (TObject(TreeList.FocusedNode.Data) is TLocalizerProperty) and
    (TLocalizerProperty(TreeList.FocusedNode.Data).State <> lItemStateUnused);
  TAction(Sender).Checked := (TAction(Sender).Enabled) and (TLocalizerProperty(TreeList.FocusedNode.Data).Status = lItemStatusTranslate);
end;

procedure TFormMain.ActionTranslationAcceptExecute(Sender: TObject);
var
  Translation: TLocalizerTranslation;
begin
  if (not TLocalizerProperty(TreeList.FocusedNode.Data).Translations.TryGetTranslation(TargetLanguageID, Translation)) then
    Translation := TLocalizerProperty(TreeList.FocusedNode.Data).Translations.AddOrUpdateTranslation(TargetLanguageID, TLocalizerProperty(TreeList.FocusedNode.Data).Value);

  Translation.Status := tStatusTranslated;

  LoadProjectPropertyNode(TreeList.FocusedNode);
end;

procedure TFormMain.ActionTranslationProposeExecute(Sender: TObject);
var
  Translation: TLocalizerTranslation;
begin
  if (not TLocalizerProperty(TreeList.FocusedNode.Data).Translations.TryGetTranslation(TargetLanguageID, Translation)) then
    Translation := TLocalizerProperty(TreeList.FocusedNode.Data).Translations.AddOrUpdateTranslation(TargetLanguageID, TLocalizerProperty(TreeList.FocusedNode.Data).Value);

  Translation.Status := tStatusProposed;

  LoadProjectPropertyNode(TreeList.FocusedNode);
end;

procedure TFormMain.ActionTranslationProposeUpdate(Sender: TObject);
var
  Translation: TLocalizerTranslation;
begin
  TAction(Sender).Enabled := (TreeList.FocusedNode <> nil) and (TreeList.FocusedNode.Data <> nil) and
    (TObject(TreeList.FocusedNode.Data) is TLocalizerProperty) and
    (TLocalizerProperty(TreeList.FocusedNode.Data).State <> lItemStateUnused) and
    (TLocalizerProperty(TreeList.FocusedNode.Data).Status <> lItemStatusDontTranslate) and
    ((not TLocalizerProperty(TreeList.FocusedNode.Data).Translations.TryGetTranslation(TargetLanguageID, Translation)) or
     (Translation.Status <> tStatusObsolete));
end;

procedure TFormMain.ActionTranslationRejectExecute(Sender: TObject);
begin
  TLocalizerProperty(TreeList.FocusedNode.Data).Translations.Remove(TargetLanguageID);

  LoadProjectPropertyNode(TreeList.FocusedNode);
end;

procedure TFormMain.BarEditItemSourceLanguagePropertiesEditValueChanged(Sender: TObject);
begin
  PostMessage(Handle, MSG_SOURCE_CHANGED, 0, 0);
end;

procedure TFormMain.BarEditItemTargetLanguagePropertiesEditValueChanged(Sender: TObject);
begin
  PostMessage(Handle, MSG_TARGET_CHANGED, 0, 0);
end;

procedure TFormMain.BarManagerBarProofingCaptionButtons0Click(Sender: TObject);
begin
  dxShowSpellingOptionsDialog(SpellChecker);
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  i: integer;
begin
  DisableAero := True;

  msxmldom.MSXMLDOMDocumentFactory.AddDOMProperty('ProhibitDTD', False);

  OpenDialogXLIFF.InitialDir := TPath.GetDirectoryName(Application.ExeName);
  OpenDialogProject.InitialDir := TPath.GetDirectoryName(Application.ExeName);

  FLocalizerProject := TLocalizerProject.Create(TPath.GetFileNameWithoutExtension(Application.ExeName), GetUserDefaultUILanguage);
  FLocalizerProject.SourceFilename := Application.ExeName;

  ClientDataSetLanguages.CreateDataSet;
  for i := 0 to TLocaleItems.Count-1 do
  begin
    ClientDataSetLanguages.Append;
    try
      ClientDataSetLanguagesLocaleID.Value := TLocaleItems.Items[i].Locale;
      ClientDataSetLanguagesLocaleName.AsString := TLocaleItems.Items[i].LocaleName;
      ClientDataSetLanguagesLanguageName.AsString := TLocaleItems.Items[i].LanguageName;
      ClientDataSetLanguagesCountryName.AsString := TLocaleItems.Items[i].CountryName;

      ClientDataSetLanguages.Post;
    except
      ClientDataSetLanguages.Cancel;
      raise;
    end;
  end;

  InitializeProject(Application.ExeName, GetUserDefaultUILanguage);

  PostMessage(Handle, MSG_TARGET_CHANGED, 0, 0);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FLocalizerProject.Free;
end;

function TFormMain.GetTargetLanguageID: Word;
begin
  Result := BarEditItemTargetLanguage.EditValue;
end;

procedure TFormMain.InitializeProject(const SourceFilename: string; SourceLocaleID: Word);
begin
  FLocalizerProject.Clear;

  FLocalizerProject.SourceFilename := SourceFilename;
  FLocalizerProject.Name := TPath.GetFileNameWithoutExtension(SourceFilename);
  FLocalizerProject.BaseLocaleID := SourceLocaleID;

  RibbonMain.DocumentName := FLocalizerProject.Name;

  BarEditItemSourceLanguage.EditValue := FLocalizerProject.BaseLocaleID;
  BarEditItemTargetLanguage.EditValue := FLocalizerProject.BaseLocaleID;

  TreeListColumnSource.Caption.Text := TLocaleItems.FindLCID(FLocalizerProject.BaseLocaleID).LanguageName;
  TreeListColumnTarget.Caption.Text := TLocaleItems.FindLCID(FLocalizerProject.BaseLocaleID).LanguageName;
end;

procedure TFormMain.LoadProject(Project: TLocalizerProject);
var
  Module: TLocalizerModule;
  Modules: TArray<TLocalizerModule>;
  ModuleNode: TcxTreeListNode;
  Item: TLocalizerItem;
  Items: TArray<TLocalizerItem>;
  Prop: TLocalizerProperty;
  Props: TArray<TLocalizerProperty>;
  PropNode: TcxTreeListNode;
begin
  TreeList.Clear;

  LockUpdates;
  try

    Modules := Project.Modules.Values.ToArray;

    TArray.Sort<TLocalizerModule>(Modules, TComparer<TLocalizerModule>.Construct(
      function(const Left, Right: TLocalizerModule): Integer
      begin
        Result := (Ord(Left.Kind) - Ord(Right.Kind));
        if (Result = 0) then
          Result := CompareText(Left.Name, Right.Name);
      end));

    for Module in Modules do
    begin
      if (Module.Kind = mkOther) then
        continue;

      ModuleNode := TreeList.Add(nil, Module);
      ModuleNode.Texts[TreeListColumnItemName.ItemIndex] := Module.Name;
      ModuleNode.ImageIndex := -1;

      Items := Module.Items.Values.ToArray;

      TArray.Sort<TLocalizerItem>(Items, TComparer<TLocalizerItem>.Construct(
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

      for Item in Items do
      begin
        Props := Item.Properties.Values.ToArray;

        TArray.Sort<TLocalizerProperty>(Props, TComparer<TLocalizerProperty>.Construct(
          function(const Left, Right: TLocalizerProperty): Integer
          begin
            Result := CompareText(Left.Name, Right.Name);
          end));

        for Prop in Props do
        begin
          PropNode := TreeList.AddChild(ModuleNode, Prop);
          LoadProjectPropertyNode(PropNode);
        end;
      end;
    end;

  finally
    UnlockUpdates;
  end;

//  RibbonMain.Contexts[0].Visible := (FLocalizerProject.Modules.Count > 0);
end;

procedure TFormMain.LoadProjectPropertyNode(PropNode: TcxTreeListNode);
var
  Prop: TLocalizerProperty;
  Translation: TLocalizerTranslation;
begin
  Assert(PropNode <> nil);
  Assert(PropNode.Data <> nil);
  Assert(TObject(PropNode.Data) is TLocalizerProperty);

  LockUpdates;
  try
    Prop := TLocalizerProperty(PropNode.Data);

    PropNode.Texts[TreeListColumnItemName.ItemIndex] := Prop.Item.Name;
    PropNode.Texts[TreeListColumnType.ItemIndex] := Prop.Item.TypeName;

    PropNode.Texts[TreeListColumnValueName.ItemIndex] := Prop.Name;
    PropNode.Values[TreeListColumnID.ItemIndex] := Prop.Item.ResourceID;
    PropNode.Values[TreeListColumnStatus.ItemIndex] := Ord(Prop.Status);
    PropNode.Texts[TreeListColumnSource.ItemIndex] := Prop.Value;
    if (Prop.Translations.TryGetTranslation(TargetLanguageID, Translation)) then
    begin
      PropNode.Texts[TreeListColumnTarget.ItemIndex] := Translation.Value;
      PropNode.Values[TreeListColumnState.ItemIndex] := Ord(Translation.Status);
    end else
    begin
      Translation := nil;
      PropNode.Texts[TreeListColumnTarget.ItemIndex] := Prop.Value;
      PropNode.Values[TreeListColumnState.ItemIndex] := Ord(tStatusPending);
    end;

    if (Prop.State = lItemStateUnused) then
      PropNode.ImageIndex := 1
    else
    if (Prop.State = lItemStateNew) and (Prop.Status = lItemStatusTranslate) and (Translation = nil) then
      PropNode.ImageIndex := 0
    else
    if (Prop.Status = lItemStatusDontTranslate) then
      PropNode.ImageIndex := 2
    else
    if (Prop.Status = lItemStatusHold) then
      PropNode.ImageIndex := 5
    else
    if (Translation <> nil) and (Translation.Status <> tStatusPending) then
    begin
      if (Translation.Status = tStatusProposed) then
        PropNode.ImageIndex := 3
      else
      if (Translation.Status = tStatusTranslated) then
        PropNode.ImageIndex := 4
      else
        PropNode.ImageIndex := 7; // Obsolete
    end else
      PropNode.ImageIndex := 6;
  finally
    UnlockUpdates;
  end;
end;

procedure TFormMain.MsgSourceChanged(var Msg: TMessage);
begin
  TreeListColumnSource.Caption.Text := TLocaleItems.FindLCID(BarEditItemSourceLanguage.EditValue).LanguageName;
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
  LocaleItem := TLocaleItems.FindLCID(BarEditItemTargetLanguage.EditValue);

  TreeListColumnTarget.Caption.Text := LocaleItem.LanguageName;

  Assert(SpellChecker.Dictionaries[0] is TdxUserSpellCheckerDictionary);
  SpellChecker.Dictionaries[0].Enabled := False;
  SpellChecker.Dictionaries[0].Unload;
  TdxUserSpellCheckerDictionary(SpellChecker.Dictionaries[0]).DictionaryPath := Format('.\dictionaries\user-%s.dic', [LocaleItem.LanguageShortName]);
  SpellChecker.Dictionaries[0].Enabled := True;
  SpellChecker.Dictionaries[0].Load;
  SpellChecker.Dictionaries[0].Language := LocaleItem.Locale;

  AnyFound := False;
  for i := 1 to SpellChecker.DictionaryCount-1 do
  begin
    Found := (SpellChecker.Dictionaries[i].Language = LocaleItem.Locale);

//    if (SpellChecker.Dictionaries[i].Enabled) and (not Found) then
//      SpellChecker.Dictionaries[i].Unload;

    SpellChecker.Dictionaries[i].Enabled := Found;
    AnyFound := AnyFound or Found;
  end;

  if (not AnyFound) then
  begin
    FilenameDic := Format('.\dictionaries\%s.dic', [LocaleItem.LanguageShortName]);
    FilenameAff := Format('.\dictionaries\%s.aff', [LocaleItem.LanguageShortName]);
    if (TFile.Exists(FilenameDic)) and (TFile.Exists(FilenameAff)) then
    begin
//      AnyFound := True;
      SpellCheckerDictionaryItem := SpellChecker.DictionaryItems.Add;
      SpellCheckerDictionaryItem.DictionaryTypeClass := TdxHunspellDictionary;
      TdxHunspellDictionary(SpellCheckerDictionaryItem.DictionaryType).Language := LocaleItem.Locale;
      TdxHunspellDictionary(SpellCheckerDictionaryItem.DictionaryType).DictionaryPath := FilenameDic;
      TdxHunspellDictionary(SpellCheckerDictionaryItem.DictionaryType).GrammarPath := FilenameAff;
      TdxHunspellDictionary(SpellCheckerDictionaryItem.DictionaryType).Enabled := True;
      SpellCheckerDictionaryItem.DictionaryType.Load;
    end;
  end;

  LoadProject(FLocalizerProject);
end;

function TreeListFindFilter(ANode: TcxTreeListNode; AData: Pointer): Boolean;
begin
  Result := (ANode.Data = AData);
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
    Node := TreeList.Find(FSpellCheckProp, nil, False, True, TreeListFindFilter);

    if (Node <> nil) then
    begin
      Node.MakeVisible;
      Node.Focused := True;
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
  Node: TcxTreeListNode;
begin
  if (Prop.State = lItemStateUnused) then
    Exit(True);

  if (Prop.Status <> lItemStatusTranslate) then
    Exit(True);

  Text := Prop.TranslatedValue[TargetLanguageID];

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
    Prop.TranslatedValue[TargetLanguageID] := CheckedText;

    // Update treenode
    Node := TreeList.Find(Prop, nil, False, True, TreeListFindFilter);
    LoadProjectPropertyNode(Node);
  end;

  Result := (TdxSpellCheckerCracker(SpellChecker).LastDialogResult = mrOK);
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
    TLocalizerProperty(TreeList.FocusedNode.Data).Translations.Remove(TargetLanguageID);
  end else
  begin
    if (not TLocalizerProperty(TreeList.FocusedNode.Data).Translations.TryGetTranslation(TargetLanguageID, Translation)) then
      Translation := TLocalizerProperty(TreeList.FocusedNode.Data).Translations.AddOrUpdateTranslation(TargetLanguageID, TLocalizerProperty(TreeList.FocusedNode.Data).Value);
    Translation.Status := TranslationStatus;
  end;

  LoadProjectPropertyNode(TreeList.FocusedNode);
end;

procedure TFormMain.TreeListColumnStatusPropertiesEditValueChanged(Sender: TObject);
begin
  TLocalizerProperty(TreeList.FocusedNode.Data).Status := TLocalizerItemStatus(TcxImageComboBox(Sender).EditValue);

  LoadProjectPropertyNode(TreeList.FocusedNode);
end;

type
  TcxCustomEditCracker = class(TcxCustomEdit);

procedure TFormMain.TreeListColumnTargetPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
var
  TextEditor: TFormTextEditor;
begin
  TextEditor := TFormTextEditor.Create(nil);
  try
    TextEditor.SourceText := TLocalizerProperty(TreeList.FocusedNode.Data).Value;
    TextEditor.Text := TLocalizerProperty(TreeList.FocusedNode.Data).TranslatedValue[TargetLanguageID];

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

procedure TFormMain.TreeListEditing(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; var Allow: Boolean);
begin
  // Only allow editing of property nodes
  Allow := (Sender.FocusedNode <> nil) and (Sender.FocusedNode.Data <> nil) and (TObject(Sender.FocusedNode.Data) is TLocalizerProperty);
end;

procedure TFormMain.TreeListEditValueChanged(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
begin
  if (AColumn = TreeListColumnTarget) then
  begin
    if (FUpdateLockCount > 0) then
      exit;

    LockUpdates;
    try

      TLocalizerProperty(TreeList.FocusedNode.Data).TranslatedValue[TargetLanguageID] := VarToStr(Sender.InplaceEditor.EditValue);

      LoadProjectPropertyNode(TreeList.FocusedNode);
    finally
      UnlockUpdates;
    end;
  end;
end;

procedure TFormMain.ActionProofingCheckSelectedExecute(Sender: TObject);
begin
  TCustomLocalizerItem(TreeList.FocusedNode.Data).Traverse(
    function(Prop: TLocalizerProperty): boolean
    begin
      Result := PerformSpellCheck(Prop);
    end);

  SpellChecker.ShowSpellingCompleteMessage;
end;

procedure TFormMain.ActionProofingCheckSelectedUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (TreeList.FocusedNode <> nil) and (TreeList.FocusedNode.Data <> nil);
end;

end.