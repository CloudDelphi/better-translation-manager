unit amLocalization.Dialog.Search;

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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.StdCtrls,
  RegularExpressions, Diagnostics, Vcl.ExtCtrls,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore,
  cxClasses, dxLayoutContainer, dxLayoutControl, cxContainer, cxEdit, dxLayoutcxEditAdapters, dxLayoutControlAdapters, cxButtons,
  cxCheckBox, cxMaskEdit, cxDropDownEdit, cxTextEdit, cxCheckComboBox, cxSpinEdit,
  cxLabel, dxBar, cxStyles, cxCustomData,
  cxFilter, cxData, cxDataStorage, cxNavigator, dxDateRanges,
  dxScrollbarAnnotations, cxGridCustomView, cxGridCustomTableView,
  cxGridTableView, cxGridLevel, cxGrid, cxMRUEdit, dxBarBuiltInMenu, cxGridCustomPopupMenu, cxGridPopupMenu,

  amLocalization.Dialog,
  amLocalization.Model;

type
  ILocalizerSearchHost = interface
    function GetProject: TLocalizerProject;
    property Project: TLocalizerProject read GetProject;
    function GetSelectedModule: TLocalizerModule;
    property SelectedModule: TLocalizerModule read GetSelectedModule;

    function GetTranslationLanguage: TTranslationLanguage;
    property TranslationLanguage: TTranslationLanguage read GetTranslationLanguage;

    function ViewItem(Item: TLocalizerProperty): boolean;
    procedure InvalidateItem(Item: TLocalizerProperty);
  end;

  ILocalizerSearchProvider = interface
    procedure Show;
    procedure Clear;

    function SelectNextResult: boolean;
    function CanSelectNextResult: boolean;
  end;

type
  TSearchScope = (ssElement, ssName, ssSource, ssTarget);
  TSearchScopes = set of TSearchScope;

type
  TLocalizerSearchResult = record
    Prop: TLocalizerProperty;
    FoundIn: string;
  end;

  TLocalizerSearchResultList = TList<TLocalizerSearchResult>;

  TLocalizerSearchResultDataSource = class(TcxCustomDataSource)
  private type
    TLocalizerSearchResultColumn = (rcModule, rcItemName, rcType, rcValueName, rcID, rcStatus, rcState, rcSource, rcTarget, rcFoundIn);
  private
    FSearchHost: ILocalizerSearchHost;
    FList: TLocalizerSearchResultList;
  protected
    function GetRecordCount: Integer; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant; override;
    function GetItemHandle(AItemIndex: Integer): TcxDataItemHandle; override;
    function IsMultiThreadingSupported: Boolean; override;
  public
    constructor Create(const ASearchHost: ILocalizerSearchHost; AList: TLocalizerSearchResultList);

    procedure DataChanged; override;
  end;

type
  TFormSearch = class(TFormDialog, ILocalizerSearchProvider)
    dxLayoutItem1: TdxLayoutItem;
    EditSearchText: TcxMRUEdit;
    dxLayoutItem2: TdxLayoutItem;
    ComboBoxSearchScope: TcxCheckComboBox;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutItem3: TdxLayoutItem;
    CheckBoxOptionSearchAll: TcxCheckBox;
    dxLayoutItem4: TdxLayoutItem;
    CheckBoxOptionCaseSensitive: TcxCheckBox;
    dxLayoutItem5: TdxLayoutItem;
    CheckBoxOptionRegExp: TcxCheckBox;
    dxLayoutItem6: TdxLayoutItem;
    ButtonRegExHelp: TcxButton;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutSeparatorItem2: TdxLayoutSeparatorItem;
    dxLayoutGroup4: TdxLayoutGroup;
    ActionSearch: TAction;
    ActionClose: TAction;
    ActionGoTo: TAction;
    ActionOptionRegExp: TAction;
    ActionOptionCaseSensitive: TAction;
    ActionOptionGlobal: TAction;
    dxLayoutItem11: TdxLayoutItem;
    CheckBoxOptionIgnoreAccelerator: TcxCheckBox;
    ActionOptionNormalize: TAction;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup6: TdxLayoutGroup;
    dxLayoutGroup7: TdxLayoutGroup;
    dxLayoutItem12: TdxLayoutItem;
    CheckBoxOptionFuzzy: TcxCheckBox;
    ActionOptionFuzzy: TAction;
    EditOptionFuzzy: TcxSpinEdit;
    dxLayoutItem13: TdxLayoutItem;
    ActionAbort: TAction;
    dxLayoutItem8: TdxLayoutItem;
    ButtonGoto: TcxButton;
    LayoutItemAbort: TdxLayoutItem;
    ButtonAbort: TcxButton;
    LayoutItemSearch: TdxLayoutItem;
    ButtonSearch: TcxButton;
    LayoutItemClose: TdxLayoutItem;
    ButtonClose: TcxButton;
    LayoutItemStatus: TdxLayoutLabeledItem;
    dxLayoutItem7: TdxLayoutItem;
    ButtonMark: TcxButton;
    ActionMark: TAction;
    PopupMenuMark: TdxBarPopupMenu;
    BarManagerSearch: TdxBarManager;
    dxBarButton1: TdxBarButton;
    dxBarButton2: TdxBarButton;
    dxBarButton3: TdxBarButton;
    ActionMarkTranslate: TAction;
    ActionMarkHold: TAction;
    ActionMarkDontTranslate: TAction;
    ActionOptionExact: TAction;
    dxLayoutItem9: TdxLayoutItem;
    CheckBoxOptionExact: TcxCheckBox;
    CheckComboBoxStatus: TcxCheckComboBox;
    dxLayoutItem14: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    CheckComboBoxState: TcxCheckComboBox;
    dxLayoutGroup5: TdxLayoutGroup;
    GridResultLevel: TcxGridLevel;
    GridResult: TcxGrid;
    LayoutItemGrid: TdxLayoutItem;
    GridResultTableView: TcxGridTableView;
    GridResultTableViewColumnModule: TcxGridColumn;
    GridResultTableViewColumnItemName: TcxGridColumn;
    GridResultTableViewColumnValueName: TcxGridColumn;
    GridResultTableViewColumnSource: TcxGridColumn;
    GridResultTableViewColumnTarget: TcxGridColumn;
    GridResultTableViewColumnFoundIn: TcxGridColumn;
    GridResultTableViewColumnType: TcxGridColumn;
    GridResultTableViewColumnStatus: TcxGridColumn;
    GridResultTableViewColumnState: TcxGridColumn;
    GridResultTableViewColumnID: TcxGridColumn;
    GridPopupMenu: TcxGridPopupMenu;
    ActionMarkProposed: TAction;
    ActionMarkTranslated: TAction;
    ActionMarkNotTranslated: TAction;
    dxBarButton4: TdxBarButton;
    dxBarButton5: TdxBarButton;
    dxBarButton6: TdxBarButton;
    dxBarSeparator1: TdxBarSeparator;
    procedure ButtonRegExHelpClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure ActionGoToExecute(Sender: TObject);
    procedure ActionGoToUpdate(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionSearchExecute(Sender: TObject);
    procedure ActionSearchUpdate(Sender: TObject);
    procedure ComboBoxSearchScopePropertiesChange(Sender: TObject);
    procedure ActionDummyExecute(Sender: TObject);
    procedure ActionOptionFuzzyUpdate(Sender: TObject);
    procedure ActionOptionRegExpUpdate(Sender: TObject);
    procedure SpinEditFuzzyPropertiesChange(Sender: TObject);
    procedure ActionAbortExecute(Sender: TObject);
    procedure ActionMarkExecute(Sender: TObject);
    procedure ActionMarkUpdate(Sender: TObject);
    procedure ActionMarkSetStatusExecute(Sender: TObject);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure ActionOptionExactUpdate(Sender: TObject);
    procedure GridResultTableViewCellDblClick(Sender: TcxCustomGridTableView;
      ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton;
      AShift: TShiftState; var AHandled: Boolean);
    procedure GridResultEnter(Sender: TObject);
    procedure GridResultExit(Sender: TObject);
    procedure ActionMarkProposedExecute(Sender: TObject);
    procedure ActionMarkTranslatedExecute(Sender: TObject);
    procedure ActionMarkNotTranslatedExecute(Sender: TObject);
  private type
    TLocalizerItemStatusSet = set of TLocalizerItemStatus;
    TTranslationStatusSet = set of TTranslationStatus;
  private
    FSearchResult: TLocalizerSearchResultList;
    FSearchResultDataSource: TLocalizerSearchResultDataSource;
    FSearchText: string;
    FRegExp: TRegEx;
    FSearchScope: TSearchScopes;
    FSearchStatus: TLocalizerItemStatusSet;
    FSearchState: TTranslationStatusSet;
    FSearchHost: ILocalizerSearchHost;
    FFuzzyThreshold: integer;
    FLastMessagePump: TStopwatch;
    FAbort: boolean;
    FSearchInProgress: boolean;
  protected
    procedure ViewItem(Item: TLocalizerProperty);
    function SearchItem(Prop: TLocalizerProperty): boolean;
    procedure DoSearch(const SearchString: string);
    procedure SetStatusText(const Msg: string);
    procedure ProcessMessages;
    procedure SetTranslationStatus(Prop: TLocalizerProperty; Status: TTranslationStatus);

    // ILocalizerSearchProvider
    procedure Show;
    function SelectNextResult: boolean;
    function CanSelectNextResult: boolean;
    procedure Clear;
  public
    constructor Create(const ASearchHost: ILocalizerSearchHost); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  UITypes,
  Math,
  RegularExpressionsCore,
  dxMessages,
{$ifdef MADEXCEPT}
  MadExcept,
{$endif MADEXCEPT}
  amCursorService,
  amShell,
  amLocale,
  amLocalization.Settings,
  amLocalization.Normalization,
  amLocalization.Data.Main;

resourcestring
  sSearchResultItems = 'Found: %d items';

const
  UpdateRate = 100; // mS
  MessagePumpRate = 50; // mS

var
  ProgressDeferTime: integer = 500; // mS
  ProgressUpdateFactor: integer = 1;

// -----------------------------------------------------------------------------

constructor TFormSearch.Create(const ASearchHost: ILocalizerSearchHost);
var
  LocaleItem: TLocaleItem;
begin
  inherited Create(Application);
  FSearchHost := ASearchHost;
  FSearchScope := [ssTarget];
  FFuzzyThreshold := 1;
  FLastMessagePump := TStopwatch.StartNew;

  LocaleItem := TLocaleItems.FindLCID(FSearchHost.Project.SourceLanguageID);
  GridResultTableViewColumnSource.Caption := LocaleItem.LanguageName;

  FSearchResult := TLocalizerSearchResultList.Create;
  FSearchResultDataSource := TLocalizerSearchResultDataSource.Create(FSearchHost, FSearchResult);
  GridResultTableView.DataController.CustomDataSource := FSearchResultDataSource;

  EditSearchText.Properties.LookupItems.Assign(TranslationManagerSettings.Search.History);
  ComboBoxSearchScope.EditValue := TranslationManagerSettings.Search.Scope;
  GridResultTableView.RestoreFromRegistry(TranslationManagerSettings.Layout.KeyPath, False, False, [gsoUseFilter], TranslationManagerSettings.Layout.SearchResult.Name);
end;

destructor TFormSearch.Destroy;
begin
  TranslationManagerSettings.Search.History.Assign(EditSearchText.Properties.LookupItems);
  TranslationManagerSettings.Search.Scope := ComboBoxSearchScope.EditValue;
  GridResultTableView.StoreToRegistry(TranslationManagerSettings.Layout.KeyPath, False, [gsoUseFilter], TranslationManagerSettings.Layout.SearchResult.Name);

  FSearchResultDataSource.Free;
  FSearchResult.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.Show;
var
  LocaleItem: TLocaleItem;
begin
  LocaleItem := TLocaleItems.FindLCID(FSearchHost.TranslationLanguage.LanguageID);
  GridResultTableViewColumnTarget.Caption := LocaleItem.LanguageName;

  inherited Show;
  SetFocus;
  EditSearchText.SetFocus;
  EditSearchText.SelectAll;
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.SpinEditFuzzyPropertiesChange(Sender: TObject);
begin
  FFuzzyThreshold := EditOptionFuzzy.Value;
end;

// -----------------------------------------------------------------------------

function TFormSearch.CanSelectNextResult: boolean;
begin
  Result := (FSearchResult.Count > 1);
end;

function TFormSearch.SelectNextResult: boolean;
begin
  if (FSearchResult.Count = 0) then
    Exit(False);

  GridResultTableView.Controller.GoToNext(False);
  GridResultTableView.Controller.ClearSelection;

  if (GridResultTableView.Controller.FocusedRow <> nil) then
    GridResultTableView.Controller.FocusedRow.MakeVisible;

  ActionGoTo.Execute;

  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.SetStatusText(const Msg: string);
begin
  LayoutItemStatus.CaptionOptions.Text := Msg;
  Update;
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.SetTranslationStatus(Prop: TLocalizerProperty; Status: TTranslationStatus);
var
  Translation: TLocalizerTranslation;
begin
  if (FSearchHost = nil) then
    Exit;

  if (Status in [tStatusProposed, tStatusTranslated]) then
  begin
    if (Prop.Translations.TryGetTranslation(FSearchHost.TranslationLanguage, Translation)) then
    begin
      if (Translation.Status = Status) then
        Exit;
      Translation.Status := Status;
    end else
      Translation := Prop.Translations.AddOrUpdateTranslation(FSearchHost.TranslationLanguage, Prop.Value, Status);

  end else
  if (Status = tStatusPending) then
  begin
    if (Prop.Translations.FindTranslation(FSearchHost.TranslationLanguage) = nil) then
      Exit;
    Prop.Translations.Remove(FSearchHost.TranslationLanguage);
  end;

  FSearchHost.InvalidateItem(Prop);
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.Clear;
begin
  FSearchResult.Clear;
  FSearchResultDataSource.DataChanged;
  SetStatusText('');
end;

procedure TFormSearch.ComboBoxSearchScopePropertiesChange(Sender: TObject);
begin
  FSearchScope := [];

  if (ComboBoxSearchScope.States[0] = cbsChecked) then
    Include(FSearchScope, ssElement);

  if (ComboBoxSearchScope.States[1] = cbsChecked) then
    Include(FSearchScope, ssName);

  if (ComboBoxSearchScope.States[2] = cbsChecked) then
    Include(FSearchScope, ssSource);

  if (ComboBoxSearchScope.States[3] = cbsChecked) then
    Include(FSearchScope, ssTarget);
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ViewItem(Item: TLocalizerProperty);
begin
  ASSERT(Item <> nil);

  if (FSearchHost <> nil) then
    FSearchHost.ViewItem(Item);
end;

// -----------------------------------------------------------------------------

function TFormSearch.SearchItem(Prop: TLocalizerProperty): boolean;

  // Based on: https://stackoverflow.com/a/10593797/2249664
  function LevenshteinDistance(const ValueA, ValueB: string): integer;
  var
    LengthA, LengthB: integer;
    Costs: array of array of integer;
    i, j: integer;
  begin
    LengthA := Length(ValueA);
    LengthB := Length(ValueB);

    if (LengthA = 0) then
      Exit(LengthB);
    if (LengthB = 0) then
      Exit(LengthA);

    SetLength(Costs, LengthA+1, LengthB+1);
    for i := 0 to LengthA do
      Costs[i, 0] := i;
    for j := 0 to LengthB do
      Costs[0, j] := j;

    for i := 1 to LengthA do
      for j := 1 to LengthB do
        Costs[i,j] :=
          Min(Min(Costs[i-1, j]  +1,                            // Deletion
                  Costs[i,   j-1]+1),                           // Insertion
                  Costs[i-1, j-1]+Ord(ValueA[i] <> ValueB[j])); // Substitution

    Result := Costs[LengthA, LengthB];
  end;

  function FuzzyMatch(const Value: string): boolean;
  var
    i: integer;
  begin
    for i := 1 to Length(Value)-Length(FSearchText)+FFuzzyThreshold+1 do
      if (LevenshteinDistance(FSearchText, Copy(Value, i, Length(FSearchText)+FFuzzyThreshold)) <= FFuzzyThreshold) then
        Exit(True);
    Result := False;
  end;

  function Match(Value: string): boolean;
  begin
    if (not ActionOptionCaseSensitive.Checked) then
      Value := AnsiUpperCase(Value);

    if (ActionOptionNormalize.Checked) then
      Value := SanitizeText(Value);

    if (ActionOptionRegExp.Checked) then
    begin
      // RegEx is a dummy in case the search string is empty
      Result := (FSearchText = '') or (FRegExp.IsMatch(Value));
      Exit;
    end;

    if (Value = '') then
      Exit(FSearchText = '');

    if (ActionOptionFuzzy.Checked) then
      Result := FuzzyMatch(Value)
    else
    if (ActionOptionExact.Checked) then
      Result := (Value = FSearchText)
    else
      Result := (FSearchText = '') or (Value.Contains(FSearchText));
  end;

var
  FoundText: string;
  Found: boolean;
  SearchResult: TLocalizerSearchResult;
  State: TTranslationStatus;
  Translation: TLocalizerTranslation;
resourcestring
  sElementName = 'Element';
  sPropertyName = 'Property';
  sValueSource = 'Source';
  sValueTarget = 'Target';
begin
  // Search in fields
  Found := False;
  FoundText := '';

  if (FSearchStatus <> []) and (not(Prop.EffectiveStatus in FSearchStatus)) then
    Exit(True);

  if (FSearchState <> []) then
  begin
    if (Prop.Translations.TryGetTranslation(FSearchHost.TranslationLanguage, Translation)) then
      State := Translation.Status
    else
      State := tStatusPending;
    if (not(State in FSearchState)) then
      Exit(True);
  end;

  if (ssElement in FSearchScope) then
  begin
    if Match(Prop.Item.Name) then
    begin
      if (not Found) then
        FoundText := sElementName
      else
        FoundText := FoundText + ', ' + sElementName;

      Found := True;
    end;
  end;

  if (ssName in FSearchScope) then
  begin
    if Match(Prop.Name) then
    begin
      if (not Found) then
        FoundText := sPropertyName
      else
        FoundText := FoundText + ', ' + sPropertyName;

      Found := True;
    end;
  end;

  if (ssSource in FSearchScope) then
  begin
    if Match(Prop.Value) then
    begin
      if (not Found) then
        FoundText := sValueSource
      else
        FoundText := FoundText + ', ' + sValueSource;

      Found := True;
    end;
  end;

  if (ssTarget in FSearchScope) then
  begin
    if Match(Prop.TranslatedValue[FSearchHost.TranslationLanguage]) then
    begin
      if (not Found) then
        FoundText := sValueTarget
      else
        FoundText := FoundText + ', ' + sValueTarget;

      Found := True;
    end;
  end;

  if (Found) then
  begin
    SearchResult.Prop := Prop;
    SearchResult.FoundIn := FoundText;
    FSearchResult.Add(SearchResult);

    SetStatusText(Format(sSearchResultItems, [FSearchResult.Count]));
  end;

  ProcessMessages;
  Result := (not FAbort);
end;

procedure TFormSearch.DoSearch(const SearchString: string);
var
  SearchRoot: TBaseLocalizerItem;
  RegExOptions: TRegExOptions;
resourcestring
  sSearchSearching = 'Searching';
begin
  Clear;

  SaveCursor(crAppStart);

  FSearchText := Trim(SearchString);
  FSearchStatus := TLocalizerItemStatusSet(Byte(CheckComboBoxStatus.Value));;
  FSearchState := TTranslationStatusSet(Byte(CheckComboBoxState.Value));

  if (ActionOptionGlobal.Checked) or (FSearchHost.SelectedModule = nil) then
    SearchRoot := FSearchHost.Project
  else
    SearchRoot := FSearchHost.SelectedModule;

  if (ActionOptionRegExp.Checked) then
  begin
    RegExOptions := [roCompiled];
    if (not ActionOptionCaseSensitive.Checked) then
      Include(RegExOptions, roIgnoreCase);

    try

      if (SearchString <> '') then
        FRegExp := TRegEx.Create(SearchString, RegExOptions)
      else
        // Empty regex raises exception so we need to handle an empty search string explicitly.
        // The regex we create here is a dummy as we test for empty search string again in SearchItem.Match()
        FRegExp := TRegEx.Create('.*', RegExOptions);

    except
      on E: ERegularExpressionError do
      begin
        ButtonRegExHelp.Visible := True;
        TaskMessageDlg('Regular Expression Error', E.Message, mtWarning, [mbOK], -1);
        exit;
      end;
    end;
  end else
  if (not ActionOptionCaseSensitive.Checked) then
    FSearchText := AnsiUpperCase(FSearchText);

  GridResultTableView.BeginUpdate(lsimImmediate);
  try

    Update;
    FAbort := False;

    LayoutItemSearch.Visible := False;
    LayoutItemAbort.Visible := True;
    ActionAbort.Enabled := True;
    FSearchInProgress := True;
    try

      SearchRoot.Traverse(SearchItem, True);

      FSearchResultDataSource.DataChanged;

    finally
      FSearchInProgress := False;
      LayoutItemAbort.Visible := False;
      LayoutItemSearch.Visible := True;
    end;

  finally
    GridResultTableView.EndUpdate;
  end;

  SetStatusText(Format(sSearchResultItems, [FSearchResult.Count]));

  if (FAbort) then
    ShowMessage('Search aborted')
  else
  if (FSearchResult.Count = 0) then
    ShowMessage('No results found');
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ActionDummyExecute(Sender: TObject);
begin
//
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ActionCloseExecute(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ActionAbortExecute(Sender: TObject);
begin
  FAbort := True;
  TAction(Sender).Enabled := False;
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ActionGoToExecute(Sender: TObject);
begin
  if (GridResultTableView.DataController.FocusedRecordIndex = -1) then
    exit;

  var Index: integer := GridResultTableView.DataController.FocusedRecordIndex;

  ViewItem(FSearchResult[Index].Prop);
end;

procedure TFormSearch.ActionGoToUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (GridResultTableView.Controller.SelectedRowCount = 1);
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ActionMarkNotTranslatedExecute(Sender: TObject);
begin
  for var i := 0 to GridResultTableView.Controller.SelectedRowCount-1 do
  begin
    var Prop := FSearchResult[GridResultTableView.Controller.SelectedRows[i].RecordIndex].Prop;
    SetTranslationStatus(Prop, tStatusPending);
    GridResultTableView.Controller.SelectedRows[i].Invalidate;
  end;
end;

procedure TFormSearch.ActionMarkProposedExecute(Sender: TObject);
begin
  for var i := 0 to GridResultTableView.Controller.SelectedRowCount-1 do
  begin
    var Prop := FSearchResult[GridResultTableView.Controller.SelectedRows[i].RecordIndex].Prop;
    SetTranslationStatus(Prop, tStatusProposed);
    GridResultTableView.Controller.SelectedRows[i].Invalidate;
  end;
end;

procedure TFormSearch.ActionMarkTranslatedExecute(Sender: TObject);
begin
  for var i := 0 to GridResultTableView.Controller.SelectedRowCount-1 do
  begin
    var Prop := FSearchResult[GridResultTableView.Controller.SelectedRows[i].RecordIndex].Prop;
    SetTranslationStatus(Prop, tStatusTranslated);
    GridResultTableView.Controller.SelectedRows[i].Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ActionMarkSetStatusExecute(Sender: TObject);
var
  i: integer;
  Prop: TLocalizerProperty;
begin
  for i := 0 to GridResultTableView.Controller.SelectedRowCount-1 do
  begin
    Prop := FSearchResult[GridResultTableView.Controller.SelectedRows[i].RecordIndex].Prop;

    Prop.Status := TLocalizerItemStatus(TAction(Sender).Tag);

    if (FSearchHost <> nil) then
      FSearchHost.InvalidateItem(Prop);

    GridResultTableView.Controller.SelectedRows[i].Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ActionMarkExecute(Sender: TObject);
begin
  PostMessage(ButtonMark.Handle, DXM_DROPDOWNPOPUPMENU, 0, 0);
end;

procedure TFormSearch.ActionMarkUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (GridResultTableView.Controller.SelectedRowCount > 0);
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ActionOptionRegExpUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (not ActionOptionFuzzy.Checked) and (not ActionOptionExact.Checked);
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ActionOptionExactUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (not ActionOptionFuzzy.Checked) and (not ActionOptionRegExp.Checked);
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ActionOptionFuzzyUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (not ActionOptionRegExp.Checked) and (not ActionOptionExact.Checked);
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ActionSearchExecute(Sender: TObject);
begin
  if (ActionOptionFuzzy.Checked) then
    EditOptionFuzzy.Value := Max(1, Min(EditOptionFuzzy.Value, Length(EditSearchText.Text)-1));

  DoSearch(EditSearchText.Text);

  if (FSearchResult.Count > 0) then
  begin
    LayoutItemGrid.Enabled := True;

    // Move focus to the list
    GridResult.SetFocus;

    // Select first item
    SelectNextResult;
  end else
    LayoutItemGrid.Enabled := False;
end;

procedure TFormSearch.ActionSearchUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FSearchHost.Project <> nil) and ((EditSearchText.Text <> '') or (FSearchScope <> []));
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ButtonRegExHelpClick(Sender: TObject);
var
  s: string;
  RegExHelpURL: string;
begin
  s := '';
  if (not CheckBoxOptionCaseSensitive.Checked) then
    s := s + 'i';

  RegExHelpURL := Format('https://regex101.com/?regex=%s&options=%s', [EditSearchText.Text, s]);

  Shell.DisplayURL(RegExHelpURL, Self);
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (FSearchInProgress) then
    Action := caNone
  else
    Action := caHide;
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
    Close;
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
begin
  if (ShortCutFromMessage(Msg) = ShortCut(Ord('A'), [ssCtrl])) then
  begin
    GridResultTableView.Controller.SelectAll;
    Handled := True;
  end;
  if (not Handled) then
    inherited;
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.GridResultEnter(Sender: TObject);
begin
  // Make Goto button default so the user can just press [Enter] to view the item
  ButtonSearch.Default := False;
  ButtonGoto.Default := True;
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.GridResultExit(Sender: TObject);
begin
  // Make Search button default so the user can just press [Enter] to search
  ButtonSearch.Default := True;
  ButtonGoto.Default := False;
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.GridResultTableViewCellDblClick(
  Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo;
  AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
begin
  if (ActionGoTo.Enabled) then
    ActionGoTo.Execute;
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ProcessMessages;
var
  Msg: TMsg;
begin
  if (FLastMessagePump.ElapsedMilliseconds < MessagePumpRate * ProgressUpdateFactor) then
    exit;

{$ifdef MADEXCEPT}
  // Indicate to MadExcept freeze detection that we're not frozen
  MadExcept.ImNotFrozen;
{$endif MADEXCEPT}

//  BeginUpdate;
  try
    // Allow threads to synchronize to avoid deadlock (e.g. busy loop showing progress waiting for thread to complete (e.g. spell check dictionary load)).
    CheckSynchronize;

    Msg.message := 0;
    try
      // Process mouse messages for Abort Button so user can press it
      while (PeekMessage(Msg, ButtonAbort.Handle, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE)) do
      begin
        if (Msg.message = WM_QUIT) then
          exit;

        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;

      // Process mouse hover/enter/exit messages for this window so button state will be updated
      while (PeekMessage(Msg, Handle, WM_NCMOUSEHOVER, WM_MOUSELEAVE, PM_REMOVE)) do
      begin
        if (Msg.message = WM_QUIT) then
          exit;

        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;

      // Process cursor update messages for this window so cursor stays responsive
      while (PeekMessage(Msg, Handle, WM_SETCURSOR, WM_SETCURSOR, PM_REMOVE)) do
      begin
        if (Msg.message = WM_QUIT) then
          exit;

        DispatchMessage(Msg);
      end;

      // Process NC mouse messages for this window so we can move the window
      // Supposedly WM_NCMOUSEMOVE causes the WM_NCHITTEST message
      // Unfortunately this open up for the user to click the Close button. We handle that
      // in the form OnClose handler.
      while (PeekMessage(Msg, Handle, WM_NCMOUSEMOVE, WM_NCXBUTTONDBLCLK, PM_REMOVE)) do
      begin
        if (Msg.message = WM_QUIT) then
          exit;

        DispatchMessage(Msg);
      end;

      // Process NC hit test messages for this window so we can move the window
      while (PeekMessage(Msg, Handle, WM_NCHITTEST, WM_NCHITTEST, PM_REMOVE)) do
      begin
        if (Msg.message = WM_QUIT) then
          exit;

        DispatchMessage(Msg);
      end;

(*
      // Process progress bar messages - This includes WM_TIMER and WM_PAINT used for progress bar animation
      while PeekMessage(Msg, ProgressBar.Handle, 0, 0, PM_REMOVE) do
      begin
        if (Msg.message = WM_QUIT) then
          exit;

        DispatchMessage(Msg);
      end;
*)

      // Process paint messages for all windows so UI can repaint itself
      while PeekMessage(Msg, 0, WM_PAINT, WM_PAINT, PM_REMOVE) or
        PeekMessage(Msg, 0, WM_ERASEBKGND, WM_ERASEBKGND, PM_REMOVE) or
        PeekMessage(Msg, Handle, DXM_SKINS_POSTREDRAW, DXM_SKINS_POSTREDRAW, PM_REMOVE) or
        PeekMessage(Msg, 0, WM_SIZE, WM_SIZE, PM_REMOVE) or
        PeekMessage(Msg, 0, WM_SIZING, WM_SIZING, PM_REMOVE) or
        PeekMessage(Msg, 0, WM_MOVE, WM_MOVE, PM_REMOVE) or
        PeekMessage(Msg, 0, WM_MOVING, WM_MOVING, PM_REMOVE) or
        PeekMessage(Msg, 0, WM_WINDOWPOSCHANGING, WM_WINDOWPOSCHANGED, PM_REMOVE) or
        PeekMessage(Msg, 0, WM_PRINT, WM_PRINT, PM_REMOVE) or
        PeekMessage(Msg, 0, WM_PRINTCLIENT, WM_PRINTCLIENT, PM_REMOVE) do
      begin
        if (Msg.message = WM_QUIT) then
          exit;

        DispatchMessage(Msg);
      end;

      PeekMessage(Msg, 0, WM_NULL, WM_NULL, PM_NOREMOVE); // Avoid window ghosting due to unresponsiveness on Vista+

    finally
      if (Msg.message = WM_QUIT) then
      begin
        PostQuitMessage(Msg.wParam);
        Abort;
      end;
    end;

    if (Msg.message = 0) then
      FLastMessagePump.Reset;

  finally
    FLastMessagePump.Start;
//    EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

{ TLocalizerSearchResultDataSource }

constructor TLocalizerSearchResultDataSource.Create(const ASearchHost: ILocalizerSearchHost; AList: TLocalizerSearchResultList);
begin
  inherited Create;
  FSearchHost := ASearchHost;
  FList := AList;
end;

procedure TLocalizerSearchResultDataSource.DataChanged;
begin
  inherited;

end;

function TLocalizerSearchResultDataSource.GetItemHandle(AItemIndex: Integer): TcxDataItemHandle;
begin
  // Contrary to documentation and posts by DevExpress support, it seems we do not
  // need to translate the ItemIndex.
  // var GridColumn := TcxCustomGridTableItem(DataController.GetItem(AItemIndex));
  // Result := TcxDataItemHandle(GridColumn.ID);

  Result := TcxDataItemHandle(AItemIndex);
end;

function TLocalizerSearchResultDataSource.GetRecordCount: Integer;
begin
  Result := FList.Count;
end;

function TLocalizerSearchResultDataSource.GetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant;
var
  Translation: TLocalizerTranslation;
begin
  var Prop := FList[NativeInt(ARecordHandle)].Prop;
  var ItemIndex := TLocalizerSearchResultColumn(AItemHandle);

  case ItemIndex of
    rcModule:
      Result := Prop.Item.Module.Name;

    rcItemName:
      Result := Prop.Item.Name;

    rcType:
      Result := Prop.Item.TypeName;

    rcValueName:
      Result := Prop.Name;

    rcID:
      Result := Prop.Item.ResourceID.ToString;

    rcStatus:
      Result := Ord(Prop.Status);

    rcState:
      if (Prop.Translations.TryGetTranslation(FSearchHost.TranslationLanguage, Translation)) then
        Result := Ord(Translation.Status)
      else
        Result := Ord(tStatusPending);

    rcSource:
      Result := Prop.Value;

    rcTarget:
      // Don't use Prop.TranslatedValue[] here. We need to get "obsolete" values too
      if (Prop.Translations.TryGetTranslation(FSearchHost.TranslationLanguage, Translation)) then
        Result := Translation.Value
      else
        Result := Prop.Value;

    rcFoundIn:
      Result := FList[NativeInt(ARecordHandle)].FoundIn;
  else
    Result := Format('Invalid column: %d', [Ord(ItemIndex)]);
  end;
end;

function TLocalizerSearchResultDataSource.IsMultiThreadingSupported: Boolean;
begin
  // We're assuming that DataChanged will never get called while another thread is fetching data
  Result := True;
end;

end.
