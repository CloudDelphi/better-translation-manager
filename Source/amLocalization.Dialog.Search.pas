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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.StdCtrls,
  RegularExpressions, Diagnostics,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore,
  cxClasses, dxLayoutContainer, dxLayoutControl, cxContainer, cxEdit, dxLayoutcxEditAdapters, dxLayoutControlAdapters, cxButtons,
  cxCheckBox, cxMaskEdit, cxDropDownEdit, cxTextEdit, cxListView, cxCheckComboBox, cxSpinEdit,

  amLocalization.Dialog,
  amLocalization.Model, Vcl.ExtCtrls, cxLabel, dxBar;

type
  ILocalizerSearchHost = interface
    function GetSelectedModule: TLocalizerModule;
    property SelectedModule: TLocalizerModule read GetSelectedModule;

    function GetTargetLanguage: TTargetLanguage;
    property TargetLanguage: TTargetLanguage read GetTargetLanguage;

    procedure ViewItem(Item: TLocalizerProperty);
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
  TFormSearch = class(TFormDialog, ILocalizerSearchProvider)
    dxLayoutItem1: TdxLayoutItem;
    EditSearchText: TcxTextEdit;
    dxLayoutItem2: TdxLayoutItem;
    ComboBoxSearchScope: TcxCheckComboBox;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutItem3: TdxLayoutItem;
    CheckBoxSearchAll: TcxCheckBox;
    dxLayoutItem4: TdxLayoutItem;
    CheckBoxCaseSensitive: TcxCheckBox;
    dxLayoutItem5: TdxLayoutItem;
    CheckBoxRegExp: TcxCheckBox;
    dxLayoutItem6: TdxLayoutItem;
    ButtonRegExHelp: TcxButton;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutSeparatorItem2: TdxLayoutSeparatorItem;
    dxLayoutGroup4: TdxLayoutGroup;
    ActionSearch: TAction;
    ActionClose: TAction;
    LayoutItemList: TdxLayoutItem;
    ListViewResult: TcxListView;
    ActionGoTo: TAction;
    ActionOptionRegExp: TAction;
    ActionOptionCaseSensitive: TAction;
    ActionOptionGlobal: TAction;
    dxLayoutItem11: TdxLayoutItem;
    CheckBoxIgnoreAccelerator: TcxCheckBox;
    ActionOptionIgnoreAccelerator: TAction;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup6: TdxLayoutGroup;
    dxLayoutGroup7: TdxLayoutGroup;
    dxLayoutItem12: TdxLayoutItem;
    cxCheckBox1: TcxCheckBox;
    ActionOptionFuzzy: TAction;
    SpinEditFuzzy: TcxSpinEdit;
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
    procedure ButtonRegExHelpClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure ListViewResultDblClick(Sender: TObject);
    procedure ActionGoToExecute(Sender: TObject);
    procedure ActionGoToUpdate(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionSearchExecute(Sender: TObject);
    procedure ActionSearchUpdate(Sender: TObject);
    procedure ListViewResultEnter(Sender: TObject);
    procedure ListViewResultExit(Sender: TObject);
    procedure ComboBoxSearchScopePropertiesChange(Sender: TObject);
    procedure ActionDummyExecute(Sender: TObject);
    procedure ActionOptionFuzzyUpdate(Sender: TObject);
    procedure ActionOptionRegExpUpdate(Sender: TObject);
    procedure SpinEditFuzzyPropertiesChange(Sender: TObject);
    procedure ActionAbortExecute(Sender: TObject);
    procedure ActionMarkExecute(Sender: TObject);
    procedure ActionMarkUpdate(Sender: TObject);
    procedure ActionMarkSetExecute(Sender: TObject);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
  private
    FSearchText: string;
    FRegExp: TRegEx;
    FSearchScope: TSearchScopes;
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

    // ILocalizerSearchProvider
    procedure Show;
    function SelectNextResult: boolean;
    function CanSelectNextResult: boolean;
    procedure Clear;
  public
    constructor Create(const ASearchHost: ILocalizerSearchHost); reintroduce;
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
  amLocalization.Utils,
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
begin
  inherited Create(Application);
  FSearchHost := ASearchHost;
  FSearchScope := [ssTarget];
  FFuzzyThreshold := 1;
  FLastMessagePump := TStopwatch.StartNew;
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.Show;
begin
  inherited Show;
  SetFocus;
  EditSearchText.SetFocus;
  EditSearchText.SelectAll;
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.SpinEditFuzzyPropertiesChange(Sender: TObject);
begin
  FFuzzyThreshold := SpinEditFuzzy.Value;
end;

// -----------------------------------------------------------------------------

function TFormSearch.CanSelectNextResult: boolean;
begin
  Result := (ListViewResult.Items.Count > 1);
end;

function TFormSearch.SelectNextResult: boolean;
begin
  Result := False;
  if (ListViewResult.Items.Count = 0) then
    exit;

  Result := True;

  if (ListViewResult.ItemIndex = -1) then
    ListViewResult.ItemIndex := 0
  else
  if (ListViewResult.ItemIndex < ListViewResult.Items.Count-1) then
    ListViewResult.ItemIndex := ListViewResult.ItemIndex + 1
  else
    // Wrap around
    ListViewResult.ItemIndex := 0;

  if (Result) then
  begin
    ListViewResult.Selected.MakeVisible(False);
    ActionGoTo.Execute;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.SetStatusText(const Msg: string);
begin
  LayoutItemStatus.CaptionOptions.Text := Msg;
  Update;
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.Clear;
begin
  ListViewResult.Items.Clear;
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
    if (ActionOptionRegExp.Checked) then
    begin
      Result := FRegExp.IsMatch(Value);
      Exit;
    end;

    if (Value = '') then
      Exit(False);

    if (not ActionOptionCaseSensitive.Checked) then
      Value := AnsiUpperCase(Value);

    if (ActionOptionIgnoreAccelerator.Checked) then
      Value := StripAccelerator(Value);

    if (ActionOptionFuzzy.Checked) then
      Result := FuzzyMatch(Value)
    else
      Result := Value.Contains(FSearchText);
  end;

var
  FoundText: string;
  Found: boolean;
  ListItem: TListItem;
resourcestring
  sElementName = 'Element';
  sPropertyName = 'Property';
  sValueSource = 'Source';
  sValueTarget = 'Target';
begin
  // Search in fields
  Found := False;
  FoundText := '';

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
    if Match(Prop.TranslatedValue[FSearchHost.TargetLanguage]) then
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
    ListItem := ListViewResult.Items.Add;
    ListItem.Data := Prop;

    ListItem.Caption := Prop.Item.Module.Name;
    ListItem.SubItems.Add(Prop.Item.Name);
    ListItem.SubItems.Add(Prop.Name);
    ListItem.SubItems.Add(Prop.Value);
    ListItem.SubItems.Add(FoundText);

    SetStatusText(Format(sSearchResultItems, [ListViewResult.Items.Count]));
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

  if (ActionOptionGlobal.Checked) then
    SearchRoot := FSearchHost.SelectedModule.Project
  else
    SearchRoot := FSearchHost.SelectedModule;

  if (ActionOptionRegExp.Checked) then
  begin
    RegExOptions := [roCompiled];
    if (not ActionOptionCaseSensitive.Checked) then
      Include(RegExOptions, roIgnoreCase);

    try

      FRegExp := TRegEx.Create(SearchString, RegExOptions);

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

  ListViewResult.Items.BeginUpdate;
  try

    FAbort := False;

    LayoutItemSearch.Visible := False;
    LayoutItemAbort.Visible := True;
    ActionAbort.Enabled := True;
    FSearchInProgress := True;
    try

      SearchRoot.Traverse(SearchItem);

    finally
      FSearchInProgress := False;
      LayoutItemAbort.Visible := False;
      LayoutItemSearch.Visible := True;
    end;

  finally
    ListViewResult.Items.EndUpdate;
  end;
  ListViewResult.Update;

  SetStatusText(Format(sSearchResultItems, [ListViewResult.Items.Count]));

  if (FAbort) then
    ShowMessage('Search aborted')
  else
  if (ListViewResult.Items.Count = 0) then
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
  ViewItem(TLocalizerProperty(ListViewResult.Selected.Data));
end;

procedure TFormSearch.ActionGoToUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (ListViewResult.SelCount = 1);
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ActionMarkExecute(Sender: TObject);
begin
  PostMessage(ButtonMark.Handle, DXM_DROPDOWNPOPUPMENU, 0, 0);
end;

procedure TFormSearch.ActionMarkSetExecute(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to ListViewResult.Items.Count-1 do
    if (ListViewResult.Items[i].Selected) then
    begin
      TLocalizerProperty(ListViewResult.Items[i].Data).Status := TLocalizerItemStatus(TAction(Sender).Tag);

      if (FSearchHost <> nil) then
        FSearchHost.InvalidateItem(TLocalizerProperty(ListViewResult.Items[i].Data));
    end;
end;

procedure TFormSearch.ActionMarkUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (ListViewResult.SelCount > 0);
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ActionOptionRegExpUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (not ActionOptionFuzzy.Checked);
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ActionOptionFuzzyUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (not ActionOptionRegExp.Checked);
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ActionSearchExecute(Sender: TObject);
begin
  if (ActionOptionFuzzy.Checked) then
    SpinEditFuzzy.Value := Max(1, Min(SpinEditFuzzy.Value, Length(EditSearchText.Text)-1));

  DoSearch(EditSearchText.Text);

  if (ListViewResult.Items.Count > 0) then
  begin
    ListViewResult.Enabled := True;

    // Move focus to the list
    ListViewResult.SetFocus;

    // Select first item
    SelectNextResult;
  end else
    ListViewResult.Enabled := False;
end;

procedure TFormSearch.ActionSearchUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (EditSearchText.Text <> '') and (FSearchScope <> []);
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ButtonRegExHelpClick(Sender: TObject);
var
  s: string;
  RegExHelpURL: string;
begin
  s := '';
  if (not CheckBoxCaseSensitive.Checked) then
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
    ListViewResult.SelectAll;
    Handled := True;
  end;
  if (not Handled) then
    inherited;
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ListViewResultDblClick(Sender: TObject);
begin
  if (ActionGoTo.Enabled) then
    ActionGoTo.Execute;
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ListViewResultEnter(Sender: TObject);
begin
  // Make Goto button default so the user can just press [Enter] to view the item
  ButtonSearch.Default := False;
  ButtonGoto.Default := True;
end;

procedure TFormSearch.ListViewResultExit(Sender: TObject);
begin
  // Make Search button default so the user can just press [Enter] to search
  ButtonSearch.Default := True;
  ButtonGoto.Default := False;
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

end.
