unit amLocalization.Dialog.Search;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.StdCtrls,
  RegularExpressions,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore,
  cxClasses, dxLayoutContainer, dxLayoutControl, cxContainer, cxEdit, dxLayoutcxEditAdapters, dxLayoutControlAdapters, cxButtons,
  cxCheckBox, cxMaskEdit, cxDropDownEdit, cxTextEdit, cxListView, cxCheckComboBox,

  amLocalization.Model;

type
  ILocalizerSearchHost = interface
    function GetSelectedModule: TLocalizerModule;
    property SelectedModule: TLocalizerModule read GetSelectedModule;

    function GetTargetLanguage: TTargetLanguage;
    property TargetLanguage: TTargetLanguage read GetTargetLanguage;

    procedure ViewItem(Item: TLocalizerProperty);
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
  TFormSearch = class(TForm, ILocalizerSearchProvider)
    LayoutControlGroup_Root: TdxLayoutGroup;
    LayoutControl: TdxLayoutControl;
    dxLayoutItem1: TdxLayoutItem;
    EditSearchText: TcxTextEdit;
    dxLayoutItem2: TdxLayoutItem;
    ComboBoxSearchScope: TcxCheckComboBox;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
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
    dxLayoutItem8: TdxLayoutItem;
    ButtonSearch: TcxButton;
    dxLayoutItem9: TdxLayoutItem;
    ButtonClose: TcxButton;
    dxLayoutGroup5: TdxLayoutGroup;
    ActionListDialog: TActionList;
    ActionSearch: TAction;
    ActionClose: TAction;
    dxLayoutItem7: TdxLayoutItem;
    ListViewResult: TcxListView;
    ActionGoTo: TAction;
    ActionRegExp: TAction;
    LayoutItemStatus: TdxLayoutLabeledItem;
    ActionCaseSensitive: TAction;
    ActionGlobal: TAction;
    dxLayoutItem10: TdxLayoutItem;
    ButtonGoto: TcxButton;
    dxLayoutItem11: TdxLayoutItem;
    CheckBoxIgnoreAccelerator: TcxCheckBox;
    ActionIgnoreAccelerator: TAction;
    procedure ButtonRegExHelpClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure ListViewResultDblClick(Sender: TObject);
    procedure ActionGoToExecute(Sender: TObject);
    procedure ActionGoToUpdate(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionSearchExecute(Sender: TObject);
    procedure ActionSearchUpdate(Sender: TObject);
    procedure ActionRegExpExecute(Sender: TObject);
    procedure ListViewResultEnter(Sender: TObject);
    procedure ListViewResultExit(Sender: TObject);
    procedure ComboBoxSearchScopePropertiesChange(Sender: TObject);
    procedure ActionDummyExecute(Sender: TObject);
  private
    FSearchText: string;
    FRegExpMode: boolean;
    FRegExp: TRegEx;
    FSearchScope: TSearchScopes;
    FSearchHost: ILocalizerSearchHost;
  protected
    procedure ViewItem(Item: TLocalizerProperty);
    function SearchItem(Prop: TLocalizerProperty): boolean;
    procedure DoSearch(const SearchString: string);
    procedure SetStatusText(const Msg: string);

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
  RegularExpressionsCore,
  amCursorService,
  amLocalization.Data.Main;

resourcestring
  sSearchResultItems = 'Found: %d items';

// -----------------------------------------------------------------------------

constructor TFormSearch.Create(const ASearchHost: ILocalizerSearchHost);
begin
  inherited Create(Application);
  FSearchHost := ASearchHost;
  FSearchScope := [ssTarget];
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

  function Match(Value: string): boolean;
  begin
    if (FRegExpMode) then
    begin
      Result := FRegExp.IsMatch(Value);
      Exit;
    end;

    if (Value = '') then
      Exit(False);

    if (not ActionCaseSensitive.Checked) then
      Value := AnsiUpperCase(Value);

    if (ActionIgnoreAccelerator.Checked) then
      Value := StripHotkey(Value);

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

  Result := True;
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

  if (ActionGlobal.Checked) then
    SearchRoot := FSearchHost.SelectedModule.Project
  else
    SearchRoot := FSearchHost.SelectedModule;

  if (FRegExpMode) then
  begin
    RegExOptions := [roCompiled];
    if (not ActionCaseSensitive.Checked) then
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
  if (not ActionCaseSensitive.Checked) then
    FSearchText := AnsiUpperCase(FSearchText);

  ListViewResult.Items.BeginUpdate;
  try

    SearchRoot.Traverse(SearchItem);

  finally
    ListViewResult.Items.EndUpdate;
  end;
  ListViewResult.Update;

  SetStatusText(Format(sSearchResultItems, [ListViewResult.Items.Count]));

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

procedure TFormSearch.ActionGoToExecute(Sender: TObject);
begin
  ViewItem(TLocalizerProperty(ListViewResult.Selected.Data));
end;

procedure TFormSearch.ActionGoToUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (ListViewResult.Selected <> nil);
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ActionRegExpExecute(Sender: TObject);
begin
  FRegExpMode := TAction(Sender).Checked;;
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.ActionSearchExecute(Sender: TObject);
begin
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

//  SigmaHelp.DisplayURL(RegExHelpURL, Self);
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

// -----------------------------------------------------------------------------

procedure TFormSearch.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
    Close;
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

end.
