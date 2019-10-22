unit amLocalization.Dialog.Filters;

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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, System.Actions, Vcl.ActnList, Vcl.ExtCtrls,
  Vcl.StdCtrls,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore, dxLayoutControlAdapters,
  dxLayoutcxEditAdapters, dxLayoutContainer, cxContainer, cxEdit, cxCustomData, cxStyles, cxTL, cxTextEdit,
  cxTLdxBarBuiltInMenu, cxInplaceContainer, cxLabel, cxClasses, cxButtons, dxLayoutControl, cxDropDownEdit,
  cxImageComboBox, cxEditRepositoryItems,

  amLocalization.Dialog,
  amLocalization.Filters;

type
  TFormFilters = class(TFormDialog)
    TreeListFilters: TcxTreeList;
    LayoutItemFilters: TdxLayoutItem;
    TreeListFiltersColumnField: TcxTreeListColumn;
    TreeListFiltersColumnValue: TcxTreeListColumn;
    TreeListFiltersColumnGroup: TcxTreeListColumn;
    TreeListFiltersColumnOperator: TcxTreeListColumn;
    EditRepository: TcxEditRepository;
    EditRepositoryTextItem: TcxEditRepositoryTextItem;
    procedure TreeListFiltersBeginDragNode(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
    procedure TreeListFiltersDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure TreeListFiltersMoveTo(Sender: TcxCustomTreeList; AttachNode: TcxTreeListNode; AttachMode: TcxTreeListNodeAttachMode; Nodes: TList; var IsCopy, Done: Boolean);
    procedure TreeListFiltersDeletion(Sender: TcxCustomTreeList; ANode: TcxTreeListNode);
    procedure TreeListFiltersNodeChanged(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AColumn: TcxTreeListColumn);
    procedure TreeListFiltersNodeCheckChanged(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AState: TcxCheckBoxState);
    procedure TreeListFiltersKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeListFiltersInitEditValue(Sender, AItem: TObject; AEdit: TcxCustomEdit; var AValue: Variant);
    procedure TreeListFiltersColumnGroupGetEditingProperties(Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var EditProperties: TcxCustomEditProperties);
    procedure TreeListFiltersColumnGroupGetDisplayText(Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var Value: string);
    procedure FormShow(Sender: TObject);
    procedure TreeListFiltersColumnGroupPropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure TreeListFiltersColumnValuePropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
  private
    FFilters: TFilterItemList;
    FUpdatingNode: boolean;
  protected
    procedure LoadFilters;
    procedure SaveFilters;
    function FindGroup(const Name: string): TcxTreeListNode;
    function AddGroup(const Name: string): TcxTreeListNode;
    function LoadFilter(Filter: TFilterItem): TcxTreeListNode;
    function DoExecute: boolean;
    procedure SaveLayout;
    procedure RestoreLayout;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute: boolean;
    function AddFilter(const AGroup: string; AField: TFilterField; AOperator: TFilterOperator; const AValue: string): boolean;

    procedure BeginAddFilter;
    function ContinueAddFilter(const AGroup: string; AField: TFilterField; AOperator: TFilterOperator; const AValue: string): TFilterItem;
    function EndAddFilter: boolean;
  end;

implementation

{$R *.dfm}

uses
  UITypes,
  RegularExpressions,
  amLocalization.Settings;

constructor TFormFilters.Create(AOwner: TComponent);
begin
  inherited;

  FFilters := TFilterItemList.Create;

  TreeListFilters.Root.CheckGroupType := ncgCheckGroup;
end;

destructor TFormFilters.Destroy;
begin
  FreeAndNil(FFilters);

  inherited;
end;

procedure TFormFilters.RestoreLayout;
var
  i: integer;
  Group: string;
begin
  if (TranslationManagerSettings.Layout.BlackList.Valid) then
    TreeListFilters.RestoreFromRegistry(TranslationManagerSettings.Layout.KeyPath, False, False, TranslationManagerSettings.Layout.BlackList.Name);

  for i := 0 to TreeListFilters.Root.Count-1 do
  begin
    Group := VarToStr(TreeListFilters.Root.Items[i].Values[TreeListFiltersColumnGroup.ItemIndex]);

    if (TranslationManagerSettings.Filters.Expanded[Group]) then
      TreeListFilters.Root.Items[i].Expand(False)
    else
      TreeListFilters.Root.Items[i].Collapse(False);
  end;
end;

procedure TFormFilters.SaveLayout;
var
  i: integer;
  Group: string;
begin
  TreeListFilters.StoreToRegistry(TranslationManagerSettings.Layout.KeyPath, False, TranslationManagerSettings.Layout.BlackList.Name);
  TranslationManagerSettings.Layout.BlackList.Valid := True;

  for i := 0 to TreeListFilters.Root.Count-1 do
  begin
    Group := VarToStr(TreeListFilters.Root.Items[i].Values[TreeListFiltersColumnGroup.ItemIndex]);
    TranslationManagerSettings.Filters.Expanded[Group] := TreeListFilters.Root.Items[i].Expanded;
  end;
end;

function TFormFilters.DoExecute: boolean;
begin
  RestoreLayout;

  Result := (ShowModal = mrOK);

  SaveLayout;
end;

function TFormFilters.AddFilter(const AGroup: string; AField: TFilterField; AOperator: TFilterOperator; const AValue: string): boolean;
begin
  BeginAddFilter;

  ContinueAddFilter(AGroup, AField, AOperator, AValue);

  Result := EndAddFilter;
end;

procedure TFormFilters.BeginAddFilter;
begin
  LoadFilters;
end;

function TFormFilters.EndAddFilter: boolean;
begin
  Result := DoExecute;

  if (Result) then
    SaveFilters;
end;

function TFormFilters.Execute: boolean;
begin
  LoadFilters;

  Result := DoExecute;

  if (Result) then
    SaveFilters;
end;

procedure TFormFilters.FormShow(Sender: TObject);
begin
  if (TreeListFilters.FocusedNode <> nil) then
    TreeListFilters.FocusedNode.MakeVisible;
end;

procedure TFormFilters.LoadFilters;
var
  Filter: TFilterItem;
begin
  TreeListFilters.BeginUpdate;
  try
    TreeListFilters.Clear;
    FFilters.Assign(TranslationManagerSettings.Filters.Filters);

    for Filter in FFilters do
      LoadFilter(Filter);
  finally
    TreeListFilters.EndUpdate;
  end;
end;

procedure TFormFilters.SaveFilters;
var
  i: integer;
begin
  // Disable filters that would match everything
  for i := 0 to FFilters.Count-1 do
  begin
    if (FFilters[i].FilterOperator <> foEquals) and (FFilters[i].Value = '') then
      FFilters[i].Enabled := False;
  end;

  TranslationManagerSettings.Filters.Filters.Assign(FFilters);
end;

function TFormFilters.LoadFilter(Filter: TFilterItem): TcxTreeListNode;
var
  GroupNode: TcxTreeListNode;
begin
  // Find group node
  GroupNode := AddGroup(Filter.Group);

  // Add filter node
  Result := GroupNode.AddChild;
  Result.Data := Filter;
  Result.CheckGroupType := ncgCheckGroup;
  Result.Checked := Filter.Enabled;
  Result.Values[TreeListFiltersColumnGroup.ItemIndex] := Filter.Group;
  Result.Values[TreeListFiltersColumnField.ItemIndex] := Ord(Filter.Field);
  Result.Values[TreeListFiltersColumnOperator.ItemIndex] := Ord(Filter.FilterOperator);
  Result.Values[TreeListFiltersColumnValue.ItemIndex] := Filter.Value;
end;

function TFormFilters.ContinueAddFilter(const AGroup: string; AField: TFilterField; AOperator: TFilterOperator; const AValue: string): TFilterItem;
var
  Node: TcxTreeListNode;
begin
  // Add filter
  Result := TFilterItem.Create;
  FFilters.Add(Result);

  Result.Group := AGroup;
  Result.Field := AField;
  Result.FilterOperator := AOperator;
  Result.Value := AValue;
  Result.Enabled := True;

  // Add filter node
  Node := LoadFilter(Result);
  TreeListFilters.FocusedNode := Node;
end;

function TFormFilters.FindGroup(const Name: string): TcxTreeListNode;
var
  i: integer;
  Group: string;
begin
  Group := Name;
  if (AnsiSameText(Group, sFilterGroupGeneralDisplay)) then
    Group := '';

  // Find group node
  Result := nil;
  for i := 0 to TreeListFilters.Root.Count-1 do
    if (AnsiSameText(Group, TreeListFilters.Root.Items[i].Values[TreeListFiltersColumnGroup.ItemIndex])) then
    begin
      Result := TreeListFilters.Root.Items[i];
      break;
    end;
end;

function TFormFilters.AddGroup(const Name: string): TcxTreeListNode;
var
  Group: string;
begin
  Group := Name;
  if (AnsiSameText(Group, sFilterGroupGeneralDisplay)) then
    Group := '';

  // Find group node
  Result := FindGroup(Group);

  if (Result = nil) then
  begin
    Result := TreeListFilters.Add;
    Result.CheckGroupType := ncgCheckGroup;
    Result.Checked := True;
    Result.Values[TreeListFiltersColumnGroup.ItemIndex] := Group;
  end;
end;

procedure TFormFilters.TreeListFiltersBeginDragNode(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
begin
  Allow := (ANode.Level > 0);
end;

procedure TFormFilters.TreeListFiltersColumnGroupGetDisplayText(Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var Value: string);
begin
  if (ANode.Level = 0) then
  begin
    if (Value = '') then
      Value := sFilterGroupGeneralDisplay;
  end;
end;

procedure TFormFilters.TreeListFiltersColumnGroupGetEditingProperties(Sender: TcxTreeListColumn; ANode: TcxTreeListNode;
  var EditProperties: TcxCustomEditProperties);
var
  i: integer;
  s: string;
begin
  if (ANode.Level = 0) then
  begin
    EditProperties := EditRepositoryTextItem.Properties;
    Exit;
  end;

  TcxComboBoxProperties(EditProperties).Items.Clear;
  for i := 0 to TreeListFilters.Root.Count-1 do
  begin
    s := VarToStr(TreeListFilters.Root.Items[i].Values[TreeListFiltersColumnGroup.ItemIndex]);
    if (TcxComboBoxProperties(EditProperties).Items.IndexOf(s) = -1) then
      TcxComboBoxProperties(EditProperties).Items.Add(s);
  end;
end;

procedure TFormFilters.TreeListFiltersColumnGroupPropertiesValidate(Sender: TObject; var DisplayValue: Variant;
  var ErrorText: TCaption; var Error: Boolean);
var
  Group: string;
  GroupNode: TcxTreeListNode;
resourcestring
  sFilterDuplicateGroup = 'Duplicate group names not allowed';
begin
  if (TreeListFilters.FocusedNode.Level <> 0) then
    Exit;

  Group := VarToStr(DisplayValue);

  // Disallow duplicate group values
  GroupNode := FindGroup(Group);
  if (GroupNode <> nil) and (GroupNode <> TreeListFilters.FocusedNode) then
  begin
    Error := True;
    ErrorText := sFilterDuplicateGroup;
    MessageDlg(sFilterDuplicateGroup, mtWarning, [mbOK], 0);
  end;
end;

procedure TFormFilters.TreeListFiltersDeletion(Sender: TcxCustomTreeList; ANode: TcxTreeListNode);
begin
  if (FFilters <> nil) and (ANode.Data <> nil) then
    FFilters.Remove(TFilterItem(ANode.Data));
end;

procedure TFormFilters.TreeListFiltersDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  TargetNode: TcxTreeListNode;
begin
  TargetNode := TreeListFilters.GetNodeAt(X, Y);
  Accept := (TargetNode <> nil) and
    (((TargetNode.Level = 0) and (TargetNode <> TreeListFilters.FocusedNode.Parent)) or
     ((TargetNode.Level = 1) and (TargetNode.Parent <> TreeListFilters.FocusedNode.Parent)));
end;

procedure TFormFilters.TreeListFiltersInitEditValue(Sender, AItem: TObject; AEdit: TcxCustomEdit; var AValue: Variant);
begin
  if (AItem = TreeListFiltersColumnGroup) and (AnsiSameText(VarToStr(AValue), sFilterGroupGeneralDisplay)) then
    AValue := '';
end;

procedure TFormFilters.TreeListFiltersKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Group: string;
begin
  if (Key = VK_INSERT) and (Shift = []) then
  begin
    if (TreeListFilters.FocusedNode <> nil) then
      Group := TreeListFilters.FocusedNode.Values[TreeListFiltersColumnGroup.ItemIndex]
    else
      Group := '';

    ContinueAddFilter(Group, ffType, foEquals, '');
    Key := 0;
  end;
end;

procedure TFormFilters.TreeListFiltersMoveTo(Sender: TcxCustomTreeList; AttachNode: TcxTreeListNode; AttachMode: TcxTreeListNodeAttachMode; Nodes: TList; var IsCopy, Done: Boolean);
var
  SourceNode, TargetNode: TcxTreeListNode;
  Group: string;
  i: integer;
begin
  TargetNode := AttachNode;

  if (TargetNode = nil) then
    Exit;

  if (TargetNode.Level > 0) then
    TargetNode := TargetNode.Parent;

  Group := TargetNode.Values[TreeListFiltersColumnGroup.ItemIndex];

  for i := 0 to Nodes.Count-1 do
  begin
    SourceNode := TcxTreeListNode(Nodes[i]);

    if (TargetNode = SourceNode.Parent) then
      Exit;

    SourceNode.MoveTo(TargetNode, tlamAddChild);

    TFilterItem(SourceNode.Data).Group := Group;
    SourceNode.Values[TreeListFiltersColumnGroup.ItemIndex] := Group;
  end;

  Done := True;
  IsCopy := False;
end;

procedure TFormFilters.TreeListFiltersNodeChanged(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AColumn: TcxTreeListColumn);
var
  GroupNode, Node, NextNode: TcxTreeListNode;
  Group: string;
begin
  if (FUpdatingNode) then
    Exit;

  if (AColumn = TreeListFiltersColumnField) then
    TFilterItem(ANode.Data).Field := TFilterField(ANode.Values[AColumn.ItemIndex])
  else
  if (AColumn = TreeListFiltersColumnOperator) then
    TFilterItem(ANode.Data).FilterOperator := TFilterOperator(ANode.Values[AColumn.ItemIndex])
  else
  if (AColumn = TreeListFiltersColumnValue) then
    TFilterItem(ANode.Data).Value := VarToStr(ANode.Values[AColumn.ItemIndex])
  else
  if (AColumn = TreeListFiltersColumnGroup) then
  begin
    Group := VarToStr(ANode.Values[TreeListFiltersColumnGroup.ItemIndex]);
    if (AnsiSameText(Group, sFilterGroupGeneralDisplay)) then
      Group := '';

    FUpdatingNode := True;
    try
      if (ANode.Level = 0) then
      begin
        // Note: Check for duplicate group has already been done in OnValidate handler.
        GroupNode := ANode;

        // Update all child nodes with the new group name value
        // Note: We cannot iterate using the Items[] array as the order of items can
        // change when we modify their values due to sorting.
        Node := GroupNode.getFirstChild;
        while (Node <> nil) do
        begin
          NextNode := Node.getNextSibling;

          // Note: Modifying the node value recursively calls NodeChanged event
          Node.Values[TreeListFiltersColumnGroup.ItemIndex] := Group;
          TFilterItem(Node.Data).Group := Group;

          Node := NextNode;
        end;
      end else
      begin
        // Modifying a child node group moves it to the specified group
        GroupNode := AddGroup(Group);
        if (ANode.Parent <> GroupNode) then
          ANode.MoveTo(GroupNode, tlamAddChild);

        TFilterItem(ANode.Data).Group := Group;

        if (ANode.Values[TreeListFiltersColumnGroup.ItemIndex] <> Group) then
          // Note: Modifying the node value recursively calls NodeChanged event
          ANode.Values[TreeListFiltersColumnGroup.ItemIndex] := Group;

        ANode.MakeVisible;
        ANode.Focused := True;
      end;
    finally
      FUpdatingNode := False;
    end;
  end;
end;

procedure TFormFilters.TreeListFiltersNodeCheckChanged(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AState: TcxCheckBoxState);
begin
  if (ANode.Level <> 1) then
    Exit;

  TFilterItem(ANode.Data).Enabled := (AState = cbsChecked);
end;

procedure TFormFilters.TreeListFiltersColumnValuePropertiesValidate(Sender: TObject; var DisplayValue: Variant;
  var ErrorText: TCaption; var Error: Boolean);
var
  s: string;
begin
  if (TFilterOperator(TreeListFilters.FocusedNode.Values[TreeListFiltersColumnOperator.ItemIndex]) = foRegExp) then
  begin
    try
      // Verify that RegEx is valid
      s := VarToStr(DisplayValue);
      if (s <> '') then
        TRegEx.Create(s, [roCompiled])
      else
        // Disable filter if criteria is empty - This is also done when filters are saved
        TreeListFilters.FocusedNode.Checked := False;

    except
      on E: Exception do
      begin
        Error := True;
        MessageDlg(E.Message, mtWarning, [mbOK], 0);
      end;
    end;
  end;
end;

end.
