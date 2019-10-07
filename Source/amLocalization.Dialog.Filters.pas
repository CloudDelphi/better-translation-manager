﻿unit amLocalization.Dialog.Filters;

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
  cxImageComboBox,

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
    procedure TreeListFiltersBeginDragNode(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
    procedure TreeListFiltersDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure TreeListFiltersMoveTo(Sender: TcxCustomTreeList; AttachNode: TcxTreeListNode; AttachMode: TcxTreeListNodeAttachMode;
      Nodes: TList; var IsCopy, Done: Boolean);
    procedure TreeListFiltersDeletion(Sender: TcxCustomTreeList; ANode: TcxTreeListNode);
    procedure TreeListFiltersNodeChanged(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AColumn: TcxTreeListColumn);
    procedure TreeListFiltersNodeCheckChanged(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AState: TcxCheckBoxState);
    procedure TreeListFiltersKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FFilters: TFilterItemList;
  protected
    procedure LoadFilters;
    procedure SaveFilters;
    function AddGroup(const Name: string): TcxTreeListNode;
    function LoadFilter(Filter: TFilterItem): TcxTreeListNode;
    function AddFilter(AGroup: string; AField: TFilterField; AOperator: TFilterOperator; const AValue: string): TFilterItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute: boolean; overload;
    function Execute(AGroup: string; AField: TFilterField; AOperator: TFilterOperator; const AValue: string): boolean; overload;
  end;

implementation

{$R *.dfm}

uses
  amLocalization.Settings;

resourcestring
  sFilterGroupGeneral = 'General';

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

function TFormFilters.Execute(AGroup: string; AField: TFilterField; AOperator: TFilterOperator; const AValue: string): boolean;
begin
  LoadFilters;

  AddFilter(AGroup, AField, AOperator, AValue);

  Result := (ShowModal = mrOK);

  if (Result) then
    SaveFilters;
end;

function TFormFilters.Execute: boolean;
begin
  LoadFilters;

  Result := (ShowModal = mrOK);

  if (Result) then
    SaveFilters;
end;

procedure TFormFilters.LoadFilters;
var
  Filter: TFilterItem;
begin
  TreeListFilters.Clear;
  FFilters.Assign(TranslationManagerSettings.Filters.Filters);

  for Filter in FFilters do
    LoadFilter(Filter);

  if (FFilters.Count = 0) then
    AddFilter('DevExpress', ffType, foEquals, 'TdxLayoutEmptySpaceItem');

  TreeListFilters.FullExpand;
end;

procedure TFormFilters.SaveFilters;
begin
  TranslationManagerSettings.Filters.Filters.Assign(FFilters);
end;

function TFormFilters.LoadFilter(Filter: TFilterItem): TcxTreeListNode;
var
  Group: string;
  GroupNode: TcxTreeListNode;
begin
  Group := Filter.Group;
  if (Group = '') then
    Group := sFilterGroupGeneral;

  // Find group node
  GroupNode := AddGroup(Group);

  // Add filter node
  Result := GroupNode.AddChild;
  Result.Data := Filter;
  Result.CheckGroupType := ncgCheckGroup;
  Result.Checked := Filter.Enabled;
  Result.Values[TreeListFiltersColumnGroup.ItemIndex] := Group;
  Result.Values[TreeListFiltersColumnField.ItemIndex] := Ord(Filter.Field);
  Result.Values[TreeListFiltersColumnOperator.ItemIndex] := Ord(Filter.FilterOperator);
  Result.Values[TreeListFiltersColumnValue.ItemIndex] := Filter.Value;
end;

function TFormFilters.AddFilter(AGroup: string; AField: TFilterField; AOperator: TFilterOperator; const AValue: string): TFilterItem;
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
  Node.MakeVisible;
  Node.Focused := True;
end;

function TFormFilters.AddGroup(const Name: string): TcxTreeListNode;
var
  i: integer;
begin
  // Find group node
  Result := nil;
  for i := 0 to TreeListFilters.Root.Count-1 do
    if (AnsiSameText(Name, TreeListFilters.Root.Items[i].Values[TreeListFiltersColumnGroup.ItemIndex])) then
    begin
      Result := TreeListFilters.Root.Items[i];
      break;
    end;

  if (Result = nil) then
  begin
    Result := TreeListFilters.Add;
    Result.CheckGroupType := ncgCheckGroup;
    Result.Checked := True;
    Result.Values[TreeListFiltersColumnGroup.ItemIndex] := Name;
  end;
end;

procedure TFormFilters.TreeListFiltersBeginDragNode(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
begin
  Allow := (ANode.Level > 0);
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

procedure TFormFilters.TreeListFiltersKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Group: string;
begin
  if (Key = VK_INSERT) then
  begin
    if (TreeListFilters.FocusedNode <> nil) then
      Group := TreeListFilters.FocusedNode.Values[TreeListFiltersColumnGroup.ItemIndex]
    else
      Group := '';
    AddFilter(Group, ffType, foEquals, '');
    Key := 0;
  end;
end;

procedure TFormFilters.TreeListFiltersMoveTo(Sender: TcxCustomTreeList; AttachNode: TcxTreeListNode; AttachMode: TcxTreeListNodeAttachMode; Nodes: TList; var IsCopy, Done: Boolean);
var
  SourceNode, TargetNode: TcxTreeListNode;
begin
  TargetNode := AttachNode;

  if (TargetNode = nil) then
    Exit;

  if (TargetNode.Level > 0) then
    TargetNode := TargetNode.Parent;

  SourceNode := TreeListFilters.FocusedNode;

  if (TargetNode = SourceNode.Parent) then
    Exit;

  if (TargetNode <> AttachNode) then
  begin
    SourceNode.MoveTo(TargetNode, tlamAddChild);
    Done := True;
  end;

  TFilterItem(SourceNode.Data).Group := TargetNode.Values[TreeListFiltersColumnGroup.ItemIndex];
  SourceNode.Values[TreeListFiltersColumnGroup.ItemIndex] := TFilterItem(SourceNode.Data).Group;

  IsCopy := False;
end;

procedure TFormFilters.TreeListFiltersNodeChanged(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AColumn: TcxTreeListColumn);
var
  i: integer;
  GroupNode, Node: TcxTreeListNode;
  Group: string;
begin
  if (ANode.Level <> 1) then
    Exit;

  if (AColumn = TreeListFiltersColumnField) then
    TFilterItem(ANode.Data).Field := TFilterField(ANode.Values[AColumn.ItemIndex])
  else
  if (AColumn = TreeListFiltersColumnOperator) then
    TFilterItem(ANode.Data).FilterOperator := TFilterOperator(ANode.Values[AColumn.ItemIndex])
  else
  if (AColumn = TreeListFiltersColumnValue) then
    TFilterItem(ANode.Data).Value := ANode.Values[AColumn.ItemIndex]
  else
  if (AColumn = TreeListFiltersColumnGroup) then
  begin
    Group := VarToStr(ANode.Values[TreeListFiltersColumnGroup.ItemIndex]);
    if (Group = '') then
    begin
      Group := sFilterGroupGeneral;
      ANode.Values[TreeListFiltersColumnGroup.ItemIndex] := Group;
    end;

    if (ANode.Level = 0) then
    begin
      // TODO : Check for duplicate group
      GroupNode := ANode;
      // Modifying the group node also modifies all child nodes
      for i := 0 to Node.Count-1 do
      begin
        Node := GroupNode.Items[i];
        Node.Values[TreeListFiltersColumnGroup.ItemIndex] := Group;

        TFilterItem(Node.Data).Group := Group;
      end;
    end else
    begin
      // Modifying a child node moves it to the specified group
      GroupNode := AddGroup(Group);
      if (ANode.Parent <> GroupNode) then
        ANode.MoveTo(GroupNode, tlamAddChild);

      TFilterItem(ANode.Data).Group := Group;

      if (ANode.Values[TreeListFiltersColumnGroup.ItemIndex] <> Group) then
        ANode.Values[TreeListFiltersColumnGroup.ItemIndex] := Group;

      ANode.MakeVisible;
      ANode.Focused := True;
    end;
  end;
end;

procedure TFormFilters.TreeListFiltersNodeCheckChanged(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AState: TcxCheckBoxState);
begin
  if (ANode.Level <> 1) then
    Exit;

  TFilterItem(ANode.Data).Enabled := (AState = cbsChecked);
end;

end.