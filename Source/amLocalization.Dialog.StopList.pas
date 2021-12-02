unit amLocalization.Dialog.StopList;

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
  amLocalization.StopList;

type
  TFormStopList = class(TFormDialog)
    TreeListStopList: TcxTreeList;
    LayoutItemFilters: TdxLayoutItem;
    TreeListStopListColumnField: TcxTreeListColumn;
    TreeListStopListColumnValue: TcxTreeListColumn;
    TreeListStopListColumnGroup: TcxTreeListColumn;
    TreeListStopListColumnOperator: TcxTreeListColumn;
    EditRepository: TcxEditRepository;
    EditRepositoryTextItem: TcxEditRepositoryTextItem;
    ButtonImport: TcxButton;
    LayoutItemButtonImport: TdxLayoutItem;
    ActionImport: TAction;
    TaskDialogImport: TTaskDialog;
    OpenDialogStopList: TOpenDialog;
    SaveDialogStopList: TSaveDialog;
    ButtonExport: TcxButton;
    LayoutItemButtonExport: TdxLayoutItem;
    ActionExport: TAction;
    procedure TreeListStopListBeginDragNode(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
    procedure TreeListStopListDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure TreeListStopListMoveTo(Sender: TcxCustomTreeList; AttachNode: TcxTreeListNode; AttachMode: TcxTreeListNodeAttachMode; Nodes: TList; var IsCopy, Done: Boolean);
    procedure TreeListStopListDeletion(Sender: TcxCustomTreeList; ANode: TcxTreeListNode);
    procedure TreeListStopListNodeChanged(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AColumn: TcxTreeListColumn);
    procedure TreeListStopListNodeCheckChanged(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AState: TcxCheckBoxState);
    procedure TreeListStopListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeListStopListInitEditValue(Sender, AItem: TObject; AEdit: TcxCustomEdit; var AValue: Variant);
    procedure TreeListStopListColumnGroupGetEditingProperties(Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var EditProperties: TcxCustomEditProperties);
    procedure TreeListStopListColumnGroupGetDisplayText(Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var Value: string);
    procedure FormShow(Sender: TObject);
    procedure TreeListStopListColumnGroupPropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure TreeListStopListColumnValuePropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure ActionImportExecute(Sender: TObject);
    procedure ActionExportExecute(Sender: TObject);
    procedure TreeListStopListColumnGroupPropertiesInitPopup(Sender: TObject);
  private
    FStopList: TStopListItemList;
    FStopListLocked: boolean;
    FUpdatingNode: boolean;
  protected
    procedure LoadStopList;
    procedure SaveStopList;
    procedure DoLoadStopList;
    function FindGroup(const Name: string): TcxTreeListNode;
    function AddGroup(const Name: string): TcxTreeListNode;
    function LoadStopListItem(StopListItem: TStopListItem): TcxTreeListNode;
    function DoExecute: boolean;
    procedure SaveLayout;
    procedure RestoreLayout;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute: boolean;
    function AddStopListItem(const AGroup: string; AField: TStopListField; AOperator: TStopListOperator; const AValue: string): boolean;

    procedure BeginAddStopList;
    function ContinueAddStopList(const AGroup: string; AField: TStopListField; AOperator: TStopListOperator; const AValue: string): TStopListItem;
    function EndAddStopList: boolean;
  end;

implementation

{$R *.dfm}

uses
  UITypes,
  IOUtils,
  RegularExpressions,
  amLocalization.Settings;

constructor TFormStopList.Create(AOwner: TComponent);
begin
  inherited;

  FStopList := TStopListItemList.Create;

  TreeListStopList.Root.CheckGroupType := ncgCheckGroup;
end;

destructor TFormStopList.Destroy;
begin
  FreeAndNil(FStopList);

  inherited;
end;

procedure TFormStopList.RestoreLayout;
var
  i: integer;
  Group: string;
begin
  if (TranslationManagerSettings.Layout.StopList.Valid) then
    TreeListStopList.RestoreFromRegistry(TranslationManagerSettings.Layout.KeyPath, False, False, TranslationManagerSettings.Layout.StopList.Name);

  for i := 0 to TreeListStopList.Root.Count-1 do
  begin
    Group := VarToStr(TreeListStopList.Root.Items[i].Values[TreeListStopListColumnGroup.ItemIndex]);

    if (TranslationManagerSettings.StopList.Expanded[Group]) then
      TreeListStopList.Root.Items[i].Expand(False)
    else
      TreeListStopList.Root.Items[i].Collapse(False);
  end;
end;

procedure TFormStopList.SaveLayout;
var
  i: integer;
  Group: string;
begin
  TreeListStopList.StoreToRegistry(TranslationManagerSettings.Layout.KeyPath, False, TranslationManagerSettings.Layout.StopList.Name);
  TranslationManagerSettings.Layout.StopList.Valid := True;

  for i := 0 to TreeListStopList.Root.Count-1 do
  begin
    Group := VarToStr(TreeListStopList.Root.Items[i].Values[TreeListStopListColumnGroup.ItemIndex]);
    TranslationManagerSettings.StopList.Expanded[Group] := TreeListStopList.Root.Items[i].Expanded;
  end;
end;

function TFormStopList.DoExecute: boolean;
begin
  RestoreLayout;

  Result := (ShowModal = mrOK);

  SaveLayout;
end;

function TFormStopList.AddStopListItem(const AGroup: string; AField: TStopListField; AOperator: TStopListOperator; const AValue: string): boolean;
begin
  BeginAddStopList;

  ContinueAddStopList(AGroup, AField, AOperator, AValue);

  Result := EndAddStopList;
end;

procedure TFormStopList.BeginAddStopList;
begin
  LoadStopList;
end;

function TFormStopList.EndAddStopList: boolean;
begin
  Result := DoExecute;

  if (Result) then
    SaveStopList;
end;

function TFormStopList.Execute: boolean;
begin
  LoadStopList;

  Result := DoExecute;

  if (Result) then
    SaveStopList;
end;

procedure TFormStopList.FormShow(Sender: TObject);
begin
  if (TreeListStopList.FocusedNode <> nil) then
    TreeListStopList.FocusedNode.MakeVisible;
end;

procedure TFormStopList.LoadStopList;
begin
  FStopListLocked := True;
  try
    FStopList.Assign(TranslationManagerSettings.StopList.StopList);

    DoLoadStopList;
  finally
    FStopListLocked := False;
  end;
end;

procedure TFormStopList.DoLoadStopList;
var
  StopListItem: TStopListItem;
begin
  TreeListStopList.BeginUpdate;
  try
    TreeListStopList.Clear;

    for StopListItem in FStopList do
      LoadStopListItem(StopListItem);
  finally
    TreeListStopList.EndUpdate;
  end;
end;

procedure TFormStopList.SaveStopList;
var
  i: integer;
begin
  // Disable items that would match everything
  for i := 0 to FStopList.Count-1 do
  begin
    if (FStopList[i].StopListOperator <> slOpEquals) and (FStopList[i].Value = '') then
      FStopList[i].Enabled := False;
  end;

  TranslationManagerSettings.StopList.StopList.Assign(FStopList);
end;

function TFormStopList.LoadStopListItem(StopListItem: TStopListItem): TcxTreeListNode;
var
  GroupNode: TcxTreeListNode;
begin
  // Find group node
  GroupNode := AddGroup(StopListItem.Group);

  // Add item node
  Result := GroupNode.AddChild;
  Result.Data := StopListItem;
  Result.CheckGroupType := ncgCheckGroup;
  Result.Checked := StopListItem.Enabled;
  Result.Values[TreeListStopListColumnGroup.ItemIndex] := StopListItem.Group;
  Result.Values[TreeListStopListColumnField.ItemIndex] := Ord(StopListItem.Field);
  Result.Values[TreeListStopListColumnOperator.ItemIndex] := Ord(StopListItem.StopListOperator);
  Result.Values[TreeListStopListColumnValue.ItemIndex] := StopListItem.Value;
end;

function TFormStopList.ContinueAddStopList(const AGroup: string; AField: TStopListField; AOperator: TStopListOperator; const AValue: string): TStopListItem;
var
  Node: TcxTreeListNode;
begin
  // Add item
  Result := TStopListItem.Create;
  FStopList.Add(Result);

  Result.Group := AGroup;
  Result.Field := AField;
  Result.StopListOperator := AOperator;
  Result.Value := AValue;
  Result.Enabled := True;

  // Add item node
  Node := LoadStopListItem(Result);
  TreeListStopList.FocusedNode := Node;
end;

function TFormStopList.FindGroup(const Name: string): TcxTreeListNode;
var
  i: integer;
  Group: string;
begin
  Group := Name;
  if (AnsiSameText(Group, sStopListGroupGeneralDisplay)) then
    Group := '';

  // Find group node
  Result := nil;
  for i := 0 to TreeListStopList.Root.Count-1 do
    if (AnsiSameText(Group, TreeListStopList.Root.Items[i].Values[TreeListStopListColumnGroup.ItemIndex])) then
    begin
      Result := TreeListStopList.Root.Items[i];
      break;
    end;
end;

procedure TFormStopList.ActionExportExecute(Sender: TObject);
begin
  if (SaveDialogStopList.InitialDir = '') then
    SaveDialogStopList.InitialDir := TranslationManagerSettings.Folders.FolderDocuments;

  if (not SaveDialogStopList.Execute(Handle)) then
    Exit;

  // Remember folder for next time
  SaveDialogStopList.InitialDir := TPath.GetDirectoryName(SaveDialogStopList.FileName);

  FStopList.SaveToFile(SaveDialogStopList.FileName);
end;

procedure TFormStopList.ActionImportExecute(Sender: TObject);
var
  Merge: boolean;
begin
  if (OpenDialogStopList.InitialDir = '') then
    OpenDialogStopList.InitialDir := TranslationManagerSettings.Folders.FolderDocuments;

  if (not OpenDialogStopList.Execute(Handle)) then
    Exit;

  // Remember folder for next time
  OpenDialogStopList.InitialDir := TPath.GetDirectoryName(OpenDialogStopList.FileName);

  if (TreeListStopList.Count > 0) then
  begin
    if (not TaskDialogImport.Execute) then
      Exit;

    if (TaskDialogImport.ModalResult = mrCancel) then
      Exit;

    Merge := (TaskDialogImport.ModalResult = 101);
  end else
    Merge := False;

  FStopListLocked := True;
  try
    FStopList.LoadFromFile(OpenDialogStopList.FileName, Merge);

    DoLoadStopList;
  finally
    FStopListLocked := False;
  end;
end;

function TFormStopList.AddGroup(const Name: string): TcxTreeListNode;
var
  Group: string;
begin
  Group := Name;
  if (AnsiSameText(Group, sStopListGroupGeneralDisplay)) then
    Group := '';

  // Find group node
  Result := FindGroup(Group);

  if (Result = nil) then
  begin
    Result := TreeListStopList.Add;
    Result.CheckGroupType := ncgCheckGroup;
    Result.Checked := True;
    Result.Values[TreeListStopListColumnGroup.ItemIndex] := Group;
  end;
end;

procedure TFormStopList.TreeListStopListBeginDragNode(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
begin
  Allow := (ANode.Level > 0);
end;

procedure TFormStopList.TreeListStopListColumnGroupGetDisplayText(Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var Value: string);
begin
  if (ANode.Level = 0) then
  begin
    if (Value = '') then
      Value := sStopListGroupGeneralDisplay;
  end;
end;

procedure TFormStopList.TreeListStopListColumnGroupGetEditingProperties(Sender: TcxTreeListColumn; ANode: TcxTreeListNode;
  var EditProperties: TcxCustomEditProperties);
begin
  if (ANode.Level = 0) then
  begin
    EditProperties := EditRepositoryTextItem.Properties;
    Exit;
  end;
end;

procedure TFormStopList.TreeListStopListColumnGroupPropertiesInitPopup(
  Sender: TObject);
var
  i: integer;
  s: string;
begin
  TcxComboBox(Sender).Properties.Items.BeginUpdate;
  try
    TcxComboBox(Sender).Properties.Items.Clear;
    for i := 0 to TreeListStopList.Root.Count-1 do
    begin
      s := VarToStr(TreeListStopList.Root.Items[i].Values[TreeListStopListColumnGroup.ItemIndex]);
      if (TcxComboBox(Sender).Properties.Items.IndexOf(s) = -1) then
        TcxComboBox(Sender).Properties.Items.Add(s);
    end;
  finally
    TcxComboBox(Sender).Properties.Items.EndUpdate;
  end;
end;

procedure TFormStopList.TreeListStopListColumnGroupPropertiesValidate(Sender: TObject; var DisplayValue: Variant;
  var ErrorText: TCaption; var Error: Boolean);
var
  Group: string;
  GroupNode: TcxTreeListNode;
resourcestring
  sStopListDuplicateGroup = 'Duplicate group names not allowed';
begin
  if (TreeListStopList.FocusedNode.Level <> 0) then
    Exit;

  Group := VarToStr(DisplayValue);

  // Disallow duplicate group values
  GroupNode := FindGroup(Group);
  if (GroupNode <> nil) and (GroupNode <> TreeListStopList.FocusedNode) then
  begin
    Error := True;
    ErrorText := sStopListDuplicateGroup;
    MessageDlg(sStopListDuplicateGroup, mtWarning, [mbOK], 0);
  end;
end;

procedure TFormStopList.TreeListStopListDeletion(Sender: TcxCustomTreeList; ANode: TcxTreeListNode);
begin
  if (not FStopListLocked) and (FStopList <> nil) and (ANode.Data <> nil) then
    FStopList.Remove(TStopListItem(ANode.Data));
end;

procedure TFormStopList.TreeListStopListDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  TargetNode: TcxTreeListNode;
begin
  TargetNode := TreeListStopList.GetNodeAt(X, Y);
  Accept := (TargetNode <> nil) and
    (((TargetNode.Level = 0) and (TargetNode <> TreeListStopList.FocusedNode.Parent)) or
     ((TargetNode.Level = 1) and (TargetNode.Parent <> TreeListStopList.FocusedNode.Parent)));
end;

procedure TFormStopList.TreeListStopListInitEditValue(Sender, AItem: TObject; AEdit: TcxCustomEdit; var AValue: Variant);
begin
  if (AItem = TreeListStopListColumnGroup) and (AnsiSameText(VarToStr(AValue), sStopListGroupGeneralDisplay)) then
    AValue := '';
end;

procedure TFormStopList.TreeListStopListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Group: string;
begin
  if (Key = VK_INSERT) and (Shift = []) then
  begin
    if (TreeListStopList.FocusedNode <> nil) then
      Group := TreeListStopList.FocusedNode.Values[TreeListStopListColumnGroup.ItemIndex]
    else
      Group := '';

    ContinueAddStopList(Group, slFieldType, slOpEquals, '');
    Key := 0;
  end;
end;

procedure TFormStopList.TreeListStopListMoveTo(Sender: TcxCustomTreeList; AttachNode: TcxTreeListNode; AttachMode: TcxTreeListNodeAttachMode; Nodes: TList; var IsCopy, Done: Boolean);
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

  Group := TargetNode.Values[TreeListStopListColumnGroup.ItemIndex];

  for i := 0 to Nodes.Count-1 do
  begin
    SourceNode := TcxTreeListNode(Nodes[i]);

    if (TargetNode = SourceNode.Parent) then
      Exit;

    SourceNode.MoveTo(TargetNode, tlamAddChild);

    TStopListItem(SourceNode.Data).Group := Group;
    SourceNode.Values[TreeListStopListColumnGroup.ItemIndex] := Group;
  end;

  Done := True;
  IsCopy := False;
end;

procedure TFormStopList.TreeListStopListNodeChanged(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AColumn: TcxTreeListColumn);
var
  GroupNode, Node, NextNode: TcxTreeListNode;
  Group: string;
begin
  if (FUpdatingNode) then
    Exit;

  if (AColumn = TreeListStopListColumnField) then
    TStopListItem(ANode.Data).Field := TStopListField(ANode.Values[AColumn.ItemIndex])
  else
  if (AColumn = TreeListStopListColumnOperator) then
    TStopListItem(ANode.Data).StopListOperator := TStopListOperator(ANode.Values[AColumn.ItemIndex])
  else
  if (AColumn = TreeListStopListColumnValue) then
    TStopListItem(ANode.Data).Value := VarToStr(ANode.Values[AColumn.ItemIndex])
  else
  if (AColumn = TreeListStopListColumnGroup) then
  begin
    Group := VarToStr(ANode.Values[TreeListStopListColumnGroup.ItemIndex]);
    if (AnsiSameText(Group, sStopListGroupGeneralDisplay)) then
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
          Node.Values[TreeListStopListColumnGroup.ItemIndex] := Group;
          TStopListItem(Node.Data).Group := Group;

          Node := NextNode;
        end;
      end else
      begin
        // Modifying a child node group moves it to the specified group
        GroupNode := AddGroup(Group);
        if (ANode.Parent <> GroupNode) then
          ANode.MoveTo(GroupNode, tlamAddChild);

        TStopListItem(ANode.Data).Group := Group;

        if (ANode.Values[TreeListStopListColumnGroup.ItemIndex] <> Group) then
          // Note: Modifying the node value recursively calls NodeChanged event
          ANode.Values[TreeListStopListColumnGroup.ItemIndex] := Group;

        ANode.MakeVisible;
        ANode.Focused := True;
      end;
    finally
      FUpdatingNode := False;
    end;
  end;
end;

procedure TFormStopList.TreeListStopListNodeCheckChanged(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AState: TcxCheckBoxState);
begin
  if (ANode.Level <> 1) then
    Exit;

  TStopListItem(ANode.Data).Enabled := (AState = cbsChecked);
end;

procedure TFormStopList.TreeListStopListColumnValuePropertiesValidate(Sender: TObject; var DisplayValue: Variant;
  var ErrorText: TCaption; var Error: Boolean);
var
  s: string;
begin
  if (TStopListOperator(TreeListStopList.FocusedNode.Values[TreeListStopListColumnOperator.ItemIndex]) = slOpRegExp) then
  begin
    try
      // Verify that RegEx is valid
      s := VarToStr(DisplayValue);
      if (s <> '') then
        TRegEx.Create(s, [roCompiled])
      else
        // Disable item if criteria is empty - This is also done when items are saved
        TreeListStopList.FocusedNode.Checked := False;

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
