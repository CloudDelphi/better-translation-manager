unit amLocalization.Dialog.Languages;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  System.Generics.Collections,
  System.Generics.Defaults,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.StdCtrls, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore, cxClasses,
  dxLayoutContainer, dxLayoutControl, cxContainer, cxEdit, dxLayoutcxEditAdapters,
  dxLayoutControlAdapters, cxCheckBox, cxCustomListBox, cxCheckListBox, cxGroupBox, dxCheckGroupBox, cxButtons,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxLookupEdit, cxDBLookupEdit, cxDBExtLookupComboBox, cxLabel,

  VirtualTrees,

  amLocale,
  amLocalization.Dialog;

type
  TFormLanguages = class(TFormDialog)
    ComboBoxSourceLanguage: TcxExtLookupComboBox;
    LayoutItemSourceLanguage: TdxLayoutItem;
    dxLayoutGroup1: TdxLayoutGroup;
    LayoutItemTargetLanguage: TdxLayoutItem;
    CheckBoxApplyFilter: TcxCheckBox;
    dxLayoutItem5: TdxLayoutItem;
    ActionList1: TActionList;
    ActionApplyFilter: TAction;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    procedure ActionApplyFilterExecute(Sender: TObject);
    procedure ActionApplyFilterUpdate(Sender: TObject);
  private
    FTreeView: TVirtualStringTree;
    FNodes: TList<PVirtualNode>;
    procedure TreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeViewIncrementalSearch(Sender: TBaseVirtualTree; Node: PVirtualNode; const SearchText: string; var Result: Integer);
  protected
    function GetApplyFilter: boolean;
    procedure SetApplyFilter(const Value: boolean);
    function GetSourceLanguageID: LCID;
    procedure SetSourceLanguageID(const Value: LCID);
    function GetTargetLanguageCount: integer;
    function GetTargetLanguage(Index: integer): LCID;

    procedure LoadLanguages;
    function NodeToLanguage(Node: PVirtualNode): LCID;
    function NodeToLocaleItem(Node: PVirtualNode): TLocaleItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute: boolean;

    function SelectTargetLanguage(LanguageID: LCID): boolean;
    procedure ClearTargetLanguages;

    property ApplyFilter: boolean read GetApplyFilter write SetApplyFilter;
    property SourceLanguageID: LCID read GetSourceLanguageID write SetSourceLanguageID;
    property TargetLanguageCount: integer read GetTargetLanguageCount;
    property TargetLanguage[Index: integer]: LCID read GetTargetLanguage;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  Math,
  amLocalization.Data.Main;


//------------------------------------------------------------------------------

function ExtractLanguage(const Value: string): string;
var
  n: integer;
begin
  Result := Value;
  n := Pos('(', Result);
  if (n <> 0) then
  begin
    if (n > 1) and (Result[n-1] = ' ') then
      Dec(n);
    SetLength(Result, n-1);
  end;
end;

//------------------------------------------------------------------------------

constructor TFormLanguages.Create(AOwner: TComponent);
begin
  inherited;

  FNodes := TList<PVirtualNode>.Create;

  FTreeView := TVirtualStringTree.Create(Self);
  FTreeView.OnGetText := TreeViewGetText;
  FTreeView.OnIncrementalSearch := TreeViewIncrementalSearch;
  FTreeView.TreeOptions.MiscOptions := FTreeView.TreeOptions.MiscOptions + [toCheckSupport];
  FTreeView.IncrementalSearch := isAll;
  FTreeView.CheckImageKind := ckSystemDefault;

  LayoutItemTargetLanguage.Control := FTreeView;

  LoadLanguages;
end;

destructor TFormLanguages.Destroy;
begin
  FNodes.Free;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TFormLanguages.TreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  LocaleItem: TLocaleItem;
begin
  if (Node = nil) or (Node = FTreeView.RootNode) then
    Exit;

  LocaleItem := NodeToLocaleItem(Node);

  if (Node.Parent = FTreeView.RootNode) and (Node.ChildCount > 0) then
    CellText := Format('%s - %s', [ExtractLanguage(LocaleItem.DisplayName), LocaleItem.ISO639_1Name.ToUpper])
  else
    CellText := Format('%s - %s', [LocaleItem.LanguageName, LocaleItem.LanguageShortName]);
//    CellText := Format('%s - %s', [LocaleItem.LanguageName, TLocaleItem.GetLocaleData(MAKELCID(MakeLangID(LocaleItem.PrimaryLanguage, SUBLANG_NEUTRAL), SORT_DEFAULT), LOCALE_SLOCALIZEDLANGUAGENAME)]);
end;

procedure TFormLanguages.TreeViewIncrementalSearch(Sender: TBaseVirtualTree; Node: PVirtualNode; const SearchText: string; var Result: Integer);
begin
  var Text: string;
  TreeViewGetText(Sender, Node, 0, ttNormal, Text);
  if (Text.ToLower.Contains(SearchText.ToLower)) then
    Result := 0
  else
    Result := 1;
end;

//------------------------------------------------------------------------------

function TFormLanguages.NodeToLocaleItem(Node: PVirtualNode): TLocaleItem;
begin
  Result := TLocaleItem(FTreeView.GetNodeData(Node)^);
end;

function TFormLanguages.NodeToLanguage(Node: PVirtualNode): LCID;
var
  Language: Word;
begin
  if (Node.Parent = FTreeView.RootNode) and (Node.ChildCount > 0) then
  begin
    Language := NodeToLocaleItem(Node).PrimaryLanguage;
    Result := MAKELCID(MakeLangID(Language, SUBLANG_NEUTRAL), SORT_DEFAULT);
  end else
    Result := NodeToLocaleItem(Node).Locale;
end;

//------------------------------------------------------------------------------

procedure TFormLanguages.LoadLanguages;
var
  i: integer;
  LanguageNode, LocaleNode: PVirtualNode;
  SortedLanguages: TList<TLocaleItem>;
  Languages: TDictionary<string, PVirtualNode>;
  LocaleItem: TLocaleItem;
  LanguageName: string;
begin
  SortedLanguages := TList<TLocaleItem>.Create;
  try
    for i := 0 to TLocaleItems.Count-1 do
      SortedLanguages.Add(TLocaleItems.Items[i]);

    SortedLanguages.Sort(TComparer<TLocaleItem>.Construct(
      function(const Left, Right: TLocaleItem): Integer
      begin
        Result := CompareText(ExtractLanguage(Left.DisplayName), ExtractLanguage(Right.DisplayName));
        if (Result = 0) then
          Result := CompareText(Left.LanguageName, Right.LanguageName);
      end));

    Languages := TDictionary<string, PVirtualNode>.Create;
    try

      for LocaleItem in SortedLanguages do
      begin
        LanguageName := ExtractLanguage(LocaleItem.DisplayName);

        if (not Languages.TryGetValue(LanguageName, LanguageNode)) then
        begin
          LanguageNode := FTreeView.AddChild(nil, LocaleItem);
          FNodes.Add(LanguageNode);
          LanguageNode.CheckType := ctCheckBox;
          Languages.Add(LanguageName, LanguageNode);
        end else
        begin
          if (LanguageNode.ChildCount = 0) then
          begin
            LocaleNode := FTreeView.AddChild(LanguageNode, NodeToLocaleItem(LanguageNode));
            FNodes.Add(LocaleNode);
            LocaleNode.CheckType := ctCheckBox;
          end;

          LocaleNode := FTreeView.AddChild(LanguageNode, LocaleItem);
          FNodes.Add(LocaleNode);
          LocaleNode.CheckType := ctCheckBox;
        end;
      end;

    finally
      Languages.Free;
    end;

  finally
    SortedLanguages.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFormLanguages.Execute: boolean;
begin
  Result := (ShowModal = mrOK)
end;

//------------------------------------------------------------------------------

procedure TFormLanguages.ActionApplyFilterExecute(Sender: TObject);
begin
  CheckBoxApplyFilter.Checked := TAction(Sender).Checked;
end;

procedure TFormLanguages.ActionApplyFilterUpdate(Sender: TObject);
var
  i: integer;
  SourceEqualsTarget: boolean;
  CheckedCount: integer;
begin
  SourceEqualsTarget := False;
  CheckedCount := 0;
  for i := 0 to FNodes.Count-1 do
    if (FNodes[i].CheckState = csCheckedNormal) then
    begin
      if (NodeToLanguage(FNodes[i]) = SourceLanguageID) then
        SourceEqualsTarget := True;
      Inc(CheckedCount);
    end;

  TAction(Sender).Enabled := (CheckedCount > 1) or ((CheckedCount = 1) and (not SourceEqualsTarget));
end;

//------------------------------------------------------------------------------

function TFormLanguages.GetApplyFilter: boolean;
begin
  Result := ActionApplyFilter.Checked;
end;

procedure TFormLanguages.SetApplyFilter(const Value: boolean);
begin
  ActionApplyFilter.Checked := Value;
end;

//------------------------------------------------------------------------------

procedure TFormLanguages.ClearTargetLanguages;
var
  i: integer;
begin
  for i := 0 to FNodes.Count-1 do
    FNodes[i].CheckState := csUncheckedNormal;
  FTreeView.Invalidate;
end;

function TFormLanguages.SelectTargetLanguage(LanguageID: LCID): boolean;
var
  i: integer;
begin
  Result := False;

  for i := 0 to FNodes.Count-1 do
    if (NodeToLanguage(FNodes[i]) = LanguageID) then
    begin
      FNodes[i].CheckState := csCheckedNormal;
      FTreeView.InvalidateNode(FNodes[i]);
      FTreeView.Expanded[FNodes[i].Parent] := True;
      Result := True;
      break;
    end;
end;

//------------------------------------------------------------------------------

function TFormLanguages.GetSourceLanguageID: LCID;
begin
  if (VarIsOrdinal(ComboBoxSourceLanguage.EditValue)) then
    Result := ComboBoxSourceLanguage.EditValue
  else
    Result := 0;
end;

procedure TFormLanguages.SetSourceLanguageID(const Value: LCID);
begin
  ComboBoxSourceLanguage.EditValue := Value;
end;

//------------------------------------------------------------------------------

function TFormLanguages.GetTargetLanguage(Index: integer): LCID;
var
  i: integer;
begin
  Result := 0;

  i := 0;
  while (i < FNodes.Count) and (Index >= 0) do
  begin
    if (FNodes[i].CheckState = csCheckedNormal) then
    begin
      if (Index = 0) then
        Exit(NodeToLanguage(FNodes[i]));
      Dec(Index);
    end;

    Inc(i);
  end;
end;

function TFormLanguages.GetTargetLanguageCount: integer;
var
  i: integer;
begin
  Result := 0;

  for i := 0 to FNodes.Count-1 do
    if (FNodes[i].CheckState = csCheckedNormal) then
      Inc(Result);
end;

end.
