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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore, cxClasses,
  dxLayoutContainer, dxLayoutControl, cxContainer, cxEdit, Vcl.Menus, dxLayoutcxEditAdapters,
  dxLayoutControlAdapters, cxCheckBox, cxCustomListBox, cxCheckListBox, cxGroupBox, dxCheckGroupBox, Vcl.StdCtrls, cxButtons,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxLookupEdit, cxDBLookupEdit, cxDBExtLookupComboBox, System.Actions, Vcl.ActnList,
  amLocalization.Dialog, Vcl.ExtCtrls, cxLabel;

type
  TFormLanguages = class(TFormDialog)
    ComboBoxSourceLanguage: TcxExtLookupComboBox;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    CheckListBoxLanguages: TcxCheckListBox;
    CheckBoxApplyFilter: TcxCheckBox;
    dxLayoutItem5: TdxLayoutItem;
    ActionList1: TActionList;
    ActionApplyFilter: TAction;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ActionApplyFilterExecute(Sender: TObject);
    procedure ActionApplyFilterUpdate(Sender: TObject);
  private
    function GetApplyFilter: boolean;
    procedure SetApplyFilter(const Value: boolean);
    function GetSourceLanguageID: LCID;
    procedure SetSourceLanguageID(const Value: LCID);
    function GetTargetLanguageCount: integer;
    function GetTargetLanguage(Index: integer): LCID;
  public
    function Execute: boolean;

    function SelectTargetLanguage(LanguageID: LCID): boolean;
    procedure ClearTargetLanguages;

    property ApplyFilter: boolean read GetApplyFilter write SetApplyFilter;
    property SourceLanguageID: LCID read GetSourceLanguageID write SetSourceLanguageID;
    property TargetLanguageCount: integer read GetTargetLanguageCount;
    property TargetLanguage[Index: integer]: LCID read GetTargetLanguage;
  end;

implementation

{$R *.dfm}

uses
  Math,
  amLocale,
  amLocalization.Data.Main;



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
  for i := 0 to CheckListBoxLanguages.Items.Count-1 do
    if (CheckListBoxLanguages.Items[i].Checked) then
    begin
      if (LCID(CheckListBoxLanguages.Items[i].Tag) = SourceLanguageID) then
        SourceEqualsTarget := True;
      Inc(CheckedCount);
    end;

  TAction(Sender).Enabled := (CheckedCount > 1) or ((CheckedCount = 1) and (not SourceEqualsTarget));
end;

procedure TFormLanguages.ClearTargetLanguages;
var
  i: integer;
begin
  for i := 0 to CheckListBoxLanguages.Items.Count-1 do
    CheckListBoxLanguages.Items[i].Checked := False;
end;

type
  TcxInnerCheckListBoxCracker = class(TcxCustomInnerCheckListBox);

function TFormLanguages.Execute: boolean;
var
  ItemHeight: integer;
begin
  ItemHeight := TcxInnerCheckListBoxCracker(CheckListBoxLanguages.InnerCheckListBox).GetStandardItemHeight;
  CheckListBoxLanguages.Perform(LB_SETITEMHEIGHT, 0, ItemHeight + 4);

  Result := (ShowModal = mrOK)
end;

procedure TFormLanguages.FormCreate(Sender: TObject);
var
  i: integer;
  Item: TcxCheckListBoxItem;
begin
  CheckListBoxLanguages.Items.BeginUpdate;
  try
    CheckListBoxLanguages.Items.Clear;

    for i := 0 to TLocaleItems.Count-1 do
    begin
      Item := CheckListBoxLanguages.Items.Add;
      Item.Checked := False;
      Item.Tag := TLocaleItems.Items[i].Locale;
      Item.Text := TLocaleItems.Items[i].LanguageName;
    end;
  finally
    CheckListBoxLanguages.Items.EndUpdate;
  end;
end;

procedure TFormLanguages.FormResize(Sender: TObject);
begin
  CheckListBoxLanguages.Columns := Max(1, CheckListBoxLanguages.Width div 200);
end;

function TFormLanguages.GetApplyFilter: boolean;
begin
  Result := ActionApplyFilter.Checked;
end;

function TFormLanguages.GetSourceLanguageID: LCID;
begin
  if (VarIsOrdinal(ComboBoxSourceLanguage.EditValue)) then
    Result := ComboBoxSourceLanguage.EditValue
  else
    Result := 0;
end;

function TFormLanguages.GetTargetLanguage(Index: integer): LCID;
var
  i: integer;
begin
  Result := 0;

  i := 0;
  while (i < CheckListBoxLanguages.Items.Count) and (Index >= 0) do
  begin
    if (CheckListBoxLanguages.Items[i].Checked) then
    begin
      if (Index = 0) then
        Exit(CheckListBoxLanguages.Items[i].Tag);
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

  for i := 0 to CheckListBoxLanguages.Items.Count-1 do
    if (CheckListBoxLanguages.Items[i].Checked) then
      Inc(Result);
end;

function TFormLanguages.SelectTargetLanguage(LanguageID: LCID): boolean;
var
  i: integer;
begin
  Result := False;

  for i := 0 to CheckListBoxLanguages.Items.Count-1 do
    if (LCID(CheckListBoxLanguages.Items[i].Tag) = LanguageID) then
    begin
      CheckListBoxLanguages.Items[i].Checked := True;
      Result := True;
      break;
    end;
end;

procedure TFormLanguages.SetApplyFilter(const Value: boolean);
begin
  ActionApplyFilter.Checked := Value;
end;

procedure TFormLanguages.SetSourceLanguageID(const Value: LCID);
begin
  ComboBoxSourceLanguage.EditValue := Value;
end;

end.
