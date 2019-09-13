unit amLocalization.Dialog.TranslationMemory.SelectDuplicate;

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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Menus, Vcl.StdCtrls, System.Actions,
  Vcl.ActnList,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore, cxContainer,
  cxEdit, dxLayoutcxEditAdapters, dxLayoutContainer, dxLayoutControlAdapters, cxButtons,
  cxListView, cxLabel, cxClasses, dxLayoutControl, cxCheckBox, cxTextEdit, cxMaskEdit, cxDropDownEdit,

  amLocalization.Dialog,
  amLocalization.Model, Vcl.ExtCtrls;

type
  TDuplicateAction = (daPrompt, daFirst, daFirstAll, daSkip, daSkipAll, daAbort);

  TFormSelectDuplicate = class(TFormDialog)
    LayoutItemSourceValue: TdxLayoutItem;
    LabelSourceValue: TLabel;
    ListViewDuplicates: TcxListView;
    LayoutItemTranslationList: TdxLayoutItem;
    CheckBoxAll: TcxCheckBox;
    dxLayoutItem5: TdxLayoutItem;
    ComboBoxAction: TcxComboBox;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutEmptySpaceItem3: TdxLayoutEmptySpaceItem;
    LayoutItemContext: TdxLayoutItem;
    LabelContext: TLabel;
    dxLayoutItem8: TdxLayoutItem;
    CheckBoxApplyToIdentical: TcxCheckBox;
    procedure ActionCancelExecute(Sender: TObject);
    procedure ActionOKExecute(Sender: TObject);
    procedure ActionOKUpdate(Sender: TObject);
    procedure ComboBoxActionPropertiesChange(Sender: TObject);
    procedure CheckBoxAllPropertiesChange(Sender: TObject);
    procedure ListViewDuplicatesDblClick(Sender: TObject);
  private
    FDuplicateAction: TDuplicateAction;
    function GetApplyToIdentical: boolean;
  public
    function SelectDuplicate(Prop: TLocalizerProperty; Duplicates: TStrings; var Value: string): boolean;
    property DuplicateAction: TDuplicateAction read FDuplicateAction write FDuplicateAction;
    property ApplyToIdentical: boolean read GetApplyToIdentical;
  end;

implementation

{$R *.dfm}

uses
  amLocalization.Data.Main;

const
  ActionMap: array[boolean, 0..2] of TDuplicateAction =
   ((daPrompt, daFirst, daSkip), (daPrompt, daFirstAll, daSkipAll));

procedure TFormSelectDuplicate.ActionCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormSelectDuplicate.ActionOKExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TFormSelectDuplicate.ActionOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FDuplicateAction <> daPrompt) or (ListViewDuplicates.Selected <> nil);
end;

procedure TFormSelectDuplicate.CheckBoxAllPropertiesChange(Sender: TObject);
begin
  FDuplicateAction := ActionMap[CheckBoxAll.Checked, ComboBoxAction.ItemIndex];
end;

procedure TFormSelectDuplicate.ComboBoxActionPropertiesChange(Sender: TObject);
begin
  FDuplicateAction := ActionMap[CheckBoxAll.Checked, ComboBoxAction.ItemIndex];

  ListViewDuplicates.Enabled := (FDuplicateAction = daPrompt);
  if (FDuplicateAction in [daFirst, daFirstAll]) then
    ListViewDuplicates.ItemIndex := 0;

  CheckBoxAll.Enabled := (FDuplicateAction <> daPrompt);
  CheckBoxAll.Checked := (FDuplicateAction in [daSkipAll, daFirstAll]);
  CheckBoxApplyToIdentical.Enabled := (FDuplicateAction = daPrompt);
end;

function TFormSelectDuplicate.GetApplyToIdentical: boolean;
begin
  Result := (FDuplicateAction = daPrompt) and (CheckBoxApplyToIdentical.Checked);
end;

procedure TFormSelectDuplicate.ListViewDuplicatesDblClick(Sender: TObject);
begin
  if (FDuplicateAction = daPrompt) and (ListViewDuplicates.Selected <> nil) then
    ActionOK.Execute;
end;

function TFormSelectDuplicate.SelectDuplicate(Prop: TLocalizerProperty; Duplicates: TStrings; var Value: string): boolean;
var
  s: string;
begin
  if (FDuplicateAction = daAbort) then
    Exit(False);

  if (not(FDuplicateAction in [daSkipAll, daFirstAll])) then
  begin
    LabelContext.Caption := Format('%s\%s\%s', [Prop.Item.Module.Name, Prop.Item.Name, Prop.Name]);
    LabelContext.Hint := LabelContext.Caption;
    LabelSourceValue.Caption := StringReplace(Prop.Value, #13, ' ', [rfReplaceAll]);
    LabelSourceValue.Hint := Prop.Value;
    CheckBoxAll.Checked := (FDuplicateAction in [daSkipAll, daFirstAll]);
    CheckBoxApplyToIdentical.Checked := True;
    case FDuplicateAction of
      daPrompt:
        ComboBoxAction.ItemIndex := 0;

      daFirst, daFirstAll:
        ComboBoxAction.ItemIndex := 1;

      daSkip, daSkipAll:
        ComboBoxAction.ItemIndex := 2;
    else
      Value := '';
    end;


    ListViewDuplicates.Items.Clear;
    for s in Duplicates do
      ListViewDuplicates.Items.Add.Caption := s;
    ListViewDuplicates.ItemIndex := 0;

    Result := (ShowModal <> mrCancel);
  end else
    Result := True;

  if (Result) then
  begin
    case FDuplicateAction of
      daPrompt:
        begin
          Assert(ListViewDuplicates.Selected <> nil);
          Value := Duplicates[ListViewDuplicates.Selected.Index];
        end;

      daFirst, daFirstAll:
        Value := Duplicates[0];

      daSkip, daSkipAll:
        Value := '';
    else
      Value := '';
    end;
  end else
    FDuplicateAction := daAbort;
end;

end.
