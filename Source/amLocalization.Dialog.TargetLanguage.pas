unit amLocalization.Dialog.TargetLanguage;

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

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore,
  dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxContainer, cxEdit, cxLabel,
  dxLayoutContainer, cxClasses, cxButtons, dxLayoutControl, cxTextEdit,
  cxMaskEdit, cxDropDownEdit, cxLookupEdit, cxDBLookupEdit, cxDBExtLookupComboBox,

  amLanguageInfo,
  amLocalization.Dialog;


type
  TFormTargetLanguage = class(TFormDialog)
    LayoutItemTargetLanguage: TdxLayoutItem;
    ComboBoxSourceLanguage: TcxExtLookupComboBox;
    procedure ActionOKUpdate(Sender: TObject);
  private
    function GetLanguage: TLanguageItem;
    procedure SetLanguage(const Value: TLanguageItem);
  public
    function Execute(const Prompt: string): boolean;

    property Language: TLanguageItem read GetLanguage write SetLanguage;
  end;

implementation

{$R *.dfm}

uses
  amLocalization.Data.Main;


{ TFormTargetLanguage }

procedure TFormTargetLanguage.ActionOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (ComboBoxSourceLanguage.ItemIndex <> -1);

end;

function TFormTargetLanguage.Execute(const Prompt: string): boolean;
begin
  SetHeader(Prompt);

  Result := (ShowModal = mrOK);
end;

function TFormTargetLanguage.GetLanguage: TLanguageItem;
begin
  Result := LanguageInfo.FindLocaleName(ComboBoxSourceLanguage.EditValue);
end;

procedure TFormTargetLanguage.SetLanguage(const Value: TLanguageItem);
begin
  if (Value <> nil) then
    ComboBoxSourceLanguage.EditValue := Value.LocaleName
  else
    ComboBoxSourceLanguage.Clear;
end;

end.
