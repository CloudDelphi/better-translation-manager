﻿unit amLocalization.Dialog.SelectModule;

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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  dxSkinsCore, Vcl.Menus, dxLayoutcxEditAdapters, dxLayoutControlAdapters, System.Actions, Vcl.ActnList, dxLayoutContainer,
  cxClasses, Vcl.StdCtrls, cxButtons, dxLayoutControl, cxLookupEdit, cxDBLookupEdit, cxDBExtLookupComboBox, cxTextEdit, cxMaskEdit,
  cxDropDownEdit,
  amLocalization.Dialog,
  amLocalization.Model, Vcl.ExtCtrls, cxLabel;


type
  TFormSelectModule = class(TFormDialog)
    ComboBoxModule: TcxComboBox;
    dxLayoutItem1: TdxLayoutItem;
  private
  public
    function Execute(Project: TLocalizerProject; const Prompt: string = ''): TLocalizerModule;
  end;

implementation

{$R *.dfm}

uses
  amLocalization.Data.Main;


{ TFormSelectModule }

function TFormSelectModule.Execute(Project: TLocalizerProject; const Prompt: string): TLocalizerModule;
begin
  SetHeader(Prompt);

  ComboBoxModule.Properties.Items.Clear;
  Project.Traverse(
    function(Module: TLocalizerModule): boolean
    begin
      ComboBoxModule.Properties.Items.AddObject(Module.Name, Module);
      Result := True;
    end);

  ComboBoxModule.ItemIndex := 0;

  if (ShowModal <> mrOK) then
    Exit(nil);

  Result := TLocalizerModule(ComboBoxModule.ItemObject);
end;

end.