unit amLocalization.Dialog.SelectModule;

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
  amLocalization.Model;


type
  TFormSelectModule = class(TForm)
    LayoutControl: TdxLayoutControl;
    ComboBoxModule: TcxComboBox;
    ButtonOK: TcxButton;
    ButtonCancel: TcxButton;
    LayoutControlGroup_Root: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    LayoutItemPrompt: TdxLayoutLabeledItem;
    dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem;
    LayoutGroupPrompt: TdxLayoutGroup;
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
  LayoutItemPrompt.CaptionOptions.Text := Prompt;
  LayoutGroupPrompt.Visible := (Prompt <> '');

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
