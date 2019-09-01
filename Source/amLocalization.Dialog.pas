unit amLocalization.Dialog;

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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore, Vcl.Menus,
  dxLayoutControlAdapters, dxLayoutContainer, cxClasses, Vcl.StdCtrls, cxButtons, dxLayoutControl, System.Actions, Vcl.ActnList,
  cxContainer, cxEdit, cxGroupBox, Vcl.ExtCtrls, dxLayoutcxEditAdapters, cxLabel;

type
  TFormDialog = class abstract(TForm)
    ActionList: TActionList;
    ActionOK: TAction;
    ActionCancel: TAction;
    LayoutControlButtons: TdxLayoutControl;
    ButtonOK: TcxButton;
    ButtonCancel: TcxButton;
    LayoutControlButtonsGroup_Root: TdxLayoutGroup;
    LayoutItemButtonOK: TdxLayoutItem;
    LayoutItemButtonCancel: TdxLayoutItem;
    LayoutGroupButtons: TdxLayoutGroup;
    LayoutControl: TdxLayoutControl;
    LayoutControlGroup_Root: TdxLayoutGroup;
    LayoutControlHeader: TdxLayoutControl;
    LayoutControlHeaderGroup_Root: TdxLayoutGroup;
    LayoutItemHeader: TdxLayoutItem;
    LabelHeader: TcxLabel;
    PanelMain: TPanel;
    procedure ActionOKExecute(Sender: TObject);
    procedure ActionCancelExecute(Sender: TObject);
  private
  public
    procedure SetHeader(const Value: string);
  end;

implementation

{$R *.dfm}

uses
  amLocalization.Data.Main;

procedure TFormDialog.ActionCancelExecute(Sender: TObject);
begin
//
end;

procedure TFormDialog.ActionOKExecute(Sender: TObject);
begin
//
end;

procedure TFormDialog.SetHeader(const Value: string);
begin
  LabelHeader.Caption := Value;
  LayoutControlHeader.Visible := (Value <> '');
end;

end.
