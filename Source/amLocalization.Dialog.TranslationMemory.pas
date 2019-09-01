unit amLocalization.Dialog.TranslationMemory;

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
  Vcl.Controls, Vcl.StdCtrls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Data.DB,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, dxSkinsCore,
  cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator, dxDateRanges,
  cxDataControllerConditionalFormattingRulesManagerDialog, cxDBData, dxLayoutControlAdapters, dxLayoutContainer,
  cxButtons, dxLayoutControl, cxGridLevel, cxClasses, cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, cxEditRepositoryItems,

  amLocalization.Dialog,
  amLocalization.Translator.TM, dxLayoutcxEditAdapters, cxContainer, System.Actions, Vcl.ActnList, Vcl.ExtCtrls, cxLabel;

type
  TFormTranslationMemory = class(TFormDialog)
    GridTMDBTableView: TcxGridDBTableView;
    GridTMLevel: TcxGridLevel;
    GridTM: TcxGrid;
    dxLayoutItem2: TdxLayoutItem;
    OpenDialogTMX: TOpenDialog;
    SaveDialogTMX: TSaveDialog;
    dxLayoutItem1: TdxLayoutItem;
    ButtonClose: TcxButton;
    dxLayoutItem3: TdxLayoutItem;
    ButtonSaveAs: TcxButton;
    dxLayoutItem4: TdxLayoutItem;
    ButtonLoad: TcxButton;
    procedure ButtonLoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GridTMDBTableViewCustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
      AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
    procedure ButtonSaveAsClick(Sender: TObject);
  private
    FDataModuleTranslationMemory: TDataModuleTranslationMemory;
  protected
    procedure CreateColumns;
  public
    function Execute(ADataModuleTranslationMemory: TDataModuleTranslationMemory): boolean;
  end;

implementation

{$R *.dfm}

uses
  IOUtils,
  amCursorService,
  amLocalization.Settings,
  amLocalization.Data.Main;

function TFormTranslationMemory.Execute(ADataModuleTranslationMemory: TDataModuleTranslationMemory): boolean;
begin
  FDataModuleTranslationMemory := ADataModuleTranslationMemory;

  FDataModuleTranslationMemory.CheckLoaded(True);

  GridTMDBTableView.BeginUpdate;
  try
    GridTMDBTableView.DataController.DataSource := FDataModuleTranslationMemory.DataSourceTranslationMemory;

    CreateColumns;
  finally
    GridTMDBTableView.EndUpdate;
  end;

  ShowModal;

  Result := True;
end;

procedure TFormTranslationMemory.ButtonLoadClick(Sender: TObject);
begin
  if (not FDataModuleTranslationMemory.CheckSave) then
    Exit;

  OpenDialogTMX.InitialDir := TPath.GetDirectoryName(TranslationManagerSettings.Translators.TranslationMemory.Filename);
  OpenDialogTMX.FileName := TPath.GetFileName(TranslationManagerSettings.Translators.TranslationMemory.Filename);

  if (not OpenDialogTMX.Execute(Handle)) then
    Exit;

  SaveCursor(crHourGlass);

  GridTMDBTableView.BeginUpdate;
  try
    FDataModuleTranslationMemory.LoadTranslationMemory(OpenDialogTMX.FileName);
    TranslationManagerSettings.Translators.TranslationMemory.Filename := OpenDialogTMX.FileName;

    CreateColumns;
  finally
    GridTMDBTableView.EndUpdate;
  end;
end;

procedure TFormTranslationMemory.ButtonSaveAsClick(Sender: TObject);
begin
  SaveDialogTMX.InitialDir := TPath.GetDirectoryName(TranslationManagerSettings.Translators.TranslationMemory.Filename);
  SaveDialogTMX.FileName := TPath.GetFileName(TranslationManagerSettings.Translators.TranslationMemory.Filename);

  if (not SaveDialogTMX.Execute(Handle)) then
    Exit;

  SaveCursor(crHourGlass);

  FDataModuleTranslationMemory.SaveTranslationMemory(SaveDialogTMX.FileName);
end;

procedure TFormTranslationMemory.CreateColumns;
var
  i: integer;
begin
  GridTMDBTableView.BeginUpdate;
  try
    GridTMDBTableView.ClearItems;
    GridTMDBTableView.DataController.CreateAllItems;

    for i := 0 to GridTMDBTableView.ColumnCount-1 do
    begin
      GridTMDBTableView.Columns[i].RepositoryItem := DataModuleMain.EditRepositoryTextItem;
      GridTMDBTableView.Columns[i].Width := 200;
      GridTMDBTableView.Columns[i].BestFitMaxWidth := 400;
    end;

    // GridTMDBTableView.ApplyBestFit;
  finally
    GridTMDBTableView.EndUpdate;
  end;
end;

procedure TFormTranslationMemory.FormCreate(Sender: TObject);
begin
  GridTMDBTableView.DataController.CreateAllItems(True);
end;

procedure TFormTranslationMemory.GridTMDBTableViewCustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
begin
(* Turns out we really don't need this.

  // We've sneakily stored the language charset in the field tag
  ACanvas.Font.Charset := TcxGridDBColumn(AViewInfo.Item).DataBinding.Field.Tag;
*)
end;

end.
