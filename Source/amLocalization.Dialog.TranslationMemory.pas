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
  Vcl.Controls, Vcl.StdCtrls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, System.Actions, Vcl.ActnList, Vcl.ExtCtrls,
  Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, dxSkinsCore,
  cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator, dxDateRanges,
  cxDataControllerConditionalFormattingRulesManagerDialog, cxDBData, dxLayoutControlAdapters, dxLayoutContainer,
  cxButtons, dxLayoutControl, cxGridLevel, cxClasses, cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, cxEditRepositoryItems, dxLayoutcxEditAdapters, cxContainer, cxLabel,
  dxBarBuiltInMenu, cxGridCustomPopupMenu, cxGridPopupMenu,

  amLocalization.Dialog,
  amLocalization.Translator.TM;

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
    GridPopupMenu: TcxGridPopupMenu;
    PopupMenuHeader: TPopupMenu;
    MenuItemDeleteLanguage: TMenuItem;
    ActionDeleteLanguage: TAction;
    procedure ButtonLoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GridTMDBTableViewCustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
      AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
    procedure ButtonSaveAsClick(Sender: TObject);
    procedure ActionDeleteLanguageExecute(Sender: TObject);
    procedure ActionDeleteLanguageUpdate(Sender: TObject);
    procedure GridPopupMenuPopupMenus0Popup(ASenderMenu: TComponent; AHitTest: TcxCustomGridHitTest; X, Y: Integer);
  private
    FDataModuleTranslationMemory: TDataModuleTranslationMemory;
    FPoupMenuColumn: TcxGridColumn;
  protected
    procedure CreateColumns;
    procedure SaveLayout;
    procedure RestoreLayout;
  public
    function Execute(ADataModuleTranslationMemory: TDataModuleTranslationMemory): boolean;
  end;

resourcestring
  sTranslationMemoryAddCompleteTitle = 'Translation added';
  sTranslationMemoryAddMergeCompleteTitle = 'Merge completed';
  sTranslationMemoryAddOpenCompleteTitle = 'Translation Memory opened';
  sTranslationMemoryAddComplete = 'The translations were added to the Translation Memory.'#13#13+
    'Values added: %.0n'#13+
    'Values merged: %.0n'#13+
    'Values skipped: %.0n'#13+
    'Duplicate values: %.0n';

implementation

{$R *.dfm}

uses
  IOUtils,
  UITypes,
  amCursorService,
  amLocale,
  amProgressForm,
  amLocalization.Settings,
  amLocalization.Data.Main;

procedure TFormTranslationMemory.RestoreLayout;
begin
  if (TranslationManagerSettings.Layout.TranslationMemory.Valid) then
    GridTMDBTableView.RestoreFromRegistry(TranslationManagerSettings.Layout.KeyPath, False, False, [gsoUseFilter], TranslationManagerSettings.Layout.TranslationMemory.Name);
end;

procedure TFormTranslationMemory.SaveLayout;
begin
  GridTMDBTableView.StoreToRegistry(TranslationManagerSettings.Layout.KeyPath, False, [gsoUseFilter], TranslationManagerSettings.Layout.TranslationMemory.Name);
  TranslationManagerSettings.Layout.TranslationMemory.Valid := True;
end;

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

  RestoreLayout;

  ShowModal;

  SaveLayout;

  Result := True;
end;

procedure TFormTranslationMemory.ActionDeleteLanguageExecute(Sender: TObject);
var
  LocaleItem: TLocaleItem;
  Field: TField;
  Clone: TFDMemTable;
resourcestring
  sDeleteLanguage = 'Are you sure you want to delete the "%s" language from the Translation Memory?';
begin
  Field := TcxGridDBColumn(FPoupMenuColumn).DataBinding.Field;
  LocaleItem := TLocaleItems.FindLCID(Field.Tag);
  Assert(LocaleItem <> nil);

  if (MessageDlg(Format(sDeleteLanguage, [LocaleItem.LanguageName]), mtConfirmation, [mbYes, mbNo], 0, mbNo) <> mrYes) then
    Exit;

  GridTMDBTableView.BeginUpdate;
  try
    // Start by removing the column
    FPoupMenuColumn.Free;

    // Then remove the field
    if (FDataModuleTranslationMemory.TableTranslationMemory.Fields.Count > 1) then
    begin
      FDataModuleTranslationMemory.TableTranslationMemory.DisableControls;
      try
        Clone := TFDMemTable.Create(nil);
        try
          Clone.CopyDataSet(FDataModuleTranslationMemory.TableTranslationMemory, [coStructure, coRestart, coAppend]);

          FDataModuleTranslationMemory.TableTranslationMemory.Close;

          Field.Free;

          FDataModuleTranslationMemory.TableTranslationMemory.Open;

          FDataModuleTranslationMemory.TableTranslationMemory.CopyDataSet(Clone, [coAppend]);
        finally
          Clone.Free;
        end;
      finally
        FDataModuleTranslationMemory.TableTranslationMemory.EnableControls;
      end;
    end else
      Field.Free;
  finally
    GridTMDBTableView.EndUpdate;
  end;
end;

procedure TFormTranslationMemory.ActionDeleteLanguageUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FPoupMenuColumn <> nil);
end;

procedure TFormTranslationMemory.ButtonLoadClick(Sender: TObject);
var
  Merge: boolean;
  Res: Word;
  Stats, OneStats: TTranslationMemoryMergeStats;
  Filename: string;
  First: boolean;
  Title: string;
  DuplicateAction: TTranslationMemoryDuplicateAction;
  Progress: IProgress;
resourcestring
  sLoadTranslationMemoryMergeTitle = 'Merge translation memory';
  sLoadTranslationMemoryMerge = 'Do you want to merge the selected file into your existing translation memory?'+#13#13+
      'If you answer No then your current translation memory will be closed and the specified file will be opened instead.';
begin
  if (not FDataModuleTranslationMemory.CheckSave) then
    Exit;

  OpenDialogTMX.InitialDir := TPath.GetDirectoryName(TranslationManagerSettings.Translators.TranslationMemory.Filename);
  OpenDialogTMX.FileName := TPath.GetFileName(TranslationManagerSettings.Translators.TranslationMemory.Filename);

  if (not OpenDialogTMX.Execute(Handle)) then
    Exit;

  if (FDataModuleTranslationMemory.TableTranslationMemory.Active) and (FDataModuleTranslationMemory.TableTranslationMemory.RecordCount > 0) then
  begin
    Res := TaskMessageDlg(sLoadTranslationMemoryMergeTitle, sLoadTranslationMemoryMerge, mtConfirmation, [mbYes, mbNo, mbCancel], 0, mbCancel);

    if (Res = mrCancel) then
      Exit;

    Merge := (Res = mrYes);
  end else
    Merge := False;

  SaveCursor(crHourGlass);

  SaveLayout;

  GridTMDBTableView.BeginUpdate;
  try
    Stats := Default(TTranslationMemoryMergeStats);
    First := True;
    DuplicateAction := tmDupActionPrompt;
    if (OpenDialogTMX.Files.Count > 1) then
    begin
      Progress := ShowProgress(sTranslationMemoryLoading);
      Progress.EnableAbort := True;
      Progress.Progress(psBegin, 0, OpenDialogTMX.Files.Count);
      SaveCursor(crAppStart);
    end else
    begin
      Progress := nil;
      SaveCursor(crHourGlass);
    end;

    for Filename in OpenDialogTMX.Files do
    begin
      if (Progress <> nil) then
        Progress.AdvanceProgress;
      OneStats := FDataModuleTranslationMemory.LoadTranslationMemory(Filename, DuplicateAction, Merge, Progress);

      if (First) and (not Merge) then
        TranslationManagerSettings.Translators.TranslationMemory.Filename := Filename;
      First := False;

      Merge := True; // Additional files will be merged

      Inc(Stats.Added, OneStats.Added);
      Inc(Stats.Merged, OneStats.Merged);
      Inc(Stats.Skipped, OneStats.Skipped);
      Inc(Stats.Duplicate, OneStats.Duplicate);

      if (DuplicateAction = tmDupActionAbort) or ((Progress <> nil) and (Progress.Aborted)) then
        break;
    end;

    if (Progress <> nil) then
      Progress.Progress(psEnd, 1, 1);

    CreateColumns;

  finally
    GridTMDBTableView.EndUpdate;
  end;

  Progress := nil;

  RestoreLayout;

  if (Merge) then
    Title := sTranslationMemoryAddMergeCompleteTitle
  else
    Title := sTranslationMemoryAddOpenCompleteTitle;

  TaskMessageDlg(Title,
    Format(sTranslationMemoryAddComplete, [Stats.Added * 1.0, Stats.Merged * 1.0, Stats.Skipped * 1.0, Stats.Duplicate * 1.0]),
    mtInformation, [mbOK], 0);
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

procedure TFormTranslationMemory.GridPopupMenuPopupMenus0Popup(ASenderMenu: TComponent; AHitTest: TcxCustomGridHitTest; X,
  Y: Integer);
begin
  if (AHitTest is TcxGridColumnHeaderHitTest) then
  begin
    FPoupMenuColumn := TcxGridColumnHeaderHitTest(AHitTest).Column;

    // If we have an OnPopup event handler then the popupmenu isn't displayed...
    // Really intuitive logic DevExpress....
    // ...so we have to display is manually.

    PopupMenuHeader.PopupComponent := GridTMDBTableView;
    PopupMenuHeader.Popup(X, Y);
  end else
    FPoupMenuColumn := nil;
end;

procedure TFormTranslationMemory.GridTMDBTableViewCustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
begin
(* Turns out we really don't need this.

  // We've sneakily stored the language charset in the field tag
  ACanvas.Font.Charset := TcxGridDBColumn(AViewInfo.Item).DataBinding.Field.Tag;

  // ...Also we no longer store the charset in the tag
*)
end;

end.
