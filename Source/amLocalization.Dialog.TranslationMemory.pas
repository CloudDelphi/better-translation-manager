unit amLocalization.Dialog.TranslationMemory;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

{$WARN SYMBOL_PLATFORM OFF}

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
  amLocalization.TranslationMemory;

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
    TaskDialogOpen: TTaskDialog;
    ActionExport: TAction;
    ActionImport: TAction;
    DataSourceTranslationMemory: TDataSource;
    procedure FormCreate(Sender: TObject);
    procedure GridTMDBTableViewCustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
      AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
    procedure ActionDeleteLanguageExecute(Sender: TObject);
    procedure ActionDeleteLanguageUpdate(Sender: TObject);
    procedure GridPopupMenuPopupMenus0Popup(ASenderMenu: TComponent; AHitTest: TcxCustomGridHitTest; X, Y: Integer);
    procedure ActionExportExecute(Sender: TObject);
    procedure ActionImportExecute(Sender: TObject);
    procedure GridTMDBTableViewInitEdit(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit);
  private
    FTranslationMemory: ITranslationMemory;
    FPoupMenuColumn: TcxGridColumn;
    FRightToLeft: array of boolean;
  protected
    procedure CreateColumns;
    procedure SaveLayout;
    procedure RestoreLayout;
  public
    function Execute(ATranslationMemory: ITranslationMemory): boolean;
  end;

resourcestring
  sTranslationMemoryAddCompleteTitle = 'Translation added';
  sTranslationMemoryMergeCompleteTitle = 'Merge completed';
  sTranslationMemoryMergeComplete = 'The translations were added to the Translation Memory.'#13#13+
    'Values added: %.0n'#13+
    'Values merged: %.0n'#13+
    'Values skipped: %.0n'#13+
    'Duplicate values: %.0n';
  sTranslationMemoryOpenCompleteTitle = 'Translation Memory opened';
  sTranslationMemoryOpenComplete = 'Values read: %.0n';

implementation

{$R *.dfm}

uses
  IOUtils,
  UITypes,
  cxTextEdit,
  cxDrawTextUtils,
  amCursorService,
  amLocale,
  amProgress,
  amLocalization.Settings,
  amLocalization.Data.Main,
  amLocalization.Dialog.TranslationMemory.SelectFileFormat,
  amLocalization.TranslationMemory.Data,
  amLocalization.TranslationMemory.FileFormats,
  amLocalization.TranslationMemory.FileFormats.TMX;

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

function TFormTranslationMemory.Execute(ATranslationMemory: ITranslationMemory): boolean;
begin
  FTranslationMemory := ATranslationMemory;

  try

    if (not FTranslationMemory.CheckLoaded(True)) then
      Exit(false);

  except
    on E: ETranslationMemory do
    begin
      if (MessageDlg(E.Message, mtWarning, [mbIgnore, mbAbort], 0, mbAbort) = mrAbort) then
        Exit(False)
    end;
  end;

  GridTMDBTableView.BeginUpdate;
  try

    DataSourceTranslationMemory.DataSet := FTranslationMemory.TranslationMemoryDataSet;
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
  Clone: IInterface;
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
    if (FTranslationMemory.TranslationMemoryDataSet.Fields.Count > 1) then
    begin
      Clone := FTranslationMemory.SaveTableTranslationMemoryClone;
      try
        Field.Free;
      finally
        Clone := nil;
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

procedure TFormTranslationMemory.ActionExportExecute(Sender: TObject);
var
  TranslationMemoryFileFormatClass: TTranslationMemoryFileFormatClass;
  TranslationMemoryFileFormat: TTranslationMemoryFileFormat;
begin
  if (SaveDialogTMX.InitialDir = '') then
    SaveDialogTMX.InitialDir := TranslationManagerSettings.Folders.FolderDocuments;
  SaveDialogTMX.FileName := TPath.GetFileName(SaveDialogTMX.FileName);

  if (not SaveDialogTMX.Execute(Handle)) then
    Exit;

  SaveCursor(crHourGlass);

  TranslationMemoryFileFormatClass := TTranslationMemoryFileFormat.FindFileFormat(SaveDialogTMX.FileName, ffcSave, TTranslationMemoryFileFormatTMX);
  Assert(TranslationMemoryFileFormatClass <> nil);

  TranslationMemoryFileFormat := TranslationMemoryFileFormatClass.Create(FTranslationMemory);
  try

    TranslationMemoryFileFormat.SaveToFile(SaveDialogTMX.FileName);

  finally
    TranslationMemoryFileFormat.Free;
  end;
end;

procedure TFormTranslationMemory.ActionImportExecute(Sender: TObject);
var
  Merge: boolean;
  Res: Word;
  Stats, OneStats: TTranslationMemoryMergeStats;
  Filename: string;
  FileFormatClasses: TTranslationMemoryFileFormatClasses;
  FileFormatClass: TTranslationMemoryFileFormatClass;
  FileFormat: TTranslationMemoryFileFormat;
  FormSelectFileFormat: TFormSelectFileFormat;
  DuplicateAction: TTranslationMemoryDuplicateAction;
  Progress: IProgress;
  MergeFile: boolean;
resourcestring
  sLoadTranslationMemoryMergeTitle = 'Merge Translation Memory?';
  sLoadTranslationMemoryMerge = 'Do you want to merge the selected file(s) into your existing Translation Memory?'+#13#13+
      'If you answer No then your current Translation Memory will be emptied and replaced with the translations in the specified file.';
begin
  if (not FTranslationMemory.CheckSave) then
    Exit;

  if (OpenDialogTMX.InitialDir = '') then
    OpenDialogTMX.InitialDir := TranslationManagerSettings.Folders.FolderDocuments;

  if (not OpenDialogTMX.Execute(Handle)) then
    Exit;

  // Remember folder for next time
  OpenDialogTMX.InitialDir := TPath.GetDirectoryName(OpenDialogTMX.FileName);

  if (FTranslationMemory.TranslationMemoryDataSet.Active) and (FTranslationMemory.TranslationMemoryDataSet.RecordCount > 0) then
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

    MergeFile := Merge;

    FileFormatClasses := TTranslationMemoryFileFormat.FindFileFormats(OpenDialogTMX.FileName, ffcLoad);

    if (Length(FileFormatClasses) = 0) then
      Exit;

    if (Length(FileFormatClasses) > 1) then
    begin
      FormSelectFileFormat := TFormSelectFileFormat.Create(nil);
      try
        FileFormatClass := FormSelectFileFormat.Execute(FileFormatClasses);
      finally
        FormSelectFileFormat.Free;
      end;
      if (FileFormatClass = nil) then
        Exit;
    end else
      FileFormatClass := FileFormatClasses[0];

    Assert(FileFormatClass <> nil);

    FileFormat := FileFormatClass.Create(FTranslationMemory);
    try
      for Filename in OpenDialogTMX.Files do
      begin
        if (Progress <> nil) then
          Progress.AdvanceProgress;

        if (not FileFormat.Prepare(Filename)) then
          break;

        OneStats := FileFormat.LoadFromFile(Filename, DuplicateAction, MergeFile, Progress);

        MergeFile := True; // Additional files will be merged regardless of choice

        Inc(Stats.Added, OneStats.Added);
        Inc(Stats.Merged, OneStats.Merged);
        Inc(Stats.Skipped, OneStats.Skipped);
        Inc(Stats.Duplicate, OneStats.Duplicate);

        if (DuplicateAction = tmDupActionAbort) or ((Progress <> nil) and (Progress.Aborted)) then
          break;
      end;
    finally
      FileFormat.Free;
    end;

    if (not Merge) then
      FTranslationMemory.SetLoaded;

    if (Progress <> nil) then
      Progress.Progress(psEnd, 1, 1);

    CreateColumns;

  finally
    GridTMDBTableView.EndUpdate;
  end;

  Progress := nil;

  RestoreLayout;

  if (Merge) then
  begin
    TaskMessageDlg(sTranslationMemoryMergeCompleteTitle,
      Format(sTranslationMemoryMergeComplete, [Stats.Added * 1.0, Stats.Merged * 1.0, Stats.Skipped * 1.0, Stats.Duplicate * 1.0]),
      mtInformation, [mbOK], 0);
  end else
  begin
    TaskMessageDlg(sTranslationMemoryOpenCompleteTitle,
      Format(sTranslationMemoryOpenComplete, [Stats.Added * 1.0]),
      mtInformation, [mbOK], 0);
  end;
end;

procedure TFormTranslationMemory.CreateColumns;
var
  i: integer;
begin
  GridTMDBTableView.BeginUpdate;
  try
    GridTMDBTableView.ClearItems;
    GridTMDBTableView.DataController.CreateAllItems;
    SetLength(FRightToLeft, GridTMDBTableView.ColumnCount);

    for i := 0 to GridTMDBTableView.ColumnCount-1 do
    begin
      FRightToLeft[i] := (TLocaleItem.GetLocaleDataInt(GridTMDBTableView.Columns[i].DataBinding.Field.Tag, LOCALE_IREADINGLAYOUT) = 1);
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
  OpenDialogTMX.Filter := TTranslationMemoryFileFormat.FileFormatFileFilters(ffcLoad) + OpenDialogTMX.Filter;
  SaveDialogTMX.Filter := TTranslationMemoryFileFormat.FileFormatFileFilters(ffcSave) + SaveDialogTMX.Filter;
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
var
  Flags: DWORD;
begin
(* Turns out we really don't need this.

  // We've sneakily stored the language charset in the field tag
  ACanvas.Font.Charset := TcxGridDBColumn(AViewInfo.Item).DataBinding.Field.Tag;

  // ...Also we no longer store the charset in the tag
*)

  if (FRightToLeft[AViewInfo.Item.Index] <> IsRightToLeft) then
  begin
    // Target language is Right-to-Left but rest of UI isn't - or vice versa
    Flags := TcxCustomTextEditViewInfo(AViewInfo.EditViewInfo).DrawTextFlags;
    if (FRightToLeft[AViewInfo.Item.Index]) then
    begin
      // Set RTL
      Flags := Flags or CXTO_RTLREADING;
      // Swap left/right alignment
      if (TranslationManagerSettings.Editor.EditBiDiMode) then
      begin
        if ((Flags and $0000000F) = CXTO_LEFT) then
          Flags := (Flags and $FFFFFFF0) or CXTO_RIGHT
        else
        if ((Flags and $0000000F) =  CXTO_RIGHT) then
          Flags := (Flags and $FFFFFFF0) or CXTO_LEFT;
      end;
    end else
      // Clear RTL
      Flags := Flags and (not CXTO_RTLREADING);

    TcxCustomTextEditViewInfo(AViewInfo.EditViewInfo).DrawTextFlags := Flags;
  end;
end;

procedure TFormTranslationMemory.GridTMDBTableViewInitEdit(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem;
  AEdit: TcxCustomEdit);
begin
  // Editor is reused between columns so we need to reset this for every column
  // in case one of the columns has changed it
  if (TranslationManagerSettings.Editor.EditBiDiMode) then
  begin
    if (FRightToLeft[AItem.Index]) then
      AEdit.BiDiMode := bdRightToLeft
    else
      AEdit.BiDiMode := bdLeftToRight;
  end;

  // Store Locale in tag so we can use it when opening the text editor.
  // See: TDataModuleMain.EditRepositoryTextItemPropertiesButtonClick
  AEdit.Tag := TcxGridDBColumn(AItem).DataBinding.Field.Tag;
end;

end.
