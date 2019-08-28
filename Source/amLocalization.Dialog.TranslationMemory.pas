unit amLocalization.Dialog.TranslationMemory;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Data.DB,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, dxSkinsCore,
  cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator, dxDateRanges,
  cxDataControllerConditionalFormattingRulesManagerDialog, cxDBData, dxLayoutControlAdapters, dxLayoutContainer,
  cxButtons, dxLayoutControl, cxGridLevel, cxClasses, cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, cxEditRepositoryItems,

  amLocalization.Translator.TM;

type
  TFormTranslationMemory = class(TForm)
    GridTMDBTableView: TcxGridDBTableView;
    GridTMLevel: TcxGridLevel;
    GridTM: TcxGrid;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutControl1: TdxLayoutControl;
    ButtonLoad: TcxButton;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    dxLayoutItem3: TdxLayoutItem;
    ButtonClose: TcxButton;
    OpenDialogTMX: TOpenDialog;
    dxLayoutItem4: TdxLayoutItem;
    ButtonSaveAs: TcxButton;
    SaveDialogTMX: TSaveDialog;
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
  amLocalization.Data.Main;

function TFormTranslationMemory.Execute(ADataModuleTranslationMemory: TDataModuleTranslationMemory): boolean;
begin
  FDataModuleTranslationMemory := ADataModuleTranslationMemory;

  if (not FDataModuleTranslationMemory.IsLoaded) then
  begin
    SaveCursor(crHourGlass);
    FDataModuleTranslationMemory.LoadTranslationMemory(FDataModuleTranslationMemory.Filename);
  end;

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

  OpenDialogTMX.FileName := FDataModuleTranslationMemory.Filename;

  if (not OpenDialogTMX.Execute(Handle)) then
    Exit;

  SaveCursor(crHourGlass);

  GridTMDBTableView.BeginUpdate;
  try
    FDataModuleTranslationMemory.LoadTranslationMemory(OpenDialogTMX.FileName);
    FDataModuleTranslationMemory.Filename := OpenDialogTMX.FileName;

    CreateColumns;
  finally
    GridTMDBTableView.EndUpdate;
  end;
end;

procedure TFormTranslationMemory.ButtonSaveAsClick(Sender: TObject);
begin
  SaveDialogTMX.FileName := FDataModuleTranslationMemory.Filename;

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
