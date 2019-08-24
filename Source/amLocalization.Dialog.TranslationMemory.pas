unit amLocalization.Dialog.TranslationMemory;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Data.DB,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, dxSkinsCore,
  cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator, dxDateRanges,
  cxDataControllerConditionalFormattingRulesManagerDialog, cxDBData, dxLayoutControlAdapters, dxLayoutContainer,
  cxButtons, dxLayoutControl, cxGridLevel, cxClasses, cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid,

  amLocalization.Data.TranslationMemory;

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
    procedure ButtonLoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GridTMDBTableViewCustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
      AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
  private
    FDataModuleTranslationMemory: TDataModuleTranslationMemory;
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

  GridTMDBTableView.DataController.DataSource := FDataModuleTranslationMemory.DataSourceTranslationMemory;

  ShowModal;

  Result := True;
end;

procedure TFormTranslationMemory.ButtonLoadClick(Sender: TObject);
begin
  SaveCursor(crHourGlass);

  GridTMDBTableView.BeginUpdate;
  try
    FDataModuleTranslationMemory.LoadTranslationMemory(TPath.ChangeExtension(Application.ExeName, '.tmx'));

    GridTMDBTableView.DataController.CreateAllItems;
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
