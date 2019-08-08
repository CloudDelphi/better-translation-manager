unit amLocalization.Data.Main;

interface

uses
  System.SysUtils, System.Classes, Data.DB, Datasnap.DBClient, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, cxNavigator, dxDateRanges, cxDataControllerConditionalFormattingRulesManagerDialog, cxDBData, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls, cxGridCustomView, cxGrid,
  amLocalization.Model;

type
  TDataModuleMain = class(TDataModule)
    ClientDataSetLanguages: TClientDataSet;
    ClientDataSetLanguagesLocaleID: TWordField;
    ClientDataSetLanguagesLocaleName: TStringField;
    ClientDataSetLanguagesLanguageName: TStringField;
    ClientDataSetLanguagesCountryName: TStringField;
    DataSourceLanguages: TDataSource;
    GridViewRepository: TcxGridViewRepository;
    GridTableViewLanguages: TcxGridDBTableView;
    GridTableViewLanguagesColumnLocaleID: TcxGridDBColumn;
    GridTableViewLanguagesColumnLocaleName: TcxGridDBColumn;
    GridTableViewLanguagesColumnLanguage: TcxGridDBColumn;
    GridTableViewLanguagesColumnCountry: TcxGridDBColumn;
    GridTableViewTargetLanguages: TcxGridDBTableView;
    GridTableViewTargetLanguagesLocaleID: TcxGridDBColumn;
    GridTableViewTargetLanguagesLocaleName: TcxGridDBColumn;
    GridTableViewTargetLanguagesLanguageName: TcxGridDBColumn;
    GridTableViewTargetLanguagesCountryName: TcxGridDBColumn;
    procedure DataModuleCreate(Sender: TObject);
    procedure GridTableViewTargetLanguagesDataControllerFilterRecord(ADataController: TcxCustomDataController;
      ARecordIndex: Integer; var Accept: Boolean);
  private
    FFilterTargetLanguages: boolean;
    FLocalizerProject: TLocalizerProject;
  public
    property FilterTargetLanguages: boolean read FFilterTargetLanguages write FFilterTargetLanguages;
    property LocalizerProject: TLocalizerProject read FLocalizerProject write FLocalizerProject;
  end;

var
  DataModuleMain: TDataModuleMain;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  amLocale;

type
  TcxCustomGridViewCracker = class(TcxCustomGridView);

procedure TDataModuleMain.DataModuleCreate(Sender: TObject);
var
  i: integer;
begin
  // Clone
  TcxCustomGridViewCracker(GridTableViewTargetLanguages).AssignPattern(GridTableViewLanguages);

  // Restore filter event handler
  GridTableViewTargetLanguages.DataController.OnFilterRecord := GridTableViewTargetLanguagesDataControllerFilterRecord;

  FFilterTargetLanguages := True;

  ClientDataSetLanguages.CreateDataSet;
  for i := 0 to TLocaleItems.Count-1 do
  begin
    ClientDataSetLanguages.Append;
    try
      ClientDataSetLanguagesLocaleID.Value := TLocaleItems.Items[i].Locale;
      ClientDataSetLanguagesLocaleName.AsString := TLocaleItems.Items[i].LocaleName;
      ClientDataSetLanguagesLanguageName.AsString := TLocaleItems.Items[i].LanguageName;
      ClientDataSetLanguagesCountryName.AsString := TLocaleItems.Items[i].CountryName;

      ClientDataSetLanguages.Post;
    except
      ClientDataSetLanguages.Cancel;
      raise;
    end;
  end;
end;

procedure TDataModuleMain.GridTableViewTargetLanguagesDataControllerFilterRecord(ADataController: TcxCustomDataController;
  ARecordIndex: Integer; var Accept: Boolean);
begin
  if (not FFilterTargetLanguages) or (FLocalizerProject = nil) then
    Exit;

  Accept := (FLocalizerProject.TargetLanguages.Contains(ADataController.Values[ARecordIndex, GridTableViewLanguagesColumnLocaleID.Index]));
end;

end.
