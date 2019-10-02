unit amLocalization.Data.Main;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  System.SysUtils, System.Classes, System.ImageList, Vcl.ImgList, Vcl.Controls,
  Data.DB, Datasnap.DBClient,
  cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, cxNavigator, dxDateRanges, cxDataControllerConditionalFormattingRulesManagerDialog, cxDBData, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls, cxGridCustomView, cxGrid, dxLayoutLookAndFeels, cxEditRepositoryItems,
  cxDBExtLookupComboBox, cxImageList,
  amLocalization.Model, cxTL;

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
    GridTableViewFilteredTargetLanguages: TcxGridDBTableView;
    GridTableViewFilteredTargetLanguagesLocaleID: TcxGridDBColumn;
    GridTableViewFilteredTargetLanguagesLocaleName: TcxGridDBColumn;
    GridTableViewFilteredTargetLanguagesLanguage: TcxGridDBColumn;
    GridTableViewFilteredTargetLanguagesCountry: TcxGridDBColumn;
    LayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    LayoutSkinLookAndFeel: TdxLayoutSkinLookAndFeel;
    EditRepository: TcxEditRepository;
    EditRepositoryTextItem: TcxEditRepositoryButtonItem;
    LayoutSkinLookAndFeelHeader: TdxLayoutSkinLookAndFeel;
    EditRepositoryComboBoxItemLanguage: TcxEditRepositoryExtLookupComboBoxItem;
    EditRepositoryComboBoxItemFilteredTargetLanguage: TcxEditRepositoryExtLookupComboBoxItem;
    ImageListTree: TcxImageList;
    ImageListState: TcxImageList;
    ImageListSmall: TcxImageList;
    ImageListLarge: TcxImageList;
    StyleRepository: TcxStyleRepository;
    StyleDefault: TcxStyle;
    StyleComplete: TcxStyle;
    StyleNeedTranslation: TcxStyle;
    StyleDontTranslate: TcxStyle;
    StyleHold: TcxStyle;
    StyleSelected: TcxStyle;
    StyleFocused: TcxStyle;
    StyleInactive: TcxStyle;
    StyleProposed: TcxStyle;
    procedure DataModuleCreate(Sender: TObject);
    procedure GridTableViewTargetLanguagesDataControllerFilterRecord(ADataController: TcxCustomDataController;
      ARecordIndex: Integer; var Accept: Boolean);
    procedure EditRepositoryTextItemPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
  private
    FFilterTargetLanguages: boolean;
    FProject: TLocalizerProject;
  public
    property FilterTargetLanguages: boolean read FFilterTargetLanguages write FFilterTargetLanguages;
    property Project: TLocalizerProject read FProject write FProject;
  end;

var
  DataModuleMain: TDataModuleMain;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  cxButtonEdit,
  amLocale,
  amLocalization.Dialog.TextEdit;

type
  TcxCustomGridViewCracker = class(TcxCustomGridView);

procedure TDataModuleMain.DataModuleCreate(Sender: TObject);
var
  i: integer;
begin
  // Clone
  TcxCustomGridViewCracker(GridTableViewFilteredTargetLanguages).AssignPattern(GridTableViewLanguages);

  // Restore filter event handler
  GridTableViewFilteredTargetLanguages.DataController.OnFilterRecord := GridTableViewTargetLanguagesDataControllerFilterRecord;

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

type
  TcxCustomEditCracker = class(TcxCustomEdit);

procedure TDataModuleMain.EditRepositoryTextItemPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
var
  TextEditor: TFormTextEditor;
begin
  TextEditor := TFormTextEditor.Create(nil);
  try
//    TextEditor.SourceText := FocusedProperty.Value;
    TextEditor.Text := TcxButtonEdit(Sender).EditingText;// FocusedProperty.TranslatedValue[TargetLanguage];

    if (TextEditor.Execute(False)) then
    begin
      // Write new value back to inner edit control. The OnChange event will occur as normally when the user exits the cell.
      TcxCustomEditCracker(Sender).InnerEdit.EditValue := TextEditor.Text;
      TcxCustomEdit(Sender).ModifiedAfterEnter := True;
    end;
  finally
    TextEditor.Free;
  end;
end;

procedure TDataModuleMain.GridTableViewTargetLanguagesDataControllerFilterRecord(ADataController: TcxCustomDataController;
  ARecordIndex: Integer; var Accept: Boolean);
begin
  if (not FFilterTargetLanguages) or (FProject = nil) then
    Exit;

  Accept := (FProject.TranslationLanguages.Contains(ADataController.Values[ARecordIndex, GridTableViewLanguagesColumnLocaleID.Index]));
end;

end.
