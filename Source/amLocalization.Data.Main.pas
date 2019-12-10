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
  MidasLib,
  cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, cxNavigator, dxDateRanges, cxDataControllerConditionalFormattingRulesManagerDialog, cxDBData, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls, cxGridCustomView, cxGrid, dxLayoutLookAndFeels, cxEditRepositoryItems,
  cxDBExtLookupComboBox, cxImageList, cxExtEditRepositoryItems, cxCheckBox,
  amLocalization.Model;

// -----------------------------------------------------------------------------
//
// TDataModuleMain
//
// -----------------------------------------------------------------------------
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
    EditRepositoryCheckComboBoxNormalization: TcxEditRepositoryCheckComboBox;
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

    function GetImageIndex(Prop: TLocalizerProperty; TranslationLanguage: TTranslationLanguage): integer;
    function GetImageHint(ImageIndex: integer): string;

    procedure GetContentStyle(Active, Focused, Selected, Editing: boolean; TranslationLanguage: TTranslationLanguage; Prop: TLocalizerProperty; var AStyle: TcxStyle);
  end;

var
  DataModuleMain: TDataModuleMain;

// -----------------------------------------------------------------------------
//
// Image List Indices
//
// -----------------------------------------------------------------------------
const
  ImageIndexAbout               = 57;
  ImageIndexInfo                = 58;
  ImageIndexBookmark0           = 27;
  ImageIndexBookmarkA           = 37;
  ImageIndexModified            = 44;
  ImageIndexNotModified         = -1;

const
  NodeImageIndexStateWarning    = 0;

const
  NodeImageIndexNew             = 0;
  NodeImageIndexUnused          = 1;
  NodeImageIndexDontTranslate   = 2;
  NodeImageIndexProposed        = 3;
  NodeImageIndexTranslated      = 4;
  NodeImageIndexHold            = 5;
  NodeImageIndexNotTranslated   = 6;
  NodeImageIndexObsolete        = 7;
  NodeImageIndexComplete25      = 8;
  NodeImageIndexComplete50      = 9;
  NodeImageIndexComplete75      = 10;

// And hints corresponding to the above images
resourcestring
  sNodeImageHintNew = 'Not translated. Added by last refresh';
  sNodeImageHintUnused = 'No longer in use';
  sNodeImageHintDontTranslate = 'Should not be translated';
  sNodeImageHintProposed = 'Translation has been proposed';
  sNodeImageHintTranslated = 'Has been translated';
  sNodeImageHintHold = 'Placed on hold';
  sNodeImageHintNotTranslated = 'Not translated';
  sNodeImageHintObsolete = 'Source value has changed since translation';
  sNodeImageHintComplete25 = 'Approximately 25% has been translated';
  sNodeImageHintComplete50 = 'Approximately 50% has been translated';
  sNodeImageHintComplete75 = 'Approximately 75% has been translated';

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  cxButtonEdit,
  amLocale,
  amLocalization.Settings,
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
  ClientDataSetLanguages.DisableControls;
  try
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
  finally
    ClientDataSetLanguages.EnableControls;
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
    TextEditor.Text := TcxButtonEdit(Sender).EditingText;
    if (TcxButtonEdit(Sender).Tag <> 0) then
      TextEditor.TargetLanguage := TLocaleItems.FindLCID(TcxButtonEdit(Sender).Tag);

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

procedure TDataModuleMain.GetContentStyle(Active, Focused, Selected, Editing: boolean; TranslationLanguage: TTranslationLanguage; Prop: TLocalizerProperty; var AStyle: TcxStyle);
var
  Translation: TLocalizerTranslation;
begin
  if (Selected) and (not Active) then
  begin
    AStyle := StyleInactive;
    Exit;
  end else
  if (Selected) and (not Focused) then
  begin
    AStyle := StyleSelected;
    Exit;
  end else
  if (Active) and (Focused) and (not Editing) then
  begin
    AStyle := StyleFocused;
    Exit;
  end;

  if (Prop.IsUnused) or (Prop.EffectiveStatus = ItemStatusDontTranslate) then
  begin
    AStyle := StyleDontTranslate;
    Exit;
  end;

  if (Prop.EffectiveStatus = ItemStatusHold) then
  begin
    AStyle := StyleHold;
    Exit;
  end;

  if (not Prop.Translations.TryGetTranslation(TranslationLanguage, Translation)) then
    Translation := nil;

  if (Translation <> nil) and (Translation.IsTranslated) then
  begin
    if (Translation.Status = TTranslationStatus.tStatusProposed) then
      AStyle := StyleProposed
    else
      AStyle := StyleComplete;
  end else
    AStyle := StyleNeedTranslation;
end;

function TDataModuleMain.GetImageHint(ImageIndex: integer): string;
begin
  case ImageIndex of
    NodeImageIndexNew:          Result := sNodeImageHintNew;
    NodeImageIndexUnused:       Result := sNodeImageHintUnused;
    NodeImageIndexDontTranslate:Result := sNodeImageHintDontTranslate;
    NodeImageIndexProposed:     Result := sNodeImageHintProposed;
    NodeImageIndexTranslated:   Result := sNodeImageHintTranslated;
    NodeImageIndexHold:         Result := sNodeImageHintHold;
    NodeImageIndexNotTranslated:Result := sNodeImageHintNotTranslated;
    NodeImageIndexObsolete:     Result := sNodeImageHintObsolete;
    NodeImageIndexComplete25:   Result := sNodeImageHintComplete25;
    NodeImageIndexComplete50:   Result := sNodeImageHintComplete50;
    NodeImageIndexComplete75:   Result := sNodeImageHintComplete75;
  else
    Result := '';
  end;
end;

function TDataModuleMain.GetImageIndex(Prop: TLocalizerProperty; TranslationLanguage: TTranslationLanguage): integer;
var
  Translation: TLocalizerTranslation;
begin
  Result := -1;

  if (Prop = nil) or (not TranslationManagerSettings.Editor.DisplayStatusGlyphs) then
    Exit;

  if (not Prop.Translations.TryGetTranslation(TranslationLanguage, Translation)) then
    Translation := nil;

  // Note: Image indicates effective status

  if (Prop.IsUnused) then
    Result := NodeImageIndexUnused
  else
  if (ItemStateNew in Prop.State) and (Prop.EffectiveStatus = ItemStatusTranslate) and (Translation = nil) then
    Result := NodeImageIndexNew
  else
  if (Prop.EffectiveStatus = ItemStatusDontTranslate) then
    Result := NodeImageIndexDontTranslate
  else
  if (Prop.EffectiveStatus = ItemStatusHold) then
    Result := NodeImageIndexHold
  else
  if (Translation <> nil) and (Translation.Status <> tStatusPending) then
  begin
    if (Translation.Status = tStatusProposed) then
      Result := NodeImageIndexProposed
    else
    if (Translation.Status = tStatusTranslated) then
      Result := NodeImageIndexTranslated
    else
    if (Translation.Status = tStatusObsolete) then
      Result := NodeImageIndexObsolete
    else
      Result := -1; // Should never happen
  end else
    Result := NodeImageIndexNotTranslated;
end;

procedure TDataModuleMain.GridTableViewTargetLanguagesDataControllerFilterRecord(ADataController: TcxCustomDataController;
  ARecordIndex: Integer; var Accept: Boolean);
begin
  if (not FFilterTargetLanguages) or (FProject = nil) then
    Exit;

  Accept := (FProject.TranslationLanguages.Contains(ADataController.Values[ARecordIndex, GridTableViewLanguagesColumnLocaleID.Index]));
end;

end.
