unit amLocalization.Dialog.NewProject;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ActnList,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  dxLayoutcxEditAdapters, dxLayoutContainer, cxClasses, cxDropDownEdit, cxLookupEdit, cxDBLookupEdit, cxDBExtLookupComboBox,
  cxTextEdit, cxMaskEdit, cxButtonEdit, dxLayoutControl, dxLayoutControlAdapters, cxButtons, dxSkinsCore,
  cxGridDBTableView, System.Actions, cxMRUEdit, cxLabel,

  amLanguageInfo,
  amLocalization.Dialog;

type
  TFormNewProject = class(TFormDialog)
    EditSourceApplication: TcxMRUEdit;
    dxLayoutItem1: TdxLayoutItem;
    ComboBoxSourceLanguage: TcxExtLookupComboBox;
    dxLayoutItem2: TdxLayoutItem;
    FileOpenDialogApplication: TFileOpenDialog;
    procedure ActionCancelExecute(Sender: TObject);
    procedure ActionOKExecute(Sender: TObject);
    procedure ActionOKUpdate(Sender: TObject);
    procedure EditSourceApplicationPropertiesButtonClick(Sender: TObject);
  private
    function GetSourceApplication: string;
    function GetSourceLanguage: TLanguageItem;
    procedure SetSourceApplication(const Value: string);
    procedure SetSourceLanguage(const Value: TLanguageItem);
  public
    procedure SetLanguageView(View: TcxGridDBTableView; ListItem: TcxGridDBColumn);
    function Execute: boolean;
    property SourceApplication: string read GetSourceApplication write SetSourceApplication;
    property SourceLanguage: TLanguageItem read GetSourceLanguage write SetSourceLanguage;
  end;

implementation

{$R *.dfm}

uses
  IOUtils,
  amLocalization.Settings,
  amLocalization.Data.Main;

procedure TFormNewProject.ActionCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormNewProject.ActionOKExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TFormNewProject.ActionOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (SourceApplication <> '') and (TFile.Exists(SourceApplication));
end;

procedure TFormNewProject.EditSourceApplicationPropertiesButtonClick(Sender: TObject);
begin
  if (SourceApplication <> '') then
  begin
    FileOpenDialogApplication.FileName := TPath.GetFileName(SourceApplication);
    FileOpenDialogApplication.DefaultFolder := TPath.GetDirectoryName(SourceApplication);
  end;

  if (FileOpenDialogApplication.Execute) then
    EditSourceApplication.Text := FileOpenDialogApplication.FileName;
end;

function TFormNewProject.Execute: boolean;
var
  i: integer;
begin
  EditSourceApplication.Properties.Items.Clear;
  for i := 0 to TranslationManagerSettings.Folders.RecentApplications.Count-1 do
    EditSourceApplication.Properties.Items.Add(TranslationManagerSettings.Folders.RecentApplications[i]);

  Result := (ShowModal = mrOK);

  if (not Result) then
    Exit;

  for i := 0 to TranslationManagerSettings.Folders.RecentApplications.Count-1 do
    if (AnsiSameText(SourceApplication, TranslationManagerSettings.Folders.RecentApplications[i])) then
    begin
      TranslationManagerSettings.Folders.RecentApplications.Move(i, 0);
      Exit;
    end;

  TranslationManagerSettings.Folders.RecentApplications.Insert(0, SourceApplication);

  // Prune to at most 10 items
  for i := TranslationManagerSettings.Folders.RecentApplications.Count-1 downto 10 do
    TranslationManagerSettings.Folders.RecentApplications.Delete(i);
end;

function TFormNewProject.GetSourceApplication: string;
begin
  Result := EditSourceApplication.Text;
end;

function TFormNewProject.GetSourceLanguage: TLanguageItem;
begin
  Result := LanguageInfo.FindLocaleName(VarToStr(ComboBoxSourceLanguage.EditValue));
end;

procedure TFormNewProject.SetLanguageView(View: TcxGridDBTableView; ListItem: TcxGridDBColumn);
begin
  ComboBoxSourceLanguage.Properties.View := View;
  ComboBoxSourceLanguage.Properties.ListFieldItem := ListItem;
end;

procedure TFormNewProject.SetSourceApplication(const Value: string);
begin
  EditSourceApplication.Text := Value;
end;

procedure TFormNewProject.SetSourceLanguage(const Value: TLanguageItem);
begin
  ComboBoxSourceLanguage.EditValue := Value.LocaleName;
end;

end.
