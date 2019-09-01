program amTranslationManager;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

uses
  madExcept,
  madListModules,
  SysUtils,
  Vcl.Forms,
  Dialogs,
  UITypes,
  amLocalization.Dialog.Main in 'amLocalization.Dialog.Main.pas' {Form4},
  amLocalization.Model in 'amLocalization.Model.pas',
  amLocalization.Dialog.NewProject in 'amLocalization.Dialog.NewProject.pas' {FormNewProject},
  amLocalization.Import.XLIFF in 'amLocalization.Import.XLIFF.pas',
  amLocalization.ResourceWriter in 'amLocalization.ResourceWriter.pas',
  amLocalization.Persistence in 'amLocalization.Persistence.pas',
  amLocalization.Engine in 'amLocalization.Engine.pas',
  amLocalization.Dialog.TextEdit in 'amLocalization.Dialog.TextEdit.pas' {FormTextEditor},
  amLocalization.Dialog.Languages in 'amLocalization.Dialog.Languages.pas' {FormLanguages},
  amLocalization.Data.Main in 'amLocalization.Data.Main.pas' {DataModuleMain: TDataModule},
  amLocalization.Translator.Microsoft.Version3 in 'amLocalization.Translator.Microsoft.Version3.pas' {DataModuleTranslatorMicrosoftV3: TDataModule},
  amLocalization.Dialog.Search in 'amLocalization.Dialog.Search.pas' {FormSearch},
  amLocalization.Dialog.TranslationMemory in 'amLocalization.Dialog.TranslationMemory.pas' {FormTranslationMemory},
  amLocalization.Translator.TM in 'amLocalization.Translator.TM.pas' {DataModuleTranslationMemory: TDataModule},
  amLocalization.Utils in 'amLocalization.Utils.pas',
  amLocalization.Dialog.TranslationMemory.SelectDuplicate in 'amLocalization.Dialog.TranslationMemory.SelectDuplicate.pas' {FormSelectDuplicate},
  amLocalization.Translator in 'amLocalization.Translator.pas',
  amLocalization.Settings in 'amLocalization.Settings.pas',
  amLocalization.Dialog.Settings in 'amLocalization.Dialog.Settings.pas' {FormSettings},
  amLocalization.Dialog.SelectModule in 'amLocalization.Dialog.SelectModule.pas' {FormSelectModule},
  amLocalization.CommandLine in 'amLocalization.CommandLine.pas';

{$R *.res}

type
  TCommandLineGUILogger = class(TInterfacedObject, ICommandLineLogger)
  private
    FMessages: string;
    FHasWarnings: boolean;
    FHasErrors: boolean;
  protected
    procedure DisplayMessages;
  private
    // ICommandLineLogger
    procedure Message(const Msg: string);
    procedure Error(const Msg: string);
    procedure Warning(const Msg: string);
  public
    destructor Destroy; override;
  end;

destructor TCommandLineGUILogger.Destroy;
begin
  DisplayMessages;
  inherited;
end;

procedure TCommandLineGUILogger.DisplayMessages;
var
  DialogType: TMsgDlgType;
begin
  if (FMessages = '') then
    Exit;

  if (FHasErrors) then
    DialogType := mtError
  else
  if (FHasWarnings) then
    DialogType := mtWarning
  else
    DialogType := mtInformation;

  MessageDlg(FMessages, DialogType, [mbOK], 0);

  FMessages := '';
  FHasErrors := False;
  FHasWarnings := False;
end;

procedure TCommandLineGUILogger.Error(const Msg: string);
begin
  FMessages := FMessages + Format('Error: %s', [Msg]);
  FHasErrors := True;

  DisplayMessages;

  Halt(1);
end;

procedure TCommandLineGUILogger.Message(const Msg: string);
begin
  FMessages := FMessages + Msg;
end;

procedure TCommandLineGUILogger.Warning(const Msg: string);
begin
  FMessages := FMessages + Format('Warning: %s', [Msg]);
  FHasWarnings := True;
end;

var
  CommandLineTool: TLocalizationCommandLineTool;
begin
  if (ParamCount > 0) then
  begin
    CommandLineTool := TLocalizationCommandLineTool.Create(TCommandLineGUILogger.Create);
    try

      CommandLineTool.Execute;

    finally
      CommandLineTool.Free;
    end;

    Exit;
  end;

  if (CheckWin32Version(6, 0)) then
  begin
    // Application.DefaultFont is the font used when TForm.ParentFont=True.
    // It is Tahoma by default but should be Segoe UI on Vista and later (according to MS UI guide lines).
    // See InitDefFontData() in graphics.pas
    Application.DefaultFont.Name := Screen.MessageFont.Name;
    // DefFontData.Name specifies the default font for everything that doesn't specify a specific font.
    // For now we leave it as is (Tahoma). At some point it should follow the system default like above:
    // DefFontData.Name := Screen.MessageFont.Name;
  end;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
