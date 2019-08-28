program amTranslationManager;

uses
  SysUtils,
  Vcl.Forms,
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
  amLocalization.Data.TranslationMemory in 'amLocalization.Data.TranslationMemory.pas' {DataModuleTranslationMemory: TDataModule},
  amLocalization.Utils in 'amLocalization.Utils.pas',
  amLocalization.Dialog.TranslationMemory.SelectDuplicate in 'amLocalization.Dialog.TranslationMemory.SelectDuplicate.pas' {FormSelectDuplicate},
  amLocalization.Translator in 'amLocalization.Translator.pas';

{$R *.res}

begin
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
