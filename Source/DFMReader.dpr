program DFMReader;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form4},
  amLocalization.Model in 'amLocalization.Model.pas',
  FormNewProject in 'FormNewProject.pas' {FormNewProject},
  amLocalization.Import.XLIFF in 'amLocalization.Import.XLIFF.pas',
  amLocalization.ResourceWriter in 'amLocalization.ResourceWriter.pas',
  amLocalization.Persistence in 'amLocalization.Persistence.pas',
  amLocalization.Engine in 'amLocalization.Engine.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
