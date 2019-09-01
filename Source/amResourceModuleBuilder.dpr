program amResourceModuleBuilder;

{$APPTYPE CONSOLE}

{$R *.res}
{$R amTranslationManager.res}

uses
  System.SysUtils,
  amLocalization.CommandLine;

var
  CommandLineTool: TLocalizationCommandLineTool;
begin
  CommandLineTool := TLocalizationCommandLineTool.Create(TCommandLineLogger.Create);
  try

    CommandLineTool.Execute;

  finally
    CommandLineTool.Free;
  end;
end.
