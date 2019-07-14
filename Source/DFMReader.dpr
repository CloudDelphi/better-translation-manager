program DFMReader;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form4},
  amLocalizer in 'amLocalizer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
