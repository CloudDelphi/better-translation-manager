unit amLocalization.System.SingleInst;

interface

uses
  Windows,
  DelphiDabbler.SingleInstance;

type
  TTranslationManagerSingleInstance = class(TSingleInstance)
  protected
    function WindowClassName: string; override;
    function Fingerprint: DWORD; override;
  end;

implementation

uses
  amVersionInfo;

function TTranslationManagerSingleInstance.Fingerprint: DWORD;
begin
  Result := $E32E2A7C;
end;

function TTranslationManagerSingleInstance.WindowClassName: string;
begin
  Result := 'Melander.TranslationManager.' + TVersionInfo.FileVersionString(ParamStr(0));
end;

initialization

  RegisterSingleInstanceClass(TTranslationManagerSingleInstance);

end.

