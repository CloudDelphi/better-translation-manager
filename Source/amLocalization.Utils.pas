unit amLocalization.Utils;

interface

// Replacement for menus.StripHotKey
// Does not allow multiple hotkeys. E.g.: "&This && &that"
function StripAccelerator(const Value: string): string;

const
  cAcceleratorPrefix = '&'; // From menus.pas

implementation

uses
  SysUtils;

function StripAccelerator(const Value: string): string;
var
  i: Integer;
  LastWasPrefix: boolean;
  HasHotKey: boolean;
  c: Char;
begin
  Result := Value;
  i := 1;
  LastWasPrefix := False;
  HasHotKey := False;
  while (i <= Length(Result)) do
  begin
    c := Result[i];
    if (c = cAcceleratorPrefix) then
    begin
      Delete(Result, i, 1);

      LastWasPrefix := not LastWasPrefix;
    end else
    begin
      if IsLeadChar(c) then
        Inc(i);

      if (LastWasPrefix) then
      begin
        if (HasHotKey) then
          Exit(Value);
        HasHotKey := True;
        LastWasPrefix := False;
      end;
    end;
    Inc(i);
  end;
end;

// -----------------------------------------------------------------------------

end.
