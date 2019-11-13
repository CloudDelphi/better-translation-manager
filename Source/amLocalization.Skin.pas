unit amLocalization.Skin;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

// -----------------------------------------------------------------------------
//
//              Skin utilities
//
// -----------------------------------------------------------------------------
function ComposeSkinName(const Name: string; const Filename: string = ''; Index: integer = -1): string;
procedure DecomposeSkinName(const Value: string; var Name, Filename: string; var Index: integer);


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  SysUtils,
  amLocalization.Environment;

// -----------------------------------------------------------------------------

function ComposeSkinName(const Name: string; const Filename: string = ''; Index: integer = -1): string;
begin
  Result := Name;
  if (Filename <> '') then
    Result := Result +'@' + EnvironmentVars.TokenizeString(Filename);
  if (Index <> -1) then
    Result := Result +',' + IntToStr(Index);
end;

procedure DecomposeSkinName(const Value: string; var Name, Filename: string; var Index: integer);
var
  n: integer;
begin
  n := LastDelimiter('@', Value);
  if (n >= 1) then
  begin
    Name := Copy(Value, 1, n-1);
    Filename := Copy(Value, n+1, MaxInt);
    n := Pos(',', Filename);
    if (n >= 1) then
    begin
      Index := StrToIntDef(Copy(Filename, n+1, MaxInt), -1);
      SetLength(Filename, n-1);
    end else
      Index := -1;
    Filename := EnvironmentVars.ExpandString(Filename);
  end else
  begin
    Name := Value;
    Filename := '';
    Index := -1;
  end;
end;

// -----------------------------------------------------------------------------

end.
