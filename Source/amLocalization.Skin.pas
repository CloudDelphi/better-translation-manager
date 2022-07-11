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
// This unit contains references to the DevExpress skin units used in the project.
//
// Skin units should not be referenced anywhere else.
//
// This applies to both standard DevExpress skins and custom skins.
//
// -----------------------------------------------------------------------------

uses
  // The following units contains the skins that are included in the project
  dxSkinBasic,
  dxSkinTheBezier;


// -----------------------------------------------------------------------------
//
//              Skin utilities
//
// -----------------------------------------------------------------------------
function ComposeSkinName(const Name: string; const Filename: string = ''; Index: integer = -1): string;
procedure DecomposeSkinName(const Value: string; var Name, Filename: string; var Index: integer);

// Call CheckSkins to verify that only required skins have been linked into the application.
// Raises an exception if additional skins are found.
procedure CheckSkins;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  SysUtils,
  Windows,
  amLocalization.Environment;

const
  // These skin resources might be present. We allow them if they are.
  cSkinStandardList: array of string = ['DEFAULTSKINICONLARGE', 'DEFAULTSKINICONSMALL', 'DEFAULTUSERSKINDATA'];

  // These are the skins that we allow
  cSkinWhiteList: array of string = [
    'Basic'
    ];

  // These are the skins that we require
  cSkinRequireList: array of string = [
    'TheBezier'
    ];

// -----------------------------------------------------------------------------

var
  ErrorMessage: string;

function EnumSkinResourceNamesProc(Module: HModule; ResType, ResName: PWideChar): BOOL; stdcall;
var
  i: integer;
begin
  Result := True;
  for i := Low(cSkinStandardList) to High(cSkinStandardList) do
    if (SameText(cSkinStandardList[i], Resname)) then
      exit;

  for i := Low(cSkinWhiteList) to High(cSkinWhiteList) do
    if (SameText(cSkinWhiteList[i], Resname)) then
      exit;

  for i := Low(cSkinRequireList) to High(cSkinRequireList) do
    if (SameText(cSkinRequireList[i], Resname)) then
      exit;

  // Return error.
  // Do not use raise as we cannot pass exceptions from a WinAPI callback.
  ErrorMessage := ResName;
  Result := False;
end;

// -----------------------------------------------------------------------------

procedure CheckSkins;

  procedure VerifyDevExpressSkins;
  begin
    // Verify that no standard devexpress skins are present by comparing all skin resources against a white list
    if (not EnumResourceNames(hInstance, PChar('DXSKINS'), @EnumSkinResourceNamesProc, 0)) then
      raise Exception.CreateFmt('Illegal skin resource found: %s', [ErrorMessage]);
  end;

  procedure VerifyApplicationSkins;
  begin
    // Verify that all required skins are present
    for var i := Low(cSkinRequireList) to High(cSkinRequireList) do
      if (FindResource(hInstance, PChar(cSkinRequireList[i]), PChar('DXSKINS')) = 0) then
        raise Exception.CreateFmt('Required skin resource not found: %s', [cSkinRequireList[i]]);
  end;

begin
  VerifyDevExpressSkins;
  VerifyApplicationSkins;
end;


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
