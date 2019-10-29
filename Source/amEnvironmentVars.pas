unit amEnvironmentVars;

(*
 * Copyright © 2008 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  Classes;

type
  TApplyEnvironmentValueProc = reference to procedure(const Name, Value: string);

  TEnvironmentGroup = (egStatic, egCustomizable);
  TEnvironmentGroups = set of TEnvironmentGroup;

  TEnvironmentValue = record
    Group: TEnvironmentGroup;
    Name: string;
    Value: string;
  end;

type
  TEnvironment = record
  private
    FValues: array of TEnvironmentValue;
    FCount: integer;
    FValidEnvironmentGroups: TEnvironmentGroups;
    FCurrentEnvironmentGroups: TEnvironmentGroups;

    function GetName(AIndex: integer): string;
    function GetValue(AIndex: integer): string;
    function GetValueByName(const AName: string): string;
    procedure SetValueByName(const AName, AValue: string);
  public
    property ValueByName[const AName: string]: string read GetValueByName write SetValueByName; default;

    procedure SetValue(const AName, AValue: string; AGroup: TEnvironmentGroup); overload;

    property Names[AIndex: integer]: string read GetName;
    property Values[AIndex: integer]: string read GetValue;
    property Count: integer read FCount;

    function ExpandString(const Name: string; Groups: TEnvironmentGroups = [egStatic, egCustomizable]): string;
    function TokenizeString(const Name: string; Groups: TEnvironmentGroups = [egStatic, egCustomizable]; AllowTouchFileSystem: boolean = True): string;

    procedure ApplyValues(Proc: TApplyEnvironmentValueProc; Groups: TEnvironmentGroups = [egStatic, egCustomizable]);

    procedure SetEnvironmentValues(Groups: TEnvironmentGroups = [egStatic, egCustomizable]; ClearExcluded: boolean = False);
    procedure ClearEnvironmentValues(Groups: TEnvironmentGroups = [egStatic, egCustomizable]);
    procedure GetNames(Strings: TStrings; Groups: TEnvironmentGroups = [egStatic, egCustomizable]);
    procedure GetValues(Strings: TStrings; Groups: TEnvironmentGroups = [egStatic, egCustomizable]);

    function GetEnvironmentVariable(const Name: string; Groups: TEnvironmentGroups = [egStatic, egCustomizable]): string;

    class function GetEnvironmentValues(Values: TStrings): boolean; overload; static;
  end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  Windows,
  ShLwApi, // must be before SysUtils or StrToInt will resolve to wrong unit
  ShlObj,
  SysUtils,
  IOUtils,
  StrUtils,
  RegularExpressions,
  amPath;


function CSIDLToNumber(Name: string): integer;
begin
  Name := AnsiUpperCase(Name);
  Result := 0;

  if Name = 'CSIDL_PERSONAL' then
    Result := $0005; { My Documents }
  if Name = 'CSIDL_APPDATA' then
    Result := $001A; { Application Data, new for NT4 }
  if Name = 'CSIDL_LOCAL_APPDATA' then
    Result := $001C; { non roaming, user\Local Settings\Application Data }
  if Name = 'CSIDL_INTERNET_CACHE' then
    Result := $0020;
  if Name = 'CSIDL_COOKIES' then
    Result := $0021;
  if Name = 'CSIDL_HISTORY' then
    Result := $0022;
  if Name = 'CSIDL_COMMON_APPDATA' then
    Result := $0023; { All Users\Application Data }
  if Name = 'CSIDL_WINDOWS' then
    Result := $0024; { GetWindowsDirectory() }
  if Name = 'CSIDL_SYSTEM' then
    Result := $0025; { GetSystemDirectory() }
  if Name = 'CSIDL_PROGRAM_FILES' then
    Result := $0026; { C:\Program Files }
  if Name = 'CSIDL_MYPICTURES' then
    Result := $0027; { My Pictures, new for Win2K }
  if Name = 'CSIDL_PROGRAM_FILES_COMMON' then
    Result := $002B; { C:\Program Files\Common }
  if Name = 'CSIDL_COMMON_DOCUMENTS' then
    Result := $002E; { All Users\Documents }
  if Name = 'CSIDL_FLAG_CREATE' then
    Result := $8000; { new for Win2K, or this in to force creation of folder }
  if Name = 'CSIDL_COMMON_ADMINTOOLS' then
    Result := $002F; { All Users\Start Menu\Programs\Administrative Tools }
  if Name = 'CSIDL_ADMINTOOLS' then
    Result := $0030; { <user Name>\Start Menu\Programs\Administrative Tools }
end;

function GetCSIDLFolder(const Name: integer): string;
var
  PIDL: PItemIdList;
  ADeskFolder: array [0 .. MAX_PATH] of Char;
begin
  SHGetSpecialFolderLocation(0, Name, PIDL);
  SHGetPathFromIDList(PIDL, ADeskFolder);
  Result := ADeskFolder;
end;

// -----------------------------------------------------------------------------

function TEnvironment.GetValueByName(const AName: string): string;
var
  Index: integer;
begin
  for Index := 0 to FCount-1 do
    if (AnsiSameText(FValues[Index].Name, AName)) then
      Exit(FValues[Index].Value);
  Result := '';
end;

procedure TEnvironment.SetValueByName(const AName, AValue: string);
begin
  SetValue(AName, AValue, egCustomizable);
end;

procedure TEnvironment.SetValue(const AName, AValue: string; AGroup: TEnvironmentGroup);
var
  Index: integer;
  Delta: Integer;
begin
  Index := 0;
  while (Index < FCount) do
  begin
    if (AnsiSameText(FValues[Index].Name, AName)) then
      break;
    Inc(Index);
  end;

  if (Index = Length(FValues)) then
  begin
    if FCount > 8 then
      Delta := 8
    else
      Delta := 4;

    SetLength(FValues, FCount + Delta)
  end;

  FValues[Index].Group := AGroup;
  FValues[Index].Name := AName;
  FValues[Index].Value := AValue;

  if (Index = FCount) then
    Inc(FCount);
end;

function TEnvironment.GetValue(AIndex: integer): string;
begin
  Result := FValues[AIndex].Value;
end;

function TEnvironment.GetName(AIndex: integer): string;
begin
  Result := FValues[AIndex].Name;
end;

// -----------------------------------------------------------------------------

class function TEnvironment.GetEnvironmentValues(Values: TStrings): boolean;
var
  EnvVars: PChar;    // pointer to start of environment block
  EnvEntry: PChar;   // pointer to an env string in block
begin
  // Clear the list
  Values.Clear;
  Result := False;

  // Get reference to environment block for this process
  EnvVars := GetEnvironmentStrings;
  if (EnvVars = nil) then
    exit;

  // We have a block: extract strings from it
  // Env strings are #0 separated and list ends with #0#0
  try
    EnvEntry := EnvVars;

    while (EnvEntry^ <> #0) do
    begin
      Values.Add(EnvEntry);
      Inc(EnvEntry, StrLen(EnvEntry) + 1);
      Result := True;
    end;
  finally
    // Dispose of the memory block
    FreeEnvironmentStrings(EnvVars);
  end;
end;


function TEnvironment.TokenizeString(const Name: string; Groups: TEnvironmentGroups; AllowTouchFileSystem: boolean): string;
var
  EnvironmentValues: TStrings;
  EnvName, EnvValue: string;
  EnvPath, Path: string;
  BestLength: integer;
  BestEnvName, BestPath: string;
  s: string;
  i: integer;
begin
  Path := ExpandString(Name);

  if (PathUtil.PathIsRelative(Path)) then
  begin
    // Relative path can not be tokenized
    Result := Name;
    exit;
  end;

  Path := PathUtil.PathMakeCanonical(Path);

  if (AllowTouchFileSystem) then
  begin
    try
      if (TFile.Exists(Path)) then
        Path := PathUtil.FilenameMakeLong(Path);
    except
      // Ignore file system errors - it's more important that we get the job done
    end;
  end;

  BestEnvName := '';
  BestLength := MaxInt;

  // First give the system environment values a go
  SetLength(BestPath, MAX_PATH);
  if (PathUnExpandEnvStrings(PChar(Path), PChar(BestPath), Length(BestPath)+1)) then
  begin
    SetLength(BestPath, StrLen(PChar(BestPath)));
    // Extract environment string name
    if (BestPath <> '') and (BestPath[1] = '%') then
    begin
      i := PosEx('%', BestPath, 2);
      if (i >= 2) then
      begin
        BestEnvName := Copy(BestPath, 2, i-2);
        // Do not use the "%SystemDrive%" string. It doesn't make sense for our use.
        if (not AnsiSameText(BestEnvName, 'SystemDrive')) then
        begin
          Delete(BestPath, 1, i);
          BestLength := Length(BestPath);
        end else
          BestEnvName := '';
      end;
    end;
  end;
  if (BestEnvName = '') then
    BestPath := Path;

  // Try the application environment values
  if (Groups <> []) then
  begin
    EnvironmentValues := TStringList.Create;
    try
      GetValues(EnvironmentValues, Groups);
      for i := 0 to EnvironmentValues.Count-1 do
      begin
        EnvName := EnvironmentValues.Names[i];
        if (EnvName = '') then
          continue;

        EnvValue := EnvironmentValues.Values[EnvName];
        if (EnvValue = '') then
          continue;

        EnvPath := PathUtil.PathMakeCanonical(EnvValue);


        // FilenameMakeRelative cannot handle a folder relative to itself, so we handle that here
        s := ExcludeTrailingPathDelimiter(EnvPath);
        if (AnsiStartsText(s, Path)) then
        begin
          s := Copy(Path, Length(s)+1, MaxInt);
          if (s <> '') and (s[1] = '\') then
            s := '.' + s
          else
            s := '.\' + s;
        end else
        begin
          // Try to make path relative to our environment value
          s := PathUtil.FilenameMakeRelative(EnvPath, Path);

          // Disallow forms of relative path that does not start with '.\'
          if (not StartsStr('.\', s)) then
            continue;

          // Disallow '..'
          if (ContainsStr(s, '..')) then
            continue;
        end;

        if (Length(s)-1 < BestLength) then
        begin
          BestEnvName := EnvName;
          BestPath := s;
          // Save relative path, but without the leading '.'.
          // We should actually also remove the leading '\' but we leave it be since the result "looks" better
          // with a "\" separating the environment value from the rest.
          // When the resulting string is expanded we will end up with a double slash, but the since
          // the ExpandEnvironmentVariable() function can handle that no harm is done.
          Delete(BestPath, 1, 1);
          // Old: Delete(BestPath, 1, 2); // Save relative path, but without the leading '.\'
          BestLength := Length(BestPath);
        end;
      end;
    finally
      EnvironmentValues.Free;
    end;
  end;

  if (BestEnvName <> '') then
    Result := '%' + BestEnvName + '%' + BestPath
  else
    Result := Path
end;

procedure TEnvironment.GetNames(Strings: TStrings; Groups: TEnvironmentGroups);
begin
  ApplyValues(
    procedure(const Name, Value: string)
    begin
      Strings.Add(Name);
    end, Groups);
end;

procedure TEnvironment.GetValues(Strings: TStrings; Groups: TEnvironmentGroups);
begin
  ApplyValues(
    procedure(const Name, Value: string)
    begin
      Strings.Values[Name] := Value;
    end, Groups);
end;

procedure TEnvironment.SetEnvironmentValues(Groups: TEnvironmentGroups; ClearExcluded: boolean);
var
  ValuesToClear: TEnvironmentGroups;
begin
  if (Groups = FValidEnvironmentGroups) and (Groups = FCurrentEnvironmentGroups) then
    exit;

  // Clear invalid values
  ValuesToClear := FCurrentEnvironmentGroups - FValidEnvironmentGroups;
  // And those that are not in the new set
  if (ClearExcluded) then
    ValuesToClear := ValuesToClear + (FCurrentEnvironmentGroups - Groups);
  // Do not clear those we are setting. We will overwrite them anyway
  ValuesToClear := ValuesToClear - Groups;

  // FCurrentEnvironmentGroups: Those that are set
  // FValidEnvironmentGroups: Those that are set and have valid values

  if (ValuesToClear <> []) then
  begin
    ClearEnvironmentValues(ValuesToClear);
    ASSERT(FValidEnvironmentGroups <= FCurrentEnvironmentGroups);

    if (FValidEnvironmentGroups = Groups) then
      exit;
  end;

  Groups := Groups-FValidEnvironmentGroups;

  if (Groups <> []) then
    ApplyValues(
      procedure(const Name, Value: string)
      begin
        Windows.SetEnvironmentVariable(PChar(Name), PChar(Value));
      end, Groups-FValidEnvironmentGroups);

  FCurrentEnvironmentGroups := FCurrentEnvironmentGroups + Groups;
  FValidEnvironmentGroups := FValidEnvironmentGroups + Groups;
end;

procedure TEnvironment.ClearEnvironmentValues(Groups: TEnvironmentGroups);
begin
  ApplyValues(
    procedure(const Name, Value: string)
    begin
      Windows.SetEnvironmentVariable(PChar(Name), nil);
    end, Groups);
  FCurrentEnvironmentGroups := FCurrentEnvironmentGroups - Groups;
  FValidEnvironmentGroups := FValidEnvironmentGroups - Groups;
end;

procedure TEnvironment.ApplyValues(Proc: TApplyEnvironmentValueProc; Groups: TEnvironmentGroups);
var
  Group: TEnvironmentGroup;
  i: integer;
begin
  for Group in Groups do
    for i := 0 to FCount-1 do
      if (FValues[i].Group = Group) then
        Proc(FValues[i].Name, FValues[i].Value);
end;

function TEnvironment.ExpandString(const Name: string; Groups: TEnvironmentGroups): string;

  function RemoveDoubleBackslashesFromPath(const Value: string): string;
  var
    n: integer;
  begin
    // Remove double slashes (but avoid breaking UNC)
    Result := Value;
    n := PosEx('\\', Result);
    while (n > 0) do
    begin
      if (n <> 1) then
        Delete(Result, n, 1)
      else
        inc(n);

      n := PosEx('\\', Result, n);
    end;
  end;

var
  p: integer;
  Buffer: string;
  Size: integer;
  n: integer;
begin
  Result := Name;
  if (Name = '') then
    exit;

  if (StrScan(PChar(Name), '%') = nil) then
    exit;

  SetEnvironmentValues(Groups, False);

  Size := ExpandEnvironmentStrings(PChar(Result), nil, 0);
  if (Size <> 0) then
  begin
    SetLength(Buffer, Size-1);
    Size := ExpandEnvironmentStrings(PChar(Result), PChar(Buffer), Size);
    SetLength(Buffer, Size-1);
    Result := Buffer;
  end;

  n := PosEx('{', Result);
  if (n <> 0) then
  begin
    Size := PosEx('}', Result, n) - n;
    if (Size > 0) then
    begin
      p := CSIDLToNumber(Copy(Result, n, Size));
      if (p > 0) then
      begin
        Delete(Result, n, Size);
        Insert(GetCSIDLFolder(p), Result, n);
      end;
    end;
  end;

  Result := StringReplace(Result, '/', '\', [rfReplaceAll]);

  Result := RemoveDoubleBackslashesFromPath(Result);
end;

function TEnvironment.GetEnvironmentVariable(const Name: string; Groups: TEnvironmentGroups): string;
var
  Count: integer;
begin
  SetEnvironmentValues(Groups, False);

  Count := Windows.GetEnvironmentVariable(PChar(Name), nil, 0);
  if (Count = 0) then
  begin
    if (GetLastError <> ERROR_ENVVAR_NOT_FOUND) then
      RaiseLastOSError;
    Exit('');
  end;

  SetLength(Result, Count-1);
  if (Windows.GetEnvironmentVariable(PChar(Name), PChar(Result), Count) = 0) then
    RaiseLastOSError;
end;

end.
