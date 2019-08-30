unit amLocalization.Utils;

interface

function ComposeSkinName(const Name: string; const Filename: string = ''; Index: integer = -1): string;
procedure DecomposeSkinName(const Value: string; var Name, Filename: string; var Index: integer);


// Removes hotkeys and format specifiers
type
  TSanitizeKind = (skAccelerator, skFormat);
  TSanitizeKinds = set of TSanitizeKind;

function SanitizeText(const Value: string; Kind: TSanitizeKinds = [skAccelerator, skFormat]): string;

// Replacement for menus.StripHotKey
// Does not allow multiple hotkeys. E.g.: "&This && &that"
// Does not allow illegal hotkeys. E.g.: "& "
function StripAccelerator(const Value: string): string;
function HasAccelerator(const Value: string): boolean;
// Escapes &. Assumes Value doesn't contain real accelerators
function EscapeAccelerators(const Value: string): string;
// Adds &. Assumes any existing & are escaped
function AddAccelerator(const Value: string): string;

function IsUppercase(const Value: string): boolean;
function StartsWithUppercase(const Value: string): boolean;

const
  cAcceleratorPrefix = '&'; // From menus.pas

implementation

uses
  System.Character,
  StrUtils,
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
        if (HasHotKey) or (not c.IsLetterOrDigit) then
          Exit(Value);
        HasHotKey := True;
        LastWasPrefix := False;
      end;
    end;
    Inc(i);
  end;
end;

function HasAccelerator(const Value: string): boolean;
var
  i: Integer;
  LastWasPrefix: boolean;
  c: Char;
begin
  Result := False;
  i := 1;
  LastWasPrefix := False;
  while (i <= Length(Value)) do
  begin
    c := Value[i];
    if (c = cAcceleratorPrefix) then
    begin
      LastWasPrefix := not LastWasPrefix;
    end else
    begin
      if IsLeadChar(c) then
        Inc(i);

      if (LastWasPrefix) then
      begin
        if (Result) or (not c.IsLetterOrDigit) then
          Exit(False);
        Result := True;
        LastWasPrefix := False;
      end;
    end;
    Inc(i);
  end;
end;

function EscapeAccelerators(const Value: string): string;
var
  i, j: integer;
  NewLength: integer;
begin
  NewLength := Length(Value);
  for i := 1 to Length(Value) do
    if (Value[i] = cAcceleratorPrefix) then
      Inc(NewLength);

  if (NewLength = Length(Value)) then
    Exit(Value);

  SetLength(Result, NewLength);
  j := 1;
  for i := 1 to Length(Value) do
  begin
    Result[j] := Value[i];
    if (Value[i] = cAcceleratorPrefix) then
    begin
      Inc(j);
      Result[j] := cAcceleratorPrefix;
    end;
  end;
end;

function AddAccelerator(const Value: string): string;
var
  i: integer;
begin
  for i := 1 to Length(Value) do
    if (Value[i].IsLetterOrDigit) and ((i = 1) or (Value[i-1] <> cAcceleratorPrefix)) then
      Exit(Copy(Value, 1, i-1)+cAcceleratorPrefix+Copy(Value, i, MaxInt));
  Result := Value;
end;

// -----------------------------------------------------------------------------

function SanitizeText(const Value: string; Kind: TSanitizeKinds): string;
var
  n: integer;
begin
  // Handle & accelerator chars
  if (skAccelerator in Kind) then
    Result := StripAccelerator(Value)
  else
    Result := Value;

  // Handle Format strings
  if (skFormat in Kind) and (Result = Value) then
  begin
    // Find first format specifier
    n := PosEx('%', Result, 1);

    while (n > 0) and (n < Length(Result)) do
    begin
      Inc(n);

      if (Result[n] = '%') then
      begin
        // Escaped % - ignore
        Delete(Result, n, 1);
      end else
      if (Result[n] in ['0'..'9', '-', '.', 'd', 'u', 'e', 'f', 'g', 'n', 'm', 'p', 's', 'x']) then
      begin
        Result[n-1] := ' '; // Replace %... with space

        // Remove chars until end of format specifier
        while (Result[n] in ['0'..'9']) do
          Delete(Result, n, 1);

        if (Result[n] = ':') then
          Delete(Result, n, 1);

        if (Result[n] = '-') then
          Delete(Result, n, 1);

        while (Result[n] in ['0'..'9']) do
          Delete(Result, n, 1);

        if (Result[n] = '.') then
          Delete(Result, n, 1);

        while (Result[n] in ['0'..'9']) do
          Delete(Result, n, 1);

        if (Result[n] in ['d', 'u', 'e', 'f', 'g', 'n', 'm', 'p', 's', 'x']) then
          Delete(Result, n, 1)
        else
          // Not a format string - undo
          Exit(Value);
      end else
        // Not a format string - undo
        Exit(Value);

      // Find next format specifier
      n := PosEx('%', Result, n);
    end;
  end;
end;

// -----------------------------------------------------------------------------

function IsUppercase(const Value: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 1 to Length(Value) do
    if (Value[i].IsLower) then
      Exit(False)
    else
    if (not Result) and (Value[i].IsUpper) then
      Result := True;
end;

function StartsWithUppercase(const Value: string): boolean;
begin
  Result := (Value.Length >= 2) and (Value[1].IsUpper) and (Value[2].IsLower);
end;

// -----------------------------------------------------------------------------

function ComposeSkinName(const Name: string; const Filename: string = ''; Index: integer = -1): string;
begin
  Result := Name;
  if (Filename <> '') then
//    Result := Result +'@' + TokenizePath(Filename);
    Result := Result +'@' + Filename;
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
//    Filename := ExpandEnvironmentVariable(Filename);
    Filename := Filename;
  end else
  begin
    Name := Value;
    Filename := '';
    Index := -1;
  end;
end;

// -----------------------------------------------------------------------------

end.
