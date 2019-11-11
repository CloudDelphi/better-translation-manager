unit amLocalization.Utils;

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
//              Restart semaphore
//
// -----------------------------------------------------------------------------
var
  sRestartSemaphoreName: string = 'Local\amTM.Restart';

function AcquireRestartSemaphore(Timeout: integer = 60000): boolean;
procedure ReleaseRestartSemaphore;

// -----------------------------------------------------------------------------
//
//              Skin
//
// -----------------------------------------------------------------------------
function ComposeSkinName(const Name: string; const Filename: string = ''; Index: integer = -1): string;
procedure DecomposeSkinName(const Value: string; var Name, Filename: string; var Index: integer);


// -----------------------------------------------------------------------------
//
//              Captions and Format strings
//
// -----------------------------------------------------------------------------
// Removes hotkeys and format specifiers
type
  TSanitizeKind = (skAccelerator, skFormat, skEnding, skSurround, skTrim);
  TSanitizeKinds = set of TSanitizeKind;

function SanitizeText(const Value: string; ReduceToNothing: boolean = True): string; overload;
function SanitizeText(const Value: string; Kind: TSanitizeKinds;  ReduceToNothing: boolean = True): string; overload;

// Replacement for menus.StripHotKey
// Does not allow multiple hotkeys. E.g.: "&This && &that"
// Does not allow illegal hotkeys. E.g.: "& "
function StripAccelerator(const Value: string): string;
function HasAccelerator(const Value: string): boolean;
// Escapes &. Assumes Value doesn't contain real accelerators
function EscapeAccelerators(const Value: string): string;
// Adds &. Assumes any existing & are escaped
function AddAccelerator(const Value: string; Default: Char = #0): string;

const
  cAcceleratorPrefix = '&'; // From menus.pas

// -----------------------------------------------------------------------------
//
//              Misc
//
// -----------------------------------------------------------------------------
function IsUpperCase(const Value: string): boolean;
function IsLowerCase(const Value: string): boolean;
function IsTitleCase(const Value: string; IgnoreAccelerator: boolean = False): boolean;
function IsSentenceCase(const Value: string; IgnoreAccelerator: boolean = False): boolean;
function StartsWithUppercase(const Value: string): boolean;

function MakeStartWithUppercase(const Value: string): string; // Note: Only modifies first letter
function MakeTitleCase(const Value: string): string;
function MakeSentenceCase(const Value: string): string;

type
  TMakeAlikeRule = (EqualizeCase, EqualizeEnding, EqualizeAccelerators, EqualizeSurround);
  TMakeAlikeRules = set of TMakeAlikeRule;

function MakeAlike(const SourceValue, Value: string): string; overload;
function MakeAlike(const SourceValue, Value: string; Rules: TMakeAlikeRules): string; overload;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  System.Character,
  Windows,
  Menus,
  SyncObjs,
  StrUtils,
  SysUtils,
  amLocalization.Settings;

type
  TSurroundPair = record
    StartSurround: Char;
    EndSurround: Char;
  end;

const
  SurroundPairs: array[0..5] of TSurroundPair = (
    (StartSurround: '('; EndSurround: ')'),
    (StartSurround: '['; EndSurround: ']'),
    (StartSurround: '{'; EndSurround: '}'),
    (StartSurround: '"'; EndSurround: '"'),
    (StartSurround: ''''; EndSurround: ''''),
    (StartSurround: '<'; EndSurround: '>')
    );

const
  TextEndings: array[0..2] of string = (
    ':',
    ';',
    '...'
    );

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
      Inc(i);
    end;
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

function AddAccelerator(const Value: string; Default: Char): string;
var
  i: integer;
begin
  if (Default <> #0) then
  begin
    i := Pos(Default, Value);
    if (i <> 0) then
      Exit(Copy(Value, 1, i-1)+cAcceleratorPrefix+Copy(Value, i, MaxInt));
  end;

  for i := 1 to Length(Value) do
    if (Value[i].IsLetterOrDigit) and ((i = 1) or (Value[i-1] <> cAcceleratorPrefix)) then
      Exit(Copy(Value, 1, i-1)+cAcceleratorPrefix+Copy(Value, i, MaxInt));

  Result := Value;
end;

// -----------------------------------------------------------------------------

function IsAnsi(c: char): boolean; Inline;
begin
  Result := (c <= #$FF);
end;

function SanitizeText(const Value: string; ReduceToNothing: boolean = True): string; overload;
begin
  Result := SanitizeText(Value, [Low(TSanitizeKind)..High(TSanitizeKind)], ReduceToNothing);
end;

function SanitizeText(const Value: string; Kind: TSanitizeKinds; ReduceToNothing: boolean): string;
var
  n: integer;
  SurroundPair: TSurroundPair;
  s: string;
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
      if (IsAnsi(Result[n])) and (AnsiChar(Result[n]) in ['0'..'9', '-', '.', 'd', 'u', 'e', 'f', 'g', 'n', 'm', 'p', 's', 'x']) then
      begin
        Result[n-1] := ' '; // Replace %... with space

        // Remove chars until end of format specifier
        while (Result[n].IsDigit) do
          Delete(Result, n, 1);

        if (Result[n] = ':') then
          Delete(Result, n, 1);

        if (Result[n] = '-') then
          Delete(Result, n, 1);

        while (Result[n].IsDigit) do
          Delete(Result, n, 1);

        if (Result[n] = '.') then
          Delete(Result, n, 1);

        while (Result[n].IsDigit) do
          Delete(Result, n, 1);

        if (IsAnsi(Result[n])) and (AnsiChar(Result[n]) in ['d', 'u', 'e', 'f', 'g', 'n', 'm', 'p', 's', 'x']) then
          Delete(Result, n, 1)
        else
        begin
          // Not a format string - undo
          Result := Value;
          break;
        end;
      end else
      begin
        // Not a format string - undo
        Result := Value;
        break;
      end;

      // Find next format specifier
      n := PosEx('%', Result, n);
    end;
  end;

  if (skSurround in Kind) and (Length(Result) > 2) then
  begin
    for SurroundPair in SurroundPairs do
      if (Result.StartsWith(SurroundPair.StartSurround)) and (Result.EndsWith(SurroundPair.EndSurround)) then
      begin
        SetLength(Result, Length(Result)-1);
        Delete(Result, 1, 1);
        Exclude(Kind, skEnding);
        break;
      end;
  end;

  if (skEnding in Kind) then
  begin
    for s in TextEndings do
      if (Result.EndsWith(s)) then
      begin
        SetLength(Result, Length(Result)-Length(s));
        break;
      end;
  end;

  if (skTrim in Kind) then
    Result := Result.Trim;

  if (not ReduceToNothing) and ((Result.IsEmpty) or ((not(skTrim in Kind)) and (Result.Trim.IsEmpty))) then
    Exit(Value);
end;

// -----------------------------------------------------------------------------

function IsUpperCase(const Value: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 1 to Length(Value) do
    if (Value[i].IsLetter) then
    begin
      if (Value[i].IsLower) then
        Exit(False)
      else
      if (not Result) and (Value[i].IsUpper) then
        Result := True;
    end;
end;

function IsLowerCase(const Value: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 1 to Length(Value) do
    if (Value[i].IsLetter) then
    begin
      if (Value[i].IsUpper) then
        Exit(False)
      else
      if (not Result) and (Value[i].IsLower) then
        Result := True;
    end;
end;

function IsTitleCase(const Value: string; IgnoreAccelerator: boolean): boolean;
var
  i: integer;
  InWord: boolean;
  HasSpace: boolean;
begin
  Result := False;
  InWord := False;
  HasSpace := False;
  for i := 1 to Length(Value) do
    if (Value[i].IsLetter) then
    begin
      if (InWord) then
      begin
        if (not Value[i].IsLower) then
          Exit(False);
        // We now have a word with both Upper and a Lowercase
        Result := HasSpace;
      end else
      begin
        if (not Value[i].IsUpper) then
          Exit(False);
        InWord := True;
      end;
    end else
    if (not IgnoreAccelerator) or (Value[i] <> cAcceleratorPrefix) or ((i > 1) and (Value[i-1] = cAcceleratorPrefix)) or (Value[i+1] = cAcceleratorPrefix) then
    begin
      InWord := False;
      HasSpace := True;
    end;
end;

function IsSentenceCase(const Value: string; IgnoreAccelerator: boolean): boolean;
var
  i: integer;
  InSentence, InWord: boolean;
begin
  Result := False;
  InWord := False;
  InSentence := False;
  for i := 1 to Length(Value) do
    if (Value[i].IsLetter) then
    begin
      if (InWord) then
      begin
        if (Value[i].IsUpper) then
          Exit(False); // Uppercase inside word
        // We now have both an Upper and a Lowercase
        Result := True;
      end else
      begin
        if (InSentence) then
        begin
          if (Value[i].IsUpper) then
            Exit(False); // Uppercase at start of word, inside sentence
        end else
        begin
          if (Value[i].IsLower) then
            Exit(False); // Lowercase at start of sentence
        end;
        InSentence := True;
        InWord := True;
      end;
    end else
    if (not IgnoreAccelerator) or (Value[i] <> cAcceleratorPrefix) or ((i > 1) and (Value[i-1] = cAcceleratorPrefix)) or (Value[i+1] = cAcceleratorPrefix) then
    begin
      InWord := False;
      if (Ord(Value[i]) <= 255) and (AnsiChar(Value[i]) in ['.', '!', '?', ':', ';']) then
        InSentence := False;
    end;
end;

function StartsWithUppercase(const Value: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 1 to Length(Value)-1 do
    if (Value[i].IsLetter) and (Value[i+1].IsLetter) and (Value[i].IsUpper) and (Value[i+1].IsLower) then
      Exit(True);
end;

function MakeStartWithUppercase(const Value: string): string;
var
  i: integer;
begin
  Result := Value;
  for i := 1 to Length(Result)-1 do
    if (Result[i].IsLetter) and (Result[i+1].IsLetter) then
    begin
      Result[i] := Result[i].ToUpper;
      Exit;
    end;
end;

function MakeTitleCase(const Value: string): string;
var
  i: integer;
  InWord: boolean;
begin
  Result := Value;
  InWord := False;
  for i := 1 to Length(Result) do
    if (Result[i].IsLetter) then
    begin
      if (InWord) then
        Result[i] := Result[i].ToLower
      else
      begin
        Result[i] := Result[i].ToUpper;
        InWord := True;
      end;
    end else
      InWord := False;
end;

function MakeSentenceCase(const Value: string): string;
var
  i: integer;
  InSentence: boolean;
begin
  Result := Value;
  InSentence := False;
  for i := 1 to Length(Result) do
    if (Result[i].IsLetter) then
    begin
      if (InSentence) then
        Result[i] := Result[i].ToLower // Lowercase inside sentence
      else
        Result[i] := Result[i].ToUpper; // Uppercase at start of sentence

      InSentence := True;
    end else
    // if (Result[i].IsPunctuation) then
    if (Ord(Result[i]) <= 255) and (AnsiChar(Result[i]) in ['.', '!', '?', ':', ';']) then
      InSentence := False;
end;

function MakeAlike(const SourceValue, Value: string): string; overload;
begin
  Result := MakeAlike(SourceValue, Value, TranslationManagerSettings.Editor.EqualizerRules);
end;

function MakeAlike(const SourceValue, Value: string; Rules: TMakeAlikeRules): string;
var
  SourceHasAccelerator: boolean;
  IgnoreAccelerator: boolean;
  Accelerator: string;
  SurroundPair: TSurroundPair;
  s: string;
begin
  Rules := Rules * TranslationManagerSettings.Editor.EqualizerRules;
  if (Rules = []) then
    Exit(Value);

  if (Value.IsEmpty) then
    Exit(Value);

  Result := Value;

  if (Rules * [EqualizeAccelerators, EqualizeCase] <> []) then
  begin
    SourceHasAccelerator := HasAccelerator(SourceValue);
    if (EqualizeAccelerators in Rules) then
      IgnoreAccelerator := SourceHasAccelerator
    else
      IgnoreAccelerator := HasAccelerator(Result);
  end else
  begin
    SourceHasAccelerator := False;
    IgnoreAccelerator := False;
  end;

  // Handle accelerator keys
  if (EqualizeAccelerators in Rules) then
  begin
    if (SourceHasAccelerator) then
    begin
      if (not HasAccelerator(Result)) then
      begin
        // If source had an accelerator then make sure the target also has one
        Accelerator := Menus.GetHotkey(SourceValue);
        Result := AddAccelerator(EscapeAccelerators(Result), Accelerator[1]);
      end;
    end else
    begin
      if (HasAccelerator(Result)) then
        // If source doesn't have an accelerator then make sure the target also doesn't have one
        Result := SanitizeText(Result, [skAccelerator]);
    end;
  end;

  // If source ends with a colon, semicolon or ellipsis the target should also do so
  if (EqualizeEnding in Rules) then
  begin
    for s in TextEndings do
    begin
      if (SourceValue.EndsWith(s)) then
      begin
        if (not Result.EndsWith(s)) and (Result[Length(Result)].IsLetterOrDigit) then
          Result := Result + s;
        break;
      end else
      if (Result.EndsWith(s)) then
      begin
        SetLength(Result, Length(Result)-Length(s));
        break;
      end;
    end;
  end;

  if (EqualizeSurround in Rules) and (Length(SourceValue) > 2) then
  begin
    for SurroundPair in SurroundPairs do
      if (SourceValue.StartsWith(SurroundPair.StartSurround)) and (SourceValue.EndsWith(SurroundPair.EndSurround)) then
      begin
        if (not Result.StartsWith(SurroundPair.StartSurround)) and (not Result.EndsWith(SurroundPair.EndSurround)) then
          Result := SurroundPair.StartSurround + Result + SurroundPair.EndSurround;
        break;
      end;
  end;

  if (EqualizeCase in Rules) then
  begin
    // If source is all UPPERCASE the target should also be so
    if (IsUpperCase(SourceValue)) then
      Result := AnsiUpperCase(Result)
    else
    // If source is all lowercase the target should also be so
    if (IsLowerCase(SourceValue)) then
      Result := AnsiLowerCase(Result)
    else
    // If source is Title Case the target should also be so
    if (IsTitleCase(SourceValue, IgnoreAccelerator)) then
      Result := MakeTitleCase(Result)
    else
    // If source is Sentence case: The target should also be so
    if (IsSentenceCase(SourceValue, IgnoreAccelerator)) then
      Result := MakeSentenceCase(Result)
    else
    // If source starts with an Uppercase letter the target should also do so
    if (StartsWithUppercase(SourceValue)) then
      Result := MakeStartWithUppercase(Result);
  end;
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
//
//              Restart semaphore
//
// -----------------------------------------------------------------------------
var
  FRestartSemaphore: TMutex = nil;

// -----------------------------------------------------------------------------

procedure ReleaseRestartSemaphore;
begin
  if (FRestartSemaphore = nil) then
    exit;
  try
    try
      FRestartSemaphore.Release;
    finally
      FreeAndNil(FRestartSemaphore);
    end;
  except
    // Ignore. It's more important that we terminate properly.
    // Error is probably "Attempt to release mutex not owned by caller" because we failed
    // to acquire the mutex (can happen if other instance hangs during shutdown).
  end;
end;

function AcquireRestartSemaphore(Timeout: integer = 60000): boolean;
begin
  ReleaseRestartSemaphore;

  FRestartSemaphore := TMutex.Create(nil, True, sRestartSemaphoreName);

  if (GetLastError = ERROR_ALREADY_EXISTS) then
    Result := (FRestartSemaphore.WaitFor(Timeout) <> wrTimeout)
  else
    Result := True;
end;

// -----------------------------------------------------------------------------


initialization
finalization
  ReleaseRestartSemaphore;
end.
