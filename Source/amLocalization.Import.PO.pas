unit amLocalization.Import.PO;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

// -----------------------------------------------------------------------------
// Import of PO files
//
// PO files are the translation file format of the GNU Gettext software translation system.
//
// References:
// http://pology.nedohodnik.net/doc/user/en_US/ch-poformat.html
// https://www.gnu.org/software/gettext/manual/html_node/PO-Files.html
// -----------------------------------------------------------------------------

uses
  Classes,
  SysUtils,
  amLocalization.Model;

// -----------------------------------------------------------------------------
//
// TLocalizerPOImport
//
// -----------------------------------------------------------------------------
type
  TLocalizerPOImport = class
  private type
    TTermElement = (teNone, teSourceReference, teComment, teTranslatorComment, teTermContext, teTermID, teTermValue, teIgnored);
    TTermElements = set of TTermElement;
  private
    FProject: TLocalizerProject;
    FLanguage: TTranslationLanguage;
    FCurrentTermElement: TTermElement;
    FTermElements: TTermElements;
    FSourceReference: string;
    FComment: string;
    FTranslatorComment: string;
    FTermContext: string;
    FTermID: string;
    FTermValue: string;
    FCountFuzzy: integer;
    FCountNotFound: integer;
    FCountExact: integer;
    FCountImported: integer;
    FCountSkipped: integer;
  protected
    procedure ProcessLine(const Line: string);
    procedure ProcessTerm;
    function Unescape(const Value: string): string;
  public
    constructor Create(AProject: TLocalizerProject);
    destructor Destroy; override;

    procedure ImportFromFile(const AFilename: string; Language: TTranslationLanguage); overload;
    procedure ImportFromFile(const AFilename: string; AEncoding: TEncoding; Language: TTranslationLanguage); overload;
    procedure ImportFromStream(AStream: TStream; Language: TTranslationLanguage); overload;
    procedure ImportFromStream(AStream: TStream; AEncoding: TEncoding; Language: TTranslationLanguage); overload;

    property CountImported: integer read FCountImported write FCountImported;
    property CountExact: integer read FCountExact write FCountExact;
    property CountFuzzy: integer read FCountFuzzy write FCountFuzzy;
    property CountNotFound: integer read FCountNotFound write FCountNotFound;
    property CountSkipped: integer read FCountSkipped write FCountSkipped;
  end;

resourcestring
  sFileFilterPO = 'GNU GetText PO files (*.po)|*.po';

implementation

uses
  Character,
  StrUtils,
  amLocalization.Utils;

// -----------------------------------------------------------------------------
//
// TLocalizerPOImport
//
// -----------------------------------------------------------------------------
constructor TLocalizerPOImport.Create(AProject: TLocalizerProject);
begin
  inherited Create;
  FProject := AProject;
end;

destructor TLocalizerPOImport.Destroy;
begin

  inherited;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerPOImport.ImportFromFile(const AFilename: string; Language: TTranslationLanguage);
begin
  ImportFromFile(AFilename, TEncoding.UTF8, Language);
end;

procedure TLocalizerPOImport.ImportFromFile(const AFilename: string; AEncoding: TEncoding; Language: TTranslationLanguage);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFilename, fmOpenRead);
  try
    ImportFromStream(Stream, AEncoding, Language);
  finally
    Stream.Free;
  end;
end;

procedure TLocalizerPOImport.ImportFromStream(AStream: TStream; Language: TTranslationLanguage);
begin
  ImportFromStream(AStream, TEncoding.UTF8, Language);
end;

procedure TLocalizerPOImport.ImportFromStream(AStream: TStream; AEncoding: TEncoding; Language: TTranslationLanguage);
var
  Reader: TStreamReader;
begin
  Reader := TStreamReader.Create(AStream, AEncoding, True, 256);
  try

    FLanguage := Language;
    FCurrentTermElement := teNone;
    FTermElements := [];

    while (not Reader.EndOfStream) do
      ProcessLine(Reader.ReadLine);

  finally
    Reader.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerPOImport.ProcessLine(const Line: string);
var
  Value: string;
begin
  Value := Line.Trim;

  if (Value.StartsWith('#')) then
  begin
    // Comment
    ProcessTerm;

    // Source reference
    if (Value[2] = ':') then
    begin
      FSourceReference := Copy(Value, 3, MaxInt).TrimLeft;
      Include(FTermElements, teSourceReference);
      FCurrentTermElement := teNone;
    end else
    if (Value[2] = '.') then
    begin
      Value := Copy(Value, 3, MaxInt).TrimLeft;
      if (FCurrentTermElement = teComment) then
        FComment := FComment + #13 + Value
      else
        FComment := Value;
      FCurrentTermElement := teComment;
      Include(FTermElements, teComment);
    end else
    if (Value[2].IsWhiteSpace) then
    begin
      Value := Copy(Value, 3, MaxInt).TrimLeft;
      if (FCurrentTermElement = teTranslatorComment) then
        FTranslatorComment := FTranslatorComment + #13 + Value
      else
        FTranslatorComment := Value;
      FCurrentTermElement := teTranslatorComment;
      Include(FTermElements, teTranslatorComment);
    end else
      FCurrentTermElement := teIgnored;
    Exit;
  end;

  if (Value.StartsWith('"')) then
  begin
    // Continuation of previous string
    if (FCurrentTermElement in [teNone, teIgnored]) then
      Exit; // This is actually an error for teNone

    Value := Unescape(Value);

    case FCurrentTermElement of
      teTermID:
        FTermID := FTermID + Value;

      teTermValue:
        FTermValue := FTermValue + Value;

      teTermContext:
        FTermContext := FTermContext + Value;
    end;
    Exit;
  end;

  if (Value.StartsWith('msgctxt')) then
  begin
    ProcessTerm;

    Delete(Value, 1, Length('msgid'));
    Value := Value.TrimLeft;

    FTermContext := Unescape(Value);
    FCurrentTermElement := teTermContext;
    Include(FTermElements, teTermContext);
    Exit;
  end;

  if (Value.StartsWith('msgid')) then
  begin
    ProcessTerm;

    Delete(Value, 1, Length('msgid'));
    // Plural forms are not supported
    if (Value.StartsWith('_plural')) then
    begin
      FCurrentTermElement := teIgnored;
      Exit;
    end;

    Value := Value.TrimLeft;

    FTermID := Unescape(Value);
    FCurrentTermElement := teTermID;
    Include(FTermElements, teTermID);
    Exit;
  end;

  if (Value.StartsWith('msgstr')) then
  begin
    Delete(Value, 1, Length('msgstr'));
    // Plural forms are not supported
    if (Value[1] = '[') then
    begin
      FCurrentTermElement := teIgnored;
      Exit;
    end;

    Value := Value.TrimLeft;

    FTermValue := Unescape(Value);
    Include(FTermElements, teTermValue);
    Exit;
  end;
end;

type
  TMatchness = (MatchNone, MatchSanitized, MatchSame, MatchExact);


procedure TLocalizerPOImport.ProcessTerm;
var
  MatchProp: TLocalizerProperty;
  Matchness: TMatchness;
begin
  if (not (teTermID in FTermElements)) then
    Exit;

  // Try to locate term in project

  (*
  if (teTermContext in FTermElements) then
  begin
    FProject.Traverse(
      function(Item: TLocalizerModule): boolean
      begin
      end);
  end;
  *)

  // TODO : Use project lookup index

  // No point in searching for the property if we have no value
  if (teTermValue in FTermElements) and (FTermValue <> '') and (FTermID <> '') then
  begin
    MatchProp := nil;
    Matchness := MatchNone;
    FProject.Traverse(
      function(Prop: TLocalizerProperty): boolean
      var
        NewMatchness: TMatchness;
      begin
        if (Prop.IsUnused) or (Prop.EffectiveStatus <> ItemStatusTranslate) then
          Exit(True);

        if (Prop.Value = FTermID) then
          NewMatchness := MatchExact
        else
        if (Matchness >= MatchSame) then
          Exit(True)
        else
        if (AnsiSameText(Prop.Value, FTermID)) then
          NewMatchness := MatchSame
        else
        if (Matchness >= MatchSanitized) then
          Exit(True)
        else
        if (AnsiSameText(SanitizeText(Prop.Value), SanitizeText(FTermID))) then
          NewMatchness := MatchSanitized
        else
          Exit(True);
        if (NewMatchness > Matchness) then
        begin
          MatchProp := Prop;
          Matchness := NewMatchness;
        end;
        Result := (Matchness <> MatchExact);
      end);

    if (MatchProp <> nil) then
    begin
      Inc(FCountImported);
      if (Matchness = MatchExact) then
        Inc(FCountExact)
      else
        Inc(FCountFuzzy);

      if (Matchness = MatchSanitized) then
        FTermValue := MakeAlike(MatchProp.Value, FTermValue);

      MatchProp.TranslatedValue[FLanguage] := FTermValue;
    end else
      Inc(FCountNotFound);
  end else
    Inc(FCountSkipped);

  FTermElements := [];
  FCurrentTermElement := teNone;
end;

// -----------------------------------------------------------------------------

function TLocalizerPOImport.Unescape(const Value: string): string;
var
  n: integer;
  i: integer;
  v: integer;
begin
  Result := Value;

  if (Result.IsEmpty) then
    Exit;

  // Do not use SysUtils.DeQuotedString. It is broken. For example it reuces "\"" to \
  if (Length(Result) >= 2) and (Result[1] = '"') and (Result[Length(Result)] = '"') then
    Result := Copy(Result, 2, Length(Result)-2);

  if (Result.IsEmpty) then
    Exit;

  n := 1;
  while (True) do
  begin
    n := PosEx('\', Result, n);
    if (n = 0) then
      break;

    // Remove escape
    Delete(Result, n, 1);
    if (n > Length(Result)) then
      break; // Invalid escape. Bail (it's an error)

    // Process escaped char
    case Result[n] of
      'a': Result[n] := #7;
      'b': Result[n] := #8;
      't': Result[n] := #9;
      'n': Result[n] := #10;
      'v': Result[n] := #11;
      'f': Result[n] := #12;
      'r': Result[n] := #13;
      'e': Result[n] := #27;
      '0'..'9': // Octal
        begin
          v := 0;
          i := 3; // Up to three digits
          while (i > 0) and (n <= Length(Result)) and (AnsiChar(Result[n]) in ['0'..'9']) do
          begin
            v := v * 8 + Ord(Result[n])-Ord('0');
            Dec(i);
            Delete(Result, n, 1);
          end;
          Result[n] := Chr(v);
        end;
      'x', 'u': // Hex/Unicode
        begin
          if (Length(Result)-n < 2) then
            raise Exception.CreateFmt('Invalid octal number in "%s"', [Value]);
          Inc(n);
          v := 0;
          i := 4; // Do not support more than 4 hex digits
          while (i > 0) and (n <= Length(Result)) and (AnsiChar(Result[n].ToUpper) in ['0'..'9', 'A'..'F']) do
          begin
            if (AnsiChar(Result[n]) in ['0'..'9']) then
              v := v * 16 + Ord(Result[n])-Ord('0')
            else
              v := v * 16 + Ord(Result[n].ToUpper)-Ord('A')+10;
            Dec(i);
            Delete(Result, n, 1);
          end;
          Dec(n);
          Result[n] := Chr(v);
        end;
    end;
    Inc(n);
  end;
end;

end.
