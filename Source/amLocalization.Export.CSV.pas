unit amLocalization.Export.CSV;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  Classes,
  amLocalization.Model;

// -----------------------------------------------------------------------------
//
// TLocalizerCsvWriter
//
// -----------------------------------------------------------------------------
// Aims to write IETF RFC4180 compliant CSV
// https://tools.ietf.org/html/rfc4180
// Exceptions:
// - Uses system default list separator instead of the required comma.
// -----------------------------------------------------------------------------
type
  TLocalizerCsvWriter = class
  private
    FWriter: TTextWriter;
    FOnlyTranslated: boolean;
    FLanguage: TTranslationLanguage;
    FDelimiter: string;
    FTerminator: string;
    FQuote: string;
    FAlwaysQuote: boolean;
    FHeader: boolean;
    FNeedHeader: boolean;
    FNeedDelimiter: boolean;
  protected
    function CsvEscape(const Value: string): string;
    procedure WriteField(const Value: string);
    procedure BeginRow(Project: TLocalizerProject);
    procedure EndRow;
    procedure WriteHeader(AProject: TLocalizerProject);
  public
    constructor Create(AWriter: TTextWriter; AOnlyTranslated: boolean = True; ALanguage: TTranslationLanguage = nil);
    destructor Destroy; override;

    procedure Write(AProject: TLocalizerProject); overload;
    procedure Write(AModule: TLocalizerModule); overload;
    procedure Write(AProp: TLocalizerProperty); overload;

    property Header: boolean read FHeader write FHeader;
    property Delimiter: string read FDelimiter write FDelimiter;
    property Terminator: string read FTerminator write FTerminator;
    property Quote: string read FQuote write FQuote;
    property AlwaysQuote: boolean read FAlwaysQuote write FAlwaysQuote;
  end;

implementation

uses
  SysUtils,
  Windows,
  amLanguageInfo;

// -----------------------------------------------------------------------------
//
// TLocalizerCsvWriter
//
// -----------------------------------------------------------------------------
constructor TLocalizerCsvWriter.Create(AWriter: TTextWriter; AOnlyTranslated: boolean; ALanguage: TTranslationLanguage);
begin
  inherited Create;
  FWriter := AWriter;
  FOnlyTranslated := AOnlyTranslated;
  FLanguage := ALanguage;

  FDelimiter := FormatSettings.ListSeparator;
  FTerminator := sLineBreak;
  FQuote := '"';

  FHeader := True;
  FNeedHeader := True;
end;

destructor TLocalizerCsvWriter.Destroy;
begin

  inherited;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerCsvWriter.BeginRow(Project: TLocalizerProject);
begin
  if (FHeader) and (FNeedHeader) then
    WriteHeader(Project);
end;

procedure TLocalizerCsvWriter.EndRow;
begin
  FWriter.Write(FTerminator);
  FNeedDelimiter := False;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerCsvWriter.WriteField(const Value: string);
begin
  if (FNeedDelimiter) then
    FWriter.Write(FDelimiter);
  FWriter.Write(CsvEscape(Value));
  FNeedDelimiter := True;
end;

// -----------------------------------------------------------------------------

function TLocalizerCsvWriter.CsvEscape(const Value: string): string;
var
  NeedQuotes: boolean;
begin
  Result := Value;
  // We don't need to check for decimal separators since we only process strings
  NeedQuotes := (FAlwaysQuote) or (Result.IndexOfAny([' ', #10, #13]) <> -1) or (Result.StartsWith(' ')) or (Result.EndsWith(' ')) or
    (Result.Contains(FQuote)) or (Result.Contains(FDelimiter));
  Result := Result.Replace(FQuote, FQuote+FQuote, [rfReplaceAll]);
  if (NeedQuotes) then
    Result := FQuote + Result + FQuote;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerCsvWriter.Write(AProject: TLocalizerProject);
begin
  AProject.Traverse(
    function(Module: TLocalizerModule): boolean
    begin
      if (Module.Status = ItemStatusTranslate) then
        Write(Module);
      Result := True;
    end, True); // Must sort or output will be confusing for the user
end;

// -----------------------------------------------------------------------------

procedure TLocalizerCsvWriter.Write(AModule: TLocalizerModule);
begin
  AModule.Traverse(
    function(Prop: TLocalizerProperty): boolean
    begin
      if (Prop.Status = ItemStatusTranslate) then
        Write(Prop);
      Result := True;
    end, True); // Must sort or output will be confusing for the user
end;

// -----------------------------------------------------------------------------

procedure TLocalizerCsvWriter.Write(AProp: TLocalizerProperty);

  procedure WriteTranslation(Language: TTranslationLanguage);
  var
    Translation: TLocalizerTranslation;
    Value: string;
  begin
    if (AProp.Translations.TryGetTranslation(Language, Translation)) and (Translation.IsTranslated) then
      Value := Translation.Value
    else
    if (not FOnlyTranslated) then
      Value := AProp.Value
    else
      Value := '';
    WriteField(Value);
  end;


var
  i: integer;
begin
  BeginRow(AProp.Item.Module.Project);

  WriteField(AProp.Item.Module.Name);
  WriteField(AProp.Item.Name);
  WriteField(AProp.Item.TypeName);
  WriteField(AProp.Name);
  WriteField(AProp.Value);
  if (FLanguage = nil) then
  begin
    for i := 0 to AProp.Item.Module.Project.TranslationLanguages.Count-1 do
      WriteTranslation(AProp.Item.Module.Project.TranslationLanguages[i])
  end else
    WriteTranslation(FLanguage);

  EndRow;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerCsvWriter.WriteHeader(AProject: TLocalizerProject);

  procedure WriteLanguage(Language: TLanguageItem);
  begin
    WriteField(Language.LocaleName);
  end;

var
  i: integer;
begin
  FNeedHeader := False;

  BeginRow(AProject);

  WriteField('Module');
  WriteField('Item');
  WriteField('ItemType');
  WriteField('Property');
  WriteLanguage(AProject.SourceLanguage);
  if (FLanguage = nil) then
  begin
    for i := 0 to AProject.TranslationLanguages.Count-1 do
      WriteLanguage(AProject.TranslationLanguages[i].Language)
  end else
    WriteLanguage(FLanguage.Language);

  EndRow;
end;

// -----------------------------------------------------------------------------

end.
