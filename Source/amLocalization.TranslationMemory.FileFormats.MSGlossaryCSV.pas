unit amLocalization.TranslationMemory.FileFormats.MSGlossaryCSV;

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
  amProgress,
  amLocalization.TranslationMemory.FileFormats;

// -----------------------------------------------------------------------------
//
// TTranslationMemoryFileFormatGlossaryCSV
//
// -----------------------------------------------------------------------------
type
  TTranslationMemoryFileFormatGlossaryCSV = class(TTranslationMemoryFileFormat)
  private
    FTargetLanguage: string;
  protected
    function DoLoadFromStream(Stream: TStream; const Progress: IProgress; DetailedProgress: boolean; Translations: TTranslationMemoryFileFormat.TTranslations;
      Languages: TTranslationMemoryFileFormat.TLanguageFields; Merge: boolean; var SourceLanguage: string): boolean; override;
  public
    class constructor Create;

    function Prepare(const Filename: string): boolean; override;
    procedure SaveToStream(Stream: TStream); override;

    class function FileFormatFileDescription: string; override;
    class function FileFormatFileType: string; override;
    class function FileFormatCapabilities: TFileFormatCapabilities; override;
  end;

  ETranslationMemoryFileFormatMSGlossary = class(ETranslationMemoryFileFormat);

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  Types,
  UITypes,
  SysUtils,
  StrUtils,
  IOUtils,
  Dialogs,
  DB,
  amDataCsvReader,
  amCursorService,
  amLocale,
  amLocalization.TranslationMemory,
  amLocalization.Dialog.TargetLanguage;

// -----------------------------------------------------------------------------
//
// TTranslationMemoryFileFormatGlossaryCSV
//
// -----------------------------------------------------------------------------
class constructor TTranslationMemoryFileFormatGlossaryCSV.Create;
begin
  RegisterFileFormat(TTranslationMemoryFileFormatGlossaryCSV);
end;

// -----------------------------------------------------------------------------

class function TTranslationMemoryFileFormatGlossaryCSV.FileFormatCapabilities: TFileFormatCapabilities;
begin
  Result := [ffcLoad];
end;

class function TTranslationMemoryFileFormatGlossaryCSV.FileFormatFileDescription: string;
resourcestring
  sFileFormatMSGlossaryDescription = 'MS Glossary';
begin
  Result := sFileFormatMSGlossaryDescription;
end;

class function TTranslationMemoryFileFormatGlossaryCSV.FileFormatFileType: string;
begin
  Result := 'csv';
end;

function TTranslationMemoryFileFormatGlossaryCSV.Prepare(const Filename: string): boolean;
var
  n: integer;
  s: string;
  TargetLanguageGuess: string;
  FormTargetLanguage: TFormTargetLanguage;
  LocaleItem: TLocaleItem;
resourcestring
  sSelectTargetLanguage = 'Specify the target language';
begin
  FTargetLanguage := '';
  TargetLanguageGuess := '';

  // Try to deduce target language from filename
  s := TPath.GetFileNameWithoutExtension(Filename);
  n := Pos('-', s);
  if (n > 0) then
    n := PosEx('-', s, n+1);
  if (n > 0) then
  begin
    s := Copy(s, 1, n-1);
    if (TLocaleItems.FindLocaleName(s) <> nil) then
      TargetLanguageGuess := s;
  end;

  // Make sure we have the correct target language
  if (FTargetLanguage = '') then
  begin
    FormTargetLanguage := TFormTargetLanguage.Create(nil);
    try
      LocaleItem := TLocaleItems.FindLocaleName(TargetLanguageGuess);

      if (LocaleItem <> nil) then
        FormTargetLanguage.LanguageID := LocaleItem.Locale;

      if (not FormTargetLanguage.Execute(sSelectTargetLanguage)) then
        Exit(False);

      LocaleItem := TLocaleItems.FindLCID(FormTargetLanguage.LanguageID);
      if (LocaleItem <> nil) then
        FTargetLanguage := LocaleItem.LocaleName;
    finally
      FormTargetLanguage.Free;
    end;
  end;

  Result := True;
end;

// -----------------------------------------------------------------------------

function TTranslationMemoryFileFormatGlossaryCSV.DoLoadFromStream(Stream: TStream; const Progress: IProgress; DetailedProgress: boolean;
  Translations: TTranslationMemoryFileFormat.TTranslations; Languages: TTranslationMemoryFileFormat.TLanguageFields; Merge: boolean;
  var SourceLanguage: string): boolean;

  function GetField(const Language: string): TField;
  var
    LocaleItem: TLocaleItem;
    LanguageName: string;
  begin
    LocaleItem := TLocaleItems.FindLocaleName(Language);
    if (LocaleItem = nil) then
      Exit(nil);

    LanguageName := LocaleItem.LocaleName;

    if (not Languages.TryGetValue(LanguageName, Result)) then
    begin
      Result := TranslationMemory.CreateField(LocaleItem);
      Languages.Add(LanguageName, Result);
    end;
  end;

var
  Settings: TCsvSettings;
  TextParserCSV: TTextParserCSV;
  ProgressStream: TStream;
  TermRow: TStringDynArray;
  Term: TTerm;
  Terms: TTerms;
  SourceField, TargetField: TField;
resourcestring
  sUnknownSourceLanguage = 'Unknown source language: "%s"';
  sUnknownTargetLanguage = 'Unknown target language: "%s"';
begin
  Settings := TCsvSettings.Default;
  Settings.Codepage := CP_UTF8;
  Settings.DelimiterChar := ',';
  Settings.FirstRow := 14; // Row 13 is the header

  SourceLanguage := 'en-US'; // Source language is always en-US

  SourceField := GetField(SourceLanguage);
  if (SourceField = nil) then
    raise ETranslationMemoryFileFormatMSGlossary.CreateFmt(sUnknownSourceLanguage, [SourceLanguage]);

  TargetField := GetField(FTargetLanguage);
  if (TargetField = nil) then
    raise ETranslationMemoryFileFormatMSGlossary.CreateFmt(sUnknownTargetLanguage, [FTargetLanguage]);

  if (DetailedProgress) then
    ProgressStream := TProgressStream.Create(Stream, Progress)
  else
    ProgressStream := Stream;
  try
    if (DetailedProgress) then
      Progress.UpdateMessage(sTranslationMemoryReadingTerms);

    TextParserCSV := TTextParserCSV.Create(Settings, ProgressStream);
    try

      while (not TextParserCSV.EndOfData) do
      begin
        if (DetailedProgress) then
          Progress.AdvanceProgress
        else
          Progress.ProcessMessages;

        TermRow := TextParserCSV.ReadRow;
        if (Length(TermRow) < 3) or (TextParserCSV.RowCount < Settings.FirstRow) then
          continue;

        Terms := TTerms.Create;
        Translations.Add(Terms);

        Term.Field := SourceField;
        Term.Value := TermRow[0];
        Terms.Add(Term);

        Term.Field := TargetField;
        Term.Value := TermRow[2];
        Terms.Add(Term);
      end;

    finally
      TextParserCSV.Free;
    end;
  finally
    if (DetailedProgress) then
      ProgressStream.Free;
  end;


  if (DetailedProgress) then
    Progress.Progress(psEnd, 1, 1);

  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TTranslationMemoryFileFormatGlossaryCSV.SaveToStream(Stream: TStream);
begin
end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

initialization
  // Ensure reference to class so class constructor gets called
  TTranslationMemoryFileFormatGlossaryCSV.ClassName;
end.
