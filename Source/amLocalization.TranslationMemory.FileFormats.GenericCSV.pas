unit amLocalization.TranslationMemory.FileFormats.GenericCSV;

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
// TTranslationMemoryFileFormatGenericCSV
//
// -----------------------------------------------------------------------------
type
  TTranslationMemoryFileFormatGenericCSV = class(TTranslationMemoryFileFormat)
  private
  protected
    function DoLoadFromStream(Stream: TStream; const Progress: IProgress; DetailedProgress: boolean; Translations: TTranslationMemoryFileFormat.TTranslations;
      Languages: TTranslationMemoryFileFormat.TLanguageFields; Merge: boolean; var SourceLanguage: string): boolean; override;
  public
    class constructor Create;

    procedure SaveToStream(Stream: TStream); override;

    class function FileFormatFileDescription: string; override;
    class function FileFormatFileType: string; override;
    class function FileFormatCapabilities: TFileFormatCapabilities; override;
  end;


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
// TTranslationMemoryFileFormatGenericCSV
//
// -----------------------------------------------------------------------------
class constructor TTranslationMemoryFileFormatGenericCSV.Create;
begin
  RegisterFileFormat(TTranslationMemoryFileFormatGenericCSV);
end;

// -----------------------------------------------------------------------------

class function TTranslationMemoryFileFormatGenericCSV.FileFormatCapabilities: TFileFormatCapabilities;
begin
  Result := [ffcLoad];
end;

class function TTranslationMemoryFileFormatGenericCSV.FileFormatFileDescription: string;
resourcestring
  sFileFormatCSVDescription = 'Generic CSV';
begin
  Result := sFileFormatCSVDescription;
end;

class function TTranslationMemoryFileFormatGenericCSV.FileFormatFileType: string;
begin
  Result := 'csv';
end;

// -----------------------------------------------------------------------------

function TTranslationMemoryFileFormatGenericCSV.DoLoadFromStream(Stream: TStream; const Progress: IProgress; DetailedProgress: boolean;
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
    end else
    // If we're merging then source language should be one of the existing languages
    if (SourceLanguage = '') then
      SourceLanguage := Language;
  end;

var
  Settings: TCsvSettings;
  Encoding: TEncoding;
  TextParserCSV: TTextParserCSV;
  ProgressStream: TStream;
  TermRow: TStringDynArray;
  Term: TTerm;
  Terms: TTerms;
  Fields: array of TField;
  i: integer;
resourcestring
  sUnknownLanguage = 'Unknown language: "%s"';
begin
  Settings := TCsvSettings.Default;
  Settings.Codepage := DetectCodePage(Stream);// CP_UTF8;
  Settings.DelimiterChar := ';';

  Stream.Position := 0;

  if (DetailedProgress) then
    ProgressStream := TProgressStream.Create(Stream, Progress)
  else
    ProgressStream := Stream;
  try
    if (DetailedProgress) then
      Progress.UpdateMessage(sTranslationMemoryReadingTerms);

    Encoding := TEncoding.GetEncoding(Settings.Codepage);
    try
      TextParserCSV := TTextParserCSV.Create(Settings, ProgressStream, Encoding);
      try

        while (not TextParserCSV.EndOfData) do
        begin
          if (DetailedProgress) then
            Progress.AdvanceProgress
          else
            Progress.ProcessMessages;

          TermRow := TextParserCSV.ReadRow;

          // First row contains language list
          if (TextParserCSV.RowCount = 1) then
          begin
            if (Length(TermRow) < 2) then
              Exit(False);

            SetLength(Fields, Length(TermRow));

            SourceLanguage := '';
            for i := 0 to Length(TermRow)-1 do
            begin
              Fields[i] := GetField(TermRow[i]);
              if (Fields[i] = nil) then
              begin
                MessageDlg(Format(sUnknownLanguage, [TermRow[i]]), mtWarning, [mbOK], 0);
                Exit(False);
              end;
            end;

            // Default source language is the first one
            if (SourceLanguage = '') then
              SourceLanguage := TermRow[0];

            continue;
          end;

          if (Length(TermRow) < 2) then
            continue;

          Terms := TTerms.Create;
          Translations.Add(Terms);

          for i := 0 to Length(TermRow)-1 do
          begin
            if (i >= Length(Fields)) then
              break;
            Term.Field := Fields[i];
            Term.Value := TermRow[i];
            Terms.Add(Term);
          end;
        end;

      finally
        TextParserCSV.Free;
      end;
    finally
      Encoding.Free;
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

procedure TTranslationMemoryFileFormatGenericCSV.SaveToStream(Stream: TStream);
begin
end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

initialization
  // Ensure reference to class so class constructor gets called
  TTranslationMemoryFileFormatGenericCSV.ClassName;
end.
