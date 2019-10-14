unit amLocalization.Translator.Microsoft.Terminology;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  System.SysUtils, System.Classes,

  amLocale,
  amLocalization.Model,
  amLocalization.Translator,
  amLocalization.Translator.Microsoft.Terminology.SOAP;

// -----------------------------------------------------------------------------
//
// TTranslationProviderMicrosoftTerminology
//
// -----------------------------------------------------------------------------
type
  TTranslationProviderMicrosoftTerminology = class(TInterfacedObject, ITranslationProvider)
  private
    FTerminology: Terminology;
  protected
    // ITranslationProvider
    function BeginLookup(SourceLanguage, TargetLanguage: TLocaleItem): boolean;
    function Lookup(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLocaleItem; Translations: TStrings): boolean;
    procedure EndLookup;
    function GetProviderName: string;
  public
  end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  Dialogs,
  amLocalization.Utils,
  amLocalization.Settings;


resourcestring
  sTranslatorNameMSTerminology = 'Microsoft Terminology Service';

// -----------------------------------------------------------------------------
//
// TTranslationProviderMicrosoftTerminology
//
// -----------------------------------------------------------------------------
function TTranslationProviderMicrosoftTerminology.BeginLookup(SourceLanguage, TargetLanguage: TLocaleItem): boolean;
begin
  FTerminology := GetTerminology;
  Result := True;
end;

procedure TTranslationProviderMicrosoftTerminology.EndLookup;
begin
  FTerminology := nil;
end;

function TTranslationProviderMicrosoftTerminology.GetProviderName: string;
begin
  Result := sTranslatorNameMSTerminology;
end;

function TTranslationProviderMicrosoftTerminology.Lookup(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLocaleItem; Translations: TStrings): boolean;
var
  Sources: TranslationSources;
  ResultMatches: Matches;
  Match: Match2;
  Translation: Translation2;
  SourceValue: string;
  TargetValue: string;
  n: integer;
begin
  SetLength(Sources, 1);
  Sources[0] := TranslationSource.UiStrings;

  // We are searching on the raw UI string since the Terminology Service can handle that.
  // We could also search on the sanitized value but that would produce more mismatches
  // and doesn't seem necessary.
  SourceValue := Prop.Value;

   // Call web service
  ResultMatches := FTerminology.GetTranslations(SourceValue, SourceLanguage.LocaleSName, TargetLanguage.LocaleSName,
    SearchStringComparison.CaseInsensitive, SearchOperator.Exact, Sources, True,
    TranslationManagerSettings.Translators.MicrosoftTerminology.MaxResult, False, nil);

  // Get result
  Result := False;
  if (Length(ResultMatches) > 0) then
  begin
    for Match in ResultMatches do
    begin
      for Translation in Match.Translations do
      begin
        TargetValue := Translation.TranslatedText;

        if (SourceValue = Match.OriginalText) then
        begin
          // Don't MakeAlike on an exact match

          // Exact match - Insert in front
          if (Translations.Count > 0) then
          begin
            n := Translations.IndexOf(TargetValue);
            if (n = -1) then
              Translations.Insert(0, TargetValue)
            else
              Translations.Move(n, 0);
          end else
            Translations.Add(TargetValue);
        end else
        begin
          TargetValue := MakeAlike(SourceValue, TargetValue);

          // Inefficient due to sequential scan, but we need to maintain returned order
          if (Translations.IndexOf(TargetValue) = -1) then
            Translations.Add(TargetValue);
        end;

        Result := True;
      end;
    end;
  end;
end;

var
  ProviderHandle: integer = -1;

initialization
  ProviderHandle := TranslationProviderRegistry.RegisterProvider(sTranslatorNameMSTerminology,
    function(): ITranslationProvider
    begin
      Result := TTranslationProviderMicrosoftTerminology.Create;
    end);

finalization
  TranslationProviderRegistry.UnregisterProvider(ProviderHandle);
end.
