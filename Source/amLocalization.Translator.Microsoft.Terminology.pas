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
    function GetServiceName: string;
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

function TTranslationProviderMicrosoftTerminology.GetServiceName: string;
begin
  Result := sTranslatorNameMSTerminology;
end;

function TTranslationProviderMicrosoftTerminology.Lookup(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLocaleItem; Translations: TStrings): boolean;
var
  Sources: TranslationSources;
  ResultMatches: Matches;
  Match: Match2;
  Translation: Translation2;
begin
  SetLength(Sources, 1);
  Sources[0] := TranslationSource.UiStrings;

   // Call web service
  ResultMatches := FTerminology.GetTranslations(Prop.Value, SourceLanguage.LocaleSName, TargetLanguage.LocaleSName,
    SearchStringComparison.CaseInsensitive, SearchOperator.Exact, Sources, True, 10, False, nil);

  // Get result
  Result := False;
  if (Length(ResultMatches) > 0) then
  begin
    for Match in ResultMatches do
    begin
      for Translation in Match.Translations do
      begin
        if (Translations.IndexOf(Translation.TranslatedText) =-1) then
          Translations.Add(Translation.TranslatedText);
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
