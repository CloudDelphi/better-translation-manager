﻿unit amLocalization.Provider.Microsoft.Terminology;

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

  amLanguageInfo,
  amLocalization.Model,
  amLocalization.Provider,
  amLocalization.Provider.Microsoft.Terminology.SOAP;

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
    function BeginLookup(SourceLanguage, TargetLanguage: TLanguageItem): boolean;
    function Lookup(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLanguageItem; Translations: TStrings): boolean;
    procedure EndLookup;
    function GetProviderName: string;
  public
  end;

  ELocalizationProviderMicrosoftTerminology = class(ELocalizationProvider);

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  Dialogs,
  Soap.InvokeRegistry,
  Soap.SOAPHTTPTrans,
  amLocalization.Normalization,
  amLocalization.Settings;


resourcestring
  sProviderNameMSTerminology = 'Microsoft Terminology Service';

// -----------------------------------------------------------------------------
//
// TTranslationProviderMicrosoftTerminology
//
// -----------------------------------------------------------------------------
function TTranslationProviderMicrosoftTerminology.BeginLookup(SourceLanguage, TargetLanguage: TLanguageItem): boolean;
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
  Result := sProviderNameMSTerminology;
end;

function TTranslationProviderMicrosoftTerminology.Lookup(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLanguageItem; Translations: TStrings): boolean;
var
  Sources: TranslationSources;
  ResultMatches: Matches;
begin
  SetLength(Sources, 1);
  Sources[0] := TranslationSource.UiStrings;

  // We are searching on the raw UI string since the Terminology Service can handle that.
  // We could also search on the sanitized value but that would produce more mismatches
  // and doesn't seem necessary.
  var SourceValue := Prop.Value;

  // Call web service
  try

    ResultMatches := FTerminology.GetTranslations(SourceValue, SourceLanguage.LocaleName, TargetLanguage.LocaleName,
      SearchStringComparison.CaseInsensitive, SearchOperator.Exact, Sources, True,
      TranslationManagerSettings.Providers.MicrosoftTerminology.MaxResult, False, nil);

  except
    on E: ERemotableException do
    begin
      var Msg := E.Message;
      if (E.FaultDetail <> '') then
        Msg := Msg + #13 + E.FaultDetail;

      Exception.RaiseOuterException(ELocalizationProviderMicrosoftTerminology.CreateFmt('Microsoft Terminology provider lookup failed: %s', [Msg]));
    end;
    on E: ESOAPHTTPException do
      Exception.RaiseOuterException(ELocalizationProviderMicrosoftTerminology.CreateFmt('Microsoft Terminology provider lookup failed: %s', [E.Message]));
  end;

  // Get result
  Result := False;
  if (Length(ResultMatches) > 0) then
  begin
    for var Match in ResultMatches do
    begin
      for var Translation in Match.Translations do
      begin
        var TargetValue := Translation.TranslatedText;

        if (SourceValue = Match.OriginalText) then
        begin
          // Don't MakeAlike on an exact match

          // Exact match - Insert in front
          if (Translations.Count > 0) then
          begin
            var n := Translations.IndexOf(TargetValue);
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
  ProviderHandle := TranslationProviderRegistry.RegisterProvider(sProviderNameMSTerminology,
    function(): ITranslationProvider
    begin
      Result := TTranslationProviderMicrosoftTerminology.Create;
    end);

finalization
  TranslationProviderRegistry.UnregisterProvider(ProviderHandle);
end.
