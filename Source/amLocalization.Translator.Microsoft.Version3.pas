unit amLocalization.Translator.Microsoft.Version3;

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
  IPPeerClient, REST.Client, Data.Bind.Components, Data.Bind.ObjectScope,

  amLocale,
  amLocalization.Model,
  amLocalization.Translator;

type
  ITranslationProviderMicrosoftV3 = interface
    ['{BCB967CF-9D86-404E-824F-3952F31B4AEC}']
    function ValidateAPIKey(const APIKey: string; var ErrorMessage: string): boolean;
  end;

// -----------------------------------------------------------------------------
//
// TTranslationProviderMicrosoftV3
//
// -----------------------------------------------------------------------------
type
  TTranslationProviderMicrosoftV3 = class(TDataModule, IUnknown, ITranslationProvider, ITranslationProviderMicrosoftV3)
    RESTRequestTranslate: TRESTRequest;
    RESTResponseResult: TRESTResponse;
    RESTClient: TRESTClient;
    RESTRequestLanguages: TRESTRequest;
    RESTRequestValidateAPIKey: TRESTRequest;
  private
    FRefCount: integer;
  protected
    // IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    // ITranslationProvider
    function BeginLookup(SourceLanguage, TargetLanguage: TLocaleItem): boolean;
    function Lookup(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLocaleItem; Translations: TStrings): boolean;
    procedure EndLookup;
    function GetServiceName: string;

    // ITranslationProviderMicrosoftV3
    function ValidateAPIKey(const APIKey: string; var ErrorMessage: string): boolean;
  public
  end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  SyncObjs,
  System.json,
  Rest.Types,
  System.json.Types,
  System.json.Writers,
  Dialogs,
  amLocalization.Utils,
  amLocalization.Settings;


{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

resourcestring
  sTranslatorNameMS = 'Microsoft Translation Service';

// -----------------------------------------------------------------------------
//
// TTranslationProviderMicrosoftV3
//
// -----------------------------------------------------------------------------
function TTranslationProviderMicrosoftV3._AddRef: Integer;
begin
  Result := TInterlocked.Increment(FRefCount);
end;

function TTranslationProviderMicrosoftV3._Release: Integer;
begin
  Result := TInterlocked.Decrement(FRefCount);
  if (Result = 0) then
    Free;
end;

function TTranslationProviderMicrosoftV3.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

// -----------------------------------------------------------------------------

function TTranslationProviderMicrosoftV3.BeginLookup(SourceLanguage, TargetLanguage: TLocaleItem): boolean;
resourcestring
  sPrompMicrosoftV3APIKey = 'You must register an API key before the Microsoft Translator v3 service can be used.';
begin
  if (TranslationManagerSettings.Translators.MicrosoftV3.APIKey = '') then
  begin
    ShowMessage(sPrompMicrosoftV3APIKey);
    Exit(False);
  end else
    RESTClient.Params[0].Value := TranslationManagerSettings.Translators.MicrosoftV3.APIKey;

  Result := True;
end;

procedure TTranslationProviderMicrosoftV3.EndLookup;
begin
//
end;

function TTranslationProviderMicrosoftV3.GetServiceName: string;
begin
  Result := sTranslatorNameMS;
end;

function TTranslationProviderMicrosoftV3.Lookup(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLocaleItem; Translations: TStrings): boolean;
var
  JsonResultArray: TJsonArray;
  JsonTranslationItem: TJsonObject;
  JsonTranslationItemArray: TJsonArray;
  JsonError: TJsonObject;
  Msg: string;
  TargetValue: string;
begin
  RESTRequestTranslate.Params[0].Value := SourceLanguage.LocaleSName;
  RESTRequestTranslate.Params[1].Value := TargetLanguage.LocaleSName;

  RESTRequestTranslate.Body.ClearBody;
  RESTRequestTranslate.Body.JSONWriter.WriteStartArray;
  RESTRequestTranslate.Body.JSONWriter.WriteStartObject;
  RESTRequestTranslate.Body.JSONWriter.WritePropertyName('Text');
  RESTRequestTranslate.Body.JSONWriter.WriteValue(SanitizeText(Prop.Value, False));
  RESTRequestTranslate.Body.JSONWriter.WriteEndObject;
  RESTRequestTranslate.Body.JSONWriter.WriteEndArray;

  // Call web service
  RESTRequestTranslate.Execute;

  // Get result
  if (RESTResponseResult.JSONValue.TryGetValue<TJsonObject>('error', JsonError)) then
  begin
    Msg := JsonError.GetValue<string>('message');
    raise Exception.CreateFmt('REST call failed'#13'Error message: %s'#13#13'%s', [Msg, RESTResponseResult.Content]);
    TargetValue := JsonError.Value;

    Result := False;
  end else
  begin
    // API key is valid - Save it if we haven't already
    if (TranslationManagerSettings.Translators.MicrosoftV3.APIKey = '') then
      TranslationManagerSettings.Translators.MicrosoftV3.APIKey := RESTClient.Params[0].Value;

    JsonResultArray := RESTResponseResult.JSONValue as TJsonArray;
    JsonTranslationItem := JsonResultArray.items[0] as TJsonObject;
    JsonTranslationItemArray := JsonTranslationItem.GetValue('translations') as TJsonArray;
    TargetValue :=  JsonTranslationItemArray.Items[0].GetValue<string>('text');

    // Service does not explicitly state if no translation was found but instead just returns Target=Source
    Result := (not AnsiSameText(Prop.Value, TargetValue));

    if (Result) then
      Translations.Add(TargetValue);
  end;
end;

// -----------------------------------------------------------------------------

function TTranslationProviderMicrosoftV3.ValidateAPIKey(const APIKey: string; var ErrorMessage: string): boolean;
var
  JsonError: TJsonObject;
begin
  // Call web service
  RESTClient.Params[0].Value := APIKey;

  RESTRequestValidateAPIKey.Body.ClearBody;
  RESTRequestValidateAPIKey.Body.JSONWriter.WriteStartArray;
  RESTRequestValidateAPIKey.Body.JSONWriter.WriteStartObject;
  RESTRequestValidateAPIKey.Body.JSONWriter.WritePropertyName('Text');
  RESTRequestValidateAPIKey.Body.JSONWriter.WriteValue('hello world');
  RESTRequestValidateAPIKey.Body.JSONWriter.WriteEndObject;
  RESTRequestValidateAPIKey.Body.JSONWriter.WriteEndArray;

  RESTRequestValidateAPIKey.Execute;

  // Get result
  if (RESTResponseResult.JSONValue.TryGetValue<TJsonObject>('error', JsonError)) then
  begin
    ErrorMessage := JsonError.GetValue<string>('message');
    Exit(False);
  end;

  ErrorMessage := '';
  Result := True;
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

var
  ProviderHandle: integer = -1;

initialization
  ProviderHandle := TranslationProviderRegistry.RegisterProvider(sTranslatorNameMS,
    function(): ITranslationProvider
    begin
      Result := TTranslationProviderMicrosoftV3.Create(nil);
    end);

finalization
  TranslationProviderRegistry.UnregisterProvider(ProviderHandle);
end.
