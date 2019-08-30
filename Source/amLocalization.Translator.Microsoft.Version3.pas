unit amLocalization.Translator.Microsoft.Version3;

interface

uses
  System.SysUtils, System.Classes,
  IPPeerClient, REST.Client, Data.Bind.Components, Data.Bind.ObjectScope,

  amLocale,
  amLocalization.Model,
  amLocalization.Translator;

type
  ITranslationServiceMS = interface
    ['{BCB967CF-9D86-404E-824F-3952F31B4AEC}']
    function ValidateAPIKey(const APIKey: string; var ErrorMessage: string): boolean;
  end;

type
  TDataModuleTranslatorMicrosoftV3 = class(TDataModule, ITranslationService, ITranslationServiceMS)
    RESTRequestTranslate: TRESTRequest;
    RESTResponseResult: TRESTResponse;
    RESTClient: TRESTClient;
    RESTRequestLanguages: TRESTRequest;
    RESTRequestValidateAPIKey: TRESTRequest;
  private
  protected
    // ITranslationService
    function BeginLookup(SourceLanguage, TargetLanguage: TLocaleItem): boolean;
    procedure EndLookup;
    function Lookup(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLocaleItem; const SourceValue: string; var TargetValue: string): boolean;
    function GetServiceName: string;

    // ITranslationServiceMS
    function ValidateAPIKey(const APIKey: string; var ErrorMessage: string): boolean;
  public
  end;

implementation

uses
  System.json,
  Rest.Types,
  System.json.Types,
  System.json.Writers,
  Dialogs,
  amLocalization.Settings;


{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ TDataModule1 }

function TDataModuleTranslatorMicrosoftV3.BeginLookup(SourceLanguage, TargetLanguage: TLocaleItem): boolean;
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

procedure TDataModuleTranslatorMicrosoftV3.EndLookup;
begin
//
end;

function TDataModuleTranslatorMicrosoftV3.GetServiceName: string;
resourcestring
  sTranslatorNameMS = 'Microsoft Translation Service';
begin
  Result := sTranslatorNameMS;
end;

function TDataModuleTranslatorMicrosoftV3.Lookup(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLocaleItem; const SourceValue: string;
  var TargetValue: string): boolean;
var
  JsonResultArray: TJsonArray;
  JsonTranslationItem: TJsonObject;
  JsonTranslationItemArray: TJsonArray;
  JsonError: TJsonObject;
  Msg: string;
begin
  RESTRequestTranslate.Params[0].Value := SourceLanguage.LocaleSName;
  RESTRequestTranslate.Params[1].Value := TargetLanguage.LocaleSName;

  RESTRequestTranslate.Body.ClearBody;
  RESTRequestTranslate.Body.JSONWriter.WriteStartArray;
  RESTRequestTranslate.Body.JSONWriter.WriteStartObject;
  RESTRequestTranslate.Body.JSONWriter.WritePropertyName('Text');
  RESTRequestTranslate.Body.JSONWriter.WriteValue(SourceValue);
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

    Result := (not AnsiSameText(SourceValue, TargetValue));
  end;
end;

function TDataModuleTranslatorMicrosoftV3.ValidateAPIKey(const APIKey: string; var ErrorMessage: string): boolean;
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

end.
