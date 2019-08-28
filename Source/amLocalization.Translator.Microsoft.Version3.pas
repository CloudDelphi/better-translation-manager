unit amLocalization.Translator.Microsoft.Version3;

interface

uses
  System.SysUtils, System.Classes,
  IPPeerClient, REST.Client, Data.Bind.Components, Data.Bind.ObjectScope,

  amLocale,
  amLocalization.Model,
  amLocalization.Translator;

type
  TDataModuleTranslatorMicrosoftV3 = class(TDataModule, ITranslationService)
    RESTRequestTranslate: TRESTRequest;
    RESTResponseResult: TRESTResponse;
    RESTClient: TRESTClient;
  private
  protected
    // ITranslationService
    procedure BeginLookup(SourceLanguage, TargetLanguage: TLocaleItem);
    procedure EndLookup;
    function Lookup(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLocaleItem; const SourceValue: string; var TargetValue: string): boolean;
    function GetServiceName: string;
  public
  end;

const
  sMicrosoftTranslatorV3_APIKEY = '3b9fd2ef5375467bb5a8302b7c6dc8e8';

implementation

uses
  System.json,
  Rest.Types,
  System.json.Types,
  System.json.Writers;


{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ TDataModule1 }

procedure TDataModuleTranslatorMicrosoftV3.BeginLookup(SourceLanguage, TargetLanguage: TLocaleItem);
begin
  RESTClient.Params[0].Value := sMicrosoftTranslatorV3_APIKEY;
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

  // Excecute Call to Microsoft API
  RESTRequestTranslate.Execute;

  // Track result
  if (RESTResponseResult.JSONValue.TryGetValue<TJsonObject>('error', JsonError)) then
  begin
    TargetValue := JsonError.Value;

    Result := False;
  end else
  begin
    JsonResultArray := RESTResponseResult.JSONValue as TJsonArray;
    JsonTranslationItem := JsonResultArray.items[0] as TJsonObject;
    JsonTranslationItemArray := JsonTranslationItem.GetValue('translations') as TJsonArray;
    TargetValue :=  JsonTranslationItemArray.Items[0].GetValue<string>('text');

    Result := (not AnsiSameText(SourceValue, TargetValue));
  end;
end;

end.
