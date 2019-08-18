object DataModuleTranslatorMicrosoftV3: TDataModuleTranslatorMicrosoftV3
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 309
  Width = 411
  object RESTRequestTranslate: TRESTRequest
    AutoCreateParams = False
    Client = RESTClient
    Method = rmPOST
    Params = <
      item
        Kind = pkURLSEGMENT
        name = 'from'
        Value = 'en'
      end
      item
        Kind = pkURLSEGMENT
        name = 'to'
        Value = 'da'
      end>
    Resource = 'translate'
    ResourceSuffix = '?api-version=3.0&from={from}&to={to}'
    Response = RESTResponseResult
    SynchronizedEvents = False
    Left = 72
    Top = 88
  end
  object RESTResponseResult: TRESTResponse
    ContentType = 'text/plain'
    Left = 72
    Top = 140
  end
  object RESTClient: TRESTClient
    Accept = 'application/json, text/plain; q=0.9, text/html;q=0.8,'
    AcceptCharset = 'UTF-8, *;q=0.8'
    AutoCreateParams = False
    BaseURL = 'https://api-eur.cognitive.microsofttranslator.com/'
    ContentType = 'application/x-www-form-urlencoded'
    Params = <
      item
        Kind = pkHTTPHEADER
        name = 'Ocp-Apim-Subscription-Key'
      end>
    HandleRedirects = True
    RaiseExceptionOn500 = False
    Left = 72
    Top = 28
  end
end
