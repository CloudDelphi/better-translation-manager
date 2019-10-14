object TranslationProviderMicrosoftV3: TTranslationProviderMicrosoftV3
  OldCreateOrder = False
  Height = 309
  Width = 411
  object RESTRequestTranslate: TRESTRequest
    AutoCreateParams = False
    Client = RESTClient
    Method = rmPOST
    Params = <
      item
        Kind = pkURLSEGMENT
        Name = 'from'
        Value = 'en'
      end
      item
        Kind = pkURLSEGMENT
        Name = 'to'
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
    Left = 72
    Top = 140
  end
  object RESTClient: TRESTClient
    Accept = 'application/json, text/plain; q=0.9, text/html;q=0.8,'
    AcceptCharset = 'UTF-8, *;q=0.8'
    AutoCreateParams = False
    BaseURL = 'https://api.cognitive.microsofttranslator.com/'
    Params = <
      item
        Kind = pkHTTPHEADER
        Name = 'Ocp-Apim-Subscription-Key'
      end>
    RaiseExceptionOn500 = False
    Left = 72
    Top = 28
  end
  object RESTRequestLanguages: TRESTRequest
    AutoCreateParams = False
    Client = RESTClient
    Params = <>
    Resource = 'languages'
    ResourceSuffix = '?api-version=3.0&scope=translation'
    Response = RESTResponseResult
    SynchronizedEvents = False
    Left = 192
    Top = 88
  end
  object RESTRequestValidateAPIKey: TRESTRequest
    AutoCreateParams = False
    Client = RESTClient
    Method = rmPOST
    Params = <>
    Resource = 'translate'
    ResourceSuffix = '?api-version=3.0&from=en-us&to=da-dk'
    Response = RESTResponseResult
    SynchronizedEvents = False
    Left = 192
    Top = 28
  end
end
