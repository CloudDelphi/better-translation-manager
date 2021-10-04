// ************************************************************************ //
// The types declared in this file were generated from data read from the
// WSDL File described below:
// WSDL     : http://api.terminology.microsoft.com/Terminology.svc?wsdl
//  >Import : http://api.terminology.microsoft.com/Terminology.svc?wsdl=wsdl0
//  >Import : http://api.terminology.microsoft.com/Terminology.svc?wsdl=wsdl0>0
//  >Import : http://api.terminology.microsoft.com/Terminology.svc?xsd=xsd0
//  >Import : http://api.terminology.microsoft.com/Terminology.svc?xsd=xsd2
//  >Import : http://api.terminology.microsoft.com/Terminology.svc?xsd=xsd3
//  >Import : http://api.terminology.microsoft.com/Terminology.svc?xsd=xsd4
//  >Import : http://api.terminology.microsoft.com/Terminology.svc?xsd=xsd1
// Encoding : utf-8
// Version  : 1.0
// (04-10-2021 22:02:56 - - $Rev: 106319 $)
// ************************************************************************ //

unit amLocalization.Provider.Microsoft.Terminology.SOAP;

interface

uses Soap.InvokeRegistry, Soap.SOAPHTTPClient, System.Types, Soap.XSBuiltIns;

const
  IS_OPTN = $0001;
  IS_UNBD = $0002;
  IS_NLBL = $0004;
  IS_REF  = $0080;


type

  // ************************************************************************ //
  // The following types, referred to in the WSDL document are not being represented
  // in this file. They are either aliases[@] of other types represented or were referred
  // to but never[!] declared in the document. The types from the latter category
  // typically map to predefined/known XML or Embarcadero types; however, they could also 
  // indicate incorrect WSDL documents that failed to declare or import a schema type.
  // ************************************************************************ //
  // !:string          - "http://www.w3.org/2001/XMLSchema"[Gbl]
  // !:int             - "http://www.w3.org/2001/XMLSchema"[Gbl]
  // !:boolean         - "http://www.w3.org/2001/XMLSchema"[Gbl]

  Unexpected           = class;                 { "https://api.terminology.microsoft.com/terminology"[Flt][GblElm] }
  MissingParameter     = class;                 { "https://api.terminology.microsoft.com/terminology"[Flt][GblElm] }
  InvalidParameters    = class;                 { "https://api.terminology.microsoft.com/terminology"[Flt][GblElm] }
  EntityBase2          = class;                 { "https://api.terminology.microsoft.com/terminology"[GblCplx] }
  Version2             = class;                 { "https://api.terminology.microsoft.com/terminology"[GblCplx] }
  Version              = class;                 { "https://api.terminology.microsoft.com/terminology"[GblElm] }
  Product2             = class;                 { "https://api.terminology.microsoft.com/terminology"[GblCplx] }
  Match2               = class;                 { "https://api.terminology.microsoft.com/terminology"[GblCplx] }
  Match                = class;                 { "https://api.terminology.microsoft.com/terminology"[GblElm] }
  Language2            = class;                 { "https://api.terminology.microsoft.com/terminology"[GblCplx] }
  Language             = class;                 { "https://api.terminology.microsoft.com/terminology"[GblElm] }
  Capability2          = class;                 { "https://api.terminology.microsoft.com/terminology"[GblCplx] }
  Capability           = class;                 { "https://api.terminology.microsoft.com/terminology"[GblElm] }
  Product              = class;                 { "https://api.terminology.microsoft.com/terminology"[GblElm] }
  EntityBase           = class;                 { "https://api.terminology.microsoft.com/terminology"[GblElm] }
  Translation2         = class;                 { "https://api.terminology.microsoft.com/terminology"[GblCplx] }
  Translation          = class;                 { "https://api.terminology.microsoft.com/terminology"[GblElm] }
  BaseFault2           = class;                 { "https://api.terminology.microsoft.com/terminology"[GblCplx] }
  Unexpected2          = class;                 { "https://api.terminology.microsoft.com/terminology"[GblCplx] }
  BaseFault            = class;                 { "https://api.terminology.microsoft.com/terminology"[GblElm] }
  MissingParameter2    = class;                 { "https://api.terminology.microsoft.com/terminology"[GblCplx] }
  InvalidParameters2   = class;                 { "https://api.terminology.microsoft.com/terminology"[GblCplx] }

  {$SCOPEDENUMS ON}
  { "http://schemas.datacontract.org/2004/07/Microsoft.Terminology.WebService.Index.Core"[GblSmpl] }
  SearchOperator = (Exact, Contains_, AnyWord);

  { "http://schemas.datacontract.org/2004/07/Microsoft.Terminology.WebService.Index.Core"[GblSmpl] }
  SearchStringComparison = (CaseInsensitive, CaseSensitive, HotKeyAndCaseSensitive);

  { "http://schemas.datacontract.org/2004/07/Microsoft.Terminology.WebService.Index.Entities"[GblSmpl] }
  TranslationSource = (Terms, UiStrings);

  {$SCOPEDENUMS OFF}



  // ************************************************************************ //
  // XML       : Unexpected, global, <element>
  // Namespace : https://api.terminology.microsoft.com/terminology
  // Info      : Fault
  // Base Types: BaseFault, Unexpected
  // ************************************************************************ //
  Unexpected = class(ERemotableException)
  private
    FMessage_: string;
    FMessage__Specified: boolean;
    FErrorCode: Integer;
    FErrorCode_Specified: boolean;
    procedure SetMessage_(Index: Integer; const Astring: string);
    function  Message__Specified(Index: Integer): boolean;
    procedure SetErrorCode(Index: Integer; const AInteger: Integer);
    function  ErrorCode_Specified(Index: Integer): boolean;
  published
    property Message_:  string   Index (IS_OPTN or IS_NLBL) read FMessage_ write SetMessage_ stored Message__Specified;
    property ErrorCode: Integer  Index (IS_OPTN) read FErrorCode write SetErrorCode stored ErrorCode_Specified;
  end;



  // ************************************************************************ //
  // XML       : MissingParameter, global, <element>
  // Namespace : https://api.terminology.microsoft.com/terminology
  // Info      : Fault
  // Base Types: BaseFault, MissingParameter
  // ************************************************************************ //
  MissingParameter = class(ERemotableException)
  private
    FMessage_: string;
    FMessage__Specified: boolean;
    FParameterName: string;
    FParameterName_Specified: boolean;
    procedure SetMessage_(Index: Integer; const Astring: string);
    function  Message__Specified(Index: Integer): boolean;
    procedure SetParameterName(Index: Integer; const Astring: string);
    function  ParameterName_Specified(Index: Integer): boolean;
  published
    property Message_:      string  Index (IS_OPTN or IS_NLBL) read FMessage_ write SetMessage_ stored Message__Specified;
    property ParameterName: string  Index (IS_OPTN or IS_NLBL) read FParameterName write SetParameterName stored ParameterName_Specified;
  end;



  // ************************************************************************ //
  // XML       : InvalidParameters, global, <element>
  // Namespace : https://api.terminology.microsoft.com/terminology
  // Info      : Fault
  // Base Types: BaseFault, InvalidParameters
  // ************************************************************************ //
  InvalidParameters = class(ERemotableException)
  private
    FMessage_: string;
    FMessage__Specified: boolean;
    procedure SetMessage_(Index: Integer; const Astring: string);
    function  Message__Specified(Index: Integer): boolean;
  published
    property Message_: string  Index (IS_OPTN or IS_NLBL) read FMessage_ write SetMessage_ stored Message__Specified;
  end;



  // ************************************************************************ //
  // XML       : EntityBase, global, <complexType>
  // Namespace : https://api.terminology.microsoft.com/terminology
  // ************************************************************************ //
  EntityBase2 = class(TRemotable)
  private
  published
  end;

  Versions   = array of Version2;               { "https://api.terminology.microsoft.com/terminology"[GblCplx] }


  // ************************************************************************ //
  // XML       : Version, global, <complexType>
  // Namespace : https://api.terminology.microsoft.com/terminology
  // ************************************************************************ //
  Version2 = class(EntityBase2)
  private
    FId: Integer;
    FId_Specified: boolean;
    FName_: string;
    FName__Specified: boolean;
    procedure SetId(Index: Integer; const AInteger: Integer);
    function  Id_Specified(Index: Integer): boolean;
    procedure SetName_(Index: Integer; const Astring: string);
    function  Name__Specified(Index: Integer): boolean;
  published
    property Id:    Integer  Index (IS_OPTN) read FId write SetId stored Id_Specified;
    property Name_: string   Index (IS_OPTN or IS_NLBL) read FName_ write SetName_ stored Name__Specified;
  end;



  // ************************************************************************ //
  // XML       : Version, global, <element>
  // Namespace : https://api.terminology.microsoft.com/terminology
  // ************************************************************************ //
  Version = class(Version2)
  private
  published
  end;

  Matches    = array of Match2;                 { "https://api.terminology.microsoft.com/terminology"[GblCplx] }
  TranslationSources = array of TranslationSource;   { "https://api.terminology.microsoft.com/terminology"[GblCplx] }
  Products   = array of Product2;               { "https://api.terminology.microsoft.com/terminology"[GblCplx] }


  // ************************************************************************ //
  // XML       : Product, global, <complexType>
  // Namespace : https://api.terminology.microsoft.com/terminology
  // ************************************************************************ //
  Product2 = class(EntityBase2)
  private
    FId: Integer;
    FId_Specified: boolean;
    FName_: string;
    FName__Specified: boolean;
    FVersions: Versions;
    FVersions_Specified: boolean;
    procedure SetId(Index: Integer; const AInteger: Integer);
    function  Id_Specified(Index: Integer): boolean;
    procedure SetName_(Index: Integer; const Astring: string);
    function  Name__Specified(Index: Integer): boolean;
    procedure SetVersions(Index: Integer; const AVersions: Versions);
    function  Versions_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property Id:       Integer   Index (IS_OPTN) read FId write SetId stored Id_Specified;
    property Name_:    string    Index (IS_OPTN or IS_NLBL) read FName_ write SetName_ stored Name__Specified;
    property Versions: Versions  Index (IS_OPTN or IS_NLBL) read FVersions write SetVersions stored Versions_Specified;
  end;

  Translations = array of Translation2;         { "https://api.terminology.microsoft.com/terminology"[GblCplx] }


  // ************************************************************************ //
  // XML       : Match, global, <complexType>
  // Namespace : https://api.terminology.microsoft.com/terminology
  // ************************************************************************ //
  Match2 = class(TRemotable)
  private
    FConfidenceLevel: Integer;
    FConfidenceLevel_Specified: boolean;
    FCount: Integer;
    FCount_Specified: boolean;
    FDefinition: string;
    FDefinition_Specified: boolean;
    FOriginalText: string;
    FOriginalText_Specified: boolean;
    FProduct: string;
    FProduct_Specified: boolean;
    FProductVersion: string;
    FProductVersion_Specified: boolean;
    FSource: TranslationSource;
    FSource_Specified: boolean;
    FTranslations: Translations;
    FTranslations_Specified: boolean;
    procedure SetConfidenceLevel(Index: Integer; const AInteger: Integer);
    function  ConfidenceLevel_Specified(Index: Integer): boolean;
    procedure SetCount(Index: Integer; const AInteger: Integer);
    function  Count_Specified(Index: Integer): boolean;
    procedure SetDefinition(Index: Integer; const Astring: string);
    function  Definition_Specified(Index: Integer): boolean;
    procedure SetOriginalText(Index: Integer; const Astring: string);
    function  OriginalText_Specified(Index: Integer): boolean;
    procedure SetProduct(Index: Integer; const Astring: string);
    function  Product_Specified(Index: Integer): boolean;
    procedure SetProductVersion(Index: Integer; const Astring: string);
    function  ProductVersion_Specified(Index: Integer): boolean;
    procedure SetSource(Index: Integer; const ATranslationSource: TranslationSource);
    function  Source_Specified(Index: Integer): boolean;
    procedure SetTranslations(Index: Integer; const ATranslations: Translations);
    function  Translations_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property ConfidenceLevel: Integer            Index (IS_OPTN) read FConfidenceLevel write SetConfidenceLevel stored ConfidenceLevel_Specified;
    property Count:           Integer            Index (IS_OPTN) read FCount write SetCount stored Count_Specified;
    property Definition:      string             Index (IS_OPTN or IS_NLBL) read FDefinition write SetDefinition stored Definition_Specified;
    property OriginalText:    string             Index (IS_OPTN or IS_NLBL) read FOriginalText write SetOriginalText stored OriginalText_Specified;
    property Product:         string             Index (IS_OPTN or IS_NLBL) read FProduct write SetProduct stored Product_Specified;
    property ProductVersion:  string             Index (IS_OPTN or IS_NLBL) read FProductVersion write SetProductVersion stored ProductVersion_Specified;
    property Source:          TranslationSource  Index (IS_OPTN or IS_NLBL) read FSource write SetSource stored Source_Specified;
    property Translations:    Translations       Index (IS_OPTN or IS_NLBL) read FTranslations write SetTranslations stored Translations_Specified;
  end;



  // ************************************************************************ //
  // XML       : Match, global, <element>
  // Namespace : https://api.terminology.microsoft.com/terminology
  // ************************************************************************ //
  Match = class(Match2)
  private
  published
  end;

  Languages  = array of Language2;              { "https://api.terminology.microsoft.com/terminology"[GblCplx] }


  // ************************************************************************ //
  // XML       : Language, global, <complexType>
  // Namespace : https://api.terminology.microsoft.com/terminology
  // ************************************************************************ //
  Language2 = class(EntityBase2)
  private
    FCode: string;
    FCode_Specified: boolean;
    procedure SetCode(Index: Integer; const Astring: string);
    function  Code_Specified(Index: Integer): boolean;
  published
    property Code: string  Index (IS_OPTN or IS_NLBL) read FCode write SetCode stored Code_Specified;
  end;



  // ************************************************************************ //
  // XML       : Language, global, <element>
  // Namespace : https://api.terminology.microsoft.com/terminology
  // ************************************************************************ //
  Language = class(Language2)
  private
  published
  end;



  // ************************************************************************ //
  // XML       : Capability, global, <complexType>
  // Namespace : https://api.terminology.microsoft.com/terminology
  // ************************************************************************ //
  Capability2 = class(TRemotable)
  private
    FSupportsAnyToAny: Boolean;
    FSupportsAnyToAny_Specified: boolean;
    FSupportsAnyToEnUs: Boolean;
    FSupportsAnyToEnUs_Specified: boolean;
    FSupportsEnUsToAny: Boolean;
    FSupportsEnUsToAny_Specified: boolean;
    procedure SetSupportsAnyToAny(Index: Integer; const ABoolean: Boolean);
    function  SupportsAnyToAny_Specified(Index: Integer): boolean;
    procedure SetSupportsAnyToEnUs(Index: Integer; const ABoolean: Boolean);
    function  SupportsAnyToEnUs_Specified(Index: Integer): boolean;
    procedure SetSupportsEnUsToAny(Index: Integer; const ABoolean: Boolean);
    function  SupportsEnUsToAny_Specified(Index: Integer): boolean;
  published
    property SupportsAnyToAny:  Boolean  Index (IS_OPTN) read FSupportsAnyToAny write SetSupportsAnyToAny stored SupportsAnyToAny_Specified;
    property SupportsAnyToEnUs: Boolean  Index (IS_OPTN) read FSupportsAnyToEnUs write SetSupportsAnyToEnUs stored SupportsAnyToEnUs_Specified;
    property SupportsEnUsToAny: Boolean  Index (IS_OPTN) read FSupportsEnUsToAny write SetSupportsEnUsToAny stored SupportsEnUsToAny_Specified;
  end;



  // ************************************************************************ //
  // XML       : Capability, global, <element>
  // Namespace : https://api.terminology.microsoft.com/terminology
  // ************************************************************************ //
  Capability = class(Capability2)
  private
  published
  end;



  // ************************************************************************ //
  // XML       : Product, global, <element>
  // Namespace : https://api.terminology.microsoft.com/terminology
  // ************************************************************************ //
  Product = class(Product2)
  private
  published
  end;



  // ************************************************************************ //
  // XML       : EntityBase, global, <element>
  // Namespace : https://api.terminology.microsoft.com/terminology
  // ************************************************************************ //
  EntityBase = class(EntityBase2)
  private
  published
  end;



  // ************************************************************************ //
  // XML       : Translation, global, <complexType>
  // Namespace : https://api.terminology.microsoft.com/terminology
  // ************************************************************************ //
  Translation2 = class(TRemotable)
  private
    FLanguage: string;
    FLanguage_Specified: boolean;
    FTranslatedText: string;
    FTranslatedText_Specified: boolean;
    procedure SetLanguage(Index: Integer; const Astring: string);
    function  Language_Specified(Index: Integer): boolean;
    procedure SetTranslatedText(Index: Integer; const Astring: string);
    function  TranslatedText_Specified(Index: Integer): boolean;
  published
    property Language:       string  Index (IS_OPTN or IS_NLBL) read FLanguage write SetLanguage stored Language_Specified;
    property TranslatedText: string  Index (IS_OPTN or IS_NLBL) read FTranslatedText write SetTranslatedText stored TranslatedText_Specified;
  end;



  // ************************************************************************ //
  // XML       : Translation, global, <element>
  // Namespace : https://api.terminology.microsoft.com/terminology
  // ************************************************************************ //
  Translation = class(Translation2)
  private
  published
  end;



  // ************************************************************************ //
  // XML       : BaseFault, global, <complexType>
  // Namespace : https://api.terminology.microsoft.com/terminology
  // ************************************************************************ //
  BaseFault2 = class(TRemotable)
  private
    FMessage_: string;
    FMessage__Specified: boolean;
    procedure SetMessage_(Index: Integer; const Astring: string);
    function  Message__Specified(Index: Integer): boolean;
  published
    property Message_: string  Index (IS_OPTN or IS_NLBL) read FMessage_ write SetMessage_ stored Message__Specified;
  end;



  // ************************************************************************ //
  // XML       : Unexpected, global, <complexType>
  // Namespace : https://api.terminology.microsoft.com/terminology
  // ************************************************************************ //
  Unexpected2 = class(BaseFault2)
  private
    FErrorCode: Integer;
    FErrorCode_Specified: boolean;
    procedure SetErrorCode(Index: Integer; const AInteger: Integer);
    function  ErrorCode_Specified(Index: Integer): boolean;
  published
    property ErrorCode: Integer  Index (IS_OPTN) read FErrorCode write SetErrorCode stored ErrorCode_Specified;
  end;



  // ************************************************************************ //
  // XML       : BaseFault, global, <element>
  // Namespace : https://api.terminology.microsoft.com/terminology
  // ************************************************************************ //
  BaseFault = class(BaseFault2)
  private
  published
  end;



  // ************************************************************************ //
  // XML       : MissingParameter, global, <complexType>
  // Namespace : https://api.terminology.microsoft.com/terminology
  // ************************************************************************ //
  MissingParameter2 = class(BaseFault2)
  private
    FParameterName: string;
    FParameterName_Specified: boolean;
    procedure SetParameterName(Index: Integer; const Astring: string);
    function  ParameterName_Specified(Index: Integer): boolean;
  published
    property ParameterName: string  Index (IS_OPTN or IS_NLBL) read FParameterName write SetParameterName stored ParameterName_Specified;
  end;



  // ************************************************************************ //
  // XML       : InvalidParameters, global, <complexType>
  // Namespace : https://api.terminology.microsoft.com/terminology
  // ************************************************************************ //
  InvalidParameters2 = class(BaseFault2)
  private
  published
  end;


  // ************************************************************************ //
  // Namespace : http://api.terminology.microsoft.com/terminology
  // soapAction: http://api.terminology.microsoft.com/terminology/Terminology/%operationName%
  // transport : http://schemas.xmlsoap.org/soap/http
  // style     : document
  // use       : literal
  // binding   : BasicHttpBinding_Terminology
  // service   : Terminology
  // port      : BasicHttpBinding_Terminology
  // URL       : http://api.terminology.microsoft.com/Terminology.svc
  // ************************************************************************ //
  Terminology = interface(IInvokable)
  ['{0A88EA60-50B7-7BCC-EBBE-FAC31545AACB}']
    function  GetTranslations(const text: string; const from: string; const to_: string; const sensitivity: SearchStringComparison; const searchOperator: SearchOperator; const sources: TranslationSources; 
                              const unique: Boolean; const maxTranslations: Integer; const includeDefinitions: Boolean; const products: Products): Matches; stdcall;
    function  GetLanguages: Languages; stdcall;
    function  GetProducts: Products; stdcall;
    function  GetCapability: Capability2; stdcall;
  end;

function GetTerminology(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): Terminology;


implementation
  uses System.SysUtils;

function GetTerminology(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): Terminology;
const
  defWSDL = 'http://api.terminology.microsoft.com/Terminology.svc?wsdl';
  defURL  = 'http://api.terminology.microsoft.com/Terminology.svc';
  defSvc  = 'Terminology';
  defPrt  = 'BasicHttpBinding_Terminology';
var
  RIO: THTTPRIO;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := defWSDL
    else
      Addr := defURL;
  end;
  if HTTPRIO = nil then
    RIO := THTTPRIO.Create(nil)
  else
    RIO := HTTPRIO;
  try
    Result := (RIO as Terminology);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := defSvc;
      RIO.Port := defPrt;
    end else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;


procedure Unexpected.SetMessage_(Index: Integer; const Astring: string);
begin
  FMessage_ := Astring;
  FMessage__Specified := True;
end;

function Unexpected.Message__Specified(Index: Integer): boolean;
begin
  Result := FMessage__Specified;
end;

procedure Unexpected.SetErrorCode(Index: Integer; const AInteger: Integer);
begin
  FErrorCode := AInteger;
  FErrorCode_Specified := True;
end;

function Unexpected.ErrorCode_Specified(Index: Integer): boolean;
begin
  Result := FErrorCode_Specified;
end;

procedure MissingParameter.SetMessage_(Index: Integer; const Astring: string);
begin
  FMessage_ := Astring;
  FMessage__Specified := True;
end;

function MissingParameter.Message__Specified(Index: Integer): boolean;
begin
  Result := FMessage__Specified;
end;

procedure MissingParameter.SetParameterName(Index: Integer; const Astring: string);
begin
  FParameterName := Astring;
  FParameterName_Specified := True;
end;

function MissingParameter.ParameterName_Specified(Index: Integer): boolean;
begin
  Result := FParameterName_Specified;
end;

procedure InvalidParameters.SetMessage_(Index: Integer; const Astring: string);
begin
  FMessage_ := Astring;
  FMessage__Specified := True;
end;

function InvalidParameters.Message__Specified(Index: Integer): boolean;
begin
  Result := FMessage__Specified;
end;

procedure Version2.SetId(Index: Integer; const AInteger: Integer);
begin
  FId := AInteger;
  FId_Specified := True;
end;

function Version2.Id_Specified(Index: Integer): boolean;
begin
  Result := FId_Specified;
end;

procedure Version2.SetName_(Index: Integer; const Astring: string);
begin
  FName_ := Astring;
  FName__Specified := True;
end;

function Version2.Name__Specified(Index: Integer): boolean;
begin
  Result := FName__Specified;
end;

destructor Product2.Destroy;
var
  I: Integer;
begin
  for I := 0 to System.Length(FVersions)-1 do
    System.SysUtils.FreeAndNil(FVersions[I]);
  System.SetLength(FVersions, 0);
  inherited Destroy;
end;

procedure Product2.SetId(Index: Integer; const AInteger: Integer);
begin
  FId := AInteger;
  FId_Specified := True;
end;

function Product2.Id_Specified(Index: Integer): boolean;
begin
  Result := FId_Specified;
end;

procedure Product2.SetName_(Index: Integer; const Astring: string);
begin
  FName_ := Astring;
  FName__Specified := True;
end;

function Product2.Name__Specified(Index: Integer): boolean;
begin
  Result := FName__Specified;
end;

procedure Product2.SetVersions(Index: Integer; const AVersions: Versions);
begin
  FVersions := AVersions;
  FVersions_Specified := True;
end;

function Product2.Versions_Specified(Index: Integer): boolean;
begin
  Result := FVersions_Specified;
end;

destructor Match2.Destroy;
var
  I: Integer;
begin
  for I := 0 to System.Length(FTranslations)-1 do
    System.SysUtils.FreeAndNil(FTranslations[I]);
  System.SetLength(FTranslations, 0);
  inherited Destroy;
end;

procedure Match2.SetConfidenceLevel(Index: Integer; const AInteger: Integer);
begin
  FConfidenceLevel := AInteger;
  FConfidenceLevel_Specified := True;
end;

function Match2.ConfidenceLevel_Specified(Index: Integer): boolean;
begin
  Result := FConfidenceLevel_Specified;
end;

procedure Match2.SetCount(Index: Integer; const AInteger: Integer);
begin
  FCount := AInteger;
  FCount_Specified := True;
end;

function Match2.Count_Specified(Index: Integer): boolean;
begin
  Result := FCount_Specified;
end;

procedure Match2.SetDefinition(Index: Integer; const Astring: string);
begin
  FDefinition := Astring;
  FDefinition_Specified := True;
end;

function Match2.Definition_Specified(Index: Integer): boolean;
begin
  Result := FDefinition_Specified;
end;

procedure Match2.SetOriginalText(Index: Integer; const Astring: string);
begin
  FOriginalText := Astring;
  FOriginalText_Specified := True;
end;

function Match2.OriginalText_Specified(Index: Integer): boolean;
begin
  Result := FOriginalText_Specified;
end;

procedure Match2.SetProduct(Index: Integer; const Astring: string);
begin
  FProduct := Astring;
  FProduct_Specified := True;
end;

function Match2.Product_Specified(Index: Integer): boolean;
begin
  Result := FProduct_Specified;
end;

procedure Match2.SetProductVersion(Index: Integer; const Astring: string);
begin
  FProductVersion := Astring;
  FProductVersion_Specified := True;
end;

function Match2.ProductVersion_Specified(Index: Integer): boolean;
begin
  Result := FProductVersion_Specified;
end;

procedure Match2.SetSource(Index: Integer; const ATranslationSource: TranslationSource);
begin
  FSource := ATranslationSource;
  FSource_Specified := True;
end;

function Match2.Source_Specified(Index: Integer): boolean;
begin
  Result := FSource_Specified;
end;

procedure Match2.SetTranslations(Index: Integer; const ATranslations: Translations);
begin
  FTranslations := ATranslations;
  FTranslations_Specified := True;
end;

function Match2.Translations_Specified(Index: Integer): boolean;
begin
  Result := FTranslations_Specified;
end;

procedure Language2.SetCode(Index: Integer; const Astring: string);
begin
  FCode := Astring;
  FCode_Specified := True;
end;

function Language2.Code_Specified(Index: Integer): boolean;
begin
  Result := FCode_Specified;
end;

procedure Capability2.SetSupportsAnyToAny(Index: Integer; const ABoolean: Boolean);
begin
  FSupportsAnyToAny := ABoolean;
  FSupportsAnyToAny_Specified := True;
end;

function Capability2.SupportsAnyToAny_Specified(Index: Integer): boolean;
begin
  Result := FSupportsAnyToAny_Specified;
end;

procedure Capability2.SetSupportsAnyToEnUs(Index: Integer; const ABoolean: Boolean);
begin
  FSupportsAnyToEnUs := ABoolean;
  FSupportsAnyToEnUs_Specified := True;
end;

function Capability2.SupportsAnyToEnUs_Specified(Index: Integer): boolean;
begin
  Result := FSupportsAnyToEnUs_Specified;
end;

procedure Capability2.SetSupportsEnUsToAny(Index: Integer; const ABoolean: Boolean);
begin
  FSupportsEnUsToAny := ABoolean;
  FSupportsEnUsToAny_Specified := True;
end;

function Capability2.SupportsEnUsToAny_Specified(Index: Integer): boolean;
begin
  Result := FSupportsEnUsToAny_Specified;
end;

procedure Translation2.SetLanguage(Index: Integer; const Astring: string);
begin
  FLanguage := Astring;
  FLanguage_Specified := True;
end;

function Translation2.Language_Specified(Index: Integer): boolean;
begin
  Result := FLanguage_Specified;
end;

procedure Translation2.SetTranslatedText(Index: Integer; const Astring: string);
begin
  FTranslatedText := Astring;
  FTranslatedText_Specified := True;
end;

function Translation2.TranslatedText_Specified(Index: Integer): boolean;
begin
  Result := FTranslatedText_Specified;
end;

procedure BaseFault2.SetMessage_(Index: Integer; const Astring: string);
begin
  FMessage_ := Astring;
  FMessage__Specified := True;
end;

function BaseFault2.Message__Specified(Index: Integer): boolean;
begin
  Result := FMessage__Specified;
end;

procedure Unexpected2.SetErrorCode(Index: Integer; const AInteger: Integer);
begin
  FErrorCode := AInteger;
  FErrorCode_Specified := True;
end;

function Unexpected2.ErrorCode_Specified(Index: Integer): boolean;
begin
  Result := FErrorCode_Specified;
end;

procedure MissingParameter2.SetParameterName(Index: Integer; const Astring: string);
begin
  FParameterName := Astring;
  FParameterName_Specified := True;
end;

function MissingParameter2.ParameterName_Specified(Index: Integer): boolean;
begin
  Result := FParameterName_Specified;
end;

initialization
  { Terminology }
  InvRegistry.RegisterInterface(TypeInfo(Terminology), 'http://api.terminology.microsoft.com/terminology', 'utf-8');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(Terminology), 'http://api.terminology.microsoft.com/terminology/Terminology/%operationName%');
  InvRegistry.RegisterInvokeOptions(TypeInfo(Terminology), ioDocument);
  { Terminology.GetTranslations }
  InvRegistry.RegisterMethodInfo(TypeInfo(Terminology), 'GetTranslations', '',
                                 '[ReturnName="GetTranslationsResult"]', IS_OPTN or IS_NLBL);
  InvRegistry.RegisterParamInfo(TypeInfo(Terminology), 'GetTranslations', 'text', '',
                                '', IS_NLBL);
  InvRegistry.RegisterParamInfo(TypeInfo(Terminology), 'GetTranslations', 'from', '',
                                '', IS_NLBL);
  InvRegistry.RegisterParamInfo(TypeInfo(Terminology), 'GetTranslations', 'to_', 'to',
                                '', IS_NLBL);
  InvRegistry.RegisterParamInfo(TypeInfo(Terminology), 'GetTranslations', 'sensitivity', '',
                                '[Namespace="http://schemas.datacontract.org/2004/07/Microsoft.Terminology.WebService.Index.Core"]');
  InvRegistry.RegisterParamInfo(TypeInfo(Terminology), 'GetTranslations', 'searchOperator', '',
                                '[Namespace="http://schemas.datacontract.org/2004/07/Microsoft.Terminology.WebService.Index.Core"]');
  InvRegistry.RegisterParamInfo(TypeInfo(Terminology), 'GetTranslations', 'sources', '',
                                '[Namespace="https://api.terminology.microsoft.com/terminology", ArrayItemName="TranslationSource"]', IS_NLBL);
  InvRegistry.RegisterParamInfo(TypeInfo(Terminology), 'GetTranslations', 'unique', '',
                                '', IS_NLBL);
  InvRegistry.RegisterParamInfo(TypeInfo(Terminology), 'GetTranslations', 'products', '',
                                '[Namespace="https://api.terminology.microsoft.com/terminology", ArrayItemName="Product"]', IS_NLBL);
  InvRegistry.RegisterParamInfo(TypeInfo(Terminology), 'GetTranslations', 'GetTranslationsResult', '',
                                '[Namespace="https://api.terminology.microsoft.com/terminology", ArrayItemName="Match"]', IS_NLBL);
  { Terminology.GetLanguages }
  InvRegistry.RegisterMethodInfo(TypeInfo(Terminology), 'GetLanguages', '',
                                 '[ReturnName="GetLanguagesResult"]', IS_OPTN or IS_NLBL);
  InvRegistry.RegisterParamInfo(TypeInfo(Terminology), 'GetLanguages', 'GetLanguagesResult', '',
                                '[Namespace="https://api.terminology.microsoft.com/terminology", ArrayItemName="Language"]', IS_NLBL);
  { Terminology.GetProducts }
  InvRegistry.RegisterMethodInfo(TypeInfo(Terminology), 'GetProducts', '',
                                 '[ReturnName="GetProductsResult"]', IS_OPTN or IS_NLBL);
  InvRegistry.RegisterParamInfo(TypeInfo(Terminology), 'GetProducts', 'GetProductsResult', '',
                                '[Namespace="https://api.terminology.microsoft.com/terminology", ArrayItemName="Product"]', IS_NLBL);
  { Terminology.GetCapability }
  InvRegistry.RegisterMethodInfo(TypeInfo(Terminology), 'GetCapability', '',
                                 '[ReturnName="GetCapabilityResult"]', IS_OPTN or IS_NLBL);
  InvRegistry.RegisterParamInfo(TypeInfo(Terminology), 'GetCapability', 'GetCapabilityResult', '',
                                '[Namespace="https://api.terminology.microsoft.com/terminology"]', IS_NLBL);
  RemClassRegistry.RegisterXSClass(Unexpected, 'https://api.terminology.microsoft.com/terminology', 'Unexpected');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(Unexpected), 'Message_', '[ExtName="Message"]');
  RemClassRegistry.RegisterXSInfo(TypeInfo(SearchOperator), 'http://schemas.datacontract.org/2004/07/Microsoft.Terminology.WebService.Index.Core', 'SearchOperator');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(SearchOperator), 'Contains_', 'Contains');
  RemClassRegistry.RegisterXSInfo(TypeInfo(SearchStringComparison), 'http://schemas.datacontract.org/2004/07/Microsoft.Terminology.WebService.Index.Core', 'SearchStringComparison');
  RemClassRegistry.RegisterXSClass(MissingParameter, 'https://api.terminology.microsoft.com/terminology', 'MissingParameter');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(MissingParameter), 'Message_', '[ExtName="Message"]');
  RemClassRegistry.RegisterXSClass(InvalidParameters, 'https://api.terminology.microsoft.com/terminology', 'InvalidParameters');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(InvalidParameters), 'Message_', '[ExtName="Message"]');
  RemClassRegistry.RegisterXSClass(EntityBase2, 'https://api.terminology.microsoft.com/terminology', 'EntityBase2', 'EntityBase');
  RemClassRegistry.RegisterXSInfo(TypeInfo(Versions), 'https://api.terminology.microsoft.com/terminology', 'Versions');
  RemClassRegistry.RegisterXSClass(Version2, 'https://api.terminology.microsoft.com/terminology', 'Version2', 'Version');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(Version2), 'Name_', '[ExtName="Name"]');
  RemClassRegistry.RegisterXSClass(Version, 'https://api.terminology.microsoft.com/terminology', 'Version');
  RemClassRegistry.RegisterXSInfo(TypeInfo(Matches), 'https://api.terminology.microsoft.com/terminology', 'Matches');
  RemClassRegistry.RegisterXSInfo(TypeInfo(TranslationSource), 'http://schemas.datacontract.org/2004/07/Microsoft.Terminology.WebService.Index.Entities', 'TranslationSource');
  RemClassRegistry.RegisterXSInfo(TypeInfo(TranslationSources), 'https://api.terminology.microsoft.com/terminology', 'TranslationSources');
  RemClassRegistry.RegisterXSInfo(TypeInfo(Products), 'https://api.terminology.microsoft.com/terminology', 'Products');
  RemClassRegistry.RegisterXSClass(Product2, 'https://api.terminology.microsoft.com/terminology', 'Product2', 'Product');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(Product2), 'Name_', '[ExtName="Name"]');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(Product2), 'Versions', '[ArrayItemName="Version"]');
  RemClassRegistry.RegisterXSInfo(TypeInfo(Translations), 'https://api.terminology.microsoft.com/terminology', 'Translations');
  RemClassRegistry.RegisterXSClass(Match2, 'https://api.terminology.microsoft.com/terminology', 'Match2', 'Match');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(Match2), 'Translations', '[ArrayItemName="Translation"]');
  RemClassRegistry.RegisterXSClass(Match, 'https://api.terminology.microsoft.com/terminology', 'Match');
  RemClassRegistry.RegisterXSInfo(TypeInfo(Languages), 'https://api.terminology.microsoft.com/terminology', 'Languages');
  RemClassRegistry.RegisterXSClass(Language2, 'https://api.terminology.microsoft.com/terminology', 'Language2', 'Language');
  RemClassRegistry.RegisterXSClass(Language, 'https://api.terminology.microsoft.com/terminology', 'Language');
  RemClassRegistry.RegisterXSClass(Capability2, 'https://api.terminology.microsoft.com/terminology', 'Capability2', 'Capability');
  RemClassRegistry.RegisterXSClass(Capability, 'https://api.terminology.microsoft.com/terminology', 'Capability');
  RemClassRegistry.RegisterXSClass(Product, 'https://api.terminology.microsoft.com/terminology', 'Product');
  RemClassRegistry.RegisterXSClass(EntityBase, 'https://api.terminology.microsoft.com/terminology', 'EntityBase');
  RemClassRegistry.RegisterXSClass(Translation2, 'https://api.terminology.microsoft.com/terminology', 'Translation2', 'Translation');
  RemClassRegistry.RegisterXSClass(Translation, 'https://api.terminology.microsoft.com/terminology', 'Translation');
  RemClassRegistry.RegisterXSClass(BaseFault2, 'https://api.terminology.microsoft.com/terminology', 'BaseFault2', 'BaseFault');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(BaseFault2), 'Message_', '[ExtName="Message"]');
  RemClassRegistry.RegisterXSClass(Unexpected2, 'https://api.terminology.microsoft.com/terminology', 'Unexpected2', 'Unexpected');
  RemClassRegistry.RegisterXSClass(BaseFault, 'https://api.terminology.microsoft.com/terminology', 'BaseFault');
  RemClassRegistry.RegisterXSClass(MissingParameter2, 'https://api.terminology.microsoft.com/terminology', 'MissingParameter2', 'MissingParameter');
  RemClassRegistry.RegisterXSClass(InvalidParameters2, 'https://api.terminology.microsoft.com/terminology', 'InvalidParameters2', 'InvalidParameters');

end.