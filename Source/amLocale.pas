unit amLocale;

(*
 * Copyright © 2014 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Generics.Collections,
  Generics.Defaults,
  Windows,
  Graphics,
  Classes,
  ActiveX;

type
//------------------------------------------------------------------------------
//
//      TLocaleItem
//
//------------------------------------------------------------------------------
  TLocaleItem = class
  private
    FLocale: LCID;
    FISO3166Name: string;
    FCountryName: string;
    FFlag: TBitmap;
    FDisplayName: string;
    FLanguageName: string;
    FLanguageShortName: string;
    FLocaleName: string;
    FCharSet: integer;
    FReadingLayout: integer;
    FIgnore: boolean;
  protected
    function GetFlag: TBitmap;
    function GetDisplayName: string;
    function GetISO3166Name: string;
    function GetLocaleName: string;
    function GetCountryCode: integer;
    function GetAnsiCodePage: integer;
    function GetLanguageName: string;
    function GetLanguageShortName: string;
    function GetLanguage: LangID;
    function GetCharSet: integer;
    function GetCountryName: string;
    function GetISO639_1Name: string;
    function GetPrimaryLanguage: Word;
    function GetSubLanguage: Word;
    function GetReadingLayout: integer;
    function GetIsLeftToRight: boolean;
    function GetIsRightToLeft: boolean;
  public
    constructor Create(ALocale: LCID);
    destructor Destroy; override;

    function ReleaseFlag: TBitmap;
    procedure DestroyFlag;

    class function GetLocaleData(ID: LCID; Flag: DWORD): string; overload;
    class function GetLocaleDataInt(ID: LCID; Flag: DWORD): DWORD; overload;

    function GetLocaleData(Flag: DWORD): string; overload;
    function GetLocaleDataInt(Flag: DWORD): DWORD; overload;

    property Flag: TBitmap read GetFlag; // Note: Include amFlags unit to include flag resources
    property DisplayName: string read GetDisplayName;
    /// <summary>ISO3166Name: The ISO3166-1 alpha-2 two letter country code. E.g. US, GB, DK, DE etc.</summary>
    property ISO3166Name: string read GetISO3166Name;
    /// <summary>ISO639_1Name: The ISO639-1 two letter language code. E.g. EN, DA, DE etc.</summary>
    property ISO639_1Name: string read GetISO639_1Name;
    /// <summary>LocaleName: The RFC 4646 language-region code. E.g. en-US, en-GB, da-DK, de-DE etc.</summary>
    property LocaleName: string read GetLocaleName;
    function LocaleSName: string; deprecated 'Use LocaleName instead';
    property CountryCode: integer read GetCountryCode;
    property CountryName: string read GetCountryName;
    property AnsiCodePage: integer read GetAnsiCodePage;
    property Locale: LCID read FLocale;
    property PrimaryLanguage: Word read GetPrimaryLanguage;
    property SubLanguage: Word read GetSubLanguage;
    property LanguageName: string read GetLanguageName;
    /// <summary>LanguageShortName: The ISO639-2 three letter language-region code. E.g. ENU, ENG, DAN, DEU etc.</summary>
    property LanguageShortName: string read GetLanguageShortName;
    property Language: LangID read GetLanguage;
    property CharSet: integer read GetCharSet;
    property ReadingLayout: integer read GetReadingLayout;
    property IsLeftToRight: boolean read GetIsLeftToRight;
    property IsRightToLeft: boolean read GetIsRightToLeft;
    property Ignore: boolean read FIgnore write FIgnore;
  end;

//------------------------------------------------------------------------------
//
//      TLocaleItems
//
//------------------------------------------------------------------------------
  TLocaleItems = class abstract
  private
    class var
      FLocaleItems: TList<TLocaleItem>;
      FCustomSorted: boolean;
  private
    class function GetLocaleItems: TList<TLocaleItem>; static;
    class function GetLocaleItem(Index: integer): TLocaleItem; static;
    class function GetCount: integer; static;
    class property LocaleItems: TList<TLocaleItem> read GetLocaleItems;
  strict private
    class destructor Destroy;
  public
    class function IndexOf(AID: LCID): integer; static;
    class function IndexOfCountry(const ISO3166Name: string): integer; static;
    class function IndexOfName(const Name: string): integer; static;
    class function IndexOfLanguageName(const Name: string): integer; static;
    class function IndexOfLanguageShortName(const Name: string): integer; static;
    class function IndexOfCountryCode(const Value: integer): integer; static; deprecated;

    class function FindLCID(Value: LCID): TLocaleItem; static;
    class function FindCountry(const Value: string): TLocaleItem; static;
    class function FindName(const Value: string): TLocaleItem; static;
    class function FindLocaleName(const Value: string; Exact: boolean = False): TLocaleItem; static;
    class function FindLanguageName(const Value: string): TLocaleItem; static;
    class function FindLanguageShortName(const Value: string): TLocaleItem; static;
    class function FindCountryCode(const Value: integer): TLocaleItem; static; deprecated;
    class function FindISO639_1Name(const Value: string): TLocaleItem; static;
    class function FindISO3166Name(const Value: string): TLocaleItem; static;

    class procedure Sort(const AComparer: IComparer<TLocaleItem>); deprecated 'Custom sort prevents binary LCID search';

    class property Items[index: integer]: TLocaleItem read GetLocaleItem;
    class property Count: integer read GetCount;
  end;


const
  LOCALE_SLANGDISPLAYNAME = $0000006f;
  LOCALE_SLOCALIZEDLANGUAGENAME = $0000006f;

//------------------------------------------------------------------------------
//
//      Utilities
//
//------------------------------------------------------------------------------
function LocaleISO3166Name: string;
function LocaleMonetaryGrouping(Index: integer): integer;
procedure ClearLocale;
procedure SetLocale(Locale: LCID);
function MakeLangID(Primary, Region: Word ): Word;
function LocaleName: string;
function LoadNewResourceModule(Locale: LCID): HModule; overload;
function LoadNewResourceModule(LocaleItem: TLocaleItem): HModule; overload;
function LoadNewResourceModule(LocaleItem: TLocaleItem; var ModuleFilename: string): HModule; overload;
function TryLocaleToISO639_1Name(Locale: LCID; var Value: string): boolean;
function LocaleToISO639_1Name(Locale: LCID; const Default: string = ''): string;
function TryISO639_1NameToLocale(const Name: string; var Value: TLCID): boolean;
function ISO639_1NameToLocale(const Name: string; Default: TLCID = 0): TLCID;


//------------------------------------------------------------------------------
//
//      MLang
//
//------------------------------------------------------------------------------
const
  IID_IMultiLanguage2: TGUID = '{DCCFC164-2B38-11D2-B7EC-00C04F8F5D9A}';
  CLSID_CMultiLanguage: TGUID = '{275C23E2-3747-11D0-9FEA-00AA003F8646}';

type
  tagMIMECPINFO = packed record
    dwFlags: LongWord;
    uiCodePage: SYSUINT;
    uiFamilyCodePage: SYSUINT;
    wszDescription: array[0..63] of WideChar;
    wszWebCharset: array[0..49] of WideChar;
    wszHeaderCharset: array[0..49] of WideChar;
    wszBodyCharset: array[0..49] of WideChar;
    wszFixedWidthFont: array[0..31] of WideChar;
    wszProportionalFont: array[0..31] of WideChar;
    bGDICharset: Byte;
    padding : array [0..2] of Byte;
  end;

  tagMIMECSETINFO = packed record
    uiCodePage: SYSUINT;
    uiInternetEncoding: SYSUINT;
    wszCharset: array[0..49] of WideChar;
  end;

  tagRFC1766INFO = packed record
    lcid: LongWord;
    wszRfc1766: array[0..5] of WideChar;
    wszLocaleName: array[0..31] of WideChar;
  end;

  tagDetectEncodingInfo = packed record
    nLangID: SYSUINT;
    nCodePage: SYSUINT;
    nDocPercent: SYSINT;
    nConfidence: SYSINT;
  end;

  __MIDL_IWinTypes_0009 = record
    case Integer of
      0: (hInproc: Integer);
      1: (hRemote: Integer);
  end;

  _RemotableHandle = packed record
    fContext: Integer;
    u: __MIDL_IWinTypes_0009;
  end;

  tagSCRIPTINFO = packed record
    ScriptId: Byte;
    uiCodePage: SYSUINT;
    wszDescription: array[0..47] of WideChar;
    wszFixedWidthFont: array[0..31] of WideChar;
    wszProportionalFont: array[0..31] of WideChar;
  end;

type
  tagMIMECONTF = (
    MIMECONTF_MAILNEWS = $00000001,
    MIMECONTF_BROWSER = $00000002,
    MIMECONTF_MINIMAL = $00000004,
    MIMECONTF_IMPORT = $00000008,
    MIMECONTF_SAVABLE_MAILNEWS = $00000100,
    MIMECONTF_SAVABLE_BROWSER = $00000200,
    MIMECONTF_EXPORT = $00000400,
    MIMECONTF_PRIVCONVERTER = $00010000,
    MIMECONTF_VALID = $00020000,
    MIMECONTF_VALID_NLS = $00040000,
    MIMECONTF_MIME_IE4 = $10000000,
    MIMECONTF_MIME_LATEST = $20000000,
    MIMECONTF_MIME_REGISTRY = $40000000
  );

// *********************************************************************//
// Interface: IEnumCodePage
// Flags:     (0)
// GUID:      {275C23E3-3747-11D0-9FEA-00AA003F8646}
// *********************************************************************//
  IEnumCodePage = interface(IUnknown)
    ['{275C23E3-3747-11D0-9FEA-00AA003F8646}']
    function Clone(out ppEnum: IEnumCodePage): HResult; stdcall;
    function Next(celt: LongWord; out rgelt: tagMIMECPINFO; out pceltFetched: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumRfc1766
// Flags:     (0)
// GUID:      {3DC39D1D-C030-11D0-B81B-00C04FC9B31F}
// *********************************************************************//
  IEnumRfc1766 = interface(IUnknown)
    ['{3DC39D1D-C030-11D0-B81B-00C04FC9B31F}']
    function Clone(out ppEnum: IEnumRfc1766): HResult; stdcall;
    function Next(celt: LongWord; out rgelt: tagRFC1766INFO; out pceltFetched: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IMLangConvertCharset
// Flags:     (0)
// GUID:      {D66D6F98-CDAA-11D0-B822-00C04FC9B31F}
// *********************************************************************//
  IMLangConvertCharset = interface(IUnknown)
    ['{D66D6F98-CDAA-11D0-B822-00C04FC9B31F}']
    function Initialize(uiSrcCodePage: SYSUINT; uiDstCodePage: SYSUINT; dwProperty: LongWord): HResult; stdcall;
    function GetSourceCodePage(out puiSrcCodePage: SYSUINT): HResult; stdcall;
    function GetDestinationCodePage(out puiDstCodePage: SYSUINT): HResult; stdcall;
    function GetProperty(out pdwProperty: LongWord): HResult; stdcall;
    function DoConversion(var pSrcStr: Byte; var pcSrcSize: SYSUINT; var pDstStr: Byte;
      var pcDstSize: SYSUINT): HResult; stdcall;
    function DoConversionToUnicode(var pSrcStr: Shortint; var pcSrcSize: SYSUINT; var pDstStr: Smallint;
      var pcDstSize: SYSUINT): HResult; stdcall;
    function DoConversionFromUnicode(var pSrcStr: Smallint; var pcSrcSize: SYSUINT; var pDstStr: Shortint;
      var pcDstSize: SYSUINT): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumScript
// Flags:     (0)
// GUID:      {AE5F1430-388B-11D2-8380-00C04F8F5DA1}
// *********************************************************************//
  IEnumScript = interface(IUnknown)
    ['{AE5F1430-388B-11D2-8380-00C04F8F5DA1}']
    function Clone(out ppEnum: IEnumScript): HResult; stdcall;
    function Next(celt: LongWord; out rgelt: tagSCRIPTINFO; out pceltFetched: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IMultiLanguage2
// Flags:     (0)
// GUID:      {DCCFC164-2B38-11D2-B7EC-00C04F8F5D9A}
// *********************************************************************//
  IMultiLanguage2 = interface(IUnknown)
    ['{DCCFC164-2B38-11D2-B7EC-00C04F8F5D9A}']
    function GetNumberOfCodePageInfo(out pcCodePage: SYSUINT): HResult; stdcall;
    function GetCodePageInfo(uiCodePage: SYSUINT; LangId: Word; out pCodePageInfo: tagMIMECPINFO): HResult; stdcall;
    function GetFamilyCodePage(uiCodePage: SYSUINT; out puiFamilyCodePage: SYSUINT): HResult; stdcall;
    function EnumCodePages(grfFlags: LongWord; LangId: Word; out ppEnumCodePage: IEnumCodePage): HResult; stdcall;
    function GetCharsetInfo(const Charset: WideString; out pCharsetInfo: tagMIMECSETINFO): HResult; stdcall;
    function IsConvertible(dwSrcEncoding: LongWord; dwDstEncoding: LongWord): HResult; stdcall;
    function ConvertString(var pdwMode: LongWord; dwSrcEncoding: LongWord; dwDstEncoding: LongWord;
      var pSrcStr: Byte; var pcSrcSize: SYSUINT; var pDstStr: Byte; var pcDstSize: SYSUINT): HResult; stdcall;
    function ConvertStringToUnicode(var pdwMode: LongWord; dwEncoding: LongWord; var pSrcStr: Shortint;
      var pcSrcSize: SYSUINT; var pDstStr: Smallint; var pcDstSize: SYSUINT): HResult; stdcall;
    function ConvertStringFromUnicode(var pdwMode: LongWord; dwEncoding: LongWord; var pSrcStr: Smallint;
      var pcSrcSize: SYSUINT; var pDstStr: Shortint; var pcDstSize: SYSUINT): HResult; stdcall;
    function ConvertStringReset: HResult; stdcall;
    function GetRfc1766FromLcid(locale: LongWord; out pbstrRfc1766: WideString): HResult; stdcall;
    function GetLcidFromRfc1766(out plocale: LongWord; const bstrRfc1766: WideString): HResult; stdcall;
    function EnumRfc1766(LangId: Word; out ppEnumRfc1766: IEnumRfc1766): HResult; stdcall;
    function GetRfc1766Info(locale: LongWord; LangId: Word; out pRfc1766Info: tagRFC1766INFO): HResult; stdcall;
    function CreateConvertCharset(uiSrcCodePage: SYSUINT; uiDstCodePage: SYSUINT; dwProperty: LongWord;
      out ppMLangConvertCharset: IMLangConvertCharset): HResult; stdcall;
    function ConvertStringInIStream(var pdwMode: LongWord; dwFlag: LongWord; var lpFallBack: Smallint;
      dwSrcEncoding: LongWord; dwDstEncoding: LongWord; const pstmIn: IStream; const pstmOut: IStream): HResult; stdcall;
    function ConvertStringToUnicodeEx(var pdwMode: LongWord; dwEncoding: LongWord; var pSrcStr: Shortint;
      var pcSrcSize: SYSUINT; var pDstStr: Smallint; var pcDstSize: SYSUINT; dwFlag: LongWord;
      var lpFallBack: Smallint): HResult; stdcall;
    function ConvertStringFromUnicodeEx(var pdwMode: LongWord; dwEncoding: LongWord; var pSrcStr: Smallint;
      var pcSrcSize: SYSUINT; var pDstStr: Shortint; var pcDstSize: SYSUINT; dwFlag: LongWord;
      var lpFallBack: Smallint): HResult; stdcall;
    function DetectCodepageInIStream(dwFlag: LongWord; dwPrefWinCodePage: LongWord; const pstmIn: IStream;
      var lpEncoding: tagDetectEncodingInfo; var pnScores: SYSINT): HResult; stdcall;
    function DetectInputCodepage(dwFlag: LongWord; dwPrefWinCodePage: LongWord; var pSrcStr: Shortint;
      var pcSrcSize: SYSINT; var lpEncoding: tagDetectEncodingInfo; var pnScores: SYSINT): HResult; stdcall;
    function ValidateCodePage(uiCodePage: SYSUINT; var hwnd: _RemotableHandle): HResult; stdcall;
    function GetCodePageDescription(uiCodePage: SYSUINT; lcid: LongWord; lpWideCharStr: PWideChar;
      cchWideChar: SYSINT): HResult; stdcall;
    function IsCodePageInstallable(uiCodePage: SYSUINT): HResult; stdcall;
    function SetMimeDBSource(dwSource: tagMIMECONTF): HResult; stdcall;
    function GetNumberOfScripts(out pnScripts: SYSUINT): HResult; stdcall;
    function EnumScripts(dwFlags: LongWord; LangId: Word; out ppEnumScript: IEnumScript): HResult; stdcall;
    function ValidateCodePageEx(uiCodePage: SYSUINT; var hwnd: _RemotableHandle; dwfIODControl: LongWord): HResult; stdcall;
  end;

function GetSystemDefaultUILanguage: LANGID; stdcall; external kernel32 name 'GetSystemDefaultUILanguage';


//------------------------------------------------------------------------------
//
//      MLang wrappers
//
//------------------------------------------------------------------------------
function DetectCodePage(Stream: TStream): integer;
function GetCodePageDescription(Codepage: integer): string;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  SysUtils,
  IOUtils;

//------------------------------------------------------------------------------
//
//      Utilities
//
//------------------------------------------------------------------------------
function SetResourceHInstance(NewInstance: HModule): HModule;
var
  CurModule: PLibModule;
begin
  CurModule := LibModuleList;
  Result := 0;
  while (CurModule <> nil) do
  begin
    if CurModule.Instance = HInstance then
    begin
      if (CurModule.ResInstance <> CurModule.Instance) then
        FreeLibrary(CurModule.ResInstance);
      CurModule.ResInstance := NewInstance;
      Result := NewInstance;
      Exit;
    end;
    CurModule := CurModule.Next;
  end;
end;

//------------------------------------------------------------------------------

function LoadNewResourceModule(Locale: LCID): HModule;
var
  LocaleItem: TLocaleItem;
begin
  LocaleItem := TLocaleItems.FindLCID(Locale);
  if (LocaleItem = nil) then
    raise Exception.CreateFmt('Invalid language ID: %.4X', [Locale]);
  Result := LoadNewResourceModule(LocaleItem);
end;

function LoadNewResourceModule(LocaleItem: TLocaleItem): HModule;
var
  Filename: string;
begin
  Result := LoadNewResourceModule(LocaleItem, Filename);
end;

function LoadNewResourceModule(LocaleItem: TLocaleItem; var ModuleFilename: string): HModule; overload;
const
  LOAD_LIBRARY_AS_IMAGE_RESOURCE = $00000020;

  function LoadResourceModule(const Filename: string; const FileType: string; var ModuleFilename: string): HModule;
  begin
    ModuleFilename := TPath.ChangeExtension(Filename, '.'+FileType);

    if (not TFile.Exists(ModuleFilename)) then
      Exit(0);

    Result := LoadLibraryEx(PChar(ModuleFilename), 0, LOAD_LIBRARY_AS_DATAFILE or LOAD_LIBRARY_AS_IMAGE_RESOURCE);
  end;

var
  Filename: string;
  NewInst: HModule;
begin
  Result := 0;

  if (LocaleItem <> nil) then
  begin
    Filename := GetModuleName(HInstance);

    if (Filename <> '') then
    begin
      // Try to load resource modules in the following order (example for Danish):
      // 1) filename.DAN (ISO639-2)
      // 2) filename.da-DK (RFC 4646 = "ISO639-1"-"IISO3166" = IETF Language Culture)
      // 3) filename.DA (ISO639-1)
      NewInst := LoadResourceModule(Filename, LocaleItem.LanguageShortName, ModuleFilename);
      if (NewInst = 0) then
        NewInst := LoadResourceModule(Filename, LocaleItem.LocaleName, ModuleFilename);
      if (NewInst = 0) then
        NewInst := LoadResourceModule(Filename, LocaleItem.ISO639_1Name, ModuleFilename);
    end else
      NewInst := 0;

    if (NewInst = 0) then
      ModuleFilename := '';
  end else
    NewInst := hInstance;

  if (NewInst <> 0) then
    Result := SetResourceHInstance(NewInst)
end;

//------------------------------------------------------------------------------

function MakeLangID(Primary, Region: Word ): Word;
begin
  Result := (Region shl 10) or Primary;
end;

//------------------------------------------------------------------------------

procedure SetLocale(Locale: LCID);
begin
  Win32Check(SetThreadLocale(Locale));
  GetFormatSettings;
  ClearLocale;
end;

//------------------------------------------------------------------------------

var
  FISO3166Name: string = '';

function LocaleISO3166Name: string;
begin
  if (FISO3166Name = '') then
    FISO3166Name := TLocaleItem.GetLocaleData(GetThreadLocale, LOCALE_SISO3166CTRYNAME);
  Result := FISO3166Name;
end;

//------------------------------------------------------------------------------

function LocaleName: string;
begin
  Result := TLocaleItem.GetLocaleData(GetThreadLocale, LOCALE_SLANGUAGE);
end;

//------------------------------------------------------------------------------

(*
LOCALE_SMONGROUPING
Sizes for each group of digits to the left of the decimal. The maximum
number of characters allowed for this string is ten, including a terminating
null character. An explicit size is needed for each group, and sizes are
separated by semicolons. If the last value is zero, the preceding value is
repeated. For example, to group thousands, specify 3;0. Indic locales group
the first thousand and then group by hundreds. For example, 12,34,56,789 is
represented by 3;2;0
*)
type
  TMonetaryGrouping = array of integer;

var
  FMonetaryGrouping: TMonetaryGrouping;

function LocaleMonetaryGrouping(Index: integer): integer;
var
  Grouping: string;
  i: integer;
  p: PChar;
  Group: integer;
begin
  if (Length(FMonetaryGrouping) = 0) then
  begin
    Grouping := GetLocaleStr(GetThreadLocale, LOCALE_SMONGROUPING , '3;0');
    // Count number of groups
    p := PChar(Grouping);
    i := 0;
    while (p^ <> #0) do
    begin
      if (p^ = ';') then
        inc(i);
      inc(p);
    end;

    if (i = 0) then
    begin
      i := 1;
      Grouping := '3;0';
    end;

    SetLength(FMonetaryGrouping, i+1);

    // Get groups
    p := PChar(Grouping);
    i := 0;
    while (p^ <> #0) do
    begin
      if (p^ <> ';') then
      begin
        Group := ord(p^)-ord('0');
        FMonetaryGrouping[i] := Group;
      end else
        inc(i);
      inc(p);
    end;
  end;

  if (Index >= Length(FMonetaryGrouping)-1) then
  begin
    if (Length(FMonetaryGrouping) > 1) and (FMonetaryGrouping[Length(FMonetaryGrouping)-1] = 0) then
      Result := FMonetaryGrouping[Length(FMonetaryGrouping)-2]
    else
      Result := 0;
  end else
    Result := FMonetaryGrouping[Index];
end;

//------------------------------------------------------------------------------

procedure ClearLocale;
begin
  FISO3166Name := '';
  SetLength(FMonetaryGrouping, 0);
end;


//------------------------------------------------------------------------------
//
//      TLocaleItems
//
//------------------------------------------------------------------------------
function EnumLocalesCallback(LocaleID: PChar): Integer; stdcall;
var
  AID: LCID;
  LocaleItem: TLocaleItem;
  s: string;
begin
  AID := StrToInt('$' + Copy(LocaleID, 5, 4));
  s := IntToHex(AID, 4);

  Result := 1;

  LocaleItem := TLocaleItem.Create(AID);
  try
    if (LocaleItem.ISO3166Name <> '') then
//      (TLocaleItems.IndexOfCountry(LocaleItem.ISO3166Name) = -1) then
      TLocaleItems.FLocaleItems.Add(LocaleItem)
    else
      LocaleItem.Free;
  except
    LocaleItem.Free;
    Result := 0;
  end;
end;

//------------------------------------------------------------------------------

class destructor TLocaleItems.Destroy;
begin
  FreeAndNil(FLocaleItems);
end;

//------------------------------------------------------------------------------

class function TLocaleItems.FindCountry(const Value: string): TLocaleItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to LocaleItems.Count-1 do
    if (AnsiSameText(FLocaleItems[i].ISO3166Name, Value)) then
    begin
      Result := FLocaleItems[i];
      break;
    end;
end;

//------------------------------------------------------------------------------

class function TLocaleItems.FindCountryCode(const Value: integer): TLocaleItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to LocaleItems.Count-1 do
    if (FLocaleItems[i].CountryCode = Value) then
    begin
      Result := FLocaleItems[i];
      break;
    end;
end;

//------------------------------------------------------------------------------

class function TLocaleItems.FindISO3166Name(const Value: string): TLocaleItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to LocaleItems.Count-1 do
    if (AnsiSameText(FLocaleItems[i].ISO3166Name, Value)) then
    begin
      Result := FLocaleItems[i];
      break;
    end;
end;

//------------------------------------------------------------------------------

class function TLocaleItems.FindISO639_1Name(const Value: string): TLocaleItem;
var
  i: integer;
  SubLanguage: Word;
  LCID: TLCID;
  BetterLocaleItem: TLocaleItem;
begin
  Result := nil;
  for i := 0 to LocaleItems.Count-1 do
    if (AnsiSameText(FLocaleItems[i].ISO639_1Name, Value)) then
    begin
      Result := FLocaleItems[i];
      break;
    end;

  // An ISO639_1 name can map to many different locales (eg en -> en-us, en-gb, en-au) so attempt to find
  // the primary one.
  if (Result <> nil) and (Result.SubLanguage <> 1) then
  begin
    SubLanguage := 0; // We should probably start with 1, but no harm's done
    BetterLocaleItem := nil;
    while (BetterLocaleItem = nil) and (SubLanguage < 32) do
    begin
      LCID := MAKELANGID(Result.PrimaryLanguage, SubLanguage);
      BetterLocaleItem := TLocaleItems.FindLCID(LCID);
      inc(SubLanguage);
    end;
    if (BetterLocaleItem <> nil) then
      Result := BetterLocaleItem;
  end;
end;

//------------------------------------------------------------------------------

class function TLocaleItems.FindLanguageName(const Value: string): TLocaleItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to LocaleItems.Count-1 do
    if (AnsiSameText(FLocaleItems[i].LanguageName, Value)) then
    begin
      Result := FLocaleItems[i];
      break;
    end;
end;

//------------------------------------------------------------------------------

class function TLocaleItems.FindLanguageShortName(const Value: string): TLocaleItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to LocaleItems.Count-1 do
    if (AnsiSameText(FLocaleItems[i].LanguageShortName, Value)) then
    begin
      Result := FLocaleItems[i];
      break;
    end;
end;

//------------------------------------------------------------------------------

class function TLocaleItems.FindLCID(Value: LCID): TLocaleItem;
var
  Index: integer;
begin
  Index := IndexOf(Value);
  if (Index <> -1) then
    Result := FLocaleItems[Index]
  else
    Result := nil;
end;

class function TLocaleItems.FindLocaleName(const Value: string; Exact: boolean): TLocaleItem;
var
  i: integer;
  Language: string;
begin
  Result := nil;
  for i := 0 to LocaleItems.Count-1 do
    if (AnsiSameText(FLocaleItems[i].LocaleName, Value)) then
      Exit(FLocaleItems[i]);

  if (Exact) then
    exit;

  Language := Value;
  i := Pos('-', Language);
  Delete(Language, 1, i);

  Result := FindISO639_1Name(Language);
end;

//------------------------------------------------------------------------------

class function TLocaleItems.FindName(const Value: string): TLocaleItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to LocaleItems.Count-1 do
    if (AnsiSameText(FLocaleItems[i].DisplayName, Value)) then
    begin
      Result := FLocaleItems[i];
      break;
    end;
end;

//------------------------------------------------------------------------------

class function TLocaleItems.GetCount: integer;
begin
  Result := LocaleItems.Count;
end;

//------------------------------------------------------------------------------

class function TLocaleItems.GetLocaleItem(Index: integer): TLocaleItem;
begin
  Result := LocaleItems[Index];
end;

//------------------------------------------------------------------------------

class function TLocaleItems.GetLocaleItems: TList<TLocaleItem>;
begin
  if (FLocaleItems = nil) then
  begin
    FLocaleItems := TObjectList<TLocaleItem>.Create;

    // Get list of all locales supported by system
    EnumSystemLocales(@EnumLocalesCallback, LCID_SUPPORTED);

    // Sort list so we can do binary search on it
    FLocaleItems.Sort(TComparer<TLocaleItem>.Construct(
      function(const A, B: TLocaleItem): integer
      begin
        Result := A.Locale - B.Locale;
      end));
  end;

  Result := FLocaleItems;
end;

//------------------------------------------------------------------------------

class function TLocaleItems.IndexOfCountry(const ISO3166Name: string): integer;
begin
  Result := LocaleItems.Count-1;
  while (Result >= 0) do
  begin
    if (AnsiSameText(FLocaleItems[Result].ISO3166Name, ISO3166Name)) then
      break;
    dec(Result);
  end;
end;

//------------------------------------------------------------------------------

class function TLocaleItems.IndexOfCountryCode(const Value: integer): integer;
begin
  Result := LocaleItems.Count-1;
  while (Result >= 0) do
  begin
    if (FLocaleItems[Result].CountryCode = Value) then
      break;
    dec(Result);
  end;
end;

//------------------------------------------------------------------------------

class function TLocaleItems.IndexOfLanguageName(const Name: string): integer;
begin
  Result := LocaleItems.Count-1;
  while (Result >= 0) do
  begin
    if (AnsiSameText(FLocaleItems[Result].LanguageName, Name)) then
      break;
    dec(Result);
  end;
end;

//------------------------------------------------------------------------------

class function TLocaleItems.IndexOfLanguageShortName(const Name: string): integer;
begin
  Result := LocaleItems.Count-1;
  while (Result >= 0) do
  begin
    if (AnsiSameText(FLocaleItems[Result].LanguageShortName, Name)) then
      break;
    dec(Result);
  end;
end;

//------------------------------------------------------------------------------

class function TLocaleItems.IndexOfName(const Name: string): integer;
begin
  Result := LocaleItems.Count-1;
  while (Result >= 0) do
  begin
    if (AnsiSameText(FLocaleItems[Result].DisplayName, Name)) then
      break;
    dec(Result);
  end;
end;

//------------------------------------------------------------------------------

class procedure TLocaleItems.Sort(const AComparer: IComparer<TLocaleItem>);
begin
  // We need the original list to be sorted by LCID in order to perform binary search on it.
  // If it is sorted in any other order then we must fall back to sequential scan.
  LocaleItems.Sort(AComparer);
  FCustomSorted := True;
end;

//------------------------------------------------------------------------------

class function TLocaleItems.IndexOf(AID: LCID): integer;
var
  Lo, Hi, Mid: Integer;
  Compare: Integer;
begin
  if (not FCustomSorted) then
  begin
    // Binary search
    Lo := 0;
    Hi := LocaleItems.Count-1;
    while (Lo <= Hi) do
    begin
      Mid := Lo + (Hi - Lo) shr 1;
      Compare := FLocaleItems[Mid].Locale - AID;
      if (Compare = 0) then
        Exit(Mid);
      if (Compare < 0) then
        Lo := Mid + 1
      else
        Hi := Mid - 1;
    end;
    Result := -1;
  end else
  begin
    // Sequential scan
    Result := LocaleItems.Count-1;
    while (Result >= 0) do
    begin
      if (FLocaleItems[Result].Locale = AID) then
        break;
      dec(Result);
    end;
  end;
end;


//------------------------------------------------------------------------------
//
//      TLocaleItem
//
//------------------------------------------------------------------------------
function TLocaleItem.GetLanguage: LangID;
begin
  // http://msdn2.microsoft.com/en-us/library/ms776294(VS.85).aspx
  Result := FLocale and $3FF;
end;

//------------------------------------------------------------------------------

function TLocaleItem.GetLanguageName: string;
begin
  (*
  LOCALE_SLANGUAGE
  localized name of language.
  *)
  if (FLanguageName = '') then
    FLanguageName := GetLocaleData(LOCALE_SLANGUAGE);
  Result := FLanguageName;
end;

//------------------------------------------------------------------------------

function TLocaleItem.GetLanguageShortName: string;
begin
  (*
  LOCALE_SABBREVLANGNAME
  abbreviated language name
  http://www.microsoft.com/globaldev/reference/winxp/langtla.mspx
  *)
  if (FLanguageShortName = '') then
    FLanguageShortName := GetLocaleData(LOCALE_SABBREVLANGNAME);
  Result := FLanguageShortName;
end;

//------------------------------------------------------------------------------

function TLocaleItem.GetLocaleData(Flag: DWORD): string;
begin
  Result := GetLocaleData(Locale, Flag);
end;

function TLocaleItem.GetLocaleDataInt(Flag: DWORD): DWORD;
begin
  Result := GetLocaleDataInt(Locale, Flag);
end;

class function TLocaleItem.GetLocaleDataInt(ID: LCID; Flag: DWORD): DWORD;
begin
  if (GetLocaleInfo(ID, Flag or LOCALE_RETURN_NUMBER, PChar(@Result), SizeOf(Result) div SizeOf(Char)) = 0) then
    RaiseLastOSError;
end;

class function TLocaleItem.GetLocaleData(ID: LCID; Flag: DWORD): string;
var
  Size: integer;
begin
  Size := GetLocaleInfo(ID, Flag, nil, 0);
  SetLength(Result, Size-1);
  if (GetLocaleInfo(ID, Flag, PChar(Result), Size) = 0) then
    RaiseLastOSError;
end;

function TLocaleItem.GetLocaleName: string;
begin
  (*
  LOCALE_SNAME
  A multi-part tag to uniquely identify the locale.
  The tag is based on the language tagging conventions of RFC 4646.
  *)
  // en-us, da-dk, etc. = Format('%s-%s', [ISO639_1Name, ISO3166Name]);
  if (FLocaleName = '') then
    FLocaleName := GetLocaleData(LOCALE_SNAME);
  Result := FLocaleName;
end;

function TLocaleItem.LocaleSName: string;
begin
  Result := GetLocaleName;
end;

//------------------------------------------------------------------------------

function TLocaleItem.GetPrimaryLanguage: Word;
begin
  Result := PRIMARYLANGID(FLocale);
end;

//------------------------------------------------------------------------------

function TLocaleItem.GetSubLanguage: Word;
begin
  Result := SUBLANGID(FLocale);
end;

//------------------------------------------------------------------------------

function TLocaleItem.ReleaseFlag: TBitmap;
begin
  Result := FFlag;
  FFlag := nil;
end;

//------------------------------------------------------------------------------

constructor TLocaleItem.Create(ALocale: LCID);
begin
  FLocale := ALocale;
  FCharSet := -1;
  FReadingLayout := -1;
end;

//------------------------------------------------------------------------------

destructor TLocaleItem.Destroy;
begin
  DestroyFlag;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TLocaleItem.DestroyFlag;
begin
  FreeAndNil(FFlag);
end;

//------------------------------------------------------------------------------

function TLocaleItem.GetAnsiCodePage: integer;
begin
  (*
  LOCALE_IDEFAULTANSICODEPAGE
  The ANSI code page used by a locale for applications that do not support
  Unicode. The maximum number of characters allowed for this string is six,
  including a terminating null character. If no ANSI code page is available,
  only Unicode can be used for the locale. In this case, the value is CP_ACP (0).
  *)
  Result := GetLocaleDataInt(LOCALE_IDEFAULTANSICODEPAGE);
end;

//------------------------------------------------------------------------------

function TLocaleItem.GetCharSet: integer;
const
  TCI_SRCLOCALE = $1000;
var
  CharsetInfo: TCharsetInfo;
  ALocale: LCID;
begin
  if (FCharset = -1) then
  begin
    ALocale := Locale;
    if (not TranslateCharsetInfo(ALocale, CharsetInfo, TCI_SRCLOCALE)) then
    begin
      // For some strage reason TranslateCharsetInfo fails on some locales
      if (GetLastError <> ERROR_INVALID_PARAMETER) then
        RaiseLastOSError;
      FCharset := DEFAULT_CHARSET;
    end else
      FCharset := CharsetInfo.ciCharset;
  end;
  Result := FCharset;
end;

//------------------------------------------------------------------------------

function TLocaleItem.GetCountryCode: integer;
begin
  (*
  LOCALE_ICOUNTRY
  Country/region code, based on international phone codes, also referred to as
  IBM country/region codes. The maximum number of characters allowed for this
  string is six, including a terminating null character.
  *)
  Result := GetLocaleDataInt(LOCALE_ICOUNTRY);
end;

//------------------------------------------------------------------------------

function TLocaleItem.GetCountryName: string;
begin
  (*
  LOCALE_SCOUNTRY
  Full localized name of the country/region. The maximum number of characters
  allowed for this string is 80, including a terminating null character. This
  name is based on the localization of the product. Thus it changes for each
  localized version.
  *)
  if (FCountryName = '') then
    FCountryName := GetLocaleData(LOCALE_SCOUNTRY);
  Result := FCountryName;
end;

//------------------------------------------------------------------------------

function TLocaleItem.GetDisplayName: string;
begin
  if (FDisplayName = '') then
  begin
    if (CheckWin32Version(6)) then
      FDisplayName := GetLocaleData(LOCALE_SLANGDISPLAYNAME)
    else
      FDisplayName := LanguageName;
  end;
  Result := FDisplayName;
end;

//------------------------------------------------------------------------------

function TLocaleItem.GetFlag: TBitmap;
var
  HResInfo: THandle;
  Name: string;
begin
  if (FFlag = nil) then
  begin
    Name := Format('flag_%s', [Lowercase(ISO3166Name)]);
    HResInfo := FindResource(HInstance, PChar(Name), RT_BITMAP);
    if (HResInfo <> 0) then
    begin
      FFlag := TBitmap.Create;
      FFlag.LoadFromResourceName(hInstance, Name);
    end;
  end;
  Result := FFlag;
end;

//------------------------------------------------------------------------------

function TLocaleItem.GetISO3166Name: string;
begin
  (*
  LOCALE_SISO3166CTRYNAME
  Country/region name, based on ISO Standard 3166, for example, "US" for the
  United States. The maximum number of characters allowed for this string is
  nine, including a terminating null character.
  *)
  if (FISO3166Name = '') then
    FISO3166Name := GetLocaleData(LOCALE_SISO3166CTRYNAME);
  Result := FISO3166Name;
end;

//------------------------------------------------------------------------------

function TLocaleItem.GetISO639_1Name: string;
begin
  (*
  LOCALE_SISO639LANGNAME
  The abbreviated name of the language based entirely on the ISO Standard 639
  values, in lowercase form, for example, "en" for English. The maximum number
  of characters allowed for this string is nine, including a terminating null
  character.
  *)
  Result := GetLocaleData(LOCALE_SISO639LANGNAME);
end;

//------------------------------------------------------------------------------

function TLocaleItem.GetReadingLayout: integer;
begin
  if (FReadingLayout = -1) then
    FReadingLayout := GetLocaleDataInt(LOCALE_IREADINGLAYOUT);
  Result := FReadingLayout;
end;

function TLocaleItem.GetIsLeftToRight: boolean;
begin
  Result := (ReadingLayout = 0);
end;

function TLocaleItem.GetIsRightToLeft: boolean;
begin
  Result := (ReadingLayout = 1);
end;

//------------------------------------------------------------------------------
//
//      MLang
//
//------------------------------------------------------------------------------
function DetectCodePage(Stream: TStream): integer;
var
  MultiLanguage: IMultiLanguage2;
  ComStream: IStream;
  EncodingInfo: TArray<tagDetectEncodingInfo>;
  Count: integer;
  SavePos: Int64;
const
  MLDETECTCP_NONE = 0;
begin
  Result := 0;

  if (CoCreateInstance(CLSID_CMultiLanguage, nil, CLSCTX_INPROC_SERVER, IID_IMultiLanguage2, MultiLanguage) <> S_OK) then
    exit;

  SavePos := Stream.Position;
  try
    ComStream := TStreamAdapter.Create(Stream);
    try
      Count := 4;
      SetLength(EncodingInfo, Count);
      if (MultiLanguage.DetectCodepageInIStream(MLDETECTCP_NONE, 0, ComStream, EncodingInfo[0], Count) <> S_OK) then
        exit;

      Result := EncodingInfo[0].nCodePage;
    finally
      ComStream := nil;
    end;
  finally
    Stream.Position := SavePos;
  end;
end;

//------------------------------------------------------------------------------

function GetCodePageDescription(Codepage: integer): string;
var
  MultiLanguage: IMultiLanguage2;
begin
  Result := '';

  if (CoCreateInstance(CLSID_CMultiLanguage, nil, CLSCTX_INPROC_SERVER, IID_IMultiLanguage2, MultiLanguage) <> S_OK) then
    exit;

  SetLength(Result, 128);
  if (MultiLanguage.GetCodePageDescription(Codepage, GetUserDefaultLCID, PChar(Result), Length(Result)) <> S_OK) then
  begin
    Result := '';
    exit;
  end;

  Result := PChar(Result);
end;

//------------------------------------------------------------------------------

function TryLocaleToISO639_1Name(Locale: LCID; var Value: string): boolean;
var
  LocaleItem: TLocaleItem;
begin
  LocaleItem := TLocaleItems.FindLCID(Locale);

  if (LocaleItem <> nil) then
  begin
    Result := True;
    Value := LocaleItem.ISO639_1Name;
  end else
  begin
    Result := False;
    Value := '';
  end;
end;
//------------------------------------------------------------------------------

function LocaleToISO639_1Name(Locale: LCID; const Default: string = ''): string;
begin
  if (not TryLocaleToISO639_1Name(Locale, Result)) then
    Result := Default;
end;

//------------------------------------------------------------------------------

function TryISO639_1NameToLocale(const Name: string; var Value: TLCID): boolean;
var
  LocaleItem: TLocaleItem;
begin
  LocaleItem := TLocaleItems.FindISO639_1Name(Name);

  if (LocaleItem <> nil) then
  begin
    Result := True;
    Value := LocaleItem.Locale;
  end else
  begin
    Result := False;
    Value := 0;
  end;
end;

//------------------------------------------------------------------------------

function ISO639_1NameToLocale(const Name: string; Default: TLCID = 0): TLCID;
begin
  if (not TryISO639_1NameToLocale(Name, Result)) then
    Result := Default;
end;

//------------------------------------------------------------------------------

initialization
  ClearLocale;
end.
