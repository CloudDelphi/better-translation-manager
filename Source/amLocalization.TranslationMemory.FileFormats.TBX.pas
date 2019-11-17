unit amLocalization.TranslationMemory.FileFormats.TBX;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  Classes,
  amProgress,
  amLocalization.TranslationMemory.FileFormats;

// -----------------------------------------------------------------------------
//
// TTranslationMemoryFileFormatTBX
//
// -----------------------------------------------------------------------------
type
  TTranslationMemoryFileFormatTBX = class(TTranslationMemoryFileFormat)
  private
    function DoLoadFromStreamDOM(Stream: TStream; const Progress: IProgress; DetailedProgress: boolean; Translations: TTranslationMemoryFileFormat.TTranslations;
      Languages: TTranslationMemoryFileFormat.TLanguageFields; Merge: boolean; var SourceLanguage: string): boolean;
    function DoLoadFromStreamSAX(Stream: TStream; const Progress: IProgress; DetailedProgress: boolean; Translations: TTranslationMemoryFileFormat.TTranslations;
      Languages: TTranslationMemoryFileFormat.TLanguageFields; Merge: boolean; var ASourceLanguage: string): boolean;
  protected
    function DoLoadFromStream(Stream: TStream; const Progress: IProgress; DetailedProgress: boolean; Translations: TTranslationMemoryFileFormat.TTranslations;
      Languages: TTranslationMemoryFileFormat.TLanguageFields; Merge: boolean; var SourceLanguage: string): boolean; override;
  public
    class constructor Create;

    procedure SaveToStream(Stream: TStream); override;
    class function FileFormatFileDescription: string; override;
    class function FileFormatFileType: string; override;
    class function FileFormatCapabilities: TFileFormatCapabilities; override;
  end;

  ETranslationMemoryTBX = class(ETranslationMemoryFileFormat);

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  UITypes,
  IOUtils,
  DateUtils,
  Variants,
  SysUtils,
  DB,
  Dialogs,
  ActiveX,
  Windows,
  ComObj,
  msxmldom,
  XMLDoc, XMLIntf, XMLDom,
  MSXML,
  amCursorService,
  amLocale,
  amVersionInfo,
  amLocalization.TranslationMemory;

// -----------------------------------------------------------------------------
//
// TTranslationMemoryFileFormatTBX
//
// -----------------------------------------------------------------------------
class constructor TTranslationMemoryFileFormatTBX.Create;
begin
  RegisterFileFormat(TTranslationMemoryFileFormatTBX);
end;

// -----------------------------------------------------------------------------

class function TTranslationMemoryFileFormatTBX.FileFormatCapabilities: TFileFormatCapabilities;
begin
  Result := [ffcLoad, ffcSave];
end;

class function TTranslationMemoryFileFormatTBX.FileFormatFileDescription: string;
resourcestring
  sFileFormatTBXDescription = 'TermBase eXchange';
begin
  Result := sFileFormatTBXDescription;
end;

class function TTranslationMemoryFileFormatTBX.FileFormatFileType: string;
begin
  Result := 'tbx';
end;

// -----------------------------------------------------------------------------

function TTranslationMemoryFileFormatTBX.DoLoadFromStream(Stream: TStream; const Progress: IProgress; DetailedProgress: boolean;
  Translations: TTranslationMemoryFileFormat.TTranslations; Languages: TTranslationMemoryFileFormat.TLanguageFields; Merge: boolean;
  var SourceLanguage: string): boolean;
begin
  Result := DoLoadFromStreamSAX(Stream, Progress, DetailedProgress, Translations, Languages, Merge, SourceLanguage);
end;

// -----------------------------------------------------------------------------

type
  TTranslationMemoryFileFormatCracker = class(TTranslationMemoryFileFormat);

type
  TFileFormatTBXSaxHandlerCallback = reference to procedure(var Terms: TTranslationMemoryFileFormatCracker.TTerms; LocaleItem: TLocaleItem;
    const Value: string; IsSource: boolean);

  TFileFormatTBXSaxHandler = class(TInterfacedObject, ISAXContentHandler)
  private type
    TSaxState = (ssNone, ssRoot, {ssHeader,} ssText, ssBody, ssTermEntry, ssLangSet, ssTIG, ssNTIG, ssTermGroup, ssTerm, ssTermHandled);
  strict private
    FState: TSaxState;
    FTermIsFromTermGroup: boolean;
    FSourceLocaleItem: TLocaleItem;
    FLocaleItem: TLocaleItem;
    FTerms: TTranslationMemoryFileFormatCracker.TTerms;
    FTermData: string;
  private
    FFileFormatTBX: TTranslationMemoryFileFormatTBX;
    FProgress: IProgress;
    FCallback: TFileFormatTBXSaxHandlerCallback;
  protected
    // IDispatch
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;

    // ISAXContentHandler
    function putDocumentLocator(const pLocator: ISAXLocator): HResult; stdcall;
    function startDocument: HResult; stdcall;
    function endDocument: HResult; stdcall;
    function startPrefixMapping(var pwchPrefix: Word; cchPrefix: SYSINT; var pwchUri: Word;
                                cchUri: SYSINT): HResult; stdcall;
    function endPrefixMapping(var pwchPrefix: Word; cchPrefix: SYSINT): HResult; stdcall;
    function startElement(var pwchNamespaceUri: Word; cchNamespaceUri: SYSINT;
                          var pwchLocalName: Word; cchLocalName: SYSINT; var pwchQName: Word;
                          cchQName: SYSINT; const pAttributes: ISAXAttributes): HResult; stdcall;
    function endElement(var pwchNamespaceUri: Word; cchNamespaceUri: SYSINT;
                        var pwchLocalName: Word; cchLocalName: SYSINT; var pwchQName: Word;
                        cchQName: SYSINT): HResult; stdcall;
    function characters(var pwchChars: Word; cchChars: SYSINT): HResult; stdcall;
    function ignorableWhitespace(var pwchChars: Word; cchChars: SYSINT): HResult; stdcall;
    function processingInstruction(var pwchTarget: Word; cchTarget: SYSINT; var pwchData: Word;
                                   cchData: SYSINT): HResult; stdcall;
    function skippedEntity(var pwchName: Word; cchName: SYSINT): HResult; stdcall;
  public
    constructor Create(AFileFormatTBX: TTranslationMemoryFileFormatTBX; const AProgress: IProgress; ACallback: TFileFormatTBXSaxHandlerCallback);
    destructor Destroy; override;
  end;

constructor TFileFormatTBXSaxHandler.Create(AFileFormatTBX: TTranslationMemoryFileFormatTBX; const AProgress: IProgress;
  ACallback: TFileFormatTBXSaxHandlerCallback);
begin
  inherited Create;
  FFileFormatTBX := AFileFormatTBX;
  FProgress := AProgress;
  FCallback := ACallback;
end;

destructor TFileFormatTBXSaxHandler.Destroy;
begin
  inherited;
end;

function TFileFormatTBXSaxHandler.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TFileFormatTBXSaxHandler.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TFileFormatTBXSaxHandler.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TFileFormatTBXSaxHandler.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;


function TFileFormatTBXSaxHandler.characters(var pwchChars: Word; cchChars: SYSINT): HResult;
var
  StartIndex: integer;
begin
  Result := S_OK;
  if (FState = ssTerm) then
  begin
    StartIndex := Length(FTermData)+1;
    SetLength(FTermData, Length(FTermData) + cchChars);
    Move(pwchChars, FTermData[StartIndex], cchChars*SizeOf(Char));
  end;
end;

function TFileFormatTBXSaxHandler.ignorableWhitespace(var pwchChars: Word; cchChars: SYSINT): HResult;
begin
  Result := S_OK;
end;

function TFileFormatTBXSaxHandler.startDocument: HResult;
begin
  Result := S_OK;
end;

function TFileFormatTBXSaxHandler.endDocument: HResult;
begin
  Result := S_OK;
end;

function TFileFormatTBXSaxHandler.processingInstruction(var pwchTarget: Word; cchTarget: SYSINT; var pwchData: Word;
  cchData: SYSINT): HResult;
begin
  Result := S_OK;
end;

function TFileFormatTBXSaxHandler.startElement(var pwchNamespaceUri: Word; cchNamespaceUri: SYSINT; var pwchLocalName: Word;
  cchLocalName: SYSINT; var pwchQName: Word; cchQName: SYSINT; const pAttributes: ISAXAttributes): HResult;

  function TryGetAttribute(const Name: string; var Value: string): boolean;
  var
    Count: integer;
    i: integer;
    p: PChar;
    Len: integer;
  begin
    pAttributes.getLength(Count);
    for i := 0 to Count-1 do
    begin
      pAttributes.getLocalName(i, PWord1(p), Len);
      if (StrLComp(PChar(Name), p, Len) = 0) then
      begin
        pAttributes.getValue(i, PWord1(p), Len);
        SetLength(Value, Len);
        Move(p^, PChar(Value)^, Len*SizeOf(Char));
        Exit(True);
      end;
    end;
    Value := '';
    Result := False;
  end;

var
  NodeName: string;
  Language: string;
begin
  Result := S_OK;

  SetLength(NodeName, cchLocalName);
  Move(pwchLocalName, NodeName[1], cchLocalName*SizeOf(Char));

  try

    FProgress.ProcessMessages;

  except
    on E: EAbort do
      Exit(E_FAIL);
  end;

  case FState of
    ssNone:
      begin
        if (NodeName <> 'martif') then
          raise ETranslationMemoryTBX.CreateFmt('XML document root node must be named "martif": %s', [NodeName]);

        FState := ssRoot;
      end;

    ssRoot:
(*
      if (NodeName = 'martifHeader') then
        FState := ssHeader
      else
*)
      if (NodeName = 'text') then
        FState := ssText;

    ssText:
      if (NodeName = 'body') then
      begin
        FState := ssBody;
        FSourceLocaleItem := nil;
      end;

    ssBody:
      if (NodeName = 'termEntry') then
      begin
        FState := ssTermEntry;
        FTerms := nil;
        FLocaleItem := nil;
      end;

    ssTermEntry:
      if (NodeName = 'langSet') then
      begin
        if (not TryGetAttribute('xml:lang', Language)) and (not TryGetAttribute('lang', Language)) then
          Exit;

        if (Language = '') then
          Exit;

        FLocaleItem := TLocaleItems.FindLocaleName(Language);
        if (FLocaleItem = nil) then
          Exit;

        FState := ssLangSet;

        // First language becomes the source language
        if (FSourceLocaleItem = nil) then
          FSourceLocaleItem := FLocaleItem;
      end;

    ssLangSet:
      if (NodeName = 'tig') then
      begin
        FState := ssTIG;
      end;

    ssTIG, ssTermGroup:
      if (NodeName = 'term') then
      begin
        FTermIsFromTermGroup := (FState = ssTermGroup);
        FState := ssTerm;
      end else
      if (FState = ssTIG) and (NodeName = 'ntig') then
      begin
        FState := ssNTIG;
      end;

    ssNTIG:
      if (NodeName = 'termGrp') then
      begin
        FState := ssTermGroup;
      end;

    ssTerm:
      begin
      end;
  end;
end;

function TFileFormatTBXSaxHandler.endElement(var pwchNamespaceUri: Word; cchNamespaceUri: SYSINT; var pwchLocalName: Word;
  cchLocalName: SYSINT; var pwchQName: Word; cchQName: SYSINT): HResult;
var
  NodeName: string;
begin
  Result := S_OK;

  SetLength(NodeName, cchLocalName);
  Move(pwchLocalName, NodeName[1], cchLocalName*SizeOf(Char));

  case FState of
    ssRoot:
      if (NodeName = 'martif') then
        FState := ssNone;

(*
    ssHeader:
      if (NodeName = 'martifheader') then
        FState := ssRoot;
*)

    ssText:
      if (NodeName = 'text') then
        FState := ssRoot;

    ssBody:
      if (NodeName = 'body') then
        FState := ssText;

    ssTermEntry:
      if (NodeName = 'termEntry') then
        FState := ssBody;

    ssLangSet:
      if (NodeName = 'langSet') then
        FState := ssTermEntry;

    ssTIG:
      if (NodeName = 'tig') then
        FState := ssLangSet;

    ssNTIG:
      if (NodeName = 'termGrp') then
        FState := ssTIG;

    ssTermGroup:
      if (NodeName = 'termGrp') then
        FState := ssNTIG;

    ssTerm:
      if (NodeName = 'term') then
      begin
        FCallback(FTerms, FLocaleItem, FTermData, FLocaleItem = FSourceLocaleItem);
        FTermData := '';

        if (FTermIsFromTermGroup) then
          FState := ssTermGroup
        else
          FState := ssTIG;
      end;
  end;
end;

function TFileFormatTBXSaxHandler.startPrefixMapping(var pwchPrefix: Word; cchPrefix: SYSINT; var pwchUri: Word; cchUri: SYSINT): HResult;
begin
  Result := S_OK;
end;

function TFileFormatTBXSaxHandler.endPrefixMapping(var pwchPrefix: Word; cchPrefix: SYSINT): HResult;
begin
  Result := S_OK;
end;

function TFileFormatTBXSaxHandler.putDocumentLocator(const pLocator: ISAXLocator): HResult;
begin
  Result := S_OK;
end;

function TFileFormatTBXSaxHandler.skippedEntity(var pwchName: Word; cchName: SYSINT): HResult;
begin
  Result := S_OK;
end;

function TTranslationMemoryFileFormatTBX.DoLoadFromStreamSAX(Stream: TStream; const Progress: IProgress; DetailedProgress: boolean;
  Translations: TTranslationMemoryFileFormat.TTranslations; Languages: TTranslationMemoryFileFormat.TLanguageFields; Merge: boolean;
  var ASourceLanguage: string): boolean;
var
  ProgressStream: TStream;
  SAXReader: ISAXXMLReader;
  SAXStream: IStream;
  SAXContentHandler: ISAXContentHandler;
  SourceLanguage: string;
begin
  SAXReader := CreateComObject(CLASS_SAXXMLReader60) as ISAXXMLReader;

  SourceLanguage := '';
  SAXContentHandler := TFileFormatTBXSaxHandler.Create(Self, Progress,
    procedure(var Terms: TTranslationMemoryFileFormatCracker.TTerms; LocaleItem: TLocaleItem; const Value: string; IsSource: boolean)
    var
      Field: TField;
      Term: TTerm;
    begin
      if (not Languages.TryGetValue(LocaleItem.LocaleName, Field)) then
      begin
        Field := TranslationMemory.CreateField(LocaleItem);
        Languages.Add(LocaleItem.LocaleName, Field);
      end;

      Term.Field := Field;
      Term.Value := Value;

      if (Terms = nil) then
      begin
        Terms := TTerms.Create;
        Translations.Add(Terms);
      end;

      if (Terms.Count > 0) and (IsSource) then
        // Identify source language as the first in the list
        Terms.Insert(0, Term)
      else
        Terms.Add(Term);

      if (IsSource) and (SourceLanguage = '') then
        SourceLanguage := LocaleItem.LocaleName;
    end);

  SAXReader.putContentHandler(SAXContentHandler);

  if (DetailedProgress) then
    ProgressStream := TProgressStream.Create(Stream, Progress)
  else
    ProgressStream := Stream;
  try
    try
      if (DetailedProgress) then
        Progress.UpdateMessage(sTranslationMemoryLoad);

      SAXStream := TStreamAdapter.Create(ProgressStream);

      SAXReader.parse(SAXStream);

    finally
      if (DetailedProgress) then
        ProgressStream.Free;
    end;
  except
    on E: Exception do
    begin
      MessageDlg(E.Message, mtWarning, [mbOK], 0);
      Exit(False);
    end;
  end;

  ASourceLanguage := SourceLanguage;

  if (DetailedProgress) then
    Progress.Progress(psEnd, 1, 1);

  Result := True;
end;

// -----------------------------------------------------------------------------

function TTranslationMemoryFileFormatTBX.DoLoadFromStreamDOM(Stream: TStream; const Progress: IProgress; DetailedProgress: boolean;
  Translations: TTranslationMemoryFileFormat.TTranslations; Languages: TTranslationMemoryFileFormat.TLanguageFields; Merge: boolean;
  var SourceLanguage: string): boolean;
var
  ProgressStream: TStream;
  XML: IXMLDocument;
  HeaderNode, BodyNode: IXMLNode;
  TermEntryNode, LanguageNode, TermGroupNode: IXMLNode;
  Node: IXMLNode;
  Term: TTerm;
  Terms: TTerms;
  LocaleItem: TLocaleItem;
  Language: string;
  LanguageName: string;
  Field: TField;
begin
  XML := TXMLDocument.Create(nil);
  XML.Active := True;

  if (DetailedProgress) then
    ProgressStream := TProgressStream.Create(Stream, Progress)
  else
    ProgressStream := Stream;
  try
    try
      if (DetailedProgress) then
        Progress.UpdateMessage(sTranslationMemoryLoad);

      XML.LoadFromStream(ProgressStream);

    finally
      if (DetailedProgress) then
        ProgressStream.Free;
    end;
  except
    on E: EDOMParseError do
    begin
      MessageDlg(E.Message, mtWarning, [mbOK], 0);
      Exit(False);
    end;
  end;

  if (XML.DocumentElement.NodeName <> 'martif') then
    raise ETranslationMemoryTBX.CreateFmt('XML document root node is not named "martif": %s', [XML.DocumentElement.NodeName]);

  HeaderNode := XML.DocumentElement.ChildNodes.FindNode('martifheader');
  if (HeaderNode <> nil) then
  begin
  end;

  Node := XML.DocumentElement.ChildNodes.FindNode('text');
  if (Node = nil) then
    raise ETranslationMemoryTBX.Create('Required node not found: martif\text');

  BodyNode := Node.ChildNodes.FindNode('body');
  if (BodyNode = nil) then
    raise ETranslationMemoryTBX.Create('Required node not found: martif\text\body');

  SourceLanguage := '';

  if (DetailedProgress) then
    Progress.Progress(psBegin, 0, BodyNode.ChildNodes.Count, sTranslationMemoryReadingTerms);

  TermEntryNode := BodyNode.ChildNodes.First;
  while (TermEntryNode <> nil) do
  begin
    if (DetailedProgress) then
      Progress.AdvanceProgress
    else
      Progress.ProcessMessages;

    if (TermEntryNode.NodeName = 'termEntry') then
    begin
      Terms := TTerms.Create;
      Translations.Add(Terms);

      LanguageNode := TermEntryNode.ChildNodes.First;
      while (LanguageNode <> nil) do
      begin
        if (LanguageNode.NodeName = 'langSet') then
        begin
          Language := VarToStr(LanguageNode.Attributes['xml:lang']);
          if (Language = '') then
            Language := VarToStr(LanguageNode.Attributes['lang']);

          if (Language <> '') then
            LocaleItem := TLocaleItems.FindLocaleName(Language)
          else
            LocaleItem := nil; // Ignore unknown language

          TermGroupNode := nil;
          if (LocaleItem <> nil) then
          begin
            LanguageName := LocaleItem.LocaleName;

            // First language becomes the source language
            if (SourceLanguage = '') then
              SourceLanguage := LanguageName;

            // <tig><term>
            TermGroupNode := LanguageNode.ChildNodes.FindNode('tig');
            if (TermGroupNode = nil) then
            begin
              // <tig><ntig><termGrp><term>
              Node := LanguageNode.ChildNodes.FindNode('ntig');
              if (Node <> nil) then
                TermGroupNode := Node.ChildNodes.FindNode('termGrp');
            end;
          end;

          if (TermGroupNode <> nil) then
          begin
            if (not Languages.TryGetValue(LanguageName, Field)) then
            begin
              Field := TranslationMemory.CreateField(LocaleItem);
              Languages.Add(LanguageName, Field);
            end;

            Term.Field := Field;
            Term.Value := VarToStr(TermGroupNode.ChildValues['term']);

            if (Terms.Count > 0) and (SourceLanguage = LanguageName) then
              // Identify source language as the first in the first
              Terms.Insert(0, Term)
            else
              Terms.Add(Term);
          end;
        end;

        LanguageNode := LanguageNode.NextSibling;
      end;
    end;

    TermEntryNode := TermEntryNode.NextSibling;
  end;

  if (DetailedProgress) then
    Progress.Progress(psEnd, 1, 1);

  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TTranslationMemoryFileFormatTBX.SaveToStream(Stream: TStream);
var
  XML: IXMLDocument;
  HeaderNode, Node, BodyNode: IXMLNode;
  TermEntryNode: IXMLNode;
  LangSetNode: IXMLNode;
  i: integer;
  Progress: IProgress;
begin
  XML := TXMLDocument.Create(nil);
  XML.Active := True;

  XML.Options := [doNodeAutoIndent];
  XML.Active := True;

  XML.AddChild('martif');
  XML.DocumentElement.Attributes['type'] := 'TBX';
  XML.DocumentElement.Attributes['xml:lang'] := 'en-US';

  HeaderNode := XML.DocumentElement.AddChild('martifHeader');
  Node := HeaderNode.AddChild('fileDesc');
  Node.AddChild('titleStmt').AddChild('title').Text := 'Translation Memory Export';
  Node.AddChild('sourceDesc').AddChild('p').Text := 'Translation Memory';

  Node := XML.DocumentElement.AddChild('text');
  BodyNode := Node.AddChild('body');

  if (TranslationMemoryDataSet.Active) and (TranslationMemoryDataSet.RecordCount > 0) then
  begin
    SaveCursor(crAppStart);

    Progress := ShowProgress(sTranslationMemorySaving);
    Progress.EnableAbort := True;

    Progress.Progress(psBegin, 0, TranslationMemoryDataSet.RecordCount);

    TranslationMemoryDataSet.DisableControls;
    try
      TranslationMemoryDataSet.First;

      while (not TranslationMemoryDataSet.EOF) and (not Progress.Aborted) do
      begin
        Progress.AdvanceProgress;

        TermEntryNode := BodyNode.AddChild('termEntry');

        for i := 0 to TranslationMemoryDataSet.FieldCount-1 do
        begin
          if (not TranslationMemoryDataSet.Fields[i].IsNull) and (not TranslationMemoryDataSet.Fields[i].AsString.IsEmpty) then
          begin
            LangSetNode := TermEntryNode.AddChild('langSet');
            LangSetNode.Attributes['xml:lang'] := TranslationMemoryDataSet.Fields[i].FieldName;
            LangSetNode.AddChild('tig').AddChild('term').Text := TranslationMemoryDataSet.Fields[i].AsString;
          end;
        end;

        TranslationMemoryDataSet.Next;
      end;
    finally
      TranslationMemoryDataSet.EnableControls;
    end;

    if (Progress.Aborted) then
      Exit;

    Progress.Progress(psEnd, 1, 1);
  end;

  SaveCursor(crHourglass);

  XML.SaveToStream(Stream);
end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

initialization
  // Ensure reference to class so class constructor gets called
  TTranslationMemoryFileFormatTBX.ClassName;
end.
