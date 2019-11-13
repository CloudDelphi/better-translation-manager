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
  msxmldom,
  XMLDoc, XMLIntf,
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
    if (DetailedProgress) then
      Progress.UpdateMessage(sTranslationMemoryLoad);

    XML.LoadFromStream(Stream);

  finally
    if (DetailedProgress) then
      ProgressStream.Free;
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
