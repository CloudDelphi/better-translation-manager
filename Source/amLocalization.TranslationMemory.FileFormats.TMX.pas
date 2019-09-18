unit amLocalization.TranslationMemory.FileFormats.TMX;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  Generics.Collections,
  Classes,
  DB,
  amProgressForm,
  amLocalization.Model,
  amLocalization.Translator.TM,
  amLocalization.TranslationMemory.FileFormats;

// -----------------------------------------------------------------------------
//
// TTranslationMemoryFileFormatTMX
//
// -----------------------------------------------------------------------------
type
  TTranslationMemoryFileFormatTMX = class(TTranslationMemoryFileFormat)
  private
  protected
    function DoLoadFromStream(Stream: TStream; const Progress: IProgress; DetailedProgress: boolean; Translations: TTranslationMemoryFileFormat.TTranslations;
      Languages: TTranslationMemoryFileFormat.TLanguageFields; Merge: boolean): boolean; override;
  public
    procedure SaveToStream(Stream: TStream); override;
  end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  Dialogs, // temporary
  UITypes,
  IOUtils,
  DateUtils,
  Variants,
  SysUtils,
  msxmldom,
  XMLDoc, XMLIntf,
  FireDAC.Comp.Client,
  FireDAC.Comp.DataSet,
  amCursorService,
  amLocale,
  amVersionInfo,
  amFileUtils,
  amLocalization.Settings,
  amLocalization.Utils;

// -----------------------------------------------------------------------------
//
// TTranslationMemoryFileFormatTMX
//
// -----------------------------------------------------------------------------

function TTranslationMemoryFileFormatTMX.DoLoadFromStream(Stream: TStream; const Progress: IProgress; DetailedProgress: boolean;
  Translations: TTranslationMemoryFileFormat.TTranslations; Languages: TTranslationMemoryFileFormat.TLanguageFields; Merge: boolean): boolean;
var
  XML: IXMLDocument;
  Body: IXMLNode;
  Node, ItemNode: IXMLNode;
  LanguageNode: IXMLNode;
  SourceLanguage, SourceLanguageTU: string;
  CreateDate: TDateTime;
  Term: TTerm;
  Terms: TTerms;
  LocaleItem: TLocaleItem;
  Language: string;
  LanguageName: string;
  Field: TField;
  s: string;
begin
  XML := TXMLDocument.Create(nil);
  XML.Active := True;

  XML.LoadFromStream(Stream);

  if (XML.DocumentElement.NodeName <> 'tmx') then
    raise ETranslationMemoryTMX.CreateFmt('XML document root node is not named "tmx": %s', [XML.DocumentElement.NodeName]);

  SourceLanguage := '';

  Node := XML.DocumentElement.ChildNodes.FindNode('header');
  if (Node <> nil) and (not Merge) then
  begin
    SourceLanguage := VarToStr(Node.Attributes['srclang']);
    if (AnsisameText(SourceLanguage, '*all*')) then
      // *all* is pretty meaningless for us, so ignore it
      SourceLanguage := '';

    if (SourceLanguage <> '') then
    begin
      LocaleItem := TLocaleItems.FindLocaleName(SourceLanguage);
      if (LocaleItem <> nil) then
        SourceLanguage := LocaleItem.LocaleSName;
    end;

    s := VarToStr(Node.Attributes['creationdate']);
    if (s.IsEmpty) or (not TryISO8601ToDate(s, CreateDate, True)) then
      CreateDate := Now;
    FileCreateDate := CreateDate;
  end;

  Body := XML.DocumentElement.ChildNodes.FindNode('body');
  if (Body = nil) then
    raise ETranslationMemoryTMX.Create('xliff node not found: tmx\body');

  if (DetailedProgress) then
    Progress.Progress(psBegin, 0, Body.ChildNodes.Count, sTranslationMemoryReadingTerms);

  SourceLanguageTU := '';

  ItemNode := Body.ChildNodes.First;
  while (ItemNode <> nil) do
  begin
    if (DetailedProgress) then
      Progress.AdvanceProgress
    else
      Progress.ProcessMessages;

    if (ItemNode.NodeName = 'tu') then
    begin
      Terms := TTerms.Create;
      Translations.Add(Terms);

      // If header didn't specify a source language, then the individual translation unit must do so
      if (SourceLanguage = '') then
        SourceLanguageTU := VarToStr(ItemNode.Attributes['srclang']);

      LanguageNode := ItemNode.ChildNodes.First;
      while (LanguageNode <> nil) do
      begin
        if (LanguageNode.NodeName = 'tuv') then
        begin
          Language := VarToStr(LanguageNode.Attributes['xml:lang']);
          if (Language = '') then
            Language := VarToStr(LanguageNode.Attributes['lang']); // Seen in the EU TMX files

          LocaleItem := TLocaleItems.FindLocaleName(Language);
          if (LocaleItem <> nil) then
            LanguageName := LocaleItem.LocaleSName
          else
            LanguageName := Language; // TODO : We should ignore language instead

          if (not Languages.TryGetValue(LanguageName, Field)) then
          begin
            Field := TranslationMemory.CreateField(LocaleItem);
            Languages.Add(LanguageName, Field);
          end;

          Term.Field := Field;
          Term.Value := VarToStr(LanguageNode.ChildValues['seg']);

          if (Terms.Count > 0) and (SourceLanguageTU = Language) then
            // Identify per-TU source language as the first in the first
            Terms.Insert(0, Term)
          else
            Terms.Add(Term);
        end;

        LanguageNode := LanguageNode.NextSibling;
      end;
    end;

    ItemNode := ItemNode.NextSibling;
  end;

  if (DetailedProgress) then
    Progress.Progress(psEnd, 1, 1);

  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TTranslationMemoryFileFormatTMX.SaveToStream(Stream: TStream);
var
  XML: IXMLDocument;
  Node, Body: IXMLNode;
  ItemNode: IXMLNode;
  LanguageNode: IXMLNode;
  i: integer;
  Progress: IProgress;
begin
  XML := TXMLDocument.Create(nil);
  XML.Active := True;

  XML.Options := [doNodeAutoIndent];
  XML.Active := True;

  XML.AddChild('tmx');
  XML.DocumentElement.Attributes['version'] := '1.4';

  Node := XML.DocumentElement.AddChild('header');
  Node.Attributes['creationtool'] := TPath.GetFileNameWithoutExtension(ParamStr(0));
  Node.Attributes['creationtoolversion'] := TVersionInfo.FileVersionString(ParamStr(0));
  Node.Attributes['datatype'] := 'plaintext';
  Node.Attributes['segtype'] := 'sentence';
  Node.Attributes['adminlang'] := 'en-us';
  Node.Attributes['srclang'] := '*all*'; // This is tecnically incorrect since we're mostly treating the first language as the source language
  Node.Attributes['creationdate'] := DateToISO8601(FileCreateDate, True);
  Node.Attributes['changedate'] := DateToISO8601(Now, False);

  Body := XML.DocumentElement.AddChild('body');

  if (TableTranslationMemory.Active) and (TableTranslationMemory.RecordCount > 0) then
  begin
    SaveCursor(crAppStart);

    Progress := ShowProgress(sTranslationMemorySaving);
    Progress.EnableAbort := True;

    Progress.Progress(psBegin, 0, TableTranslationMemory.RecordCount);

    TableTranslationMemory.DisableControls;
    try
      TableTranslationMemory.First;

      while (not TableTranslationMemory.EOF) and (not Progress.Aborted) do
      begin
        Progress.AdvanceProgress;

        ItemNode := Body.AddChild('tu');

        for i := 0 to TableTranslationMemory.FieldCount-1 do
        begin
          if (not TableTranslationMemory.Fields[i].IsNull) and (not TableTranslationMemory.Fields[i].AsString.IsEmpty) then
          begin
            LanguageNode := ItemNode.AddChild('tuv');
            LanguageNode.Attributes['xml:lang'] := TableTranslationMemory.Fields[i].FieldName;
            LanguageNode.AddChild('seg').Text := TableTranslationMemory.Fields[i].AsString;
          end;
        end;

        TableTranslationMemory.Next;
      end;
    finally
      TableTranslationMemory.EnableControls;
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

end.
