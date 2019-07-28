unit amLocalization.Import.XLIFF;

interface

uses
  Classes,
  amLocalization.Model;

// -----------------------------------------------------------------------------
//
// TModuleImporterXLIFF
//
// -----------------------------------------------------------------------------
type
  TModuleImporterXLIFF = class
  private
  protected
  public
    function LoadFromStream(LocalizerProject: TLocalizerProject; Stream: TStream; const ModuleName: string = ''): TLocalizerModule; overload;
    procedure LoadFromStream(LocalizerModule: TLocalizerModule; Stream: TStream; const ModuleName: string = ''); overload;
    function LoadFromFile(LocalizerProject: TLocalizerProject; const Filename: string): TLocalizerModule; overload;
    procedure LoadFromFile(LocalizerModule: TLocalizerModule; const Filename: string); overload;
  end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  IOUtils,
  Variants,
  Windows,
  SysUtils,
  XMLDoc, XMLIntf,
  amLocale;

// -----------------------------------------------------------------------------
//
// TModuleImporterXLIFF
//
// -----------------------------------------------------------------------------
function TModuleImporterXLIFF.LoadFromFile(LocalizerProject: TLocalizerProject; const Filename: string): TLocalizerModule;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(LocalizerProject, Stream, TPath.GetFileNameWithoutExtension(Filename));
  finally
    Stream.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TModuleImporterXLIFF.LoadFromFile(LocalizerModule: TLocalizerModule; const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(LocalizerModule, Stream);
  finally
    Stream.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TModuleImporterXLIFF.LoadFromStream(LocalizerProject: TLocalizerProject; Stream: TStream; const ModuleName: string): TLocalizerModule;
begin
  Result := LocalizerProject.AddModule(ModuleName);
  try

    LoadFromStream(Result, Stream);

  except
    Result.Free;
    raise;
  end;
end;

// -----------------------------------------------------------------------------

  (*
  ** status:
  ** 0          Untranslated
  ** 1          Translated
  ** 2          Auto translated
  ** 3          Unused
  ** 4          Newly translated
  ** 5          Hold
  ** 6          Don't translate
  ** 7          Egged
  *)
type
  TETMTranslationStatus = (tsUntranslated, tsTranslated, tsAutoTranslated, tsUnused, tsNewlyTranslated, tsHold, tsDontTranslate, tsEgged, tsFinal, tsApproved, tsUnknown);

function EscapeChar(c: Char; IsUnicode: boolean): string;
begin
//  if (TCharacter.GetUnicodeCategory(c) in [TUnicodeCategory.ucControl, TUnicodeCategory.ucUnassigned, TUnicodeCategory.ucPrivateUse]) then
    Result := Format('\x%.4X', [Ord(c)]);
//  else
//    Result := c;
end;

const
  UnicodePrefix = 'L';

type
  SetOfChar = set of char;

function Escape(const s: string; Quote: boolean = False; DontEscape: SetOfChar = []; ForceUnicode: boolean = False): string;
var
  i: integer;
  IsUnicode: boolean;
  p: PChar;
  Lines: integer;
  Chunk: string;
begin
  IsUnicode := ForceUnicode;
  p := PChar(s);
  for i := 1 to Length(s) do
  begin
    if (Ord(p^) > 127) then
    begin
      IsUnicode := True;
      break;
    end;
    inc(p);
  end;

  Result := '';
  if (IsUnicode) then
    Result := Result + UnicodePrefix;

  if (Quote) then
    Result := Result + '"';

  Lines := 1;
  p := PChar(s);
  for i := 1 to Length(s) do
  begin
    if (p^ in DontEscape) then
      Chunk := p^
    else
    case integer(p^) of
      0: Chunk := '\0';
      8: Chunk := '\a'; // Not standard
      9: Chunk := '\t';
      10: Chunk := '\n';
      13: Chunk := '\r';
      34: Chunk := '""'; // "
      92: Chunk := '\\'; // \
      32..33, 35..91, 93..127: Chunk := p^;
    else
      Chunk := EscapeChar(p^, IsUnicode);
    end;
    inc(p);

    // Max length of single string must be 4097
    // Split long strings with continuation character: \
    // We split at 4000 to make room for margin and end delimiter. Not the best
    // solution but "good enough".
    if ((Length(Result) + Length(Chunk)) div Lines > 4000) then
    begin
      Result := Result + '\'+#13#10;
      inc(Lines);
    end;
    Result := Result + Chunk;
  end;

  if (Quote) then
    Result := Result + '"';
end;

function Unescape(const Value: string): string;
var
  p: PChar;
  LastChar: char;
  Escaped: boolean;
  s: string;
  First: boolean;

  procedure Push(c: char);
  begin
    Result := Result + c;
    LastChar := #0;
    Escaped := False;
  end;

  procedure Flush;
  begin
    Result := Result + LastChar;
    LastChar := #0;
    Escaped := False;
  end;

  procedure Purge;
  begin
    LastChar := #0;
    Escaped := False;
  end;

  function Fetch: char;
  begin
    if (p^ <> #0) then
      inc(p);
    Result := p^;
  end;

begin
  Result := '';
  p := PChar(Value);

  // Skip initial 'L'
  if (p^ <> #0) and (p^ = 'L') then
    inc(p);

  Purge;
  First := True;

  while (p^ <> #0) do
  begin
    if (Escaped) then
    begin
      if (p^ = 'x') then
      begin
        s := '$'+Fetch;
        s := s + Fetch;
        s := s + Fetch;
        s := s + Fetch;
        Push(Char(StrToInt(s)));
      end else
      if (p^ = 'r') then
        Push(#13)
      else
      if (p^ = 'n') then
        Push(#10)
      else
      if (p^ = 't') then
        Push(#9)
      else
        Push(p^);
    end else
    if (p^ = '"') then
    begin
      if (First) then
        Purge // just ignore it
      else
      if (LastChar = '"') then
        Flush
      else
        LastChar := p^;
    end else
    if (p^ = '''') then
    begin
      if (First) then
        Purge // just ignore it
      else
      if (LastChar = '''') then
        Flush
      else
        LastChar := p^;
    end else
    if (p^ = '\') then
      Escaped := True
    else
      Push(p^);

    First := False;
    inc(p);
  end;
end;

procedure TModuleImporterXLIFF.LoadFromStream(LocalizerModule: TLocalizerModule; Stream: TStream; const ModuleName: string);
var
  LocalizerItem: TLocalizerItem;
  LocalizerProperty: TLocalizerProperty;
  XML: IXMLDocument;
  Header, HeaderProps: IXMLNode;
  Body: IXMLNode;
  Node, NextNode: IXMLNode;
  Child: IXMLNode;
  TargetNode: IXMLNode;
  PropChild, NextPropChild: IXMLNode;
  s: string;
  i: integer;
  SourceLanguage, TargetLanguage: string;
  SourceLocaleID, TargetLocaleID: LCID;
  LocaleItem: TLocaleItem;
  Translation: TLocalizerTranslation;
  SourceValue, TargetValue: string;
  Value: string;
  NewValue: string;
  FixCount: integer;
  OldTranslationStatus, TranslationStatus: TETMTranslationStatus;
  CountTranslated, CountUntranslated: integer;
  Localize: boolean;
  RemoveNode: boolean;
  ItemName, ItemType, PropertyName, PropertyValue: string;
begin
  XML := TXMLDocument.Create(nil);
  XML.Options := [doNodeAutoIndent];
  XML.Active := True;

  XML.LoadFromStream(Stream);

  FixCount := 0;
  CountTranslated := 0;
  CountUntranslated := 0;

  if (XML.DocumentElement.NodeName <> 'xliff') then
    raise Exception.CreateFmt('XML document root node is not named "xliff": %s', [XML.DocumentElement.NodeName]);

  Node := XML.DocumentElement.ChildNodes.FindNode('file');
  if (Node = nil) then
    raise Exception.Create('xliff node not found: xliff\file');

  if (Node.Attributes['datatype'] ='delphiform') then
      LocalizerModule.Kind := mkForm;

  Value := Node.Attributes['original'];
  if (Value <> '') then
  begin
    // Remove path from filename
    NewValue := TPath.GetFileName(Value);
    if (Value <> NewValue) then
    begin
      Node.Attributes['original'] := NewValue;
      Inc(FixCount);
    end;
    s := TPath.GetFileNameWithoutExtension(NewValue);
    if (not AnsiSameText(s, LocalizerModule.Name)) then
      LocalizerModule.Name := s;

    if (AnsiSameText(TPath.GetExtension(NewValue), '.drc')) then
      LocalizerModule.Kind := mkString;
  end else
  if (LocalizerModule.Name <> '') then
  begin
    // Fix empty filename (caused by ETM)
    Node.Attributes['original'] := TPath.ChangeExtension(LocalizerModule.Name, '.xliff');
    Inc(FixCount);
  end;

  Value := Node.Attributes['ts'];
  if (Value <> '') then
  begin
    // Remove path from filename
    NewValue := TPath.GetFileName(Value);
    if (Value <> NewValue) then
    begin
      Node.Attributes['ts'] := NewValue;
      Inc(FixCount);
    end;

    if (LocalizerModule.Name = '') then
      LocalizerModule.Name := TPath.GetFileNameWithoutExtension(NewValue);
  end else
  if (LocalizerModule.Name <> '') then
  begin
    // Fix empty filename (caused by ETM)
    Node.Attributes['ts'] := TPath.ChangeExtension(LocalizerModule.Name, '.xliff');
    Inc(FixCount);
  end;

  SourceLanguage := Node.Attributes['source-language'];
  TargetLanguage := Node.Attributes['target-language'];

  LocaleItem := TLocaleItems.FindLocaleName(SourceLanguage);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindISO3166Name(SourceLanguage);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindLanguageShortName(SourceLanguage);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindLanguageName(SourceLanguage);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindCountry(SourceLanguage);

  if (LocaleItem <> nil) then
    SourceLocaleID := LocaleItem.Locale
  else
    SourceLocaleID := 0;

  LocaleItem := TLocaleItems.FindLocaleName(TargetLanguage);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindISO3166Name(TargetLanguage);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindLanguageShortName(TargetLanguage);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindLanguageName(TargetLanguage);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindCountry(TargetLanguage);

  if (LocaleItem <> nil) then
    TargetLocaleID := LocaleItem.Locale
  else
    TargetLocaleID := 0;

  if (LocalizerModule.Project.BaseLocaleID = 0) then
    LocalizerModule.Project.BaseLocaleID := SourceLocaleID
  else
  if (SourceLanguage = '') then
    Node.Attributes['source-language'] := TLocaleItems.FindLCID(LocalizerModule.Project.BaseLocaleID).LanguageShortName; // TODO

(*
  if (LocalizerModule.Project.TargetLanguage = '') then
    LocalizerModule.Project.TargetLanguage := TargetLanguage
  else
  if (TargetLanguage = '') then
    Node.Attributes['target-language'] := LocalizerModule.Project.TargetLanguage;
*)

  // TODO : Validation that module languages matches project

  Header := Node.ChildNodes.FindNode('header');

  Body := Node.ChildNodes.FindNode('body');
  if (Body = nil) then
    raise Exception.Create('xliff node not found: xliff\file\body');

  Node := Body.ChildNodes.First;
  while (Node <> nil) do
  begin
    RemoveNode := False;
    Localize := True;
    if (Node.NodeName = 'trans-unit') then
    begin
      if (Node.Attributes['translate'] = 'no') then
        Localize := False;

      s := VarToStr(Node.Attributes['resname']);
      if (s <> '') then
      begin
        // Build a property path from the resname value.
        // Old: I:FormSearch.I:PanelMain.O:PageControl.O:TabSheetTextSearch.O:TopPanel.O:Bevel1.Properties.LineOptions.Visible
        // New: FormSearch\PanelMain\PageControl\TabSheetTextSearch\TopPanel\Bevel1.Properties.LineOptions.Visible
        // Item name: FormSearch\PanelMain\PageControl\TabSheetTextSearch\TopPanel\Bevel1
        // Property name: Properties.LineOptions.Visible
        i := 1;
        while (i < Length(s)-1) do
        begin
          if (s[i] = 'I') or (s[i] = 'O') then
          begin
            if (s[i+1] = ':') then
            begin
              if (i > 1) and (s[i-1] = '.') then
              begin
                Dec(i);
                Delete(s, i, 1);
              end;
              Delete(s, i, 1);
              s[i] := '\';
            end;
          end;
          inc(i);
        end;
        Node.Attributes['resname'] := s;

        // Property name starts at first '.'
        i := Pos('.', s);
        if (i > 0) then
        begin
          ItemName := Copy(s, 1, i-1);
          PropertyName := Copy(s, i+1);
        end else
        begin
          ItemName := s;
          PropertyName := '';
        end;
(*
        if (ItemName = '') then
          ItemName := 'item';
        if (propertyName = '') then
          propertyName := 'value';
*)
        ItemType := '';


        // Remove <alt-trans>: Old translation
        Child := Node.ChildNodes.FindNode('alt-trans');
        while (Child <> nil) do // There can be more than one
        begin
          Inc(FixCount);
          Node.ChildNodes.Remove(Child);
          Child := Node.ChildNodes.FindNode('alt-trans');
        end;

        // Get Source value
        Child := Node.ChildNodes.FindNode('source');
        if (Child <> nil) then
        begin
          s := Child.Text;

          // Only quoted strings are translated
          if (Localize) then
          begin
            if ((not s.StartsWith('''')) or (not s.EndsWith(''''))) and
               ((not s.StartsWith('"'))  or (not s.EndsWith('"'))) then
              Localize := False;
          end;

          SourceValue := Unescape(s);
          // Set source language
          s := Child.Attributes['xml:lang'];
          if (s = '') and (SourceLanguage <> '') then
            Child.Attributes['xml:lang'] := SourceLanguage;
        end else
          SourceValue := '';

        // Get Target value
        TranslationStatus := tsUnknown;
        TargetNode := Node.ChildNodes.FindNode('target');
        if (TargetNode <> nil) then
        begin
          s := TargetNode.Text;
          TargetValue := Unescape(s);

          // Set target language
          s := TargetNode.Attributes['xml:lang'];
          if (s = '') and (TargetLanguage <> '') then
            TargetNode.Attributes['xml:lang'] := TargetLanguage;

          Value := TargetNode.Attributes['state'];
          if (Value = 'final') then
            TranslationStatus := tsFinal
          else
          if (Value = 'translated') then
            TranslationStatus := tsTranslated
          else
          if (Value = 'signed-off') then
            TranslationStatus := tsApproved
          else
          if (Value = '') or (Value = 'new') then
            TranslationStatus := tsUntranslated
          else
          if (Value = 'x-ignore') then
            TranslationStatus := tsDontTranslate
          else
          if (Value = 'x-unused') then
            TranslationStatus := tsUnused
          else
          if (Value = 'x-hold') then
            TranslationStatus := tsHold
          else
            TranslationStatus := tsUnknown;
        end else
          TargetValue := '';

        Child := Node.ChildNodes.FindNode('prop-group');

        if (Child <> nil) then
        begin
          PropChild := Child.ChildNodes.First;
          while (PropChild <> nil) do
          begin
            NextPropChild := PropChild.NextSibling;
            Value := PropChild.Attributes['prop-type'];
            if (Value = 'Created') then
            begin
              // Use static timestamp
              if (PropChild.Text <> '41356') then
              begin
                PropChild.Text := '41356';
                Inc(FixCount);
              end;
            end else
            if (Value = 'Order') then
            begin
              // Use static order
              if (PropChild.Text <> '0') then
              begin
                PropChild.Text := '0';
                Inc(FixCount);
              end;
              // Child.ChildNodes.Remove(PropChild);
            end else
            if (Value = 'Type') then
            begin
              ItemType := PropChild.Text;
              if (ItemType.StartsWith('C:')) then
                Delete(ItemType, 1, 2);
            end else
            if (Value = 'Localize') then
            begin
              // Ignore item if Localize=0
              if (PropChild.Text = '0') then
                Localize := False;
            end else
            if (Value = 'Status') and (TranslationStatus = tsUnknown) then
            begin
              OldTranslationStatus := TETMTranslationStatus(StrToIntDef(PropChild.Text, 0));
              TranslationStatus := OldTranslationStatus;

              // Change variations of Translated
              if (TranslationStatus in [tsAutoTranslated, tsNewlyTranslated]) then
              begin
                TranslationStatus := tsTranslated;
              end else
              // Change "Untranslated" to "Don't translate" for non-string values
              // String in .dfn are ' delimited. Strings in .rcn are " delimited.
              if (TranslationStatus = tsUntranslated) then
              begin
                if (Length(SourceValue) < 2) or (not(SourceValue[1] in ['"', ''''])) then
                  TranslationStatus := tsDontTranslate;
              end;

(*
  Unfortunately removing properties from the DFN causes the Localization Editor to crash.

  Update Localized Projects can reconstruct the original (verbose) DFN by merging the DFN and the unlocalized DFM, so
  if the Update Localized Projects step is performed first then the DFN can be opened in the Localization Editor.
  Unfortunately this risks corrupting the localization strings as we need SigmaLocalizer to escape singlequotes before
  Localization Editor is used. Catch 22. Game over.

              if (TranslationStatus = tsDontTranslate) then
              begin
                if (Length(SourceValue) < 2) or (not(SourceValue[1] in ['"', ''''])) then
                begin
                  RemoveNode := True;
                  TranslationStatus := tsEgged; // So we don't count it below
                  break;
                end;
              end;
*)

              if (PropChild <> nil) and (TranslationStatus <> OldTranslationStatus) then
              begin
                Inc(FixCount);
                PropChild.Text := IntToStr(Ord(TranslationStatus));
              end;
            end;
            PropChild := NextPropChild;
          end;

          // Count status so we can update counters
          if (not RemoveNode) and (Localize) then
          begin
            if (TargetNode <> nil) and (TranslationStatus in [tsUntranslated, tsDontTranslate]) then
            begin
              Inc(FixCount);
              Node.ChildNodes.Remove(TargetNode);
            end;

            if (TranslationStatus in [tsTranslated, tsAutoTranslated, tsNewlyTranslated, tsDontTranslate]) then
              Inc(CountTranslated)
            else
            if (TranslationStatus in [tsUntranslated, tsHold]) then
              Inc(CountUntranslated);
          end;
        end;

      end else
        Localize := False;


      if (Localize) then
      begin
        if (ItemName.StartsWith('\')) then
          Delete(ItemName, 1, 1);
        ItemName := ItemName.Replace('\', '.', [rfReplaceAll]);

        if (not ItemType.IsEmpty) then
        begin
          i := Pos(' ', ItemType);
          if (i > 0) then
            SetLength(ItemType, i -1);
        end;

        LocalizerItem := LocalizerModule.AddItem(ItemName, ItemType);
        LocalizerProperty := LocalizerItem.AddProperty(PropertyName, SourceValue);
        if (TargetNode <> nil) then
        begin
          Translation := LocalizerProperty.Translations.AddOrUpdateTranslation(TargetLocaleID, TargetValue);
        end;
      end;
    end;

    NextNode := Node.NextSibling;

    if (RemoveNode) and (Localize) then
    begin
      Inc(FixCount);
      Body.ChildNodes.Remove(Node);
    end;

    Node := NextNode;
  end;

  if (Header <> nil) then
  begin
    HeaderProps := Header.ChildNodes.FindNode('prop-group');

    if (HeaderProps <> nil) then
    begin
      PropChild := HeaderProps.ChildNodes.First;
      while (PropChild <> nil) do
      begin
        NextPropChild := PropChild.NextSibling;
        Value := PropChild.Attributes['prop-type'];
        if (Value = 'SelfDateTime') or (Value = 'OrigDateTime') or (Value = 'XlatDateTime') then
        begin
          // Use static timestamp
          if (PropChild.Text <> '41356') then
          begin
            PropChild.Text := '41356';
            Inc(FixCount);
          end;
        end else
        if (Value = 'Translated') then
        begin
          // Update status counters
          NewValue := IntToStr(CountTranslated);
          if (PropChild.Text <> NewValue) then
          begin
            PropChild.Text := NewValue;
            Inc(FixCount);
          end;
        end else
        if (Value = 'UnTranslated') then
        begin
          // Update status counters
          NewValue := IntToStr(CountUntranslated);
          if (PropChild.Text <> NewValue) then
          begin
            PropChild.Text := NewValue;
            Inc(FixCount);
          end;
        end;
        PropChild := NextPropChild;
      end;
    end;
  end;

//  if (FixCount > 0) then
//    LogLine(Format('%s: Fixed properties: %d', [ExtractFilename(Filename), FixCount]));

//  if (FForceSave) then
//    LogLine(Format('Saving %s', [ExtractFilename(Filename)]));

//  if ((FixCount > 0) or (FForceSave)) and (not FTest) then
//    WriteDocumentToFile(TMSDOMDocument(XML.DOMDocument).MSDocument, Filename);
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

end.
