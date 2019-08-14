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
    function LoadFromStream(LocalizerProject: TLocalizerProject; Stream: TStream; const FileName: string = ''): TLocalizerModule; overload;
    function LoadFromFile(LocalizerProject: TLocalizerProject; const Filename: string): TLocalizerModule; overload;
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
    Result := LoadFromStream(LocalizerProject, Stream, Filename);
  finally
    Stream.Free;
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

const
  TranslationStatusMap: array[TETMTranslationStatus]of TTranslationStatus = (
    tStatusPending,             // tsUntranslated
    tStatusProposed,            // tsTranslated
    tStatusProposed,            // tsAutoTranslated
    tStatusPending,             // tsUnused
    tStatusProposed,            // tsNewlyTranslated
    tStatusPending,             // tsHold
    tStatusPending,             // tsDontTranslate
    tStatusPending,             // tsEgged
    tStatusTranslated,          // tsFinal
    tStatusTranslated,          // tsApproved
    tStatusObsolete);           // tsUnknown


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

function TModuleImporterXLIFF.LoadFromStream(LocalizerProject: TLocalizerProject; Stream: TStream; const FileName: string): TLocalizerModule;
var
  ModuleName: string;
  ModuleKind: TLocalizerModuleKind;
  SourceFilename: string;
  NewModule: TLocalizerModule;
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
  SourceLanguageName, TargetLanguageName: string;
  SourceLocaleID, TargetLocaleID: LCID;
  TargetLanguage: TTargetLanguage;
  LocaleItem: TLocaleItem;
//  Translation: TLocalizerTranslation;
  SourceValue, TargetValue: string;
  Value: string;
  NewValue: string;
  TranslationStatus: TETMTranslationStatus;
  Localize: boolean;
  RemoveNode: boolean;
  ItemName, ItemType, PropertyName, PropertyValue: string;
begin
  XML := TXMLDocument.Create(nil);
  XML.Options := [doNodeAutoIndent];
  XML.Active := True;

  XML.LoadFromStream(Stream);

  if (XML.DocumentElement.NodeName <> 'xliff') then
    raise Exception.CreateFmt('XML document root node is not named "xliff": %s', [XML.DocumentElement.NodeName]);

  Node := XML.DocumentElement.ChildNodes.FindNode('file');
  if (Node = nil) then
    raise Exception.Create('xliff node not found: xliff\file');

  if (Filename <> '') then
    ModuleName := TPath.GetFileNameWithoutExtension(Filename)
  else
    ModuleName := '';

  SourceFilename := Node.Attributes['original'];
  if (SourceFilename = '') then
    SourceFilename := Node.Attributes['ts'];
  if (SourceFilename <> '') then
    ModuleName := TPath.GetFileNameWithoutExtension(SourceFilename);

  if (Node.Attributes['datatype'] ='delphiform') then
    ModuleKind := mkForm
  else
  if (AnsiSameText(TPath.GetExtension(Filename), '.rcn')) then
    ModuleKind := mkString
  else
  if (AnsiSameText(TPath.GetExtension(Filename), '.dfn')) then
    ModuleKind := mkForm
  else
  if (SourceFilename <> '') and (AnsiSameText(TPath.GetExtension(SourceFilename), '.drc')) then
    ModuleKind := mkString
  else
    ModuleKind := mkForm;

  if (ModuleKind = mkString) then
    ModuleName := sModuleNameResourcestrings;

  Result := LocalizerProject.AddModule(ModuleName);
  if (Result.Kind = mkOther) then
    Result.Kind := ModuleKind;

  SourceLanguageName := Node.Attributes['source-language'];
  TargetLanguageName := Node.Attributes['target-language'];

  // Translate Language Name to Locale ID
  LocaleItem := TLocaleItems.FindLocaleName(SourceLanguageName);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindISO3166Name(SourceLanguageName);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindLanguageShortName(SourceLanguageName);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindLanguageName(SourceLanguageName);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindCountry(SourceLanguageName);

  if (LocaleItem <> nil) then
    SourceLocaleID := LocaleItem.Locale
  else
    SourceLocaleID := 0; // TODO : This is an error

  // Translate Language Name to Locale ID
  LocaleItem := TLocaleItems.FindLocaleName(TargetLanguageName);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindISO3166Name(TargetLanguageName);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindLanguageShortName(TargetLanguageName);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindLanguageName(TargetLanguageName);
  if (LocaleItem = nil) then
    LocaleItem := TLocaleItems.FindCountry(TargetLanguageName);

  if (LocaleItem <> nil) then
    TargetLocaleID := LocaleItem.Locale
  else
    TargetLocaleID := 0; // TODO : This is an error

  TargetLanguage := Result.Project.TargetLanguages.Add(TargetLocaleID);

  if (Result.Project.BaseLocaleID = 0) then
    Result.Project.BaseLocaleID := SourceLocaleID;

  // TODO : Validation that module languages matches project

  Header := Node.ChildNodes.FindNode('header');

  Body := Node.ChildNodes.FindNode('body');
  if (Body = nil) then
    raise Exception.Create('xliff node not found: xliff\file\body');

  Node := Body.ChildNodes.First;
  while (Node <> nil) do
  begin
    Localize := True;
    if (Node.NodeName = 'trans-unit') then
    begin
      TranslationStatus := tsUnknown;

      if (Node.Attributes['translate'] = 'no') then
        Localize := False;

      s := VarToStr(Node.Attributes['resname']);
      if (s = '') then
        s := VarToStr(Node.Attributes['id']);
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
        ItemType := '';

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
        end else
          SourceValue := '';

        // Get Target value
        TranslationStatus := tsUnknown;
        TargetNode := Node.ChildNodes.FindNode('target');
        if (TargetNode <> nil) then
        begin
          s := TargetNode.Text;
          TargetValue := Unescape(s);

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
          if (Value = 'new') then
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
              TranslationStatus := TETMTranslationStatus(StrToIntDef(PropChild.Text, Ord(tsUnknown)));

              // Change variations of Translated
              (*
              if (TranslationStatus in [tsAutoTranslated, tsNewlyTranslated]) then
              begin
                TranslationStatus := tsTranslated;
              end else
              *)
              // Change "Untranslated" to "Don't translate" for non-string values
              // String in .dfn are ' delimited. Strings in .rcn are " delimited.
              if (TranslationStatus = tsUntranslated) then
              begin
                if (Length(SourceValue) < 2) or (not(SourceValue[1] in ['"', ''''])) then
                  TranslationStatus := tsDontTranslate;
              end;

            end;
            PropChild := NextPropChild;
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

        // Sanity check - don't add if node is invalid
        if (Localize) and (TranslationStatus in [tsEgged, tsUnknown]) then
        begin
          // Rescue "valid" translations without status or state
          if (TranslationStatus <> tsUnknown) or (TargetValue.IsEmpty) or (TargetValue = SourceValue) then
            Localize := False;
        end;

        if (Localize) then
        begin
          LocalizerItem := Result.AddItem(ItemName, ItemType);

          LocalizerProperty := LocalizerItem.AddProperty(PropertyName, SourceValue);
          case TranslationStatus of
            tsUnused:
              LocalizerProperty.State := ItemStateUnused;

            tsDontTranslate:
              LocalizerProperty.Status := ItemStatusDontTranslate;

            tsHold:
              LocalizerProperty.Status := ItemStatusHold;
          else
            LocalizerProperty.Status := ItemStatusTranslate;
          end;

          if (TargetNode <> nil) then
            {Translation := }LocalizerProperty.Translations.AddOrUpdateTranslation(TargetLanguage, TargetValue, TranslationStatusMap[TranslationStatus])
          else
          // Translation without target value (implicit: target=source)
          if (TranslationStatusMap[TranslationStatus] in [tStatusProposed, tStatusTranslated]) then
            {Translation := }LocalizerProperty.Translations.AddOrUpdateTranslation(TargetLanguage, SourceValue, TranslationStatusMap[TranslationStatus]);
        end;
      end;
    end;

    NextNode := Node.NextSibling;

    Node := NextNode;
  end;
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

end.
