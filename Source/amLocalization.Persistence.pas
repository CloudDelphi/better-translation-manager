unit amLocalization.Persistence;

interface

uses
  Classes,
  amLocalization.Model;

// -----------------------------------------------------------------------------
//
// TLocalizationProjectFiler
//
// -----------------------------------------------------------------------------
// Save and load project.
// -----------------------------------------------------------------------------
type
  TLocalizationProjectFiler = class
  private
    const
      sModuleKind: array[TLocalizerModuleKind] of string = ('invalid', 'form', 'strings');
      sTranslationStatus: array[TTranslationStatus] of string = ('obsolete', 'pending', 'proposed', '');
      sItemState: array[TLocalizerItemState] of string = ('new', '', 'unused');
      sItemStatus: array[TLocalizerItemStatus] of string = ('', 'hold', 'skip');
  private
    class function EncodeString(const Value: string): string;
    class function DecodeString(const Value: string): string;
  public
    class procedure LoadFromStream(Project: TLocalizerProject; Stream: TStream);
    class procedure LoadFromFile(Project: TLocalizerProject; const Filename: string);

    class procedure SaveToStream(Project: TLocalizerProject; Stream: TStream);
    class procedure SaveToFile(Project: TLocalizerProject; const Filename: string);
  end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  SysUtils,
  DateUtils,
  IOUtils,
  Windows,
  Variants,
  XMLDoc, XMLIntf,
  amLocale;

// -----------------------------------------------------------------------------
//
// TLocalizationProjectFiler
//
// -----------------------------------------------------------------------------
class function TLocalizationProjectFiler.DecodeString(const Value: string): string;
begin
  Result := StringReplace(Value, '##', '#', [rfReplaceAll]);
  Result := StringReplace(Result, '#13', #13, [rfReplaceAll]);
  Result := StringReplace(Result, '#10', #10, [rfReplaceAll]);
  Result := StringReplace(Result, '#9', #9, [rfReplaceAll]);
end;

class function TLocalizationProjectFiler.EncodeString(const Value: string): string;
begin
  Result := StringReplace(Value, '#', '##', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '#13', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '#10', [rfReplaceAll]);
  Result := StringReplace(Result, #9, '#9', [rfReplaceAll]);
end;

// -----------------------------------------------------------------------------

class procedure TLocalizationProjectFiler.LoadFromFile(Project: TLocalizerProject; const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead);
  try
    LoadFromStream(Project, Stream);
  finally
    Stream.Free;
  end;
end;

// -----------------------------------------------------------------------------

class procedure TLocalizationProjectFiler.LoadFromStream(Project: TLocalizerProject; Stream: TStream);

  function StringToModuleKind(const Value: string): TLocalizerModuleKind;
  begin
    for Result := Low(Result) To High(Result) do
      if (Value = sModuleKind[Result]) then
        Exit;
    Result := Low(Result);
  end;

  function StringToTranslationStatus(const Value: string): TTranslationStatus;
  begin
    for Result := Low(Result) To High(Result) do
      if (Value = sTranslationStatus[Result]) then
        Exit;
    Result := Low(Result);
  end;

  function StringToItemState(const Value: string): TLocalizerItemState;
  begin
    for Result := Low(Result) To High(Result) do
      if (Value = sItemState[Result]) then
        Exit;
    Result := Low(Result);
  end;

  function StringToItemStatus(const Value: string): TLocalizerItemStatus;
  begin
    for Result := Low(Result) To High(Result) do
      if (Value = sItemStatus[Result]) then
        Exit;
    Result := Low(Result);
  end;

var
  XML: IXMLDocument;
  RootNode, ProjectNode: IXMLNode;
  LanguagesNode, LanguageNode: IXMLNode;
  ModulesNode, ModuleNode: IXMLNode;
  ItemsNode, ItemNode: IXMLNode;
  PropsNode, PropNode: IXMLNode;
  XlatsNode, XlatNode: IXMLNode;
  Module: TLocalizerModule;
  Item: TLocalizerItem;
  Prop: TLocalizerProperty;
  Language: TTargetLanguage;
  CachedLanguage: string;
//  Translation: TLocalizerTranslation;
  TranslationStatus: TTranslationStatus;
  s: string;
begin
  XML := TXMLDocument.Create(nil);
  XML.Options := XML.Options + [doAttrNull];
  XML.ParseOptions := XML.ParseOptions + [poPreserveWhiteSpace];
  XML.LoadFromStream(Stream);

  RootNode := XML.ChildNodes['delphi_l10n'];

  ProjectNode := RootNode.ChildNodes['project'];

  Language := nil;
  CachedLanguage := '';

  Project.BeginUpdate;
  try
    Project.Clear;

    Project.Name := VarToStr(ProjectNode.Attributes['name']);

    Project.SourceFilename := VarToStr(ProjectNode.Attributes['sourcefile']);
    if (Project.SourceFilename.IsEmpty) then
      Project.SourceFilename := Project.Name;
    if (TPath.GetExtension(Project.SourceFilename).IsEmpty) then
      Project.SourceFilename := Project.SourceFilename + '.exe';

    Project.BaseLocaleID := StrToIntDef(VarToStr(ProjectNode.Attributes['language']), 0);

    LanguagesNode := ProjectNode.ChildNodes.FindNode('targetlanguages');
    if (LanguagesNode <> nil) then
      LanguageNode := LanguagesNode.ChildNodes.First
    else
      LanguageNode := nil;

    // Precreate target languages.
    // This ensures that they are present in the language list even if there are no translations for them yet.
    while (LanguageNode <> nil) do
    begin
      if (LanguageNode.NodeName = 'language') then
      begin
        s := VarToStr(LanguageNode.Attributes['language']);
        if (Language = nil) or (s <> CachedLanguage) then
          Language := Project.TargetLanguages.Add(StrToIntDef(s, 0));
      end;
      LanguageNode := LanguageNode.NextSibling;
    end;

    ModulesNode := ProjectNode.ChildNodes.FindNode('modules');
    if (ModulesNode = nil) then
      exit;

    ModuleNode := ModulesNode.ChildNodes.First;
    while (ModuleNode <> nil) do
    begin
      if (ModuleNode.NodeName = 'module') then
      begin
        Module := Project.AddModule(VarToStr(ModuleNode.Attributes['name']));
        Module.Kind := StringToModuleKind(VarToStr(ModuleNode.Attributes['type']));
        Module.State := StringToItemState(VarToStr(ModuleNode.Attributes['state']));
        Module.Status := StringToItemStatus(VarToStr(ModuleNode.Attributes['status']));

        ItemsNode := ModuleNode.ChildNodes.FindNode('items');
        if (ItemsNode <> nil) then
        begin
          ItemNode := ItemsNode.ChildNodes.First;
          while (ItemNode <> nil) do
          begin
            if (ItemNode.NodeName = 'item') then
            begin
              Item := Module.AddItem(VarToStr(ItemNode.Attributes['name']), VarToStr(ItemNode.Attributes['type']));
              Item.ResourceID := StrToIntDef(VarToStr(ItemNode.Attributes['id']), 0);
              Item.State := StringToItemState(VarToStr(ItemNode.Attributes['state']));
              Item.Status := StringToItemStatus(VarToStr(ItemNode.Attributes['status']));

              PropsNode := ItemNode.ChildNodes.FindNode('properties');
              if (PropsNode <> nil) then
              begin
                PropNode := PropsNode.ChildNodes.First;
                while (PropNode <> nil) do
                begin
                  if (PropNode.NodeName = 'property') then
                  begin
                    s := VarToStr(PropNode.ChildValues['value']);
                    // s := s.Replace(#10, #13);
                    s := DecodeString(s);
                    Prop := Item.AddProperty(VarToStr(PropNode.Attributes['name']), s);
                    Prop.State := StringToItemState(VarToStr(PropNode.Attributes['state']));
                    Prop.Status := StringToItemStatus(VarToStr(PropNode.Attributes['status']));

                    XlatsNode := PropNode.ChildNodes.FindNode('translations');
                    if (XlatsNode <> nil) then
                    begin
                      XlatNode := XlatsNode.ChildNodes.First;
                      while (XlatNode <> nil) do
                      begin
                        if (XlatNode.NodeName = 'translation') then
                        begin
                          s := VarToStr(XlatNode.Attributes['language']);
                          if (Language = nil) or (s <> CachedLanguage) then
                            Language := Project.TargetLanguages.Add(StrToIntDef(s, 0));
                          TranslationStatus := StringToTranslationStatus(VarToStr(XlatNode.Attributes['status']));
                          s := XlatNode.Text;
                          // s := s.Replace(#10, #13);
                          s := DecodeString(s);
                          {Translation :=} Prop.Translations.AddOrUpdateTranslation(Language, s, TranslationStatus);
                        end;
                        XlatNode := XlatNode.NextSibling;
                      end;
                    end;
                  end;
                  PropNode := PropNode.NextSibling;
                end;
              end;
            end;
            ItemNode := ItemNode.NextSibling;
          end;
        end;
      end;
      ModuleNode := ModuleNode.NextSibling;
    end;
  finally
    Project.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

class procedure TLocalizationProjectFiler.SaveToFile(Project: TLocalizerProject; const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(Project, Stream);
  finally
    Stream.Free;
  end;
end;

// -----------------------------------------------------------------------------

class procedure TLocalizationProjectFiler.SaveToStream(Project: TLocalizerProject; Stream: TStream);

  procedure WriteItemState(const Node: IXMLNode; Item: TCustomLocalizerItem);
  begin
    if (not Item.InheritParentState) and (Item.State <> ItemStateExisting) then
      Node.Attributes['state'] := sItemState[Item.State];
  end;

  procedure WriteItemStatus(const Node: IXMLNode; Item: TCustomLocalizerItem);
  begin
    if (Item.Status <> ItemStatusTranslate) then
      Node.Attributes['status'] := sItemStatus[Item.Status];
  end;

  procedure WriteItemName(const Node: IXMLNode; Item: TCustomLocalizerItem);
  begin
    if (not Item.Name.IsEmpty) then
      Node.Attributes['name'] := Item.Name;
  end;

  procedure WriteItemID(const Node: IXMLNode; ID: Word);
  begin
    if (ID <> 0) then
      Node.Attributes['id'] := ID;
  end;

var
  XML: IXMLDocument;
  RootNode, Node: IXMLNode;
  ProjectNode: IXMLNode;
  LanguagesNode, LanguageNode: IXMLNode;
  ModulesNode, ModuleNode: IXMLNode;
  ItemsNode, ItemNode: IXMLNode;
  PropsNode, PropNode: IXMLNode;
  XlatsNode, XlatNode: IXMLNode;
  Module: TLocalizerModule;
  Item: TLocalizerItem;
  Prop: TLocalizerProperty;
  i: integer;
begin
  XML := TXMLDocument.Create(nil);
  XML.Options := [doNodeAutoIndent];
  XML.Active := True;

  RootNode := XML.AddChild('delphi_l10n');

  Node := RootNode.AddChild('meta');
  Node.AddChild('version').Text := '1';
  Node.AddChild('created').Text := DateToISO8601(Now, False);

  ProjectNode := RootNode.AddChild('project');
  ProjectNode.Attributes['name'] := Project.Name;
  ProjectNode.Attributes['sourcefile'] := TPath.GetFileName(Project.SourceFilename);
  ProjectNode.Attributes['language'] := Project.BaseLocaleID;

  LanguagesNode := ProjectNode.AddChild('targetlanguages');
  for i := 0 to Project.TargetLanguages.Count-1 do
  begin
    LanguageNode := LanguagesNode.AddChild('language');
    LanguageNode.Attributes['language'] := Project.TargetLanguages[i].LanguageID;
    LanguageNode.Attributes['translated'] := Project.TargetLanguages[i].TranslatedCount;
  end;

  ModulesNode := ProjectNode.AddChild('modules');

  for Module in Project.Modules.Values do
  begin
    ModuleNode := ModulesNode.AddChild('module');
    WriteItemName(ModuleNode, Module);
    ModuleNode.Attributes['type'] := sModuleKind[Module.Kind];
    WriteItemState(ModuleNode, Module);
    WriteItemStatus(ModuleNode, Module);

    ItemsNode := ModuleNode.AddChild('items');

    for Item in Module.Items.Values do
    begin
      ItemNode := ItemsNode.AddChild('item');
      WriteItemName(ItemNode, Item);
      WriteItemID(ItemNode, Item.ResourceID);
      if (Item.TypeName <> '') then
        ItemNode.Attributes['type'] := Item.TypeName;
      WriteItemState(ItemNode, Item);
      WriteItemStatus(ItemNode, Item);

      PropsNode := ItemNode.AddChild('properties');

      for Prop in Item.Properties.Values do
      begin
        PropNode := PropsNode.AddChild('property');
        WriteItemName(PropNode, Prop);
        WriteItemState(PropNode, Prop);
        WriteItemStatus(PropNode, Prop);

        PropNode.AddChild('value').Text := EncodeString(Prop.Value);

        XlatsNode := nil;
        Prop.Traverse(
          function(Prop: TLocalizerProperty; Translation: TLocalizerTranslation): boolean
          begin
            if (XlatsNode = nil) then
              XlatsNode :=  PropNode.AddChild('translations');

            XlatNode := XlatsNode.AddChild('translation');
            XlatNode.Attributes['language'] := Translation.Language.LanguageID;
            if (Translation.Status <> tStatusTranslated) then
              XlatNode.Attributes['status'] := sTranslationStatus[Translation.Status];
            XlatNode.Text := EncodeString(Translation.Value);
            Result := True;
          end);
      end;
    end;
  end;

  XML.SaveToStream(Stream);
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

end.
