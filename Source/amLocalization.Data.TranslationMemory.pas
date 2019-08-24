unit amLocalization.Data.TranslationMemory;

interface

uses
  System.SysUtils, System.Classes, Data.DB,
  dxmdaset;

type
  TDataModuleTranslationMemory = class(TDataModule)
    TableTranslationMemory: TdxMemData;
    DataSourceTranslationMemory: TDataSource;
  private
  public
    procedure LoadTranslationMemory(const Filename: string);
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  Generics.Collections,
  Dialogs,
  IOUtils,
  Variants,
  Windows,
  XMLDoc, XMLIntf,
  amLocale,
  amLocalization.Model;

type
  TTerm = record
    Field: TField;
    Value: string;
  end;

  TTerms = TList<TTerm>;

procedure TDataModuleTranslationMemory.LoadTranslationMemory(const Filename: string);
var
  XML: IXMLDocument;
  Body: IXMLNode;
  ItemNode: IXMLNode;
  LanguageNode: IXMLNode;
//  Value: string;
  Language: string;
  Languages: TDictionary<string, TField>;
  Translations: TObjectList<TTerms>;
  Term: TTerm;
  Terms: TTerms;
  LocaleItem: TLocaleItem;
  i, j: integer;
  Field: TField;
begin
  XML := TXMLDocument.Create(nil);
  XML.Active := True;

  XML.LoadFromFile(Filename);

  if (XML.DocumentElement.NodeName <> 'tmx') then
    raise Exception.CreateFmt('XML document root node is not named "tmx": %s', [XML.DocumentElement.NodeName]);

  Body := XML.DocumentElement.ChildNodes.FindNode('body');
  if (Body = nil) then
    raise Exception.Create('xliff node not found: tmx\body');


  TableTranslationMemory.Close;
  TableTranslationMemory.Fields.Clear;

  Languages := TDictionary<string, TField>.Create(TTextComparer.Create);
  Translations := TObjectList<TTerms>.Create;
  try

    ItemNode := Body.ChildNodes.First;
    while (ItemNode <> nil) do
    begin
      if (ItemNode.NodeName = 'tu') then
      begin
        Terms := TTerms.Create;
        Translations.Add(Terms);

        LanguageNode := ItemNode.ChildNodes.First;
        while (LanguageNode <> nil) do
        begin
          if (LanguageNode.NodeName = 'tuv') then
          begin
            Language := LanguageNode.Attributes['xml:lang'];

            if (not Languages.TryGetValue(Language, Field)) then
            begin
              Field := TWideMemoField.Create(TableTranslationMemory);

              LocaleItem := TLocaleItems.FindLocaleName(Language);
              if (LocaleItem = nil) then
              begin
                ShowMessageFmt('Unknown language: %s', [Language]);
                Field.FieldName := Language;
              end else
              begin
                Field.FieldName := LocaleItem.LocaleSName;
                Field.DisplayLabel := LocaleItem.LanguageName;
                Field.Tag := LocaleItem.CharSet;
              end;

              Field.DataSet := TableTranslationMemory;
              Field.DisplayWidth := 100;

              Languages.Add(Language, Field);
            end;

            Term.Field := Field;
            Term.Value := VarToStr(LanguageNode.ChildValues['seg']);

            Terms.Add(Term);
          end;

          LanguageNode := LanguageNode.NextSibling;
        end;
      end;

      ItemNode := ItemNode.NextSibling;
    end;

    // Post all the terms to the dataset
    TableTranslationMemory.Open;

    for i := 0 to Translations.Count-1 do
    begin
      TableTranslationMemory.Append;
      try

        for j := 0 to Translations[i].Count-1 do
          Translations[i][j].Field.AsString := Translations[i][j].Value;

        TableTranslationMemory.Post;
      except
        TableTranslationMemory.Cancel;
        raise;
      end;
    end;

  finally
    Translations.Free;
    Languages.Free;
  end;
end;

end.
