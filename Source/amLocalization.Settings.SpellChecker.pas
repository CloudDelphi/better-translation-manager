unit amLocalization.Settings.SpellChecker;

interface

uses
  dxSpellChecker,
  amLocalization.Settings;

type
  TranslationManagerProofingSettingsAdapter = record
    class procedure SaveFrom(Settings: TTranslationManagerProofingSettings; SpellChecker: TdxSpellChecker); static;
    class procedure ApplyTo(Settings: TTranslationManagerProofingSettings; SpellChecker: TdxSpellChecker); static;
  end;

implementation

uses
  Classes,
  cxPropertiesStore,
  amRegConfig;

type
  TConfigurationSectionCracker = class(TConfigurationSection);

class procedure TranslationManagerProofingSettingsAdapter.ApplyTo(Settings: TTranslationManagerProofingSettings; SpellChecker: TdxSpellChecker);
begin
  var PropertiesStore := TcxPropertiesStore.Create(nil);
  try
    var PropertiesStoreComponent := PropertiesStore.Components.Add;
    PropertiesStoreComponent.Component := SpellChecker;

    PropertiesStoreComponent.Properties.Add('AutoCorrectOptions');
    PropertiesStoreComponent.Properties.Add('CheckAsYouTypeOptions');
    PropertiesStoreComponent.Properties.Add('SpellingOptions');

    PropertiesStoreComponent.RestoreFromRegistry(Settings.KeyPath);
  finally
    PropertiesStore.Free;
  end;


  if (not Settings.Valid) then
    Exit;

  var Stream := TMemoryStream.Create;
  try
    TConfigurationSectionCracker(Settings).ReadStream(Settings.KeyPath+'AutoCorrectOptions', 'FirstLetterExceptions', Stream);
    Stream.Position := 0;
    SpellChecker.AutoCorrectOptions.FirstLetterExceptions.LoadFromStream(Stream);
    Stream.Size := 0;

    TConfigurationSectionCracker(Settings).ReadStream(Settings.KeyPath+'AutoCorrectOptions', 'InitialCapsExceptions', Stream);
    Stream.Position := 0;
    SpellChecker.AutoCorrectOptions.InitialCapsExceptions.LoadFromStream(Stream);
    Stream.Size := 0;

    TConfigurationSectionCracker(Settings).ReadStream(Settings.KeyPath+'AutoCorrectOptions', 'Replacements', Stream);
    Stream.Position := 0;
    SpellChecker.AutoCorrectOptions.Replacements.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

class procedure TranslationManagerProofingSettingsAdapter.SaveFrom(Settings: TTranslationManagerProofingSettings; SpellChecker: TdxSpellChecker);
begin
  var PropertiesStore := TcxPropertiesStore.Create(nil);
  try
    var PropertiesStoreComponent := PropertiesStore.Components.Add;
    PropertiesStoreComponent.Component := SpellChecker;

    PropertiesStoreComponent.Properties.Add('AutoCorrectOptions');
    PropertiesStoreComponent.Properties.Add('CheckAsYouTypeOptions');
    PropertiesStoreComponent.Properties.Add('SpellingOptions');

    PropertiesStoreComponent.StoreToRegistry(Settings.KeyPath, False);
  finally
    PropertiesStore.Free;
  end;

  var Stream := TMemoryStream.Create;
  try
    SpellChecker.AutoCorrectOptions.FirstLetterExceptions.SaveToStream(Stream);
    Stream.Position := 0;
    TConfigurationSectionCracker(Settings).WriteStream(Settings.KeyPath+'AutoCorrectOptions', 'FirstLetterExceptions', Stream);
    Stream.Size := 0;

    SpellChecker.AutoCorrectOptions.InitialCapsExceptions.SaveToStream(Stream);
    Stream.Position := 0;
    TConfigurationSectionCracker(Settings).WriteStream(Settings.KeyPath+'AutoCorrectOptions', 'InitialCapsExceptions', Stream);
    Stream.Size := 0;

    SpellChecker.AutoCorrectOptions.Replacements.SaveToStream(Stream);
    Stream.Position := 0;
    TConfigurationSectionCracker(Settings).WriteStream(Settings.KeyPath+'AutoCorrectOptions', 'Replacements', Stream);
    Stream.Size := 0;
  finally
    Stream.Free;
  end;

  Settings.Valid := True;
end;

end.
