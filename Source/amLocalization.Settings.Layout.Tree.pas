unit amLocalization.Settings.Layout.Tree;

interface

uses
  cxCustomData,
  amLocalization.Settings;

type
  TranslationManagerLayoutTreeSettingsAdapter = record
    class procedure WriteFilter(Settings: TTranslationManagerLayoutTreeSettings; Filter: TcxDataFilterCriteria); static;
    class procedure ReadFilter(Settings: TTranslationManagerLayoutTreeSettings; Filter: TcxDataFilterCriteria); static;
  end;

implementation

uses
  Classes,
  amRegConfig;

type
  TConfigurationSectionCracker = class(TConfigurationSection);

class procedure TranslationManagerLayoutTreeSettingsAdapter.ReadFilter(Settings: TTranslationManagerLayoutTreeSettings;
  Filter: TcxDataFilterCriteria);
begin
  var Stream := TMemoryStream.Create;
  try
    TConfigurationSectionCracker(Settings).ReadStream(Settings.KeyPath+'Filter', '', Stream);
    Stream.Position := 0;
    if (Stream.Size > 0) then
      Filter.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

class procedure TranslationManagerLayoutTreeSettingsAdapter.WriteFilter(Settings: TTranslationManagerLayoutTreeSettings;
  Filter: TcxDataFilterCriteria);
begin
  var Stream := TMemoryStream.Create;
  try
    Filter.SaveToStream(Stream);
    Stream.Position := 0;
    TConfigurationSectionCracker(Settings).WriteStream(Settings.KeyPath+'Filter', '', Stream);
  finally
    Stream.Free;
  end;
end;

end.
