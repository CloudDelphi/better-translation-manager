unit amLocalization.Translator;

interface

uses
  amLocale,
  amLocalization.Model;

type
  ITranslationService = interface
    ['{254E24D5-8F3C-422E-9304-1E21126C8B3C}']
    procedure BeginLookup(SourceLanguage, TargetLanguage: TLocaleItem);
    procedure EndLookup;
    function Lookup(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLocaleItem; const SourceValue: string; var TargetValue: string): boolean;

    function GetServiceName: string;
    property ServiceName: string read GetServiceName;
  end;

implementation

end.
