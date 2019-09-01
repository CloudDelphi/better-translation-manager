unit amLocalization.CommandLine;

interface

uses
  Classes,
  amLocalization.Model;

type
  ICommandLineLogger = interface
    procedure Message(const Msg: string);
    procedure Error(const Msg: string);
    procedure Warning(const Msg: string);
  end;

type
  TLocalizationCommandLineTool = class
  private
    FLogger: ICommandLineLogger;
    FProject: TLocalizerProject;
  protected
    procedure Message(const Msg: string);
    procedure Error(const Msg: string);
    procedure Warning(const Msg: string);

    procedure DoBuild(TargetLanguage: TTargetLanguage);

    procedure LoadFromFile(const Filename: string);
    procedure Build(const Language: string);
    procedure Help;
    procedure Header;
  public
    constructor Create(ALogger: ICommandLineLogger);
    destructor Destroy; override;

    procedure Execute;
  end;

type
  TCommandLineLogger = class(TInterfacedObject, ICommandLineLogger)
  private
    // ICommandLineLogger
    procedure Message(const Msg: string);
    procedure Error(const Msg: string);
    procedure Warning(const Msg: string);
  end;

resourcestring
  sCommandLineTitle = 'amTranslationManager resource module builder';
  sCommandLineHelp = 'Usage: %s <projectfile> [options]'+#13#13+
    'Options:'+#13+
    '  -?               Display help (this message)'+#13+
    '  -t language      Only build for specified language'+#13+
    '  -b               Build resource module(s)';

implementation

uses
  IOUtils,
  SysUtils,
  StrUtils,
  Windows,
  amLocale,
  amLocalization.Persistence,
  amLocalization.ResourceWriter,
  amLocalization.Engine;


{ TLocalizationCommandLine }

constructor TLocalizationCommandLineTool.Create(ALogger: ICommandLineLogger);
begin
  inherited Create;
  FLogger := ALogger;
  FProject := TLocalizerProject.Create('', GetUserDefaultUILanguage);
end;

destructor TLocalizationCommandLineTool.Destroy;
begin
  FProject.Free;
  inherited;
end;

procedure TLocalizationCommandLineTool.Message(const Msg: string);
begin
  if (FLogger <> nil) then
    FLogger.Message(Msg+#13);
end;

procedure TLocalizationCommandLineTool.Error(const Msg: string);
begin
  if (FLogger <> nil) then
    FLogger.Error(Msg+#13);
  Halt(1);
end;

procedure TLocalizationCommandLineTool.Warning(const Msg: string);
begin
  if (FLogger <> nil) then
    FLogger.Warning(Msg+#13);
end;

procedure TLocalizationCommandLineTool.Execute;
var
  Filename: string;
  Language: string;
begin
  Header;

  if (ParamCount < 1) or (FindCmdLineSwitch('h')) or (FindCmdLineSwitch('?')) or (FindCmdLineSwitch('help')) then
  begin
    Help;
    Exit;
  end;

  Filename := ParamStr(1);

  if (not TFile.Exists(Filename)) then
    Error(Format('Project file not found: %s', [Filename]));

  LoadFromFile(Filename);

  if (not FindCmdLineSwitch('t', Language)) then
    Language := '';

  if (FindCmdLineSwitch('b')) then
    Build(Language);

  Message('Done');
end;

procedure TLocalizationCommandLineTool.Header;
begin
  Message(sCommandLineTitle+#13);
end;

procedure TLocalizationCommandLineTool.Help;
begin
  Message(Format(sCommandLineHelp, [TPath.GetFileNameWithoutExtension(ParamStr(0))]));
end;

procedure TLocalizationCommandLineTool.Build(const Language: string);
var
  LocaleID: integer;
  LocaleItem: TLocaleItem;
  TargetLanguage: TTargetLanguage;
  i: integer;
begin
  if (Language <> '') then
  begin
    if (TryStrToInt(Language, LocaleID)) then
      LocaleItem := TLocaleItems.FindLCID(LocaleID)
    else
    begin
      LocaleItem := TLocaleItems.FindLocaleName(Language);
      if (LocaleItem = nil) then
        LocaleItem := TLocaleItems.FindLanguageShortName(Language);
      if (LocaleItem = nil) then
        LocaleItem := TLocaleItems.FindISO639_1Name(Language);
      if (LocaleItem = nil) then
        LocaleItem := TLocaleItems.FindISO3166Name(Language);
    end;

    if (LocaleItem = nil) then
      Error(Format('Unknown target language: %s', [Language]));

    TargetLanguage := FProject.TargetLanguages.Find(LocaleItem.Locale);

    if (TargetLanguage = nil) then
      Error(Format('Project does not contain any translations for the language: %s (%s)', [Language, LocaleItem.LanguageName]));

    DoBuild(TargetLanguage);
  end else
  begin
    for i := 0 to FProject.TargetLanguages.Count-1 do
      if (FProject.TargetLanguages[i].LanguageID <> FProject.BaseLocaleID) then
        DoBuild(FProject.TargetLanguages[i]);
  end;
end;

procedure TLocalizationCommandLineTool.DoBuild(TargetLanguage: TTargetLanguage);
var
  ProjectProcessor: TProjectResourceProcessor;
  ResourceWriter: IResourceWriter;
  LocaleItem: TLocaleItem;
  Filename: string;
begin
  LocaleItem := TLocaleItems.FindLCID(TargetLanguage.LanguageID);
  Filename := TPath.ChangeExtension(FProject.SourceFilename, '.'+LocaleItem.LanguageShortName);

  Message(Format('Building resource module for %s: %s...', [LocaleItem.LanguageName, TPath.GetFileName(Filename)]));

  ProjectProcessor := TProjectResourceProcessor.Create;
  try
    ResourceWriter := TResourceModuleWriter.Create(Filename);
    try

      ProjectProcessor.Execute(liaTranslate, FProject, FProject.SourceFilename, TargetLanguage, ResourceWriter);

    finally
      ResourceWriter := nil;
    end;
  finally
    ProjectProcessor.Free;
  end;
end;

procedure TLocalizationCommandLineTool.LoadFromFile(const Filename: string);
begin
  Message(Format('Loading project: %s...', [TPath.GetFileNameWithoutExtension(Filename)]));

  FProject.BeginUpdate;
  try

    TLocalizationProjectFiler.LoadFromFile(FProject, Filename);

  finally
    FProject.EndUpdate;
  end;
end;

{ TCommandLineLogger }

procedure TCommandLineLogger.Error(const Msg: string);
var
  s: string;
begin
  for s in Format('Error: %s', [Msg]).Split([#13]) do
    Writeln(ErrOutput, s);
  Halt(1);
end;

procedure TCommandLineLogger.Message(const Msg: string);
var
  s: string;
begin
  for s in Msg.Split([#13]) do
    Writeln(Output, s);
end;

procedure TCommandLineLogger.Warning(const Msg: string);
var
  s: string;
begin
  for s in Format('Warning: %s', [Msg]).Split([#13]) do
    Writeln(ErrOutput, s);
end;

end.
