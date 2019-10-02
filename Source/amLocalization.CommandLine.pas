unit amLocalization.CommandLine;

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
    FVerbose: boolean;
  protected
    procedure Message(const Msg: string);
    procedure Error(const Msg: string);
    procedure Warning(const Msg: string);

    procedure DoBuild(const AProjectFilename: string; TranslationLanguage: TTranslationLanguage);

    procedure LoadFromFile(const Filename: string);
    procedure Build(const AProjectFilename: string; const Language: string);
    procedure Help;
    procedure Header;
  public
    constructor Create(ALogger: ICommandLineLogger);
    destructor Destroy; override;

    procedure Execute;

    class function ProjectFilename: string;
    class function OptionBuild: boolean;
    class function OptionHelp: boolean;
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
  sCommandLineTitle = 'amTranslationManager resource module builder version %s';
  sCommandLineHelp = 'Usage: %s <projectfile> [options]'+#13#13+
    'Options:'+#13+
    '  -?               Display help (this message)'+#13+
    '  -t language      Only build for specified language'+#13+
    '  -b               Build resource module(s)'+#13+
    '  -v               Display verbose messages';

implementation

uses
  IOUtils,
  SysUtils,
  StrUtils,
  Windows,
  amLocale,
  amPath,
  amVersionInfo,
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

class function TLocalizationCommandLineTool.OptionBuild: boolean;
begin
  Result := (FindCmdLineSwitch('b')) or (FindCmdLineSwitch('build'));
end;

class function TLocalizationCommandLineTool.OptionHelp: boolean;
begin
  Result := (FindCmdLineSwitch('h')) or (FindCmdLineSwitch('?')) or (FindCmdLineSwitch('help'));
end;

class function TLocalizationCommandLineTool.ProjectFilename: string;
var
  i: integer;
begin
  // Filename should be first but look for it in all params anyway
  Result := '';
  for i := 1 to ParamCount do
    if (not CharInSet(ParamStr(1)[1], SwitchChars)) then
    begin
      Result := ParamStr(i);
      break;
    end;
end;

procedure TLocalizationCommandLineTool.Execute;
var
  Filename: string;
  Language: string;
begin
  Header;

  if (ParamCount < 1) or OptionHelp then
  begin
    Help;
    Exit;
  end;

  Filename := ProjectFilename;

  if (not TFile.Exists(Filename)) then
    Error(Format('Project file not found: %s', [Filename]));

  FVerbose := (FindCmdLineSwitch('v')) or (FindCmdLineSwitch('verbose'));

  // If command line specified a relative path then it must be relative to the "current folder"
  Filename := PathUtil.PathCombinePath(GetCurrentDir, Filename);

  LoadFromFile(Filename);

  if (not FindCmdLineSwitch('t', Language)) and (not FindCmdLineSwitch('target', Language)) then
    Language := '';

  if (OptionBuild) then
    Build(Filename, Language);

  Message('Done');
end;

procedure TLocalizationCommandLineTool.Header;
begin
  Message(Format(sCommandLineTitle, [TVersionInfo.FileVersionString(ParamStr(0))])+#13);
end;

procedure TLocalizationCommandLineTool.Help;
begin
  Message(Format(sCommandLineHelp, [TPath.GetFileNameWithoutExtension(ParamStr(0))]));
end;

procedure TLocalizationCommandLineTool.Build(const AProjectFilename: string; const Language: string);
var
  LocaleID: integer;
  LocaleItem: TLocaleItem;
  TranslationLanguage: TTranslationLanguage;
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

    TranslationLanguage := FProject.TranslationLanguages.Find(LocaleItem.Locale);

    if (TranslationLanguage = nil) then
      Error(Format('Project does not contain any translations for the language: %s (%s)', [Language, LocaleItem.LanguageName]));

    DoBuild(AProjectFilename, TranslationLanguage);
  end else
  begin
    for i := 0 to FProject.TranslationLanguages.Count-1 do
      if (FProject.TranslationLanguages[i].LanguageID <> FProject.SourceLanguageID) then
        DoBuild(AProjectFilename, FProject.TranslationLanguages[i]);
  end;
end;

procedure TLocalizationCommandLineTool.DoBuild(const AProjectFilename: string; TranslationLanguage: TTranslationLanguage);
var
  ProjectProcessor: TProjectResourceProcessor;
  ResourceWriter: IResourceWriter;
  LocaleItem: TLocaleItem;
  Filename: string;
begin
  LocaleItem := TLocaleItems.FindLCID(TranslationLanguage.LanguageID);
  Filename := TPath.ChangeExtension(FProject.SourceFilename, '.'+LocaleItem.LanguageShortName);

  Message(Format('Building resource module for %s: %s...', [LocaleItem.LanguageName, TPath.GetFileName(Filename)]));

  ProjectProcessor := TProjectResourceProcessor.Create;
  try
    ResourceWriter := TResourceModuleWriter.Create(Filename);
    try

      ProjectProcessor.Execute(liaTranslate, FProject, FProject.SourceFilename, TranslationLanguage, ResourceWriter);

    finally
      ResourceWriter := nil;
    end;
  finally
    ProjectProcessor.Free;
  end;
end;

procedure TLocalizationCommandLineTool.LoadFromFile(const Filename: string);
var
  LocaleItem: TLocaleItem;
  i: integer;
  CountItem, CountProperty: integer;
begin
  Message(Format('Loading project: %s...', [TPath.GetFileNameWithoutExtension(Filename)]));

  if (not TFile.Exists(Filename)) then
    Error(Format('Project file not found: %s', [Filename]));

  FProject.BeginUpdate;
  try

    TLocalizationProjectFiler.LoadFromFile(FProject, Filename);

  finally
    FProject.EndUpdate;
  end;

  // Make filenames absolute
  FProject.SourceFilename := PathUtil.PathCombinePath(TPath.GetDirectoryName(Filename), FProject.SourceFilename);
  FProject.StringSymbolFilename := PathUtil.PathCombinePath(TPath.GetDirectoryName(FProject.SourceFilename), FProject.StringSymbolFilename);

  if (FVerbose) then
  begin
    LocaleItem := TLocaleItems.FindLCID(FProject.SourceLanguageID);
    Message('Project information:');
    Message(Format('  Source file    : %s', [FProject.SourceFilename]));
    Message(Format('  Source Language: Locale=%.4X, Name=%s', [LocaleItem.Locale, LocaleItem.LanguageName]));
    Message(Format('  Symbol file    : %s', [FProject.StringSymbolFilename]));
    for i := 0 to FProject.TranslationLanguages.Count-1 do
    begin
      LocaleItem := TLocaleItems.FindLCID(FProject.TranslationLanguages[i].LanguageID);
      Message(Format('  Target language: Translated=%6.0n, Locale=%.4X, Name=%s', [FProject.TranslationLanguages[i].TranslatedCount*1.0, LocaleItem.Locale, LocaleItem.LanguageName]));
    end;
    CountItem := 0;
    CountProperty := 0;
    FProject.Traverse(
      function(Item: TLocalizerItem): boolean
      begin
        Inc(CountItem);
        Item.Traverse(
          function(Prop: TLocalizerProperty): boolean
          begin
            Inc(CountProperty);
            Result := True;
          end, False);
        Result := True;
      end, False);
    Message(Format('  Modules        : %6.0n', [FProject.Modules.Count*1.0]));
    Message(Format('  Items          : %6.0n', [CountItem*1.0]));
    Message(Format('  Properties     : %6.0n', [CountProperty*1.0]));
  end;

  if (not TFile.Exists(FProject.StringSymbolFilename)) then
    Warning(Format('String symbol file not found: %s', [FProject.StringSymbolFilename]));

  if (not TFile.Exists(FProject.SourceFilename)) then
    Error(Format('Source file not found: %s', [FProject.SourceFilename]));
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
