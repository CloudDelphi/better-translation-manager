unit amLocalization.Translator.TM;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

{.$define TM_BENCHMARK}

uses
  Generics.Collections,
  SyncObjs,
  System.SysUtils, System.Classes, Windows, Data.DB,

  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client,

  amLocale,
  amProgress,
  amLocalization.Model,
  amLocalization.Translator;


type
  TTranslationMemoryMergeStats = record
    Added: integer;             // New row added with value
    Merged: integer;            // Value merged into existing row
    Skipped: integer;           // Value skipped (e.g. empty value)
    Duplicate: integer;         // Exact duplicate found
  end;

  TTranslationMemoryDuplicateAction = (
    tmDupActionPrompt,          // Prompt to select action
    tmDupActionAbort,           // Abort import
    tmDupActionAcceptAll,       // Accept all duplicates
    tmDupActionRejectAll        // Reject all duplicates
  );

type
  TDuplicate = record
    SourceValue: string;
    Value: string;
    RecordID: int64;
  end;
  TDuplicateValues = class(TList<TDuplicate>)
  public
    EmptyCount: integer;
  end;

  TDuplicateTerms = TObjectDictionary<string, TDuplicateValues>;

  TDuplicates = TObjectDictionary<TField, TDuplicateTerms>;

type
  ITranslationMemoryPeek = interface
    ['{8D4D5538-3A16-4220-9FDF-F88B8A7DED6C}']
    procedure EnqueueQuery(Prop: TLocalizerProperty);
    procedure Cancel;
  end;

  ITranslationMemory = interface
    ['{8D4D5538-3A16-4220-9FDF-F88B8A7DED6C}']
    function CreateField(LocaleItem: TLocaleItem): TField;
    function SaveTableTranslationMemoryClone: IInterface;
    function AddTerm(SourceField: TField; const SourceValue, SanitizedSourceValue: string; TargetField: TField; const TargetValue: string;
      Duplicates: TDuplicates; var Stats: TTranslationMemoryMergeStats; DuplicateAction: TTranslationMemoryDuplicateAction): TTranslationMemoryDuplicateAction;
    procedure Lock;
    procedure Unlock;
    function GetTableTranslationMemory: TDataSet;
    property TableTranslationMemory: TDataSet read GetTableTranslationMemory;
  end;

// -----------------------------------------------------------------------------
//
// TDataModuleTranslationMemory
//
// -----------------------------------------------------------------------------
type
  TDataModuleTranslationMemory = class(TDataModule, ITranslationProvider, ITranslationMemory)
    DataSourceTranslationMemory: TDataSource;
    TableTranslationMemory: TFDMemTable;
    procedure TableTranslationMemoryAfterModify(DataSet: TDataSet);
  private
    FLoaded: boolean;
    FEnabled: boolean;
    FLookupIndex: TDictionary<string, TList<integer>>;
    FModified: boolean;
    FRefreshEvent: TEvent;
    FProviderHandle: integer;
  private
    function FindField(LocaleItem: TLocaleItem): TField;
    function GetHasData: boolean;
    procedure FieldGetTextEventHandler(Sender: TField; var Text: string; DisplayText: Boolean);
    function GetAvailable: boolean;
    function AddTerm(SourceField: TField; const SourceValue, SanitizedSourceValue: string; TargetField: TField; const TargetValue: string;
      Duplicates: TDuplicates; var Stats: TTranslationMemoryMergeStats; DuplicateAction: TTranslationMemoryDuplicateAction): TTranslationMemoryDuplicateAction;
    function FindTranslations(Prop: TLocalizerProperty; TargetField: TField; Translations: TStrings): boolean; overload;
    procedure AddMatch(Translations: TStrings; Prop: TLocalizerProperty; const SourceValue, TargetValue: string);
  protected
    // Threaded lookup
    procedure PopulateDictionary(SourceField, TargetField: TField; Dictionary: TStringList);
    // Protect against TM modifications. It is assumed that the TM table properties (data, fields, etc) are stable outside the lock.
    procedure Lock;
    procedure Unlock;

    property RefreshEvent: TEvent read FRefreshEvent;
  protected
    // ITranslationMemory
    function CreateField(LocaleItem: TLocaleItem): TField;
    function SaveTableTranslationMemoryClone: IInterface;
    function GetTableTranslationMemory: TDataSet;
    function ITranslationMemory.AddTerm = AddTerm;
    procedure ITranslationMemory.Lock = Lock;
    procedure ITranslationMemory.Unlock = Unlock;
  protected
    // ITranslationProvider
    function BeginLookup(SourceLanguage, TargetLanguage: TLocaleItem): boolean;
    function Lookup(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLocaleItem; Translations: TStrings): boolean;
    procedure EndLookup;
    function GetProviderName: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const Filename: string);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const Filename: string);

    function Add(SourceLanguage: Word; const SourceValue: string; TargetLanguage: Word; const TargetValue: string; var Stats: TTranslationMemoryMergeStats; DuplicateAction: TTranslationMemoryDuplicateAction = tmDupActionPrompt): TTranslationMemoryDuplicateAction; overload;
    function Add(SourceField: TField; const SourceValue: string; TargetField: TField; const TargetValue: string; var Stats: TTranslationMemoryMergeStats; DuplicateAction: TTranslationMemoryDuplicateAction = tmDupActionPrompt): TTranslationMemoryDuplicateAction; overload;

    function CheckSave: boolean;
    function CheckLoaded(Force: boolean = False): boolean;
    procedure SetLoaded; // For use when loading TMX as main TM

    function CreateBackgroundLookup(SourceLanguage, TargetLanguage: LCID; AResultHandler: TNotifyEvent): ITranslationMemoryPeek;
    function FindTranslations(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLocaleItem; Translations: TStrings): boolean; overload;

    property IsLoaded: boolean read FLoaded;
    property IsAvailable: boolean read GetAvailable;
    property HasData: boolean read GetHasData;
    property Modified: boolean read FModified;
    property Enabled: boolean read FEnabled write FEnabled;
  end;

type
  ETranslationMemory = class(Exception);
  ETranslationMemoryTMX = class(ETranslationMemory);

// -----------------------------------------------------------------------------

const
  sTranslationMemoryFilename = 'TranslationMemory.dat';

resourcestring
  sTranslationMemoryLoading = 'Loading Translation Memory';
  sTranslationMemoryLoad = 'Loading...';
  sTranslationMemoryReadingTerms = 'Reading terms...';
  sTranslationMemoryIndexingTerms = 'Indexing terms...';
  sTranslationMemoryAddingTerms = 'Adding terms...';
  sTranslationMemoryLoadingTerms = 'Loading terms...';
  sTranslationMemorySaving = 'Saving Translation Memory';

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  UITypes,
  Dialogs,
  Controls,
  IOUtils,
  DateUtils,
  StrUtils,
  Variants,
  XMLDoc, XMLIntf,
  Forms,
  Diagnostics,
  cxGraphics,
  amCursorService,
  amVersionInfo,
  amFileUtils,
  amLocalization.Settings,
  amLocalization.Utils;

resourcestring
  sTranslatorNameTM = 'Translation Memory';

const
  sTMFileSignature: AnsiString = 'amTranslationManagerTM';

  TMFileVersionMajor = 2;
  TMFileVersionMinor = 1;

type
  TTMStreamTag = (tmDescription, tmLanguages, tmTerms);


// -----------------------------------------------------------------------------
//
// TPeekDictionaryThread
//
// -----------------------------------------------------------------------------
type
  TPeekDictionaryThread = class(TThread)
  private
    FTranslationMemory: TDataModuleTranslationMemory;
    FSourceField, FTargetField: TField;
    FQueryQueue: TList<TLocalizerProperty>;
    FResultQueue: TList<TLocalizerProperty>;
    FQueueEvent: TEvent;
    FDictionary: TStringList;
    FResultHandler: TNotifyEvent;
  protected
    procedure Execute; override;
    procedure TerminatedSet; override;

    procedure Prepare;
    procedure ResultReady;
  public
    constructor Create(ATranslationMemory: TDataModuleTranslationMemory; ASourceField, ATargetField: TField; AResultHandler: TNotifyEvent);
    destructor Destroy; override;

    procedure EnqueueQuery(Prop: TLocalizerProperty);
    procedure ClearQueues;

    function DequeueResult: TLocalizerProperty;
  end;

constructor TPeekDictionaryThread.Create(ATranslationMemory: TDataModuleTranslationMemory; ASourceField, ATargetField: TField; AResultHandler: TNotifyEvent);
begin
  inherited Create(True);

  FTranslationMemory := ATranslationMemory;
  FResultHandler := AResultHandler;
  FSourceField := ASourceField;
  FTargetField := ATargetField;

  FQueryQueue := TList<TLocalizerProperty>.Create;
  FResultQueue := TList<TLocalizerProperty>.Create;
  FQueueEvent := TEvent.Create(nil, False, False, '');
  FDictionary := TStringList.Create;
  FDictionary.CaseSensitive := False;
end;

destructor TPeekDictionaryThread.Destroy;
begin
  FQueryQueue.Free;
  FResultQueue.Free;
  FQueueEvent.Free;
  FDictionary.Free;
  inherited;
end;

procedure TPeekDictionaryThread.ClearQueues;
begin
  System.TMonitor.Enter(FQueryQueue);
  try
    FQueryQueue.Clear;
  finally
    System.TMonitor.Exit(FQueryQueue);
  end;

  System.TMonitor.Enter(FResultQueue);
  try
    FResultQueue.Clear;
  finally
    System.TMonitor.Exit(FResultQueue);
  end;
end;

function TPeekDictionaryThread.DequeueResult: TLocalizerProperty;
begin
  System.TMonitor.Enter(FResultQueue);
  try
    if (FResultQueue.Count > 0) then
    begin
      Result := FResultQueue.Last;
      FResultQueue.Count := FResultQueue.Count-1;
    end else
      Result := nil;
  finally
    System.TMonitor.Exit(FResultQueue);
  end;
end;

procedure TPeekDictionaryThread.EnqueueQuery(Prop: TLocalizerProperty);
begin
  System.TMonitor.Enter(FQueryQueue);
  try
    if (FQueryQueue.Contains(Prop)) then
      Exit;

    FQueryQueue.Add(Prop);
  finally
    System.TMonitor.Exit(FQueryQueue);
  end;
  // Signal thread that there's work to do
  FQueueEvent.SetEvent;
end;

procedure TPeekDictionaryThread.Execute;
var
  Prop: TLocalizerProperty;
  WaitObject: THandleObject;
  ProcessQueue: boolean;
begin

  // Initial index load
  Prepare;

  FQueueEvent.SetEvent;

  while (THandleObject.WaitForMultiple([FTranslationMemory.RefreshEvent, FQueueEvent], INFINITE, False, WaitObject) = wrSignaled) and (not Terminated) do
  begin
    if (WaitObject = FTranslationMemory.RefreshEvent) then
    begin
      // TM has changed. Reload index.
      Prepare;
      continue;
    end else
    if (WaitObject <> FQueueEvent) then
      continue;

    ProcessQueue := True;

    while (ProcessQueue) do
    begin

      // Dequeue a job
      System.TMonitor.Enter(FQueryQueue);
      try
        if (FQueryQueue.Count = 0) then
        begin
          // Queue is empty - wait for a query to be queued
          ProcessQueue := False;
          continue;
        end;

        Prop := FQueryQueue.Last;
        FQueryQueue.Count := FQueryQueue.Count-1;
      finally
        System.TMonitor.Exit(FQueryQueue);
      end;

      // Perform lookup
      System.TMonitor.Enter(FDictionary);
      try
        if (FDictionary.IndexOf(SanitizeText(Prop.Value, False)) = -1) then
          continue;
      finally
        System.TMonitor.Exit(FDictionary);
      end;

      // Found match - put in result queue
      System.TMonitor.Enter(FResultQueue);
      try
        FResultQueue.Add(Prop);
      finally
        System.TMonitor.Exit(FResultQueue);
      end;

      // Notify client
      if (Assigned(FResultHandler)) then
        Queue(ResultReady);

    end;
  end;
end;

procedure TPeekDictionaryThread.Prepare;
begin
  System.TMonitor.Enter(FDictionary);
  try

    FDictionary.Sorted := False;

    FTranslationMemory.PopulateDictionary(FSourceField, FTargetField, FDictionary);

    FDictionary.Sorted := True;

  finally
    System.TMonitor.Exit(FDictionary);
  end;
end;

procedure TPeekDictionaryThread.ResultReady;
var
  Prop: TLocalizerProperty;
begin
  if (not Assigned(FResultHandler)) then
    Exit;

  Prop := DequeueResult;

  FResultHandler(Prop);
end;

procedure TPeekDictionaryThread.TerminatedSet;
begin
  inherited;

  // Wake thread so it can terminate
  FQueueEvent.SetEvent;
end;

// -----------------------------------------------------------------------------
//
// TTranslationMemoryLookup
//
// -----------------------------------------------------------------------------
type
  TTranslationMemoryPeek = class(TInterfacedObject, ITranslationMemoryPeek)
  private
    FThread: TPeekDictionaryThread;
  protected
    // ITranslationMemoryPeek
    procedure EnqueueQuery(Prop: TLocalizerProperty);
    procedure Cancel;
  public
    constructor Create(ATranslationMemory: TDataModuleTranslationMemory; SourceField, TargetField: TField; AResultHandler: TNotifyEvent);
    destructor Destroy; override;
  end;

constructor TTranslationMemoryPeek.Create(ATranslationMemory: TDataModuleTranslationMemory; SourceField, TargetField: TField; AResultHandler: TNotifyEvent);
begin
  inherited Create;
  FThread := TPeekDictionaryThread.Create(ATranslationMemory, SourceField, TargetField, AResultHandler);
  FThread.Start;
end;

destructor TTranslationMemoryPeek.Destroy;
begin
  FThread.Terminate;
  FThread.WaitFor;
  FThread.Free;
  inherited;
end;

procedure TTranslationMemoryPeek.EnqueueQuery(Prop: TLocalizerProperty);
begin
  FThread.EnqueueQuery(Prop);
end;

procedure TTranslationMemoryPeek.Cancel;
begin
  FThread.ClearQueues;
end;


// -----------------------------------------------------------------------------
//
// TDataModuleTranslationMemory
//
// -----------------------------------------------------------------------------
constructor TDataModuleTranslationMemory.Create(AOwner: TComponent);
begin
  inherited;

  FRefreshEvent := TEvent.Create(nil, False, False, '');
  FEnabled := True;

  FProviderHandle := TranslationProviderRegistry.RegisterProvider(sTranslatorNameTM,
    function(): ITranslationProvider
    begin
      Result := Self;
    end);
end;

destructor TDataModuleTranslationMemory.Destroy;
begin
  TranslationProviderRegistry.UnregisterProvider(FProviderHandle);

  FRefreshEvent.Free;

  inherited;
end;

// -----------------------------------------------------------------------------

function TDataModuleTranslationMemory.GetProviderName: string;
begin
  Result := sTranslatorNameTM;
end;

// -----------------------------------------------------------------------------

function TDataModuleTranslationMemory.CheckLoaded(Force: boolean): boolean;
var
  Res: integer;
resourcestring
  sLocalizerNoTMFileTitle = 'Translation Memory does not exist';
  sLocalizerNoTMFile = 'The Translation Memory database does not exist.'#13#13'Filename: %s'#13#13'A new database will be created when you save the Translation Memory.'#13#13'Do you want to save an new empty database now?';
begin
  if (not FLoaded) and ((Force) or (TranslationManagerSettings.Translators.TranslationMemory.LoadOnDemand)) then
  begin
    if (not FEnabled) and (not Force) then
      Exit(False);

    if (not TFile.Exists(TranslationManagerSettings.Translators.TranslationMemory.Filename)) then
    begin
      Res := TaskMessageDlg(sLocalizerNoTMFileTitle,
        Format(sLocalizerNoTMFile, [TranslationManagerSettings.Translators.TranslationMemory.Filename]),
        mtConfirmation, [mbYes, mbNo, mbCancel], 0, mbNo);

      if (Res = mrCancel) then
      begin
        FEnabled := False;
      end else
      if (Res = mrYes) then
      begin
        // Save empty
        SaveToFile(TranslationManagerSettings.Translators.TranslationMemory.Filename);
        // ...and load it
        LoadFromFile(TranslationManagerSettings.Translators.TranslationMemory.Filename);
      end else
      begin
        // Pretend we have loaded to avoid further prompts
        FLoaded := True;
        // Mark as dirty so new file will be saved
        FModified := True;
      end;
    end else
      LoadFromFile(TranslationManagerSettings.Translators.TranslationMemory.Filename);
  end;

  if (FLoaded) then
    FEnabled := True;

  Result := FLoaded;
end;

// -----------------------------------------------------------------------------

function TDataModuleTranslationMemory.CheckSave: boolean;
var
  Res: integer;
resourcestring
  sLocalizerSaveTMPromptTitle = 'Translation Memory has not been saved';
  sLocalizerSaveTMPrompt = 'Your changes to the Translation Memory has not been saved.'#13#13'Do you want to save them now?';
begin
  if (IsLoaded) and (Modified) then
  begin
    if (TranslationManagerSettings.Translators.TranslationMemory.PromptToSave) then
    begin
      Res := TaskMessageDlg(sLocalizerSaveTMPromptTitle, sLocalizerSaveTMPrompt,
        mtConfirmation, [mbYes, mbNo, mbCancel], 0, mbCancel);

      if (Res = mrCancel) then
        Exit(False)
      else
      if (Res = mrNo) then
        Exit(True);
    end;

    SaveCursor(crHourGlass);

    SaveToFile(TranslationManagerSettings.Translators.TranslationMemory.Filename);

    Result := (not Modified);
  end else
    Result := True;
end;

function TDataModuleTranslationMemory.CreateBackgroundLookup(SourceLanguage, TargetLanguage: LCID; AResultHandler: TNotifyEvent): ITranslationMemoryPeek;
var
  SourceLocaleItem, TargetLocaleItem: TLocaleItem;
  SourceField, TargetField: TField;
begin
  if (SourceLanguage = TargetLanguage) then
    Exit(nil);

  if (SourceLanguage = 0) or (TargetLanguage = 0) then
    Exit(nil);

  if (not CheckLoaded) then
    Exit(nil);

  SourceLocaleItem := TLocaleItems.FindLCID(SourceLanguage);
  TargetLocaleItem := TLocaleItems.FindLCID(TargetLanguage);
  Assert(SourceLocaleItem <> nil);
  Assert(TargetLocaleItem <> nil);

  SourceField := FindField(SourceLocaleItem);
  TargetField := FindField(TargetLocaleItem);

  if (SourceField = nil) or (TargetField = nil) then
    Exit(nil);

  Result := TTranslationMemoryPeek.Create(Self, SourceField, TargetField, AResultHandler);
end;

// -----------------------------------------------------------------------------

function TDataModuleTranslationMemory.CreateField(LocaleItem: TLocaleItem): TField;
begin
  if (LocaleItem = nil) then
    Exit(nil);

  Result := TWideMemoField.Create(TableTranslationMemory);

  Result.DisplayLabel := LocaleItem.LanguageName;
  Result.Tag := LocaleItem.Locale;
  Result.FieldName := LocaleItem.LocaleSName;
  Result.DataSet := TableTranslationMemory;
  Result.DisplayWidth := 100;
  Result.OnGetText := FieldGetTextEventHandler; // Otherwise memo is edited as "(WIDEMEMO)"
end;

function TDataModuleTranslationMemory.GetTableTranslationMemory: TDataSet;
begin
  Result := TableTranslationMemory;
end;

type
  TTableTranslationMemoryClone = class(TInterfacedObject)
  private
    FOriginal: TFDMemTable;
    FClone: TFDMemTable;
  public
    constructor Create(AOriginal: TFDMemTable);
    destructor Destroy; override;
  end;

constructor TTableTranslationMemoryClone.Create(AOriginal: TFDMemTable);
begin
  inherited Create;
  FOriginal := AOriginal;
  FClone := TFDMemTable.Create(nil);
  try
    if (FOriginal.Fields.Count > 0) then
      FClone.CopyDataSet(FOriginal, [coStructure, coRestart, coAppend]);
  except
    FreeAndNil(FClone);
    raise;
  end;
  FOriginal.Close;
end;

destructor TTableTranslationMemoryClone.Destroy;
begin
  try
    if (FOriginal.Fields.Count > 0) then
      FOriginal.Open;

    if (FClone <> nil) and (FClone.Fields.Count > 0) then
      FOriginal.CopyDataSet(FClone, [coAppend]);
  finally
    FClone.Free;
  end;

  inherited;
end;

function TDataModuleTranslationMemory.SaveTableTranslationMemoryClone: IInterface;
begin
  Result := TTableTranslationMemoryClone.Create(TableTranslationMemory);
end;

// -----------------------------------------------------------------------------

procedure TDataModuleTranslationMemory.FieldGetTextEventHandler(Sender: TField; var Text: string; DisplayText: Boolean);
begin
  Text := Sender.AsString;
end;

// -----------------------------------------------------------------------------

function TDataModuleTranslationMemory.FindField(LocaleItem: TLocaleItem): TField;
var
  i: integer;
begin
  for i := 0 to TableTranslationMemory.FieldCount-1 do
    if (AnsiSameText(LocaleItem.LocaleName, TableTranslationMemory.Fields[i].FieldName)) then
      Exit(TableTranslationMemory.Fields[i]);
  Result := nil;
end;

// -----------------------------------------------------------------------------

function TDataModuleTranslationMemory.GetAvailable: boolean;
begin
  Result := (FLoaded) or ((FEnabled) and (TranslationManagerSettings.Translators.TranslationMemory.LoadOnDemand));
end;

function TDataModuleTranslationMemory.GetHasData: boolean;
begin
  Result := (TableTranslationMemory.Active) and (TableTranslationMemory.RecordCount > 0);
end;

// -----------------------------------------------------------------------------

function TDataModuleTranslationMemory.AddTerm(SourceField: TField; const SourceValue, SanitizedSourceValue: string;
  TargetField: TField; const TargetValue: string;
  Duplicates: TDuplicates; var Stats: TTranslationMemoryMergeStats; DuplicateAction: TTranslationMemoryDuplicateAction): TTranslationMemoryDuplicateAction;

  function Truncate(const Value: string): string;
  begin
    Result := cxGetStringAdjustedToWidth(0, 0, Value, 250, mstEndEllipsis);
    Result := Result.Replace(#13, ' ',  [rfReplaceAll]).Replace(#10, ' ',  [rfReplaceAll]);
  end;

var
  DuplicateTerms: TDuplicateTerms;
  DuplicateValues: TDuplicateValues;
  Duplicate: TDuplicate;
  AllEmpty: boolean;
  i: integer;
  s: string;
  Res: Word;
  DuplicateFound: boolean;
  DuplicateTermPair: TPair<TField, TDuplicateTerms>;
  SourceLanguage, TargetLanguage: TLocaleItem;
  SourceLanguageName, TargetLanguageName: string;
  DuplicateCount: integer;
resourcestring
  sTranslationMemoryAddDuplicateTitle = 'Duplicates found';
  sTranslationMemoryAddDuplicate = 'You are adding a term that already has %d translation(s) in the dictionary.'#13#13+
    'Do you want to add the translation anyway?'+#13#13+
    '%s: "%s"'+#13#13+
    '%s: %s';
begin
  Assert(SourceField <> TargetField);
  Assert(SourceField <> nil);
  Assert(TargetField <> nil);

  Assert(TableTranslationMemory.Active);

  Result := DuplicateAction;

  if (TargetValue.IsEmpty) then
  begin
    Inc(Stats.Skipped);
    Exit;
  end;

  DuplicateTerms := nil;
  if (not Duplicates.TryGetValue(TargetField, DuplicateTerms)) or (not DuplicateTerms.TryGetValue(SanitizedSourceValue, DuplicateValues)) then
    DuplicateValues := nil;

  AllEmpty := True;
  if (DuplicateValues <> nil) then
    for i := 0 to DuplicateValues.Count-1 do
    begin
      if (AnsiSameText(SourceValue, DuplicateValues[i].SourceValue)) and (AnsiSameText(TargetValue, DuplicateValues[i].Value)) then
      begin
        Inc(Stats.Duplicate);
        Exit;
      end;
      if (not DuplicateValues[i].Value.IsEmpty) then
        AllEmpty := False;
    end;

  // Do not prompt for duplicate with only empty target values - just merge it
  if (DuplicateValues <> nil) and (DuplicateValues.Count > 0) and (not AllEmpty) then
  begin
    if (DuplicateAction = tmDupActionPrompt) then
    begin
      s := '';
      DuplicateCount := 0;
      // Target value on single line if only one, otherwise bullet list
      for i := 0 to DuplicateValues.Count-1 do
      begin
        if (DuplicateValues[i].Value.IsEmpty) then
          continue;
        if (DuplicateCount = 1) then
          s := #13 + '- ' + s;
        if (DuplicateCount >= 1) then
          s := s + #13 + '- ';
        s := s + '"'+Truncate(DuplicateValues[i].Value)+'"';
        Inc(DuplicateCount);
      end;
      Assert(DuplicateCount > 0);

      SourceLanguage := TLocaleItems.FindLocaleName(SourceField.FieldName);
      if (SourceLanguage <> nil) then
        SourceLanguageName := SourceLanguage.LanguageName
      else
        SourceLanguageName := SourceField.FieldName;
      TargetLanguage := TLocaleItems.FindLocaleName(TargetField.FieldName);
      if (TargetLanguage <> nil) then
        TargetLanguageName := TargetLanguage.LanguageName
      else
        TargetLanguageName := TargetField.FieldName;

      Res := TaskMessageDlg(sTranslationMemoryAddDuplicateTitle,
        Format(sTranslationMemoryAddDuplicate, [DuplicateCount, SourceLanguageName, Truncate(SourceValue), TargetLanguageName, s]),
        mtConfirmation, [mbYes, mbNo, mbYesToAll, mbNoToAll, mbAbort], 0, mbNo);

      case Res of
        mrAbort:
          Result := tmDupActionAbort;

        mrNo:
          begin
            Inc(Stats.Skipped);
            Exit;
          end;

        mrNoToAll:
          Result := tmDupActionRejectAll;

        mrYesToAll:
          Result := tmDupActionAcceptAll;
      end;
    end;

    if (Result in [tmDupActionAbort, tmDupActionRejectAll]) then
    begin
      Inc(Stats.Skipped);
      Exit;
    end;
  end;

  if (DuplicateValues <> nil) and (DuplicateValues.EmptyCount > 0) then
  begin
    // Find first empty entry.
    // First look for exact source match
    DuplicateFound := False;
    for i := 0 to DuplicateValues.Count-1 do
    begin
      if (AnsiSameText(DuplicateValues[i].SourceValue, SourceValue)) and (DuplicateValues[i].Value.IsEmpty) then
      begin
        DuplicateFound := True;
        Duplicate := DuplicateValues[i];
        Duplicate.Value := TargetValue;
        DuplicateValues[i] := Duplicate;
        break;
      end;
    end;
    // Then just for an empty entry
    if (not DuplicateFound) then
      for i := 0 to DuplicateValues.Count-1 do
      begin
        if (DuplicateValues[i].Value.IsEmpty) then
        begin
          DuplicateFound := True;
          Duplicate := DuplicateValues[i];
          Duplicate.Value := TargetValue;
          DuplicateValues[i] := Duplicate;
          break;
        end;
      end;
    Assert(DuplicateFound);
    Dec(DuplicateValues.EmptyCount);

    // Edit existing entry
    Lock;
    try
      TableTranslationMemory.RecNo := Duplicate.RecordID;
      TableTranslationMemory.Resync([rmExact]);
      TableTranslationMemory.Edit;
      try
        // Source field already has the desired value
        // SourceField.AsString := SourceValue;

        TargetField.AsString := TargetValue;

        TableTranslationMemory.Post;

      except
        TableTranslationMemory.Cancel;
        raise;
      end;
    finally
      Unlock;
    end;
    Inc(Stats.Merged);
  end else
  begin
    // Add new entry
    Lock;
    try
      TableTranslationMemory.Append;
      try
        SourceField.AsString := SourceValue;
        TargetField.AsString := TargetValue;

        TableTranslationMemory.Post;

      except
        TableTranslationMemory.Cancel;
        raise;
      end;
    finally
      Unlock;
    end;
    Inc(Stats.Added);

    // Add new to duplicate list and add empty to all other term lists
    Duplicate.SourceValue := SourceValue;
    Duplicate.RecordID := TableTranslationMemory.RecNo;
    for DuplicateTermPair in Duplicates do
    begin
      if (not DuplicateTermPair.Value.TryGetValue(SanitizedSourceValue, DuplicateValues)) then
      begin
        DuplicateValues := TDuplicateValues.Create;
        DuplicateTermPair.Value.Add(SanitizedSourceValue, DuplicateValues);
      end;

      if (DuplicateTermPair.Key = TargetField) then
        Duplicate.Value := TargetValue
      else
      begin
        Duplicate.Value := '';
        Inc(DuplicateValues.EmptyCount);
      end;

      DuplicateValues.Add(Duplicate);
    end;
  end;

  FModified := True;
end;

// -----------------------------------------------------------------------------

function TDataModuleTranslationMemory.Add(SourceField: TField; const SourceValue: string; TargetField: TField; const TargetValue: string; var Stats: TTranslationMemoryMergeStats; DuplicateAction: TTranslationMemoryDuplicateAction = tmDupActionPrompt): TTranslationMemoryDuplicateAction;
var
  Duplicates: TDuplicates;
  DuplicateTerms: TDuplicateTerms;
  DuplicateValues: TDuplicateValues;
  Duplicate: TDuplicate;
  SanitizedSourceValue: string;
begin
  Stats := Default(TTranslationMemoryMergeStats);
  Result := DuplicateAction;

  if (SourceField = TargetField) then
    Exit;

  if (not CheckLoaded) then
    Exit;

  TableTranslationMemory.DisableControls;
  try
    if (not TableTranslationMemory.Active) then
      TableTranslationMemory.Open;

    SanitizedSourceValue := SanitizeText(SourceValue, False);

    DuplicateTerms := nil;
    DuplicateValues := nil;
    Duplicates := TDuplicates.Create([doOwnsValues]);
    try
      Duplicate.SourceValue := SourceValue;

      TableTranslationMemory.First;
      while (not TableTranslationMemory.EOF) do
      begin
        if (SourceField.IsNull) or (not AnsiSameText(SanitizedSourceValue, SanitizeText(SourceField.AsString, False))) then
        begin
          TableTranslationMemory.Next;
          continue;
        end;

        if (not TargetField.IsNull) and (AnsiSameText(SourceValue, SourceField.AsString)) and (AnsiSameText(TargetValue, TargetField.AsString)) then
        begin
          Inc(Stats.Duplicate);
          Exit; // Exact duplicate - do nothing
        end;

        // Source same, target differs
        if (DuplicateTerms = nil) then
        begin
          DuplicateTerms := TDuplicateTerms.Create([doOwnsValues], TTextComparer.Create);
          Duplicates.Add(TargetField, DuplicateTerms);

          DuplicateValues := TDuplicateValues.Create;
          DuplicateTerms.Add(SanitizedSourceValue, DuplicateValues);
        end;

        Duplicate.Value := TargetField.AsString;
        Duplicate.RecordID := TableTranslationMemory.RecNo;

        DuplicateValues.Add(Duplicate);

        if (Duplicate.Value.IsEmpty) then
          Inc(DuplicateValues.EmptyCount);

        TableTranslationMemory.Next;
      end;

      Result := AddTerm(SourceField, SourceValue, SanitizedSourceValue, TargetField, TargetValue, Duplicates, Stats, DuplicateAction);

    finally
      Duplicates.Free;
    end;

  finally
    TableTranslationMemory.EnableControls;
  end;
end;

function TDataModuleTranslationMemory.Add(SourceLanguage: Word; const SourceValue: string; TargetLanguage: Word; const TargetValue: string; var Stats: TTranslationMemoryMergeStats; DuplicateAction: TTranslationMemoryDuplicateAction = tmDupActionPrompt): TTranslationMemoryDuplicateAction;
var
  SourceLocaleItem, TargetLocaleItem: TLocaleItem;
  SourceField: TField;
  TargetField: TField;
  Clone: TFDMemTable;
begin
  Stats := Default(TTranslationMemoryMergeStats);
  Result := DuplicateAction;

  if (SourceLanguage = TargetLanguage) then
    Exit;

  if (not CheckLoaded) then
    Exit;

  SourceLocaleItem := TLocaleItems.FindLCID(SourceLanguage);
  TargetLocaleItem := TLocaleItems.FindLCID(TargetLanguage);
  Assert(SourceLocaleItem <> nil);
  Assert(TargetLocaleItem <> nil);

  SourceField := FindField(SourceLocaleItem);
  TargetField := FindField(TargetLocaleItem);

  TableTranslationMemory.DisableControls;
  try
    // If either the source- or target languages doesn't exist in the dataset then we
    // will need to add them.
    // We save a copy of the dataset, close the original dataset, add the field(s) and
    // the restore the dataset from the copy.
    if (SourceField = nil) or (TargetField = nil) then
    begin
      Clone := TFDMemTable.Create(nil);
      try
        Lock;
        try
          if (TableTranslationMemory.Fields.Count > 0) then
            Clone.CopyDataSet(TableTranslationMemory, [coStructure, coRestart, coAppend]);

          TableTranslationMemory.Close;

          if (SourceField = nil) then
            SourceField := CreateField(SourceLocaleItem);
          if (TargetField = nil) then
            TargetField := CreateField(TargetLocaleItem);

          TableTranslationMemory.Open;

          if (Clone.Fields.Count > 0) then
            TableTranslationMemory.CopyDataSet(Clone, [coAppend]);
        finally
          Unlock;
        end;
      finally
        Clone.Free;
      end;
    end;

    Result := Add(SourceField, SourceValue, TargetField, TargetValue, Stats, DuplicateAction);

  finally
    TableTranslationMemory.EnableControls;
  end;
end;

// -----------------------------------------------------------------------------

procedure TDataModuleTranslationMemory.LoadFromFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TDataModuleTranslationMemory.LoadFromStream(Stream: TStream);

  procedure LoadLanguages(Reader: TReader);
  var
    Field: TField;
  begin
    Reader.ReadInteger; // Ignore count

    Reader.ReadListBegin;
    while (not Reader.EndOfList) do
    begin
      Field := TWideMemoField.Create(TableTranslationMemory);

      Reader.ReadListBegin;
      begin
        Field.Tag := Reader.ReadInteger;
        Field.FieldName := Reader.ReadString;
        Field.DisplayLabel := Reader.ReadString;

        while (not Reader.EndOfList) do
          Reader.SkipValue;
      end;
      Reader.ReadListEnd;

      Field.DataSet := TableTranslationMemory;
      Field.DisplayWidth := 100;
      Field.OnGetText := FieldGetTextEventHandler; // Otherwise memo is edited as "(WIDEMEMO)"
    end;
    Reader.ReadListEnd;
  end;

  procedure LoadTerms(Reader: TReader);
  var
    s: string;
    i: integer;
  begin
    if (TableTranslationMemory.Fields.Count > 0) then
      TableTranslationMemory.CreateDataSet; // Apperently one cannot create a dataset without fields

    TableTranslationMemory.DisableControls;
    try
      Reader.ReadInteger; // Ignore count

      Reader.ReadListBegin;
      while (not Reader.EndOfList) do
      begin
        TableTranslationMemory.Append;
        try

          Reader.ReadListBegin;
          begin
            for i := 0 to TableTranslationMemory.Fields.Count-1 do
            begin
              s := Reader.ReadString;
              if (s <> '') then
                TableTranslationMemory.Fields[i].AsString := s;
            end;
            // Future version might add additional non-field data. E.g. context info or term description
            while (not Reader.EndOfList) do
              Reader.SkipValue;
          end;
          Reader.ReadListEnd;

          TableTranslationMemory.Post;
        except
          TableTranslationMemory.Cancel;
          raise;
        end;
      end;
      Reader.ReadListEnd;
    finally
      TableTranslationMemory.EnableControls;
    end;
  end;

var
  Signature: AnsiString;
  Reader: TReader;
  VersionMajor, VersionMinor: integer;
  n: integer;
{$ifdef TM_BENCHMARK}
var
  StopWatch: TStopWatch;
{$endif TM_BENCHMARK}
begin
{$ifdef TM_BENCHMARK}
  StopWatch := TStopWatch.StartNew;
{$endif TM_BENCHMARK}

  (*
  ** Signature
  *)
  SetLength(Signature, Length(sTMFileSignature));
  if (Stream.Read(Signature[1], Length(sTMFileSignature)) <> Length(sTMFileSignature)) or (Signature <> sTMFileSignature) then
    raise ETranslationMemory.Create('Invalid TM file signature');

  TableTranslationMemory.Close;
  TableTranslationMemory.Fields.Clear;
  TableTranslationMemory.FieldDefs.Clear;

  Reader := TReader.Create(Stream, 8192);
  try
    (*
    ** Stream format version
    *)
    VersionMajor := Reader.ReadInteger;
    VersionMinor := Reader.ReadInteger;

    if (VersionMajor <> TMFileVersionMajor) then
      raise ETranslationMemory.CreateFmt('Unsupported TM file format version: %d.%d', [VersionMajor, VersionMinor]);

    Reader.ReadListBegin;
    begin
      while (not Reader.EndOfList) do
      begin
        // Read tag
        n := Reader.ReadInteger;
        if (n < Ord(Low(TTMStreamTag))) or (n > Ord(High(TTMStreamTag))) then
        begin
          // Unknown tag - skip block
          Reader.SkipValue;
          continue;
        end;

        Reader.ReadListBegin;
        begin
          case TTMStreamTag(n) of
            tmDescription:
              Reader.ReadString;

            tmLanguages:
              LoadLanguages(Reader);

            tmTerms:
              LoadTerms(Reader);

          else
            Reader.SkipValue;
          end;
        end;
        Reader.ReadListEnd;
      end;
    end;
    Reader.ReadListEnd;
  finally
    Reader.Free;
  end;

{$ifdef TM_BENCHMARK}
  StopWatch.Stop;
  OutputDebugString(PChar(Format('Read TM via TReader: %.2nmS (%d rows/mS)', [StopWatch.ElapsedMilliseconds*1.0, TableTranslationMemory.RecordCount div StopWatch.ElapsedMilliseconds])));
{$endif TM_BENCHMARK}

  FModified := False;
  FLoaded := True;
end;

procedure TDataModuleTranslationMemory.SaveToFile(const Filename: string);
begin
  SafeReplaceFile(Filename,
    function(const Filename: string): boolean
    var
      Stream: TStream;
    begin
      Stream := TFileStream.Create(Filename, fmCreate);
      try
        SaveToStream(Stream);
      finally
        Stream.Free;
      end;
      Result := True;
    end, TranslationManagerSettings.Backup.SaveBackups);
end;

procedure TDataModuleTranslationMemory.SaveToStream(Stream: TStream);

  procedure WriteLanguages(Writer: TWriter);
  var
    i: integer;
  begin
    Writer.WriteInteger(TableTranslationMemory.Fields.Count);
    Writer.WriteListBegin;
    begin
      for i := 0 to TableTranslationMemory.Fields.Count-1 do
      begin
        Writer.WriteListBegin;
        begin
          Writer.WriteInteger(TableTranslationMemory.Fields[i].Tag); // LCID
          Writer.WriteString(TableTranslationMemory.Fields[i].FieldName); // Locale short name
          Writer.WriteString(TableTranslationMemory.Fields[i].DisplayName); // Locale name
        end;
        Writer.WriteListEnd;
      end;
    end;
    Writer.WriteListEnd;
  end;

  procedure WriteTerms(Writer: TWriter);
  var
    i: integer;
  begin
    if (TableTranslationMemory.Active) then
    begin
      Writer.WriteInteger(TableTranslationMemory.RecordCount); // Row list
      Writer.WriteListBegin;
      begin
        TableTranslationMemory.DisableControls;
        try
          TableTranslationMemory.First;
          while (not TableTranslationMemory.Eof) do
          begin
            Writer.WriteListBegin;
            begin
              for i := 0 to TableTranslationMemory.Fields.Count-1 do
                Writer.WriteString(TableTranslationMemory.Fields[i].AsString); // Field value
            end;
            Writer.WriteListEnd;

            TableTranslationMemory.Next;
          end;
        finally
          TableTranslationMemory.EnableControls;
        end;
      end;
      Writer.WriteListEnd;
    end else
    begin
      Writer.WriteInteger(0);
      Writer.WriteListBegin;
      Writer.WriteListEnd;
    end;
  end;

var
  Writer: TWriter;
{$ifdef TM_BENCHMARK}
var
  StopWatch: TStopWatch;
{$endif TM_BENCHMARK}
begin
{$ifdef TM_BENCHMARK}
  StopWatch := TStopWatch.StartNew;
{$endif TM_BENCHMARK}

  (*
  ** Signature
  *)
  Stream.Write(sTMFileSignature[1], Length(sTMFileSignature));

  Writer := TWriter.Create(Stream, 8192);
  try
    (*
    ** Stream format version
    *)
    Writer.WriteInteger(TMFileVersionMajor);
    Writer.WriteInteger(TMFileVersionMinor);

    Writer.WriteListBegin;
    begin
      (*
      ** Description
      *)
      Writer.WriteInteger(Ord(tmDescription));
      Writer.WriteListBegin;
      begin
        Writer.WriteString('Default Translation Memory'); // TODO : Localize
      end;
      Writer.WriteListEnd;

      (*
      ** Languages
      *)
      Writer.WriteInteger(Ord(tmLanguages));
      Writer.WriteListBegin;
      begin
        WriteLanguages(Writer);
      end;
      Writer.WriteListEnd;

      (*
      ** Terms
      *)
      Writer.WriteInteger(Ord(tmTerms));
      Writer.WriteListBegin;
      begin
        WriteTerms(Writer);
      end;
      Writer.WriteListEnd;
    end;
    Writer.WriteListEnd;
  finally
    Writer.Free;
  end;

{$ifdef TM_BENCHMARK}
  StopWatch.Stop;
  OutputDebugString(PChar(Format('Save TM via TWriter: %.2nmS (%d rows/mS)', [StopWatch.ElapsedMilliseconds*1.0, TableTranslationMemory.RecordCount div StopWatch.ElapsedMilliseconds])));
{$endif TM_BENCHMARK}

  FModified := False;
end;

procedure TDataModuleTranslationMemory.SetLoaded;
begin
  FLoaded := True;
end;

// -----------------------------------------------------------------------------

procedure TDataModuleTranslationMemory.TableTranslationMemoryAfterModify(DataSet: TDataSet);
begin
  FModified := True;
  FRefreshEvent.SetEvent;
end;

// -----------------------------------------------------------------------------

function TDataModuleTranslationMemory.FindTranslations(Prop: TLocalizerProperty; TargetField: TField; Translations: TStrings): boolean;
var
  List: TList<integer>;
  RecordIndex: integer;
  SourceValue, TargetValue: string;
begin
  Result := False;

  if (not HasData) then
    Exit;

  Assert(FLookupIndex <> nil);

  SourceValue := SanitizeText(Prop.Value, False);
  if (not FLookupIndex.TryGetValue(SourceValue, List)) then
    Exit;

  for RecordIndex in List do
  begin
    TableTranslationMemory.RecNo := RecordIndex;

    TargetValue := TargetField.AsString;

    if (TargetValue.IsEmpty) then
      continue;

    AddMatch(Translations, Prop, SourceValue, TargetValue);
    Result := True;
  end;
end;

procedure TDataModuleTranslationMemory.AddMatch(Translations: TStrings; Prop: TLocalizerProperty; const SourceValue, TargetValue: string);
var
  Value: string;
  n: integer;
begin
  // We can't really assume anything with regards to fidelity on a TM but the hope is
  // that at least the case is correct.
  // To make sure we add an extra match where we have made sure the case is correct.

  if (Prop.Value = SourceValue) then
  begin
    // Don't mess with case on an exact match.
    Value := MakeAlike(Prop.Value, TargetValue, [EqualizeEnding, EqualizeAccelerators]);

    // Exact match - Insert in front
    if (Translations.Count > 0) then
    begin
      n := Translations.IndexOf(Value);
      if (n = -1) then
        Translations.Insert(0, Value)
      else
        Translations.Move(n, 0);
    end else
      Translations.Add(Value);

    // Fix case. This is safe since we will ignore duplicates below.
    Value := MakeAlike(Prop.Value, Value, [EqualizeCase]);
  end else
    Value := MakeAlike(Prop.Value, TargetValue);

  // Inefficient due to sequential scan, but we need to maintain returned order
  if (Translations.IndexOf(Value) = -1) then
    Translations.Add(Value);
end;

function TDataModuleTranslationMemory.FindTranslations(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLocaleItem; Translations: TStrings): boolean;
var
  SourceField: TField;
  TargetField: TField;
  SourceValue: string;
  TargetValue: string;
begin
  if (not CheckLoaded) then
    Exit(False);

  // Do nothing if there is no data but pretend everything is OK so the user gets normal feedback
  if (not HasData) then
    Exit(True);

  SourceField := FindField(SourceLanguage);
  TargetField := FindField(TargetLanguage);

  if (SourceField = nil) or (TargetField = nil) or (SourceField = TargetField) then
    // One or both languages doesn't exist in TM
    Exit(False);

  SourceValue := SanitizeText(Prop.Value, False);
  Result := False;

  // Create list of matching target terms
  TableTranslationMemory.First;

  while (not TableTranslationMemory.EOF) do
  begin
    if (not TargetField.IsNull) and (not SourceField.IsNull) then
    begin
      if (AnsiSameText(SourceValue, SanitizeText(SourceField.AsString, False))) then
      begin
        TargetValue := TargetField.AsString;

        // Match found
        AddMatch(Translations, Prop, SourceValue, TargetValue);
        Result := True;
      end;
    end;

    TableTranslationMemory.Next;
  end;
end;

// -----------------------------------------------------------------------------

function TDataModuleTranslationMemory.Lookup(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLocaleItem; Translations: TStrings): boolean;
var
  TargetField: TField;
begin
  Result := False;

  if (SourceLanguage = TargetLanguage) then
    Exit;

  if (not HasData) then
    Exit;

  TargetField := FindField(TargetLanguage);

  if (TargetField = nil) then
    Exit;

  Result := FindTranslations(Prop, TargetField, Translations);
end;

// -----------------------------------------------------------------------------

procedure TDataModuleTranslationMemory.Lock;
begin
  System.TMonitor.Enter(Self);
end;

procedure TDataModuleTranslationMemory.Unlock;
begin
  System.TMonitor.Exit(Self);
end;

procedure TDataModuleTranslationMemory.PopulateDictionary(SourceField, TargetField: TField; Dictionary: TStringList);
var
  Clone: TFDMemTable;
  SourceValue: string;
begin
  // It is the responsibility of the caller to lock Dictionary
  Dictionary.Clear;

  if (SourceField = nil) or (TargetField = nil) or (SourceField = TargetField) then
    Exit;

  Clone := TFDMemTable.Create(nil);
  try
    // Lock for the duration - we need the data to be stable while we iterate through it
    // (clone shares data with source).
    Lock;
    try

      Clone.CloneCursor(TableTranslationMemory);

      SourceField := Clone.Fields.FieldByName(SourceField.FieldName);
      TargetField := Clone.Fields.FieldByName(TargetField.FieldName);

      // Create dictionary of source terms that has translations
      Clone.First;

      while (not Clone.EOF) do
      begin
        if (not TargetField.IsNull) and (not SourceField.IsNull) then
        begin
          SourceValue := SanitizeText(SourceField.AsString, False);

          if (Dictionary.IndexOf(SourceValue) = -1) then
            Dictionary.Add(SourceValue);
        end;

        Clone.Next;
      end;

    finally
      Unlock;
    end;


  finally
    Clone.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TDataModuleTranslationMemory.BeginLookup(SourceLanguage, TargetLanguage: TLocaleItem): boolean;
var
  SourceField: TField;
  TargetField: TField;
  SourceValue: string;
  List: TList<integer>;
begin
  FLookupIndex := TObjectDictionary<string, TList<integer>>.Create([doOwnsValues], TTextComparer.Create);

  if (not CheckLoaded) then
    Exit(False);

  // Do nothing if there is no data but pretend everything is OK so the user gets normal feedback
  if (not HasData) then
    Exit(True);

  SourceField := FindField(SourceLanguage);
  TargetField := FindField(TargetLanguage);

  if (SourceField = nil) or (TargetField = nil) or (SourceField = TargetField) then
    // One or both languages doesn't exist in TM
    Exit(False);

  // Create dictionary of source terms
  TableTranslationMemory.First;

  while (not TableTranslationMemory.EOF) do
  begin
    if (not TargetField.IsNull) and (not SourceField.IsNull) then
    begin
      SourceValue := SanitizeText(SourceField.AsString, False);

      if (not FLookupIndex.TryGetValue(SourceValue, List)) then
      begin
        List := TList<integer>.Create;
        FLookupIndex.Add(SourceValue, List);
      end;
      List.Add(TableTranslationMemory.RecNo);
    end;

    TableTranslationMemory.Next;
  end;

  Result := True;
end;

procedure TDataModuleTranslationMemory.EndLookup;
begin
  FreeAndNil(FLookupIndex);
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

end.
