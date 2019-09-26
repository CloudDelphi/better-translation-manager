unit amLocalization.Translator;

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
  Generics.Collections,
  amLocale,
  amLocalization.Model;

// -----------------------------------------------------------------------------
//
// ITranslationProvider
//
// -----------------------------------------------------------------------------
type
  ITranslationProvider = interface
    ['{254E24D5-8F3C-422E-9304-1E21126C8B3C}']
    function BeginLookup(SourceLanguage, TargetLanguage: TLocaleItem): boolean;
    function Lookup(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLocaleItem; Translations: TStrings): boolean;
    procedure EndLookup;

    function GetServiceName: string;
    property ServiceName: string read GetServiceName;
  end;


// -----------------------------------------------------------------------------
//
// ITranslationService
//
// -----------------------------------------------------------------------------
type
  ITranslationService = interface
    ['{0613B25B-5D59-47E1-B38C-A9FD193E5CC5}']
    function BeginLookup(SourceLanguage, TargetLanguage: TLocaleItem): boolean;
    function Lookup(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLocaleItem; var TargetValue: string): boolean;
    procedure EndLookup;
  end;


// -----------------------------------------------------------------------------
//
// TranslationProviderRegistry
//
// -----------------------------------------------------------------------------
type
  TTranslationProviderFactory = reference to function(): ITranslationProvider;

  TranslationProviderRegistry = class abstract
  private type
    TProviderRecord = record
      ProviderName: string;
      ProviderFactory: TTranslationProviderFactory;
      NextFreeHandle: integer;
    end;
  private
    class var
      FRegistry: TList<TProviderRecord>;
      FFirstFreeHandle: integer; // Linked list of unused records
  public type
    TProvider = record
      Handle: integer;
      ProviderName: string;
    end;

    TProviderEnumerator = record
    strict private
      FList: TList<TProviderRecord>;
      FIndex: Integer;
    private
      class function Initialize(List: TList<TProviderRecord>): TProviderEnumerator; static;
    public
      function GetCurrent: TProvider;
      function MoveNext: Boolean;
      property Current: TProvider read GetCurrent;
    end;

  public
    class constructor Create;
    class destructor Destroy;

    class function RegisterProvider(const ProviderName: string; ProviderFactory: TTranslationProviderFactory): integer;
    class procedure UnregisterProvider(ProviderHandle: integer);

    function GetEnumerator: TProviderEnumerator;

    class function CreateProvider(ProviderHandle: integer): ITranslationProvider;
  end;


// -----------------------------------------------------------------------------
//
// CreateTranslationService
//
// -----------------------------------------------------------------------------
function CreateTranslationService(const ATranslationProvider: ITranslationProvider): ITranslationService;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  SysUtils,
  amLocalization.Dialog.TranslationMemory.SelectDuplicate;

type
  TTranslationService = class(TInterfacedObject, ITranslationService)
  private
    FTranslationProvider: ITranslationProvider;
    FFormSelectDuplicate: TFormSelectDuplicate;
    FDuplicateAction: TDuplicateAction;
    FConflictResolution: TDictionary<string, string>;
  protected
    function BeginLookup(SourceLanguage, TargetLanguage: TLocaleItem): boolean;
    function Lookup(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLocaleItem; var TargetValue: string): boolean;
    procedure EndLookup;
  public
    constructor Create(const ATranslationProvider: ITranslationProvider);
    destructor Destroy; override;
  end;

// -----------------------------------------------------------------------------

constructor TTranslationService.Create(const ATranslationProvider: ITranslationProvider);
begin
  inherited Create;

  FTranslationProvider := ATranslationProvider;
end;

destructor TTranslationService.Destroy;
begin
  FreeAndNil(FFormSelectDuplicate);
  FreeAndNil(FConflictResolution);

  inherited;
end;

// -----------------------------------------------------------------------------

function TTranslationService.BeginLookup(SourceLanguage, TargetLanguage: TLocaleItem): boolean;
begin
  Result := FTranslationProvider.BeginLookup(SourceLanguage, TargetLanguage);

  if (not Result) then
    Exit;

  FDuplicateAction := daPrompt;

  if (FConflictResolution = nil) then
    FConflictResolution := TDictionary<string, string>.Create
  else
    FConflictResolution.Clear;
end;

procedure TTranslationService.EndLookup;
begin
  FTranslationProvider.EndLookup;

  if (FConflictResolution <> nil) then
    FConflictResolution.Clear;
end;

function TTranslationService.Lookup(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLocaleItem; var TargetValue: string): boolean;
var
  Translations: TStrings;
begin
  Translations := TStringList.Create;
  try

    Result := FTranslationProvider.Lookup(Prop, SourceLanguage, TargetLanguage, Translations);

    if (not Result) then
      Exit;

    Assert(Translations.Count > 0);

    if (Translations.Count > 1) then
    begin
      // Attempt to resolve using previously resolved conflicts
      if (FConflictResolution.TryGetValue(Prop.Value, TargetValue)) then
        Exit;

      if (FFormSelectDuplicate = nil) then
        FFormSelectDuplicate := TFormSelectDuplicate.Create(nil);

      FFormSelectDuplicate.DuplicateAction := FDuplicateAction;

      if (not FFormSelectDuplicate.SelectDuplicate(Prop, Translations, TargetValue)) then
        Abort;

      FDuplicateAction := FFormSelectDuplicate.DuplicateAction;

      if (FFormSelectDuplicate.ApplyToIdentical) then
        FConflictResolution.Add(Prop.Value, TargetValue);

      Result := (not (FDuplicateAction in [daSkip, daSkipAll]));
    end else
      TargetValue := Translations[0];

  finally
    Translations.Free;
  end;
end;

// -----------------------------------------------------------------------------

function CreateTranslationService(const ATranslationProvider: ITranslationProvider): ITranslationService;
begin
  Result := TTranslationService.Create(ATranslationProvider);
end;

// -----------------------------------------------------------------------------
//
// TranslationProviderRegistry
//
// -----------------------------------------------------------------------------
class constructor TranslationProviderRegistry.Create;
begin
  FRegistry := TList<TProviderRecord>.Create;
  FFirstFreeHandle := -1;
end;

class destructor TranslationProviderRegistry.Destroy;
begin
  FreeAndNil(FRegistry);
end;

// -----------------------------------------------------------------------------

class function TranslationProviderRegistry.TProviderEnumerator.Initialize(List: TList<TProviderRecord>): TProviderEnumerator;
begin
  Result.FList := List;
  Result.FIndex := -1;
end;

function TranslationProviderRegistry.TProviderEnumerator.MoveNext: Boolean;
begin
  Result := (FIndex < FList.Count-1);
  if Result then
    inc(FIndex);
end;

function TranslationProviderRegistry.TProviderEnumerator.GetCurrent: TProvider;
begin
  Result.Handle := FIndex;
  Result.ProviderName := FList[FIndex].ProviderName;
end;

function TranslationProviderRegistry.GetEnumerator: TProviderEnumerator;
begin
  // This is a hack. Self=nil.
  Result := TProviderEnumerator.Initialize(TranslationProviderRegistry.FRegistry);
end;

// -----------------------------------------------------------------------------

class function TranslationProviderRegistry.CreateProvider(ProviderHandle: integer): ITranslationProvider;
begin
  Result := FRegistry[ProviderHandle].ProviderFactory();
end;

class function TranslationProviderRegistry.RegisterProvider(const ProviderName: string; ProviderFactory: TTranslationProviderFactory): integer;
var
  ProviderRecord: TProviderRecord;
begin
  ProviderRecord.ProviderName := ProviderName;
  ProviderRecord.ProviderFactory := ProviderFactory;
  ProviderRecord.NextFreeHandle := -1;

  if (FFirstFreeHandle <> -1) then
  begin
    Result := FFirstFreeHandle;
    FFirstFreeHandle := FRegistry[FFirstFreeHandle].NextFreeHandle;
    FRegistry[Result] := ProviderRecord;
  end else
    Result := FRegistry.Add(ProviderRecord);
end;

class procedure TranslationProviderRegistry.UnregisterProvider(ProviderHandle: integer);
var
  ProviderRecord: TProviderRecord;
begin
  ProviderRecord := Default(TProviderRecord);
  ProviderRecord.NextFreeHandle := FFirstFreeHandle;
  FRegistry[ProviderHandle] := ProviderRecord;
  FFirstFreeHandle := ProviderHandle;
end;

// -----------------------------------------------------------------------------

end.
