unit amLocalization.StopList;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  Generics.Collections,
  RegularExpressions,
  amProgress,
  amLocalization.Model;

type
  TStopListField = (slFieldModule, slFieldElement, slFieldType, slFieldName, slFieldTypeAndName, slFieldText);
  TStopListOperator = (slOpEquals, slOpStarts, slOpEnds, slOpContains, slOpRegExp);

  TStopListItem = class
  private
    FField: TStopListField;
    FValue: string;
    FStopListOperator: TStopListOperator;
    FGroup: string;
    FEnabled: boolean;
    FRegEx: TRegEx;
    FHasRegEx: boolean;
  protected
    procedure SetEnabled(const Value: boolean);
    procedure SetStopListOperator(const Value: TStopListOperator);
    procedure SetValue(const Value: string);
    procedure ClearState; inline;
  public
    procedure Assign(StopListItem: TStopListItem);

    function SaveToString(IncludeGroup: boolean = True): string;
    procedure LoadFromString(const AValue: string; IncludeGroup: boolean = True);

    function ExtractValue(Prop: TLocalizerProperty): string; overload;
    class function ExtractValue(StopListField: TStopListField; Prop: TLocalizerProperty): string; overload;

    function Evaluate(Module: TLocalizerModule): boolean; overload;
    function Evaluate(Prop: TLocalizerProperty): boolean; overload;
    function Evaluate(const Text: string): boolean; overload;

    function IsEquivalent(StopListItem: TStopListItem): boolean;

    property Group: string read FGroup write FGroup;
    property Field: TStopListField read FField write FField;
    property StopListOperator: TStopListOperator read FStopListOperator write SetStopListOperator;
    property Value: string read FValue write SetValue;
    property Enabled: boolean read FEnabled write SetEnabled;
  end;

  TStopListItemList = class(TObjectList<TStopListItem>)
  public type
    TStopListStats = record
      ModuleCount: integer;
      PropertyCount: integer;
    end;
  public
    procedure Assign(StopList: TStopListItemList);

    function Apply(Project: TLocalizerProject; Callback: TLocalizerObjectDelegate = nil; const Progress: IProgress = nil): TStopListStats;

    function Evaluate(Module: TLocalizerModule): boolean; overload;
    function Evaluate(Prop: TLocalizerProperty; SkipModule: boolean = True): boolean; overload;

    procedure LoadFromFile(const Filename: string; Merge: boolean = False);
    procedure SaveToFile(const Filename: string);
  end;

resourcestring
  sStopListGroupGeneralDisplay = 'General';

const
  sStopListGroupGeneral = 'General';
  sStopListTypeNameSeparator = '.';

implementation

uses
  SysUtils,
  StrUtils,
  Classes,
  System.NetEncoding;

{ TStopListItem }

procedure TStopListItem.Assign(StopListItem: TStopListItem);
begin
  ClearState;
  FField := StopListItem.Field;
  FValue := StopListItem.Value;
  FStopListOperator := StopListItem.StopListOperator;
  FGroup := StopListItem.Group;
  FEnabled := StopListItem.Enabled;
end;

function TStopListItem.SaveToString(IncludeGroup: boolean): string;

  function Escape(const Value: string): string;
  begin
    Result := TNetEncoding.URL.Encode(Value);
  end;

begin
  Result := Format('%d/%d/%d/%s', [Ord(Enabled), Ord(Field), Ord(StopListOperator), Escape(Value)]);
  if (IncludeGroup) then
    Result := Format('%s/%s', [Escape(Group), Result]);
end;

procedure TStopListItem.LoadFromString(const AValue: string; IncludeGroup: boolean);

  function Unescape(const Value: string): string;
  begin
    Result := TNetEncoding.URL.Decode(Value);
  end;

var
  Values: TArray<string>;
  n: integer;

  function HasNextValue: boolean;
  begin
    Result := (n < Length(Values));
  end;

  function GetNextValue: string;
  begin
    if (n < Length(Values)) then
    begin
      Result := Values[n];
      Inc(n);
    end else
      Result := '';
  end;

begin
  Values := AValue.Split(['/']);
  n := 0;

  if (IncludeGroup) and (HasNextValue) then
  begin
    Group := Unescape(GetNextValue);
    if (Group = sStopListGroupGeneral) then
      Group := '';
  end;

  if (HasNextValue) then
    Enabled := boolean(StrToIntDef(GetNextValue, 0));

  if (HasNextValue) then
    Field := TStopListField(StrToIntDef(GetNextValue, 0));

  if (HasNextValue) then
    StopListOperator := TStopListOperator(StrToIntDef(GetNextValue, 0));

  if (HasNextValue) then
    Value := Unescape(GetNextValue);
end;

procedure TStopListItem.ClearState;
begin
  if (FStopListOperator = slOpRegExp) and (FHasRegEx) then
  begin
    FRegEx := Default(TRegEx);
    FHasRegEx := False;
  end;
end;

procedure TStopListItem.SetEnabled(const Value: boolean);
begin
  FEnabled := Value;
  if (not FEnabled) then
    ClearState;
end;

procedure TStopListItem.SetStopListOperator(const Value: TStopListOperator);
begin
  ClearState;
  FStopListOperator := Value;
end;

procedure TStopListItem.SetValue(const Value: string);
begin
  ClearState;
  FValue := Value;
end;

function TStopListItem.Evaluate(Prop: TLocalizerProperty): boolean;
begin
  if (not Enabled) then
    Exit(False);

  Result := Evaluate(ExtractValue(Prop));
(*
  if (Result) and (Field = ffName) then
  begin
    FField := ffTypeAndName;
    FValue := Prop.Item.TypeName+':'+Prop.Name;
  end;
*)
end;

function TStopListItem.Evaluate(Module: TLocalizerModule): boolean;
begin
  Result := False;

  if (not Enabled) then
    Exit;

  if (FField <> slFieldModule) then
    Exit;

  Result := Evaluate(Module.Name);
end;

function TStopListItem.IsEquivalent(StopListItem: TStopListItem): boolean;
begin
  Result := (Field = StopListItem.Field) and
    (StopListOperator = StopListItem.StopListOperator) and
    (Value = StopListItem.Value);
end;

function TStopListItem.Evaluate(const Text: string): boolean;
begin
  Result := False;
  case FStopListOperator of
    slOpEquals:
      Result := AnsiSameText(Value, Text);

    slOpStarts:
      Result := Text.StartsWith(Value, True);

    slOpEnds:
      Result := Text.EndsWith(Value, True);

    slOpContains:
      Result := Text.ToLower.Contains(Value.ToLower);

    slOpRegExp:
      begin
        if (not FHasRegEx) then
        begin
          try

            FRegEx := TRegEx.Create(Value, [roCompiled]);

          except
            // RegEx is invalid - Disable item
            Enabled := False;
            raise;
          end;
          FHasRegEx := True;
        end;
        Result := FRegEx.IsMatch(Text);
      end;
  end;
end;

class function TStopListItem.ExtractValue(StopListField: TStopListField; Prop: TLocalizerProperty): string;
begin
  case StopListField of
    slFieldModule:
      Result := Prop.Item.Module.Name;

    slFieldElement:
      Result := Prop.Item.Name;

    slFieldType:
      Result := Prop.Item.TypeName;

    slFieldName:
      Result := Prop.Name;

    slFieldTypeAndName:
      Result := Prop.Item.TypeName + sStopListTypeNameSeparator + Prop.Name;

    slFieldText:
      Result := Prop.Value;
  else
    Result := '';
  end;
end;

function TStopListItem.ExtractValue(Prop: TLocalizerProperty): string;
begin
  Result := ExtractValue(FField, Prop);
end;

{ TStopListItemList }

function TStopListItemList.Apply(Project: TLocalizerProject; Callback: TLocalizerObjectDelegate; const Progress: IProgress): TStopListStats;
var
  Stats: TStopListStats;
begin
  if (Progress <> nil) then
    Progress.Progress(psBegin, 0, Project.Modules.Count+Project.PropertyCount);

  Stats := Default(TStopListStats);

  Project.BeginUpdate;
  try

    Project.Traverse(
      function(Module: TLocalizerModule): boolean
      begin
        if (Progress <> nil) then
          Progress.AdvanceProgress;

        if (Module.EffectiveStatus <> ItemStatusDontTranslate) then
          if (Evaluate(Module)) then
          begin
            Module.Status := ItemStatusDontTranslate;
            Inc(Stats.ModuleCount);
            if (Assigned(Callback)) then
              Callback(Module);
          end;
        Result := True;
      end);

    Project.Traverse(
      function(Prop: TLocalizerProperty): boolean
      begin
        if (Progress <> nil) then
          Progress.AdvanceProgress;

        if (Prop.EffectiveStatus <> ItemStatusDontTranslate) then
          if (Evaluate(Prop)) then
          begin
            Prop.Status := ItemStatusDontTranslate;
            Inc(Stats.PropertyCount);
            if (Assigned(Callback)) then
              Callback(Prop);
          end;
        Result := True;
      end);

  finally
    Project.EndUpdate;
  end;

  if (Progress <> nil) then
    Progress.Progress(psEnd, 1, 1);

  Result := Stats;
end;

procedure TStopListItemList.Assign(StopList: TStopListItemList);
var
  StopListItem: TStopListItem;
  i: integer;
begin
  Clear;
  for i := 0 to StopList.Count-1 do
  begin
    StopListItem := TStopListItem.Create;
    Add(StopListItem);
    StopListItem.Assign(StopList[i]);
  end;
end;

function TStopListItemList.Evaluate(Module: TLocalizerModule): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Count-1 do
    if (Items[i].Evaluate(Module)) then
      Exit(True);
end;

function TStopListItemList.Evaluate(Prop: TLocalizerProperty; SkipModule: boolean): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Count-1 do
  begin
    if (SkipModule) and (Items[i].Field = slFieldModule) then
      continue;

    if (Items[i].Evaluate(Prop)) then
      Exit(True);
  end;
end;

procedure TStopListItemList.LoadFromFile(const Filename: string; Merge: boolean);
var
  Reader: TStreamReader;
  StopListItem, NewStopListItem: TStopListItem;
  Value: string;
  First: boolean;
begin
  Reader := TStreamReader.Create(Filename, TEncoding.UTF8);
  try

    if (not Merge) then
      Clear;

    First := True;

    while (not Reader.EndOfStream) do
    begin
      Value := Reader.ReadLine.Trim;

      if (Value.IsEmpty) or (Value.StartsWith(';')) then
        continue;

      if (First) and (Value = 'stoplist version 1') then
        continue;
      First := False;

      NewStopListItem := TStopListItem.Create;
      try

        NewStopListItem.LoadFromString(Value);

        if (Merge) then
        begin
          // Check for duplicates
          for StopListItem in Self do
            if (StopListItem.IsEquivalent(NewStopListItem)) then
            begin
              // Duplicate found. Current Group and Enabled state takes precedence.
              FreeAndNil(NewStopListItem);
              break;
            end;
        end;

        if (NewStopListItem <> nil) then
          Add(NewStopListItem);
      except
        NewStopListItem.Free;
        raise;
      end;
    end;

  finally
    Reader.Free;
  end;
end;

procedure TStopListItemList.SaveToFile(const Filename: string);
var
  Writer: TStreamWriter;
  StopListItem: TStopListItem;
begin
  Writer := TStreamWriter.Create(Filename); // Always write ASCII so no need for encoding
  try

    Writer.WriteLine('stoplist version 1');
    for StopListItem in Self do
      Writer.WriteLine(StopListItem.SaveToString);

  finally
    Writer.Free;
  end;
end;

end.
