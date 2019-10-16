unit amLocalization.Filters;

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
  amProgress,
  amLocalization.Model;

type
  TFilterField = (ffModule, ffElement, ffType, ffName, ffTypeAndName, ffText);
  TFilterOperator = (foEquals, foStarts, foEnds, ftContains, foRegExp);

  TFilterItem = class
  private
    FField: TFilterField;
    FValue: string;
    FFilterOperator: TFilterOperator;
    FGroup: string;
    FEnabled: boolean;
  public
    procedure Assign(Filter: TFilterItem);

    function ExtractValue(Prop: TLocalizerProperty): string; overload;
    class function ExtractValue(FilterField: TFilterField; Prop: TLocalizerProperty): string; overload;

    function Evaluate(Module: TLocalizerModule): boolean; overload;
    function Evaluate(Prop: TLocalizerProperty): boolean; overload;
    function Evaluate(const Text: string): boolean; overload;

    property Group: string read FGroup write FGroup;
    property Field: TFilterField read FField write FField;
    property FilterOperator: TFilterOperator read FFilterOperator write FFilterOperator;
    property Value: string read FValue write FValue;
    property Enabled: boolean read FEnabled write FEnabled;
  end;

  TFilterItemList = class(TObjectList<TFilterItem>)
  public type
    TFilterStats = record
      ModuleCount: integer;
      PropertyCount: integer;
    end;
  public
    procedure Assign(Filters: TFilterItemList);

    function Apply(Project: TLocalizerProject; Callback: TLocalizerObjectDelegate = nil; const Progress: IProgress = nil): TFilterStats;

    function Evaluate(Module: TLocalizerModule): boolean; overload;
    function Evaluate(Prop: TLocalizerProperty; SkipModule: boolean = True): boolean; overload;
  end;

resourcestring
  sFilterGroupGeneralDisplay = 'General';

const
  sFilterGroupGeneral = 'General';
  sFilterTypeNameSeparator = '.';

implementation

uses
  SysUtils,
  StrUtils;

{ TFilterItem }

procedure TFilterItem.Assign(Filter: TFilterItem);
begin
  FField := Filter.Field;
  FValue := Filter.Value;
  FFilterOperator := Filter.FilterOperator;
  FGroup := Filter.Group;
  FEnabled := Filter.Enabled;
end;

function TFilterItem.Evaluate(Prop: TLocalizerProperty): boolean;
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

function TFilterItem.Evaluate(Module: TLocalizerModule): boolean;
begin
  Result := False;

  if (not Enabled) then
    Exit;

  if (FField <> ffModule) then
    Exit;

  Result := Evaluate(Module.Name);
end;

function TFilterItem.Evaluate(const Text: string): boolean;
begin
  Result := False;
  case FFilterOperator of
    foEquals:
      Result := AnsiSameText(Value, Text);
    foStarts:
      Result := Text.StartsWith(Value, True);
    foEnds:
      Result := Text.EndsWith(Value, True);
    ftContains:
      Result := Text.ToLower.Contains(Value.ToLower);
    foRegExp:
      Result := False; // TODO
  end;
end;

class function TFilterItem.ExtractValue(FilterField: TFilterField; Prop: TLocalizerProperty): string;
begin
  case FilterField of
    ffModule:
      Result := Prop.Item.Module.Name;
    ffElement:
      Result := Prop.Item.Name;
    ffType:
      Result := Prop.Item.TypeName;
    ffName:
      Result := Prop.Name;
    ffTypeAndName:
      Result := Prop.Item.TypeName + sFilterTypeNameSeparator + Prop.Name;
    ffText:
      Result := Prop.Value;
  else
    Result := '';
  end;
end;

function TFilterItem.ExtractValue(Prop: TLocalizerProperty): string;
begin
  Result := ExtractValue(FField, Prop);
end;

{ TFilterItemList }

function TFilterItemList.Apply(Project: TLocalizerProject; Callback: TLocalizerObjectDelegate; const Progress: IProgress): TFilterStats;
var
  Stats: TFilterStats;
begin
  if (Progress <> nil) then
    Progress.Progress(psBegin, 0, Project.Modules.Count+Project.PropertyCount);

  Stats := Default(TFilterStats);

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

  Progress.Progress(psEnd, 1, 1);

  Result := Stats;
end;

procedure TFilterItemList.Assign(Filters: TFilterItemList);
var
  Filter: TFilterItem;
  i: integer;
begin
  Clear;
  for i := 0 to Filters.Count-1 do
  begin
    Filter := TFilterItem.Create;
    Add(Filter);
    Filter.Assign(Filters[i]);
  end;
end;

function TFilterItemList.Evaluate(Module: TLocalizerModule): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Count-1 do
    if (Items[i].Evaluate(Module)) then
      Exit(True);
end;

function TFilterItemList.Evaluate(Prop: TLocalizerProperty; SkipModule: boolean): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Count-1 do
  begin
    if (SkipModule) and (Items[i].Field = ffModule) then
      continue;
    if (Items[i].Evaluate(Prop)) then
      Exit(True);
  end;
end;

end.
