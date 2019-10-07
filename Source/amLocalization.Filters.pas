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
  amLocalization.Model;

type
  TFilterField = (ffModule, ffElement, ffType, ffName, ffText);
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
  public
    procedure Assign(Filters: TFilterItemList);
    function Evaluate(Module: TLocalizerModule): boolean; overload;
    function Evaluate(Prop: TLocalizerProperty; SkipModule: boolean = True): boolean; overload;
  end;

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
  Result := False;

  if (not Enabled) then
    Exit;

  case FField of
    ffModule:
      Result := Evaluate(Prop.Item.Module.Name);
    ffElement:
      Result := Evaluate(Prop.Item.Name);
    ffType:
      Result := Evaluate(Prop.Item.TypeName);
    ffName:
      Result := Evaluate(Prop.Name);
    ffText:
      Result := Evaluate(Prop.Value);
  end;
end;

function TFilterItem.Evaluate(Module: TLocalizerModule): boolean;
var
  Text: string;
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
      Result := AnsiSameText(Text, Value);
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

{ TFilterItemList }

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
