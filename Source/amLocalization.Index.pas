unit amLocalization.Index;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  System.Threading,
  Generics.Collections,
  amLocalization.Utils,
  amLocalization.Model;

// -----------------------------------------------------------------------------
//
// TLocalizerProjectPropertyLookup
//
// -----------------------------------------------------------------------------
// Implements ILocalizerProjectPropertyLookup
// -----------------------------------------------------------------------------
type
  TLocalizerProjectPropertyLookup = class(TInterfacedObject, ILocalizerProjectPropertyLookup)
  private
    FProject: TLocalizerProject;
    FSanitizeRules: TSanitizeRules;
    FDictionary: TObjectDictionary<string, TLocalizerPropertyList>;
    FTask: ITask;
    FTaskTerminated: boolean;
  protected
    procedure PopulateIndex;

    // ILocalizerProjectPropertyLookup
    function Lookup(const Value: string): TLocalizerPropertyList; overload; // Lookup of sanitized value
    function Lookup(const Prop: TLocalizerProperty): TLocalizerPropertyList; overload;
  public
    constructor Create(AProject: TLocalizerProject; ASanitizeRules: TSanitizeRules);
    destructor Destroy; override;
  end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
{$ifdef DEBUG}
  Diagnostics,
  Windows,
  SysUtils,
{$endif DEBUG}
  Classes;

// -----------------------------------------------------------------------------
//
// TLocalizerProjectPropertyLookup
//
// -----------------------------------------------------------------------------
constructor TLocalizerProjectPropertyLookup.Create(AProject: TLocalizerProject; ASanitizeRules: TSanitizeRules);
begin
  FDictionary := TObjectDictionary<string, TLocalizerPropertyList>.Create([doOwnsValues], TTextComparer.Create);
  FProject := AProject;
  FSanitizeRules := ASanitizeRules;
  FTask := TTask.Run(PopulateIndex);
end;

destructor TLocalizerProjectPropertyLookup.Destroy;
begin
  if (FTask <> nil) then
  begin
    FTaskTerminated := True;
    FTask.Wait;
  end;
  FDictionary.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

function TLocalizerProjectPropertyLookup.Lookup(const Prop: TLocalizerProperty): TLocalizerPropertyList;
begin
  Result := Lookup(SanitizeText(Prop.Value));
end;

function TLocalizerProjectPropertyLookup.Lookup(const Value: string): TLocalizerPropertyList;
begin
  if (FTask <> nil) then
  begin
    FTask.Wait;
    FTask := nil;
  end;

  if (not FDictionary.TryGetValue(Value, Result)) then
    Result := nil;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerProjectPropertyLookup.PopulateIndex;
{$ifdef DEBUG}
var
  StopWatch: TStopWatch;
  Count: integer;
{$endif DEBUG}
begin
  if (FTaskTerminated) then
    Exit;

{$ifdef DEBUG}
  StopWatch := TStopWatch.StartNew;
  Count := 0;
{$endif DEBUG}

  FProject.Traverse(
    function(Module: TLocalizerModule): boolean
    begin
      if (FTaskTerminated) or (Module.IsUnused) then
        Exit(not FTaskTerminated);

      Module.Traverse(
        function(Item: TLocalizerItem): boolean
        begin
          if (FTaskTerminated) or (Item.IsUnused) then
            Exit(not FTaskTerminated);

          Item.Traverse(
            function(Prop: TLocalizerProperty): boolean
            var
              s: string;
              PropertyList: TLocalizerPropertyList;
            begin
{$ifdef DEBUG}
              Inc(Count);
{$endif DEBUG}

              if (FTaskTerminated) or (Prop.IsUnused) then
                Exit(not FTaskTerminated);

              s := SanitizeText(Prop.Value, FSanitizeRules);

              if (not FDictionary.TryGetValue(s, PropertyList)) then
              begin
                PropertyList := TLocalizerPropertyList.Create;
                FDictionary.Add(s, PropertyList);
              end;
              PropertyList.Add(Prop);

              Result := (not FTaskTerminated);
            end);

          Result := (not FTaskTerminated);
        end);

      Result := (not FTaskTerminated);
    end);

{$ifdef DEBUG}
  StopWatch.Stop;
  OutputDebugString(PChar(Format('Project indexed %.0n properties in %.0n mS', [Count*1.0, StopWatch.ElapsedMilliseconds * 1.0])));
{$endif DEBUG}
end;

// -----------------------------------------------------------------------------

end.
