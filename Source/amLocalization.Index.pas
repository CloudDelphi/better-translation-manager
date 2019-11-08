unit amLocalization.Index;

interface

uses
  System.Threading,
  Generics.Collections,
  amLocalization.Utils,
  amLocalization.Model;

type
  TPropertyList = TList<TLocalizerProperty>;

  TLocalizerProjectIndex = class
  private
    FProject: TLocalizerProject;
    FDictionary: TObjectDictionary<string, TPropertyList>;
    FTask: ITask;
    FTaskTerminated: boolean;
  protected
    procedure PopulateIndex;
  public
    constructor Create;
    destructor Destroy; override;

    procedure BuildIndex(AProject: TLocalizerProject);

    function Lookup(const Value: string): TPropertyList; overload; // Lookup of sanitized value
    function Lookup(const Prop: TLocalizerProperty): TPropertyList; overload;
  end;

implementation

uses
{$ifdef DEBUG}
  Diagnostics,
  Windows,
  SysUtils,
{$endif DEBUG}
  Classes;

{ TProjectIndexer }

constructor TLocalizerProjectIndex.Create;
begin
  FDictionary := TObjectDictionary<string, TPropertyList>.Create([doOwnsValues], TTextComparer.Create);
end;

destructor TLocalizerProjectIndex.Destroy;
begin
  if (FTask <> nil) then
  begin
    FTaskTerminated := True;
    FTask.Wait;
  end;
  FDictionary.Free;
  inherited;
end;

function TLocalizerProjectIndex.Lookup(const Prop: TLocalizerProperty): TPropertyList;
begin
  Result := Lookup(SanitizeText(Prop.Value, False));
end;

function TLocalizerProjectIndex.Lookup(const Value: string): TPropertyList;
begin
  if (FTask <> nil) then
  begin
    FTask.Wait;
    FTask := nil;
  end;

  if (not FDictionary.TryGetValue(Value, Result)) then
    Result := nil;
end;

procedure TLocalizerProjectIndex.PopulateIndex;
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
              PropertyList: TPropertyList;
            begin
{$ifdef DEBUG}
              Inc(Count);
{$endif DEBUG}

              if (FTaskTerminated) or (Prop.IsUnused) then
                Exit(not FTaskTerminated);

              s := SanitizeText(Prop.Value, False);

              if (not FDictionary.TryGetValue(s, PropertyList)) then
              begin
                PropertyList := TPropertyList.Create;
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

procedure TLocalizerProjectIndex.BuildIndex(AProject: TLocalizerProject);
begin
  if (FTask <> nil) then
  begin
    FTaskTerminated := True;
    FTask.Wait;
    FTaskTerminated := False;
  end;

  FDictionary.Clear;
  FProject := AProject;

  FTask := TTask.Run(PopulateIndex);
end;

end.
