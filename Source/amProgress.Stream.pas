unit amProgress.Stream;

interface

uses
  Classes,
  amProgress.API;

//------------------------------------------------------------------------------
//
//      TProgressStream
//
//------------------------------------------------------------------------------
type
  TProgressStream = class(TStream)
  private
    FStream: TStream;
    FProgress: IProgress;
    FMaxProgress: Int64;
    FLastProgress: int64;
  protected
    procedure UpdateProgress;
  public
    constructor Create(Stream: TStream; const Progress: IProgress); reintroduce; overload;
    constructor Create(Stream: TStream; const Progress: IProgress; AMaxProgress: Int64); reintroduce; overload;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------
//
//      TProgressStream
//
//------------------------------------------------------------------------------
constructor TProgressStream.Create(Stream: TStream; const Progress: IProgress);
begin
  Create(Stream, Progress, Stream.Size);
end;

constructor TProgressStream.Create(Stream: TStream; const Progress: IProgress; AMaxProgress: Int64);
begin
  inherited Create;
  FStream := Stream;
  FProgress := Progress;
  FMaxProgress := AMaxProgress;
  FLastProgress := 0;
end;

function TProgressStream.Read(var Buffer; Count: Longint): Longint;
begin
  UpdateProgress;
  Result := FStream.Read(Buffer, Count);
  UpdateProgress;
end;

function TProgressStream.Write(const Buffer; Count: Longint): Longint;
begin
  UpdateProgress;
  Result := FStream.Write(Buffer, Count);
  UpdateProgress;
end;

function TProgressStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FStream.Seek(Offset, Origin);
  UpdateProgress;
end;

procedure TProgressStream.UpdateProgress;
var
  CurrentPosition: Int64;
begin
  CurrentPosition := FStream.Position;
  if (CurrentPosition = FLastProgress) then //or (FMaxProgress div 100 > Abs(CurrentPosition-FLastProgress)) then
    exit;

  FLastProgress := CurrentPosition;
  if (FLastProgress < FMaxProgress) then
    FProgress.Progress(psProgress, FLastProgress, FMaxProgress)
  else
    FProgress.Progress(psEnd, FLastProgress, FMaxProgress);
end;

end.
