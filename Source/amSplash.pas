unit amSplash;

(*
 * Copyright © 2008 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

{$WARN SYMBOL_PLATFORM OFF}

uses
  MMSystem,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  // TODO : Add support for simple decryption in MemoryStream.Read()
  TMediaPlayerMemoryFile = class(TComponent)
  private
    FFileName: string;
    FMemoryStream: TMemoryStream;
  protected
    procedure SetFileName(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property MemoryStream: TMemoryStream read FMemoryStream;
  published
    property FileName: string read FFileName write SetFileName;
  end;

type
  TSplashAnimate = (saAlways, saNever, saLocal); // saLocal = Only animate if running locally (i.e. not remotely)

  TFormSplash = class(TForm)
    TimerSplash: TTimer;
    TimerBanner: TTimer;
    procedure TimerSplashTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure TimerBannerTimer(Sender: TObject);
  private
    FAbort: boolean;
    FSplashBitmap: TBitmap;
    FCountdown: integer;
    FMemoryFile: TMediaPlayerMemoryFile;
    FDisco: boolean;
    FBanner: string;
    FBannerOffset: integer;
    FBannerBitmap: TBitmap;
    FBannerScroll: integer;
    FBannerFadeZone: integer;
    FVersion: string;
    FAnimate: TSplashAnimate;
    FShouldAnimate: boolean;
    FAnimating: boolean;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPrint(var Message: TWMPrint); message WM_PRINT;
    procedure WMShow(var Message: TMessage); message WM_SHOWWINDOW;
    procedure MMNotify(var Message: TMessage); message MM_MCINOTIFY;
    property ShouldAnimate: boolean read FShouldAnimate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(Timeout: boolean = True; const ResourceName: string = 'SPLASH'; const ResourceType: string = 'PNG');
    procedure PlayThatFunkyMusicWhiteBoy(const ResName, ResType: UnicodeString);
    procedure DisplayBanner(const Value: string);
    procedure DisplayBannerResource(const ResName, ResType: UnicodeString);
    property Disco: boolean read FDisco;
    property Version: string read FVersion write FVersion;
    property Animate: TSplashAnimate read FAnimate write FAnimate;
  end;

implementation

{$R *.dfm}
{$R 'resources\amSplash.res'}

uses
  Types,
  PngImage;

//------------------------------------------------------------------------------
//
//      DetectRemoteSession
//
//------------------------------------------------------------------------------
function DetectRemoteSession: boolean;
const
  SM_REMOTECONTROL      = $2001; // This system metric is used in a Terminal
                                 // Services environment. Its value is nonzero
                                 // if the current session is remotely
                                 // controlled; otherwise, 0.

  SM_REMOTESESSION      = $1000; // This system metric is used in a Terminal
                                 // Services environment. If the calling process
                                 // is associated with a Terminal Services
                                 // client session, the return value is nonzero.
                                 // If the calling process is associated with
                                 // the Terminal Server console session, the
                                 // return value is 0. The console session is
                                 // not necessarily the physical console.
begin
  Result := (GetSystemMetrics(SM_REMOTESESSION) <> 0) or (GetSystemMetrics(SM_REMOTECONTROL) <> 0);
end;

//------------------------------------------------------------------------------
//
//      TMediaPlayerMemoryExtension
//
//------------------------------------------------------------------------------
// Adapted from a usenet post by Colin Wilson
//------------------------------------------------------------------------------
type
  TMediaPlayerMemoryExtension = class
  strict private
    class var
      FExtensionList: TList;
  private
    FExtension: string;
    FFiles: TList;
    class procedure Shutdown;
    function GetCount: integer;
  protected
  public
    constructor Create(const AExtension: string);
    destructor Destroy; override;
    property Extension: string read FExtension;
    function FindFile(const FileName: string): TMediaPlayerMemoryFile;
    class function FindExtension(const Ext: string): TMediaPlayerMemoryExtension;
    property Count: integer read GetCount;
  end;


//------------------------------------------------------------------------------

function IOProc(mmIoInfo: PAnsiChar; Msg: UINT; Param1, Param2: LPARAM): Longint stdcall;
var
  Info: PMMIOInfo;
  MemExt: TMediaPlayerMemoryExtension;
  MemFile: TMediaPlayerMemoryFile;
  FileName, FileExt: string;
begin
  Info := PMMIOInfo(mmIoInfo);

  case Msg of
    MMIOM_OPEN :
      if (Info^.adwInfo[0] <> 0) then
        raise Exception.Create('File is already open')
      else
      begin
        FileName := string(PAnsiChar(Param1));
        Delete(FileName, Length(FileName), 1);
        FileExt := ExtractFileExt(FileName);
        Delete(FileExt, 1, 1);
        FileExt := UpperCase(FileExt);
        MemExt := TMediaPlayerMemoryExtension.FindExtension(FileExt);
        if (MemExt = nil) then
          raise Exception.Create('Internal error');
        MemFile := MemExt.FindFile(FileName);
        MemFile.MemoryStream.Seek(0, soFromBeginning);
        Info^.lDiskOffset := MemFile.MemoryStream.Position;
        Info^.adwInfo[0] := Integer(MemFile);
        Info^.wErrorRet := MMSYSERR_NOERROR;
        Result := MMSYSERR_NOERROR;
      end;

    MMIOM_CLOSE :
      begin
        Info^.adwInfo [0] := 0;
        Result := MMSYSERR_NOERROR;
      end;

    MMIOM_READ :
      begin
        MemFile := TMediaPlayerMemoryFile(Info^.adwInfo[0]);
        Result := MemFile.MemoryStream.Read(pointer(Param1)^, Param2);
        Info^.lDiskOffset := MemFile.MemoryStream.Position;
      end;

    MMIOM_SEEK :
      begin
        MemFile := TMediaPlayerMemoryFile(Info^.adwInfo[0]);
        case Param2 of
          SEEK_CUR: Result := MemFile.MemoryStream.Seek(Param1, soFromCurrent);
          SEEK_END: Result := MemFile.MemoryStream.Seek(Param1, soFromEnd);
          SEEK_SET: Result := MemFile.MemoryStream.Seek(Param1, soFromBeginning);
        else
          Result := -1
        end;
        Info^.lDiskOffset := MemFile.MemoryStream.Position;
      end;

    MMIOM_WRITE :
      begin
        MemFile := TMediaPlayerMemoryFile(Info^.adwInfo[0]);
        Result := MemFile.fMemoryStream.Write(pointer(Param1)^, Param2);
        Info^.lDiskOffset := MemFile.MemoryStream.Position;
      end;

    else
      Result := -1;
  end;
end;

//------------------------------------------------------------------------------

class function TMediaPlayerMemoryExtension.FindExtension(const Ext: string): TMediaPlayerMemoryExtension;
var
  i : Integer;
begin
  Result := nil;
  if (FExtensionList <> nil) then
    for i := 0 to FExtensionList.Count - 1 do
      if (TMediaPlayerMemoryExtension(FExtensionList [i]).Extension = Ext) then
      begin
        Result := TMediaPlayerMemoryExtension(FExtensionList[i]);
        break;
      end;
end;

//------------------------------------------------------------------------------

constructor TMediaPlayerMemoryExtension.Create(const AExtension: string);
begin
  inherited Create;
  FExtension := AExtension;
  if (FExtensionList = nil) then
    FExtensionList := TList.Create;
  FExtensionList.Add (Self);
  FFiles := TList.Create;

  mmioInstallIOProc(mmioStringToFourCC(PChar(FExtension), 0), IOProc, MMIO_INSTALLPROC); // or MMIO_GLOBALPROC);
end;

//------------------------------------------------------------------------------

destructor TMediaPlayerMemoryExtension.Destroy;
begin
  if (FExtensionList <> nil) then
    FExtensionList.Remove(Self);
  FFiles.Free;
  mmioInstallIOProc(mmioStringToFourCC(PChar(FExtension), 0), nil, MMIO_REMOVEPROC);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TMediaPlayerMemoryExtension.FindFile(const FileName: string): TMediaPlayerMemoryFile;
var
  i : integer;
begin
  Result := nil;
  for i := 0 to FFiles.Count-1 do
    if (TMediaPlayerMemoryFile(FFiles[i]).FileName = FileName) then
    begin
      Result := TMediaPlayerMemoryFile(FFiles[i]);
      break
    end
end;

//------------------------------------------------------------------------------

function TMediaPlayerMemoryExtension.GetCount: integer;
begin
  Result := FFiles.Count;
end;

//------------------------------------------------------------------------------

class procedure TMediaPlayerMemoryExtension.Shutdown;
var
  i: integer;
begin
  if (FExtensionList <> nil) then
  begin
    for i := 0 to FExtensionList.Count-1 do
      TObject(FExtensionList[i]).Free;
    FExtensionList.Free;
    FExtensionList := nil;
  end;
end;



//------------------------------------------------------------------------------
//
//      TMediaPlayerMemoryFile
//
//------------------------------------------------------------------------------
// Adapted from a usenet post by Colin Wilson
//------------------------------------------------------------------------------
constructor TMediaPlayerMemoryFile.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  FMemoryStream := TMemoryStream.Create;
end;

//------------------------------------------------------------------------------

destructor TMediaPlayerMemoryFile.Destroy;
begin
  FileName := '';
  FMemoryStream.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TMediaPlayerMemoryFile.SetFileName(const Value: string);
var
  OldExt, NewExt : string;
  OldMemExt, NewMemExt : TMediaPlayerMemoryExtension;
begin
  if not(csDesigning in ComponentState) then
  begin
    if (FFileName <> Value) then
    begin
      NewMemExt := nil;

      if (Value <> '') then
        NewExt := UpperCase(Copy(ExtractFileExt(Value), 2, MaxInt))
      else
        NewExt := '';

      if (NewExt = '') and (Value <> '') then
        raise Exception.Create ('Invalid file name');

      if (FFileName <> '') then
        OldExt := UpperCase(Copy(ExtractFileExt(FFileName), 2, MaxInt))
      else
        OldExt := '';

      if (Value <> '') then
      begin
        NewMemExt := TMediaPlayerMemoryExtension.FindExtension(NewExt);
        if (NewMemExt <> nil) and (NewMemExt.FindFile(Value) <> nil) then
          raise Exception.Create ('File name is in use');
      end;

      if (OldExt <> '') then
      begin
        OldMemExt := TMediaPlayerMemoryExtension.FindExtension(OldExt);
        if (OldMemExt <> nil) then
          if (OldMemExt.Count = 1) then
            OldMemExt.Free
          else
            OldMemExt.FFiles.Remove(OldMemExt.FindFile(FFileName));
      end;

      if Value <> '' then
      begin
        if NewMemExt = Nil then
          NewMemExt := TMediaPlayerMemoryExtension.Create (NewExt);

        NewMemExt.FFiles.Add (Self)
      end
    end
  end;

  FFileName := Value
end;


//------------------------------------------------------------------------------
//
//      TFormSplash
//
//------------------------------------------------------------------------------

constructor TFormSplash.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque]; // Background covers whole form
  FSplashBitmap := TBitmap.Create;
  FBannerScroll := 1;
  FBannerFadeZone := 32;
  FAnimate := saLocal;

  if (Screen.Fonts.IndexOf(Font.Name) = -1) then
    Font.Name := Screen.MessageFont.Name;
end;

//------------------------------------------------------------------------------

procedure TFormSplash.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  // Enable alpha (required by AnimateWindow)
  Params.ExStyle := WS_EX_LAYERED;

  // Make window topmost unless [Ctrl] is pressed or debugging
  if (GetAsyncKeyState(VK_CONTROL) and $8000 = 0) xor (DebugHook <> 0) then
    Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST;

  // Make splash visible on the task bar in case the main form takes a while to appear.
  // This also ensures that the application doesn't "disappear" from the user if
  // they start the application and then immediately switch to another application.
  if (Application.MainForm = nil) or (not Application.MainForm.Visible) then
    Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

//------------------------------------------------------------------------------

destructor TFormSplash.Destroy;
begin
  FreeAndNil(FSplashBitmap);
  FreeAndNil(FBannerBitmap);

  mciSendString('close Disco', nil, 0, 0);
  FDisco := False;
  FreeAndNil(FMemoryFile);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TFormSplash.DisplayBanner(const Value: string);
begin
  FBanner := Value;
  FBanner := StringReplace(FBanner, '<version>', FVersion, [rfReplaceAll, rfIgnoreCase]);
  FBannerOffset := 0;
end;

procedure TFormSplash.DisplayBannerResource(const ResName, ResType: UnicodeString);
var
  Banner: string;
  Resource: TResourceStream;
begin
  if (FindResource(hInstance, PChar(ResName), PChar(ResType)) = 0) then
    exit;

  Resource := TResourceStream.Create(hInstance, PChar(ResName), PChar(ResType));
  try
    SetLength(Banner, Resource.Size div SizeOf(Char));

    Resource.Read(PChar(Banner)^, Resource.Size);
  finally
    Resource.Free;
  end;

  DisplayBanner(Banner);
end;

//------------------------------------------------------------------------------

procedure TFormSplash.Execute(Timeout: boolean; const ResourceName: string; const ResourceType: string);
var
  Ticks: DWORD;
  BlendFunction: TBlendFunction;
  BitmapPos: TPoint;
  BitmapSize: TSize;
  exStyle: DWORD;
  PNG: TPngImage;
  Stream: TStream;
begin
  FShouldAnimate := (FAnimate = saAlways) or ((FAnimate = saLocal) and (not DetectRemoteSession));

  // Enable window layering
  exStyle := GetWindowLongA(Handle, GWL_EXSTYLE);
  if (exStyle and WS_EX_LAYERED = 0) then
    SetWindowLong(Handle, GWL_EXSTYLE, exStyle or WS_EX_LAYERED);

  Stream := TResourceStream.Create(HInstance, ResourceName, PWideChar(ResourceType));
  try
    PNG := TPngImage.Create;
    try
      PNG.LoadFromStream(Stream);
      FSplashBitmap.Assign(PNG);
      ASSERT(FSplashBitmap.PixelFormat = pf32bit, 'Wrong bitmap format - must be 32 bits/pixel');
    finally
      PNG.Free;
    end;
    FSplashBitmap.AlphaFormat := afPremultiplied;
  finally
    Stream.Free;
  end;


  // Resize form to fit bitmap
  ClientWidth := FSplashBitmap.Width;
  ClientHeight := FSplashBitmap.Height;

  // Position bitmap on form
  BitmapPos := Point(0, 0);
  BitmapSize.cx := FSplashBitmap.Width;
  BitmapSize.cy := FSplashBitmap.Height;

  // Setup alpha blending parameters
  BlendFunction.BlendOp := AC_SRC_OVER;
  BlendFunction.BlendFlags := 0;
  BlendFunction.SourceConstantAlpha := 0; // Start completely transparent
  BlendFunction.AlphaFormat := AC_SRC_ALPHA;

  if (Application.MainForm <> nil) then
    Caption := Application.MainForm.Caption
  else
    Caption := Application.Title;

  Show;

  if (ShouldAnimate) then
  begin
    // AnimateWindowProc(Handle, 1000, AW_BLEND or AW_ACTIVATE);

    // ... and action!
    Ticks := 0;
    FAnimating := True;
    try
      while (BlendFunction.SourceConstantAlpha < 255) and (not FAbort) do
      begin
        while (Ticks = GetTickCount) do
          Sleep(10); // Don't fade too fast
        Ticks := GetTickCount;
        inc(BlendFunction.SourceConstantAlpha,
          (255-BlendFunction.SourceConstantAlpha) div 32+1); // Fade in
        UpdateLayeredWindow(Handle, 0, nil, @BitmapSize, FSplashBitmap.Canvas.Handle,
          @BitmapPos, 0, @BlendFunction, ULW_ALPHA);
        Application.ProcessMessages; // Warning: UI recursion here!
      end;
    finally
      FAnimating := False;
    end;
  end else
  begin
    BlendFunction.SourceConstantAlpha := 255;
    UpdateLayeredWindow(Handle, 0, nil, @BitmapSize, FSplashBitmap.Canvas.Handle,
      @BitmapPos, 0, @BlendFunction, ULW_ALPHA);
  end;

  // Start timer to hide form after a short while
  FCountdown := 2000 div TimerSplash.Interval; // 2 seconds
  if (FAbort) then
    Close
  else
  begin
    TimerSplash.Enabled := Timeout;
    TimerBanner.Enabled := (FBanner <> '');
  end;
end;

//------------------------------------------------------------------------------

procedure TFormSplash.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TimerBanner.Enabled := False;
  TimerSplash.Enabled := False;

  if (FAnimating) then
  begin
    // Not safe to destroy - Hide instead and wait to be closed again
    FAbort := True;
    Action := caHide;
  end else
    // Safe to destroy
    Action := caFree;
end;

//------------------------------------------------------------------------------

procedure TFormSplash.FormKeyPress(Sender: TObject; var Key: Char);
begin
  FAbort := True;
  TimerSplash.Enabled := True;
end;

//------------------------------------------------------------------------------

procedure TFormSplash.MMNotify(var Message: TMessage);
//var
//  Status: string;
begin
  // The "stopped" notification is received sporadically, so we just act on whatever we get
(*
  SetLength(Status, 255);
  mciSendString('status Disco mode', PChar(Status), Length(Status), Handle);
  if (SameText('stopped', PChar(Status))) then
*)
    TimerSplash.Enabled := True;
end;

//------------------------------------------------------------------------------

var
  StupidUniqueTrick: integer = 0;

procedure TFormSplash.PlayThatFunkyMusicWhiteBoy(const ResName, ResType: UnicodeString);
var
  FileName: string;
  Command: string;
  Resource: TResourceStream;
begin
  if (FindResource(hInstance, PChar(ResName), PChar(ResType)) = 0) then
    exit;

  FileName := Format('disco%d.mp3', [StupidUniqueTrick]);
  StupidUniqueTrick := (StupidUniqueTrick+1) and $FFFF;

  FMemoryFile := TMediaPlayerMemoryFile.Create(Self);
  FMemoryFile.FileName := Filename;


  Resource := TResourceStream.Create(hInstance, PChar(ResName), PChar(ResType));
  try
    FMemoryFile.MemoryStream.CopyFrom(Resource, 0);
  finally
    Resource.Free;
  end;
  FMemoryFile.MemoryStream.Position := 0;


  Command := Format('open %s+ type WaveAudio alias Disco', [FileName]);

  if (mciSendString(PChar(Command), nil, 0, Handle) = 0) then
  begin
    mciSendString('play Disco notify', nil, 0, Handle);
    FDisco := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TFormSplash.TimerBannerTimer(Sender: TObject);

  function Div255(n: Cardinal): Cardinal; inline;
  begin
    Result := (n * $8081) shr 23;

    // Or maybe this:
    //Result := (n + $32) shr 8;

    // Or this:
    // Result := (n * 257 + 257) shr 16;

    // Or even:
    // Result := Int64(n * $80808081) shr 39

    // Result := ((n shr 8) + n + 1) shr 8;
  end;

var
  r: TRect;
  Row, Col: integer;
  PixSrc, PixDst: PDWORD;
  BitmapPos: TPoint;
  BitmapSize: TSize;
  BlendFunction: TBlendFunction;
  Scale: Byte;
  RGB: DWORD;
begin
  if (FBannerBitmap = nil) then
  begin
    FBannerBitmap := TBitmap.Create;
    FBannerBitmap.PixelFormat := pf32bit;
    FBannerBitmap.SetSize(FSplashBitmap.Width, FSplashBitmap.Height);
    FBannerOffset := FBannerBitmap.Height;
  end;

  dec(FBannerOffset, FBannerScroll);

  FBannerBitmap.Canvas.Brush.Color := clWhite;
  FBannerBitmap.Canvas.Font.Assign(Self.Font);

  FBannerBitmap.Canvas.FillRect(FBannerBitmap.Canvas.ClipRect);

  r.Top := FBannerOffset;
  r.Left := 0;
  r.Right := FBannerBitmap.Width-1;
  FBannerBitmap.Canvas.TextRect(r, FBanner, [tfCalcRect, tfCenter, tfNoPrefix, tfTop, tfWordBreak]);

  if (r.Bottom < 0) then
  begin
    FBannerOffset := FBannerBitmap.Height - FBannerScroll;

    OffsetRect(r, 0, FBannerOffset - r.top);
  end;

  r.Right := FBannerBitmap.Width-1;
  FBannerBitmap.Canvas.TextRect(r, FBanner, [tfCenter, tfNoPrefix, tfTop, tfWordBreak]);

  for Row := 0 to FBannerBitmap.Height-1 do
  begin
    PixSrc := PDWORD(FSplashBitmap.ScanLine[Row]);
    PixDst := PDWORD(FBannerBitmap.ScanLine[Row]);

    for Col := 0 to FBannerBitmap.Width-1 do
    begin
      Scale := ((PixDst^ and $FF) + (PixDst^ shr 8 and $FF) + (PixDst^ shr 16 and $FF)) div 3; // Must use average of all three to work around ClearType AA

      if (Scale <= 255) then
      begin
        // Fade
        if (Row < FBannerFadeZone) then
          Scale := 255 - ((255-Scale) * (Row+1) div FBannerFadeZone)
        else
        if (Row > FBannerBitmap.Height-FBannerFadeZone) then
          Scale := 255 - ((255-Scale) * (FBannerBitmap.Height-Row) div FBannerFadeZone);
      end;

      if (Scale = 255) then
        PixDst^ := PixSrc^
      else
      if (Scale = 0) then
        PixDst^ := PixSrc^ and $FF000000
      else
      begin
        RGB := PixSrc^ and $00FFFFFF;
        PixDst^ := (PixSrc^ and $FF000000) or
          (Div255((RGB shr 16) * Scale) shl 16) or
          (Div255((RGB shr 8 and $FF) * Scale) shl 8) or
          (Div255((RGB and $FF) * Scale));
      end;

      inc(PixSrc);
      inc(PixDst);
    end;
  end;

  BitmapPos := Point(0, 0);
  BitmapSize.cx := FBannerBitmap.Width;
  BitmapSize.cy := FBannerBitmap.Height;
  BlendFunction.BlendOp := AC_SRC_OVER;
  BlendFunction.BlendFlags := 0;
  BlendFunction.SourceConstantAlpha := 255;
  BlendFunction.AlphaFormat := AC_SRC_ALPHA;
  UpdateLayeredWindow(Handle, 0, nil, @BitmapSize, FBannerBitmap.Canvas.Handle, @BitmapPos, 0, @BlendFunction, ULW_ALPHA);
end;

//------------------------------------------------------------------------------

procedure TFormSplash.TimerSplashTimer(Sender: TObject);
var
  BlendFunction: TBlendFunction;
  BitmapPos: TPoint;
  BitmapSize: TSize;
  Ticks: DWORD;
  Bitmap: TBitmap;
begin
  dec(FCountdown);

  if (FCountdown <= 0) then
  begin
    if (ShouldAnimate) and (not FAbort) then
    begin
      TimerSplash.Enabled := False;
      TimerBanner.Enabled := False;

      Bitmap := FSplashBitmap;
      if (FBannerBitmap <> nil) then
        Bitmap := FBannerBitmap;

      // Position bitmap on form
      BitmapPos := Point(0, 0);
      BitmapSize.cx := Bitmap.Width;
      BitmapSize.cy := Bitmap.Height;

      // Setup alpha blending parameters
      BlendFunction.BlendOp := AC_SRC_OVER;
      BlendFunction.BlendFlags := 0;
      BlendFunction.SourceConstantAlpha := 255; // Start completely opaque
      BlendFunction.AlphaFormat := AC_SRC_ALPHA;

      // ... and action!
      Ticks := 0;
      FAnimating := True; // Attempt to work around spurious errors involving UpdateLayeredWindow and Bitmap.Canvas.Handle
      try
        while (BlendFunction.SourceConstantAlpha > 0) and (not FAbort) do
        begin
          while (Ticks = GetTickCount) do
            Sleep(10); // Don't fade too fast
          Ticks := GetTickCount;
          dec(BlendFunction.SourceConstantAlpha,
            BlendFunction.SourceConstantAlpha div 16+1); // Fade out
          UpdateLayeredWindow(Handle, 0, nil, @BitmapSize, Bitmap.Canvas.Handle,
            @BitmapPos, 0, @BlendFunction, ULW_ALPHA);
          Application.ProcessMessages;
        end;
      finally
        FAnimating := False;
      end;
    end;

    FAbort := True;
  end;

  if (FAbort) then
    Close;
end;

//------------------------------------------------------------------------------

procedure TFormSplash.WMActivate(var Message: TWMActivate);
begin
  if (Message.Active = WA_INACTIVE) then
  begin
    // Something else has been activated (e.g. user has clicked main form) - hide splash
    FAbort := True;
    TimerSplash.Enabled := True;
  end else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TFormSplash.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1; // Avoid flicker
end;

//------------------------------------------------------------------------------

procedure TFormSplash.WMNCHitTest(var Message: TWMNCHitTest);
begin
  Message.Result := HTCAPTION;
end;

//------------------------------------------------------------------------------

procedure TFormSplash.WMPrint(var Message: TWMPrint);
begin
  // AnimateWindow requires that WM_PRINT is implemented.
  try
    PaintTo(Message.DC, 0, 0);
  except
    // Kill range check errors
  end;
end;

//------------------------------------------------------------------------------

procedure TFormSplash.WMShow(var Message: TMessage);
begin
(*
  // WM_SHOWWINDOW is received after OnShow has fired and the window position
  // has been set, but before the window is actually visible.
  if (Message.wParam <> 0) and (not IsWindowVisible(Handle)) then
  begin
    if (FShouldAnimate) then
      AnimateWindowProc(Handle, 1000, AW_BLEND or AW_ACTIVATE);
//    HideBootLogger;
  end;
*)
  inherited;
end;

//------------------------------------------------------------------------------

initialization
finalization
  TMediaPlayerMemoryExtension.Shutdown;
end.
