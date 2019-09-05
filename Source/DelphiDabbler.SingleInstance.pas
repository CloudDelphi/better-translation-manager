unit DelphiDabbler.SingleInstance;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

(*
  The original code is Copyright Peter D Johnson
  www.delphidabbler.com, (c) 2004-2005.

  The code may be used in any way providing it is not sold or passed off as
  the work of another person.

  The code is used at the user's own risk. No guarantee or warranty is provided.
*)

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

interface

uses
  Windows,
  Controls,
  Messages;

type
  TSingleInstanceParamHandler = procedure(const Param: string) of object;

  TSingleInstanceClass = class of TSingleInstance;

  TSingleInstance = class(TObject)
  private
    FOnProcessParam: TSingleInstanceParamHandler;
    FRequestRestoreMsg: UINT;
  protected
    function WindowClassName: string; virtual;
    function Fingerprint: DWORD; virtual;
    function FindDuplicateMainWindow: HWND; virtual;
    function SendParamsToPrevInst(Window: HWND): Boolean; virtual;
    function SwitchToPrevInst(Window: HWND): Boolean;
    procedure ProcessRestoreRequest(Window: HWND; var Msg: TMessage); dynamic;
    procedure WMCopyData(var Msg: TWMCopyData); dynamic;
  public
    constructor Create;

    procedure CreateParams(var Params: TCreateParams);
    function HandleMessages(Window: HWND; var Msg: TMessage): Boolean;
    function CanStartApp: Boolean;

    property OnProcessParam: TSingleInstanceParamHandler read FOnProcessParam write FOnProcessParam;
  end;

procedure RegisterSingleInstanceClass(ASingleInstClass: TSingleInstanceClass);
function SingleInstance: TSingleInstance;


implementation

uses
  SysUtils,
  Forms;

var
  FSingleInstance: TSingleInstance = nil;
  FSingleInstanceClass: TSingleInstanceClass = nil;

function SingleInstance: TSingleInstance;
begin
  if (FSingleInstance = nil) then
  begin
    if (FSingleInstanceClass <> nil) then
      FSingleInstance := FSingleInstanceClass.Create
    else
      FSingleInstance := TSingleInstance.Create;
  end;
  Result := FSingleInstance;
end;

procedure RegisterSingleInstanceClass(ASingleInstClass: TSingleInstanceClass);
begin
  FSingleInstanceClass := ASingleInstClass;
end;

{ TSingleInstance }

function TSingleInstance.CanStartApp: Boolean;
var
  Handle: HWND;
begin
  Handle := FindDuplicateMainWindow;
  if Handle = 0 then
    Result := True
  else
    Result := not SwitchToPrevInst(Handle);
end;

constructor TSingleInstance.Create;
begin
  inherited;
  FRequestRestoreMsg := RegisterWindowMessage('SingleInstanceRequestRestore');
end;

procedure TSingleInstance.CreateParams(var Params: TCreateParams);
begin
  inherited;
  StrPLCopy(Params.WinClassName, WindowClassName, SizeOf(Params.WinClassName) - 1);
end;

procedure TSingleInstance.ProcessRestoreRequest(Window: HWND; var Msg: TMessage);
begin
  if IsIconic(Application.Handle) then
    Application.Restore
  else
  if (Application.MainForm <> nil) and (Application.MainForm.WindowState = wsMinimized) then
    Application.MainForm.WindowState := wsNormal;

  if (Application.MainForm <> nil) and (not Application.MainForm.Visible) then
    Application.MainForm.Visible := True;

  Application.BringToFront;
  SetForegroundWindow(Window);
end;

function TSingleInstance.FindDuplicateMainWindow: HWND;
begin
  Result := FindWindow(PChar(WindowClassName), nil);
end;

function TSingleInstance.HandleMessages(Window: HWND; var Msg: TMessage): Boolean;
begin
  if (Application.MainForm <> nil) and (not Application.MainForm.Visible) then
    Exit(false); // e.g if license window is displayed -> returns control to calling program, i.e. not running single instance

  if (Msg.Msg = WM_COPYDATA) then
  begin
    WMCopyData(TWMCopyData(Msg));
    Result := True;
  end else
  if (Msg.Msg = FRequestRestoreMsg) then
  begin
    ProcessRestoreRequest(Window, Msg);
    Result := True;
  end else
    Result := false;
end;

function TSingleInstance.SendParamsToPrevInst(Window: HWND): Boolean;
var
  CopyData: TCopyDataStruct;
  i: Integer;
  DataSize: Integer;
  Data: string;
  PData: PChar;
begin
  DataSize := 0;
  for i := 1 to ParamCount do
    Inc(DataSize, Length(ParamStr(i)) + 1);
  Inc(DataSize);
  SetLength(Data, DataSize);

  PData := PChar(Data);
  for i := 1 to ParamCount do
  begin
    StrPCopy(PData, ParamStr(i));
    Inc(PData, Length(ParamStr(i)) + 1);
  end;
  // Extra terminating zero - not really necessary since we're passing the size along with the data
  PData^ := #0;

  CopyData.lpData := PChar(Data);
  CopyData.cbData := DataSize * SizeOf(Char);
  CopyData.dwData := Fingerprint;

  Result := SendMessage(Window, WM_COPYDATA, 0, LPARAM(@CopyData)) = 1;
end;

function TSingleInstance.SwitchToPrevInst(Window: HWND): Boolean;
var
  OtherProcessID: DWORD;
begin
  Assert(Window <> 0);

  // See: http://blogs.msdn.com/b/oldnewthing/archive/2009/02/20/9435239.aspx
  if (GetWindowThreadProcessId(Window, OtherProcessID) <> 0) then
    AllowSetForegroundWindow(OtherProcessID);

  if ParamCount > 0 then
    Result := SendParamsToPrevInst(Window)
  else
    Result := True;

  if Result then
    SendMessage(Window, FRequestRestoreMsg, 0, 0);
end;

function TSingleInstance.Fingerprint: DWORD;
begin
  Result := 0;
end;

function TSingleInstance.WindowClassName: string;
begin
  Result := 'SingleInst.MainWdw';
end;

procedure TSingleInstance.WMCopyData(var Msg: TWMCopyData);
var
  PData: PChar;
  Param: string;
  Size: integer;
begin
  if (Msg.CopyDataStruct.dwData = Fingerprint) then
  begin
    if Assigned(FOnProcessParam) then
    begin
      PData := Msg.CopyDataStruct.lpData;
      Size := Msg.CopyDataStruct.cbData;

      while (Size > 0) and (PData^ <> #0) do
      begin
        Param := PData;

        FOnProcessParam(Param);

        Inc(PData, Length(Param) + 1);
        Dec(Size, Length(Param) + 1);
      end;
    end;
    Msg.Result := 1;
  end else
    Msg.Result := 0;
end;

initialization

finalization

  FSingleInstance.Free;

end.
