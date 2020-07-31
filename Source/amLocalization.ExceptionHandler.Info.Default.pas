unit amLocalization.ExceptionHandler.Info.Default;

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
  amLocalization.ExceptionHandler.API;

type
  TCustomExceptionInfoProvider = class;
  TExceptionInfoProviderClass = class of TCustomExceptionInfoProvider;

  TCustomExceptionInfoProvider = class abstract(TInterfacedObject, IExceptionInfoProvider)
  private
    class var FExceptionInfoProviders: TDictionary<TExceptionInfoProviderClass, IExceptionInfoProvider>;
  protected
    // IExceptionInfoProvider
    procedure GetExceptionInfo(const ExceptIntf: IUnknown; const ExceptionInfoConsumer: IExceptionInfoConsumer); virtual; abstract;
  public
    class destructor Destroy;

    class procedure RegisterExceptionInfoProvider;
    class procedure UnregisterExceptionInfoProvider;
  end;

  TExceptionInfoProviderSystem = class(TCustomExceptionInfoProvider)
  protected
    procedure GetExceptionInfo(const ExceptIntf: IUnknown; const ExceptionInfoConsumer: IExceptionInfoConsumer); override;
  end;

  TExceptionInfoProviderApplication = class(TCustomExceptionInfoProvider)
  protected
    procedure GetExceptionInfo(const ExceptIntf: IUnknown; const ExceptionInfoConsumer: IExceptionInfoConsumer); override;
  end;

  TExceptionInfoProviderForms = class(TCustomExceptionInfoProvider)
  protected
    procedure GetExceptionInfo(const ExceptIntf: IUnknown; const ExceptionInfoConsumer: IExceptionInfoConsumer); override;
  end;

  TExceptionInfoProviderFolders = class(TCustomExceptionInfoProvider)
  protected
    procedure GetExceptionInfo(const ExceptIntf: IUnknown; const ExceptionInfoConsumer: IExceptionInfoConsumer); override;
  end;

implementation

uses
  Windows,
  SysUtils,
  Themes,
  Forms,
  IOUtils;

function BoolToStr(Value: boolean): string;
begin
  if Value then
    Result := 'true'
  else
    Result := 'false';
end;

procedure TExceptionInfoProviderSystem.GetExceptionInfo(const ExceptIntf: IInterface; const ExceptionInfoConsumer: IExceptionInfoConsumer);

  function GetProcessDepState: string;
  {$ifndef WIN64}
  const
    PROCESS_DEP_ENABLE: DWORD=$00000001;
    PROCESS_DEP_DISABLE_ATL_THUNK_EMULATION: DWORD=$00000002;
  var
    GetProcessDEPPolicy: function(hProcess: THandle; out Flags: DWORD; out Permanent: BOOL): BOOL; stdcall;
    Flags: DWORD;
    Permanent: BOOL;
  {$endif WIN64}
  begin
  {$ifdef WIN64}
    Exit('(not supported on Win64)');
  {$else WIN64}
    GetProcessDEPPolicy := GetProcAddress(GetModuleHandle(kernel32), 'GetProcessDEPPolicy');
    if Assigned(GetProcessDEPPolicy) then
    begin
      if (GetProcessDEPPolicy(GetCurrentProcess(), Flags, Permanent)) then
      begin
        if (Flags <> 0) then
        begin
          Result := '';
          if (Flags and PROCESS_DEP_ENABLE <> 0) then
          begin
            Result := 'Enabled';
            Flags := Flags and not PROCESS_DEP_ENABLE;
          end;
          if (Flags and PROCESS_DEP_DISABLE_ATL_THUNK_EMULATION <> 0) then
          begin
            if (Result <> '') then
              Result := Result + ',';
            Result := Result + 'ATL thunk emulation disabled';
            Flags := Flags and not PROCESS_DEP_DISABLE_ATL_THUNK_EMULATION;
          end;
          if (Flags <> 0) then
          begin
            if (Result <> '') then
              Result := Result + ',';
            Result := Result + IntToHex(Flags, 8);
          end;
        end else
          Result := 'Disabled';
        if (Permanent) then
          Result := Result + ' (permanent)';
      end else
        Result := '(failed:'+SysErrorMessage(GetLastError)+')';
    end else
      Result := '(Unsupported)';
  {$endif WIN64}
  end;

const
  sSection = 'System';
begin
  ExceptionInfoConsumer.AddExceptionInfo(sSection, 'System locale', IntToHex(GetSystemDefaultLCID));
  ExceptionInfoConsumer.AddExceptionInfo(sSection, 'User locale', IntToHex(GetUserDefaultLCID));
  ExceptionInfoConsumer.AddExceptionInfo(sSection, 'Current locale', IntToHex(GetThreadLocale));
  ExceptionInfoConsumer.AddExceptionInfo(sSection, 'Decimal separator', FormatSettings.DecimalSeparator);
  ExceptionInfoConsumer.AddExceptionInfo(sSection, 'Thousand separator', FormatSettings.ThousandSeparator);
  ExceptionInfoConsumer.AddExceptionInfo(sSection, 'Themes available', BoolToStr(StyleServices.Available));
  ExceptionInfoConsumer.AddExceptionInfo(sSection, 'Themes enabled', BoolToStr(StyleServices.Enabled));
  ExceptionInfoConsumer.AddExceptionInfo(sSection, 'DEP', GetProcessDepState);
end;

procedure TExceptionInfoProviderApplication.GetExceptionInfo(const ExceptIntf: IInterface; const ExceptionInfoConsumer: IExceptionInfoConsumer);
const
  sSection = 'Application';
begin
  if (Application.MainForm <> nil) then
    ExceptionInfoConsumer.AddExceptionInfo(sSection, 'Main form', 'Instantiated')
  else
    ExceptionInfoConsumer.AddExceptionInfo(sSection, 'Main form', 'Not instantiated');

  try
    var Value := False;
    var CurModule := LibModuleList;
    while (CurModule <> nil) do
    begin
      if (CurModule.Instance = HInstance) and (CurModule.ResInstance <> CurModule.Instance) then
      begin
        Value := True;
        break;
      end;
      CurModule := CurModule.Next;
    end;
    ExceptionInfoConsumer.AddExceptionInfo(sSection, 'Localization loaded', BoolToStr(Value));
  except
    on E: Exception do
      ExceptionInfoConsumer.AddExceptionInfo(sSection, 'Localization loaded', 'Exception: '+E.Message);
  end;
end;

procedure TExceptionInfoProviderFolders.GetExceptionInfo(const ExceptIntf: IInterface; const ExceptionInfoConsumer: IExceptionInfoConsumer);
const
  sSection = 'Folders';
begin
  ExceptionInfoConsumer.AddExceptionInfo(sSection, '^Folder', 'Path');
  ExceptionInfoConsumer.AddExceptionInfo(sSection, 'TempPath', TPath.GetTempPath);
  ExceptionInfoConsumer.AddExceptionInfo(sSection, 'HomePath', TPath.GetHomePath);
  ExceptionInfoConsumer.AddExceptionInfo(sSection, 'ApplicationPath', ExtractFilePath(ParamStr(0)));
end;

procedure TExceptionInfoProviderForms.GetExceptionInfo(const ExceptIntf: IInterface; const ExceptionInfoConsumer: IExceptionInfoConsumer);
const
  sSection = 'Forms';
begin
  ExceptionInfoConsumer.AddExceptionInfo(sSection, '^Class', 'Caption');

  try
    for var i := 0 to Screen.CustomFormCount-1 do
    begin
      try

        var Name := Screen.CustomForms[i].ClassName;

        try
          ExceptionInfoConsumer.AddExceptionInfo(sSection, Name, Screen.CustomForms[i].Caption);
        except
          on E: Exception do
            ExceptionInfoConsumer.AddExceptionInfo(sSection, Name, 'Exception: '+E.Message);
        end;
      except
        on E: Exception do
          ExceptionInfoConsumer.AddExceptionInfo(sSection, 'Exception: '+E.Message, '');
      end;
    end;
    if (Screen.CustomFormCount = 0) then
      ExceptionInfoConsumer.AddExceptionInfo(sSection, '(none)', '');
  except
    on E: Exception do
      ExceptionInfoConsumer.AddExceptionInfo(sSection, 'Not available', 'Exception: '+E.Message);
  end;
end;

{ TCustomExceptionInfoProvider }

class destructor TCustomExceptionInfoProvider.Destroy;
begin
  FreeAndNil(FExceptionInfoProviders);
end;

class procedure TCustomExceptionInfoProvider.RegisterExceptionInfoProvider;
begin
  if (FExceptionInfoProviders = nil) then
    FExceptionInfoProviders := TDictionary<TExceptionInfoProviderClass, IExceptionInfoProvider>.Create;

  if (FExceptionInfoProviders.ContainsKey(Self)) then
    Exit;

  var ExceptionInfoProvider: IExceptionInfoProvider := Create;
  FExceptionInfoProviders.Add(Self, ExceptionInfoProvider);

  ExceptionHandler.RegisterExceptionInfoProvider(ExceptionInfoProvider);
end;

class procedure TCustomExceptionInfoProvider.UnregisterExceptionInfoProvider;
begin
  if (FExceptionInfoProviders = nil) then
    Exit;

  var ExceptionInfoProvider: IExceptionInfoProvider;
  if (not FExceptionInfoProviders.TryGetValue(Self, ExceptionInfoProvider)) then
    Exit;

  ExceptionHandler.UnregisterExceptionInfoProvider(ExceptionInfoProvider);
  FExceptionInfoProviders.Remove(Self);
end;

initialization
  TExceptionInfoProviderSystem.RegisterExceptionInfoProvider;
  TExceptionInfoProviderApplication.RegisterExceptionInfoProvider;
  TExceptionInfoProviderForms.RegisterExceptionInfoProvider;
  TExceptionInfoProviderFolders.RegisterExceptionInfoProvider;
finalization
  TExceptionInfoProviderSystem.UnregisterExceptionInfoProvider;
  TExceptionInfoProviderApplication.UnregisterExceptionInfoProvider;
  TExceptionInfoProviderForms.UnregisterExceptionInfoProvider;
  TExceptionInfoProviderFolders.UnregisterExceptionInfoProvider;
end.
