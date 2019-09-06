unit amLocalization.Shell;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

// -----------------------------------------------------------------------------
//
// Windows shell related stuff
//
// -----------------------------------------------------------------------------

type
  TranslationManagerShell = class abstract
  public
    const
      // Shell registration AppIDs and ProgIDs
      sApplicationShellAppID = 'Melander.TranslationManager.Application.1.0.0.0';
      sApplicationShellProgID = 'Melander.TranslationManager.Application.1.0.0.0.ProgID';

    const
      // File type classes
      sProjectFileClass = 'translation.project';

    const
      // File types
      sProjectFileType = '.xlat';
  protected
    class procedure RegisterExt(const Extension, IntName: string; const Icon: string; Description, FileName: string; KeepExisting: boolean = False); static;
  public
    class procedure RegisterApplication; static;
    class procedure RegisterFileTypeHandler; static;
    class procedure RegisterFileTypes; static;

    class procedure RegisterShellIntegration; static;
  end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  Windows,
  IOUtils,
  SysUtils,
  Registry,
  ShellAPI,
  SHlObj,
  Perevoznyk.WindowsFileRegistrationHelper;


resourcestring
  sProjectFileDescription = 'Translation project';

// -----------------------------------------------------------------------------

class procedure TranslationManagerShell.RegisterShellIntegration;
begin
  RegisterApplication;
  RegisterFileTypes;

{$ifdef DEBUG}
  RegisterFileTypeHandler;
{$endif DEBUG}

  // Notify the shell that associations has changed
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

// -----------------------------------------------------------------------------

class procedure TranslationManagerShell.RegisterFileTypeHandler;
var
  FileRegistrationHelper : TFileRegistrationHelper;
begin
  FileRegistrationHelper := TFileRegistrationHelper.Create(sApplicationShellProgID, ParamStr(0), sProjectFileDescription, sApplicationShellAppID, sProjectFileType);
  try
    FileRegistrationHelper.RegisterToHandleFileType(True);
  finally
    FileRegistrationHelper.Free;
  end;
end;

// -----------------------------------------------------------------------------

class procedure TranslationManagerShell.RegisterApplication;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create(KEY_ALL_ACCESS);
  try
    // Application Registration
    // http://msdn.microsoft.com/en-us/library/windows/desktop/ee872121%28v=vs.85%29.aspx
    Registry.RootKey := HKEY_CURRENT_USER;
    if (Registry.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\'+TPath.GetFileName(ParamStr(0)), True)) then
    begin
      (*
      ** (Default)
      ** Is the fully qualified path to the application. The application name provided in the (Default) entry can be
      ** stated with or without its .exe extension. If necessary, the ShellExecuteEx function adds the extension when
      ** searching App Paths subkey. The entry is of the REG_SZ type.
      *)
      Registry.WriteString('', ParamStr(0));
      (*
      ** UseUrl
      ** Indicates that your application can accept a URL (instead of a file name) on the command line. Applications
      ** that can open documents directly from the internet, like web browsers and media players, should set this entry.
      ** When the ShellExecuteEx function starts an application and the UseUrl=1 value is not set, ShellExecuteEx
      ** downloads the document to a local file and invokes the handler on the local copy.
      ** For example, if the application has this entry set and a user right-clicks on a file stored on a web server,
      ** the Open verb will be made available. If not, the user will have to download the file and open the local copy.
      ** The UseUrl entry is of REG_DWORD type, and the value is 0x1.
      ** In Windows Vista and earlier, this entry indicated that the URL should be passed to the application along
      ** with a local file name, when called via ShellExecuteEx. In Windows 7, it indicates that the application can
      ** understand any http or https url that is passed to it, without having to supply the cache file name as well.
      ** The registry key SupportedProtocols contains multiple registry values to indicate which URL schemes are supported.
      *)
      // Registry.WriteInteger('UseUrl', 1);

      Registry.CloseKey;
    end;

  finally
    Registry.Free;
  end;
end;

// -----------------------------------------------------------------------------

class procedure TranslationManagerShell.RegisterExt(const Extension, IntName: string; const Icon: string; Description, FileName: string; KeepExisting: boolean);
var
  Registry: TRegistry;
  s: string;
begin
  Registry := TRegistry.Create(KEY_ALL_ACCESS);
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if (not Registry.OpenKey('Software\Classes\' + Extension, True)) then
      RaiseLastOSError;

    if (KeepExisting) then
    begin
      s := Registry.ReadString('');
      if (s <> IntName) then
        exit;
    end;

    Registry.WriteString('', IntName);
    Registry.CloseKey;

    if (not Registry.OpenKey('Software\Classes\' + IntName, True)) then
      RaiseLastOSError;

    Registry.WriteString('', Description);
    if (Icon <> '') then
    begin
      Registry.OpenKey('DefaultIcon', True);
      Registry.WriteString('', Filename + ',' + Icon);
      Registry.CloseKey;
    end;
    Registry.OpenKey('Software\Classes\' + IntName, False);
    Registry.OpenKey('shell', True);
    Registry.OpenKey('open', True);
    Registry.OpenKey('command', True);
    Registry.WriteString('', FileName + ' "%1"');
    Registry.CloseKey;

    // Remove association made by windows (open with...) as it will dominate the other option
    if (not KeepExisting) then
    begin
      Registry.RootKey := HKEY_CURRENT_USER;
      if Registry.KeyExists('Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\' + Extension) then
      begin
        if (not Registry.OpenKey('Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\' + Extension, False)) then
          RaiseLastOSError;
        Registry.DeleteValue('Application');
        Registry.DeleteKey('UserChoice');
        Registry.CloseKey;
      end;
    end;

  finally
    Registry.Free;
  end;
end;

// -----------------------------------------------------------------------------

class procedure TranslationManagerShell.RegisterFileTypes;
begin
  RegisterExt(sProjectFileType, sProjectFileClass, '0', sProjectFileDescription, ParamStr(0));
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

end.
