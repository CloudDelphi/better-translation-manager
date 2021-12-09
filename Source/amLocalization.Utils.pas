unit amLocalization.Utils;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  Windows;

type
  LocalizationTools = record
    class function LoadResourceModule(ALanguage: LCID): boolean; static;
  end;

implementation

uses
  UITypes,
  Dialogs,
  Controls,
  SysUtils,
  amLocale,
  amVersionInfo;

class function LocalizationTools.LoadResourceModule(ALanguage: LCID): boolean;
var
  LocaleItem: TLocaleItem;
  Module: HModule;
  ModuleFilename: string;
  ApplicationVersion, ModuleVersion: string;
const
  // Do not localize - localizations has not yet been loaded
  sResourceModuleUnknownLanguage = 'Unknown language ID: %d'+#13#13+
    'The default language will be used instead.';

  sResourceModuleOutOfSync = 'The resource module for the current language (%s) appears to be out of sync with the application.'+#13#13+
    'Application version: %s'+#13+
    'Resource module version: %s'+#13#13+
    'The default language will be used instead.';
begin
  Result := False;

  if (ALanguage = 0) then
    Exit;

  LocaleItem := TLocaleItems.FindLCID(ALanguage);
  if (LocaleItem = nil) then
  begin
    MessageDlg(Format(sResourceModuleUnknownLanguage, [ALanguage]), mtWarning, [mbOK], 0);
    Exit;
  end;

  Module := LoadNewResourceModule(LocaleItem, ModuleFilename);

  if (Module <> 0) and (ModuleFilename <> '') then
  begin
    ApplicationVersion := TVersionInfo.FileVersionString(ParamStr(0));
    // Note: GetModuleFileName (used by GetModuleName) can not be used with modules loaded with LOAD_LIBRARY_AS_DATAFILE
    ModuleVersion := TVersionInfo.FileVersionString(ModuleFilename);

    if (ApplicationVersion <> ModuleVersion) then
    begin
      LoadNewResourceModule(nil, ModuleFilename);
      MessageDlg(Format(sResourceModuleOutOfSync, [LocaleItem.LanguageName, ApplicationVersion, ModuleVersion]), mtWarning, [mbOK], 0);
    end else
      Result := True;
  end else
    // Use default application language if we failed to load a resource module
    LoadNewResourceModule(nil, ModuleFilename);
end;

end.
