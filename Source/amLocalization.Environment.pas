unit amLocalization.Environment;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  amEnvironmentVars;

var
  EnvironmentVars: TEnvironment;

implementation

uses
  SysUtils,
  IOUtils,
  amLocalization.Settings;

const
  sApplicationFolder = 'TranslationManager\';

initialization
  EnvironmentVars.SetValue('INSTALL', TranslationManagerSettings.FolderInstall, egStatic);
  EnvironmentVars.SetValue('DATA', IncludeTrailingPathDelimiter(TPath.GetCachePath) + sApplicationFolder, egStatic);
  EnvironmentVars.SetValue('DOCUMENTS', IncludeTrailingPathDelimiter(TPath.GetDocumentsPath) + sApplicationFolder, egCustomizable);
end.
