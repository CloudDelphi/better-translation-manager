unit amLocalization.Portable;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface


implementation

uses
  UITypes,
  SysUtils,
  IOUtils,
  Dialogs,
  Controls,
  Classes,
  amFileUtils,
  amLocalization.Settings,
  amLocalization.Environment;


// -----------------------------------------------------------------------------

procedure OnSettingsCreatingHandler(Settings: TTranslationManagerSettings);
var
  Filename: string;
  Stream: TStream;
  Res: Word;
begin
  Settings.System.Portable := True;

  // Redirect the App Data to the Install folder
  EnvironmentVars['DATA'] := EnvironmentVars['INSTALL'];

  // Restore registry branch from external file
  Filename := TPath.ChangeExtension(ParamStr(0), '.portable');
  if (not TFile.Exists(Filename)) then
    Exit;

  try

    Stream := TFileStream.Create(Filename, fmOpenRead);
    try
      Settings.LoadRegistryFromStream(Stream);
    finally
      Stream.Free;
    end;

  except
    on E: Exception do
    begin
      Res := TaskMessageDlg('Portable Configuration', Format('Unable to restore the portable configuration.'#13+
        'Would you like to reset the configuration?'#13#13+
        'Error: %s', [Settings.Registry.LastErrorMsg]), mtWarning, [mbYes, mbNo, mbCancel], 0, mbNo);

      if (Res = mrCancel) then
        Halt(0);

      // Delete current settings to use default
      if (Res = mrYes) then
        Settings.Registry.EraseSection('');
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure OnSettingsDestroyingHandler(Settings: TTranslationManagerSettings);
var
  Filename: string;
begin
  if (not Settings.System.Portable) then
    Exit;

  // Save registry branch to external file
  Filename := TPath.ChangeExtension(ParamStr(0), '.portable');

  try
    SafeReplaceFile(Filename,
      function(const Filename: string): boolean
      var
        Stream: TStream;
      begin
        Stream := TFileStream.Create(Filename, fmCreate);
        try
          Settings.SaveRegistryToStream(Stream);
        finally
          Stream.Free;
        end;
        Result := True;
      end, False);

    // Enable this once we're confident that everything else works...
    (*
    if (Settings.System.PortablePurge) then
      Settings.Registry.EraseSection('');
    *)

  except
    on E: Exception do
      TaskMessageDlg('Portable Configuration', Format('Unable to save the portable configuration.'#13#13+
        'Error: %s', [E.Message]), mtWarning, [mbOK], 0);
  end;
end;

// -----------------------------------------------------------------------------

initialization
  if (FindCmdLineSwitch('portable', True)) or (TFile.Exists(TTranslationManagerSettings.FolderInstall + 'portable')) then
  begin
    TTranslationManagerSettings.OnSettingsCreating := OnSettingsCreatingHandler;
    TTranslationManagerSettings.OnSettingsDestroying := OnSettingsDestroyingHandler;
  end;
end.
