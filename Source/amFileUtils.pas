unit amFileUtils;

(*
 * Copyright © 2008 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface


type
  TSaveFileDelegate = reference to function(const Filename: string): boolean;

function SafeReplaceFile(const Filename: string; SaveFileDelegate: TSaveFileDelegate; Backup: boolean = True; const BackupTemplate: string = '%s.$%.4X'): boolean;


implementation

uses
  IOUtils,
  SysUtils;

function SafeReplaceFile(const Filename: string; SaveFileDelegate: TSaveFileDelegate; Backup: boolean; const BackupTemplate: string): boolean;
var
  i: integer;
  TempFilename, BackupFilename: string;
begin
  TempFilename := Filename;

  // Save to temporary file if destination file already exist
  if (TFile.Exists(Filename)) then
  begin
    i := 0;
    repeat
      TempFilename := Format('%s\savefile%.4X%s', [TPath.GetDirectoryName(Filename), i, '.tmx']);
      Inc(i);
    until (not TFile.Exists(TempFilename));
  end;

  // Save file
  Result := SaveFileDelegate(TempFilename);

  if (not Result) then
    Exit;

  // Save existing file as backup
  if (TempFilename <> Filename) then
  begin
    i := 0;
    repeat
      BackupFilename := Format(BackupTemplate, [Filename, i]);
      Inc(i);
    until (not TFile.Exists(BackupFilename));

    TFile.Move(Filename, BackupFilename);

    // Rename temporary file to final file
    if (TFile.Exists(TempFilename)) then
    begin
      try

        TFile.Move(TempFilename, Filename);

      except
        // Restore backup
        try
          TFile.Move(BackupFilename, Filename);
        except
          // Ignore
        end;
        raise;
      end;
    end;

    // All done
    if (not Backup) then
      TFile.Delete(BackupFilename);
  end;
end;

end.
