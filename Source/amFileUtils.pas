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

// Format template:
// 0: folder
// 1: file name
// 2: file type
// 3: unique id

function SafeReplaceFile(const Filename: string; SaveFileDelegate: TSaveFileDelegate; Backup: boolean = True; const BackupTemplate: string = '%0:s\%1:s%2:s.$%3:.4X'): boolean;

var
  sSafeReplaceFileTempTemplate: string = '%0:s\savefile%3:.4X%2:s';

implementation

uses
  IOUtils,
  SysUtils;

function SafeReplaceFile(const Filename: string; SaveFileDelegate: TSaveFileDelegate; Backup: boolean; const BackupTemplate: string): boolean;

  function ApplyTemplate(const Template: string; ID: integer): string;
  begin
    Result := Format(Template, [TPath.GetDirectoryName(Filename), TPath.GetFileNameWithoutExtension(Filename), TPath.GetExtension(Filename), ID]);
  end;

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
      TempFilename := ApplyTemplate(sSafeReplaceFileTempTemplate, i);
      Inc(i);
    until (i = MaxInt) or (not TFile.Exists(TempFilename));

    if (i = MaxInt) then
      // Give up and hope for the best
      TempFilename := Filename;
  end;

  // Save file
  try

    Result := SaveFileDelegate(TempFilename);

  except
    on E: EAbort do
    begin
      // Try to clean up
      if (TFile.Exists(TempFilename)) then
      begin
        try
          TFile.Delete(TempFilename);
        except
        end;
      end;
      Exit(False);
    end;
  end;

  if (not Result) then
    Exit;

  // Save existing file as backup
  if (TempFilename <> Filename) then
  begin
    i := 0;
    repeat
      BackupFilename := ApplyTemplate(BackupTemplate, i);
      Inc(i);
    until (i = MaxInt) or (not TFile.Exists(BackupFilename));

    if (i = MaxInt) then
      // Just create some random unique name
      BackupFilename := TPath.GetGUIDFileName;

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
