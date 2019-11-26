unit amFileUtils;

(*
 * Copyright © 2008 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows;

//------------------------------------------------------------------------------
//
//      SafeReplaceFile
//
//------------------------------------------------------------------------------
// Safely replace one file with another.
// - Optionally saves backup of original file.
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
//
//      CheckAccessToFile
//
//------------------------------------------------------------------------------
// CheckAccessToFile is used to determine if the user has a specific set of
// access rights to a file.
// If CheckAccessToFile fails the default value is returned.
//------------------------------------------------------------------------------
// Example:
//   if (not CheckAccessToFile(FILE_LIST_DIRECTORY or FILE_ADD_FILE, Path)) then
//     ...
//------------------------------------------------------------------------------
function CheckAccessToFile(DesiredAccess: DWORD; const Filename: string; Default: boolean = True): Boolean;


//------------------------------------------------------------------------------
//
//      CheckDirectory
//
//------------------------------------------------------------------------------
// Verify that directory exist and create if it doesn't.
// If that fails prompt user to revert to default value and retry.
// Optionally also verify that directory is listable and writable.
//------------------------------------------------------------------------------
type
  TCheckDirectoryPredicate = reference to function(const Filename: string): string;

function CheckDirectory(const FolderName, FolderDefault: string; var Folder: string; TestWritable: boolean = False; Predicate: TCheckDirectoryPredicate = nil): boolean;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  IOUtils,
  WinApi.ShLwApi, // must be before SysUtils or StrToInt will resolve to wrong unit
  SysUtils,
  Dialogs,
  Controls,
  UITypes;

//------------------------------------------------------------------------------
//
//      SafeReplaceFile
//
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
//
//      CheckAccessToFile
//
//------------------------------------------------------------------------------
function CheckAccessToFile(DesiredAccess: DWORD; const Filename: string; Default: boolean): Boolean;
const
  FILE_ALL_ACCESS        = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $1FF;
  FILE_GENERIC_READ      = STANDARD_RIGHTS_READ or FILE_READ_DATA or FILE_READ_ATTRIBUTES or FILE_READ_EA or SYNCHRONIZE;
  FILE_GENERIC_WRITE     = STANDARD_RIGHTS_WRITE or FILE_WRITE_DATA or FILE_WRITE_ATTRIBUTES or FILE_WRITE_EA or FILE_APPEND_DATA or SYNCHRONIZE;
  FILE_GENERIC_EXECUTE   = STANDARD_RIGHTS_EXECUTE or FILE_READ_ATTRIBUTES or FILE_EXECUTE or SYNCHRONIZE;
const
  GenericFileMapping: TGenericMapping = (
    GenericRead: FILE_GENERIC_READ;
    GenericWrite: FILE_GENERIC_WRITE;
    GenericExecute: FILE_GENERIC_EXECUTE;
    GenericAll: FILE_ALL_ACCESS
    );
var
  LengthNeeded: DWORD;
  SecurityDescriptor: PSecurityDescriptor;
  ClientToken: THandle;
  AccessMask: DWORD;
  PrivilegeSet: PPrivilegeSet;
  PrivilegeSetLength: DWORD;
  GrantedAccess: DWORD;
  AccessStatus: BOOL;
  SidOwner, SidGroup: PSID;
  SidDefaulted: LongBool;
  SizeName, SizeDomain: DWORD;
  SidUse: DWORD;
  // SidName, SidDomain: string;
begin
  Result := Default;

  (*
  ** AccessCheck() does not seem to work reliably against a network file.
  **
  ** My own experience is that it works on network files within the current domain, but fails in other cases
  ** (e.g. workgroup with file on NAS, file share via vpn). By fail I mean the function returns without an error, but
  ** the return value is incorrect.
  **
  ** I have found that in the cases where it fails I have not been able to resolve the file owner and group SID to a
  ** name, so that is the test I'm using.
  **
  ** Another approach could be to just abort (i.e. use the default) if the file is on a network (see diabled call to
  ** PathIsNetworkPath() below).
  **
  ** See also:
  ** http://microsoft.public.platformsdk.security.narkive.com/Yq0r14VI/geteffectiverightsfromacl-returns-error-invalid-acl
  **
  ** 04032015, am, Reverting to use PathIsNetworkPath as TopDanmark has experienced that files on a network drive is
  ** incorrectly marked read-only.
  *)

  //(*
  if (PathIsNetworkPath(PChar(Filename))) then
    exit;
  //*)

  LengthNeeded := 0;
  GetFileSecurity(PChar(Filename), OWNER_SECURITY_INFORMATION or GROUP_SECURITY_INFORMATION or DACL_SECURITY_INFORMATION, nil, 0, LengthNeeded);
  if (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
    Exit;

  SecurityDescriptor := PSecurityDescriptor(AllocMem(LengthNeeded));
  if (SecurityDescriptor = nil) then
    Exit;
  try
    if (not GetFileSecurity(PChar(Filename), OWNER_SECURITY_INFORMATION or GROUP_SECURITY_INFORMATION or DACL_SECURITY_INFORMATION, SecurityDescriptor, LengthNeeded, LengthNeeded)) then
      Exit;

    (*
    ** Attempt to resolve the Owner and Group SIDs to a name.
    ** If this fails we assume that AccessCheck() will return an incorrect value.
    *)
    if (not GetSecurityDescriptorOwner(SecurityDescriptor, SidOwner, SidDefaulted)) then
      exit;
    SizeName := 0;
    SizeDomain := 0;
    LookupAccountSid(nil, SidOwner, nil, SizeName, nil, SizeDomain, SidUse);
    if (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
      Exit;
    (* No need for the actual values:
    if (GetLastError = ERROR_NONE_MAPPED) then
      exit;
    SetLength(SidName, SizeName-1); // Size includes zero termination, string length does not
    SetLength(SidDomain, SizeDomain-1);
    if (not LookupAccountSid(nil, SidOwner, PChar(SidName), SizeName, PChar(SidDomain), SizeDomain, SidUse)) then
      exit;
    *)

    if (not GetSecurityDescriptorGroup(SecurityDescriptor, SidGroup, SidDefaulted)) then
      exit;
    SizeName := 0;
    SizeDomain := 0;
    LookupAccountSid(nil, SidGroup, nil, SizeName, nil, SizeDomain, SidUse);
    if (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
      Exit;
    (*
    if (GetLastError = ERROR_NONE_MAPPED) then
      exit;
    SetLength(SidName, SizeName-1);
    SetLength(SidDomain, SizeDomain-1);
    if (not LookupAccountSid(nil, SidGroup, PChar(SidName), SizeName, PChar(SidDomain), SizeDomain, SidUse)) then
      exit;
    *)

    if (not ImpersonateSelf(SecurityImpersonation)) then
      Exit;
    try
      if (not OpenThreadToken(GetCurrentThread, TOKEN_QUERY or TOKEN_IMPERSONATE or TOKEN_DUPLICATE or STANDARD_RIGHTS_READ, False, ClientToken)) then
        Exit;
      try
        AccessMask := DesiredAccess;
        MapGenericMask(AccessMask, GenericFileMapping);

        PrivilegeSetLength := 0;
        PrivilegeSet := nil;
        AccessCheck(SecurityDescriptor, ClientToken, AccessMask, GenericFileMapping, PrivilegeSet^, PrivilegeSetLength, GrantedAccess, AccessStatus);
        if (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
          Exit;
        PrivilegeSet := AllocMem(PrivilegeSetLength);
        try
          if (AccessCheck(SecurityDescriptor, ClientToken, AccessMask, GenericFileMapping, PrivilegeSet^, PrivilegeSetLength, GrantedAccess, AccessStatus)) then
            Result := AccessStatus;
        finally
          FreeMem(PrivilegeSet);
        end;
      finally
        CloseHandle(ClientToken);
      end;
    finally
      RevertToSelf;
    end;
  finally
    FreeMem(SecurityDescriptor);
  end;
end;


//------------------------------------------------------------------------------
//
//      CheckDirectory
//
//------------------------------------------------------------------------------
function CheckDirectory(const FolderName, FolderDefault: string; var Folder: string; TestWritable: boolean; Predicate: TCheckDirectoryPredicate): boolean;
var
  Path: string;
  Res: Word;
  s: string;
  Buttons: TMsgDlgButtons;
  DefaultButton: TMsgDlgBtn;
  FolderRestored: boolean;
  Error: string;
resourcestring
  sCreateFolderError = 'Unable to create the %s folder:'#13'%s'#13#13'Error: %s';
  sAccessFolderError = 'Unable to write to the %s folder:'#13'%s'#13#13'Error: %s';
  sAccessFolderReadOnly = 'Folder is read-only';
  sAccessFolderDenied = 'Access Denied';
  sFolderRevert = #13#13'Do you want to revert to the default location?';
begin
  Result := True;

  Path := Folder;
  if (Assigned(Predicate)) then
    Path := Predicate(Path);

  FolderRestored := False;

  while (True) do
  begin
    // Test if folder exist
    while (not TDirectory.Exists(Path)) do
    begin
      try

        // Try to create folder
        // Do NOT use TDirectory.CreateDirectory() as that fails silently.
        if (not ForceDirectories(Path)) then
          // We allow system folders to not exist since we don't need to write to them.
          if (TestWritable) then
            RaiseLastOSError;

        break;

      except
        on E: Exception do
        begin
          s := Format(sCreateFolderError, [FolderName, Path, E.Message]);

          if (not FolderRestored) and (not AnsiSameText(Folder, FolderDefault)) then
          begin
            s := s + sFolderRevert;
            Buttons := [mbYes, mbNo, mbAbort];
            DefaultButton := mbYes;
          end else
          begin
            Buttons := [mbAbort, mbIgnore];
            DefaultButton := mbAbort;
          end;


          Res := MessageDlg(s, mtWarning, Buttons, 0, DefaultButton);

          if (Res = mrYes) then
          begin
            FolderRestored := True;
            Path := FolderDefault;
            if (Assigned(Predicate)) then
              Path := Predicate(Path);
            continue; // Retry
          end else
          if (Res = mrIgnore) or (Res = mrNo) then
            Exit(True) // Ignore
          else
            Exit(False); // Fail
        end;
      end;
    end;

    // Test if folder is writable
    if (TestWritable) then
    begin
      try
        if (not CheckAccessToFile(FILE_LIST_DIRECTORY or FILE_ADD_FILE, Path)) then
          Error := sAccessFolderDenied
        else
        if (TFileAttribute.faReadOnly in TDirectory.GetAttributes(Path)) then
          Error := sAccessFolderReadOnly
        else
          break; // Success
      except
        // We might not be able to read the file attributes if the folder access rights doesn't allow this.
        // E.g.: Even though "C:\Users\anders\Application Data\myfile.dat" exists and is writable the "Application Data"
        // folder can not be read.
        on E: EDirectoryNotFoundException do
          Error := sAccessFolderDenied;

        on E: Exception do
          Error := E.Message;
      end;

      s := Format(sAccessFolderError, [FolderName, Path, Error]);

      if (not FolderRestored) and (not AnsiSameText(Folder, FolderDefault)) then
      begin
        s := s + sFolderRevert;
        Buttons := [mbYes, mbNo, mbAbort];
        DefaultButton := mbYes;
      end else
      begin
        Buttons := [mbAbort, mbIgnore];
        DefaultButton := mbAbort;
      end;

      Res := MessageDlg(s, mtWarning, Buttons, 0, DefaultButton);

      if (Res = mrYes) then
      begin
        FolderRestored := True;
        Path := FolderDefault;
        if (Assigned(Predicate)) then
          Path := Predicate(Path);
        continue; // Retry
      end else
      if (Res = mrIgnore) or (Res = mrNo) then
        Exit(True) // Ignore
      else
        Exit(False); // Fail
    end else
      break;
  end;

  if (FolderRestored) then
    Folder := FolderDefault;
end;

end.
