unit amLocalization.System.Restart;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  SyncObjs;

// -----------------------------------------------------------------------------
//
//              Restart semaphore
//
// -----------------------------------------------------------------------------
type
  RestartSemaphore = class abstract
  private
    class var
      FSemaphoreName: string;
      FSemaphore: TMutex;
  private
    class constructor Create;
    class destructor Destroy;
  public
    class function Acquire(Timeout: integer = 60000): boolean;
    class procedure Release;

    class property SemaphoreName: string read FSemaphoreName write FSemaphoreName;
  end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  Windows,
  SysUtils;


// -----------------------------------------------------------------------------
//
//              Restart semaphore
//
// -----------------------------------------------------------------------------
class constructor RestartSemaphore.Create;
begin
  FSemaphoreName := 'Local\amTranslationManager.Restart';
end;

class destructor RestartSemaphore.Destroy;
begin
  Release;
end;

// -----------------------------------------------------------------------------

class procedure RestartSemaphore.Release;
begin
  if (FSemaphore = nil) then
    exit;
  try
    try
      FSemaphore.Release;
    finally
      FreeAndNil(FSemaphore);
    end;
  except
    // Ignore. It's more important that we terminate properly.
    // Error is probably "Attempt to release mutex not owned by caller" because we failed
    // to acquire the mutex (can happen if other instance hangs during shutdown).
  end;
end;

class function RestartSemaphore.Acquire(Timeout: integer = 60000): boolean;
begin
  Release;

  FSemaphore := TMutex.Create(nil, True, FSemaphoreName);

  if (GetLastError = ERROR_ALREADY_EXISTS) then
    Result := (FSemaphore.WaitFor(Timeout) <> wrTimeout)
  else
    Result := True;
end;

// -----------------------------------------------------------------------------

end.
