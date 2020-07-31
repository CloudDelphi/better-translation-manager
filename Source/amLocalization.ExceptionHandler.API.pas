unit amLocalization.ExceptionHandler.API;

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
//              IExceptionHandler
//
// -----------------------------------------------------------------------------
type
  IExceptionInfoProvider = interface;

  IExceptionHandler = interface
    procedure ExceptionHandler(const ExceptIntf: IUnknown; var Handled: boolean);
    procedure RegisterExceptionInfoProvider(const ExceptionInfoProvider: IExceptionInfoProvider);
    procedure UnregisterExceptionInfoProvider(const ExceptionInfoProvider: IExceptionInfoProvider);
  end;

  IExceptionInfoConsumer = interface
    ['{183EEFC0-AF2B-45CE-AD2B-AC8E1C5489E3}']
    procedure AddExceptionInfo(const Section, Name, Value: string);
  end;

  IExceptionInfoProvider = interface
    ['{9A364488-CE5B-4655-B6E8-D17C8B5E7805}']
    procedure GetExceptionInfo(const ExceptIntf: IUnknown; const ExceptionInfoConsumer: IExceptionInfoConsumer);
  end;


// -----------------------------------------------------------------------------
//
//              IExceptionHandler factory
//
// -----------------------------------------------------------------------------
type
  TExceptionHandlerFactory = function: IExceptionHandler;

procedure RegisterExceptionHandlerFactory(ExceptionHandlerFactory: TExceptionHandlerFactory);


// -----------------------------------------------------------------------------
//
//              ExceptionHandler
//
// -----------------------------------------------------------------------------
function ExceptionHandler: IExceptionHandler;
procedure InitializeExceptionHandler;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  System.Classes;

type
  TDummyExceptionHandler = class(TInterfacedObject, IExceptionHandler)
  private
    // IExceptionHandler
    procedure ExceptionHandler(const ExceptIntf: IUnknown; var Handled: boolean);
    procedure RegisterExceptionInfoProvider(const ExceptionInfoProvider: IExceptionInfoProvider);
    procedure UnregisterExceptionInfoProvider(const ExceptionInfoProvider: IExceptionInfoProvider);
  end;

procedure TDummyExceptionHandler.ExceptionHandler(const ExceptIntf: IInterface; var Handled: boolean);
begin
  Handled := False;
end;

procedure TDummyExceptionHandler.RegisterExceptionInfoProvider(const ExceptionInfoProvider: IExceptionInfoProvider);
begin
end;

procedure TDummyExceptionHandler.UnregisterExceptionInfoProvider(const ExceptionInfoProvider: IExceptionInfoProvider);
begin
end;

function DummyExceptionHandlerFactory: IExceptionHandler;
begin
  Result := TDummyExceptionHandler.Create;
end;

var
  FExceptionHandlerFactory: TExceptionHandlerFactory = DummyExceptionHandlerFactory;
  FExceptionHandler: IExceptionHandler;

procedure RegisterExceptionHandlerFactory(ExceptionHandlerFactory: TExceptionHandlerFactory);
begin
  FExceptionHandlerFactory := ExceptionHandlerFactory;
end;

procedure InitializeExceptionHandler;
begin
  FExceptionHandler := FExceptionHandlerFactory;
end;

function ExceptionHandler: IExceptionHandler;
begin
  if (FExceptionHandler = nil) then
    FExceptionHandler := FExceptionHandlerFactory;

  Result := FExceptionHandler;
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

initialization
finalization
  FExceptionHandler := nil;
end.
