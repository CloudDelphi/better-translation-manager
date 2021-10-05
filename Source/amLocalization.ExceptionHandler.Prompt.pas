unit amLocalization.ExceptionHandler.Prompt;

(*
 * Copyright © 2021 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

// -----------------------------------------------------------------------------
//
//              PromptPropagateError
//
// -----------------------------------------------------------------------------
// Display prompt with Abort/Ignore choice. Returns True if user selected Abort.
// -----------------------------------------------------------------------------
function PromptPropagateError(const Title, Msg: string): boolean; overload;
function PromptPropagateError(const Msg: string): boolean; overload;
function PromptPropagateError(const Title, Msg: string; const Args: array of const): boolean; overload;
function PromptPropagateError(const Msg: string; const Args: array of const): boolean; overload;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  SysUtils,
  UITypes,
  Dialogs,
  Controls,
  Forms;

resourcestring
  sPropagateErrorAbortToReport = 'Select OK to continue, Abort to handle problem as a bug';

function PromptPropagateError(const Msg: string): boolean; overload;
begin
  if (not Application.Terminated) then
  begin
    var Res := MessageDlg(Msg+#13#13+sPropagateErrorAbortToReport, mtWarning, [mbOK, mbAbort], 0, mbOK);
    Result := (Res = mrAbort);
  end else
    // Application is terminating so we cannot display a modal dialog
    Result := True;
end;

function PromptPropagateError(const Title, Msg: string): boolean; overload;
begin
  if (not Application.Terminated) then
  begin
    var Res := TaskMessageDlg(Title, Msg+#13#13+sPropagateErrorAbortToReport, mtWarning, [mbOK, mbAbort], 0, mbOK);
    Result := (Res = mrAbort);
  end else
    // Application is terminating so we cannot display a modal dialog
    Result := True;
end;

function PromptPropagateError(const Msg: string; const Args: array of const): boolean; overload;
begin
  Result := PromptPropagateError(Format(Msg, Args));
end;

function PromptPropagateError(const Title, Msg: string; const Args: array of const): boolean; overload;
begin
  Result := PromptPropagateError(Title, Format(Msg, Args));
end;

end.
