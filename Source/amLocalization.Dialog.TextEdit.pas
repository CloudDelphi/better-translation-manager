﻿unit amLocalization.Dialog.TextEdit;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ToolWin, ActnMan, ActnCtrls, ActnList,
  StdActns, PlatformDefaultStyleActnCtrls, ImgList,
  Menus, ActnPopup, System.Actions, System.ImageList,

  cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore, cxControls,
  cxContainer, cxEdit, cxTextEdit, cxMemo, cxRichEdit, cxButtons, cxSplitter, cxImageList,
  dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxLabel, dxLayoutContainer, cxClasses, dxLayoutControl,

  amLanguageInfo,
  amLocalization.Dialog;

type
  TEditDelete = class(StdActns.TEditDelete)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

//------------------------------------------------------------------------------
//
//      TFormTextEditor
//
//------------------------------------------------------------------------------
type
  TFormTextEditor = class(TFormDialog)
    ImageListNormal: TcxImageList;
    ActionManager1: TActionManager;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    EditDelete1: TEditDelete;
    FileOpen1: TFileOpen;
    FileSaveAs1: TFileSaveAs;
    SearchFind1: TSearchFind;
    SearchFindNext1: TSearchFindNext;
    SearchReplace1: TSearchReplace;
    SearchFindFirst1: TSearchFindFirst;
    ToolActionBar2: TActionToolBar;
    PopupActionBar1: TPopupActionBar;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    N1: TMenuItem;
    LayoutItemEditors: TdxLayoutItem;
    PanelEditors: TPanel;
    SplitterEditors: TcxSplitter;
    PanelSource: TPanel;
    PanelText: TPanel;
    EditSourceText: TcxRichEdit;
    EditText: TcxRichEdit;
    LabelSourceName: TcxLabel;
    LabelTargetName: TcxLabel;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FileOpen1Accept(Sender: TObject);
    procedure FileSaveAs1Accept(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FTargetLanguage: TLanguageItem;
    FSourceLanguage: TLanguageItem;
    function GetText: string;
    procedure SetText(const Value: string);
    function GetSourceText: string;
    procedure SetSourceText(const Value: string);
    procedure SetTargetLanguage(Value: TLanguageItem);
    procedure SetSourceLanguage(const Value: TLanguageItem);
  public
    function Execute(ShowSourceValue: boolean = True): boolean;
    property Text: string read GetText write SetText;
    property SourceText: string read GetSourceText write SetSourceText;
    property SourceLanguage: TLanguageItem read FSourceLanguage write SetSourceLanguage;
    property TargetLanguage: TLanguageItem read FTargetLanguage write SetTargetLanguage;
  end;

implementation

{$R *.dfm}

uses
  Consts,
  amLocalization.Common,
  amLocalization.Data.Main,
  amLocalization.Settings;

//------------------------------------------------------------------------------
//
//      TFormTextEditor
//
//------------------------------------------------------------------------------

function TFormTextEditor.Execute(ShowSourceValue: boolean): boolean;
begin
  PanelSource.Visible := ShowSourceValue;
  SplitterEditors.Visible := ShowSourceValue;

  if (TargetLanguage.IsRightToLeft <> IsRightToLeft) and (TranslationManagerSettings.Editor.EditBiDiMode) then
  begin
    if (TargetLanguage.IsRightToLeft) then
      EditText.BiDiMode := bdRightToLeft
    else
      EditText.BiDiMode := bdLeftToRight;
  end;

  Result := (ShowModal = mrOK);
end;

//------------------------------------------------------------------------------

procedure TFormTextEditor.FileOpen1Accept(Sender: TObject);
begin
  EditText.Lines.LoadFromFile(FileOpen1.Dialog.FileName);
end;

//------------------------------------------------------------------------------

procedure TFormTextEditor.FileSaveAs1Accept(Sender: TObject);
begin
  EditText.Lines.LoadFromFile(FileSaveAs1.Dialog.FileName);
end;

//------------------------------------------------------------------------------

type
  TcxSplitterCracker = class(TcxSplitter);

procedure TFormTextEditor.FormCreate(Sender: TObject);
begin
  FileOpen1.Dialog.Filter := sFileTextFilter + '|' + SDefaultFilter;
  FileOpen1.Dialog.FilterIndex := 0;
  FileSaveAs1.Dialog.Filter := sFileTextFilter + '|' + SDefaultFilter;
  FileSaveAs1.Dialog.FilterIndex := 0;
  SplitterEditors.CloseSplitter;

  // Only way to avoid skinning splitter
  TcxSplitterCracker(SplitterEditors).LookAndFeel.SkinName := '';
end;

//------------------------------------------------------------------------------

procedure TFormTextEditor.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
    Close;
end;

//------------------------------------------------------------------------------

function TFormTextEditor.GetSourceText: string;
begin
  Result := EditSourceText.Text;
end;

procedure TFormTextEditor.SetSourceText(const Value: string);
begin
  EditSourceText.Text := Value;
end;

//------------------------------------------------------------------------------

procedure TFormTextEditor.SetSourceLanguage(const Value: TLanguageItem);
begin
  FSourceLanguage := Value;

  if (FSourceLanguage <> nil) then
    LabelSourceName.Caption := FSourceLanguage.LanguageName;

  LabelSourceName.Visible := (FSourceLanguage <> nil) and (FTargetLanguage <> nil);
  LabelTargetName.Visible := LabelSourceName.Visible;
end;

procedure TFormTextEditor.SetTargetLanguage(Value: TLanguageItem);
begin
  FTargetLanguage := Value;

  if (FTargetLanguage <> nil) then
    LabelTargetName.Caption := FTargetLanguage.LanguageName;

  LabelTargetName.Visible := (FSourceLanguage <> nil) and (FTargetLanguage <> nil);
  LabelSourceName.Visible := LabelTargetName.Visible;
end;

//------------------------------------------------------------------------------

function TFormTextEditor.GetText: string;
begin
  Result := EditText.Text;
end;

procedure TFormTextEditor.SetText(const Value: string);
begin
  EditText.Text := Value;
end;

{ TEditDelete }

procedure TEditDelete.ExecuteTarget(Target: TObject);
begin
  if (GetControl(Target).SelLength > 0) then
    inherited ExecuteTarget(Target)
  else
    SendMessage(GetControl(Target).Handle, WM_KEYDOWN, VK_DELETE, 0);
end;

procedure TEditDelete.UpdateTarget(Target: TObject);
begin
  Enabled := (not GetControl(Target).ReadOnly);
end;

end.
