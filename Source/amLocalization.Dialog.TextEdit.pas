unit amLocalization.Dialog.TextEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ToolWin, ActnMan, ActnCtrls, ActnList,
  StdActns, PlatformDefaultStyleActnCtrls, ImgList,
  Menus, ActnPopup, System.Actions, System.ImageList, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore, cxControls,
  cxContainer, cxEdit, cxTextEdit, cxMemo, cxRichEdit, cxButtons, cxSplitter, cxImageList;

//------------------------------------------------------------------------------
//
//      TFormTextEditor
//
//------------------------------------------------------------------------------
type
  TFormTextEditor = class(TForm)
    Panel1: TPanel;
    EditText: TcxRichEdit;
    ButtonOK: TcxButton;
    ButtonCancel: TcxButton;
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
    ActionList1: TActionList;
    ToolActionBar2: TActionToolBar;
    PopupActionBar1: TPopupActionBar;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    N1: TMenuItem;
    EditSourceText: TcxRichEdit;
    Splitter: TcxSplitter;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FileOpen1Accept(Sender: TObject);
    procedure FileSaveAs1Accept(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetText: string;
    procedure SetText(const Value: string);
    function GetSourceText: string;
    procedure SetSourceText(const Value: string);
  public
    function Execute: boolean;
    property Text: string read GetText write SetText;
    property SourceText: string read GetSourceText write SetSourceText;
  end;

implementation

{$R *.dfm}

uses
  Consts;

//------------------------------------------------------------------------------
//
//      TFormTextEditor
//
//------------------------------------------------------------------------------

function TFormTextEditor.Execute: boolean;
begin
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

procedure TFormTextEditor.FormCreate(Sender: TObject);
resourcestring
  sFileTextFilter = 'Text files (*.txt)|*.txt';
begin
  FileOpen1.Dialog.Filter := sFileTextFilter + '|' + SDefaultFilter;
  FileOpen1.Dialog.FilterIndex := 0;
  FileSaveAs1.Dialog.Filter := sFileTextFilter + '|' + SDefaultFilter;
  FileSaveAs1.Dialog.FilterIndex := 0;
  Splitter.CloseSplitter;
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
  Result := EditSourceText.Lines.Text;
end;

function TFormTextEditor.GetText: string;
begin
  Result := EditText.Lines.Text;
end;

//------------------------------------------------------------------------------

procedure TFormTextEditor.SetSourceText(const Value: string);
begin
  EditSourceText.Lines.Text := Value;
end;

procedure TFormTextEditor.SetText(const Value: string);
begin
  EditText.Lines.Text := Value;
end;

end.
