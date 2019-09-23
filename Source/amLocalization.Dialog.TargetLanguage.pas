unit amLocalization.Dialog.TargetLanguage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, System.Actions, Vcl.ActnList, Vcl.ExtCtrls,
  Vcl.StdCtrls,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore,
  dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxContainer, cxEdit, cxLabel,
  dxLayoutContainer, cxClasses, cxButtons, dxLayoutControl, cxTextEdit,
  cxMaskEdit, cxDropDownEdit, cxLookupEdit, cxDBLookupEdit, cxDBExtLookupComboBox,

  amLocalization.Dialog;


type
  TFormTargetLanguage = class(TFormDialog)
    LayoutItemTargetLanguage: TdxLayoutItem;
    ComboBoxSourceLanguage: TcxExtLookupComboBox;
    procedure ActionOKUpdate(Sender: TObject);
  private
    function GetLanguageID: integer;
    procedure SetLanguageID(const Value: integer);
  public
    function Execute(const Prompt: string): boolean;

    property LanguageID: integer read GetLanguageID write SetLanguageID;
  end;

implementation

{$R *.dfm}

uses
  amLocalization.Data.Main;


{ TFormTargetLanguage }

procedure TFormTargetLanguage.ActionOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (ComboBoxSourceLanguage.ItemIndex <> -1);

end;

function TFormTargetLanguage.Execute(const Prompt: string): boolean;
begin
  LabelHeader.Caption := Prompt;

  Result := (ShowModal = mrOK);
end;

function TFormTargetLanguage.GetLanguageID: integer;
begin
  Result := ComboBoxSourceLanguage.EditValue;
end;

procedure TFormTargetLanguage.SetLanguageID(const Value: integer);
begin
  ComboBoxSourceLanguage.EditValue := Value;
end;

end.
