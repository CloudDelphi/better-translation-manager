unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormMain = class(TForm)
    ListBoxValues: TListBox;
    PanelTop: TPanel;
    LabelValue: TLabel;
    EditValue: TEdit;
    ButtonAdd: TButton;
    procedure ButtonAddClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  UITypes;

procedure TFormMain.ButtonAddClick(Sender: TObject);
begin
  ListBoxValues.Items.Add(EditValue.Text);
  EditValue.Text := '';
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
resourcestring
  sCloseQuery = 'Are you sure you want to exit the application?';
begin
  CanClose := (MessageDlg(sCloseQuery, mtConfirmation, [mbYes, mbNo], 0, mbNo) = mrYes);
end;

end.
