unit amLocalization.Dialog.NewProject;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.StdCtrls,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  dxLayoutcxEditAdapters, dxLayoutContainer, cxClasses, cxDropDownEdit, cxLookupEdit, cxDBLookupEdit, cxDBExtLookupComboBox,
  cxTextEdit, cxMaskEdit, cxButtonEdit, dxLayoutControl, dxLayoutControlAdapters, cxButtons, dxSkinsCore,
  cxGridDBTableView, dxLayoutLookAndFeels, System.Actions, Vcl.ActnList;

type
  TFormNewProject = class(TForm)
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutControl1: TdxLayoutControl;
    EditSourceApplication: TcxButtonEdit;
    dxLayoutItem1: TdxLayoutItem;
    ComboBoxSourceLanguage: TcxExtLookupComboBox;
    dxLayoutItem2: TdxLayoutItem;
    ButtonOK: TcxButton;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    ButtonCancel: TcxButton;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    FileOpenDialogApplication: TFileOpenDialog;
    LayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    LayoutSkinLookAndFeel: TdxLayoutSkinLookAndFeel;
    ActionList1: TActionList;
    ActionOK: TAction;
    ActionCancel: TAction;
    procedure EditSourceApplicationPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure ActionCancelExecute(Sender: TObject);
    procedure ActionOKExecute(Sender: TObject);
    procedure ActionOKUpdate(Sender: TObject);
  private
    function GetSourceApplication: string;
    function GetSourceLanguageID: Word;
    procedure SetSourceApplication(const Value: string);
    procedure SetSourceLanguageID(const Value: Word);
  public
    procedure SetLanguageView(View: TcxGridDBTableView; ListItem: TcxGridDBColumn);
    function Execute: boolean;
    property SourceApplication: string read GetSourceApplication write SetSourceApplication;
    property SourceLanguageID: Word read GetSourceLanguageID write SetSourceLanguageID;
  end;

implementation

{$R *.dfm}

uses
  IOUtils;

procedure TFormNewProject.ActionCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormNewProject.ActionOKExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TFormNewProject.ActionOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (SourceApplication <> '') and (TFile.Exists(SourceApplication));
end;

procedure TFormNewProject.EditSourceApplicationPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  FileOpenDialogApplication.FileName := EditSourceApplication.Text;
  FileOpenDialogApplication.DefaultFolder := TPath.GetDirectoryName(EditSourceApplication.Text);

  if (FileOpenDialogApplication.Execute) then
    EditSourceApplication.Text := FileOpenDialogApplication.FileName;
end;

function TFormNewProject.Execute: boolean;
begin
  Result := (ShowModal = mrOK);
end;

function TFormNewProject.GetSourceApplication: string;
begin
  Result := EditSourceApplication.Text;
end;

function TFormNewProject.GetSourceLanguageID: Word;
begin
  Result := ComboBoxSourceLanguage.EditValue;
end;

procedure TFormNewProject.SetLanguageView(View: TcxGridDBTableView; ListItem: TcxGridDBColumn);
begin
  ComboBoxSourceLanguage.Properties.View := View;
  ComboBoxSourceLanguage.Properties.ListFieldItem := ListItem;
end;

procedure TFormNewProject.SetSourceApplication(const Value: string);
begin
  EditSourceApplication.Text := Value;
end;

procedure TFormNewProject.SetSourceLanguageID(const Value: Word);
begin
  ComboBoxSourceLanguage.EditValue := Value;
end;

end.
