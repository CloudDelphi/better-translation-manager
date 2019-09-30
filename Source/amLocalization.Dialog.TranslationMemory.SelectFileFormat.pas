unit amLocalization.Dialog.TranslationMemory.SelectFileFormat;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, System.Actions, Vcl.ActnList, Vcl.ExtCtrls,
  Vcl.StdCtrls,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore, dxLayoutControlAdapters,
  dxLayoutcxEditAdapters, cxContainer, cxEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxLabel,
  dxLayoutContainer, cxClasses, cxButtons, dxLayoutControl,

  amLocalization.Dialog,
  amLocalization.TranslationMemory.FileFormats;

type
  TFormSelectFileFormat = class(TFormDialog)
    ComboBoxFileFormat: TcxComboBox;
    LayoutItemFileFormat: TdxLayoutItem;
    procedure ActionOKUpdate(Sender: TObject);
  private
  public
    function Execute(FileFormatClasses: TTranslationMemoryFileFormatClasses): TTranslationMemoryFileFormatClass;
  end;

implementation

{$R *.dfm}

{ TFormSelectFileFormat }

procedure TFormSelectFileFormat.ActionOKUpdate(Sender: TObject);
begin
    TAction(Sender).Enabled := (ComboBoxFileFormat.ItemIndex <> -1);
end;

function TFormSelectFileFormat.Execute(FileFormatClasses: TTranslationMemoryFileFormatClasses): TTranslationMemoryFileFormatClass;
var
  FileFormatClass: TTranslationMemoryFileFormatClass;
begin
  for FileFormatClass in FileFormatClasses do
    ComboBoxFileFormat.Properties.Items.Add(FileFormatClass.FileFormatFileDescription);

  ComboBoxFileFormat.ItemIndex := 0;

  if (ShowModal = mrOK) then
    Result := FileFormatClasses[ComboBoxFileFormat.ItemIndex]
  else
    Result := nil;
end;

end.
