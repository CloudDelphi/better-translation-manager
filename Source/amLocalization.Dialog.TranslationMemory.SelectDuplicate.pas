unit amLocalization.Dialog.TranslationMemory.SelectDuplicate;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore, cxContainer,
  cxEdit, dxLayoutcxEditAdapters, dxLayoutContainer, dxLayoutControlAdapters, Vcl.ComCtrls, Vcl.Menus, Vcl.StdCtrls, cxButtons,
  cxListView, cxLabel, cxClasses, dxLayoutControl, System.Actions, Vcl.ActnList, cxCheckBox, cxTextEdit, cxMaskEdit, cxDropDownEdit;

type
  TDuplicateAction = (daPrompt, daFirst, daFirstAll, daSkip, daSkipAll, daAbort);

  TFormSelectDuplicate = class(TForm)
    LayoutControlGroup_Root: TdxLayoutGroup;
    LayoutControl: TdxLayoutControl;
    dxLayoutLabeledItem1: TdxLayoutLabeledItem;
    dxLayoutItem1: TdxLayoutItem;
    LabelSourceValue: TcxLabel;
    ListViewDuplicates: TcxListView;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    cxButton1: TcxButton;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    ActionList1: TActionList;
    ActionOK: TAction;
    ActionCancel: TAction;
    dxLayoutItem4: TdxLayoutItem;
    cxButton2: TcxButton;
    CheckBoxAll: TcxCheckBox;
    dxLayoutItem5: TdxLayoutItem;
    ComboBoxAction: TcxComboBox;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutEmptySpaceItem3: TdxLayoutEmptySpaceItem;
    procedure ActionCancelExecute(Sender: TObject);
    procedure ActionOKExecute(Sender: TObject);
    procedure ActionOKUpdate(Sender: TObject);
    procedure ComboBoxActionPropertiesChange(Sender: TObject);
    procedure CheckBoxAllPropertiesChange(Sender: TObject);
  private
    FDuplicateAction: TDuplicateAction;
  public
    function SelectDuplicate(const Source: string; Duplicates: TStrings; var Value: string): boolean;
    property DuplicateAction: TDuplicateAction read FDuplicateAction write FDuplicateAction;
  end;

implementation

{$R *.dfm}

uses
  amLocalization.Data.Main;

const
  ActionMap: array[boolean, 0..2] of TDuplicateAction =
   ((daPrompt, daFirst, daSkip), (daPrompt, daFirstAll, daSkipAll));

procedure TFormSelectDuplicate.ActionCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormSelectDuplicate.ActionOKExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TFormSelectDuplicate.ActionOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FDuplicateAction <> daPrompt) or (ListViewDuplicates.Selected <> nil);
end;

procedure TFormSelectDuplicate.CheckBoxAllPropertiesChange(Sender: TObject);
begin
  FDuplicateAction := ActionMap[CheckBoxAll.Checked, ComboBoxAction.ItemIndex];
end;

procedure TFormSelectDuplicate.ComboBoxActionPropertiesChange(Sender: TObject);
begin
  FDuplicateAction := ActionMap[CheckBoxAll.Checked, ComboBoxAction.ItemIndex];

  ListViewDuplicates.Enabled := (FDuplicateAction = daPrompt);
  if (FDuplicateAction in [daFirst, daFirstAll]) then
    ListViewDuplicates.ItemIndex := 0;

  CheckBoxAll.Enabled := (FDuplicateAction <> daPrompt);
  CheckBoxAll.Checked := (FDuplicateAction in [daSkipAll, daFirstAll]);
end;

function TFormSelectDuplicate.SelectDuplicate(const Source: string; Duplicates: TStrings; var Value: string): boolean;
var
  s: string;
begin
  if (FDuplicateAction = daAbort) then
    Exit(False);

  if (not(FDuplicateAction in [daSkipAll, daFirstAll])) then
  begin
    LabelSourceValue.Caption := Source;
    CheckBoxAll.Checked := (FDuplicateAction in [daSkipAll, daFirstAll]);
    case FDuplicateAction of
      daPrompt:
        ComboBoxAction.ItemIndex := 0;

      daFirst, daFirstAll:
        ComboBoxAction.ItemIndex := 1;

      daSkip, daSkipAll:
        ComboBoxAction.ItemIndex := 2;
    else
      Value := '';
    end;


    ListViewDuplicates.Items.Clear;
    for s in Duplicates do
      ListViewDuplicates.Items.Add.Caption := s;

    Result := (ShowModal <> mrCancel);
  end else
    Result := True;

  if (Result) then
  begin
    case FDuplicateAction of
      daPrompt:
        begin
          Assert(ListViewDuplicates.Selected <> nil);
          Value := Duplicates[ListViewDuplicates.Selected.Index];
        end;

      daFirst, daFirstAll:
        Value := Duplicates[0];

      daSkip, daSkipAll:
        Value := '';
    else
      Value := '';
    end;
  end else
    FDuplicateAction := daAbort;
end;

end.
