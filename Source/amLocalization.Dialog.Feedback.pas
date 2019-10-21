unit amLocalization.Dialog.Feedback;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ExtCtrls, ActnList, System.Actions,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore, dxSkinscxPCPainter, cxContainer,
  cxEdit, dxLayoutcxEditAdapters, dxLayoutControlAdapters, cxButtons, cxTextEdit, dxGDIPlusClasses,
  cxImage, cxMemo, dxLayoutContainer, cxCheckBox, dxLayoutLookAndFeels, cxClasses, dxLayoutControl, cxLabel;

type
  TFeedbackKind = (FeedbackPositive, FeedbackNegative);

  TFormFeedback = class(TForm)
    LayoutControlGroup_Root: TdxLayoutGroup;
    LayoutControl: TdxLayoutControl;
    LayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    LayoutLookAndFeelNormal: TdxLayoutSkinLookAndFeel;
    LayoutLabeledItemHeader: TdxLayoutLabeledItem;
    LayoutLookAndFeelHeader: TdxLayoutSkinLookAndFeel;
    CheckBoxBug: TcxCheckBox;
    LayoutItemCheckBoxBug: TdxLayoutItem;
    MemoDescription: TcxMemo;
    LayoutItemMemoBug: TdxLayoutItem;
    CheckBoxScreenShot: TcxCheckBox;
    LayoutItemCheckBoxScreenShot: TdxLayoutItem;
    ImageScreenShot: TcxImage;
    LayoutItemScreenShotImage: TdxLayoutItem;
    LayoutGroupHeader: TdxLayoutGroup;
    LayoutGroupBug: TdxLayoutGroup;
    LayoutGroupScreenShot: TdxLayoutGroup;
    LayoutGroupEmail: TdxLayoutGroup;
    LayoutItemEmail: TdxLayoutItem;
    CheckBoxEmail: TcxCheckBox;
    EditEmail: TcxTextEdit;
    LayoutItemEmailEdit: TdxLayoutItem;
    ButtonSend: TcxButton;
    LayoutItemButtonSend: TdxLayoutItem;
    LayoutLabeledItemFooter: TdxLayoutLabeledItem;
    PanelBackground: TPanel;
    LabelScreenShotEdit: TcxLabel;
    LayoutItemScreenShotEdit: TdxLayoutItem;
    LayoutGroupScreenShotEdit: TdxLayoutGroup;
    ActionList: TActionList;
    ActionScreenshot: TAction;
    ActionFeedbackBug: TAction;
    ActionAddEmail: TAction;
    ActionSend: TAction;
    procedure FormCreate(Sender: TObject);
    procedure MemoDescriptionEnter(Sender: TObject);
    procedure MemoDescriptionExit(Sender: TObject);
    procedure LabelScreenShotEditClick(Sender: TObject);
    procedure ActionScreenshotExecute(Sender: TObject);
    procedure ActionAddEmailExecute(Sender: TObject);
    procedure ActionSendExecute(Sender: TObject);
    procedure ActionFeedbackBugExecute(Sender: TObject);
  private
    FHasDescription: boolean;
    FKind: TFeedbackKind;
  protected
    procedure UpdateDescription(Edit: boolean);
  public
    function Execute(AKind: TFeedbackKind): boolean;
  end;

implementation

{$R *.dfm}

uses
  IOUtils,
  madNVBitmap,
  MadExcept;

resourcestring
  sFeedbackPositiveHeader = 'Thanks for your feedback. What did you like?';
  sFeedbackNegativeHeader = 'We appreciate your feedback. Is there something we can do better?';
  sFeedbackPraisePrompt = 'Tell us what you liked';
  sFeedbackImprovementPrompt = 'Tell us what we could do better';
  sFeedbackBugPrompt = 'Tell us about the bug. What appears broken?';

procedure TFormFeedback.ActionAddEmailExecute(Sender: TObject);
begin
  EditEmail.Enabled := TAction(Sender).Checked;
end;

procedure TFormFeedback.ActionFeedbackBugExecute(Sender: TObject);
begin
  UpdateDescription(False);
end;

procedure TFormFeedback.ActionScreenshotExecute(Sender: TObject);
begin
  LayoutItemScreenShotImage.Visible := TAction(Sender).Checked;
  LayoutItemScreenShotEdit.Visible := TAction(Sender).Checked;
end;

procedure TFormFeedback.ActionSendExecute(Sender: TObject);
var
  Attachments: IMEAttachments;
  Filename: string;
  Subject: string;
  Msg: string;
begin
  if (ActionScreenshot.Checked) then
  begin
    Filename := TPath.GetTempFileName;
    ImageScreenShot.Picture.Graphic.SaveToFile(Filename);

    Attachments := MadExcept.NewAttachments;
    Attachments.Add(Filename, 'screenshot.png');
  end else
    Attachments := nil;

  if (FHasDescription) then
    Msg := MemoDescription.Text
  else
    Msg := '';

  Msg := Msg + #13#10 + '---';

  if (ActionFeedbackBug.Checked) then
    Msg := Msg + #13#10 + '[bug:true]';
  (*
  if (ActionAddEmail.Checked) then
    Msg := Msg + #13#10 + Format('[email:%s]', [EditEmail.Text]);
  *)

  if (FKind = FeedbackPositive) then
    Subject := 'Feedback: amTranslationManager made me happy :-)'
  else
    Subject := 'Feedback: amTranslationManager made me sad :-(';

  SendMapiMail(MESettings.MailAddr,
    Subject, Msg,
    Attachments,
    Handle);
end;

function TFormFeedback.Execute(AKind: TFeedbackKind): boolean;
begin
  FKind := AKind;

  if (FKind = FeedbackPositive) then
    LayoutLabeledItemHeader.CaptionOptions.Text := sFeedbackPositiveHeader
  else
    LayoutLabeledItemHeader.CaptionOptions.Text := sFeedbackNegativeHeader;

  LayoutItemCheckBoxBug.Visible := (FKind = FeedbackNegative);

  UpdateDescription(False);

  AutoSize := True;

  Result := (ShowModal = mrOK);
end;

procedure TFormFeedback.FormCreate(Sender: TObject);
var
  ScreenShot: INVBitmap;
  Stream: TMemoryStream;
  s: AnsiString;
begin
  // Caption := BrandString(Caption);

  ScreenShot := madNVBitmap.ScreenShot(True);
  s := ScreenShot.AsBmpStr;
  Stream := TMemoryStream.Create;
  try
    Stream.Size := Length(s);
    Move(PAnsiChar(s)^, Stream.Memory^, Length(s));
    ScreenShot := nil;
    ImageScreenShot.Picture.Bitmap.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
  // Maintain height of 200px. Scale width to fit.
  ImageScreenShot.Width := Trunc(200/ImageScreenShot.Picture.Height * ImageScreenShot.Picture.Width + (ImageScreenShot.Height-200));
end;

procedure TFormFeedback.LabelScreenShotEditClick(Sender: TObject);
(*
var
  Editor: TBitmapEditor;
*)
begin
(*
  Editor := TBitmapEditor.Create(Self);
  try
    Editor.Image := ImageScreenShot.Picture.Graphic;

    if (not Editor.Execute) then
      exit;

    ImageScreenShot.Picture.Assign(Editor.Image);
  finally
    Editor.Free;
  end;
*)
end;

procedure TFormFeedback.MemoDescriptionEnter(Sender: TObject);
begin
  UpdateDescription(True);
end;

procedure TFormFeedback.MemoDescriptionExit(Sender: TObject);
begin
  FHasDescription := (MemoDescription.Text <> '');
  UpdateDescription(False);
end;

procedure TFormFeedback.UpdateDescription(Edit: boolean);
begin
  if (FHasDescription) or (Edit) then
  begin
    if (not FHasDescription) then
      MemoDescription.Text := '';

    MemoDescription.Style.TextColor := clWindowText;
  end else
  begin
    MemoDescription.Style.TextColor := clGrayText;
    if (FKind = FeedbackPositive) then
      MemoDescription.Text := sFeedbackPraisePrompt
    else
    if (ActionFeedbackBug.Checked) then
      MemoDescription.Text := sFeedbackBugPrompt
    else
      MemoDescription.Text := sFeedbackImprovementPrompt;
  end;
end;

end.

