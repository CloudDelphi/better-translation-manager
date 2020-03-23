object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Hello World'
  ClientHeight = 239
  ClientWidth = 405
  Color = clBtnFace
  ParentFont = True
  Padding.Left = 12
  Padding.Top = 12
  Padding.Right = 12
  Padding.Bottom = 12
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object ListBoxValues: TListBox
    AlignWithMargins = True
    Left = 12
    Top = 45
    Width = 381
    Height = 182
    Margins.Left = 0
    Margins.Top = 8
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    ItemHeight = 13
    Items.Strings = (
      'Foo'
      'Bar')
    TabOrder = 0
    ExplicitLeft = 9
    ExplicitTop = 34
    ExplicitWidth = 329
    ExplicitHeight = 146
  end
  object PanelTop: TPanel
    Left = 12
    Top = 12
    Width = 381
    Height = 25
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitWidth = 353
    object LabelValue: TLabel
      Left = 0
      Top = 0
      Width = 26
      Height = 25
      Align = alLeft
      Caption = 'Text:'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object EditValue: TEdit
      AlignWithMargins = True
      Left = 34
      Top = 3
      Width = 252
      Height = 22
      Margins.Left = 8
      Margins.Right = 20
      Margins.Bottom = 0
      Align = alClient
      TabOrder = 0
      TextHint = '(enter some text)'
      ExplicitLeft = 76
      ExplicitTop = 2
      ExplicitWidth = 165
      ExplicitHeight = 21
    end
    object ButtonAdd: TButton
      Left = 306
      Top = 0
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'Add text'
      Default = True
      TabOrder = 1
      OnClick = ButtonAddClick
      ExplicitLeft = 264
    end
  end
end
