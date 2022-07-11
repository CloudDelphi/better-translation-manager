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
  Menu = MainMenu
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
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
    ItemHeight = 15
    Items.Strings = (
      'Foo'
      'Bar')
    TabOrder = 0
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
    object LabelValue: TLabel
      Left = 0
      Top = 0
      Width = 25
      Height = 25
      Align = alLeft
      Caption = 'Text:'
      Layout = tlCenter
      ExplicitHeight = 15
    end
    object EditValue: TEdit
      AlignWithMargins = True
      Left = 33
      Top = 3
      Width = 203
      Height = 22
      Margins.Left = 8
      Margins.Right = 20
      Margins.Bottom = 0
      Align = alClient
      TabOrder = 0
      TextHint = '(enter some text)'
      ExplicitWidth = 253
      ExplicitHeight = 23
    end
    object ButtonAdd: TButton
      Left = 256
      Top = 0
      Width = 125
      Height = 25
      Align = alRight
      Caption = 'Add text'
      Default = True
      TabOrder = 1
      OnClick = ButtonAddClick
    end
  end
  object MainMenu: TMainMenu
    Left = 168
    Top = 72
    object MenuItemFile: TMenuItem
      Caption = '&File'
      object MenuItemExit: TMenuItem
        Caption = 'E&xit'
        ShortCut = 32883
        OnClick = MenuItemExitClick
      end
    end
    object MenuItemLanguage: TMenuItem
      Caption = '&Language'
    end
  end
end
