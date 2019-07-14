object Form4: TForm4
  Left = 0
  Top = 0
  HelpType = htKeyword
  HelpKeyword = 'xxx'
  ActiveControl = ListView1
  Caption = 'Form4'
  ClientHeight = 385
  ClientWidth = 704
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 0
    Top = 41
    Width = 704
    Height = 344
    Hint = 'Test'
    Align = alClient
    Columns = <
      item
        Caption = 'Module'
        Width = 100
      end
      item
        Caption = 'Item'
        Width = 100
      end
      item
        Caption = 'Type'
        Width = 100
      end
      item
        Caption = 'Name'
        Width = 100
      end
      item
        AutoSize = True
        Caption = 'Value'
      end
      item
        Caption = 'New value'
        Width = 100
      end>
    FlatScrollBars = True
    GridLines = True
    ReadOnly = True
    RowSelect = True
    SortType = stText
    TabOrder = 0
    ViewStyle = vsReport
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 704
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 728
    object Button1: TButton
      Left = 12
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Parse DFM'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 108
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Load XLIFF'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'XLIFF files (*.xlf;*.xliff)|*.xlf;*.xliff|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 360
    Top = 196
  end
end
