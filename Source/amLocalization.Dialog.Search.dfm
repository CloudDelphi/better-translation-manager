object FormSearch: TFormSearch
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Find'
  ClientHeight = 349
  ClientWidth = 717
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 500
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object LayoutControl: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 717
    Height = 349
    Align = alClient
    TabOrder = 0
    LayoutLookAndFeel = DataModuleMain.LayoutSkinLookAndFeel
    DesignSize = (
      717
      349)
    object EditSearchText: TcxTextEdit
      Left = 35
      Top = 6
      Anchors = [akLeft, akTop, akRight]
      Style.HotTrack = False
      TabOrder = 0
      Width = 393
    end
    object ComboBoxSearchScope: TcxCheckComboBox
      Left = 35
      Top = 33
      Anchors = [akTop, akRight]
      Properties.Items = <
        item
          Description = 'Element'
        end
        item
          Description = 'Name'
        end
        item
          Description = 'Source text'
          ShortDescription = 'Source'
        end
        item
          Description = 'Translated text'
          ShortDescription = 'Translation'
        end>
      Properties.OnChange = ComboBoxSearchScopePropertiesChange
      EditValue = 8
      Style.HotTrack = False
      TabOrder = 1
      Width = 393
    end
    object CheckBoxSearchAll: TcxCheckBox
      Left = 446
      Top = 6
      Action = ActionOptionGlobal
      Style.HotTrack = False
      TabOrder = 2
      Transparent = True
    end
    object CheckBoxCaseSensitive: TcxCheckBox
      Left = 446
      Top = 25
      Action = ActionOptionCaseSensitive
      Style.HotTrack = False
      Style.TransparentBorder = True
      TabOrder = 3
      Transparent = True
    end
    object CheckBoxRegExp: TcxCheckBox
      Left = 446
      Top = 44
      Action = ActionOptionRegExp
      Style.HotTrack = False
      Style.TransparentBorder = True
      TabOrder = 4
      Transparent = True
    end
    object ButtonRegExHelp: TcxButton
      Left = 565
      Top = 44
      Width = 21
      Height = 21
      Cursor = crHandPoint
      Hint = 'Test the current expression with the regex101 web site '
      OptionsImage.Glyph.SourceDPI = 96
      OptionsImage.Glyph.Data = {
        424D360400000000000036000000280000001000000010000000010020000000
        000000000000C40E0000C40E0000000000000000000000000000000000000000
        00000000000000000002000000070000000C0000000F0000000F0000000C0000
        0007000000020000000000000000000000000000000000000000000000000000
        0001000000060403021A37211D8354322BB86F4339E56E4339E556332CBB3721
        1D830503021B0000000600000001000000000000000000000000000000010000
        00081C110F4B70453CE0B4958EFFDFD1CEFFF6F2F0FFF6F3F1FFDFD2CFFFB495
        8EFF70453CE01D12104F0000000900000001000000000000000100000006291A
        165F91655AFDDED0CCFFE0C2B1FFC38766FFB87550FFB46B45FFC58969FFE2C3
        B2FFDED0CCFF91645AFD281A165F0000000600000001000000020A0706228559
        4EF0E5D9D7FFCB9676FFB56A3EFFC48A69FFFFEAE0FFF6D9CFFFB1643AFFB468
        3DFFCA9274FFE5D9D7FF85584EF00A0706230000000200000006442E2885BFA2
        9BFFE4C7B5FFBC7044FFB86E43FFC68D6DFFFFEAE3FFF8DFD6FFB6693EFFB86C
        40FFB96B3FFFE3C4B2FFBFA29BFF442E298600000006000000097A544BD1E7DC
        D9FFCE9674FFBF7548FFBD7346FFB76C41FFB8764FFFB36C43FFB86D42FFBC71
        44FFBB7043FFCC916EFFE7DCD9FF7A544BD10000000A0000000A94695EEDF7F3
        F2FFC7875DFFC68657FFC9895BFFC18253FFE7C9B8FFDCB6A0FFBA774BFFC27F
        51FFC07B4EFFC37F56FFF7F3F2FF94695EED0000000B000000099A7063EDF8F4
        F3FFD5A176FFCF9769FFCA8F61FFBE8054FFE3C5B2FFFFF0EDFFDEBCA9FFC187
        5EFFC78A5AFFCD9268FFF8F4F3FF987064ED0000000A0000000786645ACCECE1
        DEFFE0B895FFCD966AFFDFBEA6FFE8CDBFFFBE8864FFFEF2EDFFFFF4EFFFE2C4
        B1FFC88A5CFFDAAC88FFECE2DEFF86645ACD0000000800000004523E387FD1B6
        AEFFF4E5D7FFD29D6FFFE5C9B7FFFFF7F2FFCFA891FFFAECE7FFFFF4F1FFE6C9
        B9FFCB9162FFF1E0D3FFD1B6AEFF523E388000000004000000010D0A091AAD86
        79EFEFE6E3FFECD4BCFFCE9C73FFEBD7C9FFFFF8F4FFFFF8F4FFE7CEBFFFCA95
        6BFFE9CEB7FFEFE6E3FFAE8679EF0D0A091B000000010000000000000003362A
        2651C29B8DFCEBDDD8FFF5ECE5FFE3C6B0FFD3AA85FFD3A985FFE3C6AFFFF6ED
        E6FFEBDCD8FFC29A8DFC362A2652000000030000000000000000000000000000
        00032A211E3EA78477DBD9BFB5FFEBDDD8FFF6F1EFFFF6F1EFFFEBDDD8FFD9BF
        B5FFA78377DB2A211E3F00000003000000010000000000000000000000000000
        0000000000020706050F58464075856B60ADAF8E7FE0AF8E7FE0876C62B05747
        3F760706050F0000000200000000000000000000000000000000000000000000
        0000000000000000000100000002000000030000000400000004000000030000
        00020000000100000000000000000000000000000000}
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 5
      OnClick = ButtonRegExHelpClick
    end
    object ButtonSearch: TcxButton
      Left = 555
      Top = 320
      Width = 75
      Height = 23
      Action = ActionSearch
      Anchors = [akTop, akRight]
      Default = True
      TabOrder = 12
    end
    object ButtonClose: TcxButton
      Left = 636
      Top = 320
      Width = 75
      Height = 23
      Action = ActionClose
      Anchors = [akTop, akRight]
      Cancel = True
      ModalResult = 2
      TabOrder = 13
    end
    object ListViewResult: TcxListView
      Left = 6
      Top = 71
      Width = 705
      Height = 231
      ColumnClick = False
      Columns = <
        item
          Caption = 'Module'
          MaxWidth = 200
          MinWidth = 50
          Width = 60
        end
        item
          AutoSize = True
          Caption = 'Element'
          MaxWidth = 400
          MinWidth = 50
        end
        item
          Caption = 'Name'
          MaxWidth = 400
          MinWidth = 50
          Width = 100
        end
        item
          Caption = 'Text'
          MaxWidth = 400
          MinWidth = 50
          Width = 150
        end
        item
          Caption = 'Found in'
          MaxWidth = 200
          MinWidth = 50
          Width = 100
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 9
      ViewStyle = vsReport
      OnDblClick = ListViewResultDblClick
      OnEnter = ListViewResultEnter
      OnExit = ListViewResultExit
    end
    object ButtonGoto: TcxButton
      Left = 6
      Top = 320
      Width = 75
      Height = 23
      Action = ActionGoTo
      Anchors = [akTop, akRight]
      Cancel = True
      TabOrder = 11
    end
    object CheckBoxIgnoreAccelerator: TcxCheckBox
      Left = 592
      Top = 6
      Action = ActionOptionIgnoreAccelerator
      Style.HotTrack = False
      Style.TransparentBorder = True
      TabOrder = 6
      Transparent = True
    end
    object cxCheckBox1: TcxCheckBox
      Left = 592
      Top = 25
      Action = ActionOptionFuzzy
      Style.HotTrack = False
      Style.TransparentBorder = True
      TabOrder = 7
      Transparent = True
    end
    object SpinEditFuzzy: TcxSpinEdit
      Left = 666
      Top = 44
      Properties.Alignment.Horz = taRightJustify
      Properties.LargeIncrement = 1.000000000000000000
      Properties.MaxValue = 5.000000000000000000
      Properties.MinValue = 1.000000000000000000
      Properties.UseLeftAlignmentOnEditing = False
      Properties.OnChange = SpinEditFuzzyPropertiesChange
      Style.HotTrack = False
      TabOrder = 8
      Value = 1
      Width = 45
    end
    object ButtonAbort: TcxButton
      Left = 474
      Top = 320
      Width = 75
      Height = 23
      Action = ActionAbort
      Anchors = [akTop, akRight]
      Cancel = True
      TabOrder = 10
    end
    object LayoutControlGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup7
      AlignHorz = ahClient
      CaptionOptions.Text = '&Find:'
      Control = EditSearchText
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup7
      AlignHorz = ahClient
      CaptionOptions.Text = 'in'
      Control = ComboBoxSearchScope
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 393
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = LayoutControlGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = LayoutControlGroup_Root
      CaptionOptions.Text = 'Separator'
      Index = 2
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Visible = False
      Control = CheckBoxSearchAll
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 122
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Visible = False
      Control = CheckBoxCaseSensitive
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup4
      CaptionOptions.Visible = False
      Control = CheckBoxRegExp
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup4
      CaptionOptions.Visible = False
      Control = ButtonRegExHelp
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 21
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      UseIndent = False
      Index = 0
    end
    object dxLayoutSeparatorItem2: TdxLayoutSeparatorItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'Separator'
      Index = 1
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object LayoutItemSearch: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = ButtonSearch
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = ButtonClose
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutGroup5: TdxLayoutGroup
      Parent = LayoutControlGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 3
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = LayoutControlGroup_Root
      AlignVert = avClient
      Control = ListViewResult
      ControlOptions.OriginalHeight = 259
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object LayoutItemStatus: TdxLayoutLabeledItem
      Parent = dxLayoutGroup5
      AlignHorz = ahClient
      AlignVert = avCenter
      CaptionOptions.ShowAccelChar = False
      Index = 2
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutGroup5
      CaptionOptions.Visible = False
      Control = ButtonGoto
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutGroup6
      CaptionOptions.Visible = False
      Control = CheckBoxIgnoreAccelerator
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = dxLayoutGroup2
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object dxLayoutGroup6: TdxLayoutGroup
      Parent = dxLayoutGroup1
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      ShowBorder = False
      UseIndent = False
      Index = 1
    end
    object dxLayoutGroup7: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = dxLayoutGroup6
      CaptionOptions.Visible = False
      Control = cxCheckBox1
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem13: TdxLayoutItem
      Parent = dxLayoutGroup6
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Threshold:'
      Padding.Left = 18
      Padding.AssignedValues = [lpavLeft]
      Control = SpinEditFuzzy
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 45
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object LayoutItemAbort: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Visible = False
      Visible = False
      Control = ButtonAbort
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
  object ActionListDialog: TActionList
    Left = 324
    Top = 204
    object ActionSearch: TAction
      Category = 'Buttons'
      Caption = 'S&earch'
      OnExecute = ActionSearchExecute
      OnUpdate = ActionSearchUpdate
    end
    object ActionClose: TAction
      Category = 'Buttons'
      Caption = 'Close'
      OnExecute = ActionCloseExecute
    end
    object ActionGoTo: TAction
      Category = 'Buttons'
      Caption = 'Go to'
      OnExecute = ActionGoToExecute
      OnUpdate = ActionGoToUpdate
    end
    object ActionOptionRegExp: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = '&Regular Expression'
      OnExecute = ActionDummyExecute
      OnUpdate = ActionOptionRegExpUpdate
    end
    object ActionOptionCaseSensitive: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = '&Case sensitive'
      OnExecute = ActionDummyExecute
    end
    object ActionOptionGlobal: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'Se&arch entire project'
      Checked = True
      OnExecute = ActionDummyExecute
    end
    object ActionOptionIgnoreAccelerator: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'Strip accelerators'
      Checked = True
      OnExecute = ActionDummyExecute
    end
    object ActionOptionFuzzy: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'Fuzzy match:'
      OnExecute = ActionDummyExecute
      OnUpdate = ActionOptionFuzzyUpdate
    end
    object ActionAbort: TAction
      Category = 'Buttons'
      Caption = 'Abort'
      ShortCut = 27
      Visible = False
      OnExecute = ActionAbortExecute
    end
  end
end
