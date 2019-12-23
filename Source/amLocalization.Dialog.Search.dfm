inherited FormSearch: TFormSearch
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeable
  Caption = 'Find'
  ClientHeight = 339
  ClientWidth = 707
  Constraints.MinHeight = 300
  Constraints.MinWidth = 500
  OnClose = FormClose
  OnKeyPress = FormKeyPress
  OnShortCut = FormShortCut
  ExplicitWidth = 723
  ExplicitHeight = 378
  PixelsPerInch = 96
  TextHeight = 13
  inherited LayoutControlButtons: TdxLayoutControl
    Top = 291
    Width = 707
    ExplicitTop = 291
    ExplicitWidth = 707
    inherited ButtonOK: TcxButton
      Left = 540
      TabOrder = 5
      ExplicitLeft = 540
    end
    inherited ButtonCancel: TcxButton
      Left = 621
      TabOrder = 6
      ExplicitLeft = 621
    end
    object ButtonGoto: TcxButton [2]
      Left = 11
      Top = 11
      Width = 75
      Height = 25
      Action = ActionGoTo
      Anchors = [akTop, akRight]
      Cancel = True
      TabOrder = 0
    end
    object ButtonAbort: TcxButton [3]
      Left = 378
      Top = 11
      Width = 75
      Height = 25
      Action = ActionAbort
      Anchors = [akTop, akRight]
      Cancel = True
      TabOrder = 3
    end
    object ButtonSearch: TcxButton [4]
      Left = 297
      Top = 11
      Width = 75
      Height = 25
      Action = ActionSearch
      Anchors = [akTop, akRight]
      Default = True
      TabOrder = 2
    end
    object ButtonClose: TcxButton [5]
      Left = 459
      Top = 11
      Width = 75
      Height = 25
      Action = ActionClose
      Anchors = [akTop, akRight]
      Cancel = True
      ModalResult = 2
      TabOrder = 4
    end
    object ButtonMark: TcxButton [6]
      Left = 92
      Top = 11
      Width = 75
      Height = 25
      Action = ActionMark
      Anchors = [akTop, akRight]
      Cancel = True
      DropDownMenu = PopupMenuMark
      Kind = cxbkDropDownButton
      TabOrder = 1
    end
    inherited LayoutItemButtonOK: TdxLayoutItem
      Visible = False
      Enabled = False
      Index = 5
    end
    inherited LayoutItemButtonCancel: TdxLayoutItem
      Visible = False
      Enabled = False
      Index = 6
    end
    inherited LayoutGroupButtons: TdxLayoutGroup
      ItemIndex = 1
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = LayoutGroupButtons
      CaptionOptions.Visible = False
      Control = ButtonGoto
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object LayoutItemAbort: TdxLayoutItem
      Parent = LayoutGroupButtons
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Visible = False
      Control = ButtonAbort
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object LayoutItemSearch: TdxLayoutItem
      Parent = LayoutGroupButtons
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = ButtonSearch
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object LayoutItemClose: TdxLayoutItem
      Parent = LayoutGroupButtons
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = ButtonClose
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = LayoutGroupButtons
      CaptionOptions.Visible = False
      Control = ButtonMark
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  inherited LayoutControlHeader: TdxLayoutControl
    Width = 707
    Visible = False
    ExplicitWidth = 707
    inherited LabelHeader: TcxLabel
      ExplicitWidth = 685
      Width = 685
    end
  end
  inherited PanelMain: TPanel
    Width = 707
    Height = 252
    ExplicitTop = 39
    ExplicitWidth = 707
    ExplicitHeight = 252
    inherited LayoutControl: TdxLayoutControl
      Width = 691
      Height = 240
      ExplicitLeft = 8
      ExplicitWidth = 691
      ExplicitHeight = 240
      DesignSize = (
        691
        240)
      object EditSearchText: TcxTextEdit [0]
        Left = 46
        Top = 6
        Anchors = [akLeft, akTop, akRight]
        Style.HotTrack = False
        TabOrder = 0
        Width = 356
      end
      object ComboBoxSearchScope: TcxCheckComboBox [1]
        Left = 46
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
        Width = 356
      end
      object CheckBoxOptionSearchAll: TcxCheckBox [2]
        Left = 420
        Top = 6
        Action = ActionOptionGlobal
        Style.HotTrack = False
        TabOrder = 4
        Transparent = True
      end
      object CheckBoxOptionCaseSensitive: TcxCheckBox [3]
        Left = 420
        Top = 25
        Action = ActionOptionCaseSensitive
        Style.HotTrack = False
        Style.TransparentBorder = True
        TabOrder = 5
        Transparent = True
      end
      object CheckBoxOptionRegExp: TcxCheckBox [4]
        Left = 420
        Top = 44
        Action = ActionOptionRegExp
        Style.HotTrack = False
        Style.TransparentBorder = True
        TabOrder = 6
        Transparent = True
      end
      object ButtonRegExHelp: TcxButton [5]
        Left = 539
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
        TabOrder = 7
        OnClick = ButtonRegExHelpClick
      end
      object ListViewResult: TcxListView [6]
        Left = 6
        Top = 90
        Width = 679
        Height = 138
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
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 12
        ViewStyle = vsReport
        OnDblClick = ListViewResultDblClick
        OnEnter = ListViewResultEnter
        OnExit = ListViewResultExit
      end
      object CheckBoxOptionIgnoreAccelerator: TcxCheckBox [7]
        Left = 566
        Top = 6
        Action = ActionOptionIgnoreAccelerator
        Style.HotTrack = False
        Style.TransparentBorder = True
        TabOrder = 8
        Transparent = True
      end
      object CheckBoxOptionFuzzy: TcxCheckBox [8]
        Left = 566
        Top = 44
        Action = ActionOptionFuzzy
        Style.HotTrack = False
        Style.TransparentBorder = True
        TabOrder = 10
        Transparent = True
      end
      object EditOptionFuzzy: TcxSpinEdit [9]
        Left = 640
        Top = 63
        Hint = 
          'Levenshtein distance|Number of deletions, insertions and substit' +
          'utions required to make the strings equal'
        Properties.Alignment.Horz = taRightJustify
        Properties.LargeIncrement = 1.000000000000000000
        Properties.MaxValue = 5.000000000000000000
        Properties.MinValue = 1.000000000000000000
        Properties.UseLeftAlignmentOnEditing = False
        Properties.OnChange = SpinEditFuzzyPropertiesChange
        Style.HotTrack = False
        TabOrder = 11
        Value = 1
        Width = 45
      end
      object CheckBoxOptionExact: TcxCheckBox [10]
        Left = 566
        Top = 25
        Action = ActionOptionExact
        Style.HotTrack = False
        Style.TransparentBorder = True
        TabOrder = 9
        Transparent = True
      end
      object CheckComboBoxStatus: TcxCheckComboBox [11]
        Left = 46
        Top = 60
        Properties.EmptySelectionText = '(any)'
        Properties.Items = <
          item
            Description = 'Translate'
          end
          item
            Description = 'Hold'
          end
          item
            Description = 'Don'#39't translate'
          end>
        Style.HotTrack = False
        TabOrder = 2
        Width = 158
      end
      object CheckComboBoxState: TcxCheckComboBox [12]
        Left = 245
        Top = 60
        Properties.EmptySelectionText = '(any)'
        Properties.Items = <
          item
            Description = 'Obsolete'
          end
          item
            Description = 'Pending'
          end
          item
            Description = 'Proposed'
          end
          item
            Description = 'Translated'
          end>
        Style.HotTrack = False
        TabOrder = 3
        Width = 157
      end
      inherited LayoutControlGroup_Root: TdxLayoutGroup
        AlignVert = avClient
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
        LayoutDirection = ldHorizontal
        ShowBorder = False
        Index = 0
      end
      object dxLayoutItem3: TdxLayoutItem
        Parent = dxLayoutGroup3
        CaptionOptions.Visible = False
        Control = CheckBoxOptionSearchAll
        ControlOptions.OriginalHeight = 19
        ControlOptions.OriginalWidth = 122
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutItem4: TdxLayoutItem
        Parent = dxLayoutGroup3
        CaptionOptions.Visible = False
        Control = CheckBoxOptionCaseSensitive
        ControlOptions.OriginalHeight = 19
        ControlOptions.OriginalWidth = 90
        ControlOptions.ShowBorder = False
        Index = 1
      end
      object dxLayoutItem5: TdxLayoutItem
        Parent = dxLayoutGroup4
        CaptionOptions.Visible = False
        Control = CheckBoxOptionRegExp
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
        ItemIndex = 2
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
      object LayoutItemList: TdxLayoutItem
        Parent = LayoutControlGroup_Root
        AlignVert = avClient
        Control = ListViewResult
        ControlOptions.OriginalHeight = 259
        ControlOptions.OriginalWidth = 121
        ControlOptions.ShowBorder = False
        Index = 1
      end
      object dxLayoutItem11: TdxLayoutItem
        Parent = dxLayoutGroup6
        CaptionOptions.Visible = False
        Control = CheckBoxOptionIgnoreAccelerator
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
        ItemIndex = 2
        ShowBorder = False
        Index = 0
      end
      object dxLayoutItem12: TdxLayoutItem
        Parent = dxLayoutGroup6
        CaptionOptions.Visible = False
        Control = CheckBoxOptionFuzzy
        ControlOptions.OriginalHeight = 19
        ControlOptions.OriginalWidth = 85
        ControlOptions.ShowBorder = False
        Index = 2
      end
      object dxLayoutItem13: TdxLayoutItem
        Parent = dxLayoutGroup6
        AlignHorz = ahLeft
        CaptionOptions.Text = 'Threshold:'
        Padding.Left = 18
        Padding.AssignedValues = [lpavLeft]
        Control = EditOptionFuzzy
        ControlOptions.OriginalHeight = 21
        ControlOptions.OriginalWidth = 45
        ControlOptions.ShowBorder = False
        Index = 3
      end
      object LayoutItemStatus: TdxLayoutLabeledItem
        Parent = LayoutControlGroup_Root
        CaptionOptions.ShowAccelChar = False
        Index = 2
      end
      object dxLayoutItem9: TdxLayoutItem
        Parent = dxLayoutGroup6
        CaptionOptions.Visible = False
        Control = CheckBoxOptionExact
        ControlOptions.OriginalHeight = 19
        ControlOptions.OriginalWidth = 85
        ControlOptions.ShowBorder = False
        Index = 1
      end
      object dxLayoutItem14: TdxLayoutItem
        Parent = dxLayoutGroup5
        AlignHorz = ahClient
        CaptionOptions.Text = 'Status:'
        Control = CheckComboBoxStatus
        ControlOptions.OriginalHeight = 21
        ControlOptions.OriginalWidth = 121
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutItem10: TdxLayoutItem
        Parent = dxLayoutGroup5
        AlignHorz = ahClient
        CaptionOptions.Text = 'State:'
        Control = CheckComboBoxState
        ControlOptions.OriginalHeight = 21
        ControlOptions.OriginalWidth = 121
        ControlOptions.ShowBorder = False
        Index = 1
      end
      object dxLayoutGroup5: TdxLayoutGroup
        Parent = dxLayoutGroup7
        CaptionOptions.Visible = False
        ButtonOptions.Buttons = <>
        LayoutDirection = ldHorizontal
        ShowBorder = False
        Index = 2
      end
    end
  end
  inherited ActionList: TActionList
    Images = DataModuleMain.ImageListSmall
    Left = 324
    Top = 204
    inherited ActionOK: TAction
      Enabled = False
    end
    inherited ActionCancel: TAction
      Enabled = False
    end
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
    object ActionMark: TAction
      Category = 'Buttons'
      Caption = 'Mark...'
      OnExecute = ActionMarkExecute
      OnUpdate = ActionMarkUpdate
    end
    object ActionMarkTranslate: TAction
      Category = 'Mark'
      Caption = 'Translate'
      ImageIndex = 10
      OnExecute = ActionMarkSetExecute
    end
    object ActionMarkHold: TAction
      Tag = 1
      Category = 'Mark'
      Caption = 'Hold'
      ImageIndex = 11
      OnExecute = ActionMarkSetExecute
    end
    object ActionMarkDontTranslate: TAction
      Tag = 2
      Category = 'Mark'
      Caption = 'Don'#39't translate'
      ImageIndex = 12
      OnExecute = ActionMarkSetExecute
    end
    object ActionOptionExact: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'Exact match'
      OnExecute = ActionDummyExecute
      OnUpdate = ActionOptionExactUpdate
    end
  end
  object PopupMenuMark: TdxBarPopupMenu
    BarManager = BarManagerSearch
    ItemLinks = <
      item
        Visible = True
        ItemName = 'dxBarButton1'
      end
      item
        Visible = True
        ItemName = 'dxBarButton2'
      end
      item
        Visible = True
        ItemName = 'dxBarButton3'
      end>
    UseOwnFont = False
    Left = 408
    Top = 203
    PixelsPerInch = 96
  end
  object BarManagerSearch: TdxBarManager
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Categories.Strings = (
      'Default')
    Categories.ItemsVisibles = (
      2)
    Categories.Visibles = (
      True)
    ImageOptions.Images = DataModuleMain.ImageListSmall
    ImageOptions.StretchGlyphs = False
    PopupMenuLinks = <>
    UseSystemFont = True
    Left = 500
    Top = 203
    PixelsPerInch = 96
    object dxBarButton1: TdxBarButton
      Action = ActionMarkTranslate
      Category = 0
    end
    object dxBarButton2: TdxBarButton
      Action = ActionMarkHold
      Category = 0
    end
    object dxBarButton3: TdxBarButton
      Action = ActionMarkDontTranslate
      Category = 0
    end
  end
end
