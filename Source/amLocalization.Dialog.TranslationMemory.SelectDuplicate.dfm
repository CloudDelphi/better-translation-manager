object FormSelectDuplicate: TFormSelectDuplicate
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Duplicate translations found'
  ClientHeight = 355
  ClientWidth = 502
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object LayoutControl: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 502
    Height = 355
    Align = alClient
    TabOrder = 0
    LayoutLookAndFeel = DataModuleMain.LayoutSkinLookAndFeel
    ExplicitWidth = 598
    ExplicitHeight = 349
    object LabelSourceValue: TcxLabel
      Left = 73
      Top = 108
      Caption = '-'
      Style.HotTrack = False
      Properties.ShowAccelChar = False
      Properties.ShowEndEllipsis = True
      Transparent = True
    end
    object ListViewDuplicates: TcxListView
      Left = 73
      Top = 131
      Width = 423
      Height = 150
      ColumnClick = False
      Columns = <
        item
          AutoSize = True
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      ShowColumnHeaders = False
      TabOrder = 4
      ViewStyle = vsReport
      OnDblClick = ListViewDuplicatesDblClick
    end
    object cxButton1: TcxButton
      Left = 340
      Top = 324
      Width = 75
      Height = 25
      Action = ActionOK
      TabOrder = 6
    end
    object cxButton2: TcxButton
      Left = 421
      Top = 324
      Width = 75
      Height = 25
      Action = ActionCancel
      TabOrder = 7
    end
    object CheckBoxAll: TcxCheckBox
      Left = 287
      Top = 42
      Caption = 'Do this for all conflicts'
      Enabled = False
      Properties.OnChange = CheckBoxAllPropertiesChange
      Style.HotTrack = False
      TabOrder = 1
    end
    object ComboBoxAction: TcxComboBox
      Left = 73
      Top = 42
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Use the translation I select'
        'Use first available translation'
        'Skip value')
      Properties.OnChange = ComboBoxActionPropertiesChange
      Style.HotTrack = False
      TabOrder = 0
      Text = 'Use the translation I select'
      Width = 192
    end
    object LabelContext: TcxLabel
      Left = 73
      Top = 85
      Caption = '-'
      Style.HotTrack = False
      Properties.ShowAccelChar = False
      Properties.ShowEndEllipsis = True
      Transparent = True
    end
    object CheckBoxApplyToIdentical: TcxCheckBox
      Left = 73
      Top = 287
      Caption = 'Use this translation for identical conflicts'
      Style.HotTrack = False
      TabOrder = 5
    end
    object LayoutControlGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 7
      ShowBorder = False
      Index = -1
    end
    object dxLayoutLabeledItem1: TdxLayoutLabeledItem
      Parent = LayoutControlGroup_Root
      CaptionOptions.ShowAccelChar = False
      CaptionOptions.Text = 'Select the translation to use.'
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = LayoutControlGroup_Root
      CaptionOptions.Text = 'Source text:'
      Control = LabelSourceValue
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 46
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = LayoutControlGroup_Root
      AlignVert = avClient
      CaptionOptions.AlignVert = tavTop
      CaptionOptions.Text = 'Translations:'
      Control = ListViewDuplicates
      ControlOptions.OriginalHeight = 156
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = cxButton1
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = LayoutControlGroup_Root
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 9
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = LayoutControlGroup_Root
      CaptionOptions.Text = 'Separator'
      Index = 8
    end
    object dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Parent = LayoutControlGroup_Root
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = cxButton2
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = CheckBoxAll
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 126
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 2
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'Action:'
      Control = ComboBoxAction
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 192
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem
      Parent = LayoutControlGroup_Root
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 3
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = LayoutControlGroup_Root
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object dxLayoutEmptySpaceItem3: TdxLayoutEmptySpaceItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 1
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = LayoutControlGroup_Root
      CaptionOptions.Text = 'Context:'
      Control = LabelContext
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 8
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = LayoutControlGroup_Root
      CaptionOptions.Text = ' '
      Control = CheckBoxApplyToIdentical
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 7
    end
  end
  object ActionList1: TActionList
    Left = 288
    Top = 156
    object ActionOK: TAction
      Caption = 'OK'
      OnExecute = ActionOKExecute
      OnUpdate = ActionOKUpdate
    end
    object ActionCancel: TAction
      Caption = 'Cancel'
      OnExecute = ActionCancelExecute
    end
  end
end
