inherited FormSelectDuplicate: TFormSelectDuplicate
  ActiveControl = ComboBoxAction
  Caption = 'Duplicate translations found'
  ClientHeight = 355
  ClientWidth = 502
  ExplicitWidth = 518
  ExplicitHeight = 393
  PixelsPerInch = 96
  TextHeight = 15
  inherited LayoutControlButtons: TdxLayoutControl
    Top = 300
    Width = 502
    ExplicitTop = 300
    ExplicitWidth = 502
    inherited ButtonOK: TcxButton
      Left = 250
      ExplicitLeft = 250
    end
    inherited ButtonCancel: TcxButton
      Left = 414
      TabOrder = 2
      ExplicitLeft = 414
    end
    object ButtonSkip: TcxButton [2]
      Left = 332
      Top = 13
      Width = 75
      Height = 25
      Action = ActionSkip
      Default = True
      TabOrder = 1
    end
    inherited LayoutItemButtonCancel: TdxLayoutItem
      Index = 2
    end
    inherited LayoutGroupButtons: TdxLayoutGroup
      ItemIndex = 1
    end
    object LayoutItemButtonSkip: TdxLayoutItem
      Parent = LayoutGroupButtons
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = ButtonSkip
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  inherited LayoutControlHeader: TdxLayoutControl
    Width = 502
    ExplicitWidth = 502
    inherited LayoutItemHeader: TdxLayoutLabeledItem
      CaptionOptions.Text = 'Select the translation to use'
    end
  end
  inherited LayoutControl: TdxLayoutControl
    Width = 502
    Height = 255
    ExplicitTop = 45
    ExplicitWidth = 502
    ExplicitHeight = 259
    object LabelSourceValue: TLabel [0]
      Left = 80
      Top = 77
      Width = 415
      Height = 16
      AutoSize = False
      Caption = '-'
      EllipsisPosition = epEndEllipsis
      ShowAccelChar = False
      Transparent = True
      Layout = tlCenter
    end
    object ListViewDuplicates: TcxListView [1]
      Left = 80
      Top = 100
      Width = 415
      Height = 121
      ColumnClick = False
      Columns = <
        item
          AutoSize = True
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      ShowColumnHeaders = False
      TabOrder = 2
      ViewStyle = vsReport
      OnDblClick = ListViewDuplicatesDblClick
    end
    object CheckBoxAll: TcxCheckBox [2]
      Left = 296
      Top = 7
      Caption = '&Do this for all ambiguities'
      Enabled = False
      Properties.OnChange = CheckBoxAllPropertiesChange
      Style.HotTrack = False
      TabOrder = 1
      Transparent = True
    end
    object ComboBoxAction: TcxComboBox [3]
      Left = 80
      Top = 7
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
    object LabelContext: TLabel [4]
      Left = 80
      Top = 54
      Width = 415
      Height = 16
      AutoSize = False
      Caption = '-'
      EllipsisPosition = epPathEllipsis
      ShowAccelChar = False
      Transparent = True
      Layout = tlCenter
    end
    object CheckBoxApplyToIdentical: TcxCheckBox [5]
      Left = 80
      Top = 228
      Caption = '&Use this translation for identical ambiguities'
      Style.HotTrack = False
      TabOrder = 3
      Transparent = True
    end
    inherited LayoutControlGroup_Root: TdxLayoutGroup
      AlignVert = avClient
      ItemIndex = 5
    end
    object LayoutItemSourceValue: TdxLayoutItem
      Parent = LayoutControlGroup_Root
      CaptionOptions.Text = 'Source text:'
      Control = LabelSourceValue
      ControlOptions.OriginalHeight = 14
      ControlOptions.OriginalWidth = 46
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object LayoutItemTranslationList: TdxLayoutItem
      Parent = LayoutControlGroup_Root
      AlignVert = avClient
      CaptionOptions.AlignVert = tavTop
      CaptionOptions.Text = '&Translations:'
      Control = ListViewDuplicates
      ControlOptions.OriginalHeight = 156
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = CheckBoxAll
      ControlOptions.OriginalHeight = 20
      ControlOptions.OriginalWidth = 158
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 2
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = '&Action:'
      Control = ComboBoxAction
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 192
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem
      Parent = LayoutControlGroup_Root
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      CaptionOptions.Text = 'Empty Space Item'
      Index = 1
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = LayoutControlGroup_Root
      CaptionOptions.Visible = False
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutEmptySpaceItem3: TdxLayoutEmptySpaceItem
      Parent = dxLayoutGroup2
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      CaptionOptions.Text = 'Empty Space Item'
      Index = 1
    end
    object LayoutItemContext: TdxLayoutItem
      Parent = LayoutControlGroup_Root
      CaptionOptions.Text = 'Context:'
      Control = LabelContext
      ControlOptions.OriginalHeight = 14
      ControlOptions.OriginalWidth = 8
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = LayoutControlGroup_Root
      CaptionOptions.Text = ' '
      Control = CheckBoxApplyToIdentical
      ControlOptions.OriginalHeight = 20
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 5
    end
  end
  inherited ActionList: TActionList
    inherited ActionOK: TAction
      OnUpdate = ActionOKUpdate
    end
    object ActionSkip: TAction
      Caption = 'Skip'
      Hint = 'Skip this value'
      OnExecute = ActionSkipExecute
    end
  end
end
