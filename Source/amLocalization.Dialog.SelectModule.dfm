inherited FormSelectModule: TFormSelectModule
  AutoSize = True
  Caption = 'Select module'
  ClientHeight = 185
  ClientWidth = 337
  ExplicitWidth = 353
  ExplicitHeight = 223
  PixelsPerInch = 96
  TextHeight = 15
  inherited LayoutControlButtons: TdxLayoutControl
    Top = 134
    Width = 337
    Align = alTop
    ExplicitTop = 134
    ExplicitWidth = 337
    inherited ButtonOK: TcxButton
      Left = 167
      ExplicitLeft = 167
    end
    inherited ButtonCancel: TcxButton
      Left = 249
      ExplicitLeft = 249
    end
  end
  inherited LayoutControlHeader: TdxLayoutControl
    Width = 337
    ExplicitWidth = 337
    inherited LabelHeader: TcxLabel
      ExplicitWidth = 311
      Width = 311
    end
  end
  inherited LayoutControl: TdxLayoutControl
    Width = 337
    Height = 93
    Align = alTop
    AutoSize = True
    ExplicitLeft = 0
    ExplicitTop = 144
    ExplicitWidth = 337
    ExplicitHeight = 93
    object ComboBoxModule: TcxComboBox [0]
      Left = 57
      Top = 47
      Properties.DropDownListStyle = lsEditFixedList
      Properties.Sorted = True
      Style.HotTrack = False
      TabOrder = 0
      Width = 252
    end
    inherited LayoutControlGroup_Root: TdxLayoutGroup
      ItemIndex = 1
    end
    object LayoutItemModule: TdxLayoutItem
      Parent = LayoutControlGroup_Root
      CaptionOptions.Text = 'Module:'
      Control = ComboBoxModule
      ControlOptions.AlignHorz = ahLeft
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 252
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object LayoutItemPrompt: TdxLayoutLabeledItem
      Parent = LayoutControlGroup_Root
      CaptionOptions.ShowAccelChar = False
      CaptionOptions.Text = 'Lorem ipsum dolor'
      CaptionOptions.WordWrap = True
      Index = 0
    end
    object LayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Parent = LayoutControlGroup_Root
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 1
    end
    object LayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem
      Parent = LayoutControlGroup_Root
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 3
    end
  end
  inherited ActionList: TActionList
    inherited ActionOK: TAction
      OnUpdate = ActionOKUpdate
    end
  end
end
