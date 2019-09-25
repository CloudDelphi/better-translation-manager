inherited FormSelectModule: TFormSelectModule
  AutoSize = True
  Caption = 'Select module'
  ClientHeight = 192
  ClientWidth = 337
  ExplicitWidth = 343
  ExplicitHeight = 220
  PixelsPerInch = 96
  TextHeight = 13
  inherited LayoutControlButtons: TdxLayoutControl
    Top = 144
    Width = 337
    Align = alTop
    ExplicitTop = 116
    ExplicitWidth = 337
    inherited ButtonOK: TcxButton
      Left = 170
      ExplicitLeft = 170
    end
    inherited ButtonCancel: TcxButton
      Left = 251
      ExplicitLeft = 251
    end
  end
  inherited LayoutControlHeader: TdxLayoutControl
    Width = 337
    ExplicitWidth = 337
    inherited LabelHeader: TcxLabel
      ExplicitWidth = 315
      Width = 315
    end
  end
  inherited PanelMain: TPanel
    Width = 337
    Height = 105
    Align = alTop
    ExplicitWidth = 337
    ExplicitHeight = 77
    inherited LayoutControl: TdxLayoutControl
      Width = 321
      Height = 93
      Align = alTop
      AutoSize = True
      ExplicitWidth = 321
      ExplicitHeight = 65
      object ComboBoxModule: TcxComboBox [0]
        Left = 49
        Top = 42
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
        ControlOptions.OriginalHeight = 21
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
  end
  inherited ActionList: TActionList
    inherited ActionOK: TAction
      OnUpdate = ActionOKUpdate
    end
  end
end
