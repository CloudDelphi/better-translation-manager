inherited FormLanguages: TFormLanguages
  Caption = 'Languages'
  ClientHeight = 371
  ClientWidth = 601
  ExplicitWidth = 617
  ExplicitHeight = 409
  PixelsPerInch = 96
  TextHeight = 15
  inherited LayoutControlButtons: TdxLayoutControl
    Top = 323
    Width = 601
    ExplicitTop = 323
    ExplicitWidth = 601
    inherited ButtonOK: TcxButton
      Left = 414
      ExplicitLeft = 414
    end
    inherited ButtonCancel: TcxButton
      Left = 496
      ExplicitLeft = 496
    end
  end
  inherited LayoutControlHeader: TdxLayoutControl
    Width = 601
    ExplicitWidth = 601
    inherited LabelHeader: TcxLabel
      Caption = 'Project languages'
      ExplicitWidth = 558
      Width = 558
    end
  end
  inherited PanelMain: TPanel
    Width = 601
    Height = 284
    ExplicitTop = 39
    ExplicitWidth = 601
    ExplicitHeight = 284
    inherited LayoutControl: TdxLayoutControl
      Width = 585
      Height = 272
      ExplicitLeft = 8
      ExplicitWidth = 585
      ExplicitHeight = 272
      object ComboBoxSourceLanguage: TcxExtLookupComboBox [0]
        Left = 104
        Top = 7
        RepositoryItem = DataModuleMain.EditRepositoryComboBoxItemLanguage
        Style.HotTrack = False
        TabOrder = 0
        Width = 199
      end
      object CheckBoxApplyFilter: TcxCheckBox [1]
        Left = 104
        Top = 217
        Action = ActionApplyFilter
        Style.HotTrack = False
        TabOrder = 1
        Transparent = True
      end
      inherited LayoutControlGroup_Root: TdxLayoutGroup
        AlignVert = avClient
        ItemIndex = 3
      end
      object LayoutItemSourceLanguage: TdxLayoutItem
        Parent = LayoutControlGroup_Root
        CaptionOptions.Text = '&Source language:'
        Control = ComboBoxSourceLanguage
        ControlOptions.AlignHorz = ahLeft
        ControlOptions.OriginalHeight = 23
        ControlOptions.OriginalWidth = 199
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutGroup1: TdxLayoutGroup
        Parent = LayoutControlGroup_Root
        AlignVert = avBottom
        CaptionOptions.Text = 'New Group'
        LayoutDirection = ldHorizontal
        ShowBorder = False
        Index = 4
      end
      object LayoutItemTargetLanguage: TdxLayoutItem
        Parent = LayoutControlGroup_Root
        AlignHorz = ahClient
        AlignVert = avClient
        CaptionOptions.AlignVert = tavTop
        CaptionOptions.Text = '&Target languages:'
        ControlOptions.OriginalHeight = 249
        ControlOptions.OriginalWidth = 474
        ControlOptions.ShowBorder = False
        Index = 1
      end
      object dxLayoutItem5: TdxLayoutItem
        Parent = LayoutControlGroup_Root
        CaptionOptions.ShowAccelChar = False
        CaptionOptions.Text = ' '
        Control = CheckBoxApplyFilter
        ControlOptions.OriginalHeight = 19
        ControlOptions.OriginalWidth = 85
        ControlOptions.ShowBorder = False
        Index = 2
      end
      object dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
        Parent = LayoutControlGroup_Root
        SizeOptions.Height = 10
        SizeOptions.Width = 10
        CaptionOptions.Text = 'Empty Space Item'
        Index = 3
      end
    end
  end
  object ActionList1: TActionList
    Left = 296
    Top = 192
    object ActionApplyFilter: TAction
      AutoCheck = True
      Caption = '&Filter target language lists with above selection'
      OnExecute = ActionApplyFilterExecute
      OnUpdate = ActionApplyFilterUpdate
    end
  end
end
