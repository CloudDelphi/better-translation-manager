inherited FormLanguages: TFormLanguages
  Caption = 'Languages'
  ClientHeight = 371
  ClientWidth = 601
  OnCreate = FormCreate
  OnResize = FormResize
  ExplicitWidth = 607
  ExplicitHeight = 399
  PixelsPerInch = 96
  TextHeight = 13
  inherited LayoutControlButtons: TdxLayoutControl
    Top = 323
    Width = 601
    ExplicitTop = 323
    ExplicitWidth = 601
    inherited ButtonOK: TcxButton
      Left = 434
      ExplicitLeft = 434
    end
    inherited ButtonCancel: TcxButton
      Left = 515
      ExplicitLeft = 515
    end
  end
  inherited LayoutControlHeader: TdxLayoutControl
    Width = 601
    ExplicitWidth = 601
    inherited LabelHeader: TcxLabel
      Caption = 'Project languages'
      ExplicitWidth = 579
      Width = 579
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
        Left = 99
        Top = 6
        RepositoryItem = DataModuleMain.EditRepositoryComboBoxItemLanguage
        Style.HotTrack = False
        TabOrder = 0
        Width = 199
      end
      object CheckListBoxLanguages: TcxCheckListBox [1]
        Left = 99
        Top = 33
        Width = 480
        Height = 181
        Columns = 2
        EditValueFormat = cvfIndices
        Items = <
          item
            State = cbsChecked
            Text = 'DA-dk'
          end
          item
            Text = 'EN-us'
          end>
        ParentColor = True
        Sorted = True
        Style.TransparentBorder = True
        TabOrder = 1
      end
      object CheckBoxApplyFilter: TcxCheckBox [2]
        Left = 99
        Top = 220
        Action = ActionApplyFilter
        Style.HotTrack = False
        TabOrder = 2
        Transparent = True
      end
      inherited LayoutControlGroup_Root: TdxLayoutGroup
        AlignVert = avClient
        ItemIndex = 3
      end
      object dxLayoutItem2: TdxLayoutItem
        Parent = LayoutControlGroup_Root
        CaptionOptions.Text = '&Source language:'
        Control = ComboBoxSourceLanguage
        ControlOptions.AlignHorz = ahLeft
        ControlOptions.OriginalHeight = 21
        ControlOptions.OriginalWidth = 199
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutGroup1: TdxLayoutGroup
        Parent = LayoutControlGroup_Root
        AlignVert = avBottom
        CaptionOptions.Text = 'New Group'
        ButtonOptions.Buttons = <>
        LayoutDirection = ldHorizontal
        ShowBorder = False
        Index = 4
      end
      object dxLayoutItem1: TdxLayoutItem
        Parent = LayoutControlGroup_Root
        AlignHorz = ahClient
        AlignVert = avClient
        CaptionOptions.AlignVert = tavTop
        CaptionOptions.Text = '&Target languages:'
        Control = CheckListBoxLanguages
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
        CaptionOptions.Text = 'Empty Space Item'
        SizeOptions.Height = 10
        SizeOptions.Width = 10
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
