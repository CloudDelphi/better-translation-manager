object FormTranslationMemory: TFormTranslationMemory
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Translation Memory'
  ClientHeight = 446
  ClientWidth = 712
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object dxLayoutControl1: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 712
    Height = 446
    Align = alClient
    TabOrder = 0
    object GridTM: TcxGrid
      Left = 10
      Top = 10
      Width = 692
      Height = 383
      TabOrder = 0
      object GridTMDBTableView: TcxGridDBTableView
        Navigator.Buttons.CustomButtons = <>
        OnCustomDrawCell = GridTMDBTableViewCustomDrawCell
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        OptionsBehavior.CellHints = True
        OptionsBehavior.FocusCellOnTab = True
        OptionsBehavior.GoToNextCellOnEnter = True
        OptionsBehavior.IncSearch = True
        OptionsCustomize.ColumnFiltering = False
        OptionsCustomize.ColumnHiding = True
        OptionsCustomize.ColumnsQuickCustomization = True
        OptionsView.CellEndEllipsis = True
        OptionsView.GroupByBox = False
        OptionsView.HeaderEndEllipsis = True
        OptionsView.Indicator = True
      end
      object GridTMLevel: TcxGridLevel
        GridView = GridTMDBTableView
      end
    end
    object ButtonLoad: TcxButton
      Left = 10
      Top = 411
      Width = 75
      Height = 25
      Caption = 'Load TM...'
      TabOrder = 1
      OnClick = ButtonLoadClick
    end
    object ButtonClose: TcxButton
      Left = 627
      Top = 411
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Close'
      ModalResult = 8
      TabOrder = 3
    end
    object ButtonSaveAs: TcxButton
      Left = 91
      Top = 411
      Width = 75
      Height = 25
      Caption = 'Save as...'
      TabOrder = 2
      OnClick = ButtonSaveAsClick
    end
    object dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = ButtonLoad
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignVert = avBottom
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = GridTM
      ControlOptions.OriginalHeight = 200
      ControlOptions.OriginalWidth = 250
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'Separator'
      Index = 1
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = ButtonClose
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup1
      CaptionOptions.Visible = False
      Control = ButtonSaveAs
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  object OpenDialogTMX: TOpenDialog
    Filter = 'Translation Memory files (*.tmx)|*.tmx|All files (*.*)|*.*'
    FilterIndex = 4
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 120
    Top = 224
  end
  object SaveDialogTMX: TSaveDialog
    Filter = 'Translation Memory files (*.tmx)|*.tmx|All files (*.*)|*.*'
    FilterIndex = 4
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 232
    Top = 224
  end
end
