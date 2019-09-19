inherited FormTranslationMemory: TFormTranslationMemory
  BorderIcons = [biSystemMenu, biMaximize]
  BorderStyle = bsSizeable
  Caption = 'Translation Memory'
  ClientHeight = 436
  ClientWidth = 702
  OnCreate = FormCreate
  ExplicitWidth = 718
  ExplicitHeight = 474
  PixelsPerInch = 96
  TextHeight = 13
  inherited LayoutControlButtons: TdxLayoutControl
    Top = 388
    Width = 702
    ExplicitTop = 388
    ExplicitWidth = 702
    inherited ButtonOK: TcxButton
      Left = 535
      TabOrder = 3
      ExplicitLeft = 535
    end
    inherited ButtonCancel: TcxButton
      Left = 616
      TabOrder = 4
      ExplicitLeft = 616
    end
    object ButtonClose: TcxButton [2]
      Left = 454
      Top = 11
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Close'
      ModalResult = 8
      TabOrder = 2
    end
    object ButtonSaveAs: TcxButton [3]
      Left = 92
      Top = 11
      Width = 75
      Height = 25
      Action = ActionExport
      TabOrder = 1
    end
    object ButtonLoad: TcxButton [4]
      Left = 11
      Top = 11
      Width = 75
      Height = 25
      Action = ActionImport
      TabOrder = 0
    end
    inherited LayoutItemButtonOK: TdxLayoutItem
      Visible = False
      Enabled = False
      Index = 3
    end
    inherited LayoutItemButtonCancel: TdxLayoutItem
      Visible = False
      Enabled = False
      Index = 4
    end
    inherited LayoutGroupButtons: TdxLayoutGroup
      ItemIndex = 2
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = LayoutGroupButtons
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = ButtonClose
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = LayoutGroupButtons
      CaptionOptions.Visible = False
      Control = ButtonSaveAs
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = LayoutGroupButtons
      CaptionOptions.Visible = False
      Control = ButtonLoad
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
  inherited LayoutControlHeader: TdxLayoutControl
    Width = 702
    Visible = False
    ExplicitWidth = 702
    inherited LabelHeader: TcxLabel
      ExplicitWidth = 680
      Width = 680
    end
  end
  inherited PanelMain: TPanel
    Width = 702
    Height = 349
    ExplicitTop = 39
    ExplicitWidth = 702
    ExplicitHeight = 349
    inherited LayoutControl: TdxLayoutControl
      Width = 686
      Height = 337
      ExplicitLeft = 8
      ExplicitWidth = 686
      ExplicitHeight = 337
      object GridTM: TcxGrid [0]
        Left = 6
        Top = 6
        Width = 674
        Height = 325
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
      inherited LayoutControlGroup_Root: TdxLayoutGroup
        AlignVert = avClient
      end
      object dxLayoutItem2: TdxLayoutItem
        Parent = LayoutControlGroup_Root
        AlignVert = avClient
        CaptionOptions.Visible = False
        Control = GridTM
        ControlOptions.OriginalHeight = 200
        ControlOptions.OriginalWidth = 250
        ControlOptions.ShowBorder = False
        Index = 0
      end
    end
  end
  inherited ActionList: TActionList
    inherited ActionOK: TAction
      Enabled = False
    end
    inherited ActionCancel: TAction
      Enabled = False
    end
    object ActionDeleteLanguage: TAction
      Category = 'TM'
      Caption = 'Delete language...'
      OnExecute = ActionDeleteLanguageExecute
      OnUpdate = ActionDeleteLanguageUpdate
    end
    object ActionExport: TAction
      Category = 'TM'
      Caption = 'Export...'
      OnExecute = ActionExportExecute
    end
    object ActionImport: TAction
      Category = 'TM'
      Caption = 'Import...'
      OnExecute = ActionImportExecute
    end
  end
  object OpenDialogTMX: TOpenDialog
    Filter = '|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 120
    Top = 224
  end
  object SaveDialogTMX: TSaveDialog
    Filter = '|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 232
    Top = 224
  end
  object GridPopupMenu: TcxGridPopupMenu
    Grid = GridTM
    PopupMenus = <
      item
        GridView = GridTMDBTableView
        HitTypes = [gvhtColumnHeader]
        Index = 0
        OnPopup = GridPopupMenuPopupMenus0Popup
        PopupMenu = PopupMenuHeader
      end>
    Left = 396
    Top = 115
  end
  object PopupMenuHeader: TPopupMenu
    Left = 396
    Top = 159
    object MenuItemDeleteLanguage: TMenuItem
      Action = ActionDeleteLanguage
    end
  end
  object TaskDialogOpen: TTaskDialog
    Buttons = <
      item
        Caption = 'Replace'
        Default = True
        CommandLinkHint = 'Close current TM and open the specified file instead'
        ModalResult = 100
      end
      item
        Caption = 'Merge'
        CommandLinkHint = 'Add the translations to the current TM'
        ModalResult = 101
      end>
    Caption = 'Open Translation Memory'
    CommonButtons = [tcbCancel]
    Flags = [tfAllowDialogCancellation, tfUseCommandLinksNoIcon]
    RadioButtons = <>
    Text = 
      'Do you want to merge the selected file into your existing transl' +
      'ation memory?'
    Title = 'Replace or Merge Translation Memory'
    Left = 128
    Top = 135
  end
end
