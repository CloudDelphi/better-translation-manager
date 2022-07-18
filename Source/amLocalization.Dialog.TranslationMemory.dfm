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
  TextHeight = 15
  inherited LayoutControlButtons: TdxLayoutControl
    Top = 381
    Width = 702
    ExplicitTop = 381
    ExplicitWidth = 702
    inherited ButtonOK: TcxButton
      Left = 532
      TabOrder = 3
      ExplicitLeft = 532
    end
    inherited ButtonCancel: TcxButton
      Left = 614
      TabOrder = 4
      ExplicitLeft = 614
    end
    object ButtonClose: TcxButton [2]
      Left = 450
      Top = 13
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Close'
      ModalResult = 8
      TabOrder = 2
    end
    object ButtonSaveAs: TcxButton [3]
      Left = 95
      Top = 13
      Width = 75
      Height = 25
      Action = ActionExport
      TabOrder = 1
    end
    object ButtonLoad: TcxButton [4]
      Left = 13
      Top = 13
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
  end
  inherited LayoutControl: TdxLayoutControl
    Width = 702
    Height = 336
    ExplicitTop = 45
    ExplicitWidth = 702
    ExplicitHeight = 340
    object GridTM: TcxGrid [0]
      Left = 7
      Top = 102
      Width = 688
      Height = 227
      TabOrder = 2
      object GridTMDBTableView: TcxGridDBTableView
        Navigator.Buttons.CustomButtons = <>
        ScrollbarAnnotations.CustomAnnotations = <>
        OnCellDblClick = GridTMDBTableViewCellDblClick
        OnCustomDrawCell = GridTMDBTableViewCustomDrawCell
        OnInitEdit = GridTMDBTableViewInitEdit
        DataController.DataSource = DataSourceTranslationMemory
        DataController.Options = [dcoAnsiSort, dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding]
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        DataController.OnFilterRecord = GridTMDBTableViewDataControllerFilterRecord
        OptionsBehavior.CellHints = True
        OptionsBehavior.FocusCellOnTab = True
        OptionsBehavior.GoToNextCellOnEnter = True
        OptionsBehavior.IncSearch = True
        OptionsBehavior.FixedGroups = True
        OptionsBehavior.ImmediateEditor = False
        OptionsCustomize.ColumnFiltering = False
        OptionsCustomize.ColumnHiding = True
        OptionsCustomize.ColumnsQuickCustomization = True
        OptionsData.Deleting = False
        OptionsData.Inserting = False
        OptionsSelection.MultiSelect = True
        OptionsSelection.InvertSelect = False
        OptionsSelection.UnselectFocusedRecordOnExit = False
        OptionsView.CellEndEllipsis = True
        OptionsView.GroupByBox = False
        OptionsView.HeaderEndEllipsis = True
        OptionsView.Indicator = True
        Styles.OnGetContentStyle = GridTMDBTableViewStylesGetContentStyle
        Styles.Selection = DataModuleMain.StyleSelected
        OnColumnPosChanged = GridTMDBTableViewColumnPosChanged
      end
      object GridTMLevel: TcxGridLevel
        GridView = GridTMDBTableView
      end
    end
    object ComboBoxLanguages: TcxComboBox [1]
      Left = 83
      Top = 53
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = ComboBoxLanguagesPropertiesChange
      Style.HotTrack = False
      TabOrder = 0
      Width = 162
    end
    object ComboBoxOptions: TcxCheckComboBox [2]
      Left = 320
      Top = 53
      RepositoryItem = DataModuleMain.EditRepositoryCheckComboBoxNormalization
      Properties.Items = <>
      Properties.OnChange = ComboBoxOptionsPropertiesChange
      Style.HotTrack = False
      TabOrder = 1
      Width = 168
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
      Index = 1
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = LayoutGroupDuplicates
      CaptionOptions.Text = 'Language:'
      Control = ComboBoxLanguages
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 162
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = LayoutGroupDuplicates
      CaptionOptions.Text = 'Options:'
      Control = ComboBoxOptions
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 168
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object LayoutGroupDuplicates: TdxLayoutGroup
      Parent = LayoutControlGroup_Root
      CaptionOptions.Text = '&View duplicates'
      Visible = False
      ButtonOptions.ShowExpandButton = True
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      Index = 0
      Buttons = <
        item
          Glyph.SourceDPI = 96
          Glyph.Data = {
            89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
            610000001D744558745469746C6500436C6F73653B457869743B426172733B52
            6962626F6E3B4603B9E8000002AD49444154785E85934B4C546714C77FF73232
            0C041DC2237644C368F109868D98A0290BDA60E2DE9526921856C3C2A08C2E1A
            A336A64DB48D81A8892D6E241A7CA20B1530261849DA4D47596818082F015118
            0698B973DFB7773EB1333BEE97FFCD3927DFF97DE77B1CE9E3AF17293FF3B30C
            48426B7D9939765A1290337AE1DC4B8F2CD73B0E802306C24CEB9BE7ACFA6206
            86650FECFCE55283079025DBAE2F3D5CB79A94B58E206692C5CF1606933DAF7F
            006451BA69986099A8C3C38C3C7846D791D3BCB97413351A151AFCADD38D8589
            DE7F412A3A426A388AC801D92300A689E36A2A1225129923D8D1C9FC9387FCDD
            D587244BC4037BDC581B91AB97716C93F2AA6D989A004802A0AF28382EF19F3B
            FD34F4F7515852C4E2AE20FFFED10140CDC9101B0ABD54B4FFCEAB1F7F62D3F9
            0AB4643203589E9C469F9926B8BF8AF947DD94B5849072246A5A43E040913F9F
            BC75394C3DEE2658BB9BD4F07BE2E3B35915A83AFAA719766CCDE7C3403F43C8
            6C6D6EC65FE8C391C0B660E8FA359203BD546EF7A34E4C60A49C2C8066602514
            B4D94F98F12596150DDB721043DC84CC4A3A168FA14FA75CA085A17D05885B30
            D280548AE8FB39947D8D048E3661D936DE5C99BC5C8FB0BF3BD684527B88D1D1
            18B6AAE22E9A5D8186954C105B50680C87503D3EF2BDEE9E6FFD0940F9F11380
            8FFA700BBD4FEEB2A94842D73D990A74DDC45C49B2A37A336FDB4E5122AB8C5F
            6FE7CBBDDB7C763571A39D328FCEBB702BDF6F2FC352750C4DCFBC0343750189
            243E3381131BE369DD018A4BD71328CE136730D7D3CDD3CEBFD8B2A518AF99C0
            4C030CFEDF82EBE802602C2CE22DC8676F7500633981E2DE0C1694FAFD6C2C29
            419D8FA12515B06C4C43461080822B6595CF7D927410475A3DF9ACA6B0859FDD
            50C2566C7BF0ECD258A3E846200FF066B5AAB4462B3B800628FF0122CC6063F4
            5F96130000000049454E44AE426082}
          OnClick = LayoutGroupDuplicatesButton0Click
        end>
    end
    object dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Parent = LayoutGroupDuplicates
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      CaptionOptions.Text = 'Empty Space Item'
      Index = 1
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
      Category = 'Language'
      Caption = 'Delete language...'
      OnExecute = ActionDeleteLanguageExecute
      OnUpdate = ActionDeleteLanguageUpdate
    end
    object ActionExport: TAction
      Category = 'TM'
      Caption = 'Export...'
      OnExecute = ActionExportExecute
      OnUpdate = ActionExportUpdate
    end
    object ActionImport: TAction
      Category = 'TM'
      Caption = 'Import...'
      OnExecute = ActionImportExecute
      OnUpdate = ActionImportUpdate
    end
    object ActionViewDuplicates: TAction
      Category = 'Language'
      Caption = 'View duplicates'
      OnExecute = ActionViewDuplicatesExecute
      OnUpdate = ActionViewDuplicatesUpdate
    end
    object ActionRowInsert: TAction
      Category = 'Grid'
      Caption = 'Insert row'
      ShortCut = 45
      OnExecute = ActionRowInsertExecute
      OnUpdate = ActionRowInsertUpdate
    end
    object ActionRowDelete: TAction
      Category = 'Grid'
      Caption = 'Delete row(s)...'
      ShortCut = 16430
      OnExecute = ActionRowDeleteExecute
      OnUpdate = ActionRowDeleteUpdate
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
      end
      item
        GridView = GridTMDBTableView
        HitTypes = [gvhtCell, gvhtRecord]
        Index = 1
        PopupMenu = PopupMenuGrid
      end>
    Left = 396
    Top = 115
  end
  object PopupMenuHeader: TPopupMenu
    Left = 396
    Top = 159
    object Findduplicates1: TMenuItem
      Action = ActionViewDuplicates
    end
    object N1: TMenuItem
      Caption = '-'
    end
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
  object DataSourceTranslationMemory: TDataSource
    Left = 76
    Top = 63
  end
  object StyleRepository: TcxStyleRepository
    Left = 340
    Top = 295
    PixelsPerInch = 96
    object StyleDuplicateOdd: TcxStyle
      AssignedValues = [svColor]
      Color = 15918295
    end
    object StyleDuplicateEven: TcxStyle
      AssignedValues = [svColor]
      Color = clWindow
    end
    object StyleDuplicate: TcxStyle
      AssignedValues = [svColor]
      Color = 15389113
    end
  end
  object PopupMenuGrid: TPopupMenu
    Left = 396
    Top = 207
    object Insertrow1: TMenuItem
      Action = ActionRowInsert
    end
    object Deleterows1: TMenuItem
      Action = ActionRowDelete
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Viewduplicates1: TMenuItem
      Action = ActionViewDuplicates
    end
  end
end
