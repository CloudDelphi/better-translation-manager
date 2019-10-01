object FormMain: TFormMain
  Left = 0
  Top = 0
  Action = ActionMain
  Caption = 'Better Translation Manager'
  ClientHeight = 643
  ClientWidth = 964
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  ShowHint = True
  OnClick = ActionMainExecute
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShortCut = FormShortCut
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object RibbonMain: TdxRibbon
    Left = 0
    Top = 0
    Width = 964
    Height = 158
    ApplicationButton.Visible = False
    BarManager = BarManager
    Style = rs2016
    ColorSchemeAccent = rcsaBlue
    ColorSchemeName = 'Office2016Colorful'
    QuickAccessToolbar.Toolbar = BarManagerBarQuickAccess
    SupportNonClientDrawing = True
    Contexts = <>
    TabAreaSearchToolbar.Visible = False
    TabAreaToolbar.Toolbar = BarManagerBarFeedback
    TabOrder = 0
    TabStop = False
    object RibbonTabMain: TdxRibbonTab
      Active = True
      Caption = 'Main'
      Groups = <
        item
          ToolbarName = 'BarManagerBarFile'
        end
        item
          ToolbarName = 'BarManagerBarProject'
        end
        item
          ToolbarName = 'BarManagerBarLanguage'
        end>
      KeyTip = 'F'
      Index = 0
    end
    object RibbonTabEdit: TdxRibbonTab
      Caption = 'Edit'
      Groups = <
        item
          ToolbarName = 'BarManagerBarClipboard'
        end
        item
          ToolbarName = 'BarManagerBarFind'
        end
        item
          ToolbarName = 'BarManagerBarProofing'
        end
        item
          ToolbarName = 'BarManagerBarMark'
        end>
      Index = 1
    end
    object RibbonTabTranslation: TdxRibbonTab
      Caption = 'Translation'
      Groups = <
        item
          ToolbarName = 'BarManagerBarLanguage'
        end
        item
          ToolbarName = 'BarManagetBarTranslationStatus'
        end
        item
          ToolbarName = 'BarManagetBarTranslationState'
        end
        item
          ToolbarName = 'BarManagerBarMachineTranslation'
        end>
      KeyTip = 'T'
      Index = 2
    end
    object RibbonTabTools: TdxRibbonTab
      Caption = 'Tools'
      Groups = <
        item
          ToolbarName = 'BarManagerBarMigrate'
        end
        item
          ToolbarName = 'BarManagerBarExport'
        end>
      Index = 3
    end
  end
  object StatusBar: TdxRibbonStatusBar
    Left = 0
    Top = 620
    Width = 964
    Height = 23
    Images = DataModuleMain.ImageListSmall
    Panels = <
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        PanelStyle.AutoHint = True
        Fixed = False
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        PanelStyle.ImageIndex = 53
        Visible = False
        Width = 20
        OnClick = StatusBarPanels1Click
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        Width = 20
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        Width = 220
      end>
    Ribbon = RibbonMain
    OnHint = StatusBarHint
    ParentFont = True
    OnMouseDown = StatusBarMouseDown
    OnMouseMove = StatusBarMouseMove
  end
  object SplitterTreeLists: TcxSplitter
    Left = 240
    Top = 158
    Width = 4
    Height = 462
    HotZoneClassName = 'TcxSimpleStyle'
    ResizeUpdate = True
    ExplicitTop = 125
    ExplicitHeight = 495
  end
  object PanelModules: TPanel
    Left = 0
    Top = 158
    Width = 240
    Height = 462
    Align = alLeft
    BevelOuter = bvNone
    FullRepaint = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 3
    ExplicitTop = 125
    ExplicitHeight = 495
    object TreeListModules: TcxTreeList
      Left = 0
      Top = 0
      Width = 240
      Height = 403
      Align = alClient
      Bands = <
        item
          FixedKind = tlbfLeft
          Options.Moving = False
          Options.OnlyOwnColumns = True
        end>
      Images = DataModuleMain.ImageListTree
      Navigator.Buttons.CustomButtons = <>
      OptionsBehavior.CellHints = True
      OptionsBehavior.ImmediateEditor = False
      OptionsBehavior.ExpandOnDblClick = False
      OptionsBehavior.RecordScrollMode = rsmByRecord
      OptionsCustomizing.BandCustomizing = False
      OptionsCustomizing.BandMoving = False
      OptionsCustomizing.ColumnFiltering = bTrue
      OptionsCustomizing.ColumnVertSizing = False
      OptionsCustomizing.StackedColumns = False
      OptionsData.AnsiSort = True
      OptionsData.CaseInsensitive = True
      OptionsData.Deleting = False
      OptionsSelection.MultiSelect = True
      OptionsView.CellEndEllipsis = True
      OptionsView.ColumnAutoWidth = True
      OptionsView.DynamicIndent = True
      OptionsView.FixedSeparatorWidth = 1
      OptionsView.GridLineColor = 16050401
      OptionsView.GridLines = tlglHorz
      OptionsView.ShowRoot = False
      OptionsView.TreeLineStyle = tllsNone
      PopupMenu = PopupMenuTree
      Styles.Background = DataModuleMain.StyleDefault
      Styles.Inactive = DataModuleMain.StyleInactive
      Styles.Selection = DataModuleMain.StyleSelected
      Styles.OnGetContentStyle = TreeListModulesStylesGetContentStyle
      Styles.Indicator = DataModuleMain.StyleDefault
      Styles.UseOddEvenStyles = bFalse
      TabOrder = 0
      OnDblClick = TreeListDblClick
      OnEnter = TreeListModulesEnter
      OnExit = TreeListModulesExit
      OnFocusedNodeChanged = TreeListModulesFocusedNodeChanged
      OnGetCellHint = TreeListGetCellHint
      OnGetNodeImageIndex = TreeListModulesGetNodeImageIndex
      OnSelectionChanged = TreeListModulesSelectionChanged
      ExplicitHeight = 436
      object TreeListColumnModuleName: TcxTreeListColumn
        BestFitMaxWidth = 200
        Caption.AlignVert = vaTop
        Caption.Text = 'Module'
        DataBinding.ValueType = 'String'
        Options.Editing = False
        Options.Filtering = False
        Options.Focusing = False
        Width = 160
        Position.ColIndex = 0
        Position.RowIndex = 0
        Position.BandIndex = 0
        SortOrder = soAscending
        SortIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object TreeListColumnModuleStatus: TcxTreeListColumn
        PropertiesClassName = 'TcxImageComboBoxProperties'
        Properties.ImmediatePost = True
        Properties.ImmediateUpdateText = True
        Properties.Items = <
          item
            Description = 'Translate'
            ImageIndex = 0
            Value = 0
          end
          item
            Description = 'Hold'
            ImageIndex = 5
            Value = 1
          end
          item
            Description = 'Don'#39't translate'
            ImageIndex = 2
            Value = 2
          end>
        Properties.OnEditValueChanged = TreeListColumnModuleStatusPropertiesEditValueChanged
        Caption.Text = 'Status'
        DataBinding.ValueType = 'Integer'
        Width = 70
        Position.ColIndex = 1
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
    end
    object LayoutControlModules: TdxLayoutControl
      Left = 0
      Top = 403
      Width = 240
      Height = 59
      Align = alBottom
      TabOrder = 1
      AutoSize = True
      LayoutLookAndFeel = DataModuleMain.LayoutSkinLookAndFeel
      ExplicitTop = 436
      object LabelCountTranslated: TcxLabel
        Left = 66
        Top = 6
        AutoSize = False
        Caption = '0'
        ParentFont = False
        Style.HotTrack = False
        Style.TransparentBorder = False
        Properties.Alignment.Horz = taRightJustify
        Properties.ShowAccelChar = False
        Transparent = True
        Height = 14
        Width = 60
        AnchorX = 126
      end
      object LabelCountPending: TcxLabel
        Left = 66
        Top = 20
        AutoSize = False
        Caption = '0'
        ParentFont = False
        Style.HotTrack = False
        Style.TransparentBorder = False
        Properties.Alignment.Horz = taRightJustify
        Properties.ShowAccelChar = False
        Transparent = True
        Height = 14
        Width = 60
        AnchorX = 126
      end
      object LabelCountTranslatedPercent: TcxLabel
        Left = 132
        Top = 6
        Caption = ' '
        ParentFont = False
        Style.HotTrack = False
        Style.TransparentBorder = False
        Properties.Alignment.Horz = taLeftJustify
        Properties.ShowAccelChar = False
        Transparent = True
      end
      object LayoutControlModulesGroup_Root: TdxLayoutGroup
        AlignHorz = ahClient
        AlignVert = avTop
        ButtonOptions.Buttons = <>
        Hidden = True
        ShowBorder = False
        UseIndent = False
        Index = -1
      end
      object dxLayoutItem2: TdxLayoutItem
        Parent = dxLayoutGroup1
        AlignHorz = ahLeft
        CaptionOptions.ShowAccelChar = False
        CaptionOptions.Text = 'Translated:'
        Control = LabelCountTranslated
        ControlOptions.AlignHorz = ahLeft
        ControlOptions.OriginalHeight = 14
        ControlOptions.OriginalWidth = 60
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutItem1: TdxLayoutItem
        Parent = LayoutControlModulesGroup_Root
        AlignHorz = ahLeft
        CaptionOptions.ShowAccelChar = False
        CaptionOptions.Text = 'Pending:'
        Control = LabelCountPending
        ControlOptions.AlignHorz = ahLeft
        ControlOptions.OriginalHeight = 14
        ControlOptions.OriginalWidth = 60
        ControlOptions.ShowBorder = False
        Index = 1
      end
      object dxLayoutItem3: TdxLayoutItem
        Parent = dxLayoutGroup1
        AlignHorz = ahLeft
        CaptionOptions.Visible = False
        Control = LabelCountTranslatedPercent
        ControlOptions.OriginalHeight = 13
        ControlOptions.OriginalWidth = 3
        ControlOptions.ShowBorder = False
        Index = 1
      end
      object dxLayoutGroup1: TdxLayoutGroup
        Parent = LayoutControlModulesGroup_Root
        CaptionOptions.Visible = False
        ButtonOptions.Buttons = <>
        ItemIndex = 1
        LayoutDirection = ldHorizontal
        ShowBorder = False
        Index = 0
      end
    end
  end
  object TreeListItems: TcxVirtualTreeList
    Left = 244
    Top = 158
    Width = 720
    Height = 462
    Align = alClient
    Bands = <
      item
        Options.OnlyOwnColumns = True
      end>
    Images = DataModuleMain.ImageListTree
    Navigator.Buttons.CustomButtons = <>
    OptionsBehavior.CellHints = True
    OptionsBehavior.GoToNextCellOnEnter = True
    OptionsBehavior.GoToNextCellOnTab = True
    OptionsBehavior.ImmediateEditor = False
    OptionsBehavior.ExpandOnDblClick = False
    OptionsBehavior.FocusCellOnCycle = True
    OptionsBehavior.RecordScrollMode = rsmByRecord
    OptionsCustomizing.BandCustomizing = False
    OptionsCustomizing.BandMoving = False
    OptionsCustomizing.ColumnFiltering = bTrue
    OptionsCustomizing.ColumnsQuickCustomization = True
    OptionsCustomizing.ColumnsQuickCustomizationShowCommands = False
    OptionsCustomizing.ColumnVertSizing = False
    OptionsCustomizing.StackedColumns = False
    OptionsData.AnsiSort = True
    OptionsData.CaseInsensitive = True
    OptionsData.Deleting = False
    OptionsData.CheckHasChildren = False
    OptionsSelection.MultiSelect = True
    OptionsView.CellEndEllipsis = True
    OptionsView.Buttons = False
    OptionsView.FixedSeparatorWidth = 0
    OptionsView.GridLineColor = 16050401
    OptionsView.GridLines = tlglHorz
    OptionsView.Indicator = True
    OptionsView.IndicatorWidth = 50
    OptionsView.ShowRoot = False
    OptionsView.TreeLineStyle = tllsNone
    PopupMenu = PopupMenuTree
    StateImages = DataModuleMain.ImageListState
    Styles.Background = DataModuleMain.StyleDefault
    Styles.Inactive = DataModuleMain.StyleInactive
    Styles.Selection = DataModuleMain.StyleSelected
    Styles.OnGetContentStyle = TreeListItemsStylesGetContentStyle
    Styles.Indicator = DataModuleMain.StyleDefault
    Styles.UseOddEvenStyles = bFalse
    TabOrder = 4
    OnClick = TreeListItemsClick
    OnCustomDrawDataCell = TreeListItemsCustomDrawDataCell
    OnCustomDrawIndicatorCell = TreeListItemsCustomDrawIndicatorCell
    OnDblClick = TreeListDblClick
    OnEditing = TreeListItemsEditing
    OnEditValueChanged = TreeListItemsEditValueChanged
    OnEnter = TreeListModulesEnter
    OnExit = TreeListModulesExit
    OnGetCellHint = TreeListGetCellHint
    OnGetNodeImageIndex = TreeListItemsGetNodeImageIndex
    OnMouseDown = TreeListItemsMouseDown
    OnMouseMove = TreeListItemsMouseMove
    ExplicitTop = 125
    ExplicitHeight = 495
    object TreeListColumnItemName: TcxTreeListColumn
      BestFitMaxWidth = 300
      Caption.AlignVert = vaTop
      Caption.Text = 'Element'
      DataBinding.ValueType = 'String'
      Options.Editing = False
      Options.Filtering = False
      Options.Focusing = False
      Width = 198
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 0
      SortOrder = soAscending
      SortIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object TreeListColumnType: TcxTreeListColumn
      Visible = False
      BestFitMaxWidth = 200
      Caption.AlignVert = vaTop
      Caption.Text = 'Type'
      DataBinding.ValueType = 'String'
      Options.Editing = False
      Options.Focusing = False
      Width = 100
      Position.ColIndex = 7
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object TreeListColumnValueName: TcxTreeListColumn
      BestFitMaxWidth = 200
      Caption.AlignVert = vaTop
      Caption.Text = 'Name'
      DataBinding.ValueType = 'String'
      Options.Editing = False
      Options.Focusing = False
      Width = 125
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 0
      SortOrder = soAscending
      SortIndex = 1
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object TreeListColumnID: TcxTreeListColumn
      PropertiesClassName = 'TcxLabelProperties'
      Properties.Alignment.Horz = taRightJustify
      Properties.ShowAccelChar = False
      Visible = False
      Caption.AlignVert = vaTop
      Caption.Text = 'ID'
      DataBinding.ValueType = 'String'
      Options.Editing = False
      Options.Filtering = False
      Options.Focusing = False
      Width = 100
      Position.ColIndex = 8
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object TreeListColumnStatus: TcxTreeListColumn
      PropertiesClassName = 'TcxImageComboBoxProperties'
      Properties.Items = <
        item
          Description = 'Translate'
          ImageIndex = 0
          Value = 0
        end
        item
          Description = 'Hold'
          ImageIndex = 5
          Value = 1
        end
        item
          Description = 'Don'#39't translate'
          ImageIndex = 2
          Value = 2
        end>
      Properties.OnEditValueChanged = TreeListColumnStatusPropertiesEditValueChanged
      Caption.AlignVert = vaTop
      Caption.Text = 'Status'
      DataBinding.ValueType = 'Integer'
      Width = 70
      Position.ColIndex = 2
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object TreeListColumnEffectiveStatus: TcxTreeListColumn
      PropertiesClassName = 'TcxImageComboBoxProperties'
      Properties.Items = <
        item
          Description = 'Translate'
          ImageIndex = 0
          Value = 0
        end
        item
          Description = 'Hold'
          ImageIndex = 5
          Value = 1
        end
        item
          Description = 'Don'#39't translate'
          ImageIndex = 2
          Value = 2
        end>
      Properties.ReadOnly = True
      Visible = False
      Caption.AlignVert = vaTop
      Caption.Text = 'Effective status'
      DataBinding.ValueType = 'Integer'
      Options.Focusing = False
      Width = 90
      Position.ColIndex = 3
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object TreeListColumnState: TcxTreeListColumn
      PropertiesClassName = 'TcxImageComboBoxProperties'
      Properties.Items = <
        item
          Description = 'Obsolete'
          ImageIndex = 7
          Value = 0
        end
        item
          Description = 'Pending'
          ImageIndex = 6
          Value = 1
        end
        item
          Description = 'Proposed'
          ImageIndex = 3
          Value = 2
        end
        item
          Description = 'Translated'
          ImageIndex = 4
          Value = 3
        end>
      Properties.OnEditValueChanged = TreeListColumnStatePropertiesEditValueChanged
      Caption.AlignVert = vaTop
      Caption.Text = 'State'
      DataBinding.ValueType = 'String'
      Width = 70
      Position.ColIndex = 5
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object TreeListColumnSource: TcxTreeListColumn
      PropertiesClassName = 'TcxMemoProperties'
      BestFitMaxWidth = 400
      Caption.AlignVert = vaTop
      Caption.Text = 'Source text'
      DataBinding.ValueType = 'String'
      Options.Editing = False
      Options.Filtering = False
      Options.Focusing = False
      Width = 120
      Position.ColIndex = 4
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object TreeListColumnTarget: TcxTreeListColumn
      PropertiesClassName = 'TcxButtonEditProperties'
      Properties.Buttons = <
        item
          Default = True
          Hint = 'Open in text editor'
          Kind = bkEllipsis
        end>
      Properties.OnButtonClick = TreeListColumnTargetPropertiesButtonClick
      BestFitMaxWidth = 400
      Caption.AlignVert = vaTop
      Caption.Text = 'Translation'
      DataBinding.ValueType = 'String'
      Options.Filtering = False
      Width = 120
      Position.ColIndex = 6
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
  end
  object BarManager: TdxBarManager
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    CanCustomize = False
    Categories.Strings = (
      'Default')
    Categories.ItemsVisibles = (
      2)
    Categories.Visibles = (
      True)
    ImageOptions.Images = DataModuleMain.ImageListSmall
    ImageOptions.LargeImages = DataModuleMain.ImageListLarge
    ImageOptions.UseLargeImagesForLargeIcons = True
    ImageOptions.UseLeftBottomPixelAsTransparent = False
    MenusShowRecentItemsFirst = False
    PopupMenuLinks = <>
    ShowShortCutInHint = True
    UseSystemFont = True
    Left = 824
    Top = 44
    PixelsPerInch = 96
    object BarManagerBarFile: TdxBar
      Caption = 'File'
      CaptionButtons = <>
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 408
      FloatTop = 320
      FloatClientWidth = 85
      FloatClientHeight = 135
      ItemLinks = <
        item
          Visible = True
          ItemName = 'BarButtonOpenProject'
        end
        item
          BeginGroup = True
          ViewLevels = [ivlLargeControlOnly, ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'BarButtonNewProject'
        end
        item
          Visible = True
          ItemName = 'BarButtonSaveProject'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object BarManagerBarProject: TdxBar
      Caption = 'Project'
      CaptionButtons = <>
      DockedLeft = 172
      DockedTop = 0
      FloatLeft = 818
      FloatTop = 2
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'BarButtonUpdateProject'
        end
        item
          Visible = True
          ItemName = 'BarButtonBuildProject'
        end
        item
          Visible = True
          ItemName = 'BarButtonPurgeProject'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object BarManagerBarMigrate: TdxBar
      Caption = 'Migration'
      CaptionButtons = <>
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 818
      FloatTop = 2
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton2'
        end
        item
          Visible = True
          ItemName = 'dxBarButton21'
        end
        item
          Visible = True
          ItemName = 'dxBarButton22'
        end
        item
          Visible = True
          ItemName = 'dxBarSubItem1'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object BarManagerBarLanguage: TdxBar
      Caption = 'Language'
      CaptionButtons = <
        item
          Hint = 'Select available target languages'
          OnClick = BarManagerBarLanguageCaptionButtons0Click
        end>
      DockedLeft = 347
      DockedTop = 0
      FloatLeft = 818
      FloatTop = 2
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'BarEditItemSourceLanguage'
        end
        item
          Visible = True
          ItemName = 'BarEditItemTargetLanguage'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object BarManagetBarTranslationStatus: TdxBar
      Caption = 'Status'
      CaptionButtons = <>
      DockedLeft = 159
      DockedTop = 0
      FloatLeft = 818
      FloatTop = 2
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'BarButtonStatusTranslate'
        end
        item
          Visible = True
          ItemName = 'BarButtonStatusDontTranslate'
        end
        item
          Visible = True
          ItemName = 'BarButtonStatusHold'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object BarManagetBarTranslationState: TdxBar
      Caption = 'State'
      CaptionButtons = <>
      DockedLeft = 274
      DockedTop = 0
      FloatLeft = 818
      FloatTop = 2
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'BarButtonStatePropose'
        end
        item
          Visible = True
          ItemName = 'BarButtonStateAccept'
        end
        item
          Visible = True
          ItemName = 'BarButtonStateReject'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object BarManagerBarProofing: TdxBar
      Caption = 'Validation'
      CaptionButtons = <
        item
          Hint = 'Settings'
          OnClick = BarManagerBarProofingCaptionButtons0Click
        end>
      DockedLeft = 286
      DockedTop = 0
      FloatLeft = 824
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'BarButtonSpellCheck'
        end
        item
          Visible = True
          ItemName = 'dxBarButton10'
        end
        item
          Visible = True
          ItemName = 'dxBarButton9'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'dxBarButton26'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object BarManagerBarClipboard: TdxBar
      Caption = 'Clipboard'
      CaptionButtons = <>
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 824
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarLargeButton5'
        end
        item
          Visible = True
          ItemName = 'dxBarButton11'
        end
        item
          Visible = True
          ItemName = 'dxBarButton12'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object BarManagerBarFind: TdxBar
      Caption = 'Find'
      CaptionButtons = <>
      DockedLeft = 110
      DockedTop = 0
      FloatLeft = 824
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton13'
        end
        item
          Visible = True
          ItemName = 'dxBarButton25'
        end
        item
          Visible = True
          ItemName = 'dxBarButton14'
        end
        item
          Visible = True
          ItemName = 'BarButtonGotoNext'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object BarManagerBarMachineTranslation: TdxBar
      Caption = 'Machine Translation'
      CaptionButtons = <>
      DockedLeft = 414
      DockedTop = 0
      FloatLeft = 998
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'BarButtonAutoTranslate'
        end
        item
          Visible = True
          ItemName = 'BarButtonTM'
        end
        item
          Visible = True
          ItemName = 'BarButtonTMAdd'
        end
        item
          Visible = True
          ItemName = 'BarButtonTMLookup'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object BarManagerBarExport: TdxBar
      Caption = 'Export'
      CaptionButtons = <>
      DockedLeft = 216
      DockedTop = 0
      FloatLeft = 998
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton17'
        end
        item
          Visible = True
          ItemName = 'dxBarButton20'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object BarManagerBarMark: TdxBar
      Caption = 'Mark'
      CaptionButtons = <>
      DockedLeft = 567
      DockedTop = 0
      FloatLeft = 998
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'ButtonItemBookmark'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object BarManagerBarQuickAccess: TdxBar
      AllowClose = False
      AllowCustomizing = False
      AllowQuickCustomizing = False
      AllowReset = False
      Caption = 'Quick Access'
      CaptionButtons = <>
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 998
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      Hidden = True
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton34'
        end
        item
          Visible = True
          ItemName = 'dxBarButton35'
        end>
      OneOnRow = True
      Row = 0
      ShowMark = False
      SizeGrip = False
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object BarManagerBarFeedback: TdxBar
      Caption = 'Feedback'
      CaptionButtons = <>
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 998
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'BarEditItemSearch'
        end
        item
          Visible = True
          ItemName = 'BarButtonFeedback'
        end>
      OneOnRow = True
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object BarButtonOpenProject: TdxBarLargeButton
      Action = ActionProjectOpen
      Category = 0
      ButtonStyle = bsDropDown
      DropDownMenu = PopupMenuRecentFiles
    end
    object BarButtonNewProject: TdxBarLargeButton
      Action = ActionProjectNew
      Category = 0
    end
    object BarButtonSaveProject: TdxBarButton
      Action = ActionProjectSave
      Category = 0
      LargeImageIndex = 2
    end
    object BarButtonUpdateProject: TdxBarLargeButton
      Action = ActionProjectUpdate
      Category = 0
    end
    object BarButtonBuildProject: TdxBarLargeButton
      Action = ActionBuild
      Category = 0
    end
    object dxBarButton2: TdxBarButton
      Action = ActionImportXLIFF
      Category = 0
    end
    object BarEditItemSourceLanguage: TcxBarEditItem
      Align = iaRight
      Caption = '&Source:'
      Category = 0
      Hint = 'Source language'
      Visible = ivAlways
      PropertiesClassName = 'TcxExtLookupComboBoxProperties'
      Properties.OnEditValueChanged = BarEditItemSourceLanguagePropertiesEditValueChanged
      RepositoryItem = DataModuleMain.EditRepositoryComboBoxItemLanguage
    end
    object BarEditItemTargetLanguage: TcxBarEditItem
      Align = iaRight
      Caption = 'T&arget:'
      Category = 0
      Hint = 'Target language'
      Visible = ivAlways
      OnEnter = BarEditItemTargetLanguageEnter
      OnExit = BarEditItemTargetLanguageExit
      PropertiesClassName = 'TcxExtLookupComboBoxProperties'
      Properties.OnEditValueChanged = BarEditItemTargetLanguagePropertiesEditValueChanged
      Properties.OnInitPopup = BarEditItemTargetLanguagePropertiesInitPopup
      RepositoryItem = DataModuleMain.EditRepositoryComboBoxItemFilteredTargetLanguage
    end
    object BarButtonPurgeProject: TdxBarButton
      Action = ActionProjectPurge
      Category = 0
    end
    object BarButtonStatusTranslate: TdxBarButton
      Action = ActionStatusTranslate
      Category = 0
      ButtonStyle = bsChecked
      GroupIndex = 1
      Down = True
    end
    object BarButtonStatusDontTranslate: TdxBarButton
      Action = ActionStatusDontTranslate
      Category = 0
      ButtonStyle = bsChecked
      GroupIndex = 1
    end
    object BarButtonStatusHold: TdxBarButton
      Action = ActionStatusHold
      Category = 0
      ButtonStyle = bsChecked
      GroupIndex = 1
    end
    object BarButtonStatePropose: TdxBarButton
      Action = ActionTranslationStatePropose
      Category = 0
    end
    object BarButtonStateAccept: TdxBarButton
      Action = ActionTranslationStateAccept
      Category = 0
    end
    object BarButtonStateReject: TdxBarButton
      Action = ActionTranslationStateReject
      Category = 0
    end
    object BarButtonSpellCheck: TdxBarLargeButton
      Action = ActionProofingCheck
      Category = 0
      LargeImageIndex = 6
    end
    object dxBarButton9: TdxBarButton
      Action = ActionProofingLiveCheck
      Category = 0
      ButtonStyle = bsChecked
    end
    object dxBarButton10: TdxBarButton
      Action = ActionProofingCheckSelected
      Category = 0
    end
    object dxBarLargeButton5: TdxBarLargeButton
      Action = ActionEditPaste
      Category = 0
      LargeImageIndex = 7
    end
    object dxBarButton11: TdxBarButton
      Action = ActionEditCopy
      Category = 0
    end
    object dxBarButton12: TdxBarButton
      Action = ActionEditCut
      Category = 0
    end
    object dxBarButton13: TdxBarButton
      Action = ActionFindSearch
      Category = 0
    end
    object dxBarButton14: TdxBarButton
      Action = ActionFindReplace
      Category = 0
    end
    object BarButtonAutoTranslate: TdxBarLargeButton
      Action = ActionAutomationTranslate
      Category = 0
      ButtonStyle = bsDropDown
      DropDownMenu = PopupMenuTranslateProviders
      LargeImageIndex = 11
    end
    object BarButtonTM: TdxBarButton
      Action = ActionTranslationMemory
      Category = 0
    end
    object BarButtonGotoNext: TdxBarSubItem
      Action = ActionGotoNext
      Category = 0
      LargeImageIndex = 9
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton16'
        end
        item
          Visible = True
          ItemName = 'dxBarButton27'
        end
        item
          Visible = True
          ItemName = 'dxBarSubItem2'
        end
        item
          Visible = True
          ItemName = 'dxBarSubItem3'
        end
        item
          Visible = True
          ItemName = 'ButtonGotoBookmark'
        end>
    end
    object dxBarSubItem1: TdxBarSubItem
      Action = ActionImportFile
      Category = 0
      LargeImageIndex = 10
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton18'
        end
        item
          Visible = True
          ItemName = 'dxBarButton19'
        end>
    end
    object dxBarButton18: TdxBarButton
      Action = ActionImportFileSource
      Category = 0
    end
    object dxBarButton19: TdxBarButton
      Action = ActionImportFileTarget
      Category = 0
    end
    object dxBarButton17: TdxBarButton
      Caption = 'Save to CSV'
      Category = 0
      Enabled = False
      Hint = 'Save to CSV'
      Visible = ivAlways
      ImageIndex = 63
    end
    object dxBarButton20: TdxBarButton
      Caption = 'Save to Excel'
      Category = 0
      Enabled = False
      Hint = 'Save to Excel'
      Visible = ivAlways
      ImageIndex = 62
    end
    object dxBarButton21: TdxBarButton
      Action = ActionImportCSV
      Category = 0
      ImageIndex = 61
    end
    object dxBarButton22: TdxBarButton
      Caption = 'Import from Excel'
      Category = 0
      Enabled = False
      Hint = 'Import from Excel'
      Visible = ivAlways
      ImageIndex = 60
    end
    object BarButtonTMAdd: TdxBarButton
      Action = ActionTranslationMemoryAdd
      Category = 0
    end
    object BarButtonTMLookup: TdxBarButton
      Action = ActionTranslationMemoryTranslate
      Category = 0
    end
    object dxBarButton25: TdxBarButton
      Action = ActionFindNext
      Category = 0
    end
    object dxBarButton26: TdxBarButton
      Action = ActionValidate
      Category = 0
      LargeImageIndex = 13
    end
    object dxBarButton16: TdxBarButton
      Action = ActionGotoNextUntranslated
      Category = 0
    end
    object dxBarButton27: TdxBarButton
      Action = ActionGotoNextWarning
      Category = 0
    end
    object ButtonGotoBookmark: TdxBarButton
      Action = ActionGotoNextBookmark
      Category = 0
      Hint = 'Find bookmarked item'
      ButtonStyle = bsDropDown
      DropDownMenu = PopupMenuBookmark
    end
    object ButtonItemBookmarkAny: TdxBarButton
      Action = ActionGotoBookmarkAny
      Category = 0
    end
    object ButtonItemBookmark: TdxBarButton
      Action = ActionEditMark
      Category = 0
      ButtonStyle = bsCheckedDropDown
      DropDownMenu = PopupMenuBookmark
    end
    object dxBarSubItem2: TdxBarSubItem
      Action = ActionGotoNextStatus
      Category = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton28'
        end
        item
          Visible = True
          ItemName = 'dxBarButton29'
        end
        item
          Visible = True
          ItemName = 'dxBarButton30'
        end>
    end
    object dxBarSubItem3: TdxBarSubItem
      Action = ActionGotoNextState
      Category = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton31'
        end
        item
          Visible = True
          ItemName = 'dxBarButton33'
        end
        item
          Visible = True
          ItemName = 'dxBarButton32'
        end>
    end
    object dxBarButton28: TdxBarButton
      Action = ActionGotoNextStatusTranslate
      Category = 0
      Visible = ivNotInCustomizing
    end
    object dxBarButton29: TdxBarButton
      Action = ActionGotoNextStatusHold
      Category = 0
      Visible = ivNotInCustomizing
    end
    object dxBarButton30: TdxBarButton
      Action = ActionGotoNextStatusDontTranslate
      Category = 0
      Visible = ivNotInCustomizing
    end
    object dxBarButton31: TdxBarButton
      Action = ActionGotoNextStateNew
      Category = 0
      Visible = ivNotInCustomizing
    end
    object dxBarButton32: TdxBarButton
      Action = ActionGotoNextStateUnused
      Category = 0
      Visible = ivNotInCustomizing
    end
    object dxBarButton33: TdxBarButton
      Action = ActionGotoNextStateExisting
      Category = 0
      Visible = ivNotInCustomizing
    end
    object dxBarButton34: TdxBarButton
      Action = ActionSettings
      Category = 0
    end
    object BarButtonFeedbackPositive: TdxBarButton
      Action = ActionFeedbackPositive
      Category = 0
    end
    object BarButtonFeedbackNegative: TdxBarButton
      Action = ActionFeedbackNegative
      Category = 0
    end
    object BarButtonFeedbackHide: TdxBarButton
      Action = ActionFeedbackHide
      Category = 0
      Visible = ivNotInCustomizing
    end
    object BarButtonFeedback: TdxBarSubItem
      Action = ActionFeedback
      Category = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'BarButtonFeedbackPositive'
        end
        item
          Visible = True
          ItemName = 'BarButtonFeedbackNegative'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'BarButtonFeedbackHide'
        end>
    end
    object BarEditItemSearch: TcxBarEditItem
      Caption = 'Search'
      Category = 0
      Hint = 'Search'
      Visible = ivAlways
      PropertiesClassName = 'TdxOfficeSearchBoxProperties'
      Properties.BarManager = BarManager
      Properties.Nullstring = '(search)'
      Properties.Ribbon = RibbonMain
      Properties.SearchSource = RibbonMain
      Properties.ShowResultPaths = True
      Properties.UseNullString = True
    end
    object dxBarButton35: TdxBarButton
      Action = ActionAbout
      Category = 0
      LargeImageIndex = 14
    end
  end
  object SkinController: TdxSkinController
    NativeStyle = False
    ScrollbarMode = sbmClassic
    SkinName = 'UserSkin'
    Left = 748
    Top = 48
  end
  object OpenDialogXLIFF: TOpenDialog
    Filter = 
      'XLIFF files (*.xlf;*.xliff)|*.xlf;*.xliff|Delphi form translatio' +
      'ns (*.dfn)|*.dfn|Delphi resourcestring translations (*.rcn)|*.rc' +
      'n|All supported files (*.xlf;*.xliff;*.dfn;*.rcn)|*.xlf;*.xliff;' +
      '*.dfn;*.rcn|All files (*.*)|*.*'
    FilterIndex = 4
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 344
    Top = 364
  end
  object ActionList: TActionList
    Images = DataModuleMain.ImageListSmall
    Left = 384
    Top = 232
    object ActionProofingCheck: TAction
      Category = 'Validation'
      Caption = 'Spell check'
      Hint = 'Perform spell check on the whole project'
      ImageIndex = 13
      ShortCut = 118
      OnExecute = ActionProofingCheckExecute
      OnUpdate = ActionHasModulesUpdate
    end
    object ActionProjectOpen: TAction
      Category = 'File'
      Caption = 'Open project'
      Hint = 'Open an existing translation project'
      ImageIndex = 0
      ShortCut = 16463
      OnExecute = ActionProjectOpenExecute
    end
    object ActionProofingCheckSelected: TAction
      Category = 'Validation'
      Caption = 'Check selected'
      Hint = 'Perform spell check on the selected items'
      ImageIndex = 13
      ShortCut = 8310
      OnExecute = ActionProofingCheckSelectedExecute
      OnUpdate = ActionHasItemFocusedUpdate
    end
    object ActionProjectNew: TAction
      Category = 'File'
      Caption = 'New project'
      Hint = 'Create a new translation project'
      ImageIndex = 1
      ShortCut = 24654
      OnExecute = ActionProjectNewExecute
    end
    object ActionProjectSave: TAction
      Category = 'File'
      Caption = 'Save...'
      Hint = 'Save the current translation project'
      ImageIndex = 2
      ShortCut = 16467
      OnExecute = ActionProjectSaveExecute
      OnUpdate = ActionProjectSaveUpdate
    end
    object ActionProjectUpdate: TAction
      Category = 'Project'
      Caption = 'Update'
      Hint = 'Update project from source'
      ImageIndex = 3
      ShortCut = 116
      OnExecute = ActionProjectUpdateExecute
      OnUpdate = ActionHasProjectUpdate
    end
    object ActionBuild: TAction
      Category = 'Project'
      Caption = 'Build...'
      Hint = 'Build translated resource module'
      ImageIndex = 4
      ShortCut = 120
      OnExecute = ActionBuildExecute
      OnUpdate = ActionHasProjectUpdate
    end
    object ActionImportXLIFF: TAction
      Category = 'Import'
      Caption = 'Import XLIFF'
      Hint = 
        'Import translation from XLIFF file (used by Delphi amonst others' +
        ')'
      ImageIndex = 5
      OnExecute = ActionImportXLIFFExecute
      OnUpdate = ActionHasProjectUpdate
    end
    object ActionProjectPurge: TAction
      Category = 'Project'
      Caption = 'Purge'
      Hint = 'Remove unused items'
      ImageIndex = 6
      OnExecute = ActionProjectPurgeExecute
      OnUpdate = ActionHasModulesUpdate
    end
    object ActionTranslationStatePropose: TAction
      Category = 'Translation'
      Caption = '&Propose translation'
      ImageIndex = 7
      ShortCut = 32848
      OnExecute = ActionTranslationStateProposeExecute
      OnUpdate = ActionTranslationStateUpdate
    end
    object ActionTranslationStateAccept: TAction
      Category = 'Translation'
      Caption = 'Accept &translation'
      ImageIndex = 8
      ShortCut = 32852
      OnExecute = ActionTranslationStateAcceptExecute
      OnUpdate = ActionTranslationStateUpdate
    end
    object ActionTranslationStateReject: TAction
      Category = 'Translation'
      Caption = '&Reject translation'
      ImageIndex = 9
      ShortCut = 32850
      OnExecute = ActionTranslationStateRejectExecute
      OnUpdate = ActionTranslationStateUpdate
    end
    object ActionStatusTranslate: TAction
      Category = 'Translation'
      Caption = 'Translate'
      ImageIndex = 10
      OnExecute = ActionStatusTranslateExecute
      OnUpdate = ActionStatusTranslateUpdate
    end
    object ActionStatusDontTranslate: TAction
      Category = 'Translation'
      Caption = '&Don'#39't translate'
      ImageIndex = 12
      ShortCut = 32836
      OnExecute = ActionStatusDontTranslateExecute
      OnUpdate = ActionStatusDontTranslateUpdate
    end
    object ActionStatusHold: TAction
      Category = 'Translation'
      Caption = '&Hold'
      ImageIndex = 11
      ShortCut = 32840
      OnExecute = ActionStatusHoldExecute
      OnUpdate = ActionStatusHoldUpdate
    end
    object ActionProofingLiveCheck: TAction
      Category = 'Validation'
      AutoCheck = True
      Caption = 'Live check'
      Hint = 'Enable or disable check-as-you-type spell check'
      ImageIndex = 14
      OnExecute = ActionProofingLiveCheckExecute
      OnUpdate = ActionProofingLiveCheckUpdate
    end
    object ActionEditPaste: TAction
      Category = 'Edit'
      Caption = 'Paste'
      Hint = 'Paste from clipboard'
      ImageIndex = 15
    end
    object ActionEditCopy: TAction
      Category = 'Edit'
      Caption = 'Copy'
      Hint = 'Copy to clipboard'
      ImageIndex = 17
    end
    object ActionEditCut: TAction
      Category = 'Edit'
      Caption = 'Cut'
      Hint = 'Cut to clipboard'
      ImageIndex = 16
    end
    object ActionFindSearch: TAction
      Category = 'Find'
      Caption = 'Find'
      Hint = 'Search for text'
      ImageIndex = 18
      ShortCut = 16454
      OnExecute = ActionFindSearchExecute
      OnUpdate = ActionHasModulesUpdate
    end
    object ActionFindReplace: TAction
      Category = 'Find'
      Caption = 'Find and replace'
      Hint = 'Find and replace text'
      ImageIndex = 19
      ShortCut = 24646
      Visible = False
      OnUpdate = ActionHasModulesUpdate
    end
    object ActionMain: TAction
      Caption = 'Better Translation Manager'
      OnExecute = ActionMainExecute
      OnUpdate = ActionMainUpdate
    end
    object ActionImportFile: TAction
      Category = 'Import'
      Caption = 'Import application'
      Hint = 
        'Update source or target values from arbitrary application or lib' +
        'rary file'
      ImageIndex = 21
      OnExecute = ActionImportFileExecute
      OnUpdate = ActionHasProjectUpdate
    end
    object ActionImportFileSource: TAction
      Category = 'Import'
      Caption = 'Import source language'
      Hint = 'Import values in the source language from an external file'
      OnExecute = ActionImportFileSourceExecute
      OnUpdate = ActionHasProjectUpdate
    end
    object ActionImportFileTarget: TAction
      Category = 'Import'
      Caption = 'Import target language'
      Hint = 'Import translated values from an external file'
      OnExecute = ActionImportFileTargetExecute
      OnUpdate = ActionImportFileTargetUpdate
    end
    object ActionAutomationTranslate: TAction
      Category = 'Automation'
      Caption = 'Auto translate...'
      Hint = 'Perform machine translation on the selected items'
      ImageIndex = 22
      OnExecute = ActionAutomationTranslateExecute
      OnUpdate = ActionAutomationTranslateUpdate
    end
    object ActionTranslationMemory: TAction
      Category = 'Automation'
      Caption = 'Translation Memory'
      Hint = 'Open Translation Memory'
      ImageIndex = 23
      OnExecute = ActionTranslationMemoryExecute
    end
    object ActionTranslationMemoryAdd: TAction
      Category = 'Automation'
      Caption = 'Add to TM'
      Hint = 'Add the selected translations to the Translation Memory'
      ImageIndex = 24
      ShortCut = 24641
      OnExecute = ActionTranslationMemoryAddExecute
      OnUpdate = ActionTranslationMemoryAddUpdate
    end
    object ActionTranslationMemoryTranslate: TAction
      Category = 'Automation'
      Caption = 'Translate from TM'
      Hint = 'Translate the selected items from Translation Memory'
      ImageIndex = 25
      ShortCut = 24660
      OnExecute = ActionTranslationMemoryTranslateExecute
      OnUpdate = ActionTranslationMemoryTranslateUpdate
    end
    object ActionFindNext: TAction
      Category = 'Find'
      Caption = 'Find next'
      Hint = 'Go to next search match'
      ImageIndex = 26
      ShortCut = 114
      OnExecute = ActionFindNextExecute
      OnUpdate = ActionFindNextUpdate
    end
    object ActionGotoNext: TAction
      Category = 'Find'
      Caption = 'Goto...'
      ImageIndex = 20
      ShortCut = 16455
      OnExecute = ActionDummyExecute
      OnUpdate = ActionHasModulesUpdate
    end
    object ActionGotoNextUntranslated: TAction
      Category = 'Find'
      Caption = 'Next untranslated'
      Hint = 'Find the next untranslated item'
      ImageIndex = 10
      ShortCut = 16462
      OnExecute = ActionGotoNextUntranslatedExecute
      OnUpdate = ActionHasModulesUpdate
    end
    object ActionGotoNextWarning: TAction
      Category = 'Find'
      Caption = 'Next warning'
      Hint = 'Find next item that has validation warnings'
      ImageIndex = 53
      ShortCut = 16471
      OnExecute = ActionGotoNextWarningExecute
      OnUpdate = ActionHasModulesUpdate
    end
    object ActionGotoNextBookmark: TAction
      Category = 'Find'
      Caption = 'Bookmark...'
      Hint = 'Find next bookmarked item'
      ImageIndex = 43
      SecondaryShortCuts.Strings = (
        'Shift+F4')
      ShortCut = 115
      OnExecute = ActionGotoNextBookmarkExecute
    end
    object ActionGotoBookmarkAny: TAction
      Tag = -1
      Category = 'Bookmark'
      Caption = 'A&ny bookmark'
      ImageIndex = 49
      OnExecute = ActionGotoBookmarkAnyExecute
    end
    object ActionEditMark: TAction
      Tag = -1
      Category = 'Bookmark'
      Caption = 'Set bookmark'
      ImageIndex = 49
      SecondaryShortCuts.Strings = (
        'Shift+Ctrl+B')
      ShortCut = 16450
      OnExecute = ActionEditMarkExecute
      OnUpdate = ActionEditMarkUpdate
    end
    object ActionValidate: TAction
      Category = 'Validation'
      Caption = '&Validate project'
      ImageIndex = 48
      OnExecute = ActionValidateExecute
      OnUpdate = ActionHasModulesUpdate
    end
    object ActionGotoNextStatus: TAction
      Category = 'Find'
      Caption = 'Next with Status...'
      OnExecute = ActionDummyExecute
      OnUpdate = ActionHasModulesUpdate
    end
    object ActionGotoNextStatusTranslate: TAction
      Category = 'Find'
      Caption = 'Translate'
      ImageIndex = 8
      OnExecute = ActionGotoNextStatusExecute
    end
    object ActionGotoNextStatusHold: TAction
      Tag = 1
      Category = 'Find'
      Caption = 'Hold'
      ImageIndex = 11
      OnExecute = ActionGotoNextStatusExecute
    end
    object ActionGotoNextStatusDontTranslate: TAction
      Tag = 2
      Category = 'Find'
      Caption = 'Don'#39't translate'
      ImageIndex = 12
      OnExecute = ActionGotoNextStatusExecute
    end
    object ActionGotoNextState: TAction
      Category = 'Find'
      Caption = 'Next with State...'
      OnExecute = ActionDummyExecute
      OnUpdate = ActionHasModulesUpdate
    end
    object ActionGotoNextStateNew: TAction
      Category = 'Find'
      Caption = 'New'
      ImageIndex = 50
      OnExecute = ActionGotoNextStateExecute
    end
    object ActionGotoNextStateExisting: TAction
      Tag = 1
      Category = 'Find'
      Caption = 'Existing'
      ImageIndex = 10
      OnExecute = ActionGotoNextStateExecute
    end
    object ActionGotoNextStateUnused: TAction
      Tag = 2
      Category = 'Find'
      Caption = 'Unused'
      ImageIndex = 51
      OnExecute = ActionGotoNextStateExecute
    end
    object ActionSettings: TAction
      Category = 'File'
      Caption = 'Settings'
      Hint = 'Settings'
      ImageIndex = 52
      OnExecute = ActionSettingsExecute
    end
    object ActionFeedback: TAction
      Category = 'Feedback'
      Caption = 'Feedback...'
      Hint = 'Let us know how we'#39're doing'
      ImageIndex = 56
      OnExecute = ActionDummyExecute
    end
    object ActionFeedbackPositive: TAction
      Category = 'Feedback'
      Caption = 'BTM made me happy because...'
      Hint = 'Provide positive feedback'
      ImageIndex = 54
      OnExecute = ActionFeedbackPositiveExecute
    end
    object ActionFeedbackNegative: TAction
      Category = 'Feedback'
      Caption = 'BTM made me sad because...'
      Hint = 'Provide negative feedback'
      ImageIndex = 55
      OnExecute = ActionFeedbackNegativeExecute
    end
    object ActionFeedbackHide: TAction
      Category = 'Feedback'
      Caption = 'Hide feedback button'
      OnExecute = ActionFeedbackHideExecute
    end
    object ActionAbout: TAction
      Category = 'File'
      Caption = 'About...'
      Hint = 'About...'
      ImageIndex = 57
      OnExecute = ActionAboutExecute
    end
    object ActionImportCSV: TAction
      Category = 'Import'
      Caption = 'Import CSV...'
    end
  end
  object OpenDialogProject: TOpenDialog
    Filter = 'Translation projects (*.xlat)|*.xlat|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 448
    Top = 364
  end
  object SpellChecker: TdxSpellChecker
    CheckAsYouTypeOptions.Active = True
    DictionaryItems = <
      item
        DictionaryTypeClassName = 'TdxUserSpellCheckerDictionary'
        DictionaryType.Enabled = False
        DictionaryType.DictionaryPath = '.\Dictionaries\user-xxx.dic'
      end>
    SpellingFormType = sftWord
    UseThreadedLoad = True
    OnCheckAsYouTypeStart = SpellCheckerCheckAsYouTypeStart
    OnCheckWord = SpellCheckerCheckWord
    OnSpellingComplete = SpellCheckerSpellingComplete
    Left = 536
    Top = 244
  end
  object ReplaceDialog: TReplaceDialog
    Left = 164
    Top = 380
  end
  object PopupMenuTree: TdxRibbonPopupMenu
    BarManager = BarManager
    ItemLinks = <
      item
        Visible = True
        ItemName = 'BarButtonStatusTranslate'
      end
      item
        Visible = True
        ItemName = 'BarButtonStatusDontTranslate'
      end
      item
        Visible = True
        ItemName = 'BarButtonStatusHold'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'BarButtonStatePropose'
      end
      item
        Visible = True
        ItemName = 'BarButtonStateAccept'
      end
      item
        Visible = True
        ItemName = 'BarButtonStateReject'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'dxBarButton10'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'ButtonItemBookmark'
      end>
    Ribbon = RibbonMain
    UseOwnFont = False
    Left = 276
    Top = 240
    PixelsPerInch = 96
  end
  object OpenDialogEXE: TOpenDialog
    Filter = 
      'Applications (*.exe)|*.exe|Library modules (*.dll)|*.dll|Delphi ' +
      'package (*.bpl)|*.bpl|All modules (*.exe, *.dll, *.bpl)|*.exe;*.' +
      'dll;*.bpl|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 448
    Top = 412
  end
  object PopupMenuBookmark: TdxRibbonPopupMenu
    BarManager = BarManager
    ItemLinks = <
      item
        Visible = True
        ItemName = 'ButtonItemBookmarkAny'
      end>
    Ribbon = RibbonMain
    UseOwnFont = False
    OnPopup = PopupMenuBookmarkPopup
    Left = 276
    Top = 312
    PixelsPerInch = 96
  end
  object PopupMenuRecentFiles: TdxRibbonPopupMenu
    BarManager = BarManager
    ItemLinks = <>
    Ribbon = RibbonMain
    UseOwnFont = False
    Left = 80
    Top = 177
    PixelsPerInch = 96
  end
  object OpenDialogDRC: TOpenDialog
    Filter = 'Symbol files (*.drc)|*.drc|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 448
    Top = 464
  end
  object SaveDialogProject: TSaveDialog
    Filter = 'Translation projects (*.xlat)|*.xlat|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 556
    Top = 364
  end
  object SaveDialogEXE: TOpenDialog
    Filter = 'All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 556
    Top = 416
  end
  object TimerHint: TTimer
    Enabled = False
    OnTimer = TimerHintTimer
    Left = 616
    Top = 244
  end
  object HintStyleController: TcxHintStyleController
    Global = False
    HintStyleClassName = 'TdxScreenTipStyle'
    HintStyle.ScreenTipLinks = <>
    HintStyle.ScreenTipActionLinks = <>
    HintHidePause = 5000
    Left = 616
    Top = 332
  end
  object ScreenTipRepository: TdxScreenTipRepository
    AssignedFonts = [stbFooter]
    FooterFont.Charset = DEFAULT_CHARSET
    FooterFont.Color = clGray
    FooterFont.Height = -12
    FooterFont.Name = 'Segoe UI'
    FooterFont.Style = []
    Left = 616
    Top = 288
    PixelsPerInch = 96
    object ScreenTipTranslationMemory: TdxScreenTip
      Header.Glyph.SourceDPI = 96
      Header.Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        610000001974455874536F6674776172650041646F626520496D616765526561
        647971C9653C0000024E4944415478DA6C53CB6ED340143DE357421ECD03AA8A
        040A810D82352C104262531608901065531680C40700DF41FA0148C0029050D9
        00EA8625424810688125041052891AB09D87D387ED78DC3B93D82411939CD873
        E79C3BF79ECCB0300CC118C383A5E7C7E9FD0EE13C2185FF0CE26D125E12EE5E
        9FBF58935AF1F3F0D98B5BBAAE57CBA57D28E4F2C84E6521E21362385D07AD4E
        1BBF1B6BF07DFFF6B5CB171635B1C839AF1E3D724C12FB4100BBD506C2C9EDE5
        17B9A99CC4EAE7952A4D1715B12676ABEC2F219FCB2261685416176969611C62
        4D702AB3A5B84259015314BCAB7D40A95CC29E6211994C4A122292285F7C7A9B
        5BB02C1BDFEA75D2B0910444084386F5F53F44E8C0300C241309A477A5C02987
        EB6E61DBF5E0F75DD13B22E3FF2520285485A6A912AAAAC83927B5327C17311E
        AAC22F043C9015C5095AB6B5DCB6BBE7A6A777A35828A290CF4BD12842F2C071
        7AB06D1BA66592D9DE729C40D70D77E1CA3CBED67F10A185B546832AD1A0ABBA
        ECB54F65FB419F5A4AE2F0A183383B77064F9E2EB97182742673A9B6F209E5F2
        5E540ECCD27CD2440105BD8D81891F57BF484D9C40B4C383108D4613A6D9862E
        4C34C8C41499C80726BA9E07CF1F981888E07028033D93AE46FDAAC36710707A
        B2312F248FB1F85F50A24C1C3C264450D8F83CDA0423C75C26F8F5F37BD5B42C
        747B8E2C51533569E21828E6D15AC7E9A269FE951A59FDF050CC2CDCB83977E2
        E4A9AB99ECD4698A27437119C2F1CB4463BBE7745FBF7FFBE6D1E3FBF75E91B6
        C9464E559A3023EECB686B1343B8D72134091B42BB23C000264119580032DFDB
        0000000049454E44AE426082}
      Header.Text = 'Translation Memory'
      Description.PlainText = False
      Description.Text = 
        '{\rtf1\ansi\ansicpg1252\deff0{\fonttbl{\f0\fnil\fcharset0 Segoe ' +
        'UI;}{\f1\fnil\fcharset2 Symbol;}}'#13#10'{\colortbl ;\red76\green76\bl' +
        'ue76;\red0\green128\blue255;}'#13#10'\viewkind4\uc1\pard\cf1\lang1030\' +
        'f0\fs18 Lorem ipsum dolor:\par'#13#10'\pard{\pntext\f1\'#39'B7\tab}{\*\pn\' +
        'pnlvlblt\pnf1\pnindent0{\pntxtb\'#39'B7}}\fi-200\li200\cf2 One\cf1\p' +
        'ar'#13#10'\cf2{\pntext\f1\'#39'B7\tab}Two\cf1\par'#13#10'}'#13#10
      Footer.Text = 'Click to translate from Translation Memory'
    end
  end
  object TaskDialogTranslate: TTaskDialog
    Buttons = <>
    CommonButtons = [tcbYes, tcbNo]
    DefaultButton = tcbNo
    FooterIcon = 1
    FooterText = 'X of the selected values are not elegible for translation.'
    RadioButtons = <>
    Text = 
      'Do you want to perform machine translation on the selected X val' +
      'ues?'
    Title = 'Translate using XXX?'
    VerificationText = 'Only translate strings that have not already been translated'
    Left = 708
    Top = 244
  end
  object PopupMenuTranslateProviders: TdxRibbonPopupMenu
    BarManager = BarManager
    ItemLinks = <>
    Ribbon = RibbonMain
    UseOwnFont = False
    OnPopup = PopupMenuTranslateProvidersPopup
    Left = 380
    Top = 300
    PixelsPerInch = 96
  end
  object TimerToast: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = TimerToastTimer
    Left = 616
    Top = 200
  end
end
