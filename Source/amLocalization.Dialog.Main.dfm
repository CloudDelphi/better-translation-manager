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
  OnKeyDown = FormKeyDown
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
          ToolbarName = 'BarManagerBarLookup'
        end>
      KeyTip = 'T'
      Index = 2
    end
    object RibbonTabTools: TdxRibbonTab
      Caption = 'Tools'
      Groups = <
        item
          ToolbarName = 'BarManagerBarImport'
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
    Images = ImageListSmall
    Panels = <
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        PanelStyle.AutoHint = True
        Fixed = False
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
      Images = ImageListTree
      Navigator.Buttons.CustomButtons = <>
      OptionsBehavior.CellHints = True
      OptionsBehavior.ImmediateEditor = False
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
      OptionsView.ShowRoot = False
      OptionsView.TreeLineStyle = tllsNone
      PopupMenu = PopupMenuTree
      Styles.Inactive = StyleSelected
      Styles.Selection = StyleSelected
      Styles.OnGetContentStyle = TreeListModulesStylesGetContentStyle
      TabOrder = 0
      OnEnter = TreeListModulesEnter
      OnExit = TreeListModulesExit
      OnFocusedNodeChanged = TreeListModulesFocusedNodeChanged
      OnGetNodeImageIndex = TreeListModulesGetNodeImageIndex
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
        Properties.Items = <
          item
            Description = 'Translate'
            ImageIndex = 0
            Value = 0
          end
          item
            Description = 'Hold'
            Value = 1
          end
          item
            Description = 'Don'#39't translate'
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
        Top = 26
        AutoSize = False
        Caption = '0'
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
    Images = ImageListTree
    Navigator.Buttons.CustomButtons = <>
    OptionsBehavior.CellHints = True
    OptionsBehavior.ImmediateEditor = False
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
    OptionsView.Indicator = True
    OptionsView.IndicatorWidth = 50
    OptionsView.ShowRoot = False
    OptionsView.TreeLineStyle = tllsNone
    PopupMenu = PopupMenuTree
    StateImages = ImageListState
    Styles.Inactive = StyleSelected
    Styles.Selection = StyleSelected
    Styles.OnGetContentStyle = TreeListItemsStylesGetContentStyle
    TabOrder = 4
    OnClick = TreeListItemsClick
    OnCustomDrawIndicatorCell = TreeListItemsCustomDrawIndicatorCell
    OnEditing = TreeListItemsEditing
    OnEditValueChanged = TreeListItemsEditValueChanged
    OnEnter = TreeListModulesEnter
    OnExit = TreeListModulesExit
    OnGetCellHint = TreeListItemsGetCellHint
    OnGetNodeImageIndex = TreeListItemsGetNodeImageIndex
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
          Value = 1
        end
        item
          Description = 'Don'#39't translate'
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
          Value = 1
        end
        item
          Description = 'Don'#39't translate'
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
          ImageIndex = 0
          Value = 0
        end
        item
          Description = 'Pending'
          Value = 1
        end
        item
          Description = 'Proposed'
          Value = 2
        end
        item
          Description = 'Translated'
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
    ImageOptions.Images = ImageListSmall
    ImageOptions.LargeImages = ImageListLarge
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
          ItemName = 'dxBarLargeButton1'
        end
        item
          BeginGroup = True
          ViewLevels = [ivlLargeControlOnly, ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarLargeButton2'
        end
        item
          Visible = True
          ItemName = 'dxBarButton1'
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
          ItemName = 'dxBarLargeButton3'
        end
        item
          Visible = True
          ItemName = 'dxBarLargeButton4'
        end
        item
          Visible = True
          ItemName = 'dxBarButton3'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object BarManagerBarImport: TdxBar
      Caption = 'Import'
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
          ItemName = 'BarButton4'
        end
        item
          Visible = True
          ItemName = 'dxBarButton4'
        end
        item
          Visible = True
          ItemName = 'dxBarButton5'
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
          ItemName = 'dxBarButton6'
        end
        item
          Visible = True
          ItemName = 'dxBarButton7'
        end
        item
          Visible = True
          ItemName = 'dxBarButton8'
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
    object BarManagerBarLookup: TdxBar
      Caption = 'Machine translation'
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
          ItemName = 'dxBarLargeButton6'
        end
        item
          Visible = True
          ItemName = 'dxBarButton15'
        end
        item
          Visible = True
          ItemName = 'dxBarButton23'
        end
        item
          Visible = True
          ItemName = 'dxBarButton24'
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
      DockedLeft = 197
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
          ItemName = 'BarButtonFeedback'
        end>
      OneOnRow = True
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object dxBarLargeButton1: TdxBarLargeButton
      Action = ActionProjectOpen
      Category = 0
      ButtonStyle = bsDropDown
      DropDownMenu = PopupMenuRecentFiles
    end
    object dxBarLargeButton2: TdxBarLargeButton
      Action = ActionProjectNew
      Category = 0
    end
    object dxBarButton1: TdxBarButton
      Action = ActionProjectSave
      Category = 0
      LargeImageIndex = 2
    end
    object dxBarLargeButton3: TdxBarLargeButton
      Action = ActionProjectUpdate
      Category = 0
    end
    object dxBarLargeButton4: TdxBarLargeButton
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
    object dxBarButton3: TdxBarButton
      Action = ActionProjectPurge
      Category = 0
    end
    object BarButton4: TdxBarButton
      Action = ActionStatusTranslate
      Category = 0
      ButtonStyle = bsChecked
      GroupIndex = 1
      Down = True
    end
    object dxBarButton4: TdxBarButton
      Action = ActionStatusDontTranslate
      Category = 0
      ButtonStyle = bsChecked
      GroupIndex = 1
    end
    object dxBarButton5: TdxBarButton
      Action = ActionStatusHold
      Category = 0
      ButtonStyle = bsChecked
      GroupIndex = 1
    end
    object dxBarButton6: TdxBarButton
      Action = ActionTranslationStatePropose
      Category = 0
    end
    object dxBarButton7: TdxBarButton
      Action = ActionTranslationStateAccept
      Category = 0
    end
    object dxBarButton8: TdxBarButton
      Action = ActionTranslationStateReject
      Category = 0
    end
    object BarButtonSpellCheck: TdxBarLargeButton
      Action = ActionProofingCheck
      Category = 0
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
    object dxBarLargeButton6: TdxBarLargeButton
      Action = ActionAutomationWebLookup
      Category = 0
    end
    object dxBarButton15: TdxBarButton
      Action = ActionAutomationMemory
      Category = 0
    end
    object BarButtonGotoNext: TdxBarSubItem
      Action = ActionGotoNext
      Category = 0
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
    end
    object dxBarButton20: TdxBarButton
      Caption = 'Save to Excel'
      Category = 0
      Enabled = False
      Hint = 'Save to Excel'
      Visible = ivAlways
    end
    object dxBarButton21: TdxBarButton
      Caption = 'Import CSV'
      Category = 0
      Enabled = False
      Hint = 'Import CSV'
      Visible = ivAlways
    end
    object dxBarButton22: TdxBarButton
      Caption = 'Import from Excel'
      Category = 0
      Enabled = False
      Hint = 'Import from Excel'
      Visible = ivAlways
    end
    object dxBarButton23: TdxBarButton
      Action = ActionAutomationMemoryAdd
      Category = 0
    end
    object dxBarButton24: TdxBarButton
      Action = ActionAutomationMemoryTranslate
      Category = 0
    end
    object dxBarButton25: TdxBarButton
      Action = ActionFindNext
      Category = 0
    end
    object dxBarButton26: TdxBarButton
      Action = ActionValidate
      Category = 0
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
  object ImageListSmall: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 19923288
    ImageInfo = <
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000002B744558745469746C65004F70656E3B466F6C6465723B426172733B
          526962626F6E3B5374616E646172643B4C6F6164F1C3C4630000022249444154
          785EA593BD6B545110C57F6FF3242AA9B412144B3FB1500CA20663FC032C6C44
          B0B010041194147616366295805A88A058898D8D106C444D6C44123426120959
          36B8316FD9F8F2B15F6FDFBD7746B9EFAD82019B1CB81C6698397366E006AACA
          461000859CC959F327FC079DC1E1C4C381D785807EB21815A827E642DF8DB1E7
          E30F4E8BA2A0CA1F12A5F7FA281D8488F61FBAFC940C8A6DD5987A76F3DE9D4B
          FB468F5C7D53653D0470B94B426704DC2AA4DF517184380E9EBBB6DD99E1F2A9
          03DB4072BB0AA2608C1B1BB8F5E10C6032012BA00E5C422016D4D2250D0E5F1C
          24E8DE0104A00208AA8ED1BB57FA80AEBF02A9CB046C82DA26625BE00CD4CBA8
          CF1B54F21A1556AB75809E20080A800BDEDD3EA67D8343C8CA272AD3E3C40B15
          54404510C92EA7A28888E7A5F92AEAD38A38AD862675B4E28864A1C872D460FF
          F961BC786E1B71A85A108B3A8B8A0149894BDF78FB6868DEAFB05A2E11CFCCD2
          B3AB174D221AB32F51DF90DD47457C8C737E1D114769B2CCCA9A19094DDBB154
          9A632D5A66E7F1BDB42BD3D85ADC29CC9A54C04F17C40B0951B1C29772FD4598
          B62D4B73737477F7B0796B487D6602DB58F6D63B2E347391E71CB59536F1CFC6
          D7FBEFA362D86AA4B018B1E7C44992C529926AD14F50BF7BA749BD082A5EA8BA
          9010D7CD0860C24633FD88AB1D9D1C79C5647E5D72560001505401554421B55A
          FEFCA3F9184803600BB00928B01EC13FB10206489E9CDDED0036FC9D7F01FAB6
          A14B22EE620A0000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000003D744558745469746C65004E65773B506167653B426172733B526962
          626F6E3B5374616E646172643B4974656D3B426C616E6B3B44656661756C743B
          456D7074793B130452ED0000016349444154785E6D51314E04310C1CEF1E0289
          B7D0D3C035347C006A2A3AC413A8A9111D15E20B743420D1DE6310E2F612DBC8
          B11372DC269BC4BBF1CC8EC70B5505118D8F2FABB7611C970A850D3BEC2EC2B6
          D9A79CA68FDBABE33355DD2CE06300D1F2FAF2C8E21D10407EC6FEF0BC3A0130
          1AB01210B3946093045A49D4379BFE10F6F706E45C72879E002CCE2E1DDAF1F1
          1ACBE859B8C27A05A9D61DA0DE030A02BBA3AA80FE1178AA4803951C356564D0
          CEC4CCBB04393114A180C84F4F6B31A99794D2AE026CC2448181B7DB286122DB
          1245E61905927301899802448CE8405352569E333145FB44B5815BDD2EA3DDE7
          AC331E30438BC46656AB5B10C362CB9933910B525D41DF3E9BBD1A19C0336D44
          628163156182CF4AA643FB01F30C8164F7E0F060E11D28C904F5DF7B77C44B10
          F13A7B82BC9EBEDF6FEE5E4F15CD441069B09707146AA6F5D7A775BE27989EEE
          2FCE01EC0118AA2FFDD9C51AE01F03FE022E9833255C6BD8490000000049454E
          44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000000B744558745469746C6500536176653BF9E8F9090000020349444154
          785E7D52316B145110FEDEE6EE9A03EF2E57284A6C6DAC42C0262185829D0A36
          16820AFE010BADC47F60AB2022585A588875B01041822158D858081204956872
          DC5DD6DD3733CF99D93DD713CCC0F7BEF776E7FB66E6ED0654912982633EFCFC
          9F67A2603B2CAC9DBFB2810CEB4B274EE1E4D271B0303832A24450A94C113112
          2846ECED8F301A7DB59CD76F379E9F6D9901425A5F3DB38C4F9FC7B87BE70690
          60E1E49C80BC886049B87EF33656964F6373EBFD1A8096190466017352B0ABA6
          BF22C4D5C9C5972E5CD632010F1F3F0553C4CECE17088B8F9ED99292B898884C
          EFE2548B270711D942867E7F114717BB9E0308582A83962D2C6EE0EE2925EFA6
          245671899205ED561B9D4EA77EC7CA6D887063904CCCE29735C923BEEF4F91C4
          A771DC7FF008C7865DEF88896B23FA6310BC03226FEFEAB55B9A203E63823143
          5490445C3883D05F23F8FCC228F26F180C06B3CB730059BD57866F20229EDF18
          D42358BC78F60487C5EAB98B5A6408E63903B1F6BD358B9FE302F06D729610BC
          B2AE20B63CF9D780602072579FB9F98B6082E6DF8D044932FF156CA648EC89F7
          5E7E844802299851B3C26667F30BCAF31D043B3091DFC3917E0F5C895C1C4D3C
          03E095C79303A4CA20984111CBFCDDE6D6F6CA70D0C39B571F2A7164AF129589
          14C65E59B0FB631765596C03C8030033E929BA8A0C4D8443F6A2982AF67E037E
          F6AC9379188DF20000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C00000027744558745469746C6500526566726573683B52657065
          61743B426172733B526962626F6E3B52656C6F6164CD4DF6E9000000B1494441
          54785EC593B10D02310C454162001688D8E0FA1BC2A2A3A14F895241CF128C40
          7B5DE86E1F96301FE947F249761A4EA2784DBEF3EC28C946557F623D81DC5F8D
          040AA8E04D2AD7126B42C1165C80064C608C047B2094DC40060792C16C24C913
          9C5920A683CD0723290B0137A941107A92CCBCB66CB1399230F3E8875FC1F5F9
          D8F59AD8F12A83EC8C1F1ED30A0A83190C8EC04AC4BB8504A62671AEF16424E1
          431A9B24E048C97A4FF9FFBFF103FFEC875AFFDCA9F30000000049454E44AE42
          6082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000011744558745469746C65004578706F72743B5361766546D49E310000
          02A849444154785E75935D48145D1CC69F597752C3B24C5E83EAA58B280C5E2A
          56892EB22E22C88BA4ABBC780D0BCA2ECCD0A88B500991288A85C2A8E8830A82
          E0C525FB80B20431B008D74273D12CECA2CD6D5BF755576767E77CFC3B737649
          43FA313F660F67FFCF3C676040440B74A9BC50A05598FBCF2F9FDA5E67068ACB
          B38A0064298DB206133B941EFC012125AE1F7F09858709324E55B5EC5BBB29FF
          9DEF80778F1BDAE36746FB59BD0F43E9D1C9739ACA5C659EB2B0A265E974D80A
          507FB88D8E5EF689AD47CC4B797F19F9EE8C1E3E78C2DF05183B133FA2902450
          BE6B1B5E472E226A0F00A42FF8EBCF2162BDC712F36F74F77E40A0F3E160F433
          AF7603BCD50D7E56EAFB072F9E76E2E6D516907050D3B601574EDE01490E490C
          F15408E19920B864C8C95A86443C1777DB3B925E008694029C7108CE202561D6
          72C0B944D4EA5343FD1092E94121B9FE9D901388C46D4C4FA5900E10028C7325
          0317128280352BB6A0A6F90CD21838565B0A41025230F4F68EE34D772C141F13
          877580105237E09CEB06D39640EDDE7B906A3D339340C30D9F3A7B2146232378
          FE388CB1A1E4AD6F6F65D36C9462BF1A389C4328FFADAAD34F2229414458BF6E
          353808C1C14F78F6E83B56792A30FAE4FE690013ED1F778B7403B73EE328DE5C
          A2872408B393FF23168BA1A070251033D015209414D5C3B6090A6B63A5573436
          77410770B7418A213AFE15FFDD6EC53CF4B12CEB109A5AAF810903C3C3437009
          3DE070D101EE5939635A97D8940D0DB9170130111A194376F662388E03858788
          E602DC41E69ADED4C7A04C403AC2004B39F09AB99042426120C3FC77A0FFD4D8
          31022E094220735712A970068F9D849442A7CE0FE089C978CF40B0AF6C91E9C5
          60F00BB83054558694CD95496838834824C058EA95BB52FEF621E528CDCC7A6E
          6F21A4644A9B8874959FB191A9A729553D740000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001D744558745469746C650041727469636C653B4C6F61643B4F70656E
          3B4973737565C206CFD50000024F49444154785E7D93414854511486BF19DE84
          944544CBC05A15448586619BD2DA07096D0229A8B08D9BA0856D8A16EE5A242D
          8A40284C68232ECA9D6042B590CA214A28279519B5C61C199B7166DEBDF79CE8
          300C2EC20BE7FDDCC73DDFFF5F0E375255001E0DCFD0D7D39AE0FF6BEB7F1D78
          FC5EFB7B3B008830C19A1F0C4D4F005DAACABF1253B0BD288AE27D98BC73F3F4
          F9FE5E150324128964DD21198274DDBE7E8AEDD6BD87539D5B1345E927672692
          093A2528AF7C00A01207140045C5F096A2B92985B73324816000443B8FDE7881
          F79EB1A74B008C8CCD34A29B621F7A2FB7E35C0380018213084574731EEF7603
          D0D3DD6A0012D6083404173C400AA861002FA001F5559CDF09C0D0CB0F983BD4
          D5AE42DF950EBC25209ABA7B121188421CEA800ACE0900572FB56D7155D04463
          E7BC019ACFDEFF180321F2B182C4846A91F63DDF987D36880641551051101B21
          2262DA5D5DE5E2AD63591B73D0D5C8C5814AE1271BD9798EEF5DE270F7003659
          11541D8843438C95AFA1BE6A5558CCF06E6478310AB54031B740FEFB1CFB0EB6
          A1B522E5B9D7A8784B62D733770F21A0121009E4D20BAC17FDB825F8BD90A194
          5FA7A5E310D59534FE4FA171D09A542078038981847CE6179F73A5D128AE79D6
          327334A59AD8117936173F112A1B20A19142EB29104B4069BD4AA150F93AF836
          FF23AA9463746585FD278E5059FE426D2D6B0E2AE65C07A9415031503E5762AD
          E4C6011795CB6E1ABFD13EFB669AD9499B3BA8290A20008A2A600F0C62AFB9F4
          F2E6101047D746B367811490DCFE195B29E080EAF30B07C2B9965DFC05AC49C4
          7E0DF9E81B0000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C00000019744558745469746C65005461626C653B466F726D6174
          3B436C6561723B1A97D51A000002CA49444154785E75935D685C4518869F397B
          CEC698A695986E76A14AB3EEA6D96852891A134A4345633590DC48945E0822A5
          8A1755420D815294629188A5469B86D28ABD68F5A25650A4172524B5244D1316
          4D5057A3C1354BB2DBFC904D9BC4A47B7E3EE95CB804EA030FCC5CBC2FF3CD30
          0AF00106FF8F228F68C1151101300F7C32D0AF0C5F2302A215B49E20E83D61BF
          03E221028E6B5F3B7AA8E539C0D6059EA71ADB9AAB110480893F17D8112D0514
          2024FE9867FF9E7250683EEAB9D208F8FE2BB073AE0ECFAFD82042F2E66D4A42
          9B41D04C656EE3382E82C2320D6CDBD9309699B31D7E49CC919A5D469480C0C8
          E814E8A5864B97C708068A896E0FF0CCAE0ABA3614E41CAA6201B63EB4054471
          6324497DFD76942804616838C9CBAD4FF0D3CF29EE1FBFCAC3FD97F9F6D986F7
          94528701D7B83B02FAE2D038AE8080A7E3100916E333C01FEFC398FD8BF2575F
          21B2A7B1E3AB86DAA380A99ADB2FC9D375513D6B1E5D48C5B62D84D42D76E592
          58F37F531ADB81D86B185601BFC7135CFFF262977AE3DDF3D275A40DDBF110E0
          E27771DA5A9FC4327D7CFDFD1095993182B90502B108E4D611C08A543179EC53
          266FA6316D7DC3E0010AF044304D03CBE7F288B546E9D2342595619C6C1644B0
          AA7692E8FC90B995652667E74EA87D6FF65C33ADC2DD0808426524482C1A623C
          95C5DC5A46933F4D786A14A5C05F53CBC4FBDD4C2F2DF26366E6E4B14CBA5301
          45803FFFB66C6E7EFBDC919AA71E7F3D1C2EC3F6A02A35C2CE0761E283532417
          E7184AA77A7BB28B1DC03F88C80681C2BDEDDFC8891F66E4F8C0B4F6C258460E
          1EFC5CBE8856CB6B459B4E03C580B1CF2AB8E727F27E1B1DEE1EEC8B73C77629
          2982E3DD57E94B3B9C5CB5CE9E5B5DE9005607EB9FF7961D877B9DC0008A4375
          EF7CF6E84BBD52D1D22DE54D1F4BB0EED019E001C0785119EC45D18442E9501E
          4A1E7B8BECAFBD065000DC07F8000F58BFEB0B189E4B3EF32FA0B35AC6D4781A
          190000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000003F744558745469746C6500436F6E646974696F6E616C466F726D6174
          74696E7349636F6E5365745369676E73333B436F6E646974696F6E616C466F72
          6D617474696E673BC5369B60000002EB49444154785E5D536F48536F143EEF76
          27685991058549C36CA985454490CD597D8A82FC4384F8A1A2B0D035B5BEE497
          FA90066626044260189893A00F15094EC2922611A39A12A63FB7F99BD3253A37
          67B9393777DFB7F3BE779A74EECECE3D7F9EE73CEFBD5C70F95F084723E86AE3
          FDFCE2EA26FDD3DAE682AFB75A0C511E795E557FAC04FB123A71F8CCE09833C3
          CD47060067025C663A945ED3A47FDBF0AC9859BE36B0414F1B73CC75F188F93D
          56DF5EC46E341EEF3E5F99B70B01AA315F27E03C08F0E5BA23B9B75A4EF83ADF
          D5B021EF13F6D155C72CA3D758F7F025D63352C1FA9DB7D9E0542B7BDE6B62D5
          0FF4B3A5D70F683989A9311FC8DEBC6DC9A7CB759F0BF30B0EA667A44220E402
          CA647120C6008D89480881B48D5930ED5984BE81819EB6BBB6526CC6C8D53B47
          AFECC9CC68371872617E6902C11484AD81C5BF5242D2B4642D58ADA3E0189BBC
          6A7E68EF90240D29D36569613E3C053295D78657592832204E446E81B0077459
          BBC1EDF69663DA250190C31B360344681498185206293A41350817A4D81391E2
          95B2990251B13CFED624949C4649586C17A66C52404281C884AFF2534D0864C6
          B6F01D523C2ECF8796425B89860F5001123F854C5145D6D50881700417AED05F
          3C55C596E9907F6E09C1042895D119B2D3C43D15A4A246454DA8F0FB22105D8E
          0FF315526831FA7ADC1138B57DE70E04F235CA51D8BFC7614CBC4A3510187706
          617121FA125B2B2AEB2BB7D9EB0DFC3731BE006A2209CC9A028C3246BE5D51AF
          8109D76FF0FC1FE8EBED709AB11457057D91D04FE7E2C541DBB4DF3112048224
          04545C8120E1A6226A017662DF6E9B0DB87F046BB11C29ADDECF54D8973F757B
          86866D3EFDB7CFD3EFFB2D1E98F546418E4990A44981F88A0433DE08F45B26C1
          FE69E6C388CD57F0DD3AE3E038CA95D9A71E0B75892F2DF5645966E5D98A7DD6
          22634EB0C494C3CE19B383672A74D6C20BDA2AEC6F4ACC912F9E662832660329
          36E502614C487ED33ACA8992122E29C4A215478F7147108375F6070B79B97E38
          56EEF50000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000021744558745469746C65004170706C793B4F4B3B436865636B3B4261
          72733B526962626F6E3B6463C8680000037D49444154785E4D8E7F4C94051CC6
          3FEF7B77E02073EA92742577579B684891E62AB6C4526B6013696BC9DC5A0B33
          D0322DA716D3CA94A21A8E96E976AEB654688E409B46B859CC249500E3F81588
          234EE0F875DC1DF7A3BB7BDFF7DBC16AEBD9F3D9BEFF3CCFF7C13555CB58A801
          40014CC5E5696BF638D24FBEF7EDF2D683550F7B0E5666B4969C5A5EBBEBCB65
          2F0209803A116E6438F82377A66A60385007A0E4EFB2A5BC51B1B4AEF4EC5AB9
          D476583A87AA642C7055BA47CE4A43F72752713157F67D93DE54B0DFBE04308D
          867E9E290050725F4BBDB7F8E8B29EAA86B7C4E5BF203DDEE3D23E71585AC6F6
          48E7E4C7D2E777C870A05E7E68DE277B4F668C6EDE6BCF00D4017F350A607EF5
          48DAB99CECBC9CF4343BC3E1264CAA60C282AAA8288A028A30313E852DE509EE
          0C4D72EEF26967CD17FD4F0EDE0A064DF9BBEDEB6CD6C51F3C9DF5382EFF1540
          104014216E500C2ED6DDA4F67C3BEDB79BC9C95EC3E8F8784AD28288BBADC1D3
          6C4E98652A7C2C7D2543816674430304C4885B0755E1CC99EBCC51D750F14E35
          DBCB32E91DF98DCCA5ABE8FCB36733E0500D3132EF9EAB108C7AE9ED1BA6B4AC
          969F2E39896A11CE5F68212529975D5B4A395A59C40B79CF6049D0489AAD81AA
          3C0A9854436741140FE148809AEA16CA8AAEA34C65F1E9E7F524EBEBD99A7F80
          53751FB2707118EB836642311F31C63174497C286BEE6C55D3F48971DF2088C1
          A60D6BF9BAB6849D0547D8FD520D2F3F5F822FD8C7AFCEEF58B16A11FEC82831
          3DC6A87F8868C488745C9D0C9AF5A8D2E51EF15BE72FD248B127E2F5FE8DE3FB
          FDEC28280755E1FDCFB691BF310B6FC48566C4C030F08D458984B40E4057837E
          ADAAA7CB87A0E2090EB2E491594C1A4DD45C2EC779AB0E53B287C4399384A353
          718288A8F4767B09F8F4F380069094BBDD7AB3E474869CB8B1428E5DCB90AAB6
          0DB2E59055B2B621C72EAF93134D99723C8EE3F79572A83A5336EEB439EF9A67
          990FA82A1071F7855EF9E35AC0D3EB0C010A9EF000799B56F1EEDBAFC7BF87D0
          0D411185BEEE30AD8DFE88AB2B501CF0C4FC5706DE34CC0D7F15E9AB53BF6A17
          784ED78C4AB72BF6803DDD82B6B013D5A420064CB875FABB628CB8A21DEEDBA1
          A2D6FAB11B8066480C7EE92F045000737CD6BCA736DFB77F7D616A63EE769BCC
          B0C326CF6E4D6D5B5D70FF47C9732CF700164099CE4D3373FCA76CAB43052CFF
          62065440001D884E130F19FC4FFF00FE20CB5D5DF1FFF30000000049454E44AE
          426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000023744558745469746C650043616E63656C3B53746F703B457869743B
          426172733B526962626F6E3B4C9696B20000038849444154785E1D906B4C5367
          18C7FF3DBD40CB1A2E32B55C9D598B4CA675D8D13836652E9B0359B67D589665
          3259E644A52571644474CB4CB6ECC23770C4449DD38D2885005E4683AB69C616
          8DA12384264EC8AAAC0C9149A1175ACEE9E939CFDE9EE7E477F2CBFFB924E720
          E6E943CC3B8895D12B00A0FEE3D08167A75A5BBAEEB71D9D081E6B4DA549FBDD
          A3CEEFDD1F3658016818AA98A71FD1915E202DE980A19D741E3EF6E0F8A7FC7F
          673B6979E002C5BC43B4C2581EB8480BE7BA68E6441BEF3B72F03300990C8E1D
          5016554E7B55D6C1ED9543C6C2B5BB739FDF025988838424E4240F10A0D2EAA0
          D26540AD37203CFE17C2C187A3EDBFDE7CF3DAD4748403A06EA8A8E830AC5FB3
          3B7BAB1901B717AE23DFE1CEC5EBEC90A0E0EB71A3CFD981C0B017C6F252180B
          D6BD74BCFA856E003A0CBDFD966DF250532AD4FF038DB734D18557DF21CFB08F
          2E37B5D370ED5E72D7D52BEEF9654CE9F91C1FD392EB0C4D3A0E4BE7F6ECD909
          CFDEFAD381AF4ED0A3D35FD399E272BA3F3D478F971234FD2044BDCE930AF798
          CF2FAED0DF5373CACCFCA92F2970B29DDCAFD7F56B48945E918201C41738945A
          2D581C7461ADA3192AB50AD64F9A010272730CC8D4AA313BE44289D58CF85D3F
          2411504BB28D93845489145E041F9CC1863C09A11BD7E1EFEA86240339463DB2
          B3F59025C0DFD98DD0C83594E6886C360831F408523265D208BC0021B20A35A7
          82B8BC0429C2239A10D812417988007088B14C8A8421EA75A094044A8A48F200
          17E78587629220B370E69F2884EA3750F07E23245946868E43A64EA3B8695F23
          F8EA7A046763EC780AC9640AF155FEB1269AE0BD91AC8CFDF910108E26F15A5B
          33788D1E860CF6CDE7CF225D45FB3F02A0C7CE36076E5CBD84825C3562A20E4B
          097E0CAD051B5FFCA97C9BE4ABAEA05B2FDBE9E6BE0F880F8568FCDB0E1AA9AA
          646C579C654AEF564D15FDB96333FDBCC94A8E751B6A0140DF5168B9E42A7B86
          266AB6D2ED1A1BF559CAC853B58DFCB576F2D7D9D3AE64B777D96862D716EA2F
          2BA76F4CE62B008C1A00C2F9C57F9D8DA2C99212C5E72C85323699F320A77FD2
          72040021DF9885F56BF2204457706F9EC74C4CF2F744169A012430DBF21E00A8
          2B754F98BEC82EEEED7AF2291A306FA451EBD3346633938FF13BF341969D62BD
          CF738AAF6ED6EA4B006882CE77A14ABFD255D2799903606830E4EF28E274070C
          1C67D74255041044C25C9CE43B4149F8B16735F41B8038DB9300E07F6924ECFB
          01D589CC0000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001B744558745469746C65004164643B506C75733B426172733B526962
          626F6E3B9506332F0000036349444154785E35927D6C535518C69F73EE6DEB64
          63A3AEFB60A3A36E33B8C56581E0D8707E21CC1A43A2A22304FE3001512A86C4
          E900132451FF503367420043B244364C483031465C248B4441C0980C45B4D065
          CDBA4ECAE82AAC5DBBDE8FF3E1BD27F1397973DE9C3CBFF7233964226FC2D543
          A53E0280443E3FD752525AB14323FA06685A3381E492F329C6ADF39954E2F8C9
          C3DBA6018858DE940A9C2C5870C1D51BB6FAF61DBB327860F81A1BFE25297FB8
          3127C7EFE4E5D5745E9EBB9991239766E481937FE4DE1818DB0DC0EB322EABBA
          B63FD5EB7D6CCBBE6F1B83FE9E67BA82E084C0E4123697CAE0D109BC94805B0C
          E7AFCC606A66EEECF75FBCBB753AFAEB2201A0BD3E7861B02914D8DBF34408A9
          AC0D2181D3672E23319D81AB950D016CEBED824E809A722FC62E4CE17A343130
          D4DF73507FB9FFAB551E9F6FCF93EB82B879BB088D52504A14FCC9CE4E95F79D
          B80CD396284A8179C7D3DD1144F29FEC5BE1D73E1BA6BEB2C09BEDCD955A7CCE
          44D1744C1687C9045C05EBFC686F0DAADCB08413D2098E89B4E1BC5779965687
          5ED585D03ACBFDA548E7197EFA711C776EDFC5FF12200A7075F4E85975D7D4FA
          F1F4A635A82C5F02A2956CD46D2EEB1D160B455BC19FEE5E0F4A885A45828071
          81137D1B61DB0C1E5D43E4C8CF5858E4D0A1810BBA5CB76DEEBDB768C1E604AE
          EA6B1F40D9121F0A265385BC0E5457530109404A8010E27805EEE60598CDA15B
          8699C8E7CD4784EEC3F2BA00767C340A4AA9327E79300CE1505BDEFF0E9AA681
          5082150DD5604CA26858282E1693D428E42F6666B3909068EF68C5E6171FC7E6
          17BA611A260C93A9029C713CF7FC3A3C1BEE404B5B2398E0989FCBA190FD774C
          CFA46243B11B4B77ADADF67BB236478E10500AA5D2121D5C48354D3A674108A1
          56114C201E4BB1D9F86FA70880FB1EDD3E34B0A229B4E7E1350FC2E22E2011BF
          16C3FCBD050557562DC3CA964608B8B4C4E49F4924A27F1F193F1DD9AF03B0FE
          1AFDE03D113EDC6431B1A96575089212B4AD6D555F581280D902398343308EC9
          EB49DC9A981A75E043000CA46D09005A49457059DB4BC78E77EDFCDAEAFDF892
          DC3B1295EF7C13977D4E444E45E52BCE5BE7AE338555E10FDF0650EE32B30E4B
          D24C0212A8F210EAAED3D01969BB3FD0BCDDE32BEB06D56AD5D09CCDDA66EE62
          EED6EF43A9AB2331008603ABCEFF019D3AAD15CCD8D2E00000000049454E44AE
          426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C00000011744558745469746C650050617573653B53746F703B3B
          8A7A3C0000012249444154785EAD91C14AC4301086A7D68AE89328EC9BE81EF5
          A6AFA04751F02D647D0341104FB23E8908227A12B6AE2D6B366B9B99D4FE2565
          1743C8C51F063EBE0993494BFF996434DEBFB97E1C7EB4958FC6C35B3814180E
          3D9C71DE4B7AF5B0D7E4FAA9992E9E1B309CF370E82DBDCB1AA067664BD9464A
          2F933B02C3F53ECD127ACDEF97DE657DF509681AAB481A2170BF6AC07B1B101B
          A1DA7E93888063DE1F604C7F1383C33E3C40A812859BC0111F1850B322B602F6
          7CC53A32A01632A2A8D06F1DFBFE1D1CDB604E95D160CFB33038F21159135B03
          8EF8D06FC4411170D4FB03D8DAE9E717CD94E978D5174549E5CCF940B283939D
          B3A3F3C1E4F862501E9EEE5EC2A1C070E8E14CE75D923FDB6CB6B5ED846EEB07
          E0FC96E3B9F3168D5F3C1F3F11AF0B16D00000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C00000014744558745469746C650052756C65733B5761726E696E
          673B0BD1AE46000002EB49444154785E4D935F689B5518879F73929A6649930E
          57B7A64163BA1B61F32E5DA70C04A573A23015E79D0C51A2171365420B6DC310
          D7A288A28ECE4E6138D9D83A6FD64E443BB032717688FF8515265D8DB36B3086
          CE6C59BEEF7CE73507F6C19EC3EFF0DE3C2FEF81F7281121442905A06F490407
          086081C0E55647037C91BFDDC90A687BBF3BB5ED7026F5C6B16CFAEC5436BD7C
          BC27BDFC712675F6C0FAE45BA3EBD63C08C49552FA44368DC34D10CAB10F33E9
          9752118A11212F2204A2B022B8230237C42E5D311C1EA95E7B07A88B88D5CE75
          F24426F5CADA882A6954BEE7F15D6CF9E0083BCEFFC6CE1F17B8FFD011EE7CE2
          6922E8BB366806873BE3834042B5D04074DF1DC907D24A152D24EE797990FEF7
          0EB1EEA1EDCCCC7DCDA9AFE6A8ACEFA1EFED03DCBB77088B8A776976EF4EC6B6
          036D006BDEEDEE983C994DCBB93DCF4B48A9342A07270ECAF8F8B84C4E4ECAFC
          FCBC38665F785626D6C6E5D554EC28D0A981F604EA3E6B2133F00821F57A9DA6
          D7A45AADD26834B8B4740947EEE147F1C592843E2011056271A5EE0E44E82AF4
          13D26C7AD46A352A950AC944C2D5F014746FD98A09A01DB2403C0A44ACA09408
          8EA9A99318E3B3BABACAE2E222BEEF532EFFC5C68DBD3804C1582150E05C0D04
          351B5C76FECAF9EFE8EB2B303B7B866BF5BA93F13CAF95A67B128EF2B96F3162
          F937B015C0D34073C5D85F2CC2D2E733E4723946464730418031ADF8C635A150
          28E0B830338DB1B012C8EF4043BBEB0F2FF8ACD5B17AF1D3137CFFE67E7AF379
          C6C6C6D8BC79138E8181018AC52273AFBFC6CFC78E520B64B51CD8D3C07F0AD0
          40D78BA9F6FD9BDAF4330ADA7A9FDC456EC76364FAB7E2F8B335F685E9697E6A
          C94D11B32072FC94912160395CE528D0F35CF2B67DF9A8DE99103A7D2B04D662
          AC60C4D542CDCAD54561E6742025A02C22BE06F8644387012E7F54F786BF699A
          E15F4DF06545ECDF1ECA73F947585EB072E607CB484B1E727229A6FDF0331172
          73B5E3400AE8B8593B1A401DB80A5C17111B3AFF0338A781C131C708F6000000
          0049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000017744558745469746C65005370656C6C436865636B3B5370656C6C3B
          53FC4602000002E349444154785E65927F48936B14C71F9DBFCA19DDA925534B
          9D3A6D9BDBEB720ABAAE9BEE75BB2A9A9699962204A636B2A566D245B952C1D4
          98FD7060964ED3A20CEE3547F68722256AA2E62F340B94B21FE294A4A4A86B9E
          DE07DED1C60E1C5E5EF83C9FF33D0F0F9290258A28D5799028CE92FE3CE5F648
          653944269C03715CB16A978FD09190A9E5118A52D84F960242C81E0090652309
          0587CBD4A911641950024F0C7204C9FE62450978ED8D700B8FD3E0832E543B50
          6D6723C0A01F4FE54F819420914DC8CF00212B0681B4B0D2831DF68730F63416
          3885C5AA112E1B818882451484415FAEC28F02614F488248F0E72970670BDCF9
          078AB080892578059C02A7A1FF1908833EDCF8600C7A07C97D79D242E0490B20
          589273C8DD5BE81A28CE2643A3F30137967805495CD32B7BDA73752340AA0D7F
          E1548ED868F175A6DB819EC6A0EF609B2F5FC64AAF7CD2D534F00E7ADE6C4076
          EDD0EF5D70656B07D1FCFF80E67E6CA1D9EF5B68E6DB4F34F57513E1E89CA834
          4F6AF240F3F07BE89C5B873C6D3F08536AFFB612D0D39CE834F6E6C3DEFB6259
          072F189FB68F2DC3DDE935286A1886B0942B068A71B3143048758BEA58DD1024
          971BBB88240D875E8599A079D87573E02DB48C99A0BC630A84A9D7469C99BB3D
          B0DC52E092553308C685CFD03EFA018E54F77D8AC9A93F292F686BAC33BE02C3
          B809AAFE7D0592CC9B1F77711379F88EF8297A642970921CD5EB4ED40F42EB84
          097A97BE80A669146ABAE7E1CEE42A5CEA5904695EDB265B785C4927B30B4DBC
          6125B0C3718934DDF5DCDA6750D3BB04FFBD5E07C38B15A87EBC08A91546F08B
          2ED36206B3F767D7105779D54A80E887B2839FA4D5675DEE87AAEE05A87CB400
          F9FAE7C0915D1CA7004FCCDC9B594577264D2888D4590BC49906B364E73EE5C5
          C68CEA3E287BF01244690D1BAC00528AD7EC985E45AD132BE8D6E8320A8CBF62
          2D20329A1171F8B659C20A21FF698AC9EF04365150618ECE91D7A1008BB610D8
          1403AF43C766D2529BFA05BA6881B1E52780230000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000012744558745469746C65005370656C6C3B636865636B3B752FF3FB00
          00026549444154785ECD93EB4B536118C0F721F05FE85B51C302459B15EA8745
          25AE084BC5629169DEC2215B6B11695A6462494A562211813742A5DC0C375C5E
          16EE4C410D9CBB9C4D669663E4B633DC663B3BDBD9D9D94EEF6306FBDE87FAF0
          3BBCCFEDF73EE7C0E1711CF757FC0702C1693904C588B914A408750A638806C4
          0384368526DEF1823B20E8E81FD63932850DDCA6CBBB087163DBC0B7BCF30AA6
          A0EC21DDF264C805E24D17D17953DEBD2510D6B3A2D27B2194EB83E13484BA5A
          F66AFB7255FBCE9B7E8D03041592E7DECE9E31FBB6FFE7C29113555C88A40CB5
          F21744DDAD2E2F1D63A6282A3A0D1B80202F4C4527730B15CCBB0F3AB3B8BACD
          877253A5D71F39CF16C9896B75ED7E45F3EB2D34841D3D5995FCB2625FDE7B95
          340004B79513FAE563C29A588DACCB939E2366917DF6A2B8D1592B7D6A7DF672
          18CFCCAF60562DEB4BFC9CAB4993F5EBD29E601F0082917A79C7F72B954DEEDE
          B7E378767E7954A5C6564ACB9B3DAD1D7D36321C994C1794306AAD615554220B
          564A1E7B693AF6194002C5AE2033574C6AB4980EBE76B5A47503B85026F5F0B3
          8A68D12589EFEEFD6E473CCE6A8CA6B5FE73C512FFFE436712A70A6FEC20412F
          0894C96412986318460FB02CAB8FC7E3FA68348A452211EC4F9E24492C140A19
          689A86BA2E9148B4F3A0F9C7CCACC9A9FD649ECEC8E6800D8DC6BC3EA1B6CCEC
          C576A5CA8A8F29AD10CF646471C691517C6DFCA3152EE081597980CFA90EF239
          9B6ADC821A2D4A74064CA3EF71E3F0280E75889707876C8B0383B6DFF5C35C20
          1030C0063DA9AB8135180C1AA008EB033E9F6F9E2088F970388C41AFDBED5E00
          288AEAFDF73FD32FD93DEE00CB2643620000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000021744558745469746C650050617374653B426172733B526962626F6E
          3B5374616E646172643B259B079D000001EF49444154785E75523B681451143D
          6F77418D86A056DB0AC26A61A512035A8C168A59B0102C5C2C836817145448E3
          070C2954B00AC4C23695209A66D32A29B7B6B050D2C8261A7666DFE7DEEBDC97
          9D0167DD03877BE631E7BC3B87312282024FDF7CD6D18541A2C7C20266C26090
          6D3A3BBC7C78F32E142FBFA428502BC4B7F54EF712DE0A33270B9D8B98BDD0C2
          F9B916CECD9E427FA79F246655EEDC6F775141A3106A6C5D5FC4A7B51E9E3CDB
          867316DE79386B71FCE01E4ECE77B0D3FB904C0E20C1D16333B83DF713DBDF7F
          00C6A05ECBD9A8E1C8D40198BD3EB2D46262000546A30EB4CE9C40B33983DD5F
          BB00044669540A606A58FE9A1D5A8E071000AE2CB1F7EEA69CBEB188D0DF8277
          0E2B1B6721C239B5C89C3183A316D267C6ABA5F654B941F0040916E22D82B5DA
          099E3FB882FFC113E3E18B8F2AEB6500B900D0101C2CC8E72482E277EA2190A8
          45460440C46A336580B701EC33B0B3FB648682D55C1A254E551CE8DF80603DD8
          65FA0991C46A8D09718A1453A20E24E31B884F630FE41C288C0A4434A82E8354
          108F6DE0202103FB7C9203094723AB510C507EC2E40E36BAABEFAFEA4DE93080
          9AED7D33974645D1C578C0FCEBAD6B8536C64C2F3CE23F1C572F8AAC0471A583
          0A586FB8F7783DBEC8C2A31EB8BA519814305C5BB9353DFAD50D14E393000CFE
          023DB570481AE91B650000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001F744558745469746C65004375743B426172733B526962626F6E3B53
          74616E646172643B9EE424DE0000032549444154785E85927D4C535718C62FB4
          F845196EF10F3448540644AD12C568158206D94417701D322C6D808252AC42B1
          463E8528D888AD016A6BB063912F8923925AA44D05B2588D18503794206A022D
          E8D46C685B688720218FF734AD1AFFD8DEE4C939E7FEDEF39C73DFF750747817
          CABBBBF34E5DE3D173062DAF8A0B3D54B9FA0E75A2DA44159CEDA48E567450E2
          92364A286DA1084F48AD49DE93A2E82273978154A6970D0D8F4354F49B945EFB
          100080FA5224979BAE940C5BFE0627B6E03459BB1CA3E3327D8F9ED20D3D338F
          439857AFA6C17C023D06EE9398DC749572F4C51B44FF50FA787968EC22CF0D5C
          CE02F185C893D59DB323CFADD82FAAD5AD08DDB6D87D8257F8961F593F65A8DB
          5EFF338954B16A6E3DE7E05617F384B8444B06265FAC91376B1F60ECA51D8919
          AADE90F571419C18E1525EF6C59E37562754759D088F3A5C4972433664501F43
          54D8EABA66087B87FFFE6CCDD3BE47AF68131BE205F2D1B4237516EBC414EEF6
          5BB079E7B127DF04ACFB8AE4AE64F33F1A78AA4BC4D8117F2C263BBF71EEF63D
          332A553730E19886E5B5034942C55C303B693BC901400586FDFCC94090D34006
          4FB158BB92CAAF97290C70FCFB1EC32F9DA8D218111691A6A799BFA7434BBF4D
          A4BE0C06AD45097C5956D119DDECA47306CF9E4FE2CE1F7F215B5A8BBD82CAD9
          A0D55C099D438ACBFCBC035E918212DFCC5FFAAB44F54F270FD50F41DD350AFD
          FD571834DB91537409EAABF7A0348CE0806600A9AA070E9EDCA4647F2FF4F398
          3079D57D35655A0B6E8D38611A7140613023FFD7FB58BB4584D08DE9286E7C08
          B9610C1D8F9D681F7420B77E1071457A156D308F182C8C97F5BCBDF9E23D3859
          4DF8EEB81606F33438E236D06C0D516C8111AD833308E7D5625B56331AFADF61
          6B6EBB95667EC4801529D15BABFF9C42B2C2849CA60154F4391096D264A7D912
          F2CF1B0F5CB51D373AB1ABD8007E4D2FC43A3BCD9B6D84B96E10CC3D5F17997F
          03B9372770F8F70944480C58B2BDBC919813BE7CF7B9BA4D1223922F5BB1AFF9
          2DD8A28ECF39C5F0F93A78198B53DAC28A3E67F58D92DB1644145EF1F60B0A24
          8C88B978D57FF04F2D242F2CC02D7F77ABFE977F003C798BE83E395B10000000
          0049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C00000025744558745469746C6500436F70793B426172733B5269
          62626F6E3B5374616E646172643B436C6F6E656D0EDF5B000000644944415478
          5EEDD2310E8030088651EFE41599B9014773E5100EC59088F91B2D51D251922F
          4CBC2E5DCCAC8B886C503BF78A378FC0681C60E6CD912AE06B47E43ACA52D500
          BA00C85F159140A26F804F202500CB81BC49C00F447500BFFD5CE05D77E00041
          2F543873A5691C0000000049454E44AE426082}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          200000000000000400000000000000000000000000000000000000000000452B
          1670925D2EEF3B25126000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000925D
          2EEF9D6331FF9D6331FF27190C40000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000001E13
          0930925D2EEF9D6331FF925D2EEF1E1309300000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00001E130930925D2EEF9D6331FF925D2EEF1E1309301E1309304F3219804F32
          19804F3219803B2512600A060310000000000000000000000000000000000000
          0000000000001E130930925D2EEF9D6331FF9D6331FF9D6331FF9D6331FF9D63
          31FF9D6331FF9D6331FF925D2EEF3B2512600000000000000000000000000000
          0000000000000000000027190C409D6331FF9D6331FFAF7D51FFD9C0A9FFE6D5
          C5FFE6D5C5FFC6A381FF9D6331FF9D6331FF3B25126000000000000000000000
          0000000000000000000057381B8F9D6331FFC09975FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFEBE0D3FFA36B3BFF925D2EEF140C0620000000000000
          000000000000000000009D6331FFA36B3BFFF8F4F0FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFCCAC8FFF9D6331FF4F321980000000000000
          00000000000027190C409D6331FFC6A381FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F4F0FF9D6331FF764925BF000000000000
          00000000000027190C409D6331FFCCAC8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9D6331FF764925BF000000000000
          00000000000027190C409D6331FFC09975FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFF2EAE1FF9D6331FF764925BF000000000000
          00000000000000000000925D2EEFA36B3BFFF8F4F0FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFCCAC8FFF9D6331FF452B1670000000000000
          000000000000000000004F3219809D6331FFAF7D51FFF8F4F0FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFD9C0A9FF9D6331FF925D2EEF0A060310000000000000
          00000000000000000000000000007F5028CF9D6331FFA36B3BFFC6A381FFE6D5
          C5FFD9C0A9FFB5875CFF9D6331FF9D6331FF311F0F5000000000000000000000
          00000000000000000000000000000A06031057381B8F9D6331FF9D6331FF9D63
          31FF9D6331FF9D6331FF88572BDF27190C400000000000000000000000000000
          00000000000000000000000000000000000000000000140C06203B2512604F32
          19804F32198027190C4000000000000000000000000000000000}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000013744558745469746C65005265706C6163653B456469743B69AAF970
          000002F149444154785E75915D6C937514C67FFFFFDB8FE55D69D7956EEB04D9
          66A62E534CBCD1B8044214A251D165892C92DDA937A80496A1018D5E2C1A45B8
          003F50512442442E9CCA0226651780103BB20FD8071B3A098432F75537BBEE6D
          DF36EFB14B7AC1C57692274F724E9E5F4ECEE1FD1383BC9717A00057DB57BF37
          BE7DE4F2E13D47FB7ADE393690D9FD5D6FCFAEC3B16FDE3CD0D5047800BDE83B
          0E5D6A075CECF9FE0A806E6EFB6275DBD7DD9D1FFD744DCE0C4E49773C2557A7
          3372E956527EEE9F94F61F07E48D83174E6FD8D25AFBDADEDFF66E3F1413C054
          80DEBAFBC84391AAFAB3F5F79785EB6AC324B342D611727969402B459186E1D1
          09864627161EAC2D33874727F9724783DF15A9595BBC225C736CED03E5E1F255
          4146EEA418B83AC6CD1B534C4FFD4765652955D51554DF574EE5EA107EBF698A
          D78DD51B07D0FA89A6B75A827EDFC381B09FBFC6E7E98AF6712E1AEBF8F34ACF
          D6A1E88135D7FA622DE7BB62BFF65E1E63FC5F8B9468C667D3585606C07029A3
          F8A57B569510CF3707FBC718EE1FFA65ECE2B7DB66E383338033F577F799C79A
          F7AD0F044C1C1473E91CB6ED904EDB005A6D6EEB4C28A582083840E2F650CBC5
          1F769D7C6EE729BB73FFF39E275F3DFA81C70CB68A0882B0E8885A74A29F3596
          02F8819540391002BC80062878F15DF30A205250087003E83F1A1ED976A1BE5A
          4ED4449E2D841091BB05A09B3E471A0F22CF7C8CAC7F974F004303DAC965373A
          8681CEC926C060E9D2730998BCC1E6F93BAC931CAD8FBE42AD06DCD905FB054B
          38AD1CE745C0BD1C206B41661E236B13CA58303FC1AC3E5E57F5F4CC5C929393
          89FD02F76E2FF1D5017A098002304BE9C04B8795E0D3EBA7F28022918D019FC9
          CBC11567B54099529B96D942A7123031427332CE16D1BC5EF9382B752669AD8B
          8D4FEF7B6AE4E69A3971A25E910D806729809D816C1AC35EC095CB80F61072D9
          76B6FE9F5CAE15984939CEB922A17D99438A52E08B703C0F213DCB87B7CF731D
          C007780B21132801DC4BBCD1007C8579002802F4FFAFBC55FD76C606D2000000
          0049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001B744558745469746C65004E6578743B506C61793B4172726F773B52
          6967687416E40EAE000002AF49444154785EA5925D6895751CC73FCFD14D46BE
          B41AF6C20A870425152C2F82A08B0ABD295D05416F572184045D2644082A425A
          831A62128E2E448C59108D9430D84A1C81DBD4D6B6D05C3B739B5B3BB1EDAC73
          CEF33CFFDF4BCFE1807076DB173E7CF95FFC3E7C2FFE91BB136501D600D556C0
          3F3839EC8262062A86A9A3E2881B224AE9DF949E032F90A396B5877A46C3BEAF
          0687DF3DDABB0B683CBAE7A99C064352458212AA484053218943F55D3BA4969C
          A8B2E7C5C71E3FF7EBFAEFDEEBFAA56FE1D6C4FECFF73E7D1908638BC1821A6A
          10D439DCD58FA8D7092255A3A5B9898EE71F213FBBF9B99F06365D7CFBE0F9AF
          676E0C1FDED6DC70030803B3154B122388616AF582204A2A30B562DCD5BC8137
          3AB6716DEC9ED7FBD76F7C75C3E6B3DD7F0EF67EFCCC834D7380EC78BFD7D457
          2D90E0A46A9412A5583688E081875A78B3F5DEC6CBD75AF636346D7AAB65EBB3
          9DBF5F387EE242D7AE2520D0E59EBBB320552AC1A8A4423938C58A905F4C3302
          AD5BEE67F72BDB373EB1BDFDC0932F7D34D4FE5AE73B4063DD822408A5D4AB20
          6224197195580966AC6B8878B47D0B0F6FBDAFB5E754F205701A48EE082AA540
          A124148A29E6206A04ABC9725184C6C6D0C84D4606AF2FAE14663E03A47E811A
          CB15214E0D7343CC897211EBD6464C8C4F73F5D278BCFCF7ADEEF93F7A3B97A7
          06E680A44E90A6CA526CC4C18822674D2EC7FCC43C570646AD3033F5CD3F13FD
          870AD7CFFD05C480B5ED3CEEAB048195B262EE141796B87A7194DBF97CDFCAF4
          9583B77F3B35045400FDF0C7593F7DE45B4484D50B982B14B3C33126C76F8E94
          1646F74F0F7ED9079401D9F7C3AC03A880AA60AB7EA2A6B1F27DF7F9A9CAE2E4
          91C94B9F9C014A4068DB71CCCD9C339F9EA5DAA686BB634E9DA0FCF38997EFA6
          961848DB761E7337C7CC5071DC0C73C7AB4419A60044EECEFFC97FFDEAC21326
          FC988F0000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000027744558745469746C6500526566726573685069766F745461626C65
          3B526566726573683B5570646174653BCE2F81B9000002D949444154785E8D92
          7B48946914C63F2F9997220802211A5128C882C255D7B4B551CB14C108F1D2EA
          AA38D3B6EABA3AB9BA5E721A2FE46DF29638339A94288EE688D1344C0682A5E8
          286B65D36639BA56DB7A5975839AB152779FDEF7EB53CCF9A70F7E9CF73D9CF7
          39E7F07C4C6175B7ACA8E62EBE065ACB308C05B3F1CBAFBC83D5FFFE877169C5
          8C77A6E57596575621A9D082085851916F037996AC405EB9862DAEBEF79A65EE
          DFF7985D5C62199B9A84AAA701254D6771BED20FA2F20824157B4BCE157A1D4E
          957E87C44B470E3159C5B7360AB00F67164C18181D4046650894DDC57830711B
          336F476964EF195521D00ED62341EC2E67328A6E5201DA79BDBBDE308E9FF2FD
          A1EEAF8561410BFD4C2B1E4D5F67E3F8BC06C38636BC7CD387F0B40360D20BBA
          364EC076AF5396BEA96DFF11BFBFAA42EBFD64FC7625902DCE2251FD301D3DCF
          73A19B2A4368E23E302249E7BA4055EF5F989E372232CDEBCFCE3E21D48F0510
          498F821FE5941424704145DB29748CC4A059170E8D5E8893092E6052C52A3381
          E8347EBA5FB493CE3FC609FC33BC0662D676F760C723C7A27857696E0DBFEF79
          0A2625AFC36C850BE22207F2682BC18EC3968BF604070A77B76192736F7CF11F
          BC357E44664D70CB2F64F4357E2EF55108C49E6E54949E692EA5CC47975CE21D
          CF9CCB6C6948CA69C767DA9098AD8440E281C5A567F8C7A8C7BCE90F6806E410
          4A3C119BED2694AB7EC582690C0F2734105CF41864E818DC483B083B09BBA83D
          4FE73A31F2BA1E83130A945E132036CB03979B92D1FBB8114F66DB51A74A4558
          8AAB980A5812AC095BB85D1DA83DBD06099AFB13D03D9689A117523C9D6DC4E8
          DF32F44D1642A68E475CB63B8E473BEF6500ACB12E7622CEB95EAA0C25DEBB22
          47C687421B86AE07F16CCC95FB21527410C49100DA74B38005C1CAFBF4EE6F02
          7E7002B5CE37624F21B16B98D8F681583BE41BB9A7C03DC8713FE792B5B90037
          85DD366B5A60BBC93A7B2E67C3AD6DF90981823369E83C28690000000049454E
          44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000000E744558745469746C6500436F756E7472793B9D0B04DD
          0000031B49444154785E4D935B685C4518C77FE7EC7D938DA1692EBB691B1A22
          4A917AA98992A60F2ADABC288A8A08A636489F144AD5B73EA8980A5E1FBC8005
          8BA020C127D1072595A248106C8BD8606B5BEDEA26D9DDB639BB6777E7EC3967
          CECD49C86AFFF0CDC0F0FD7FDF37C337DACCEE5180FE03535BAE7B9E37235D39
          E53AB26ADBB2EC3ACE4CBB2DCF1F596CBC007873B769D1D1A5901BB50E18037E
          7A72BCE7DBE124B33D9A46C59218C265389745D88253E797DF9D5BF25E036C20
          8894D8940EFC099C28AEC907B31A8C44827B6275467487C06A91F74D76F5A55F
          7AE8DEC7BF1FBDFFCD5920A3699ABEEBB1F90E008057CE149DCB3FAED897FEB1
          5C82768D6105A95456295E6DD06C59DCED9E9AC8F7C43E1EDBFFC18743771C1A
          BBF0D5D33180380010988E7C767139795130C02339C93757D3D4FA46D0CC32D7
          260FE20E4F31E86675EF6F6336F0640C38AC3A69760094263E9914CEEFE978E9
          53DAD176EC278ED13B50C00FA0409AB69B24D90A9061124B88670AE32FFE5A3E
          FDDE711DE0F37357D4ADD2D3E9D171CDDD36CE453349229564706890FEA13CDD
          DD39E2099D980EE96C86EC4DBD7A3CD53B0D64F4FF1F339AECDDBA85AEC90344
          9A4EF1EBE35456CBB832C00B2282008210C20892B91C7AA26B37908977006118
          EC8CA752AA729EF8DEE70817DE6265E934853BF7E3043A8E02B95E8854A1C533
          E87A620048753AD0225F96CCBA83EB27C86EBB8BD4C3AF93D83141A31DD21401
          C20A683B0A22D56E097C290C20EC0008BDE6999A21944125F859A2FEDBB1FD6E
          EAAD807A53D2B07C848AB61362D7EBF8F6DA25C0D53FFBED2F80505AC682A899
          A1B166516BF998CA68347D0CD353009F863A6B29B868B470CD6A2445F907C0FA
          6F0E2E7FF7FC97374F7FB42F84838EECDF786D0D0889F07D907E882D04ED4A11
          AB7CF6E4DA1FF327004B07889400BBB9F2F3DBADF2B92F6AA52BE1F5E5150CA3
          41A3E961D61A98AB25ACE50B91282D2E58D55F8E0155650B50CB46DCFAE87C67
          32FBF27B0EBFBC7DEFAB2747EE7BA73AFAC0FBFE8E7D6F5C2BEC39B2B8F596A7
          E6809D401AD0946F63E146AD7F94CD842E2003248008700001581B9537F52F15
          F6A51913BE4D2A0000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C00000018744558745469746C650044617461536F757263653B4F
          7074696F6E7318FCC056000002A749444154785E85915F48935118C69F6F4E6D
          E8226D54282462E63F50872093106B32A61888A05B7FA82C4923BAD09B8ABA30
          24E82E2F52B1F2A284C890C894946C1189A4A685CBF96722EACC744E73EAD86C
          FBBE734EFBD8846A931E38BC1CCEFBFE78CEF372F049D2FFA0AC5802E4714002
          63341E8C4B23948C53C2E689406608211F35B7BB3B0150E615FE9064B0416798
          EEAA634B9F5BD9CF890EE6B0F4326A1F645BD3EDCC36DCC82C863A666CAD62EF
          EA0AFBC47E717EE74801708CD1FC446D25C008403DDEE206E8362215B188888A
          06232EC4A4A460E5714BAE080040E1930F407802C1390F6944BCF80C0E140C14
          5422E0D7EA0FB8D71701F732082F40ECFFCBBE08A08482F29B105C360061E0ED
          0370589AB13A528FD9CE662CBCEF807B6D0144204101A03CC1FF05107F5F8003
          8108D85D1CE4F15990A796E2A8A63CB803C2D3C0B1109937931884EF3F0CD9A1
          2418C678B40C87E2EACD877739AF6AEE3C4575ED137F8842A03569641A363D32
          AC4639B1B44E316076E3824E8DC696379557AE370D6D3B5DC4ED768EFAB710F8
          0571C7AF3FB9C0ED39089B6D1325454AC4C52AA02FC9DD6BE81B6D5744CB3136
          39D7E30B91040FF1D03E0287C389AA722D8CC6299C2AAF85F1DB142E9DD160C3
          BE8135EB4ABF08A0CE6DBE7BA4FD0516478760FF3E230241F9759C504660D23C
          0F8F87C7C097699C56C7598DA639104A31F2758274B5DDBBCFF9530D6F3A9BA1
          97854A8A438023120992E3B20B43C7B714CC8A035CC5B902BCFD608469CA826C
          65225459496878F40A66F3EC2D6E2774FF46A462F5DF433425D52F95E9C99A65
          EB1AB4DA63484F4BC0DCEC229EB5F540BE37122693B9570C2BE81121D9C7CF27
          E4A82F9679EBE582D21B8EE71D7D4CA5AE7064AA745519393A7D6A6651E2AE80
          7F9C85E5155EABCF3F59C33254FA7A00E13B4E7F038A7C6509079C5294000000
          0049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000030744558745469746C650044423B536F757263653B53746F723B6461
          7461736F757263653B44617461626173653B4164643B4E657722FBFACB000002
          8B49444154785E7DCE5168D56518C7F1EFFB3FEF71E8118732A9664AD344E782
          A4E8C2080BBD58811D8DD0C0180B73154A1122E1A54CD02B9D1769434548BA91
          290C4756A363CB226D93A00BBBB0CCCE6173B5ADE65C9EFDCFFFFF3ECFE3F042
          C641F681EFEDF3FC1C90F9AEE3CD2DD94CB43E72ACC0F429CC3589CAF510B418
          52B95949C2E54DFBBFEE06C4A631D3954FB7156EF4B4DBEDFE33F6EF6FDD3659
          EC351DBF6A776F74D9C8C0312B7EDB6EBF9E79DF7ADB5FFB01F066C6CC3CA61B
          5636BF0726A0092615D029E6D72D21B770112665EA1B1BF9E7E4A99780882A5E
          5221DCFB0B9F6B80081C8AA16814884787A8FC37089561240D008E2A918AA2E9
          04A13C02CC211DBFC264B193D16B1DFC79A19352A19BCA580909F2C8035E5361
          367E5E2DD9BA35D4AD5A0EF4659C7311A00F170409CC66EE138D9C1FF9999EA8
          977DA79FFFFF9313CF1580686FE75AF67CB6964852653696C9726B6298D62DBB
          D8F1C62E422AAF00D1CEB77693C48297205473991CBE7631B9154F1392409208
          A2C270F947924401FCEA856D00E6250D547335CBF8A2BF9B5B778631055523C8
          144948886361C781C629354352FDDEAB08D50CCF1F6343B4B5ECC44C0123D10A
          E974F9ADAB782CF702201C39FAF9CB7EF25EF2CDB5AEB3CDF54D2F52FB6413F3
          6A04E0C1A754624A777FC2503043119C1AC5F13E024625161C903BBCED99ED0B
          6A7C3E1BB1DC67DCCAC7D7ACCB5E9CF8C506E351170278EFC8BFB314C43877AA
          485C56540D09567080033C9005324034238068F3470D23BB3F7E7D7ACD55CE1F
          FF9BAF4E94160009A098597500E43F6CE0E2CD0F00E6BCDAB6CC7A7E7FD78E0D
          3C6B1B5B97185073EEFADB6C68598AE7D14862E1E8C12F01AC52D642477BEF46
          000DEE12A0C70F5D0607F701C8A46B6782959C4D0000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C00000011744558745469746C650044617461536F757263653B05
          A9AFE10000029149444154785E75925D48145F18877FB3B95BB65D587A538126
          D99F153F36C58A02113569236A2550032F94408D8A6E1282A402292AA29BB4E0
          4F416A82997D88824499920959DB85160AB982AB4991A54BAEAEBB33F39EB7C3
          B06B6ADB030FE762661EDE73CE283080A9BFAEC869027214603BB348042B2924
          6858107B48A731227A5D50D3D50140B004CB300DD417778F76D6F2D7F74D3C33
          D2CEBE89172CBC033C37DAC6D3AEDB3CD15DCB434D55FCB2F6601F001333236C
          14008559E4EF385009300142055310108BD810B715D68D9BC0E4C796E4647CBF
          7B2F1B3220150861044823E80B1E4459138DC70A04584A8A86E0ECAC740A087E
          03693A248A0406A19A22484068BFA0FBA70158A079DF2230EF81EF870773D3F3
          98758F23F87312A4931190AE08406884D530334E5F1F446BCF18A2E3ED30C7D9
          40A1F7F6E595606F6EF19F0974D2B11A218411316D4EC5C35E37A26293119390
          8EF0C42CC452409605224132527CCC098AB5A1B1F31362B6A51901F0CA80427A
          A42D48054390C0FE823CF82C0978F6E633249681BE278AE0E5014DFF3B00060B
          32A60804346466ED821A9304FB6EC7050066577FBB12BE46088A7C88421A08A8
          98F1FAE41AC4CECC2C90AE57834143AEE79700A8C64FB1B0A8757D686BC5D4E0
          3B78BF8C8583C63AE7F3A3A7FB153CE31378D0D08C916137E21392CE390A2B2F
          86CF5091AEBB536A2FBB5F9EF1B4A93CE363F3F10CD5F5A88E8F949CE19ACB75
          9C93EB54EB1B3AB8A8AC9A6CA97BD20158A5E6D0B70847D648D74AA3A5EBA571
          878B4EB12D2DFB56E27F59A527CF5EE5F357FEE7FC43653700580E1E3DB1B4D7
          884ACC29F6DC6BA1A0D55158E16E7CDCCB0E67C5248068476155E816FE8D3E3C
          D45B03202055FD0BF3375B9A5BFCAA1A6803400C01097E03FB69726383EA83CB
          0000000049454E44AE426082}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000006346
          298FB17D4AFF432F1C6000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000009B6C
          41DFB17D4AFFB17D4AFF432F1C60000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000002118
          0E30A67444EFB17D4AFFB17D4AFF432F1C600000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000021180E30A67444EFB17D4AFFB17D4AFF432F1C60382717506E4E2E9F855E
          37BF855E37BF593F258016100920000000000000000000000000000000000000
          00000000000021180E30A67444EFB17D4AFFB17D4AFFB17D4AFFB17D4AFFB17D
          4AFFB17D4AFFB17D4AFFB17D4AFF6346298F0000000000000000000000000000
          0000000000000000000021180E30A67444EFB17D4AFFC59E77FFE7D6C6FFFFFF
          FFFFFAF7F4FFE2CEBBFFB68555FFB17D4AFF6346298F00000000000000000000
          000000000000000000000000000080776D8FE7D6C6FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFAF7F4FFB68555FFB17D4AFF2C1F1340000000000000
          0000000000000000000090A468FFEEEEEEEFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFE2CEBBFFB17D4AFF6E4E2E9F000000000000
          00000000000090A468FF90A468FF90A468FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB17D4AFF855E37BF000000000000
          000090A468FF90A468FF90A468FF90A468FF90A468FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB17D4AFF855E37BF000000000000
          000090A468FF24291A4090A468FFBAC6A1FF90A468FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFAF7F4FFB17D4AFF855E37BF000000000000
          00000000000000000000879A61EF97AA71FFEAEEE3FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFD8BEA5FFB17D4AFF593F2580000000000000
          000000000000000000003F482E7090A468FF97AA71FFEAEEE3FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFE2CEBBFFB17D4AFFA67444EF0B080510000000000000
          00000000000000000000000000005A66419F90A468FF90A468FFBAC6A1FFC8D2
          B4FF00000000C0956CFFB17D4AFFA67444EF3827175000000000000000000000
          0000000000000000000000000000000000003F482E70879A61EF90A468FF90A4
          68FF00000000B17D4AFF855E37BF21180E300000000000000000000000000000
          000000000000000000000000000000000000000000000000000024291A402429
          1A40000000001610092000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000452F1F608A5F3EC0B77E52FFB77E52FFB77E52FFB77E
          52FF8A5F3EC0452F1F6000000000000000000000000000000000000000000000
          000017100A20946743D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FF946743D017100A200000000000000000000000001710
          0A20AB774CF0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFAB774CF017100A2000000000000000009467
          43D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FF946743D000000000452F1F60B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF452F1F608A5F3EC0B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFC39470FFF9F5F2FFFDFBFAFFCFA9
          8CFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0B77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFE8D7C9FFCBA282FFC59773FFFCF9
          F8FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFF9F5F2FFB77E52FFB77E52FFFFFF
          FFFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFFFFFFFFFB77E52FFB77E52FFFFFF
          FFFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFFFFFFFFFB88054FFB77E52FFFFFF
          FFFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFEFE2D9FFD1AE92FFCCA486FFF1E7
          DFFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0452F1F60B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFC79B7AFFF4EBE5FFF5EEE8FFC89D
          7CFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF452F1F60000000009467
          43D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FF946743D000000000000000001710
          0A20AB774CF0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFAB774CF017100A2000000000000000000000
          000017100A20946743D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FF946743D017100A200000000000000000000000000000
          00000000000000000000452F1F608A5F3EC0B77E52FFB77E52FFB77E52FFB77E
          52FF8A5F3EC0452F1F6000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000452F1F608A5F3EC0B77E52FFB77E52FFB77E52FFB77E
          52FF8A5F3EC0452F1F6000000000000000000000000000000000000000000000
          000017100A20946743D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FF946743D017100A200000000000000000000000001710
          0A20AB774CF0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFAB774CF017100A2000000000000000009467
          43D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FF946743D000000000452F1F60B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF452F1F608A5F3EC0B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFD1AE92FFF9F5F2FFFFFFFFFFDFC6
          B3FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0B77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFD5B59BFFDFC6B3FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFD5B59BFFDFC6B3FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFD5B59BFFDFC6B3FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFDABEA8FFDFC6B3FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFD3B095FFFFFFFFFFDFC6B3FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0452F1F60B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFD3B095FFDABEA8FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF452F1F60000000009467
          43D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FF946743D000000000000000001710
          0A20AB774CF0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFAB774CF017100A2000000000000000000000
          000017100A20946743D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FF946743D017100A200000000000000000000000000000
          00000000000000000000452F1F608A5F3EC0B77E52FFB77E52FFB77E52FFB77E
          52FF8A5F3EC0452F1F6000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000452F1F608A5F3EC0B77E52FFB77E52FFB77E52FFB77E
          52FF8A5F3EC0452F1F6000000000000000000000000000000000000000000000
          000017100A20946743D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FF946743D017100A200000000000000000000000001710
          0A20AB774CF0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFAB774CF017100A2000000000000000009467
          43D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FF946743D000000000452F1F60B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF452F1F608A5F3EC0B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0B77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFDFC6B3FFDBC0AAFFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFDFC6B3FFDEC4B0FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFE5D0C0FFD3B0
          95FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFFDFB
          FAFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFBF8D67FFBF8D67FFC89D7CFFFFFF
          FFFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0452F1F60B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFECDED2FFF5EEE8FFF4EBE5FFCEA7
          89FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF452F1F60000000009467
          43D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FF946743D000000000000000001710
          0A20AB774CF0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFAB774CF017100A2000000000000000000000
          000017100A20946743D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FF946743D017100A200000000000000000000000000000
          00000000000000000000452F1F608A5F3EC0B77E52FFB77E52FFB77E52FFB77E
          52FF8A5F3EC0452F1F6000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000452F1F608A5F3EC0B77E52FFB77E52FFB77E52FFB77E
          52FF8A5F3EC0452F1F6000000000000000000000000000000000000000000000
          000017100A20946743D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FF946743D017100A200000000000000000000000001710
          0A20AB774CF0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFAB774CF017100A2000000000000000009467
          43D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FF946743D000000000452F1F60B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF452F1F608A5F3EC0B77E
          52FFB77E52FFB77E52FFB77E52FFCCA486FFF5EEE8FFF9F5F2FFECDED2FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0B77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFE6D2C3FFD6B7
          9EFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFC1906AFFE7D5C6FFD5B5
          9BFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFD1AE92FFFCF9F8FFEFE2D9FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFEAD9CCFFCFA9
          8CFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFBF8D67FFBF8D67FFE6D2C3FFD9BC
          A5FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0452F1F60B77E
          52FFB77E52FFB77E52FFB77E52FFCCA486FFF5EEE8FFF5EEE8FFEEE0D6FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF452F1F60000000009467
          43D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FF946743D000000000000000001710
          0A20AB774CF0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFAB774CF017100A2000000000000000000000
          000017100A20946743D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FF946743D017100A200000000000000000000000000000
          00000000000000000000452F1F608A5F3EC0B77E52FFB77E52FFB77E52FFB77E
          52FF8A5F3EC0452F1F6000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000452F1F608A5F3EC0B77E52FFB77E52FFB77E52FFB77E
          52FF8A5F3EC0452F1F6000000000000000000000000000000000000000000000
          000017100A20946743D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FF946743D017100A200000000000000000000000001710
          0A20AB774CF0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFAB774CF017100A2000000000000000009467
          43D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FF946743D000000000452F1F60B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF452F1F608A5F3EC0B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFFFFFFFFFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0B77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFBF8D67FFBF8D67FFBF8D67FFFFFFFFFFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFE
          FEFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFEAD9CCFFCBA282FFB77E52FFFFFFFFFFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFF1E7DFFFC1906AFFFFFFFFFFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFC1906AFFF3E9E2FFFFFFFFFFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0452F1F60B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFCCA486FFF5EEE8FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF452F1F60000000009467
          43D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FF946743D000000000000000001710
          0A20AB774CF0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFAB774CF017100A2000000000000000000000
          000017100A20946743D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FF946743D017100A200000000000000000000000000000
          00000000000000000000452F1F608A5F3EC0B77E52FFB77E52FFB77E52FFB77E
          52FF8A5F3EC0452F1F6000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000452F1F608A5F3EC0B77E52FFB77E52FFB77E52FFB77E
          52FF8A5F3EC0452F1F6000000000000000000000000000000000000000000000
          000017100A20946743D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FF946743D017100A200000000000000000000000001710
          0A20AB774CF0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFAB774CF017100A2000000000000000009467
          43D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FF946743D000000000452F1F60B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF452F1F608A5F3EC0B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFCCA486FFF5EEE8FFF7F2EEFFE8D7
          C9FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0B77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFE7D5
          C6FFD5B59BFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFBF8D67FFBF8D67FFE6D2
          C3FFD8B9A2FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFD5B59BFFFFFFFFFFFEFEFEFFECDE
          D2FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFD5B59BFFDFC6B3FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFD5B59BFFDFC6B3FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0452F1F60B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFD1AE92FFF5EEE8FFF5EDE7FFF5EE
          E8FFDABEA8FFB77E52FFB77E52FFB77E52FFB77E52FF452F1F60000000009467
          43D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FF946743D000000000000000001710
          0A20AB774CF0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFAB774CF017100A2000000000000000000000
          000017100A20946743D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FF946743D017100A200000000000000000000000000000
          00000000000000000000452F1F608A5F3EC0B77E52FFB77E52FFB77E52FFB77E
          52FF8A5F3EC0452F1F6000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000452F1F608A5F3EC0B77E52FFB77E52FFB77E52FFB77E
          52FF8A5F3EC0452F1F6000000000000000000000000000000000000000000000
          000017100A20946743D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FF946743D017100A200000000000000000000000001710
          0A20AB774CF0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFAB774CF017100A2000000000000000009467
          43D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FF946743D000000000452F1F60B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF452F1F608A5F3EC0B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFC59773FFF9F4F1FFF9F5F2FFD1AE
          92FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0B77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFEEE2D8FFC89D7CFFBA845AFFFCF9
          F8FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFFEFEFEFFC1906AFFC59773FFFDFB
          FAFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFFEFEFEFFECDDD2FFF5EEE8FFCFA9
          8CFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFFAF6F4FFBE8B64FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFE7D4C6FFE1CAB9FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0452F1F60B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFECDDD2FFF5EEE8FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF452F1F60000000009467
          43D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FF946743D000000000000000001710
          0A20AB774CF0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFAB774CF017100A2000000000000000000000
          000017100A20946743D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FF946743D017100A200000000000000000000000000000
          00000000000000000000452F1F608A5F3EC0B77E52FFB77E52FFB77E52FFB77E
          52FF8A5F3EC0452F1F6000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000452F1F608A5F3EC0B77E52FFB77E52FFB77E52FFB77E
          52FF8A5F3EC0452F1F6000000000000000000000000000000000000000000000
          000017100A20946743D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FF946743D017100A200000000000000000000000001710
          0A20AB774CF0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFAB774CF017100A2000000000000000009467
          43D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FF946743D000000000452F1F60B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF452F1F608A5F3EC0B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFC2926DFFF6F0EBFFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0B77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFE7D5C6FFD0AC8FFFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFBE8B64FFF6F0EBFFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFE5D0C0FFD5B5
          9BFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFBE8B64FFF8F2
          EEFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFBF8D67FFBF8D67FFE6D2
          C3FFD4B298FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0452F1F60B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFD1AE92FFF5EEE8FFF5EEE8FFF5EE
          E8FFDABEA8FFB77E52FFB77E52FFB77E52FFB77E52FF452F1F60000000009467
          43D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FF946743D000000000000000001710
          0A20AB774CF0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFAB774CF017100A2000000000000000000000
          000017100A20946743D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FF946743D017100A200000000000000000000000000000
          00000000000000000000452F1F608A5F3EC0B77E52FFB77E52FFB77E52FFB77E
          52FF8A5F3EC0452F1F6000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000452F1F608A5F3EC0B77E52FFB77E52FFB77E52FFB77E
          52FF8A5F3EC0452F1F6000000000000000000000000000000000000000000000
          000017100A20946743D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FF946743D017100A200000000000000000000000001710
          0A20AB774CF0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFAB774CF017100A2000000000000000009467
          43D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FF946743D000000000452F1F60B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF452F1F608A5F3EC0B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFD0AC8FFFFDFBFAFFFDFBFAFFD4B2
          98FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0B77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFFAF7F4FFC59773FFBD8960FFFFFF
          FFFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFF9F5F2FFB77E52FFBE8B64FFFDFB
          FAFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFD3B095FFEBDBCFFFFDFBFAFFCCA4
          86FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFF3E9E2FFD5B59BFFBD8960FFF1E7
          DFFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFFDFBFAFFC69976FFC39470FFFFFF
          FFFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0452F1F60B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFD4B298FFF4EBE5FFF4EBE5FFD4B2
          98FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF452F1F60000000009467
          43D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FF946743D000000000000000001710
          0A20AB774CF0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFAB774CF017100A2000000000000000000000
          000017100A20946743D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FF946743D017100A200000000000000000000000000000
          00000000000000000000452F1F608A5F3EC0B77E52FFB77E52FFB77E52FFB77E
          52FF8A5F3EC0452F1F6000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000452F1F608A5F3EC0B77E52FFB77E52FFB77E52FFB77E
          52FF8A5F3EC0452F1F6000000000000000000000000000000000000000000000
          000017100A20946743D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FF946743D017100A200000000000000000000000001710
          0A20AB774CF0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFAB774CF017100A2000000000000000009467
          43D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FF946743D000000000452F1F60B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF452F1F608A5F3EC0B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFD1AE92FFF8F2EEFFDDC2ADFFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0B77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFF4EBE5FFC190
          6AFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFDFC6B3FFD5B5
          9BFFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFECDDD2FFFDFBFAFFF6F0EBFFDEC4
          B0FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFD4B298FFE8D6C8FFB88054FFD8B9A2FFD9BC
          A5FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0B77E
          52FFB77E52FFB77E52FFB77E52FFD0AC8FFFEADBCEFFBF8D67FFEBDBCFFFCAA0
          80FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF8A5F3EC0452F1F60B77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFE8D6C8FFF5EEE8FFE3CDBDFFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FF452F1F60000000009467
          43D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFB77E52FF946743D000000000000000001710
          0A20AB774CF0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FFB77E52FFAB774CF017100A2000000000000000000000
          000017100A20946743D0B77E52FFB77E52FFB77E52FFB77E52FFB77E52FFB77E
          52FFB77E52FFB77E52FF946743D017100A200000000000000000000000000000
          00000000000000000000452F1F608A5F3EC0B77E52FFB77E52FFB77E52FFB77E
          52FF8A5F3EC0452F1F6000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000666666FF666666FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000666666FF666666FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000666666FF666666FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000666666FF666666FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000666666FF666666FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000666666FF666666FF00000000A29B63E045432B6000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000666666FF666666FF00000000B8B172FFB8B172FFA29B63E045432B600000
          0000000000000000000000000000000000000000000000000000000000000000
          0000666666FF666666FF00000000B8B172FFB8B172FFB8B172FFB8B172FFA29B
          63E045432B600000000000000000000000000000000000000000000000000000
          0000666666FF666666FF00000000B8B172FFB8B172FFB8B172FFB8B172FFB8B1
          72FFB8B172FFA29B63E045432B60000000000000000000000000000000000000
          0000666666FF666666FF00000000B8B172FFB8B172FFB8B172FFB8B172FFB8B1
          72FFB8B172FFB8B172FFB8B172FF8B8556C00000000000000000000000000000
          0000666666FF666666FF00000000B8B172FFB8B172FFB8B172FFB8B172FFB8B1
          72FFB8B172FFA29B63E045432B60000000000000000000000000000000000000
          0000666666FF666666FF00000000B8B172FFB8B172FFB8B172FFB8B172FFA29B
          63E045432B600000000000000000000000000000000000000000000000000000
          0000666666FF666666FF00000000B8B172FFB8B172FFA29B63E045432B600000
          0000000000000000000000000000000000000000000000000000000000000000
          0000666666FF666666FF00000000A29B63E045432B6000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000A35A86E04627396000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000B96898FFB96898FFA35A86E0462739600000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000B96898FFB96898FFB96898FFB96898FFA35A
          86E0462739600000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000B96898FFB96898FFB96898FFB96898FFB968
          98FFB96898FFA35A86E046273960000000000000000000000000000000000000
          0000808080FF808080FF00000000B96898FFB96898FFB96898FFB96898FFB968
          98FFB96898FFB96898FFB96898FF8B4E72C00000000000000000000000000000
          0000808080FF808080FF00000000B96898FFB96898FFB96898FFB96898FFB968
          98FFB96898FFA35A86E046273960000000000000000000000000000000000000
          0000808080FF808080FF00000000B96898FFB96898FFB96898FFB96898FFA35A
          86E0462739600000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000B96898FFB96898FFA35A86E0462739600000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000A35A86E04627396000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF000000002C49BADF1320516000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF000000003255D6FF3255D6FF2C49BADF132051600000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF000000003255D6FF3255D6FF3255D6FF3255D6FF2C49
          BADF132051600000000000000000000000000000000000000000000000000000
          0000808080FF808080FF000000003255D6FF3255D6FF3255D6FF3255D6FF3255
          D6FF3255D6FF2C49BADF13205160000000000000000000000000000000000000
          0000808080FF808080FF000000003255D6FF3255D6FF3255D6FF3255D6FF3255
          D6FF3255D6FF3255D6FF3255D6FF2540A0BF0000000000000000000000000000
          0000808080FF808080FF000000003255D6FF3255D6FF3255D6FF3255D6FF3255
          D6FF3255D6FF2C49BADF13205160000000000000000000000000000000000000
          0000808080FF808080FF000000003255D6FF3255D6FF3255D6FF3255D6FF2C49
          BADF132051600000000000000000000000000000000000000000000000000000
          0000808080FF808080FF000000003255D6FF3255D6FF2C49BADF132051600000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF000000002C49BADF1320516000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000839166DF393F2C6000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF0000000097A776FF97A776FF839166DF393F2C600000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF0000000097A776FF97A776FF97A776FF97A776FF8391
          66DF393F2C600000000000000000000000000000000000000000000000000000
          0000808080FF808080FF0000000097A776FF97A776FF97A776FF97A776FF97A7
          76FF97A776FF839166DF393F2C60000000000000000000000000000000000000
          0000808080FF808080FF0000000097A776FF97A776FF97A776FF97A776FF97A7
          76FF97A776FF97A776FF97A776FF707C58BF0000000000000000000000000000
          0000808080FF808080FF0000000097A776FF97A776FF97A776FF97A776FF97A7
          76FF97A776FF839166DF393F2C60000000000000000000000000000000000000
          0000808080FF808080FF0000000097A776FF97A776FF97A776FF97A776FF8391
          66DF393F2C600000000000000000000000000000000000000000000000000000
          0000808080FF808080FF0000000097A776FF97A776FF839166DF393F2C600000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000839166DF393F2C6000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF0000000072AACDDF3149586000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF0000000082C2EAFF82C2EAFF72AACDDF314958600000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF0000000082C2EAFF82C2EAFF82C2EAFF82C2EAFF72AA
          CDDF314958600000000000000000000000000000000000000000000000000000
          0000808080FF808080FF0000000082C2EAFF82C2EAFF82C2EAFF82C2EAFF82C2
          EAFF82C2EAFF72AACDDF31495860000000000000000000000000000000000000
          0000808080FF808080FF0000000082C2EAFF82C2EAFF82C2EAFF82C2EAFF82C2
          EAFF82C2EAFF82C2EAFF82C2EAFF6191AFBF0000000000000000000000000000
          0000808080FF808080FF0000000082C2EAFF82C2EAFF82C2EAFF82C2EAFF82C2
          EAFF82C2EAFF72AACDDF31495860000000000000000000000000000000000000
          0000808080FF808080FF0000000082C2EAFF82C2EAFF82C2EAFF82C2EAFF72AA
          CDDF314958600000000000000000000000000000000000000000000000000000
          0000808080FF808080FF0000000082C2EAFF82C2EAFF72AACDDF314958600000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF0000000072AACDDF3149586000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000A17242DF45311D6000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000B8824DFFB8824DFFA17242DF45311D600000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000B8824DFFB8824DFFB8824DFFB8824DFFA172
          42DF45311D600000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000B8824DFFB8824DFFB8824DFFB8824DFFB882
          4DFFB8824DFFA17242DF45311D60000000000000000000000000000000000000
          0000808080FF808080FF00000000B8824DFFB8824DFFB8824DFFB8824DFFB882
          4DFFB8824DFFB8824DFFB8824DFF8A613ABF0000000000000000000000000000
          0000808080FF808080FF00000000B8824DFFB8824DFFB8824DFFB8824DFFB882
          4DFFB8824DFFA17242DF45311D60000000000000000000000000000000000000
          0000808080FF808080FF00000000B8824DFFB8824DFFB8824DFFB8824DFFA172
          42DF45311D600000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000B8824DFFB8824DFFA17242DF45311D600000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808080FF808080FF00000000A17242DF45311D6000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
          00097048597300000EC300000EC301C76FA8640000008E49444154384F63A01A
          484B4BFB0FC220F6E38D19F71F6E4C4F004B100BD00CF80FC5C41B84C300E20D
          2260000CE336884803C0F8D1C68CFD604DC880D62E203B0CC88E05148DFFFFFF
          E7374C9ABBC03079EE7FC3E43917F4E3E70B848787FB43A52100D9005C00DD10
          A83004603300A218179E7301AA0C0268620032003919A409A699282F2003BC9A
          C9070C0C0056010EB763EBFDCE0000000049454E44AE426082}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000140A
          0B50C06860FFB05850FFA05050FFA05050FFA05050FF904850FF904840FF9048
          40FF804040FF803840FF803840FF703840FF703830FF0000000000000000D068
          70FFF09090FFE08080FFB04820FF403020FFC0B8B0FFC0B8B0FFD0C0C0FFD0C8
          C0FF505050FFA04030FFA04030FFA03830FF703840FF0000000000000000D070
          70FFFF98A0FFF08880FFE08080FF705850FF404030FF907870FFF0E0E0FFF0E8
          E0FF908070FFA04030FFA04040FFA04030FF803840FF0000000000000000D078
          70FFFFA0A0FFF09090FFF08880FF705850FF000000FF404030FFF0D8D0FFF0E0
          D0FF807860FFB04840FFB04840FFA04040FF804040FF0000000000000000D078
          80FFFFA8B0FFFFA0A0FFF09090FF705850FF705850FF705850FF705850FF7060
          50FF806860FFC05850FFB05050FFB04840FF804040FF0000000000000000E080
          80FFFFB0B0FFFFB0B0FFFFA0A0FFF09090FFF08880FFE08080FFE07880FFD070
          70FFD06870FFC06060FFC05850FFB05050FF904840FF0000000000000000E088
          90FFFFB8C0FFFFB8B0FFD06060FFC06050FFC05850FFC05040FFB05030FFB048
          30FFA04020FFA03810FFC06060FFC05850FF904840FF0000000000000000E090
          90FFFFC0C0FFD06860FFFFFFFFFFFFFFFFFFFFF8F0FFF0F0F0FFF0E8E0FFF0D8
          D0FFE0D0C0FFE0C8C0FFA03810FFC06060FF904850FF0000000000000000E098
          A0FFFFC0C0FFD07070FFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F0FFF0F0F0FFF0E8
          E0FFF0D8D0FFE0D0C0FFA04020FFD06860FFA05050FF0000000000000000F0A0
          A0FFFFC0C0FFE07870FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F0FFF0F0
          F0FFF0E8E0FFF0D8D0FFB04830FFD07070FFA05050FF0000000000000000F0A8
          A0FFFFC0C0FFE08080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8
          F0FFF0F0F0FFF0E8E0FFB05030FFE07880FFA05050FF0000000000000000F0B0
          B0FFFFC0C0FFF08890FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFF8F0FFF0F0F0FFC05040FF603030FFB05850FF0000000000000000F0B0
          B0FFFFC0C0FFFF9090FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFF8F0FFC05850FFB05860FFB05860FF0000000000000000F0B8
          B0FFF0B8B0FFF0B0B0FFF0B0B0FFF0A8B0FFF0A0A0FFE098A0FFE09090FFE090
          90FFE08890FFE08080FFD07880FFD07870FFD07070FF00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000013090950C068
          60FFB05850FFA05050FFA05050FFA05050FF904850FF904840FF904840FF8040
          40FF906060FFB08080FFB0A8B0FF303890FF303890FF03040730C06060FFF090
          90FFE08080FFB04820FF403020FFC0B8B0FFC0B8B0FFD0C0C0FFD0C8C0FF7068
          60FFC08880FFC0A8B0FF303890FF6070B0FF6078D0FF303890FFC06060FFFF98
          A0FFF08880FFE08080FF705850FF404030FF907870FFF0E0E0FFF0E8E0FFC0B0
          A0FFB0A8B0FF304850FF808880FF7088D0FF7090E0FF6070B0FFC06860FFFFA0
          A0FFF09090FFF08880FF705850FF000000FF504040FFF0E0D0FFF0E8E0FFB0B8
          C0FF304850FF5098B0FF70C8E0FFD0B8B0FF6070B0FF0B0D1350C06870FFFFA8
          B0FFFFA0A0FFF09090FF706050FF706050FF807060FFA09890FFB0B8B0FF3048
          50FF5098B0FF60C0D0FF90E0F0FF408090FF080A0B4000000000C07070FFFFA8
          B0FFFFA0A0FFFFA0A0FFF09090FFF09890FFF0A8B0FFD0C0C0FF304850FF5098
          B0FF60C0D0FF90E0F0FF408090FFC0B8C0FF0000000000000000C07070FFFFA8
          B0FFF098A0FFD06860FFD06860FFD08880FFC0A8A0FF304850FF5098B0FF60C0
          D0FF90E0F0FF408090FFD0C0C0FFC09090FF0000000000000000C07870FFFFA8
          B0FFD06860FFFFFFFFFFFFFFFFFFFFF8FFFF304850FF5090B0FF70D0E0FF90E0
          F0FF50A0B0FFC0B8B0FFE098A0FFA06860FF0000000000000000C07880FFFFA8
          B0FFD07070FFFFFFFFFFFFFFFFFFFFFFFFFFA0A0A0FFFFFFFFFF90B8C0FF50A0
          B0FFD0D8E0FFD08870FFD08080FFA05850FF0000000000000000D08080FFFFA8
          B0FFD07070FFFFFFFFFF808080FFB0A8A0FF505850FFA0A0A0FFA0A0A0FFE0E0
          E0FFF0E8E0FFC06050FFE07880FFA05050FF0000000000000000D08080FFFFA8
          B0FFE07870FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8FFFFFFF0
          F0FFF0E8E0FFB05840FFE07880FFA05050FF0000000000000000D08880FFFFA8
          B0FFE08080FFFFFFFFFFC0B8B0FFC0B8B0FFC0C0C0FFC0C0C0FFC0C0C0FFC0B8
          C0FFF0F0F0FFC05040FF603030FFB05850FF0000000000000000D08880FFFFA8
          B0FFF08890FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFF8F0FFC05850FFB05860FFB05860FF0000000000000000D08890FFD088
          80FFD08880FFD08080FFD08080FFD07880FFC07880FFC07870FFC07070FFC070
          70FFC06870FFC06870FFC06860FFC06860FF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000140A
          0B50C06860FFB05850FFA05050FFA05050FFA05050FF904850FF904840FF9048
          40FF804040FF803840FF803840FF703840FF703830FF0000000000000000D068
          70FFF09090FFE08080FFB04820FF403020FFC0B8B0FFC0B8B0FFD0C0C0FFD0C8
          C0FF505050FFA04030FFA04030FFA03830FF703840FF0000000000000000D070
          70FFFF98A0FFF08880FFE08080FF705850FF404030FF907870FFF0E0E0FFF0E8
          E0FF908070FFA04030FFA04040FFA04030FF803840FF0000000000000000D078
          70FFFFA0A0FFF09090FFF08880FF705850FF000000FF404030FFF0D8D0FFF0E0
          D0FF807860FFB04840FFB04840FFA04040FF804040FF0000000000000000D078
          80FFFFA8B0FFFFA0A0FFF09090FF705850FF705850FF705850FF705850FF7060
          50FF806860FFC05850FFB05050FFB04840FF804040FF0000000000000000E080
          80FFFFB0B0FFFFB0B0FFFFA0A0FFF09090FFF08880FFE08080FFE07880FFD070
          70FFD06870FFC06060FFC05850FFB05050FF904840FF0000000000000000E088
          90FFFFB8C0FFFFB8B0FFD06060FFC06050FFC05850FFC05040FFB05030FFB048
          30FFE7CFC7FFCFB8AFFFD6C1C1FFEFD5D3FF904840FF0000000800000000E090
          90FFFFC0C0FFD06860FFFFFFFFFFFFFFFFFFFFF8F0FFF0F0F0FFE9E1D9FFC7C2
          C1FF9C9C9CFF898989FF898989FF9C9C9CFFB0A2A4FF0000000F00000000E098
          A0FFFFC0C0FFD07070FFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F0FFEAEAEAFFA4A4
          A4FFC0C0BDFFEBEBE8FFEBEBE8FFC1C1BFFFA5A5A4FF8A8A8AC500000000F0A0
          A0FFFFC0C0FFE07870FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBEBEBEFFBDBD
          B6FFF0F0E9FFF1F1EBFFF1F1EBFFF0F0E9FFBFBFBAFFBEBEBEFF00000000F0A8
          A0FFFFC0C0FFE08080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFADADADFFD9D9
          CBFFF1F1EAFF3F3F3FFF494949FF616161FFADADA8FFADADADFF00000000F0B0
          B0FFFFC0C0FFF08890FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB3B3B3FFD9D9
          C9FFEFEFE8FF555555FFF1F1EBFFEFEFE8FFDBDBCEFFB3B3B3FF00000000F0B0
          B0FFFFC0C0FFFF9090FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCACACAFFC3C3
          BAFFDDDDCCFF6C6C6CFFDDDDCCFFDEDECDFFC4C4BCFFCACACAFF00000000F0B8
          B0FFF0B8B0FFF0B0B0FFF0B0B0FFF0A8B0FFF0A0A0FFE098A0FFEBD8D8FFBDBD
          BCFFC7C7BFFFC2C2BEFFE8E8E1FFC7C7BFFFBDBDBCFF8D8D8DC4000000000000
          0000000000000000000000000000000000000000000000000000000000008D8D
          8DC4CECECEFFBEBEBEFFBEBEBEFFCECECEFF8D8D8DC400000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000140A
          0B50C06860FFB05850FFA05050FFA05050FFA05050FF904850FF904840FF9048
          40FF804040FF803840FF803840FF703840FF703830FF0000000000000000D068
          70FFF09090FFE08080FFB04820FF403020FFC0B8B0FFC0B8B0FFD0C0C0FFD0C8
          C0FF505050FFA04030FFA04030FFA03830FF703840FF0000000000000000D070
          70FFFF98A0FFF08880FFE08080FF705850FF404030FF907870FFF0E0E0FFF0E8
          E0FF908070FFA04030FFA04040FFA04030FF803840FF0000000000000000D078
          70FFFFA0A0FFF09090FFF08880FF705850FF000000FF404030FFF0D8D0FFF0E0
          D0FF807860FFB04840FFB04840FFA04040FF804040FF0000000000000000D078
          80FFFFA8B0FFFFA0A0FFF09090FF705850FF705850FF705850FF705850FF7060
          50FF806860FFC05850FFB05050FFB04840FF804040FF0000000000000000E080
          80FFFFB0B0FFFFB0B0FFFFA0A0FFF09090FFF08880FFE08080FFE07880FFD070
          70FFD06870FFC06060FFC05850FFB05050FF904840FF0000000000000000E088
          90FFFFB8C0FFFFB8B0FFD06060FFC06050FFC05850FFC05040FFB05030FFB049
          32FF575C68FF4792B9FF4494BEFF6D6A81FF904941FF0000000000000000E090
          90FFFFC0C0FFD06860FFFFFFFFFFFFFFFFFFFFF8F0FFF0F0F0FFEFE7E0FF5AA0
          C4FF8AC9E4FFA9EBFAFF8EE1F5FF6BBCDEFF326391FF0000000500000000E098
          A0FFFFC0C0FFD07070FFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F0FF96C4DCFF90CC
          E5FFC9F6FDFF66C8EAFF47BBE4FF7DD6EDFF63B8DCFF01121A5F00000000F0A0
          A0FFFFC0C0FFE07870FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF6CB3D4FFDCF8
          FDFF93D7F0FF68B7E3FF49A7DCFF3BB4E1FF81D3ECFF114A65B700000000F0A8
          A0FFFFC0C0FFE08080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF58AAD3FFDCFB
          FFFF8DCCECFF6ABAE3FF4BA9DDFF34A4DBFF6ED2EEFF318DB9F300000000F0B0
          B0FFFFC0C0FFF08890FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF53A8D2FFDDFD
          FFFF8BCBEBFF6AB7E3FF4CA7DCFF33A2DAFF6CD3EDFF389DCCFF00000000F0B0
          B0FFFFC0C0FFFF9090FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF53A7D2FFDAFB
          FFFF87CDECFF6BC3E9FF4BB3E0FF2CA4DAFF69D1EDFF389AC8FD00000000F0B8
          B0FFF0B8B0FFF0B0B0FFF0B0B0FFF0A8B0FFF0A0A0FFE098A0FF56AAD3FFF7FD
          FDFFCEF2FBFFBDEDF9FFAAE7F5FF91D8EFFF8EDEF2FF3A9ECDFF000000000000
          00000000000000000000000000000000000000000000000000003B92BDF5B2DB
          EDFF2C6F8FD508415FB406405DB320688BD579C1DFFF2487B8F5}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
          00097048597300000EC300000EC301C76FA864000001BA49444154384FAD914B
          2F03511886FB3F2875D9100B2CC4026523117FC02F4094A8957BDB7424D894C4
          C2AD2C48DC139A4888C4E547D8B0425CC6ADDACE94B9B4F3FACEE93062CA42BC
          C9BB3AF33C67BEEF38FE2D6333FB6015A6767987433BBCBD635BBCDEE02A7A82
          6B2DE6E7F630F8B730812F14F959F25D601806D2542D6540D5D35C707695FC59
          323ABD67A25698444F5B82AF35312B81C9888959F910282490D514A2B28A0B31
          06AF7FD130312B81896D13CB848F40301B21AEBCA16EA3069E130FA2AF29740D
          85B3FC812960E007AC132C290ADC9BB5683E2A47A1E0C233093A87E6ED027F28
          23104EBDA88A54E0555339DCB0E546D37105F20527442989A7A40E4F36C1C0F8
          261794AE17A3F1B010656B2568D820F8B012AE11821332DDAEE3810BE6EC82BE
          D1752EB895A2285A71A27A2F17F507A5C813727027491C7E2458944930386B17
          7803CB5C105752B88C3DA260D90957300F37B1049E4CF89EE05B4943C7401641
          B77F890BD89232D5F9BC0C7C205094084E68B88A6B68EF9FB10B5A7BE7B9E05E
          D63E2BD26D7754065E1378F1A2E2FC59415BFFB45DC036DBE55BA086AD0E87D1
          496567AC1DB4BC769A3FEB2BFC2D0EC73B02D5155B8641533A0000000049454E
          44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000734944415478DA63FCFFFF3F0325801166407A7A3A983173E64CC6BA
          BABA078C8C8C0D8D8D8D0BC832A0BEBEFEBF828202C3FDFBF71F123208A70140
          4D0CDBB76F6778F1E2055E83F01A0003F80C22CA0064834E9C38C100D473B0A9
          A9C981F62EA0280C288A058AD301214DA306E030805C00007F69CDE1F8BB91C9
          0000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000002B44944415478DA95536D485351187ECFBDFBB4E9DACCA196B3E9B45C
          AEC034FA202CB5267E402041A1D18F2286E49FA218062D09A21F8942840499F4
          23C10821B1C414A18C0CC1E16E6998D24A9BE4D7862E75DBBDF7DCCE590AD10A
          ECBD5CCEE1DCF779DEE77DEF73902449400391A08BBDB1DBB249B3F90C8B6485
          C0B01908B008A2E81130DFE7F37A9A5AEA2ABF923C2CAD01115D29D85C5CAC28
          2B77DE566B626AB2D275AC215E0D097A3550DA395F106617566174623EE09B5F
          B872FF72510BC1F294245295828BCB9DCFD28D7A5BD141238804151225E0C94B
          13E432040A06811816A06F600A3E4FCE76B637D79C9AE1B815FA9DB537BC6A30
          9B0C35B67C134C2FF2441F44054308080F246A15D0F3DA03DCC897FA870E5B2D
          AAB8DABA2B3935DD7DB6229BF5F8C2C0324C24F96F0458C2641C00DBF50A78FC
          94E34787067251E5CDEEA682FD66BB2651072181C861016432268A0001259008
          018E90F1BE4578FED2D5884ED7F50E579459F7CC86A3ABFE2B68912D4A044FDA
          8746D04967EF7C6989357E6659D83801695317C34257E7B01F9DA8ED5ECA3F66
          895D16FE43C1DA40DFF6707E5472A9833B70D462C532E586C01279A8023E1886
          C1BE61372AB8D07ACF9AB7A35AB72D61C30A2889DFEB0757FFE01D642D73E66C
          CDCC7D9777245B2EFEB23499723408210630C6402D82050CDC9B8FC2A7818E5C
          DAB86A5F55737D8AD9549DB5370DC2D47D4802C51FBF5220E7986E88CB26B849
          F08C7CB8EB6ABBE8A0048C569B1A97517AA32D292DE5B825C70412199246298B
          58989A5DE0312C054552598489F793F06DCCF3C2D576BE926097D62F13ABD51A
          E38C858E5BB13AC3B994CC24B92159072A8D32227975390473A4E7A971EF8ACF
          3B7E7DACEBDA0372FC836045F4DB75A69A55A987ECBB63F43BABE4AAD8C312C3
          26916E8883F9EFE160A03F30E56E9E763F1A2379C1F5EBFC13749B22D6ED599B
          F50000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
          00097048597300000EC300000EC301C76FA8640000026549444154384F6D93DB
          52D3501486D3A280380E374A4FA4074A9AA64DD3A63934699BB4F4900AA505AC
          8882E70BDF89C7C00B9171780266BCF125A4EA03E8CD72AD9D7012D7CC3FBBC9
          5EDFBFD6DE59E5AE45081516F5DE58D2FA8792E69D1575EF37FE3E13D5DE61AE
          D2DEC2FD9920EF568492792796AFF68F94DA06586BBBE07807D01ABC02A7BF0F
          B5F6532899EB90AB748F78B11EA77C1FF3232496D64449F7CEF5E60EB803025F
          82EB910E9881D3DB8766EF39A8F511E4D4EEF76C51E789F3F1426156543B5F0D
          D7871D0F01849AFD170C6AA0EA5DD21ED43B7BCC44909D8F48DE61BC5069BF91
          8DC7981C002CF93AF40C6C141DCB6AEFB2E3E4D52EA4F3D66BC4C39C50723FEB
          CDED2B3000EC351F228064B6481330DD0994F19E329275820677B955D9FD6161
          824D1502809E6B1740EB09182ECAD9019D691B547B4C1D9CA3C12C1A38084D6E
          5609201DEF8581D8A1466A6C41B54E1A414AACFD4183792E5368FC2457F3B28A
          9F4C50F51218E3E58D195841D1A74E0AFA140DE6B894689C94F01259055605C1
          002240AD6F42C51E41D91E42D942D586EC1297B3EA17768444B6FA212B370380
          2A216423646DFA004A41483137709848EB90166B1049C9EFD1802633B6C0AF6A
          DF24B5730511806D96CC210398B04B127E3588274BC7ECFCC130859712396539
          5B9DD2A60F10EC033423456300B23E80956203E26965FAE06142208E600A7299
          598CA4D2F18CF229291890535A50A8F6A1A80D40C255905BC0673588F0F2F1E2
          527285F219F94F90E37C8417DF4679F9349A927FC5520AD01AE58BA78FE2E23B
          DCBF87FA2F7C11D40DCDF81C6A01753F58E9BCF4FEC6BF90E338EE2F55193229
          700A35F90000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000024744558745469746C6500446F63756D656E7450726F706572746965
          733B50726F706572746965733B3656A5830000033849444154785E7D937D4C53
          5718C69F7B7B4AE52355971095599D2B3A0264A6219A85AA03679832C3A21665
          A2F891CD124704D9206340746B46623019C6A82C565315043F96383F83124D86
          D3B9580BCC39C0E98A6C4080B60B850BB7F79E7BBC6D5AFE5A78935FCE93933C
          EF7DCFCDF3629AE25478154D484F57D5872FA3B2F6D29429C817DF380ACA6BCE
          B3325B23DB576DDF116E14812BAEB6E3F38A93200060FBD2C2590ACAE2CABF6B
          BCAA50DAEF1919A8A18C59766C30833106FBF93B96DC5D158FE3E72CA800C719
          9F3C6CC93E62FBF43F6BF971C647C65D98987A65775E46C6969CF4ADC6C4C44E
          C6F0D192450930BC190F4561D9C9C9A91DF99B32F2ADDBD7BE976A7AFF270044
          12259048834040F2F8C72690B4643E9216CFE7BD3E3F3484872228282C58CB2F
          5E34175461B8FBB30B7DAF7ABD002049D2D4CFD1E4EEFA3A2539E55D5769610E
          2F49140A63E0390E924C43CF5025C40045D5B7F5CAD3F6472BDA1F5D730290B8
          A0796751ED368D56BB9968C8BAAAD23C4E1B45F0C7B3976868BA054234D8BE35
          1B06430286863DA83BFEA332210AADA22034FFD97EAF8900204437C3B1273F4B
          1DDD0042087CFE71D41D6B46FF3FEECD248A7027CFE2C2A18356F01A82BD9F6D
          E4BD5E5FD6E973B7B2DC7FB92E11009C1C0884DEE71F9F84A45050CA10131B0D
          DD0C5DB4362A4AD5B1102519E38208AA500C794621CB0104BD3C00F9E5F3AEA2
          AF0ED4B5EEAF3CC10607BD989C10B1735B0E92524D67826C5CBF1263C2243CBE
          51D86C2760B737B4F5B9BB4B010422E1D1A5993F5EBEA7E47BF9EFBE21F6C0D9
          C3DA1E77B1DFBBFB588F7B80B53FEB652D6D9DEC5AAB93ADB3942886B7D33201
          C4A8F0040053417C82B12C739549234C8AA1AF8D8CF830365B1FCC008646BC98
          39531F0AD5F2654BB9DE172FF603CEBC7796AE57A672C0281644C7EA70F3F62F
          68BD7D1F94D3B2A2BD9F700149467D7D239BA58F41DA3213A7D7C781CA745ED0
          23530991244A1DBFDDD870E468C3AF0E47F3F5A71DF7B32684D1078303C370BB
          FF85E0F73DEC74B6655F6EBA78F5D40F0E57FF2BD71600A2422942B53AA718E1
          258909136B4ACF2D367F58C8CC6BACCC98F2410980B8E07DF8D4AA706F25AD99
          7695A355DE0812D63CFEA75E03CFF56ADF743CC8850000000049454E44AE4260
          82}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000001B744558745469746C65005761726E696E673B4E6F7469
          6669636174696F6E3BB6E779860000024149444154785EA58F5F48536118C64F
          C5A02E9288EE2BA8816585ABFC83DA28EBA6CB6A12CC300BA2024D27589A0CB4
          ADA4DC859995D36DD1A4CD5DA428E94636D7E98FD236D79A04DBB42E14664E73
          5B46E982A7F73BD448362FA40F7E9C87F77D7EE77C87F3EA8EFE179C475B9812
          3A6BDC0F8F9410729657ECB91F1C4E8209439A823CCA7162C9DE949F4D3975F7
          DD5D69129D57F76F1A69967E9E1E7D8C90CB00CA7E7D55665AAA2E37AC295806
          9DB5FCED7C63D0AAC6AF6F9F108F05101C5081663AB64BEABF6ECC4BC0AE6EAB
          CF29766AE588935C76A51AE5443CE287B3ED349E37E4C8586799E368C84DD0A5
          906C1FAACF9D0FB98C589A0F407EE682C062D883A9B76DA0DD9CA952B2F55F87
          1BACCB12A83CBE4D64AB3DC87B9F5409C28F49072A2B140C217F9FE886C7580E
          EA0C49D337AFFBEB71D6EA03C2D57B15923A5E23A32F8F233A66C042B01BFA96
          1B30102C477D3A7A491FF8A693A06E0D7304F759452667BEB8279B9E8B730107
          621F3B11713523EA6EC5F4700B43C8C2CCDB8E49BE15AC4B4E1673B93BA776A6
          F55CDE17F49A9588BE6F4778508159470D621F1E41A3563258A6592DC22FAAF0
          F58D1A23F74B418EBFF1C48E8D9CE57C86DE7E538688CF84195B19A6FB4A0562
          3E230E498F31584ECCBF0C5CC20C7F0BD6EB85E83A97D1C1594A768527FA5588
          B8B59877DE436CCC44FF6AC3CFD028A2213F22535E2C8CF70BBBB9572A84EDD7
          30FBB21EC15E2598CBE98AC43DE6E274A4C27276AFC04AFB8E22F1538ECE1642
          4CEC5E25E23F2E2722D6131B56097344BF0168ADF1039DB8E68C000000004945
          4E44AE426082}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          000000000000000000005D5D5D60B8B8B8BFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
          F6FFB8B8B8BF5D5D5D6000000000000000000000000000000000000000000000
          00001F1F1F20C8C8C8CFB9ECF8FF4DD9FCFF00CCFFFF00CCFFFF00CCFFFF00CC
          FFFF4DD9FCFFB9ECF8FFC8C8C8CF1F1F1F200000000000000000000000001F1F
          1F20E7E7E7EF6CDEFBFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CC
          FFFF00CCFFFF00CCFFFF6CDEFBFFE7E7E7EF1F1F1F200000000000000000C8C8
          C8CF5CDCFCFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF04C3F3FF04C3F3FF00CC
          FFFF00CCFFFF00CCFFFF00CCFFFF6CDEFBFFC8C8C8CF000000005D5D5D60B9EC
          F8FF00CCFFFF00CCFFFF00CCFFFF14A1C4FF414141FF414141FF414141FF4141
          41FF14A1C4FF00CCFFFF00CCFFFF00CCFFFFB9ECF8FF5D5D5D60B8B8B8BF4DD9
          FCFF00CCFFFF00CCFFFF14A1C4FF414141FF2187A0FF08BBE7FF08BBE7FF2187
          A0FF414141FF14A1C4FF00CCFFFF00CCFFFF4DD9FCFFB8B8B8BFF6F6F6FF00CC
          FFFF00CCFFFF00CCFFFF414141FF2187A0FF00CCFFFF00CCFFFF00CCFFFF00CC
          FFFF2187A0FF414141FF00CCFFFF00CCFFFF00CCFFFFF6F6F6FFF6F6F6FF00CC
          FFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CC
          FFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFFF6F6F6FFF6F6F6FF00CC
          FFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CC
          FFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFFF6F6F6FFF6F6F6FF00CC
          FFFF00CCFFFF00CCFFFF00CCFFFF414141FF3D4A4DFF00CCFFFF00CCFFFF4141
          41FF414141FF00CCFFFF00CCFFFF00CCFFFF00CCFFFFF6F6F6FFB8B8B8BF4DD9
          FCFF00CCFFFF00CCFFFF00CCFFFF355B65FF355B65FF00CCFFFF00CCFFFF355B
          65FF355B65FF00CCFFFF00CCFFFF00CCFFFF4DD9FCFFB8B8B8BF5D5D5D60B9EC
          F8FF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CC
          FFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFFB9ECF8FF5D5D5D6000000000C8C8
          C8CF6CDEFBFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CC
          FFFF00CCFFFF00CCFFFF00CCFFFF6CDEFBFFC8C8C8CF00000000000000001F1F
          1F20E7E7E7EF5CDCFCFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CC
          FFFF00CCFFFF00CCFFFF6CDEFBFFE7E7E7EF1F1F1F2000000000000000000000
          00001F1F1F20C8C8C8CFB9ECF8FF4DD9FCFF00CCFFFF00CCFFFF00CCFFFF00CC
          FFFF4DD9FCFFB9ECF8FFC8C8C8CF1F1F1F200000000000000000000000000000
          000000000000000000005D5D5D60B8B8B8BFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
          F6FFB8B8B8BF5D5D5D6000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          000000000000000000005D5D5D60B8B8B8BFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
          F6FFB8B8B8BF5D5D5D6000000000000000000000000000000000000000000000
          00001F1F1F20C8C8C8CFB9ECF8FF4DD9FCFF00CCFFFF00CCFFFF00CCFFFF00CC
          FFFF4DD9FCFFB9ECF8FFC8C8C8CF1F1F1F200000000000000000000000001F1F
          1F20E7E7E7EF6CDEFBFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CC
          FFFF00CCFFFF00CCFFFF6CDEFBFFE7E7E7EF1F1F1F200000000000000000C8C8
          C8CF5CDCFCFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CC
          FFFF00CCFFFF00CCFFFF00CCFFFF6CDEFBFFC8C8C8CF000000005D5D5D60B9EC
          F8FF00CCFFFF00CCFFFF395259FF297588FF00CCFFFF00CCFFFF00CCFFFF00CC
          FFFF257E94FF355B65FF00CCFFFF00CCFFFFB9ECF8FF5D5D5D60B8B8B8BF4DD9
          FCFF00CCFFFF00CCFFFF10A9D0FF414141FF2D6C7CFF10A9D0FF10A9D0FF2D6C
          7CFF414141FF10A9D0FF00CCFFFF00CCFFFF4DD9FCFFB8B8B8BFF6F6F6FF00CC
          FFFF00CCFFFF00CCFFFF00CCFFFF10A9D0FF355B65FF414141FF414141FF355B
          65FF10A9D0FF00CCFFFF00CCFFFF00CCFFFF00CCFFFFF6F6F6FFF6F6F6FF00CC
          FFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CC
          FFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFFF6F6F6FFF6F6F6FF00CC
          FFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CC
          FFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFFF6F6F6FFF6F6F6FF00CC
          FFFF00CCFFFF00CCFFFF00CCFFFF414141FF414141FF00CCFFFF00CCFFFF4141
          41FF414141FF00CCFFFF00CCFFFF00CCFFFF00CCFFFFF6F6F6FFB8B8B8BF4DD9
          FCFF00CCFFFF00CCFFFF00CCFFFF355B65FF355B65FF00CCFFFF00CCFFFF355B
          65FF355B65FF00CCFFFF00CCFFFF00CCFFFF4DD9FCFFB8B8B8BF5D5D5D60B9EC
          F8FF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CC
          FFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFFB9ECF8FF5D5D5D6000000000C8C8
          C8CF6CDEFBFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CC
          FFFF00CCFFFF00CCFFFF00CCFFFF6CDEFBFFC8C8C8CF00000000000000001F1F
          1F20E7E7E7EF5CDCFCFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CC
          FFFF00CCFFFF00CCFFFF6CDEFBFFE7E7E7EF1F1F1F2000000000000000000000
          00001F1F1F20C8C8C8CFB9ECF8FF4DD9FCFF00CCFFFF00CCFFFF00CCFFFF00CC
          FFFF4DD9FCFFB9ECF8FFC8C8C8CF1F1F1F200000000000000000000000000000
          000000000000000000005D5D5D60B8B8B8BFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
          F6FFB8B8B8BF5D5D5D6000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000F0F
          0F10F6F6F6FFEDEDEDF74D4D4D50000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000000F0F0F101E1E
          1E1FF6F6F6FF434342FFE6E6E6FD606060630000000000000000000000000000
          0000000000000000000004040404000000000000000000000000F6F6F6FFF6F6
          F6FFF6F6F6FF444443FF545453FFBABABAFFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
          F6FFD7D7D7E01C1C1C1DBCBCBCC4000000000000000000000000F6F6F6FF4343
          42FF434342FF434342FFE1E0E0FF434342FF434342FF434342FF434342FF4343
          42FFE1E1E1E9E6E6E6EEF6F6F6FF000000000000000000000000F6F6F6FF4343
          42FFF0EFEFFFF0EFEFFFF0EFEFFFF0EFEFFFF0EFEFFFF0EFEFFFF0EFEFFF9999
          98FFF2F2F2FF434342FFEBEBEBFF000000000000000000000000F6F6F6FF4343
          42FFF0EFEFFFF0EFEFFFF0EFEFFFF0EFEFFFF0EFEFFFF0F0F0FFF4F4F4FFE7E7
          E6FF434342FF454544FFEBEBEBFF000000000000000000000000F6F6F6FF4343
          42FFF0EFEFFFF5F5F5FFEBEBEBFFEBEBEBFFEBEBEBFFEBEBEBFFD1D1D1FF4343
          42FFDBDBDBFF4E4E4DFFE0E0E0FFEBEBEBFFEBEBEBFFF6F6F6FFF6F6F6FF4343
          42FFF0EFEFFFF6F6F6FF434342FF434342FF434342FF434342FF434342FFDBDB
          DBFFF0EFEFFF434342FF434342FF434342FF434342FFF6F6F6FFF6F6F6FF4343
          42FF434342FFEBEBEBFF434342FFF0EFEFFFF0EFEFFFF0EFEFFFF0EFEFFFF0EF
          EFFFF0EFEFFFF0EFEFFFF0EFEFFFF0EFEFFF434342FFF6F6F6FFF6F6F6FFEBEB
          EBFFEBEBEBFFF5F5F5FF434342FFF0EFEFFF434342FF434342FF434342FF4343
          42FF434342FFF0EFEFFFF0EFEFFFF0EFEFFF434342FFF6F6F6FF000000000000
          00000F0F0F10F6F6F6FF434342FFF0EFEFFFF0EFEFFFF0EFEFFFF0EFEFFFF0EF
          EFFFF0EFEFFFF0EFEFFFF0EFEFFFF0EFEFFF434342FFF6F6F6FF000000000000
          00000F0F0F10F6F6F6FF434342FFF0EFEFFF434342FF434342FF434342FF4343
          42FF434342FF434342FF434342FFF0EFEFFF434342FFF6F6F6FF000000000000
          00000F0F0F10F6F6F6FF434342FFF0EFEFFFF0EFEFFFF0EFEFFFF0EFEFFFF0EF
          EFFFF0EFEFFFF0EFEFFFF0EFEFFFF0EFEFFF434342FFF6F6F6FF000000000000
          00000F0F0F10F6F6F6FF434342FF434342FF434342FF434342FF434342FF4343
          42FF434342FF434342FF434342FF434342FF434342FFF6F6F6FF000000000000
          00000F0F0F10F6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
          F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end>
  end
  object ImageListLarge: TcxImageList
    SourceDPI = 96
    Height = 32
    Width = 32
    FormatVersion = 1
    DesignInfo = 19923368
    ImageInfo = <
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F40000002B744558745469746C65004F70656E3B466F6C6465723B426172733B
          526962626F6E3B5374616E646172643B4C6F6164F1C3C4630000054249444154
          785EE597DF8B5D5715C73FFBCC9D4CDBA4650C6DD334516223452CD81A8B208A
          587D50109F2A2DD42789885450FF8116F441F4455A141F2CEA836FFAD23C444B
          25A4A5A57D504A29D8265183CEA493CCDCCECCFD79EEB9E7ECB5BE827B73CEE5
          1221948A0F5DB0D86B9F75EFFE7ED777AD7DE0F424F1FFB41EF03E2710420008
          DC9889F7C824B50A04A0C8BE6CA103C6B3EBBD6E41F1C2539FFBC2FAA1D51F15
          213C48362D0593B279EC33DF7DF1F780BDFECB2FE6B49002A0657D902020A42E
          7DEA3BE7AF4F60FDE0EA6F4F7EE97B470EDE797211BA8DE36CC485677FF2F453
          8F7FFCC5EFFFE28D9D07BE75CEDF8DEAEDBA4C40AE23878EDE07CD859457565C
          295EEDF5B8FBD4E7EFF8D4F0CC4F81D3C08D10084BE096DDF3BE9B0139204F8E
          72BCB8361C3E71827B1F7AF8D13FFF8C47176B51A7F9624BBA3A80687A6D6BAF
          7AE2911FFFE51CD02C13409E81644BC0DE7A50CDE10F1EE5F0E91F4071DBF55A
          F55F9F8DB72E9FE2773FFF0DF061202EB72078F4F4DC6D1918DC01CF246BA887
          600E18F294975BFEADA5D81773915B6EBE0BCCEF5CBC69218405029E41F341CB
          04945794C0526CC80C3CAF6A5A022D293324C3572AEA2A02AC6412CA9E099812
          B8C7EB82A2AE223C1D8A3528CE515323ABD37F2D91F14C4216913B856E66BC3F
          03B80970A0C9ABF500DC048AC82264095106CA7B99779567F0663AE2ED7F6C32
          D89D800B1072474A2BAE14EB22A8E4CCE3F75C13C2C534463DFBC833FFFC463B
          0356576836451EA1AB2AED2D1172F3F4CC1A46FD5D76B66B8E7DFA313E72F2C1
          4E55D18D3FDEC692B7FBE9DEB583CFFFF09B9F058ADC0227CE4A6C5AA27AC6B8
          BFC53B57AE32AF1AC8D54810DC900B97583FF1093EFAF0D759E941F3F69FD07C
          84A4A45248D57B3B478667F5280EB0F9E6DFA86A3F07A8552056639AD110ABA6
          6C5DDAE0635F7B92B55B6F2799B2D3DE1297A3D916CDD53720D6480958AEC539
          CAE4AD1BD05E60E7D26546657CAE2360A29E8CA98623A6BB3BACDE7637070EDE
          4A73E58F283659DADC53940E0270C7597C7F2801C9F16E9612E16880F0067636
          AE5567FF3A3E0F780F08D618F3F188D960C8E05A9F3B1EF8249AF5F17909A8AB
          4A8900EA6E89502296C01270CEE111CFABDC2114F4AF5CA52A9B97CEBE359A66
          05C01A518D87947B23C67B53EE3D7E029B6CA6EA9501255C960E4648DE0207AC
          AD58128A31C96FD65E6179030A6CFF7D834965CF03DEBD071AA31A0E18EFED53
          AC1EE0964337515D7E19B9137A6B84A28703215F31296670476E2813C30C57EE
          B5722ED678AC913514BD35B63776B9D49F3F07583B03B13166C301A3FE8023F7
          DD8F953BD8640F2124501021AC127A0740052A8A549D2717069600DD1A880D8A
          CD7F62E59B2002E3FD8AF1B0BAF8F4ABBBFF02D4295047CAFD01E5A4E4F0F163
          34C34D2CD6597A4086790972DCE97A1C423B071E230143DEB62CB90B700281FE
          C68849A5F3E4EA7FFDD5634981BA71CAC1008FF081BBD669FA17F07ADE1D2243
          2690B787E36DDC8228934142D9C96D0B2B05BBDB13F6CAF80AE080E4A2071475
          658CDFD965FDE8ED508F68867D641170DC32901C39045956458B37A1DD07D1CA
          EE0894DC63417FBB9A9FB9347E212B807B6A81628C54E309F7DCFF219AC1165E
          579D94D9C995AA93372B026028D584210A0997C84962E30C76E78C67F6EAEBDB
          D504D0335F3E2A941468E635AFADAE71EACD972FF2D64B179080E0B8070200BE
          F09A6F030864F901D42903D0C6C25D5473DB7F656BF62450035EAC14E421A4FC
          F61F361F02D68062F91B6179FF2E72CA92CF8119D0FCEA2BC71DD41288400954
          DCB8056EDC58FEAE387DF60A5247C05B96FF7B134BD6CB4CF4BEFD38FD379B7D
          158A0D532ADA0000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F40000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000003D744558745469746C65004E65773B506167653B426172
          733B526962626F6E3B5374616E646172643B4974656D3B426C616E6B3B446566
          61756C743B456D7074793B130452ED0000007349444154785EEDCE310A80400C
          44D13D5FCE953647D8A3A5123C8416716D830A5996B19981DF0EAF45C4AFBD02
          08204055A3D83692517B680E50D98D30B3B3F72EE90B0770F7840003C632020F
          C8083420238E1B01076484AA0A02F0D5BE1A50C64D0108208000020820808045
          5500F808B800327A1D263F84669D0000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F40000000B744558745469746C6500536176653BF9E8F9090000054549444154
          785EC557BF8B5C5514FEEE7D33B3EE6E36B1D02882853F1A41D046540C1241B4
          B5B3482C36601A41831048A148F01FD022B516366A21181051D0C2422DACC222
          61A322E81A22C4EC8FECECBC7B7E48BEC39DF774D80109EAEE5CCEBB770EE73B
          E73BDFB96F37B93BFECF9F34BB47BAA958F3977796AB0F860CA0A1E59A097EB3
          E0155801080003E0831E40F3F29BEFBC315A583893726E524AC8D9F1F3C575EC
          6C6F33DF83CB4B78F8A10760E630535A3583DD586AD01BCB14A60E55C1DADA1A
          960E2EC31D70A59F8A94F73FF9F0DC090065268166343A7DE491079B9C123C03
          C3A6C1D98FCE6379296369F900F2CA019C5C7D2EF87300B41E00A0EDCE0C7861
          F5149E7DFA28133235987BF3D9E75F3E0FE064B000F413C86636823BBEFF7103
          9680C5E1B032171F37A83936AEEE76C0B48E13C78E47E752B87F7CFE03FA5EB8
          7009A24AC6EE387C1BCCADB6195D02B1928B43CC482B6B1A3488CA12AD9A41D4
          508A72DF4F20E5014C17909070F7BDF7A12D06538565836BF88B29FA629F49C0
          5C604C4019C82A403890CA228622DA07E773CA0D9AD420370DCC9C3E2CC4127D
          61606CB72AFE590628149510514A0E3705D10912622BD23160D6B190F3008366
          C444CCC2A702BA1B1719F1FD13486ED102332103AC8EBF011209284A11021BC0
          24C0041A0C4723348301D40D93220474846ECC13AD9B55BCD90454850CA81852
          4E5013A0A39994B645A2BA6E0268536AB0B872887B53431B0C80076422CE1DB3
          0CD4839863D5108D27B6C2BD5B95DA5622306BF10879E6EC59DC7EEB52F83942
          2766A45C482199658C7D5B605A0820AAC89618E0F05D77E2F2AF1BD8D9B98695
          8382574FBD0603A924D5608F09C4E7F8CE09B872E810CC8DBE6CAF1A8BAA7833
          09882844249848405B5A3CFED493481C4320E4CE0F0113590136AF5D45BBBB0B
          EFB342C11134C4EDCED6CE6320FA5F04AAC2FDD6B620F7D2F5C427027B02ADAA
          E28F2BBFD3A10A13912DAC6A84E318F131670A28C222A15EB7B8AFAC03EFD266
          1087A784EB9B9BEC710AB0AA09520FAFAE46ABB5553DCCBFB6403534D00A9013
          C0C084428A2D12174F48F378E73ADC957BC0C31F98566EFD6B5C0566BAFF1458
          2928A54044812682EDEE6C414B81A78EDAA8D2216D8148A1C098265131BDA472
          CE5858589CEE952FA5392D10DE01857D8502098632D9C3BBE75E8F96047C7D3F
          B09AC0440566325507C7564F63301CC5182B606270DF7F0A083E29424BB1C121
          25F420BD2ADDC1A5D6ED031EB1E7D4F0EA65C52187F92DE0A1A942DBB8E9728E
          40E63AADFA8BAF7F22A89A626B6B0F6D89AB59D598A888D2BE78FC31C04126CD
          AC261963A8B36398FA53D096021381A6CA8A4C031C7DF49E182FEB546E7D061C
          DD9B2F318100E4B9431A5E53FBBE0B20F1A2E14D9852CD5AA7E2FAF4AB75EEB7
          775A4C4461A26480D51783A8F0E27969F54865607A2BAA0359C9C03C110A8417
          91C641ED1BA2D2679EB81FAC41630AE2C3EA7A62AC3D07935123207D285AC47E
          760CA90183460B6A30F6D7ADCA9F60F5B12FBE2E21EE5358115888B713ADEDFF
          1711AA065404E869E0BDEF7EC3D6B840D4312E420DA8D5A02008E7DC416BD329
          316A25D5C41573FF222245CAACAB58A28F79B488810DA1E2183546F0C6007547
          2638E299ADE9C693FA5183A504D497D7DF4498C3F0545DCDD494A04E914542D1
          4BA78DEA120C209D5627C108CE67207C9C3D9FF6AA8EA4001062760C305E3B1E
          6FBF7D69FDE22BC9916BA06193F1ED373F6032919810E9FE09916A4599608C9D
          134835D695CBBFA0C7B9B5EDF5B7004C88D99DD30E01DCC2D5BF1FE6AFF40FBE
          F7A81C7B00C6005A00DED74001A0F12532FE9D1F8FCAA1F3FE3BFE2F3E5E377F
          02C73EB2E2D2D4F54C0000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F40000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C00000027744558745469746C6500526566726573683B52657065
          61743B426172733B526962626F6E3B52656C6F6164CD4DF6E90000014C494441
          54785EED96314AC44018852D766DB6132C245B5818C809B44B4E305749699B20
          8847706F3007B0F100DBA54E95C2C22B6CFFFB1606098FC9CEFC132108293E16
          927DBC8F4926FF5C89C8A2A802ABC02A605E3E0B50030B7A7072F4C0BA7B855E
          205C9C81160C40020CEEBF59AA00973F820F201A5CA6D40A70F9993BF0052481
          634822546EDCEFFD48A2030DA8C08DA372D7BA8995C852040C10927805F9854C
          0E0E1E89365AE0D9BE6F46E54212DB889DB2F3480C53BB83CB6F47E58C516CD7
          DCF338EAA00095CF9568286B7D1945B94E02998A72BD2FA32D6709CAAB31149E
          2BA1CF4F2D5F4F812ABCECFAC77849C052A00994AB5FE490404DA10EE47F2061
          CE5B3D46A0F04CBE03D845945F835F092ADF6866410B8425223EC56FE081244C
          CA2CC8780C2B86D13749248FE3121C816871127B30FB4052CE38903C2D7A24FB
          3787D245590556811FFFE4F2FAE6AD10150000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F400000011744558745469746C65004578706F72743B5361766546D49E310000
          068849444154785EB5976F8C156715C67F3373EF2E2CB48B15411BAA50AD49C5
          BF8126D47F69C06AFAC90F36DAC8176D52AA5DC5D8A62AE54389FD404514A325
          6A484948DA7EB0A218684BD3042B901636459B6E77C16CA184520A0B2CECDE7B
          97B977DE738EF1E4CDE4DEBD48B58D277973E67DB333CF739FF33C33D9C4CCF8
          5F2B491262F1BDCD37128B47060E27403AF09B1BB70DBF78E1FBCF3F717A1230
          C056AEBBBE0BC80C52DE6D99F1B3BB77F0F0AA1DC44AC1565EFFF1B97FFFEAEA
          EB960115207D7CDDB1E4776B77F2EFB5E1BEDFF2F0BDBFC6302AE52F7A87A506
          AA5A8A03646AC6DA3B7FB570CB8E0D7FADFC287B68FB86E31B81705575B100F6
          FAC49F0D1453C347D0462029FB954B81B4FBEFFDACF7EE4D1F9DDC30F024F5E6
          719EDDBB87A7F7EDD87362B871D7E0CE73A78002D0D1F127EC818D6B5C810EF6
          D31ECC15082571551DB85C54450CB5C0641865F91797F0E10F7D64F9B6BF6C19
          EC9F57FDEE738FBEF50CD0BCE19A6F0A605181C4C1573FB4F5C14AB5BA062333
          6078709089F3E32CF8E0752C5BF619CCCCE5563582084B3F75037B5F5BCFD9FA
          081818947DE3EAC73835B507804A3A8BC9C982279FDAC9C8C8E8E697768DFF74
          EC783E0914ED0AA459A5B2E6F34B1767499A90A5292F3CB39B59B35ACC9C3983
          DB577E0DCCB0882022F425057F3834CC2F7EF0188A122980EF5A1452B812C72E
          0E51CF6B7CE9D68FB1E0DA7903D5DE835F38F9CFFAB70FED1A3F5269933355D1
          4C0D4EBC314651088E65869A7AA724608828920A2246B09C13B53D600A80D334
          63E2D279CED64ED20C4DCC94372E8E307FD1357C63FEF24FEEDA7D60FF9C0FF4
          DCDBAE002A828952AB4DD1DB5BC5706042083878B9F02E2284601492D30A39CD
          628AA00553CD096AF9455AD244D51C5CCC30555A6993A9A973D426A6981C6FA6
          1D2654158209A60296E14018220135A324A1FE30487125C6EAA35CCA7307514D
          E849FB794FDF6C4E5E388294EA25F4F7CDE5D54363EC7BFEE8C89963F95D475F
          A8BF324D014582F84D1A2C4A0921080E6A6D0B508585F36FE281F5EB311CC47B
          02ACFACE0A5AA170E92B592F3D5CC5CEED87197DF5FCD6D1FDB507EBE7C245A0
          D9A98008129C049A09A8F99210508D847C2FB4F2264D0BDC79DB23C8AD0151F1
          51351A0DD66DBB95D93DEF25E8087DD5D99C7B4BF8DBB347383E72EE9ED17DF5
          3F019E0040DA4D98040984E803375E62DE43285C85A2555014852B658A93BAD4
          0CBE174B0902FD3366F8791E1ACCC8AEE6D08B639C796D0E9F5B38C073BFBFFF
          29A006B4005DF2F539D665C220FE6BC004148826AC4F34A2BF01538C942405D5
          94244BC90C4C32D2CC8971F6EC79766D3FCAB5336E61C5276EE3E9DD7B012E01
          05A08F0E7ED9366F18A4630412823BDB4720C6DCF7CF63ECF41948737E7CDF4F
          CA789591F466312DB87277DCFE1527F0F8D6216E597C0F8D5A1F2F0FBDCEA5BC
          051000DB727085052D508C6E1316E2249AAD82A59FBD19E27B00551430F13DEA
          6980BC5E63AA51070C0DCAFBE6CF67D1DC9B58B2E80E0E1F39C5E9B137997375
          3F60C462F3CF5F2256A702A108B40A57817ABD51BA1D55C42071326D51346572
          FC7C790D70E6CC053EBD6025070E1E266F16246478AAD41C03E0E53F4E5C9600
          1A42945F2841C0814CC177EA828329CD3CC70DE97BBCEF3F38E464CC80242993
          632A5D1FB66E05422014859388375282C6B92BB1ABD16C34B0389AC40900446F
          00C4FB832866D6050E302D86E27193A84008459C7F54A1349E22129C8CA5E01C
          E9342609719F945FD0F68A84A69B305084C21F6E86C76FCBA6FB89E3C588AE27
          8128B30118A5620A44D1F8D6AAB540078104881D03BA63188A80049F2B3E0A85
          20564690085A46303228C95982A1DE558434CD507DFB11003860E1AFDD80A92B
          828395A8A0D3E78CA1D394A0ED6B999096A6BE5CA51D269480B4CAEF81FB21CE
          BD13C4CA39A26D877EAE94631011341AD6541DE38A29D0102842AB4C818450CA
          5E26C0B7118C3652C4F372BAE011ADB837FEAB14203105A22E591C4182A25809
          36BDB78DC33ACF454254E93F8FA0FB635404EF31056C3DF026171A2D97513154
          41D441110555F32516AFE388AA3367E10AF87982BE0D01C3F145421132110183
          2042A577263D5A75603573F04C41CC481D3C5E7B5288E40C4C51159CB63FCF02
          20805D8E8002459E37360D0FBDF243830C8C4AA5C23F0E1D6762328FE3519FAB
          881262F791A93B1E1543D5CFF16AD4A9D76BEED6566BEA97402B629555FE5F10
          C9F4F8EAF44659EFF0CC00019A400E14805DEE4D1822BB66575CDE7DC580A257
          F20080F2FFAD2E27FE0B3AC8D37AF98699190000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F40000001D744558745469746C650041727469636C653B4C6F61643B4F70656E
          3B4973737565C206CFD5000005F849444154785EC5574B6C95C517FFCD77DBDB
          96525ADE29948487C685096025716308B210E22BFA8F60E22B1157480C1BB7E0
          C2C4C74E17B810E54FE2420D318A8144890A110221E11513791AA03429F4C57D
          7DDFBDDF63668E9E6F2633B4D7525818A73DDF37736672CEEFFCCE3973EF1544
          84FF72B4C00E21C4E43D81E6713F67E8CEF75481B64C6134B06FD1ECA8793EC5
          5A59D116C47400BCF38F3E3BF2B308C43A5E18E064FFE11E56EF753ED4DC8A92
          F2B71D6F6F781240765700CDD4A3008175EF6C598BE9074DA9FDE0D35FD9402B
          00D99C629F929649D4050C80B4D98C53E54C36F973F10BC78E10AC11E82816A0
          8D83C274B5D4E272EEA5A83559DA3568023EF26B220F8EBC8AC89C313610DC4B
          1704473E5EBBBE6766EBFB20AC89338DE354430EC1DAFEE6C0EF20976FE16BC2
          5169227FF9B9959E1DAD7167319FD9F504F56F3BFCCF007A3A5BBF5CB161FBC2
          F6B94B51AF3770E28728B7AE61C6ABCFAF9A905F5F9C562CA6542AC78256E4EC
          B3FCED5C3B13930190A68533173C08593B0B8A62905AE418E0C7DE6FCF8287AF
          7CFB24ABB39BAFBCB09AD1E43A4DB9BF0E00098014806C6E4B5B03F9597E680D
          AD2447C0E28CBFFEBF477CA077698838930E6851D770E0DDC7AE7577B6F2DE99
          9BA578C7E60F4FFD32B92D5B5CBE4881B484961917D084FCEED9779A17DE3939
          06ECD2807DED6FA064D7858E2EACDAB413F3E6CE423236D82FF6EDFA3F806500
          64531768C9D14B402B689581943235400421802D9BFAD9C1B4A39148973A9D46
          48CEFF84A4BB88F69E2580D20B4C3DF8ABDF03E08899019542672914AFAD4726
          67F7D7A7DCDA2271ED68F5868117FB7996EB945250F506549B04A509D2380FBC
          6041108B07A018720662C96B409B28ACCF375F5A33F5E5E7FB027566C032475A
          218DCA50AD45A8F6126AA50600B4DB22CCEC5B31003000CE3F65123249A0B918
          6D3BB1E97A9C59273EE7821D25558C9FFE1CD1C0D11C2DB158E74FB503E1B846
          381682AE8C635E5F37F6BFB5FC1681F86824257DBF79F7F5375C0DE8B401D988
          90D64368A55DB3314C41C2AEE180448327513DB7178B1EDD8839EB37DDC9EAC4
          5BD2A548BB7574FB56E7A1F7B63E0E20B029D0908D3A9230425CA9627EE106AE
          EEDF86B472D3440532C5C07332EFEEA52BF1D0335B51282824D7BE03C5658B81
          4082AC2F7B9EC8769A060AED18BC781D71AAB925C93190356A48C3088D7205AB
          8AC7F0C0BAED68EB9AEB2261F1534E91828E06900C9D02646A8A5110C8B58161
          D1A4C5386710A2B580913F0750ADCB1F3D0045B9F37AB982FAED2A66F72E41B1
          7306922B5F8164EA6965638E4EE46B33D3B01BC611515E476081618059E6412A
          C0C8E0687CF08FDA61DECC01A84C21AD9573FAD3284257EF72E870083A8E8C6F
          4DB6BD7C5E35F9FCC247691CC38005B3C46FAD0C43410163436388EBD9D18317
          AA9165005019210EAB884A21549C6076DF52A8CA75E82CB3CC6B77BF4F282A82
          A115D6B1D59152209F77D719A250C4C8B55B086375888DFA7B20536854AA084B
          55B475746246D72CC4574FF2A56423366D29DC6784326A13B92B4C686D41B298
          3DAF2304040CDF18C7E5D184F3AF5C0DC84C21E6FC571BE85BF130547D042A2C
          B93B9E601D30138AB5363A1BA1DD679DA15A3B00BCB667046AA518B54A7CE993
          13E303136FC25422AA5490C429E62C598CAC320825534B1F5C2E4D8E612254CA
          3BE63FA5210457BA2B442316B880C0E88D2AC2980EC346BFE7D9C5868134D3A8
          572A105A60F6C21E64A317A1D304E429CF2307B99CFAE8BD13C3802D52B202CB
          8A2804181F0E71BB2E8FDBFC13836300411A2B44A532BA7BE7036915596514A4
          647E4E2BEB88450382C1107C8E2738D51004DF1156CFA26580D1E138D97FB976
          C432C0D8720024A5441C4658BE7A19B2F210741A7B2AADC0464A9E5E57F56C8F
          4C4C3C4360DBD46E42661AE5F104B5863A716E380E7963F7C65E021906B224C5
          99D6B6A0FFFCB14BB870F4A2895070BE3973FEA23181BB09204C2798419E195E
          BA39E5F51327AA747CA8B1D37E3DD34121709F2C45FB31D9E6BFC5FAD1B4BEFF
          3DB29427001A1CF0174FF799889A7E13DCFB10B8BF61AAD408B1F85F465EA9F0
          EF0F9AACF80BA05D20D02252B9300000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F40000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C00000019744558745469746C65005461626C653B466F726D6174
          3B436C6561723B1A97D51A0000079549444154785EA5970B6C14C71DC67F735E
          DB38726C134193F2083606038D018B36A90821895B8828843E5355D0AA55A206
          B54A2A554D4BD24A69EB80AA84568D95B401430221C1B40D6D9A12688481808D
          31E010283636061BFBC0BC6C5EF1E36EEF6E6F67BAB73BAB954F8714299F3C37
          B7DAF3FDBEFF37FF59CD093CA5CD9F590250045200CA51A60F86FC9166407C7A
          D8ADA5E112B001996EC200B2BEB3FAC3DD2294F5109F410FDC150ACA562084A6
          2B9076B2E1172B2A17EADBE90608A5E04F2FABE056BA706990F19F2FE0566A3A
          7E91158BCBB8956A6A9B1ED42C9B34197E842868BF122193CE9CEA67508440E9
          8F0A3562A9FB6E44895B32BD385082DC9C103A75419A8410810109DE07050825
          829C5048A908564EF97FC1A554A0FCDC950B56489494EEFFDAB69DD1C0F1654B
          3D030E587F09A8145C68829654521B508C9407B6A544795F844C0D69934C26BD
          D916585622A3013B696B0312C2BD37693F7D357DE3F831D3DE7AC1BFD69106B7
          9582F7779F70AB955262272DC6B6EFA35C5D736F4E88E403E4027121846C7EEC
          6B4A69B38687514C1C5F44C4C8498BD25BA7F696F3CC983971A4B740B49FE865
          C98299D8B6C44A24183E7B86BC6B0663EEFF362878F060233B1FAD7C6EC98E7D
          BF07ECFBFEF9811C3E58A78EBFB4C64BC075AE082A93CAAF0CDC482128DA2F3B
          B022ED54D592841523D2D549EE477514949612F9DF51508AC292494C9F3FFF99
          9DB64C2EF9A0BE0A48E4CF7B44EE5B5CA93C033A0EA48E5800D2EB76A1706375
          EFA767A0FB2369DBC4E3A6537927A30EEFA2B0B484F885F3A0943BE2BDE7195D
          3C8929F31E7876BB25D5D7F71C5805242AFFBBCFF696C05D3BCDD7D113BC45DA
          8A800F412F48B77ACB810F7676907BC8819738F0F3E7502E3C6854F35C983B26
          4DA474DEDCE7B65996F86EFDE155405C00058B9FAF1B5878EF245ABBAEA63597
          224D813FDDED7622C6B8781F4FE6763A55DE4DFCD245006D2018FEF56D6565F4
          F55EE6F8BFDE5DF383431FBFA07B004A268F41E6E70570CD1640EBB130E5738A
          03034ABA0D9774E005FD6759D8DB41E1B88998E1B0074C378067C028284014E4
          635CBE48F1AC8A955B000300E5ADB1929A28D1748554C28179654BA9F4832989
          158F717B7F370BCE35307AC20407DEED47A33D8C4C20ABA09051D3CAE859BF11
          D312E4FDAE9AC2DA9A9541134AE547AB5D004AE0ED5789ADA407B76D12319342
          A7F2AFF6ECA768DC78A2DDDD1A98D94056A1866FD884990C91F3EB35986BFF48
          F68DCBFE36F49B5069B4421118C196A0F4568B45DDD82BBB3EE48E3BEF227AF6
          2C9A4C00F62F1C7891070F6FDC4CD476E0BF7989C41B2F133EB097363356EDEF
          020E1FE9A6EF7A64C4331EBFD980634D9D242D13695B7C63D19788944F256FCB
          9F3DF0C8175D3918297899037FF36D076E909D82BF5E4D4FC31E4E4463AF54F7
          5FF59AB0E2F638CF7FBF8278428248872BFEF1DE11163D5C4E4BF715868CD17C
          61DA9D0C9849FA1F799CE93BD6A7C10152B117903B7D0AE1B76B315536D94EEC
          C937AAE972E0C723B1575FBB7EBD0A88FA4BA0E3F3831728DDB949CB22168D70
          A2EB120359A3B967EA58FA87E20018E5E59CB09E60E6F6756879DDEEC2A7D2B3
          E51D4C72527037F6CEFA3D1C33CD5737DCB8E9C201CB0077EDF5D001B8BB0217
          3E343444FF70940251C48CD2B15C1E882394F093C2B86716C7E23F66F6F6D700
          2FF6DC69A584B76EC314A3309CD82DA7F233F5753447CDB56F0D0EAE02224002
          5086C79308813E3C78062C2B492C364C5BCF158A2B2A985C3C868B9FC482E396
          DFA30844F96C9AE32B98DB584B6ED964C2DBDEC30CE5613C9B82BF4C47C36E9A
          86231BDF89445643007F3C3FDF3560C513F1FDBF5DF39F871500122595BBCFA3
          D2E68B95F3292D1D43EFCD1821083A5E2F943F67CF99C3F5680FD6D68DC48CDB
          C85AF922C98DD5B437D4D15838EEFABFFB5AFE000CF9F053DF5AAA56EFDA8B00
          72805C3D670142CFF9CF6C683EFDE57B8BE94BC50E28FC1313282108E99E31B2
          4348CBE240432715A7EB99F5D8A3C8375F71E10722D14DDB63660A7E4D579F6C
          5FBA540154EDDE830124011B8801420F0350494B72EE6A142D0D573A79815290
          EDC0AD88E9C0CF902D2525CB97D1B17327F1FDBB6834CD9A1D31733530089880
          DDBE78B1C2B6014848E982D21FC0FEFB684B6BDB5F4CC5D3138BC77A7002B9D1
          39F068244AE3FED3E408C5371F9ACCAE8F7A3918CE4118E3D69D8A9D7C41C3E3
          80DDB66891DB6FBE924A79E7C1F4932A1002B28182FB7FF4D7AA29B367FFF4EE
          92CF0141F7E5E484B0E3110ED677304A08967D65AA0B6F6AEEE06AB8BD66E0F4
          A62A6040C3650A4E9A7EB97B2F21324BE9A5196EDAFC54555BF3D1755D672E12
          4F5ACEB091211B736080FD75AD1896CDF72AA7B2B3A987034D6DF4779FF4E183
          99E1816C5466030F3FB54D015277ECD0C77FFF7955DB91A335673B2E0110BD39
          C0BEBA160CDB66F98232DE6FEC72E027B9166E5B3FD8B9D98F3D06C8BFAD5CAB
          C82C6CA530C8A0842599FBE4567568C372A9AB186C7DF7575596F5A2EA6CEFFD
          89928AD17982F965065B77B5D0D36F71A3B7A326D2539B827FE2573EA568867A
          FD4F3F0340A5BD6A65EE81FB9EA845DADE19E1D85B3F14BA2772803C3D0C40A4
          2D570C88EAD4E4E4A2E9AA78B01319744E2665369041428F2C3D8732FE020E86
          E253EAFF4E60C805383D93BB0000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F40000001B744558745469746C65004164643B506C75733B426172733B526962
          626F6E3B9506332F000008F449444154785E8D570B7055C5F9FFED9E739FB909
          E40121E4C13320963A80888DA47F4450B41D915AA0481D6B6BA7B5159D3E9CFE
          517CD4D13AB66594DA426BB55AB1A530814A5B194B41D1227104E4116C54484C
          02216F929BFB3CE79E7376BB67F79CE44A8769BF99DFFDF6EEDDB3BFEFFB7DDF
          6E4EC8435B1B410801A104D4F5181D43780200EEBC87076E9D437069C3E30DC7
          38631C5C407AAE3CE30C8E333AEF30079BEE5E04DD5D70CFEA05504CAE237963
          65E541CD1F928B906F5C000FAF9AE77A896EC3E1DCFB490C140019D4D3AF344A
          0EDD9F24F2237F9C4F0C2A40D63FBFEF8A6861D10AA20596504A2294D02B00C0
          B6ED53E0C8E672C69BC9A1C1DD9BEEBBB90900AB086B0C003F9FB5392E322F32
          E80E93A33C620E0E820A45EE427BF0A5B7BF142A2878626C41B8765A55312AC7
          C510D435948E09CB2517868D39A665E35C5FB2AEB5B368C323DB0E9FC924861F
          DB78F7F53B013895119D75666DC5E4ABE0F1522EEBE4FFA07C454892D3754FEF
          9AF2C81FDF3B585359BE63F9C2DADAD54B2FC3ECDAF188C44220111DE7333901
          1324AC212CE63E3BA31C6BAE9F85158B66D64E9D5CF587F52F1D3AB876FDE6E9
          00F4AA884EABA33AE123E5E0172BA03E26467499F5F77FF5FAE2C231C53BEAE7
          5417CF9A3A0E71D34647C28046FC1E21C271F95CDA71C038306C0A2F06D17000
          5FFCFC7434B7F65FF54F90C6BB7EB2FDF6DF6D58F30600ABA62080F694C5D9A8
          020C9CAB8D2A3DF275CFFCEDBAB12565AFDD7CEDCCE29AAA12740C1B4898B68C
          5C042CC15CCF213C94E7A35D3F6C38681DCCA2AAB218B72CBEACB8BC72D2EE3B
          1EDBB60440C06FDE91001C47915729727AE78F5F9E1E1B5BB24D6410A4A1207A
          53A6DC5C40914BEF13FAC17881F8417145D09530C10241AC587C59A0AC7CE22B
          CBD76D9C01409F1C0B505F796A598ECCCC6FB8B1E3AB5FFCBF79934A1D5DC390
          9193646E90CC7149946F69EDC3A6CDFBF0C307B6E3FE07B7E3D92DFF406B5B9F
          DCC776B85C6F7BC15D48E760100D8BAE9A565231F58AE7010405E86F7FB0542A
          41932243511799FD379F7A75D5B892C2BA4A21DD60C6F636637098DAD0F7DB76
          3462E1F4623CF8952BB161CD955834B30C0DBBDE0330AA0473A45252B1BE540E
          1515C5282F1B73F59A07B6AE02A0FBA5D0B38635927D20147BF49AB935E88C1B
          EA5832B58C7001E19977309329034B16D4624A751902411DE98C813F37B6A9CC
          051863AA648E07316EBB9045FDBC4938DB39B001C02E015B80D39D4FAC90D9AF
          FAFF9717971517D406A311983906CB7BD8F260BBB0D59C6BB158087A50934405
          D130745DF308D9C85A4B2926BFE72C061209A37CDC98695FF8EEB3D702D00488
          0E6534C702CB26579608B94C494601700210AAB2A7E0A0848C5E611C5ED713B9
          2610A01E1983707014B1AF84FCDE3D640AD54AD1DC3CE606006F08D87E2D28E3
          FA021A0CA23F6E898C34689C836A045D670770F06033BABB872EBEF82521DC75
          946260308DC79FDC09DF264C28467DFD67306E4289EA1F9B216D31140AB54083
          F33D0530A2000399C902010C244C24321A4A8A748483147BF61CC5B23995987A
          CD2468BA065DA30808CF38918DC641609B0E7E7DFF4D48650C2910A540477702
          BFDFFB3EEEB873A92C692A6B833B0C05B10038D56AF34B405C70C68A723691B5
          1363F40EE6100E6948A74D2C9C3B05D595A508057559062200019B01E04CDD23
          134BC0F22EA4DA2913B065F77124330E12691B8CA9D3643AF291429F97421971
          9B2769E4E426B6B7D8301CD570D110344DF38E21BC9A3209C605F28E29938008
          3E0000B2F9DCBDFDDF52394B7E1F3986F0CCB659226BE44A1D682A4B4EC12950
          5010C6997383A8A92A050154F600D2860D65EAFA2D8804DC215451084E7CDC8D
          68414836A24FCE094336CB0497938467BAFFF2E058B9D366DAACA3D1286CB913
          0301455DFD6C6CF9EB096C6A382CEBAF09E89A86171EBAC54F02B1888E5B7FB4
          1D94C22F910C7CE9757391356DF9F6C3993A5556C680651AAD0064017D0558CE
          CC1E4DC5537585E12818E1B0E5F60EA2454558B97A3174AAEAEEDA8B2FECC170
          228B582CACB2E69035FEFA376E829163E05C653D94B290CBD9925C7CCA6C5343
          49E4B2C963009C4F05904DF4EFEFECE8BB77D6C471702C064EA894326D3AC8E4
          32D04080D11854B339AEACA38DD73D682065A82E63720D6430FE5A4DA738D7D1
          874CBCEF801F00F54AE01C7FF5D1B77B7A2E7C32D4979033B2C16CE6351B8725
          BC7FABB9A6530A47122BD90589FA9D3158729DFFAC24972C83FDC3E8EE1E683F
          F9DA930746AEE2ABEF78817BF5B0D2F1EEA7DA3F6A03A8CACA71617B70A03CE3
          8846C368E9BC80B1B1104A0A433879A60791481896ED28E2FCF56E120E97CAB5
          7FD48E647FC746008617007436F2A606EBE4EE871B026BB77CABABAD67FEF8EA
          09F23E00A1A0840302C45B39FFEACBF18B8623F8E9D643702D1C09A26EE16C64
          0C47F60207469A8E73F5DCF9F61EF476F59DFC60CF130D004CBF09A9FBC0BBE7
          53DCAB8939D871ECDE96532D4389E1041854296C019677CEB5C2186E5C5E8F95
          B72DC1CAB54B70D32DF5704251E4A4ECFE5F40E9A592897812679A5AE27D67DE
          B90F404665AF8CCCBBED37D8FCF3DBE15A5D554C07109EB1F4FE65A5D597FF69
          E6FC5981A29222990A910A788D887C53133CEFAD86E5BD66272F24F0E1D166AB
          BFEDC4D75ADEFAE5EB00D202F6BC35CFF163DBBF0DCA18C0A1E0AB707AFFC67D
          FDEDA7BEDA74A869B8E76C6FFE8B8602870097F06F42FF1AF65502077A3A7A71
          E29DA6446FEBB13B05F95E3FFB773B539C7901EB7EE43E3C79B22D079EF9FBC4
          392B6FB0CDEC7362A33993674D86AB06710908C77F9AAF1047623089F6E6760C
          F5F636F59F3EF09DEE537F69F6C82D5F1C9F97BA033916683C97E2F941749DD8
          F9AFA65DDFBBA1EBE3E3EB8EBF75F4EC91FD87F1C987ED880FC4918AA7C1D55B
          B01827111747ACADB90D47F61FC5F1370F9F3BFFD1FBF789679709F2534A7645
          7E48707CEAFF02D7939112A820AEA98E11008E7F5C5ADEF8D92B0076D67CEEAE
          05F19E4937EAA1583D40C2540FCD90E5B1CDD300376C23792833D8BEF7DCE197
          0E03C8E61D37C727271E992F3C99FDE5CDB8947DB0EB9EFCFF0D3581401E3439
          AF8C79B0F2E07880E0E097D81F04FF9B913C4FA5F7A18CE78149EFCFFF17FB37
          66AFE935F6829A2E0000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F400000021744558745469746C65004170706C793B4F4B3B436865636B3B4261
          72733B526962626F6E3B6463C8680000098749444154785E8D570B7054D519FE
          CEDDBB9BCD6349480C7910214882488804C25B5E2518F041A5D5215550192D30
          BCA28E531D713A6851C6B1EA0C68950A5A2B2275C096F2465AE340B0BC820931
          40020979BFC826FBCA66F73ECEE9B967EF4D769C71DAFFCE37FF393B9BFBFDFF
          F7FFE73F1BB2E5E3FB21D98880CD80046BCF01489201BE2600B111AC7FF008C1
          CF1BDE3FB284E99482E90C3A87C641752ABCAE510ED3F3FDDB1BBF87CCC0F0EC
          F29520C643220020F6109F0169714F58A4E4278836C6814D0F9F30BC40AB7F2F
          63E00F63A0CCF0D45C53ECFC72B7E09169E42B0695F0E235661019F183C41207
          D9B6A7E8DE78977D991C438A2409B184907BB987A6EAD58C61203CA07FEBED0B
          1F7CFDB767AE00A059AE1514006BF6EE315F6D1A231C919D0C33326A0621C819
          9099F0B495A5EDADBD45BF8A75C96FB8E25DB9A3D2C6202D39030ED981C4F814
          18E6EDEF2D50B4103ADCEDB39A3B6FBDFAEE3F8A6E04BCCAEB5B569D3900401F
          95F8246DF47C4605970188A5A90035369A491CE1CC723D6B2CA49777CCCEBE23
          3D6E6F7AEA88199372272325291903AA07612D00151A3AFC4D4235598A059125
          64676660FCE809E8F5F5E656D5557CF1E6BE79CFDDBAEA796AF7D62B0DD949AB
          3400ECA67B1783A9BA59021D0688F98C1EB65664BD79E79C5F240D777E559857
          387C4C56167CA14EB4795BCDEA987D42221969340C4A19FCAC87EFEBE1B02560
          56C16C34B6B64C03BBF07DE91F0B57EEF85DC5BF01A83929AB51DBB393314A87
          14A0D0203182ECA48D82FCA5EDB3162627C71D9E3F6D8EC319CF33F55E15C406
          3B61C259120A0C351A85A1685077C31FEA414A4A06164E2F1A5E76BEECE0FA6D
          05CB3EDC5C2982E0ECFC311550354D9460CCF01784EC6B5F9F9CE31A1EF3E5FC
          69331DD4EE86271830B32591004874374522B10415C930030C7575DDB878E93C
          1E7D6406164C9F6F3F1E3AB5A7E4B9F10BBEDA7EBD767CEA7AADF4DD422A0208
          0407409906ABE192D3623E2D9C383145957B785BFB20116236272049569FB021
          723320EB34793D611C3D5A8B8CC44528C85E8CBF1FFA14CB1F9330F5DE29C9FD
          81EF770158C44177BC58218E2A79F2F779D8B3B5469097BE3DA564D4E8115FCC
          9B33017DC15608498814A580F05607588D14E96C063434F4A2EC9B3E3CB6E80D
          E48D9B02095EBCB4A318C50FDC85FCDC3C949FBD8ADADAB6673EDB5AB30F80C2
          4165C6E860F676A76DCBA4BC1CB8032D42154244001C881C52CA4186C80931BC
          8800577FBC8DCA0B4E6C28D983D1776622CEA9E0B50F4BB172F922C4247AD0ED
          6FC0A4BCF168EBE87E15C0D71C9A50C0225FFEC2B84513F2338F2F583016DE60
          A718BBD663B0F22D303425ADDCC52B2A2A3A50579D8875251F223B2B034E8782
          57B69760EEBC4C0CBBA31FFD8A579C92E4F82C9C2D6F407545EB23073FBA7912
          802A5B93CEE69016DF91928EF6BE7638EC549C0A227120527BCAF792352BC810
          7979791BBA1A33B0E1F10F90959106BB1CC6CBEF95A0707A3212520308843DA0
          0CE23EE8F6B6237D443A6E24741603304E84260356A9C9F4D804825E7F801332
          24C5C7F04024681AC3951FDC98322D15B24D1A9A011C953FDC46676326D6977C
          809199A9888BB1E1FDBD9B917127436636833FDC2732D7548A605813AAC7B952
          20C9642A001B07061560C0DD924383AE68D028438F974296249CFDB60B0E9A8B
          43CD4DB8FFE1118875CAA2048DB7FCB85AE944E98AF79099910A1B9170F1C723
          B8D57916CB7F5300777F1B544D87A2E8DC3350508083385443D15C3300622940
          18C3308D0C40339A8F49A0D071E17437D212E6E08987DEC08DC6CB38786033E6
          172741A70CE5653EACFEF527B833331386296A37761D780DCB4BA6A0C5DD24AE
          5C0AF3F603336704A02304C6E0B2782D0508A380AA0D80722F1106354CE0F3A8
          D8B86C1D46A6A72039693E626376E08B63CF43D1827862C976DC33F61ED1974E
          07C1B68F5FC4CCD963D0A7B440D535733282830ECD09468DBF1565B16A29C134
          5D67BE50488D1C30CA9010EFC4234B67E283FD1B40582F9C4E3BF22714E29965
          BBF1C07DAF60EAC4D9B0C936714CCF5CDA075FB81EA923091435049D52014A75
          3026BC088401E01C06971F51F73C33A02BAC2E1460002542AE012500D5D18BFB
          E66463CBCE1590A5001C7619E3C78EC7FC69CB6077D845869AEEC65F0FBF85D9
          7373D117EC86C684DC11586B911405A180C1A186683D00CAC12C05286F964BBE
          3E5D9C320A26E00FF6C296E0C3DD79097873D72A0C8B6590650971713142E218
          AEC09747B76162FE48045997208B266783DE04213038C203FA65003A0793AD00
          825EF55F5D6DC14D29A362C0A80A888621F004DC189E9A8AB410C53B9FAFC18B
          4FED465F4087615D3D55A8AA3F85E287F278F6ED436319885A47C8B913B3A5AB
          AD1F7E8F5A2602889E841CF14BD7E55E2E98EBBA2B2E915A3FD306EF80545726
          6EDDF023D1968F358FBE87B0A2E10F1F2F436E5E0CB418237B8B58500A811933
          F72C32B6FBBD12AACAFD8D87FE543703809743911E5C9BCBCC7AA8813EE5AD5B
          D7544844162FA48C1A10EBDBBE768C1D9784AEFE0A7C75722B2ED57C0D6AEB86
          1CDF0F4DD323B2EB3A984E394CF961958342820D4DD71478BA43EF0008997701
          64668509A8657F6BDAFFE09A9C351D4D646A6A9604C62C251828083A3C4DC89F
          340615178FE1C4B9CF515434113E437A625E4E11A5C51AE2D8411881848E6605
          3D9D0355A7F737EF07101E6C42C6DDF19A0DCCAC49B8BD3EB0A9A126DCD7EFA5
          919751F33C9B8DD4D2DB80A9D34763E5E30B11827B5025F613C5740630018280
          97A1A17AC0D37CCD5B0A2028B2378D2C7E36079B9EBF5FE4B834FFCF3200E7CC
          5F662D4ECF8EDB973359B6BB9225B31798353B84271187E84F59D4860976C0D7
          ABA3BE52535B6F069EBE78B4FD38807E0E6DC9EA1C7662D74D4888324B857387
          5A4FB537F4AFB87E5EF576B7E8603055B0BA1A667DAD7F340C44D79C463EEF6E
          D570FDBCE66BAD0DACE2E427ADEC0F57AF638C12444D420ACB4C79062E1C693B
          515FE929BE79395C59F31F05FE3E5112F35C4380320C0560B63277E2BB57CFA9
          A8BBA45CA9AB703F70F144FB3100010E75888F5901584BE048F55A161DC4B573
          B76B4E7EDA50DC7C2DB8B1FA74B8B9EABB305AAE6BF0BA75881E31874C90AF7D
          BD142DB52AAABE53507D5A6969AA09967EF397FAC57517DDD5427693FC30E720
          1832D99AC804148C7B439EA5F91F1100BA755CCEFDB3750F8003050BD3A7BBDB
          9D4B1C4EDB1C02E6B4D9A571A26E2AAB63602125A49FF576874F5695755E3012
          883A6EBA450ECBA29BF0E7ECE42737AD60257358D92D98FBC1516E428D826E02
          9C83FDCCFB41F0FF1989F292E94D086316A20B2CFCFFB0FF02A614B488B3A126
          BC0000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F400000023744558745469746C650043616E63656C3B53746F703B457869743B
          426172733B526962626F6E3B4C9696B2000009AA49444154785E8D96097054D7
          9946CF7DBD6869ADAD1648AC025B8AC35694C106EC84C910E2B552C626768D17
          927126953126D86106B298941D3B4E4219882B95C2E5D8659C541687C5186C27
          98D8C090106C30036637C8C6420809099044AB97F7FADD25AF6EF54B75854A25
          57F5EBEB5BAA7AE7FB37BD165D2B9E020460E5CA63C00840DB0BA3BFFBB8E01F
          1F3A9E586E0C06A30D020255F633068CD14198E25DD3B6FAA744D186E6871610
          3A305C79A2C996102A4AE34AABD0F2E40F0D60C3EF395E2483556D306830868F
          57BD0440545B57DAC229C50B1180C785100710FB962D995253563E2FE2389F17
          980A4730C56883D2EA089ABC2BFD1D17B3D9CD739E7BE130A063CD13B435D27D
          D4A035C69AB181560A6BC028EBE8CAAC1BC68759460E7D67E99D8978FCE9584D
          756BA2ED2ACAC78EC089C5890F6B000C5EEFC5A9DAF3C87774CDAA3AD1BEFCF8
          D225ED97F3F92767AD797E23A0622326E942D7618D36802D3F4A692C474985D1
          BAA4C286D8B03601383B163DD432B2AEEE378991CD33EA6E9846D9F006E4E020
          EAF265D4904FA6F323301AA722018E43D55523A9BB6E326EEFA5D6F23FEDFDF5
          A1C58B1E3D70AEFBCB0F6E7AED747CD4140918AF63BFB11590B2D802A5213400
          C49AAEB159BFF7E8E27F4F5627D6A566CFAC4FB4B550E83E47F683330060DB66
          15B4467A0356FDBEF3D650A4A696E6DBFE8DC4C933D74D7BE7CF7BB6DCFB1F0F
          DCF1CAEFB6037E59CB74BCF6778D94AA68404A821B08417CE4440BDFFDF0C239
          C9DA9A379ABE38371E118ADCD143608A6C0CA258CA709AB193AD8350A034FE85
          3E0AE7CF53317A0CA3EFB9BD5EADFFFDE64D5F9A3FEFAE8DAF5A135A29A38B06
          C489258F32FE9B0F523676AA2DFBE6FF5CD0DA3A7CF8EED1F36F6D3099016450
          EED2F10C81188A6A570BAC81623B95C2DE038DD62711A966DA7FB5A57F5F67E7
          E716BFFDF64940EEB9F73E7DC32BBF255AB89C06290907AEA5BA766DEAC6E90D
          7AE042001FF8BBD5B4E0A2DA08E1816A9BBD3514A8D1D2DEBDA01231AFC08839
          B392535ECFBD08CC057400B72938856C9EB2ABA6DBEC777DEDAB775734266755
          D49653B8D887F16510BE0D7730CDBEF56FF187956B39F8C6FF21F36EF837A4EB
          72F0CD3FF1FAAA5FB267C31F71D343185FA17D89917ED08E6E2A130E95C31B66
          BC72CB6D7703514060B74029C2EC134EF489E4B409E43B3B402A8C1000F83997
          FD01544D9A49E382A50CFE7E3DFB376FE7DADB3E0BC6F0FE9BBB709BAE66C4D3
          4BC8ECDCCABB9B7670FDED3752561643DB762872A73E64C48CC9F4779C5B0EBC
          0A48C08810BEE1E65BE7B64D6CDB3A6AF654BCDE1E848563F5E05B7BF0DAA691
          BAEB7E9A5235F852717CF50AE2E74E5903B9003EFCBFBEC1F0C65A9C489413CF
          FD0C3ED8CDB573A75938520722291F398A8F769FE0E0F153772C7EFFDD6D801F
          05009C84706E8E2562648E7C40A4B61E1171084B933E7F91D9CFFE377911A3BE
          B60A84C159F65D8EAEFC315269863DB888A6A624A9861A6B68F6D247D93C770B
          F805B432D8E194926C7B3B895435754EE426603B209D22C38918737D141FB7E3
          1332C70E2107FA318502DAF71976750BA75F584373633560D01A1A1AEB98B8EC
          31C62E5C4273006FA8AFB66B99288B7078D533345D3D06E54BB45F406632B83D
          3D76AEE2514D0C311D8800FCAD0228FD29BC1CD22F8067C89CFC9068A28A6843
          8AB6191338B6F3001FACF81153BFF318195701D010649C0C02C000D5017CFF53
          DF277DF400533E772D7E4F376A68085570ED7A8220EA7B08655A8B0644388D42
          6A5D433E87F61438B6F7F84369FCCC106E572757B58EE1E4FB7BD8F7E4F7B9E6
          5BCB01A7F41D82569AF77EFC14E9FD7B98785D1BD96347304A85AB6A0D0442D4
          CBA095AA0EB9610584F25500CBDA61C1080402843562F73ADB711AE3BAF4F70F
          71E14286C692CC35964376286FFB9E3DD381830583213460CDF8D92C526ACB2C
          6D014AC9B497F71A1C5DFCC7220442601504672FE429B44DA7E6FE8731029431
          166EF106C061F423DFE6D4CA1FF0F1F1BD8C1B596D096883C15805819BF19052
          0E855C073080710BEA9497F7D15A61DF904AA203D552D2D3EFE15F731DD50F2C
          64585392645D0263347595116A2B6318FB7C4D32594DEBFF2EC74C9C41674F06
          2D354A29B4AFB0AA35D94C81BC941F87850B2BA0B385C2FE4C363A2B562940FA
          48276C8320EFC3E4FF5946D644B17008E0B1600D7F64E193826DE8CFF800D427
          6B98F9F813ECBAF3566BDE846F4E6D6CC3D3699F21AF700050941AE875F3EFD4
          A5A38B6BE2805416AE1D70105426AA70B7BF4ECB82AF9175253515514EAC5E41
          FF5F76D9FE1E133061E963A4733E5515713E59BB96EA9A4A942C20B4C1983060
          60C0A5CF7377961A30805A7EEAC8AEB59F9E763A1D33E3AB2B44F125031A8761
          C3CAE97C6D0300E3EF9CCFB155CF7369CF2E4635558380AEDD3B396E0C931E5A
          48FBCB1BE8D8B88E31236BF12EF4DACC2DD97118E8CF3230A43A56F77EB21390
          80717E71CDB47090FDEE7C7E45DFA0414A85B2FD0F5449DCBEF334A712F4BCB1
          8977EEBD9BF4FBBB696AA820DBDD4DEE5C37231A135CDEBB9BADF3EFA27BCB46
          468F08E07D7D685F15E748E37B92AE8B8AB36E6E15E002B2F832D2E136F9DFEB
          38BE61CDF8C95FAFECD7D31BAA852D3F8E00A9C805B0D4B01491541299CB920F
          C0DA1800B2E77A48A5523407215D975C4F8F5D6761849D01AD0D3DFD3E035973
          68F5C5331B008F70089552EC9833DDCCD9B15F01DEFFA707160B5DFB878828D4
          D7C4238888C0FE488FA1B3DD200CE28A6FE5866C770F082C102300835D55A519
          C848BA07C5E0DE5CFA112017660F209E1B3B91D66601C017DE3B1A05CA97348E
          BDB9B5BCF295E15585586D854324E2E0005A08AB06AC6A4A8F097F833156A52F
          83AC15E70622FE312FF7959786CE6F05B2805C59DF62960D7410B5BBEAC9F061
          B60ACF5E38F3F6A2E4E8FB0B7EC58BAE57A8ADAB8078DCC1118E3541291C1366
          5C1461E7C79392FE0C9CCF88F4875E66E1CBD9BE6D61F69B278D33273BC3AFE5
          5A53F02518C22381FC9AFEB36FCDAB4ADD34D3AFF9F9E5BC999A2CF7A98C1B9C
          88433C0810441C0703282DC1807D9654643DB8948D30503087F714D20BB7B983
          C78B701F40157C947100103F69BC9AF1F52E1A8363605EFBB9B0B911A00CA85C
          5C33E29EE648FC5B15113326115354C634424059545BB02705DA0872BE20E339
          E42567BB7461E5F3D9DE0D40B674EAD78F6B366068EF8BB13C7B16B1AA613C2D
          355944883530FF93DEF0E600D1A2918AFB2A1BAF1F1589DD5249E4330EA23C26
          68C340C1E853DAE0668CFA4B00DEB6CEBDB40FC8978015C0BA318D619DF9E852
          5960A00BF14CFDB87068AC2208BF72F3ED7467A99108102B8948D120802E865F
          12AA18FC3031CA70E5C11AE05F3BA2449DA28601604A421715ABFFE4FC153983
          B3D2BAD003AD0000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F400000048744558745469746C6500436F6E646974696F6E616C466F726D6174
          74696E7349636F6E53657453796D626F6C73436972636C6564333B436F6E6469
          74696F6E616C466F726D617474696E673B94FCCE9B000009CB49444154785EA5
          95797454E519C67FF7CE6485C06421211012081020252C09022A95D06095208B
          62D9E4A0B4581037AA506DD5635BE8A12D2E88222D9B5431820261332A1202C8
          2E4B30094B12320949866492C96492D9EFF235B9A0C7FFFA47DE39CFF9CEFDBE
          F7B9CF73DF7BE73C52B5F32011660B0951BF940079E9BF321E88EC6E5E101626
          679A43E41484A8D13451E36953F3DE5B7E651FA036B9CFEA41BD0D55F393440F
          10608AC936F8A7DECF9E161A224F359BA4D12659CAD07451A2AAE2B2D7AF164C
          FCC3898380A6D8BF1242C0D57D6BA1DEF52D80F4E8F383139E7D7BC4D77FCF9B
          22BEB9F44F515AFB8568749D11576D7B4551E9DB62DDFE9962E5A6D1DFCF5B99
          36043035B41FA7C67900D571D4E07FF2EA98BE17364E3A7063D75CD152FCBEF0
          DEDA2314FB61E1ADD92D1C97D6896B7973C4D9F7B30F6E5A3E3A099083B70F72
          7143360679CA6F537B2F5D3BB23CAF7085A8717E2DAE356D11571AFE25CED7BF
          264AEC6F8B8A96EDA2DE5528F69F7B5DACDC98659FFB72DA0840B6B6EC33F807
          56DD9B7E69D3647BDDB1D744A06E8FF0DFDC207CD7FE29BCA57F13DEB235C277
          639DF0D7EC14B7BE7D459C5F9FDDB8F1F991FD01F9FC7B0F2001E6457F1DBE3F
          77D2CCDCE14353A9775FC4240BCC5208B224234912480247733BA989E3A8B339
          39509857B26F63E57DF537DD9EC999BDC2573F957E2625EBC191317D12D1DCF5
          8046F937A7B959F43D03B3C73078F258840053B73E386C762ACE1E2998F4A7D3
          8F0141F9D117D2727AC7F7CD1D96D60F6BCB09742D80AA690475054D573BA050
          F0F54536FEA78877B6AE2739298694BE0332B267F75B049856CE1A342F2AA6EF
          C8E85E16D4B66A84AA7440A5FAE46566ECBD4DF5A92B2047A2AB0A6A6B35D171
          DD89EED52777CF2B594F00B23934DCB4F89E8C31D4B75D36044160123A42D740
          96D8F5F959A2431E60FD2B9FB16CCD58CA6F9F6374FA58AEDE289F076C099799
          1B939C86EAB681103F21AC670F7C0E1BE1160B9822105AB3B1AFB6D5119B3C80
          86AAEAF9C0A7665DE8A3A32C12ED81566A6A9AD8BFEF3C633287903D69188547
          CAE81DF510CFCC7E8DBF6FFE1D8FCDC8C11CA212116A98CB044C422333325242
          680AE8028100A1133778008D170B891B3A14A1064055D005C65944781892AE8F
          004C665D233E289CF8021EF6EFBDC0DA174FB2E7C866DE79771713463CC9D38F
          BFCE275FAE2621C94F726A0CDEA093C81007BA26C2D2EF8D8D128A162BA91E74
          5D010C018410A48C1DCEF96DAB18FFDC12F4403BBAAA023A4217C852E7B56E01
          2459553547B3CB6610673EF220DBF6BEC90B0B56F1F282CF796AC69F71B9AB38
          79E50B32EF49A23DD084A229D85D3694801EB87AC6E10904D416BFA70D7415E3
          FD6BC64A6898CAC43F2E212C42A0795B8C09759830CE03EE76823ECD05206B41
          AE3536B6E1533CC4F7D74848F1B165F7EBDC3B723C164B24FFF8681933A74FA0
          D55F873FE82510F4D0D8D84AC0A796019AD7AB16B7755CEBC6C7A7182B1DA83C
          7E91C36FBCC58D4387D0837E8C73CD3048ABBD1DB757290584EC695777965F77
          22906971DB481B118943B948FE91F5FC507E1853642B613D5C7803ED78831E40
          A6F28613B74B3D00A8CD4E7F7E7D85CDB8B17EE7E98DB5EE7225F7BDB98BBA62
          2B5268F7BB93D1D03585FA4A3B8DCEC0E78022177D5ABDDB76CB75C57AA30D09
          13CDEE3AC6DEDF87A357B6F0E70DB3B8276B20ADBE6682AA8224246AAADAA8AE
          74969EDA5FF701A0AC29B8B5E3764DD3755BB90DEE3CA1817E59C339FDD73924
          DF370ED91C79775FC15661A7BACA7EE4A57DD61D802A0381862ACFA21FCEBB5A
          6E96B9919068F1D6327DE6385E7D6929A13D7CE8BA30C4ADE55E8ACFB6066AAF
          B72DF33895B663952FEB5687DF7DC1EA5A5876DEDA6C2DA9FD69D4FDC70D61F2
          9B2F3168E23DA8BE1663DF5A6AA3F45CADE358856B39E0FB74F640613E5AF1A2
          F6ABC1EF950A785853F5CF1AEA820307A487A1F6BE866492400747A34ACD8D00
          F6BA405983D5F34C7161E37940D5850AA0BD55642B9E3F2A3021DB15D870BBCA
          91D33F3D1E4B9C9FD0763B019F426B931B6B991D5B83FB686155FB73BBCB9C95
          80A6693A14962F03900073B7E890980766A7FCE9C1A7524FE72E19240C2C1D24
          7EBD28F5878973FAAF8EEC11D20B0801A44E5E27DCA5EB7FE203517FCBE9F3CC
          C66929273E7A34C5F9F16303C44733539C1F4E4D3EF1974989CB801E77FBA4B6
          4B6BD83EA3BF71237EAC9CB40F6543C080D1280302D08060273AFA757E56F5AF
          160020907872AF5502420DDC15BACB577FE46F9F9122F859499E6F7601D0EDA1
          3977F27CE9D3D342CDE6A966A933CFE50C55D34A54212EFB824AC1C4CD5B8D3C
          7717E409808A2FBF63D4071FD295927F14FFF837B3FA5C7876697E4C4C747E72
          56FAE2D45F8DCB4A7DF8FED08139E3B29233D317C7C558F69EFDFDE2FC4D33A6
          F5ED9E3B5F46D3D1548DAE96DC297E60C1FC61BF4888BF1C37A8DFB43EA30763
          763711BC5E8CF7DC7704CB8A0969B793D4B11F3F38F99191F10917374ECD4DEE
          3E6D81ACAA4AD70D4C4E4D0D8F8F88C88B1F92D20B572B67DECAA3FA7809BAD7
          07AA86EEF35271A498536B3F43381C240C4D894FB3446F0042C66FFD48EAB281
          15E3C7CFEB66891AD94DF273BDE00223B66FC33C69063565763455A5F2B28D6E
          8F2F2463C7762A8A4A88322B58627AE67E317DA691E75D3610263137AA574F82
          F5754427C5E038759AC439B3089F328BD293B7E8F9C462E2A63C4C73D171E206
          F646753AB0F48A2202693E60EEB201A1EA99A141B71116C9C3E3F114E463FFEA
          30F133A631F0DD75C44E9E84FDE02182455F913CA22FAAC74BA8DF85ACDEC9F3
          2E1BD0836AAC686B41282A7A9B8BE4340BB7376FC65F6723AC5F121E6B0D4D1F
          FF9701A392509AECA81D3DC2D58AAA2A469ED3C53207FCC19640BB3726540681
          4E4D690D098B9712D227117FCD2DC293FB11BB602155FBF348EC6D022108F882
          74F05C74BD903D017FB1C7ABA1A90A7515AD747BFC496226E7D0B8EF20552B56
          E0282C2276EA14421E7A1C5B7D103DA8D2EED171FBFD469E77D94053BB37FF76
          B31F1485D6962043A64FC6FF4D01CAE17C86E70CC6B3EBBFE8A7BF237DCE545C
          2D01A4B0106C1DABDDED35F2BCCB06D65E2DDED168775D6F68F693322C96138F
          CEC779680F4983A309D4D6929800F5DB365194FB1B52321271B8A1D6EE3AF2CA
          D54B469ED3C532577BDCEE0B8EA685683105FD153D2E7D6C3CB2398460830DCD
          ED012148ECD5BDC3503F1A1B3DDCA8703A8F37371A79BE756856D75F01A0BD57
          75BDF8607DED84626B4BE1D953566A4B2AF13A5A118A4AC0AFD070CBC19963D7
          387DE1E6D13DB72AEFDFD9585B0E68BAAAD2D5929AD7BD41DCF25512600222DE
          481EB220CE1C362F4C96324C481655E8AD7E5D2FB12BFE9D6BEA2A77005E40B3
          AF5E2EF66E2D6449D50F5D33B069D028000482259557FE6F9EFF7B40C6CFC7DE
          6503FF03568070FAFE27A8C80000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F40000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C00000011744558745469746C650050617573653B53746F703B3B
          8A7A3C0000017349444154785EED97B14A03411086679358D8DAE4850441B0B4
          B4158BA0B63E848D951622D818B01404B1F615F4092C44B44B727BB7F38FBB91
          010B77D3CC62081EFCC5775FF33373DCDD0E4484FEF21AC42C6701E79C590111
          59F6092CCE7F0177FEB073E69CDB8D308C791391DB83ADBBC3240B4EAC0AF444
          68B4B779A13CBC7EDC1F45384E50706C56001062F2F4F23A9E8BC4E97E829CB3
          2CE098418C8644301789D3FD0425675580C042811A824079A1332D10C2F70400
          28979C7D016650901941A05C74F6050228201560E592ABB382301FB32817DC4A
          4E80411D37C48072C1555BC18C26ED3BADF5D613FFEADA30A9B70286A789FF50
          2EB85A13E0EC7B405DE587903D315839EFAABD88D81384950BAEDA0A3A624079
          A133FF18315A0258B9E42AAD2074D43193F72E71C9991790661A2EC7574FDBBD
          BEDB00CB67E870AFBF5C3967590037A7CF47114E62FA311C338D419239675A20
          A689697F0868819C335D410CA764BCBAD53917E8796EF54F465FD5BBF840931F
          16EA0000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F40000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C00000014744558745469746C650052756C65733B5761726E696E
          673B0BD1AE460000097049444154785E95577B8C1D551DFE66EEDCF7B3F7EEFB
          D1EE6EBB6D29A5A0DDDD22552850A422EDD296A7B12AC61205816834426C8289
          913FD44420684222A081D454A50F855A2CA00B2D8FB6A1F641ED76BBBBEC7677
          CBEE7677EFDDFB98F739FEE6CC4C5C0A69F4DC7CF9CDDC49E6FB7EDFF9CDEF9C
          2371CE71A9214912FCCB8BE2C5D7FCD3E327319753F91FC8250FF2C5B84800F3
          486DEFDA07BF2487AF666F6B4E44C6B990BE6168C6270E10943B5391DC35D1E0
          9AB884AB4312960720B501BC9A73C0E67C52677C50E33839CBD87B0754ABE755
          D59C0260122C02FBCDBC2877DEED2B7A6846FDA403CBBBBBC5D3054F3DEF1307
          BF918934AE8A86EE4A4A589FABA95915CB66114BA7104E24A184C342B0A96ACD
          6AB1D85CCC17AE9D9D9ABA3F3D3E71647538F0CABBBAB563AF6A8D00D0EF9F51
          ADC35B3633308E832FBFF2C92970F316E4B2272CF28BDAE4976B02D2371B9B1B
          D7661A1A2105C398191DC5B953BDA8944AD074138C33041505914804B1441C55
          D539646BAA3BA6A7663A222323D72D5202BF7FAAA8FF1540B9F385978C435FD9
          C4D89C4991BD08E664FEA44F8EF8AF6A93F72DCC247FDEBE62F9DA4CF3024C7C
          780EC363E3087DEE3A2C79EC71ACF9CB7EDC76BC0F1B8FF5E1BA3DAFA2FD278F
          2374FDCD385736F151FF201261054B17B6ADB92C95FCD9F712A1EF00C810C25D
          DB77CAECA21A985B6C4142E297B5C9AD8BABB28F342E6ECF3899CE9434D4AFDF
          84964DB723B1A0159C339CED3B0BCBB6D1D6D606C6186C02B32C14060730B867
          17465FDE8364A58868268391C9A9C2C9E999279E2E9BCF00C81374027786EF80
          3FE7D1C7AA13DDAD99D4430D443E7B611AA5581ACBB7FD1457FDE011A4DB1642
          92654200273FF800C78E1DC3850B53E8E9E9C189E327C4B3644B0B967DF7612C
          FBE1A3A82C5884FCE424EA32C9F4C244FCDB5BA2C1CD009273A77EAE80D0BA44
          B8B54991EFAD6F6B6910739CADC68A1F6DC3FC9BD7B9C40459962093639AA6C1
          300C98968181B30378E7BDB7BDEFD57D5EBFE6462C7BF061186D4B50CECFA22E
          9DAC6D92B16585222F0610F5B965CFFE0021BE2616BCA7757ED317A8C2C9761D
          4BEF7B00355D57839821137A4FF7E2C05B07303931015DD7A1A92A98CDA01A1A
          0932307A7E0C3B77EDC2C18307411A50F5999568FFDABD28C4920890D8B66CA6
          AB4B91EF06902604251A7E4309AD8C280D3945FE9253EDE3FD0368587F1B1A6F
          A2CCFD8E4399BF7FF428060606286B8B0875E88E03A6099DDC30758DA28E53A7
          4EE1B5FDAFC11FF5D7AE41FD2DB762BAAC22410E56CBB8A94596E6038810645F
          40E4FA58686D534DF5155228043D9E46CBC63BC0395C00608C0922CDF0ACD735
          E182619AE25EA36B8B84E9BA2B8C0304F7D77CCB7A98350DE2650D89F8E2AB14
          792D80D8C704542972573C9B437E640455AB5623D1D2EABE82038C338ADC25D4
          0C9F4444D3D0DD7BCD89060CF1BF26C8242E090713CDCDC87574A2A4E922ED2A
          091D00E28480EC577F52C292683A8DC2D434B29F5D09D1361981DBE036771D10
          64AA20545595B2D684102A488F9C9EAB8E2813F01DE05CC4792BAE445937110A
          2A48000B3D018AEF403822A3319C48A05C2C21BDE43270E664CD5C72CE84004D
          D588D4CD5C33DDACD58A0A83C87D270C8B84989A20660E5C2548B5B7A3625808
          53CE51A0D6AF01C5FF048390D34E6FD74965840A9109EF1920918D36058A1A65
          6B119926A6C2B5DBB66DE826DD537615D575C7B62C2100026E22B1DA7A98B68D
          802443919074380974E78E806715C1CD363F338343870EE18DD75F47B95C169F
          9BA169827CF7AE9D0E9110B063C70EE180619978F18517603945492007BDDA01
          985F478027ECBF3DC817C035C68B4645831250503A370C4D5371E8C821D1EDB6
          6FDF8E6269D62325DBBD62333D113AC1D474982EB988CC6316C414CBD4239480
          04C3718CF33200DB17C00966C166E3159AFF48248CE9531F20994A211E890BD2
          515A019F7DFE776EF1910B96E10AD0FC2FC1B9B79D6BD3F914451132E10017CE
          716EA370A617612508CDB251E4B800C02030D9DBB5E8176C3E54CAE7114FC431
          F9FE11D1F936746F40221E17C5363A3CEC662D1C506188EC9D8C0D024522B76D
          B72730318D5C2C507E314E1D3F8EA81280665AC8733E0CA042B065CF0A75C4B2
          8FD16602A9AA1C26DF7D1B85817E844221DC71D75D4866D24EF50B529DA2A9BB
          84A6E908A2E8909A4E249003F575F5226BCE6C3105C5A1215C387218712AF522
          899EE0380EA04CB07C01E5039AD5D3777EFC438B320F8C8F394BAAA8F070388C
          AF6FD982CC3C478437CF8EE59688029669D1BD259ED9B685A54B2FA3E81633A3
          38FCB7BD90CF8FC2A4FB21DD1839C1F83F0094E64E813A6CB1C1418B1D9CA146
          94ABA9C618ADE7236FEC775E28763B5BBFB515AB5675224CD794A5432C082DCB
          26B8E41691D7D5D6A1B3B3C3B19FC031F6660FCE9380543884BCAA638CE11D72
          A0DF73C096390DAF20A67B54F3A57F0D8F9C362409F14A11BDCF3D8B09B2CE22
          27A2D128366FBE1DDDEBBB91CBCE2352B29B60B851B8555B5787EEEE6EB453D3
          E136A35A3A8AB3DB5F44AC30039584F66A7ADF51C6FF04B845387743C208A533
          163B79D264BBFA47C70BB14C0681FED338F1E413187DE335D8962D483ABA5662
          0B4DC98D37DC88C6FA06C8E0686A6AC2BA75EB70DFD6AD58FDF9D5C2FAD137FF
          897F3FF36BC8BDA71108C83857D18A6739F69C73E7BF48B0E76EC9247F4F4068
          B927167CF89A6CE69EE679C988B399988DA550474B2AAD6A6261115D9E015C44
          BF55BB9D6F76E8430CEDDB8B8FC876CADC252FABFA11DDFAE33EC69F0030E0CD
          BFCD69F8E7025F844248115AEF8C280F74A4E29BE66752296733315556C5929A
          EDEC44F68A2B915AD48EA8A876269A4CE1CC19FAD48E39D52E0A2E4D735EB12D
          22D78BC72D7BF73E9B3FED91170816F7887D01787A5E140FE635B131F54434AF
          0B2B772F0D481B2FCF66DA130109CEAFA8EA28EB0654D3826EDB22EBA02C23A4
          28888614C482415133794DC769CDEC3FCBF9EE376DFE0700C31EB9F9A822F1C7
          4DF6F16D39271CFEEA6627989ECAC17DBAF5DC6EDDFAF1FEC9E93F1F9E981E9D
          AC688004641351B4546570795D0ECBEAAAB0209746361611EF982857F07EBE34
          F6966AECFCBBCDB611F96FBDCCF30473DFAD5FE49F7A347B2A137597220E7C7F
          5697FC8D8AB78BADAE95A5455706A41B6A2569455AC2FC18A45C44E231C6018D
          F3CA2CC774813A9CD3644ED2773EC9D1076092304BD008CCC9DC27160E78022E
          7538F5372C616F0B95F20425BCFF827087E5919408450F65823EE7B0FAFF9F8E
          491C2311FE8957F3A646F130F774CC3D12CB834FCA052E31FE03110904762004
          AD290000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F400000017744558745469746C65005370656C6C436865636B3B5370656C6C3B
          53FC46020000089C49444154785ECD976954544716C70BCC28884B20718B202D
          3BDDD03474B38A0828880AB2EF62D446D088823BB8206626C6C4A3C6050405A1
          15501951E28AA8B8A01844657141059400A2A0ED86D02EE09DAAC72BF33A643E
          38F361A6CEF99D5BE7F5EDF7FFD7AD5B058D00E07FCAFF87014B2729B2729E8D
          F050C1F4C1A892B9C4753662E75F60FE4622FDCC0A7F87E4B2CFFBB151856F13
          8C3E6B100316636732E2D62E52BD715E8B41E434AB918AD9B9454F35B79F0662
          D7D9E03039F6B9A92470313524B00F8EE25B07FF2EB00D051349E06DF2CC581C
          801272AB59AA50424E358ACFA922301ABD600C8C99CEAC54EC3433D6CE7D5EA5
          C425B2CBD8D25B4284AC5D2367091D2200CF354563A7279A5A077569EBDB0E35
          B30B99652209E8D2379F123B545B3892C79F60412A6268E98BE2B32BD1722CB8
          7C6F25C3DA821A56BC57C5FA3006840ED31079201C137E4AE418B1C1DA35EA8E
          B1D82F89245A39CF88148F8F025389CF44A17DF83123B16F959A86A696C02EEC
          8A8185D7499CA341B745DFC21BE90BBDD052D90DB4E6500D4AC4B0C2AAAC60FF
          C80D79CE2BF6575D8CDA544856AD460C30493A06365A78F50ABEB5EF542B9799
          E92662BF32F2720B87F028632B3F30B10EB88DFB040C445EA938FF2B33BBB047
          3CBE7B3231A96FEE89F4CCA7308C369B84EADE7E44B518CEAAD59C4316E82C95
          FD96B3B9A80E2E36B7C3C2F432B9646A0C8F1AE863661F1284F70FEF6510485C
          A2406013D2A535CC485B601B34D748E44D9286E999B97BE212838E9173B09943
          D82DB60283305FB0422ABAA6EEA856C188D3556B48D7E74D4DCCBBF5E4F85D39
          DCEDE886D2A71FA0A0AA1502D7161EA106FAF26D82B20C2D3C2BC9EA740CC758
          E21203CFC435C258EC1D231C13011A8386E9F3F86EAB469B4D8661A3C413744D
          C7CF37B50EEE36B0F05C3752DFD16338CF76022B484BDE77888EA166CC8EE28D
          9B4FD57657C8DF4299FC3D14B728605F5913782D3DF4D861FAF6306AA09F9ED0
          EB1E4F30712BDB20FD79661E35BAFC097B46E83984EB0A3C4057305131CA647C
          15165B423EC70C18A1E7F80F6D23D73A6DE3F13052DFB9843CA725B7740BD18E
          4BBF522A2B6D845BAFBBA0B0B113CE362B607BE17D7099BBF7C2689B40039CA7
          4E0D10C76AACB82AFB12751635B6CC5F6206D21CCE77066334490E9BABEE2E5D
          235A9C5976BFF8C14BB82A7F07F9F59DD880027E3E740B1CA5E939EA83870F61
          72E929A0236CFD7914B5AD14DDEBFCF86FE8EEC5DD4EDA6C4C1F68F82C4A714B
          C8AE7876B1A91DCE3429E0C0FD7638F1A003D61FBA090E33D2528959565C75DC
          DC7D48C940E0DFCFA2D01F8B7B5D18773B3E22DC3C9FA8E1C0111FE01D97EC99
          78A0AAA3BC5501471F7640EE9D575050DB0E3F1DBE4DC465449C56106F0B728A
          CE5536E09F749A7B74FA126877D7BCE946943B2C5C71CFD814EFC47D958AAB6D
          6F21EFDE6B90DD7C097935AF605B512D38CC4C3F459A9BAE9C6854BFFC801C67
          672B1B2042F4E84C8A9545F82CCF959297B3CF546F6351867625710DB7E88D93
          E365D7DF9561F13DD52F21BDE20564DD7C0199A5CDE03C27BB466B94D528D21B
          74315558BCEAC5073426926380BB1A97A8B4E8F8AC6BDD2BB2AE81DBBC8C656C
          83F5654AD7DEC5AD92867D60BCFDBCEDA52F2F3DEAC4C272482D97C3CE6B7290
          95B781475C9E9C671B6E49F2A8780516AE78F11E553C7F8FEC67EDE11860BBDA
          3932551A9F59DE5DF2580125F8CCAE929583E38CE4D5E414D02D618DF6B79C34
          5724DD78AEF55C433BA461D14D97DA60C3F956D859D606FE2B8F7D30755FE243
          4E0EBD1F6E60D14FC8DF23DB99B25E0634DC17E6D71DBD835F76A115D6153F81
          627C7E576696813868E35AB689D4491ECFD2C32828E944EDF11A396CB9DC0A6B
          8B5A187E296985E8AD9740E4BF613DC9674DAB5CC782946BCF7AB0F936AB7705
          4453573807269D7A937AF909FC70A605BE3FDD02A71EBE81F8F452107AAF272F
          1D3170C868DE84D8FD65FBAF3D86E42B4F21F16433C33A9CBB3AEF0E48C27690
          4B6928B7E9CA9FBD4384AB4FFF401291F9973D30D0D2272924F487331FB6E1D5
          249D7CC4081CAB6F87F85D97C1C463CD36A7C85D07B79EBC07BBCA9FC28A638D
          0C2B8F37C14F458DE0149DFD62387FA20577DFCBB018E537425B0F9269BB7B19
          50654BF6A53868D36AE92F25F073710BAC38DE080958A4001FAF84F44BB0F9F8
          5DC8AE7C0ECB7F6D8065BF36621AE0C7D3CDE0B7FA0418BBC74793EFB38B5121
          4257DADE62D8D8DA4329C62A8C6B40D9443FCC57E2D06D69D1C965F07D61132C
          2B6880A505BFC33F6FBF823D15CF60C9E10686C59895471B617EC60D10066C29
          624BDF8F5E6244887299CB93B748149AA16CC05E9ACB35A18E1966199C726C7E
          46052E71232CCA6F8085871FC0C243F510978FE3414C7E3D241E7900F852793D
          5CE069478E312D3D11B9C4A18441F1096170BAB201C9B77B09DC73DEBFBF96AE
          AE247CD7D5B8BDD5B8020F617E5E1DC41CACC3B11E62F03CE14803F82516E2D2
          AF5E85F3B568D733228F316CBC486951A00B2C66813B950D584DDB8391911271
          4D0CD0D4B513584FCF6C5E72E01E23FCDDFE3A987BA016161CC431A312CCFCB6
          5CC779DF60D469D75311CA79C2A31ECE31B113F103FE6440149689F7258B9688
          7B32068D1005BB3844E6BC5A8485E7E4DE873939F761717E3D29FDBB1142FFF1
          2487361E4708D3F92916139A3BD159029E9BF8EE5036601EBC1B990765905251
          A8095256CD9136D200C7E803EF16ECAF85185C059FA422309CB8660B693C7A55
          1301CAD9BFA2A987334D1DC8C82745D980206017E2FBEF644BD589619C5313FD
          305FEB8C8D9B376E41FEC7D9BBABC13C30A5596DF037FADCFF8C59212CC0A583
          C0CC4F3374301878252B1B30F14FEBC12F0D19FB125249E49E0C35CC509E6BE2
          F7C2D0DD30DA79C977E4CCD3BB5EE09F8A04FE3B1013E9DC8F8D183EC12F0519
          7B2733E27A9EDB950D18F9A4B2EC6030F4A6A428996045BFE6FE956405B0208E
          7FC2D4178BE27233A253B673600D7CC650E1FE56E4FE8E44FFE1E01AF83C2394
          FF72FC0BDF07F6E4857816860000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F400000012744558745469746C65005370656C6C3B636865636B3B752FF3FB00
          0007DB49444154785EED94695094571686AF8949A55235DB9F4931E3361A1451
          D911C1061A4459C31A4411954D9B75914D015917356012457132018228518344
          258186160CD2DD2CA211946DB0BBD9BA4140591A68403EF4CE393D7655574DCD
          BFA9C91FA97AEABB9C7BFA9EF72CF7124AE96FCA3B01EF04106B97E3C4EA2D6C
          977825F0B7421DB6731C617FA622567DFF3D44DDD7C2319AA8F11FBEEAFE2CFB
          28A20CA802032D2F2F7FB7B4B4747F71715109C33088A9C5DBC0B07F01EC050B
          0B0B350A8542C9FCFC7C25FC5F00F6E0E1E1E18F590EC708CBFE18999E9EFE13
          DAC0A7606E6EEE9E0AF0BFF2EAD5ABB4478F1E7D40D4B383CD4F30A87F64DE9C
          C3FE0C6AE396487FAE69EA005BB44A35EE07C75D9207449D9F753998B5ECEE7B
          8AF18B38379BFDD535E9E8D84B21EC9F2B2B2B7B1F8298A1B081C191E62FCE97
          4903A2F2669D0E64BC39C0C95584C7E78D4F4DCB4B950230335576A0CC4D2491
          B6D879A5511460EB994293B28A46C07E43553E10D9807BBBDC13E95EFFECF97D
          81A7E7D1177FE317963B2597CFF0C7C7C7F520EBBB95BCE64EB7C3A7186BF065
          3BC75368B3F2EBEA93ACC02A80D00FA14F31885200FC28F5F2F55A111E1E12FF
          F769FB7DE9D4C53B6501830E0C0C6880CFFB3333337C0C08A269154FF8F4C58B
          17027E635B1BFA9A3B4453DEBD07ED93939349ED1DCF1E3A79A7BFB6708A05A1
          E9B335754DED13131382C1A1E1A6A79DCF5AA10525185339282CC76324353575
          2508A84CC82C19B7841F5D2BAFFBA7B34FD66BB6530CEDEC163F989D9D754701
          72B99C8FD9B21CA228F76EE3D3A1A1A1E4E19191463BAF9437A67BC294A240C0
          CF0999C5633057D4CE2376B17F60A809E6A1402693E9F6F5F5AD853959031558
          65601940C84EBB48B2D336924C4D4DE94E4D4F0B5C0F6532667611B4ED49772B
          27E682DCDA398EE617FC2886CC7340C04AF0E3EF81D6B0C1FE13B7A1A3AEFE41
          FBF1F4C2D19DF651D4E350D2CCF8F80B2156C5D9276D0905659C291C04416515
          1515BF53CD91AEB91FD163F9125DB3C384801341230408BCC7FFB59DED1A4F9D
          F7272A46464684B917CA8676B99DA08783B32721F35A369BFD111CC6B7811661
          7688956B027C8F53379F93738FDB3B5BC7C6C6AE0C0EC91AAD5C4E50632B0E2D
          2EADE8191D1DF5C7F9D9667A886CDBE143B6EE38007893AD26DE846CB7E610DC
          8412E7E7E4FD3084992564FC63647070B0B10232B406013B6C38AF3133B1586C
          887DC48026BB43E9414ED6A47FD89909DBCF13180B6895A3679CA2ABFB594B77
          8FA8C51CDA68C03E428B4A6EF73C7FFEDC1763686FDF47B48DBD946C468CBC08
          31B008206D6D6D7F841EDD3F1C9A2B576586D36A096210636B0EADA8AA7F0A07
          85C2840BCC1D63A8BE851FBD79ABA6432A95D67474F5B438EE3FB9686C1D4463
          92BE92A178CBCF625E1BB139F464FAC54168C97598B18F542DD864E04136E97B
          104D7D3742D00059D98A2403CD30B16FF01023289D21FB2835B58DA0F0A85093
          5D21342EE96B1996171060BFA194F4E66D5E078FC7338341E40646E64EE85B06
          50CF83F19370637E3918746ACA785728B5720A5D9048FA9B5EBE7C992F1289B6
          747777AFC5418498AB3FD571FEB700C8FE78F1F755BD26BBC320B02FD3D7D7DF
          843300D93586C59D1B37B109A3568E410AE8A510265868B2279CEAB27CE98D72
          6E27046BAC6F68796CE918B9B4D9782F8D3E7156D6DFDF7FFE5A597597110837
          647360A6E2E41595754FE01608257D03CDAD8F9E3CC4D711DBA2EC3F08288F4E
          BA38AA67EE4F3D7CE227A1645C4343C38FA1E4C9970A7F1419DB84502D232FDA
          D1D9D382028CAC839507EF768D5ED8E31EBBA0C3F2A3DADBF753634B9FC5DE67
          E266F009C19B9090963FA40B671A4035117DCB2374ABD9216A61EB370B1528C2
          6BAD140037A0DCDC3E7851CB702FCD39572C0641B9B809429C1EB775B56A6FF7
          A69A7AEEF4D2B7377AB105B66E11730616BECC26F0673B7014568E47E792332F
          F48B25FDCD30CC3FD4D6D6FE01AEAB3F0EECE5D2DB5DEEDEB153DB76782F6F34
          F0A43AA6FB1877EFA8097501F8BEDFC40706FAC4876F0308F04061304C1A50AA
          0610C2474028EE15821F17DA21C06A40AB04C055D82F02BC4B4B4B7FFFF6E095
          F07B16D8CE82E8BBD04E21FC2FC42F0C7235C4C9C6D84A01F8CAC1CB54055F25
          B0AE441B02CF70153873115C83720F08EE0962B8F02654236887433FC73D5857
          82C82A04D72A7F3C03C4D420B846BB52281C1004E56AF8F5CBB3A23BFAFACC1D
          7D3DE6616EAE181CF8C8839C1C71B98E2E53AEA3C3B49CF9428C958060FCA6D3
          A7C537B66E61AE6B6F611A610D59F2114156B6F8AAD666E6CA262DA621334B0C
          732440EE6764888B376E6490FAF40CA53FD843086C56D747474BAF6B6AD2BAA8
          28695D64A4F4DA860DB43622525A1B1121BDBAFE6F94171E2EE585854B4BD6AD
          A3D5A161326E48A8ECBB356B293724448614AD5A432B838265C8B77F59457FE2
          04C92A381CD9371A7FA5778E1C9521973ED1A0B7028FCA6E051E915DFCB306BD
          0D36B82D3C027DE17DBF591BD4668AA1A70200D54A4A2083CB1BB5985FD2D325
          707D04485D6A9AA470832653B8FE53A636255502F75F88F0925324DFAC5BCF20
          35492725D86B849B9824C95FB38EB9B87A2D53959028812B2B442A13122479AB
          5633BDBDBD77B1025F4239B0975C587B22507A5C5723B886E09E08AE61E0AA11
          BCAA2A3BAE41780DA26EC761C39712C13588DA8BA8ECB0FE9AE0B4021FBCFDBE
          87A86CFF23FB87C87FB1A33F59A1064170FD7FB3534A7F53DE097827E05FD027
          A85E2DA4E2670000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F400000021744558745469746C650050617374653B426172733B526962626F6E
          3B5374616E646172643B259B079D0000064449444154785E85976D885C6715C7
          7FE7DE3B33BB33BBD96EB2D98D6E52562A7EA968DB101B59535F4A5F44285811
          151B69B596C6375A44C837DB6F7EAD504441ADB5AE2DE24B1015B41429D85243
          A2821F5AD0C686E6656D926E32C9EECEDCE7397F93DD87B95CDCDD39309C7B9F
          E7E19EFFF99FFF39F78E4902C0CCA80C6363B32F3FFAC4DCF43BAE7F32CF8ADB
          DF3A7FFECFE7CEFEE7D02F7EFCD809406C6AD59E543F565037AB7E00E4402379
          806272C7AEA7EEB96BEF81EB774FF2DAEB8B773CF36C7F01B807080080032B40
          4C81BD0E82AD011CFFD9C18F8CB61ADF31635F67EE2E9E7A31E7CA6A814C283A
          125C775D8737CE5E246B1490151FF8E2B77E741609C919C99D073E9A539E7C9E
          3CE3F89595FEE1DBBFF1AB1712106D05C0001B6D160BBBF77F6EA6BD7D0FDD95
          16E77EFD020F7EE1E3CCECDC467488EEBC7D69854B977B84E0DC7FDF1D8C8D36
          E887C8A9D3E778FA2747B8D2B889DDFBEFA3152FDCB278FC773F05F6003E9C01
          C8CCB4163C5EFE2779D8CE9D37459EFCDE73ECDC3DC7ECEC14EDCE28EDF60879
          9EB3BCBCCAEB27CF7271E9126F9C38CD5B674E72E0C68CDEF97FB3AC65266FD8
          0BEE33409612D4D0122846E4115FF92F992E70EBCDB38C8DBCC98BAF1CE7E8BF
          8C2B65C6CA6A4188804A1A59C96811989D32E66F1D636AB2608C53E4A5C8BC44
          EE35510F2B01EE0E0A28044C7D9AE1356EBC618C8991494EBDB9C4CA6A9F3246
          A2048222379AAD51461B4EA7D9BDEA97E98C148C8DE698AD3FA702F01866368C
          01813BF2003819A2E54BCC4D47C6F392A50B5DDC851CCC846546061485513432
          1A79C14833A33091C909C199D931DB1C7FE7BBE3AE89C558145F51B71B74ECD8
          0FB43180149C1810C25C80939BB36BEE66B64DAFD25B2E114232CC1C0C4C0990
          19990118B198A2BD7B1F133BCEB7C71A53857B2FF47A16DA6DEFEFDDFB50B806
          A20EA02A011E03489000E08E9781DFFF639A334B2308A181B204020964800400
          0AC03C07BF367F3AAD1043F9973FFE66E1EEF1F15107C58D45A8C8358F1C7347
          38D1B5B6767AA9C5E1431F669855E0EADA7FFCBB7F9A37AC198256E171FB3F00
          1E85428962097224211C45E11EF0945D1984502D9A44DDE4882C796835735CC2
          206FB532DBA40B2228400C7500417870E402AA60AAC4036420100ED4F73DDDC8
          1D97E5319606B001038E7C5D03264702C9D1DA7A1C0414C2A016B0F2C9241CAF
          18023C08C0B61E44B10F31E072D28CC7A3882120799D01BC9621B58C537B0072
          12264F00D890013C3A7840310543E0426BEB5509C05102F2CC91BFE32E5CC245
          EA2488EE3CF8997D156BA43DC83665C083A358220F24FE913BB89047A2940257
          EABBF7EEF7A605AB389118384F059370694B06CC63445EA2C11CA8BA20461F64
          E70C9A9E9FFF769D0139B847E4223AB8C4A183FB2BD500DA82014B6D08B19F00
          008A20A128D6F501D41880CF7EE2FDE94A55C669173754090697B62C011E031E
          4A88011072C71DA4F5EC5DCEC004423CFDCB63443972924F6C487CFDFE0FD518
          88615809828397691E3828511B1DA58783AA2E70E3F39FBC054958AA8A5481AB
          DA1124E11EB616618C0EB18418911C530A1885475F070628090BC40F9FFB2B42
          C885BB522090C4235FBA0D5159D49039E021E2A13FE8021F7402283A3E48BDA2
          F5814FEF83942118C853C6D4B421817BD2C02625D07A09FA981578EC41AABDDC
          D6BC4721C011A464BEBFF032526205E1EE9034F0CD873F46550950F42D19F032
          C4C5F3A7CECC8C77B6514848914C8220ACD9AEDA5024BB26B403D56B19434A57
          80AA830888DA9C0107FA2716971FF5E75F7A2293766A201C70199DF76CC37DAE
          DE05F5D18BEA6F4764E9DE954AB9B90604948F3CFBEA11E00F40511D6CDAF6ED
          D3ADED3BFF3673E7A7E68F5693502988D53481A503896D32100947DC7A0E4460
          15E857C13118B3A9A977B5C627A6DBC2ABAE160803D5A950A67A5D3DA94610E5
          6426F2BCB1210315909ABDCF2626F6647953FD9401E3A30D808AF23A2B40FD13
          4D325C8EBB10C45ECF550320A9025237CC50A7F3D5D8F3CBCB65BFF7F2C38717
          3E883408AE74ED9EC0E0780D8C00903BA1EC1F15BA5C14E6F0ED0460A819DDEE
          43A1DDCEBBAFBEF2D2BD666507B28632654206D4C66BDD6386616E809752A35B
          E4F9DBDD6E28C194000CB76B9FD0573FA57BE3E3C585108A8BFDD6156BC79114
          68F8DFFAE5E6AA9ABD8E1A0D8BDDAE95579FE700FF035802C08A010C91F50000
          000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F40000001F744558745469746C65004375743B426172733B526962626F6E3B53
          74616E646172643B9EE424DE000007E549444154785ECD5569505467167D4689
          5BC0C424B319C71A97188D32A06283CB18D4E0862B820AB8208A2C2D826C0A88
          4D03DDD2D0021284661751B0C1B02F238282A86C362A12356A2B101985882B13
          DB68CE7CDFAB7E5D5D3443D58C3F666ED5A9C7BB55E79D733FEE779A01F03FC5
          FF8781A4EC06269180565064197320A2944520FB2C610225254C4078310B7F8A
          4345CC7E0A7111B38F4521B34F54C8F811F8861568E01D4A1092CF901AD4076C
          690C2466B1E2835273AFE068C6450125044A88201566058B59416D513F4E34AC
          808008534156349FF1A20821106AC43FD823C815780A4E83FECDF6B40DC84ED6
          B3068E665E2E69B8D68198D4EA604A7ACFE3E5C407BB079D0A2E3BD78A9576A2
          52FAAE6320FE441D139F59C7488E964D3CFEFD95DEBAE63644C8CE0ADFC7847A
          D221FCC02C6169552B767925F47EB3D27B4ABF274026678E1EBFCC3A8E4A3E17
          50DDA0C4C5A6FB083B521A421DFF175353113DD7FD2742A8787AF63958580B0E
          D05EBF3B1047C48F1CAB659B365BBC471C4E3EAFEC7AF20A35F577111491174A
          27A1C4FF40FC43E77DC7434B2A6FA0F3510F2CED43957F9E686A4087E9D7C077
          C72E32D169B55C7FD081F0DC85A9F27ABCF9F51DCE5DFA11BE21F230CEFDC0E2
          ACC05027BF6361549C96D7C134CC5DE26EC1F1E72DF763740C44A7D632646AB6
          E91592471F4382A485F20B4D4AD64445CD4DB8076488E864546800F1614E3E69
          A2E2CA16D0AABCD002F335FEB9A43F9C9B7EEE526F1D0344BC9A912454B24DCF
          60D6C0A09D7BA2C68BE3CA9FDFEF7C0AD5AF6F5156D502272F999833D1477C08
          1571F44A11179F65C5D1DDF30A1B764A9F4F9BBDFE2B6E7AB3255E0C6FB1878E
          01225EC588E22AD826B9AF2CA863BE7FDADECCD3F5E8EC7E01D59BB7283AD38C
          CD2E31E1F498A9B096F8C8ED9E49878AD4E22F7B5588961563CE120F1FEDE9A9
          F82C73775D0362222E8C29679BEE41392C2861DC04C391FC808C56456B071E3E
          7ECE9AF8BEB401EBB74B247462F5641F3978248517555C07AD9E972ADCBCF310
          E6AB025A47E87F365A7B77669AF319E3F97C5D03C2E832E6A0B4986DEE26E2FC
          0372861F28674FC1DA2164812032FF5DDBA367E878F41CAF8989530597B0CC26
          504A08BFDBB23B3EA2502DDEF5EC35BA9EFE0227CFB877D3781BBFA526B5B284
          88BB31867377E91A0892D298CD679B6E44D82DE014E34AA05EB0A1B6CE51E939
          254D6827261E743EC1CB57FF444A66396C1CA58D9C78E7935E16792575305DEC
          9E4178067D6F8EE19C5DCC34DE0E5D03FEE27C92E5B96CD3D53F9B8867332EE4
          C9259AC9DF368C73F448E8BE71A71377DB1FE3C6ED36145528C8EDB8055A0F1E
          F5A2FDF14B28DB7BB0DCE660CF17E3795FD21B41B9DA06A699EE64A6CE76D435
          E01D729AF114B0474E84B318E7FD04FBB2B40346CF628DAF9B502A47CBED07C8
          2EB88C0B8D4AFC0640F9D30B283B5FE02E7906879F8491D936BA65FAFD85D714
          93EDCC5733B6EA1A18B8340133DA7C95CFD52329E5B840A25AD1D286F4531771
          F6E26DDC6A7F8A7AC55D9898EF6E2684DF130CD564FE0035A0012E56B98021F8
          C3DAAD227941F935FC46460F9616A0EE6A1B79E6E307650F1CDD63316DB62D0D
          913F12E87106B431B0015D513DF524FAF4A3560E92D8BCB26656FCEAED6E1C49
          AB829D731C8EA45622B7F012DC7CE3E1EA130743D32D0984F3A93AB0F4387059
          309001567C47F4D9892E890D0297E42BF52E49CDAAEDDF5D52D809F21A320B1A
          A152BD41FDCDC768BAD5A541CBAD7FC07ED761C4A69420A7B4117EA26C58BA1D
          6DD81C5ED66477A8E2B58DB0A8616D60AE70A153B42611FF9D810FEC0F154FDA
          115F279755DE47FDBD67E87AF516CD6D2F5074ED67945FEF466CFADFB16CA318
          4B6DC4B0582FC2E275A158B82698FEEF316B011F9EC24CA4D5B423B5A603676E
          74E3C79EB784D785D0AC2BB0D89B953BD34638955B4E8D01ED3CB78FAA0E8E3B
          730F55ADDDD82A398FAFED32B1415C8D826B4F9052D58E399B63E89219124C26
          F89A603A01FDE8048A6F1DA31589E7DA9154F5132C03CA317543222C03CB2123
          EFDEB25AF0B6C58771E1D4D7006D0CB51257369CF9A1079B24D598EE989138F6
          1BE75986DBD393578B6B71BCBE0BC68EE9D709EF4FEABD18A6C6876A8C5CEA7D
          529154FD108B7CCBF0D78DD2C4716676B38C3645262FF62B4774D97D186E8A6B
          E2AE687F06862F3950AAAAE9780323E71C8C99EF64427A9F8F99E7606AE49A87
          D8C65F30D93645C52D589F2D1F4CF90B7667A8C22A9EC2684B12C69A6E62F963
          675B9BCE704885A0FC0926AD89A6FC8FB98CE86B6004CF25A331AAFA11AC62AF
          83E79193327E91B399E99E9CB475E4DD36E50EBE581EA920C44FB80F18CE71A2
          5C0DDFD83EA6D136E12656865583E7244B193FDFDECCD449966629AAC1EAC8AB
          18B328543190816193D7878B5C9315702DF81936C94AAC3A7C1DD6C9F7609BD5
          8DF97B0BF189995F243D42EE4A71B9CEF1FFB23C4864233E8B0D494A58452BB0
          22E43CD6455D81B5EC1EE6F1E518CDF38AD0E6F75D42BDCF676F9B3E7E9D287F
          6140315624DC8357D973B89E50629E77214699FA158EF8D26A8656C673A5E17F
          6A6C377DDC0A41BE093F07CB22AEC23AA9034B0E35C3D8598E8F793E852327AD
          E5F83A4BA899E2A3296B8D0C4CDC2406663E8A5173FC55063CDF66FD197CE9F0
          89AB66D245EB2F50B4F9FA93571B19CC74938CE2792B4699ED277C9F667D6357
          E9F0092B75F8FD05D160ADF41B4DF0997AE94651737DC439EE7BF0078EE221DA
          31AAFBE3A2E1BE075FD780C6880E748BE3BE17FF5FAC57759B4588C1B9000000
          0049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F40000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C00000025744558745469746C6500436F70793B426172733B5269
          62626F6E3B5374616E646172643B436C6F6E656D0EDF5B000000A74944415478
          5EEDD7310A04210C8651CFE7B96C45B1F7642195B087982DB24EFBA30CEE4876
          0712F85A79061B9D88FCB467004208B258EBF99E1BF41D60654E448CF15D6BF5
          70961E809901A10CE883087D002246805B8FAD942244340320E2381108B87DD3
          9492B4D6A600447480DF0A6066445C6DEEB515D00710D7E8DD0044A80346087D
          0022884805302DE7BC1DB08A3380010C6000038C52DF801BF5DF0003ECEC11BF
          E30FBB3E9B500CE46CA50000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
          00097048597300000EC300000EC301C76FA86400000150494441545847ED95B1
          6EC2301086F308BC4987A47B47461EA19BC354D66E7D11C490786181910DA9CF
          508985A52F501E21DC7FBD0B0601314E82977CD2499673BEDFBE3B3BC9401B5E
          3E16A35753BE67A65C65C61EB2DC566C18D31CBEC147DCBB25CB8BD999E82D63
          9F6226CBBA21CDED4205C69FEB6ABED9553FBF7F958231E6F04DFDB04696B7C3
          15874813F0E96C139C7609B6FCDE8B4433F0D575C1E5403369CD7D4E7E499D09
          8A11D498DCED1400750D457B02B124AC3F7CD5024FAF9CB250AE24AC3F9A7EB7
          DB1F056BFF37600F12D61F5E48D6168D2361FD89BF81E82588DD84D1AF61F487
          0850802D07217BFA53ECFE84D47C32519F9C2CF867744D5C0D7585887B3B30C6
          9CD61CD687F8567BE2AEB14F876987E969D04C7C3B7045DDCD604C73F816DC70
          4DE2BD3288C71137F62B9A3820B1FA957BBA3848A776124D5C494DF186FB8B72
          C8D440CF24C9110EC5CE4F7BAB0A0F0000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F400000013744558745469746C65005265706C6163653B456469743B69AAF970
          0000085249444154785E9D570D6C95D5197ECEF7737FDADED296B6B0524A47D1
          8A4506825010112A5264C1C58DA9414864996E6C4C16F027A2C2CCC212B398CD
          28E82671C33926718E2C71B00981315AFEE447A4DA851F5B2848697B6F7B7BEF
          FD7ECFCFFA7D9E6FF672B7BBC537797372F3BDE73CCF79DEF73DE75C822F6FF9
          E68A2FBF48FE58394ACF2522B23C874C4E2C340CB397F75E80E002420870E18F
          587B4F7D00A03CFCEC96E27113A7DEADE9E16F0A901B85E01542906ACED86546
          791FE7EE392395DEF9C9B13D7BF7BCF5621200972EB2C80F2397A5C04B7F3B87
          FB16D405FC50A329C4035EB9E1D5B2AF36CC7856D7438F8C2E2F8C568F8EA1AC
          2482485847414447DA74610E796F22838EAE04BAAEF45B4626FD465BEBEE4DFB
          77FC320E8049606DD58B7F5DFFEABA453F054001F02C0518E3C3813D579F7BB3
          F5DBE1C2D82B632B0B4B6FBBA51AA1880697715006B8422061503F548B8430A6
          3A849AEA3218293B72F24CD70F84F28D15636F9AB1E6CDE797FD0900566EDAB9
          5E28FA93005E00C0725240258180ED86DF1DDE30A2A4F899F933C72156148141
          390C83820B99643F4D0097A3ED70F40F3A6010187F43356A6B2A627B5BF43796
          6FF8C3143E6477CCBCF9C7874E5D04001D800D80652B40196A7445F1C0D7FFB6
          75637945E9FABB1A6BC1150509092CC1A40BDF290792691786C9FCA40A0E74DB
          0E7455C59D7326A2E51079AC6EEC484C98508503C73E05005542922C024FDF37
          C9075FF7EAFB4B63C5B1F5F367D4C01280695030011FD5B629CE9CB9888E8EAB
          88F7A6901C345054184549690C636A2A505337061CAA4FD4A60219476056633D
          C2211D1D711BAEEB2BAF4895A15DD722CA9247378E8A14956C6E9A590B8B2B48
          DB0CCCEF0CE0FCB9AB38B0FF34920303FB1C23B5C74CC54FB5B7BCDD76D3EDF7
          DFD2171B39F56247D1C2B6D317E64D6F6C40D9E80AB88CF96A7C663170E2FA0A
          522609F8964B40ABFBDA1DEB268EAF28D1C221C433148C73704170E283B3387E
          B43D918E5F7962DFB6A7BCA272652189C37FFCD90100AD00B6CE7970E3D28386
          B969CAB489A55575E360510640A60C02AEC3731508763F79EEBD2342E182EF4D
          18578EABFD369206854B392E765CC3A963FFBCF8E9895D4BDA0E6CEF0C0AE8BA
          43C66F8796B79FDFD5FCFD2DB30AA3A115218D60D0E2121CFE48299578B90A68
          B72E5876EFA8F258A4DF64E8EAB37DE9296538FFC905DE7BE9A35543E01DFF05
          9CC8C20ADFFDE8E6C79BEE9AB5A27EE2385C4C9870A93CD80008B95E6E0AE402
          A6AD2C2E8945D19F667E4B71003D5D3D480FF6FFA5F59D178E0270E44ED1DA99
          120202736A8B03F068D3235B9E99D7D4B8BAAEBE06E77B0D0C1A549EC9024276
          B82420FE930262304D6F7EF7BD13186E8400833DD7DE016049D9F1F5B53BC4A6
          97766198290B566D7D2A1489AD6939720E2D47CE420C8311F28737124220EB47
          0CCF8522C984A5AB522621831D006696F4B9F2470044E5FCA0C8020F8CCB8DA4
          E5288F62A98E0C70241965D824578ED996BBB093059E7B6B0AA9229598825C7F
          0E345557867F357FC66570512A00647AE3E6D6B39D6336777627F392C8065296
          6F538F8278266A3923A59C893ED745A765889D57DBF1C689D7790F00A19CBC73
          2A4ECC9D026964E3B486D9D4B44B3965006350742DBAA8BCB449CA4C0E354E42
          EB4CCF1B647E45E059EF0021702B81989A4A89D2E4208745511E8E607AD108B2
          A9743CF64E7A888C044014CAB9DF6ED294225559EC18068C8C79C1A5CC52751D
          11221607D252CAE0B933E4794CB11DC0B681BEABB8FCDE63BCA8EB9468C80CE2
          379C017A9834148D218F7B710AA5DC5F3028261D68360D0B8954FA1FA6ED7EE8
          710B296AB3AC0BE24A0294F2BCA960AE2F20388567FCF86BFCC2F16D7C9D6508
          707BC85DD4C1578031CC3ED2E6E7EDF5D9536AC1D8CD4EC6424F2AF341CA728E
          324AA1E9A1312FDF503DD98BB9EBC3B3C49BE3791E23D415701D01C144D0654A
          D574543BB6402A2DE058E263FF03FB62276A5D6174B1633AB01C0727FB060EF7
          1AE661E632105545B9A22E0EEAC0610C2ECD4F20008F142234E387646ED373E4
          27B12ABC9719048C383E19388F5FFB6DE8B22FE48F08F12DCBB4E0B8FCD2CF2F
          75773DC0C4E0DA1B6B994214552764A17CC9B84BDA3BC9BB136A443E02CC01C0
          014D4565AC12BB140DE04CC031F051FC633CD07510095F8185A7CFF9E04FD4D7
          56285CCCD0341D7A69F13100FA8E2BD74CD376DAA94B11D5B4690F8D1C5129E5
          FCBF52C01C81448FC824AE883F27AF891E6A030545983C6A320E8C5F82DB3C5C
          2DD87D63716C1173A82A844021C8D283B3A72E05F764E4605E80A2A833A39185
          BF47729B5F638C893C4548A8037F9ED18781F6ED5809804F5A86A6E26A6C0FC7
          50A917E03500D3FF4DA0909066D771D1D31707F780E183A2BCAC041E11555511
          116806F01600B2FC5237C9F707C4234014803B803CFDDCB6EDD8376D15F68722
          B847084CFCCAEDA8F509D44623E110E7F30DCB864379F247577A26F533660020
          DF35ACBAFB2BCB0F4108B54055E7556A6AB487321B00CF5F8480A202B2568974
          8553445DDB2708C1A02A00D4276BAA66714A4B32A68594CBFE3E049E0290F17C
          6B77DF39D3714F31C61052D592EF1417CD92DD90CF0877014E01BDC0078E1454
          A264D283582380799901809938DB7D041DFEA553A19066461932A68D7ED77D5F
          5E3E3468CFA4E3EE29D3D4E93A0146FADD80BD00F2A680390011404121AA6E79
          18DD9CF999849100EC1452563F567B381A002412C90549082F809DB4ECDDD7DD
          7EF464BC7F77BDAE3D0D0144419AF0BF8DA713929DBCC005478AB9E864368E5B
          71FC22FE31CE03A064D83B4093A0CEF007836C3B5DC610A94CF02CE3420461C1
          6303C19A213966BD07E43C2A9D6B92A325038320F14A59894F607562202015A4
          44C8EF7CE81BB22CE76D91F316801CF950077823FE05A9F885EF479F861D0000
          000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F40000001B744558745469746C65004E6578743B506C61793B4172726F773B52
          6967687416E40EAE0000047449444154785EED576D6855651CFF3DE7DEED6ECE
          0D448415B694BD6956732013665114984145D6B2A8D0308A28933E8444A14E96
          424E6F19E4BEB495D207B5316B6856DEB9686D6D6BB3BDE972EDFDD5ED6ECBDB
          DDDB39E779E93ECF39072F83053B057DC87BF9F370CEF3E1F7F2FF3DFF730E11
          42E0BFFC6900FEDF046E11F02EB44108C1C1B256BCF7D43D445E466D89FC53BF
          0AC10121FF428073006A8D1404C0012EAC3D21EF09EBFAD86BB90020EF2F4C20
          BA1CF0C2AFDA587846FF3E3C39BEE7C35D8F5CC97F2E5B41BE7BA24128123620
          574C006E832A709B84108B7320BA344A39B6DC97B6B9A629EEE13D2535257D57
          1AF69F3EBA7BFCD08E0D1291F7EB5C28B54A9D2C61ADB0DC395AFC3384E0AE33
          40248195C949787AF31ACF0339A9AFA467E55E7BE3E3C0DBD90F3D932045A4F8
          34EDCE380FB1D1ED45812BF5A6C922F50F08989429357F1A40DAEA1578F1C9AC
          C49CF5A987EECF7BAB79C7FB65DB00F8249155F15E6DF5122F91FE3B2D610A5C
          952B02C422C055B028E3081B1CD30CC8C9BE03DBB766ADBA7B5DC6173B0F0702
          4FEC2EDAE010494D8CD132126309A34229970E9A94BBCE00A12657965215264B
          DD9C0110CD8B2D0F66206B4D72EE77554B7F7A767FF9C9CE5F2E1C68FCA66814
          005DB73C8E0110797B2F08CE857B02066532D94A45747F19E708CF52C426C6E3
          F9ADF76ACD6DD75FF2F912F29233371D69F8DA5F34DA7D390C8096163CAA88C8
          7245C034381813304CE798D90484506D99D13926C202C929CBB17DE5B2A5D5F5
          7DF95E5FFECED058FF811F4A76950198036042351262310488E300D519425326
          3C1E0242887281D9AEC8FD599D63C4A08A5CFADADB919EB622A5AA7659B1F7CD
          D3AFDE18E978A7A1746F2D00C306108B7240D7290C263035471530ECC9C6B895
          0BC69413AACC48750729623D041B37A52333F3B68DE5E73C9500126D17C4E243
          481974A9D46437330080330EC601CA558BAC950BC4C77A647271A9A21D5D1D83
          9DD39383FB0030D7193074068372E8F234E0A6035CAA9765BB11E3D19010035C
          6D1B405B53CF44283850F8DBC52327F4A960D851EF2E038C61D6B07A2D006BB6
          47CD7C8F469014E78DA81DC6E5BA4EE34670E4D3BEC653FEC9DEDA2000C309A0
          EB632887C91C532E44CD79C00320C1E7C5D8C81F08D474E0FAE050E95847C5C1
          E196B3BD00F4E8E4AFDF562C9ACEBCEC761031E886CA8142D68804F660263C83
          8B816BE8EB1AAC090DB716F4541FAF778E9CD3737FE580F8FC936F655EDC3F0D
          4D4A31CBAC91BAC4A781981C753F76A0BDA5B76B7ABCB3E0F7CA0FCEDBC07A74
          D8FC97FA052180607C5EFA1699016A0A15C084580DED4DBD6869E89E989A1CF0
          F7541DFBCC989998B2FB4C9D3E1F89004B97344222656505C2FD28168C513437
          F5A3BEBAC3088D0D158FB47EE90F0D35066DC5D4568DC28A3E954E8FA24D1430
          85C4E6800B07886DA529A840A0BCFACC644FD5E1D1AB67BBE78FD6BB1E2F52FA
          4A3E3AEFD8365F832B02B001F4BA932F24D9846874C0D63E765C38EF780B8328
          7057041CDA0E20E63D4C44FBB9D7FFE5B7E28549B0BF97E4BE6E7D1BDE22F017
          8FBF08C63BAD11CF0000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F400000027744558745469746C6500526566726573685069766F745461626C65
          3B526566726573683B5570646174653BCE2F81B90000057649444154785EC596
          7B5054551CC757417C94D5586A218862BEA99141D47C80492AA646222222AEA2
          2108CB2EC86ABC1445D444507C0CB230A192814028BAA3016298049632A1026A
          012A222AF2721064AC66BE9DDF9D7BB7BD41701999E98FCF9CFDFDCEE57B3EF7
          CCB96790451CCC7165E433D04350962B00991464E131593FB5BCF803AD6D7FA2
          E5C5AB41199445999205B6EFCFE2167FDAD886DA578432288B32250B84457F4F
          F65C80AFE65A3BF8BE242883B22853B240E8DE732281033F3E14A05AD4EF0491
          00654A1608DEA3ED7101CA942CF0E5AEB33D2E4099920536EFCCECF133409992
          053645E8047A04CAA24CC902013B4E77B103DD17A04C65D44C595770021BC34F
          890462F21E087429F0A4E1858EC7F5AD1CCD2D2FB1717B064B96F522F405A86E
          27E0CF1ED617D877F19EC07F0A080B9654DC46E6A5E33894AA46E81147A8A26D
          B035CE11CAAFE45044CE50AF0F9F369149F4267CF7CE3CC224402389E804FCB6
          8905A272CA05A8EE70F147752DD05E3E896D09CB909C138182D293A878928FDA
          969B34524D7D6EDE33629AC27BF747713E7BA6A3A9AD1234F252BD380155D877
          22813DE7EF0850DD6EF18AEA1A249CDA89446D088AEF9E4155531ECAEBB5B8FD
          341D65B52934524D7D9AA7E798C4543C6E2E42D5B34BF49B040C7502CAADE922
          815DDA1201AADBBDB9267D0792B32370AFF107DCAA4D4546E11644A7C811B07F
          2EBED83E8546AAA9CFCDDF6DB880FB4D9750CAE488B5619349A00FA33727E0BB
          452C1091795D806AD1E2191792109BAE4679DD7914541C464C9A3B56055BC249
          65A198BF6AF46463F381836874525AF8529FE68B1EC6E25AF561145645A2B8E6
          6BB8054D2201239D8022344D24109E5124201228BE5D0AE5EEF9B85C9688A26A
          0DA292E570548CDF3D61EAE0A12C6C00A33FCF0027D5C484E50116B8F12809F9
          777721AF228C607F1707D62781BEFF0884A475790FD0893FA1D5B0D3AEC0AFD5
          F1F8265705677F0B4C983678080BEAC7BF91218F9193DF04DC6BC8C62F550771
          F1F7505CF82D8860F501D09C68077C4252BBFC7F8004EE3FAA47F4D1102CF11D
          8FB0435E58E23DE973FE4D0C8553CD63E0E0334E43CF758483F7B878D121F40A
          4CBEF2BCF565A7FF11D1E5F2ACB90D8DCF5A51DFF81C750DCD50AAFC07F287C9
          80C21CBCC7CA68E4650C79B9010C7AEE0D1AF9DA88247502EEAAF8B51EEAE345
          DE4127D13129D810980217A52B16798DE990859E63B8CB85472431576EBE70E1
          FAD199F41C1B35D6F6C60345F7006FFB3A6310E3ED0E788731E4538FF751D598
          88CABA7811D4A3392194FD1676C2C06AEE7B6F2DF21C83B3F941DC7331275661
          C1BA516A12A0E73801009D218419DAAF1B8582CA6064952974E4DEF107F5E7BB
          9B6B6841FD1DB05E60FCE6BC35E63BA28F3BE3EAFDADC828F4C2EA402BD8B98D
          E4AE6796275980DECC68DE9A913857E281A49F9D38BEBDEA8CC0C3B361BFD61C
          735CCD36D82C33FD60ACF5A0FE1FAF309B68E736C293FA51C71C915BAA82F6E6
          3A6C3E600BBB9566BEC225C4F2BA25D0F713F9089CB9B11A89050E1C49579622
          BBCC03C9397284C7DA43BEC992768246AE4ECB754761A51FCE5C5F8DE0585BCC
          596946BBD44FF802585EF776608E9B194E17AFC0B1C2CF40BF1591D688CB5A8C
          9C5BEEB852E983EBD5FE28A951D34835D7D7642F866AEF54D8BA0C8FFFD076B0
          A9FE17C032BA25D067F6CAE1387BC30534CE7236499CEE382CC05139167EFBA6
          2032D50E47F31623BD68298D54737D9A9FB1CCC4EFDF9F1FCB9011DD3A8436CB
          4D34B62B4C69F104FA6A08CB7943AD662C1D163C6BB9C9395B17D3076CFE2F1A
          A9A6BED582772DE97AD65F9C65C878240A88BF6B233DFA12C2FDCF784D80AFFB
          0937A5B0B88D8B894C0FC9023A890E30E0C5FA88E5A8D62DDC5B58FC55047422
          ED6B422C28F4440B4B10F85FF81BAC51A1B805198A790000000049454E44AE42
          6082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F40000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000000E744558745469746C6500436F756E7472793B9D0B04DD
          000009AD49444154785EC5976D8C5CD57DC67FE7DC7BE7CEEBCE78BC5EEF9BD7
          BB0683B1EB601A1C371817D4268AAAF45BA4004A9BF0A292F6431BA944084494
          B41F5A8AD2120922944869A4B4AA2A2A5E1A14DA10686ABA35B80263B08DB11D
          2FB6D7EB5DEFEBECBCDEB92FE79C9E5EDD0FABAE2A3EF6AF7DF4DC3D5733CFF3
          7FFEF3BFA311C618FE3F4B64CC3BF7E401509ABD893677260A62657E19297331
          4AC4EF4789793254E699AFBE997C171080C9980DFFB399376363D3EE86F3BAD2
          E667D22D7EB656A860A4607E798D28EC53AE6D65F74D137C7C69EEA9BFB9FD7A
          F0C8BBEA471BC4E506033A83CA587F9219F1C51B790238F047B7F83B6E99BAF1
          50B9588720A0EC06C4387CD4C8E3967C8AF112C5E01227967CF342EFCEE74E4F
          3C50925EF9204E6E4A2BD5D249704605EDE351F3DAF1E6E5A3671B177EBE0A44
          4002A89BEF7BC1A01330064838F7FCEF9119602F30FDB951B7F83BFB27F395A0
          895A59C635924AD9A71D4ADEBBD623E71BEA03501910BC71ADC6FB87BFC7AE4F
          1F666AB4C6D5959099F93697E75AACAD75E9763AD783D5CB3F6C5E3AFAF2EA87
          2F5D06FA407CC763A77492689A573EE0DC3F7D358BCF70D6E29137E693B5D74F
          5F5E5F571AE9418CA6DD89A8E40437D47C8200D6DAD06E19EE1C6E3175ECCFD8
          EAB5A8150D5BAA2E77FCDA20771D1CE3F64F4FB277FFD4F0B6A97DDFD976EB3D
          2FEFB8FB5BF7003520FFD65F7DCA1108308AAC90087875869F18C32F5FB3268E
          5D695D6B95EB383E28A9E9F6FAD44B0E5BF2B9D4C4FB8BF0D2E53A011E0BFFFC
          347947D30A14CBEDD89A71D951F7A8950B4CED1A63747272AA3A79F0073B3FF7
          178FE62AA34340FEBFFEFA5607BDD1800180E51E7FA221F9976BB17B7CB6B7DC
          2FD588A4E1172B9ABF5B2EF39F6191D33D41F3B30F32FCC4AB8C3FF61223F7FD
          3903A53C9EE3102786B9F5C85E0B46AD09216160A0C2B6D1315119DFFBA72387
          1E7E14A803FEF997FE400A21C812C8D67081EE7297872243E9B5F9B0323DA7CD
          74B34AEF2BCFB1FD3BAFB3F7E963DCF6EC7176DEFB04233B26D8363C4C79A04A
          D126E3380229C11582855642DE930C57BC942786CAEC9C18A73ABCE70FC70FFF
          F1034015F000012001BE78030650D6C4D9D5C079665DFB627A05F1F16D0F53DD
          7B98BDBB46B9F9861D6CDD7933F5C16D948A25BC9C8FE7B938528010080B3008
          03CD9E62A4E632B1D525529A4EE291AB0C8BE2B6DD0FD7777FFE2050021C614B
          C2868D067D75FCFE8FC3DA3E7F198F0B1F1E63B5D9254A40C81CC3B52223559F
          6AC1A5E80AA400630CAE14547C69EF79DCB43DC7E4568F4819A2D8B0D64A305A
          E0F803F8D51D6395B103F76F4801099041B8C5ADEE965D77DF53BFEB1B08E9D0
          B97A9A33FFFA639AAB8B5CB6F3996D44744245C183B1AACBFE111FAD0CBF3EEE
          73E360CE1A93B443CD470B21334B21BE0B9E2BD046A7700A759BC4C8DDD58943
          FB813C2025C0AB1711803B7CE0DEDDE5C1D1CFDF7AFBA718F9C237312A62EECD
          9F70FCDF7FCEF5C5251AED888566CCAF16FB9CBC1AF0D6C73D94D24CCFF478FB
          52C0A9B93E9796ADC940D10B35BD4853F605263128C0B845DCE2D096F2D02D5F
          201B43960002F00A83371DAC568A695C437BEEA47CE04B205D4E3EFF2433E7CF
          D2586FD10B62FA91268C35B13229A25011279A3806959D25169D409393A0B406
          6DD0B1417865DC7C79FFFF3620015F16EA876BB522312E5EB9CEF891AF511AB9
          19AD132EBCF234A98130218834419C7648122B82C410C59A585B24865869540C
          EDBE461A83B2504A632CE3967072C59D400170DC347E3203FEC06D03953C5164
          C87B05CA83631C79E82F79E7F517F18A5522E3D08D0C90759F28A2C4210C1561
          A209238D3562919AA01F6A8A058D529086A0C0481F47E606011F90EEC61148AF
          B063627B817AD96320EF50CE1770CD1626266F4C77BA5A29E379394060944E0D
          047DC591C99CE5846E4FD10EA01518D6BB9AF58EA16DCF46070C41CFD0978644
          BA345C59005C406C34204D1275CFCF76F395720EDF15E43CC96FEC2EF3C1428C
          510621FB08D1471B88B521490CBFBDCBE795533DFA91B248BBB65084B1C6249A
          C1BCB61BA451898512B8AE42C7619FAC5CC064D049AF31B3D6EC6F755C496211
          2963BB48105AA6F30693421950690286208CE9F69334FA3032A463482C424D5E
          18C2D8A4ABAA14200D26E9A2A34E0350809159F71A88C3D6B553AD75DB4D5FD3
          B5B091B2DC8A11C6D0B2D71D2BD4B1DCEE59D14061852D546ACEBEC68AA79D13
          DAEB48295C4713669BA1B54108017193246CCE037D4049630BB0200A96CFBDD7
          EDF6D31D0E328185D5088149D7AFD34D6865E29D9E15EF697BAEE85B049145A8
          ACB84AC5E35023D104969531184020205C25EAAC9D07BAA90180CC40D8BEFAF6
          E97EAFD1E874FAE9AAF5ACC04223C231A0137080D101977DDB3D0EEDCC7164CA
          A51D247C665C706058B267503232007969700524A1A2D935180376AC08DD245E
          BFDCE9ADCFBE057400E502009A3481F30BEDABEFBEEC17071EDC32B81D21216A
          6AAE979CF48BA59413E9A77A6E45D1E8699A81E2E098E4D84C4C3E07BE303818
          B6FA8AC040101BBA91464A89740434E708562F9E6CCD9F7907E86506C0D81242
          244073FD57AFFDB43878CBEFBA7E71C8F78B0CD98E55AC0985E6C2B584E55682
          3106AD2C2CB7EB0EEBDD04DDD4241A74A2A915049E48D22DD95276684702115E
          275A3EBDDE5BBBF22AB004F401E302645040B73B7FF2C3F59937FED62F561EAF
          8E8EA195C399D92EB5B26313900456CC088381D440A39D3E8AD126154769D2F8
          8344D24B4853ABC816BDD6053A8B1FFDA2317BE235A0012469E31600085B8004
          CAC0D4C81DDF787CF086DFFCB2280D81C8610C760C5ECAB32B1106C0186EDA26
          393BAFC91261A82A2CC36AD7A47397AA8D6C9EA13173F4E8FCA957BE0D9C03D6
          C90C48B2CAB64167B3B9B674E2C7CFAE5E3CFAD3687D16D56FA76B7465294C1F
          36235587BC803836740245A215791FB667E2CDBEC0F51C64B4885E7B3F155FBE
          F81FDF05668016993880C838C59E7B9FE7FCF3F70AC0036AC0D4D0ADF73D3430
          7EFB97DCDA645DB85B304E815A5192F7A01F1B5C34B191382265DA21903411DD
          39A2E553CDFF89DDFE7D1FB808AC01D1EEBBBEAE2F1CFD0100920D658CE6E037
          4F1A20CEE67471E9837F7C66F1837FF856E7D21BD3F1CA89AEE95CA4DF5DA0D5
          6901315A9894FB619BA87505B1F61EF1ECBF759B177E766C7566FA292BFE6416
          FB2A10D9F7D72803C0E604BEFCF7609485E1FC8B0F0AC005F2591ADB4BC3FB6E
          AB6CDFF75B6EA1B6C7CB97471CAF50958EEBEB380855D86E26FDD6F5A8BB723E
          685C7DAB75FDEC7160316BA40724BB8F7CDD90E95D98FEE126039B206C01D948
          F08132309071293B938006C24CA80B34330E8024BB6F36FD38DD64E0938D3880
          9BB1F37FFC304D3228C064C86AB381FF062CC1A4BBF2054CA60000000049454E
          44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F40000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C00000018744558745469746C650044617461536F757263653B4F
          7074696F6E7318FCC0560000086449444154785EAD970B8C54D51DC67FE7DE3B
          8FDDD95D768175915DC4E5B120A8485179F92012A848B1566AADADF88A8D425A
          437D244D6C40D16A526DB46A4BDAC6A0286855D0F22E4F85E016741741414411
          79BF66776776DEF7DE73FEDD4C6693D90D864DD32FF3CD9D47EEFDFFE67FCE37
          E75C252228A5E8265574ECFE65F7F7D2EDB59CE373448473C9394761F5C4FD13
          C3378D1B747D30608DB32CD560A11A0C462951FD416A041023A78D98132260B4
          39A03BEC7ABA71D947DF6C7DF1BD5D5940BA019D1740AD7CEED6AA81FD2AE639
          4EE0AE48F5C555E1DEB584CAAA08462A018D1D0CE17418E3E267DA6BDC74AC06
          E3916B6F1D9D4BC648B644B973AAD376DBA4A16FECF9E6EC53B35FDCD2DA5300
          055883FA57AEAB193DE3AA5E178DC20E0640E7109D0171416711D1E0B5E5011C
          CBC38E04C1D8844BABC1F4E6828B2F42BBD9AAD663471FB2C44C003A8C00D2A3
          0EC4A3F1AB460C9F8EE85670A3800BE2A38C463028D1A04094051DC618404004
          C4001ADBB1A81E58C7BEEDBBAEEC1CD21E031863F093DF6147FA433882D22930
          494467416710E5839F41290D28C4B240822814A21C1007D13932D1D318AD012C
          0AEA198036A4BE594B49BFCBB0FB0C43291B6595A15409A2C22865A10202C6C5
          7829DCD871947888DF8EB829FCE8315287F7E2074A31C6D0A99ECE81A2930C46
          04F2AD36881BC5A43EC38B37913EF8396E34493A9A221DCB10A828A5AA7E0825
          653548AA304F00F10D80E23CB2BA77E0FF256D84827A0CC0FF06A08ADC29D5F3
          21A048AEAB1B33B1B6F1A1BE3E764F0A2B851508831542D94144E5D05A889F6E
          C5A8D04EC0EA24534A9D3F05368CD7BE70E2B31DD8BD4E1028EF4B2012C109B8
          98D4596C4B7716C70AF7A1B4F730220346E0DAA5283B40E2F8E79CDCD3C8806B
          66523FF8A2AB274FABACDEB4F69D138000F2C8FCBF4A8740044178E1A9DF749D
          035A6BCAFBD55237763295434601103FF425873F5CCDA12DDB687E6D334DCBF6
          B36BFD77B424EA680D0CE7DF7B832C6D0AB3F92B43DF4BC73172C6ED540D1BC3
          8EC38609D7DFF8E5DCC7FFFCEA030F3F3306507F7A728E7AFE89D93CFFE49CC2
          70771B02D11AF133A0B394F6A9A7B47A20A871E0B660327BF1935F903DF2256E
          6B024AEBD87CA49411E3AE65D6A806D66CD841D37787B8A2F66276EC8FD1A77E
          24774F191F6EDAFDD59D4B976F748107002D00025AEBEE314419DD7D6133798B
          B8884E61DC36BCD459BC441AD171D02EAEEBE31B61DA94B12C5AD2C2BAE6A30C
          1D54C7FDB74CC4D386742687F6DC346003625B960198FDE873520C20E74D810A
          62851B707A07102B8D8406634735B17802258A4422C77DBF988E13B0D15A4867
          73584A116B4FE07AAE010280EAFC550B9F7F4C0352DC0172AEBFA3230563CF99
          02BB2FB15C098D072DCA6C9F6452B3BFB5947BA60DC1F335DA18DADAD30842E1
          9107B876EC157CBEF7EB39773D382F675B760D4A99782CFACAF2252FED067497
          14584AC69E3B051E7630CCD64F5BE97BE195B4B4C5A14A983D630C65651192A9
          6C1EA0B370E7B316305AF3E0BD339DF55B763C36A0FF0508B0E8CD0FACC29C30
          5D00C437F91454D78C20E397903A7D3C9F82D4A983940EBC8C3DFB93FC7EC648
          6CC70105999C473C91C6180104413002220090CB79E45C0FC7514C9B32111185
          52860EE89F8EBBEE47BFFBCFD65567BA006823E74C81E45AF0BC0C430F37B3FE
          C39D4CBEFE6A62890C48BE2020445BDAF8E7DB2B387AEC24B5B51772EBCC1B29
          89948180E7094732AD545595B17EFD365AA2AD8B3A8AC700B1BAA4C0D7DF9B02
          DBB431734A35B96833AF2F5D454938886F34A6C35A1BDE7B6F0D232E19CCB34F
          FF9611C3EB59B36A1308F85AE31B4365AF525E5FFC3E6BD66E98F7EEE2E71E07
          7C408A5320F2BD29108C771CC9C5B8E6D2100BDE38C02C6B1A5A0B46048C108B
          C5B976E20F30584C183F9A9DCDFBE85556C29996761008D8365F7DFD2D2BDF5D
          F86AA1B801B08A0074CED33B0F7DB60F2F9BA5AB14CAA9450203D8D094E2B291
          0D58B68DAF0D9ED7616D183A74103B9BF6627C97A68E6B0C1B5A8F63E72399EF
          5422996178433D3FFCF1FDBF0282800DD0B92D5740E067A3FB5D306970E59391
          50F0B69A4B469557375C4EC5857544AA4A2118E69D757BA91B7805D3A64EE46C
          6B8258220D081800CDC78DBB3872F424F503EBB861D2D5F85A38156DEF8C26F5
          75D5AC59B785D56BB7BCF6F187CBE7C65A4F278B0154812A7869BF48E4B6CB6B
          AEEB15B22638B63D24142A193CF4A6BBAB973625AA9F9EFF109E67F20948A573
          78BEE9B006255496975055114150B4C5529C696BC7F7359605A5E160BE5355E5
          619E78EAE574F38E4D0DDF1ED875BA13A078F762156C171D1D2074F3EDBF7EE6
          2737DF786F5B3C81883076CC289C508874CE43C40060C4800085485A96C2689F
          C6C6262A2ACA088542BCBF7CD5928D2B5F9D03A49DA2BB1629ACDB0630802EDE
          B203D9E8D9132F2DFB60F529CF75DB05E9F3C5BE03B367FDF2D688B21C3CA311
          0191AEFF84B94C96B7DF5E913B72F8E03F6CDB095AB653DED672F215C0054CBE
          0345FABE8D832AEE46C12593A7DFFDC8D4A9373C3669D2445A6229CACAC2041C
          9B6CD623914E130987D9B4F123B67DB4EDA56D1BDFFA239001FC42710F3016DD
          2422DD0D20B7DCF1B02E9C942D5C28A39465555694130A0628090758F6EE2A16
          2C78810FDE5F43492048CEF3F26D179042F154C12E6000B1E8A1B4D1C4936929
          5AA7B54089EDD864B259B66FFF845DCD4D0BD72EFB4B4373D3A77FDFB37B0F6E
          CECD0F83522A04F8059B53D1B8B961FA7D02D06300A335C608ADF1542784DFD6
          7266F19B4B96BDF687675F4E6EDCB079E5D6F54B1700D16D1BDE9ABF62C5BA95
          7F5BB828F5AF156B16B7B59C7A0370013971362646A4B07E80D3630063F8F93D
          8F82089D00CD8DAB7703736B075E323F9B4E6481764003D94FB6AFBCB7576575
          F9C9635FC7812CE00372C7ACB95D56CCFF02A5A697D511F79A39000000004945
          4E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F400000030744558745469746C650044423B536F757263653B53746F723B6461
          7461736F757263653B44617461626173653B4164643B4E657722FBFACB000006
          0B49444154785EB5977B885C6719C67FDF39672E3BB3D975B3D75C6813DA6C53
          6BDA689B10F34753AFB4188A44222A16FF511425D0FEA1486991D28A8A171AA5
          0A05F1C252E83F56A425464C9A429ADE2CAEA4B9983636DD6C36BBC9CEEEECEE
          CC9E33E77CDFFB9A0CA7279B6D6A66353EF0C0CC301FDFF3BEE779DEF37D01FF
          1986A541B9025495F743F03E1B9A859FAF2248530268C616112CDAC4FBF69736
          15777E7CFD9D41603EEA7B66D0336650518C9A95A0FD0AA8E884A88CA982B3F2
          A675EE442D4C5E19DA7BFC85DFED3D1A0292096955C06F1FBA67F9C6757D0FE7
          F3F9FBCABD6BBA8ACB575168EF225FFE00E0F0F305820B44626C38DB1FCF57FB
          9184C6ECD4871BB52AB5CA24BB3E5798FEDAF65B860E1D1E7BEC3B4FBE58C984
          5C458001BCDBD70FEC59F1917BEFE8BCEE36FC7C0E5C037521680C2E42D54132
          0D121378097E390FE2532CF5822CA76FCD75B838EA9A1A3DBDCB43B70217A9AD
          0800F0E62AB3777C68FD67503705F12410835A8C3814C1A803036A3CB848498B
          530515C0E1071EBDD7AFE6E88B7FBF1DF05AF580018C88606BA7F0CB2BA158C6
          B83A480D7511B81035166C88310E30A8E781E63118D404A001EA1A8493138873
          A4024CAB1E409C507F6B0F6D031BF0BB6FC2181FE3B5634C1B6A8A18E361720A
          1223499DB87A06A3096A67D1B88E9D1CA5FECE116CAE8488009825A5205D0408
          A20ACD560B1A4F22F5619299D7993F799878B2C6FC649DF96A48AEA344D7DA1B
          696BEF47EBA94F00B5C29263284EB85670A24BEE00D75240EA01966242E2D8BE
          1C56A7B7147A2C3EFF1D9C13AAE315A2867D7561078C3157F7808FD9E2AC3236
          FC0A7EE718B9653DE4CA65825C8CD4CFE37B6E41854A1C392448A88E4D31EDEA
          D4CF4D13CECED0B96A35BE6133E0A51440AF3A079C732C1B58456FFF07096D1B
          F58933CCBC7D8CDAD8716CED34B3A74F104D4D631B0E152597F3F0F2968EA444
          476F3B03370D52EAEEC3EFBB8DDC7802BB0F05AD88C83AA0CEA136041751EA5E
          4BA9F77A305B20AE20E1116CED30D1C83192A939C2A91AF3D37304CBCA74AE5E
          4BA1AD1B99ADE1F7DCC25F670E32E18DF38BE7EE9944C1891EB8FFDE3F7F1290
          C7FF74B7AA2AA8F2C067FFB2D8844A061490265563D4D591781A5B3F4F325723
          9EA913CF8628092E9A438312EA123428706262846FEEF83929D8FDF4B7EE22ED
          C2573EF533007E32F4F5FF4F0A54C13A47C356393D7D9062D0D9FC9E4DDBB4CA
          F665F9F78EE28BCEBD9082CD4B4981EA02A288139C15C425242EC237456C22D9
          58EE2E6FF0161D5CF4520A3CDDBC9414505C4179F063783D6B882420D679A2B1
          0A4922381AC417057821D63A805C5AA8A614C05D6E422B2DA7404459B77D2707
          1AC7991C7D397B218A2ACE39AC2424362230059C13BEF79B3B6745154DE9ACBE
          F0C36F1CFA349064029C68CB2998AFD428AEEEE7CDA3FBD9F5C5EFB318613245
          6C433CAFC09777DCC740C70694140A8FFEEAFE6D800FD84C8058D7720A1A3375
          728DA8D9DE5A63826367F7A057F047C38D33138EF34EE5F5EC01B4E53AB0F692
          3183ECFF4B4F01CE3AAAE149AAF3E3996C5451A3D4A319CA854E400183A28032
          A5CD7597A540011B25F6B5B7878F6E5A77D70DE45B38AB4BA3468FE9E697BF7E
          2A7BFE2A4A79599ECDDB7A11159C082FED1FA75A895155521FE012D907384053
          01C4CF0C4F7CBE1E1E78E41FFB0EED18B879637BEFE0AD74AC584D7B571189C6
          692B66AA710ECEBDF13C5FB8F56ECCC66E6C62A85743A62AB3F153E79ECEDFBC
          722BAF9E7C0E11D74CC51F76FFB30748168C649B3213609F3D72FEEC05EEBAB1
          BBEDC19D67E7B62D7FE9D09682EFDDE01BD600E43D6F0D464BDAAC54E7231939
          650E3E4923D191C8BA9313B5F8B5BDA3EE6F5BBFDAF786AA20A23871CD54A49B
          47805B747FC80448AAD0BD55091B3FD87FEA8FC0B3A953BD8C9743B2B519C96D
          4A7A1051441541B1562E73F5F09927F4911FFF94671EFF1700C1A2C9E4529A85
          6CE156A4A95871CEA10885A044B30B2E13A0C3A34F68D3032257BA9ABD37492C
          1D76F26CB4EFD1C7863E81427333A799E11EF8EE8F509472A7472640555BBA64
          B670BA11C0EEFBFDC8F6B4306F81E112409F1F1A613102AE1D14700BFC64005D
          C4FF5D80AA5E4D84B204FC1B2978EAAF250E06750000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F40000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C00000011744558745469746C650044617461536F757263653B05
          A9AFE1000006B849444154785EAD976B8C54671DC67FEF3933BBCBCE5E58603B
          ECB2DC11DBDD2DB40A75A1F66AB1DAA65A5B89C4103E194DA3D1449B4ABFD41A
          63A01AD39292C6D4D60F6D638282411A2D605BC1962C17294861215C16E9C202
          DDDBECEE5CCE9C79DFFFDFDDCD4C3219365996F224BF9C39F3CE649E3CEFFBCC
          3FC7A82AC6184A648AAEA58BA5F75AF25AC7791F55653C45C6F961F3FCF7EEAE
          78A46DC17D6551AFCDF3CC620FB3581063D43482C61550D1ABA2D2AD0AE2E4B4
          1B21CCB9F66D7BCFFEFBA5AD4702404B0C4D68C0BCFDDB27EAE6CEAC792E1289
          AE8BD5CFABAB98368BF2AA3ACA625301875F564E640424C46686E2613A114772
          6487FAEFCC261324FB7A59FBD5C8C0EAFB3FF7E6B1B33DBF7AEAA57FF55FAF01
          03780B1AA7EE8CDFF9D8F2DA394BF1CBA2E0B2A8CB8086E002541DE406404222
          5E0E3F5606E25351590F328D5BE6CDC185415DFFC5AE1F7B2A2B815134CFC409
          0CF60E2E6FBEF551D4F543D80B84A016230E4530EAC0801A0F46110114741401
          1C7EC4A37E6E131DFB8E2C2B6CE9751B10116CF27FF8B146A888615C0A2489BA
          005C0635166C06631C6050CF032DC3605013018DA02E4BA6F72AE21C80479126
          36E084D4D9779832F376FCE99FC7181FE35561CC14D454608C87892A4888E452
          84894B18CDA176080D53D8DE8BA42E9CC0462B11114A34E11928FA9220AA3016
          B5A0612F923A4A6EF030E9731F13F62649F7A6482732446B2AA99BBF88295571
          34953F27805A01304C20AF34819B25270AC0640C70330D887393D8823C61E8DA
          33898115E5332C3E3726E784C4953EC2D0ED2FDE0263CCC42DF06185B34AF7D1
          03F8B5DD44AB67108DC588444324D583EFB9A2B4943070482447569AC8242204
          03969EE39D4C9DD58487B6156A78DD2D70CE513D7316F5F16632760AA9AB9718
          3C7F9264F7296CB28BA1AED304FD03D8AC434589463D4C9965DA8A160E9D0B59
          B5AC81F8A2D9840317E8EC3A492181753F78AE781EA00AA8F0D66BBF2E3600EA
          1C6A33E0022AA7CFA7B27E2E983608FB90CC096CF263824F4E92EB1F26D39F24
          3D308C5715A3A6613ABBB776E045233CD8320FCF3A9CEBA020112936000AA0A5
          35C4882B1D6C32866A88BA14120E60533DE48693848329C2A10CBE84B8308313
          C5562D62D747C759B5FC56A6CEED060E7A00BFDBF00C008AA20A3FFDF946548B
          0D807E9616A88E223CFED8036CDDEED875F014CD0BEF00F0012325A358450A06
          F08A12201BDA03232D40AC65321211740CB8E7BE365CED623E38931C06CA00BF
          A1BED6A82AA3C8282238916BFF883CA35F2AB4A0FBC06E7A8EEF27D17982D4A7
          DD23F4E072D7B6209BB604C90051451092E92C5FBEB78D9A39ADD58F3EF1FDA7
          810A20D278CB544FF2269C3854E4DA16A89549B740239658328B3A41059C1386
          530177DF7317CEC9D318ECDFB7BDBA01C834C5EBE8BADC2F2A6346C6A9A1E8A4
          5B606295D436CC40F412A23A664075D484E30B5F5C4A1004EBC529EF6CFFC306
          2098DD30CD3EFE9D9F888EDB02EB26DD024F42C46611515405EB1C8892738EC4
          608ADB5A5A704ED63B1176EF787D2390D9BE6593FDC6EA1F49690B546FB8058A
          8843642C014484E4709A6C264B80A1F9F656AC75EB9DCDE97BFF78E337407AC7
          5F365B40BD22032E9B7307CF1FED20170493AC20885344149BB30C0DA5C7CE81
          A8A2E2488CDCB72C69E5A1555F79F6C1AFAD7D0688E5D337C509D89D1D3D4F06
          D9BDBF3CF65EFBEAF86D4BABEB172FA1A6A189AABA0A24B8C2940A573474209D
          31A04A24112063F10B83434932991C9E0101EAA7D5800180E5772D43C53DEB9C
          E8DE7FFEE905205D782E280C0D1F286B9D198BAD5E12BFB7B6DC5B19F1BC45BE
          613E40C4980518AD545554341D8A768AA8695DF3B38A17B7772C7C79D3F3EC3F
          7206DFF7A888FA4446F8FD2B7F240C43505014D4A0AA84B9E0177B76BEB9B160
          80A2A9E5E5F14BAEA6888234BF5EFBC8B79E3ABBE9C5E7397CE23CD81C7FDBFE
          B67E77ED1AF3C1DE0FD9F3FEBB2F1CFC70C76B401AB07902208C140D09CD1B91
          3C0E6082916A80285026228C62C3806D7FFEEBE5BE9E9EA397BA2E7CBD75492B
          FF3974F8496033300C848014F0B8569AA7C8C8C4880A57AEF6B075CBD68B87F7
          BFBFE6CAE5F3AFEEDB7788A6C6384DB39B162D5DF6D04A80A20404D0C8F53CBF
          01E32EE413534054849737BFDE75ECA37DDFFEE4FCF14EA07C66E3C2A3FD7D7D
          7734B734D379F6DC3AE0DD429AF73FBC4EF7EC7A038FCF2E0142E7ECB9FF1E6E
          FF66E7E923A78024904A0D27DE6ADF7F88EADA6AFC48F481E2AD54CDCF829B64
          20DB73B5EBE133A70E5E067280009C3979604B6555ED0F0FB41FFA34991C7805
          C816D254290C20556E5485F6E4F10ADB51D4A6F23C1E100201E08A3EC3FF0169
          9B59CDABAB87F90000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
          00097048597300000EC300000EC301C76FA86400000150494441545847ED95B1
          6EC2301086F308BC4987A47B47461EA19BC354D66E7D11C490786181910DA9CF
          508985A52F501E21DC7FBD0B0601314E82977CD2499673BEDFBE3B3BC9401B5E
          3E16A35753BE67A65C65C61EB2DC566C18D31CBEC147DCBB25CB8BD999E82D63
          9F6226CBBA21CDED4205C69FEB6ABED9553FBF7F958231E6F04DFDB04696B7C3
          15874813F0E96C139C7609B6FCDE8B4433F0D575C1E5403369CD7D4E7E499D09
          8A11D498DCED1400750D457B02B124AC3F7CD5024FAF9CB250AE24AC3F9A7EB7
          DB1F056BFF37600F12D61F5E48D6168D2361FD89BF81E82588DD84D1AF61F487
          0850802D07217BFA53ECFE84D47C32519F9C2CF867744D5C0D7585887B3B30C6
          9CD61CD687F8567BE2AEB14F876987E969D04C7C3B7045DDCD604C73F816DC70
          4DE2BD3288C71137F62B9A3820B1FA957BBA3848A776124D5C494DF186FB8B72
          C8D440CF24C9110EC5CE4F7BAB0A0F0000000049454E44AE426082}
      end>
  end
  object ActionList: TActionList
    Images = ImageListSmall
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
      ShortCut = 16449
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
      OnExecute = ActionTranslationStateProposeExecute
      OnUpdate = ActionTranslationStateUpdate
    end
    object ActionTranslationStateAccept: TAction
      Category = 'Translation'
      Caption = '&Accept translation'
      ImageIndex = 8
      OnExecute = ActionTranslationStateAcceptExecute
      OnUpdate = ActionTranslationStateUpdate
    end
    object ActionTranslationStateReject: TAction
      Category = 'Translation'
      Caption = '&Reject translation'
      ImageIndex = 9
      OnExecute = ActionTranslationStateRejectExecute
      OnUpdate = ActionTranslationStateUpdate
    end
    object ActionStatusTranslate: TAction
      Category = 'Translation'
      Caption = 'T&ranslate'
      ImageIndex = 10
      OnExecute = ActionStatusTranslateExecute
      OnUpdate = ActionStatusTranslateUpdate
    end
    object ActionStatusDontTranslate: TAction
      Category = 'Translation'
      Caption = '&Don'#39't translate'
      ImageIndex = 12
      OnExecute = ActionStatusDontTranslateExecute
      OnUpdate = ActionStatusDontTranslateUpdate
    end
    object ActionStatusHold: TAction
      Category = 'Translation'
      Caption = '&Hold'
      ImageIndex = 11
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
    object ActionAutomationWebLookup: TAction
      Category = 'Automation'
      Caption = 'Web lookup'
      Hint = 'Perform machine translation using a web service'
      ImageIndex = 22
      OnExecute = ActionAutomationWebLookupExecute
      OnUpdate = ActionAutomationWebLookupUpdate
    end
    object ActionAutomationMemory: TAction
      Category = 'Automation'
      Caption = 'Open dictionary'
      Hint = 'Open translation memory'
      ImageIndex = 23
      OnExecute = ActionAutomationMemoryExecute
    end
    object ActionAutomationMemoryAdd: TAction
      Category = 'Automation'
      Caption = 'Add to dictionary'
      Hint = 'Add the selected translations to the translation memory'
      ImageIndex = 24
      OnExecute = ActionAutomationMemoryAddExecute
      OnUpdate = ActionAutomationMemoryAddUpdate
    end
    object ActionAutomationMemoryTranslate: TAction
      Category = 'Automation'
      Caption = 'Translate from dictionary'
      Hint = 'Translate the selected items from translation memory'
      ImageIndex = 25
      OnExecute = ActionAutomationMemoryTranslateExecute
      OnUpdate = ActionAutomationMemoryTranslateUpdate
    end
    object ActionFindNext: TAction
      Category = 'Find'
      Caption = 'Find next'
      ImageIndex = 26
      OnExecute = ActionFindNextExecute
      OnUpdate = ActionFindNextUpdate
    end
    object ActionGotoNext: TAction
      Category = 'Find'
      Caption = 'Goto...'
      ImageIndex = 20
      OnExecute = ActionDummyExecute
      OnUpdate = ActionHasModulesUpdate
    end
    object ActionGotoNextUntranslated: TAction
      Category = 'Find'
      Caption = 'Next untranslated'
      Hint = 'Find the next untranslated item'
      ImageIndex = 50
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
      ImageIndex = 37
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
      ImageIndex = 9
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
      ImageIndex = 10
      OnExecute = ActionGotoNextStateExecute
    end
    object ActionGotoNextStateExisting: TAction
      Tag = 1
      Category = 'Find'
      Caption = 'Existing'
      ImageIndex = 50
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
  end
  object OpenDialogProject: TOpenDialog
    Filter = 'Translation projects (*.xlat)|*.xlat|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 448
    Top = 364
  end
  object ImageListTree: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 18874432
    ImageInfo = <
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001B744558745469746C65004164643B506C75733B426172733B526962
          626F6E3B9506332F0000036349444154785E35927D6C535518C69F73EE6DEB64
          63A3AEFB60A3A36E33B8C56581E0D8707E21CC1A43A2A22304FE3001512A86C4
          E900132451FF503367420043B244364C483031465C248B4441C0980C45B4D065
          CDBA4ECAE82AAC5DBBDE8FF3E1BD27F1397973DE9C3CBFF7233964226FC2D543
          A53E0280443E3FD752525AB14323FA06685A3381E492F329C6ADF39954E2F8C9
          C3DBA6018858DE940A9C2C5870C1D51BB6FAF61DBB327860F81A1BFE25297FB8
          3127C7EFE4E5D5745E9EBB9991239766E481937FE4DE1818DB0DC0EB322EABBA
          B63FD5EB7D6CCBBE6F1B83FE9E67BA82E084C0E4123697CAE0D109BC94805B0C
          E7AFCC606A66EEECF75FBCBB753AFAEB2201A0BD3E7861B02914D8DBF34408A9
          AC0D2181D3672E23319D81AB950D016CEBED824E809A722FC62E4CE17A343130
          D4DF73507FB9FFAB551E9F6FCF93EB82B879BB088D52504A14FCC9CE4E95F79D
          B80CD396284A8179C7D3DD1144F29FEC5BE1D73E1BA6BEB2C09BEDCD955A7CCE
          44D1744C1687C9045C05EBFC686F0DAADCB08413D2098E89B4E1BC5779965687
          5ED585D03ACBFDA548E7197EFA711C776EDFC5FF12200A7075F4E85975D7D4FA
          F1F4A635A82C5F02A2956CD46D2EEB1D160B455BC19FEE5E0F4A885A45828071
          81137D1B61DB0C1E5D43E4C8CF5858E4D0A1810BBA5CB76DEEBDB768C1E604AE
          EA6B1F40D9121F0A265385BC0E5457530109404A8010E27805EEE60598CDA15B
          8699C8E7CD4784EEC3F2BA00767C340A4AA9327E79300CE1505BDEFF0E9AA681
          5082150DD5604CA26858282E1693D428E42F6666B3909068EF68C5E6171FC7E6
          17BA611A260C93A9029C713CF7FC3A3C1BEE404B5B2398E0989FCBA190FD774C
          CFA46243B11B4B77ADADF67BB236478E10500AA5D2121D5C48354D3A674108A1
          56114C201E4BB1D9F86FA70880FB1EDD3E34B0A229B4E7E1350FC2E22E2011BF
          16C3FCBD050557562DC3CA964608B8B4C4E49F4924A27F1F193F1DD9AF03B0FE
          1AFDE03D113EDC6431B1A96575089212B4AD6D555F581280D902398343308EC9
          EB49DC9A981A75E043000CA46D09005A49457059DB4BC78E77EDFCDAEAFDF892
          DC3B1295EF7C13977D4E444E45E52BCE5BE7AE338555E10FDF0650EE32B30E4B
          D24C0212A8F210EAAED3D01969BB3FD0BCDDE32BEB06D56AD5D09CCDDA66EE62
          EED6EF43A9AB2331008603ABCEFF019D3AAD15CCD8D2E00000000049454E44AE
          426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000042744558745469746C6500436F6E646974696F6E616C466F726D6174
          74696E7349636F6E5365745175617274657273353B436F6E646974696F6E616C
          466F726D617474696E673B0760EE330000027D49444154785E4D93D952135110
          860F8BAC17EE98C4AC8421936466C22CC9CC649909D94608AB884870412F7C27
          1E032F44CAE209A8F2C69790A80FA0376D77D71093AAAF4E55D2DFDFE774CE11
          EDFE7B063F13C8A46C76760A66EF0CB92E5AC19F82D9BD968DEE594E6FED62CD
          14D579C189F07A0391373A42B4FAA72C67955A346F76CF357B139CF54368F44E
          C00FDE40A33B00BBF902D4CA06E4F4F6795AB663545F6F1F0B59EF0896574BEB
          B982D5BBB1EAFBE00524BE06AF479C7040A333807AE715E8D56D0AF9812189D0
          13E251343B23EBED6F658F65EC3C60A9DE3D66A98654DBC411545B471C22A9DE
          270C9896345F88D552F39D527E86C5A1C0C5E3D24B70113A96D33CE4E36043C8
          149CB73433B1AAF95FACFADE48648158278905A6E2130750F10EA08473CAE4DD
          4B0CB82324D5FFE960814B1D42C121E956F09F43D9431AFB60317BA0BB3B1470
          830133624569A07430EA322E59DE3E8BB44393A8ED825125B60107F91703E6C4
          72A1F68B522BA32E5CCC923112764047485C43E8AF4E4AD6100366455AAE5CAA
          384473D4652F945840710BD6DC6D28B97D283988DDE721C6B3FA573E423C6B7C
          CC2A7512189D241725678B0446B391CA26A8CC06A473364452C50F7C33E717EF
          2D2456ACEF79BDF55F22C126A14F0283BB24E80E402CA55DD0F9C3EB2F26979E
          CA5A7CC518E28FA140320BA020C572008A15C072B106B14C6978FF7152222F9A
          D6C8E794A9074BA9742CA37D4E4A65C0BB0105A30B4533803CAE92E243226B42
          24A15CDC7D9858A67A7A4891A42A047614D8857782CC3D89CBA791A472154D29
          BFA3290D688D248B574BB11C3DD97992E911E12005D6098145229A5671556F77
          338DCC220BC862B8CE85DF4F9034CE3F501D414BCD7956C90000000049454E44
          AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000044744558745469746C6500436F6E646974696F6E616C466F726D6174
          74696E7349636F6E536574526564546F426C61636B343B436F6E646974696F6E
          616C466F726D617474696E673B98A0D4C8000002D249444154785E4D904D685C
          5518869F7373D330C9CC346D8DC224216A37D5858A228822940856A9687ECD4C
          4B25585D68144A8A15244A37AE224840574550680B75E1602A81800846D4855D
          B8C8A2D4D8665A4D9BA449E6CE4C66EEBD73EEF99CCB3D48CFE1E17DCFE17BBF
          8F73902F67620014D076757C64E8467EECDCCD63AF5F593B3E11940AE357AE4F
          8C9E5B1E191A065C40C9DC19626E4D8CF17F78FEC5177A57F363F3EB6F9D90FA
          D9F7447FF681C8DC872D3D2DF54FA6E4F664415646872E5F3AFC7C1FE0C8EC34
          ABA3C300A85F8EBEF468293FBA519E3E2966F694C8C79322670A22D3E322A7F3
          221F9D10F3E9946CBF3F292BC3AFAE5F7CEED9070167E5B5575047FB7A53734F
          3FF57BCFC1FB1FCFA6814A192203825D0262C000D92C5E0D56AF6D2C3CB1B838
          0284EA8F2347DECCF564BFCA0D7442C54B02912401654004B0670C7465289502
          AEAF974F0EFEBAF48DDB81E4D3FBDA61671B8C80DC8B4930582FE06FB237DB4D
          E71D8E01175C9AD19329F1210C40489A609267401232366C7D574795B6307A0C
          687325D007DCDD2A441AB0C546126F9B8808AA8518137B5C1DA1B5E9069413F8
          CD6D5D6B40D08430408216CDB0A521E8F82E44B520C6FA60D727F49B1E805B0B
          FD3FBD5AC7E0FE94C651A0A2640A765A0CB11A00414542B9EE50F5F53220EE46
          3D28AEADABC103398D510206C4181050242A221069C4FA7FB6DAB9DDF0BF059A
          EECCADBFCF7FA11F9ADABFC73994DB2B104560042199AEAC62042258AB4069CB
          FCF8F6E68DF380765742BFF65BD57BC3D1E985A06EEE1BD86750F6D789C56EA3
          E1A6A7B876D7D95AAC7BA780C60FDD03D206C8CF8DEA467B24C554C37DA4E2F1
          B0AB22DA89708D21080D772BB07C07AEEEF053B1E18D7FED97FF02A2BC9B8166
          E1190005B840E6F3AE07DEB998EE5B2A667A77E6D3FDF25D3AB773A133B7349B
          EA7917C8DA3A15BE7C90622A87FA3EDD0F806018AAFDAB803D16D73616400361
          4C2B24DCB3FE03F304B94D918F11270000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000003F744558745469746C6500436F6E646974696F6E616C466F726D6174
          74696E7349636F6E5365745369676E73333B436F6E646974696F6E616C466F72
          6D617474696E673BC5369B60000002EB49444154785E5D536F48536F143EEF76
          27685991058549C36CA985454490CD597D8A82FC4384F8A1A2B0D035B5BEE497
          FA90066626044260189893A00F15094EC2922611A39A12A63FB7F99BD3253A37
          67B9393777DFB7F3BE779A74EECECE3D7F9EE73CEFBD5C70F95F084723E86AE3
          FDFCE2EA26FDD3DAE682AFB75A0C511E795E557FAC04FB123A71F8CCE09833C3
          CD47060067025C663A945ED3A47FDBF0AC9859BE36B0414F1B73CC75F188F93D
          56DF5EC46E341EEF3E5F99B70B01AA315F27E03C08F0E5BA23B9B75A4EF83ADF
          D5B021EF13F6D155C72CA3D758F7F025D63352C1FA9DB7D9E0542B7BDE6B62D5
          0FF4B3A5D70F683989A9311FC8DEBC6DC9A7CB759F0BF30B0EA667A44220E402
          CA647120C6008D89480881B48D5930ED5984BE81819EB6BBB6526CC6C8D53B47
          AFECC9CC68371872617E6902C11484AD81C5BF5242D2B4642D58ADA3E0189BBC
          6A7E68EF90240D29D36569613E3C053295D78657592832204E446E81B0077459
          BBC1EDF69663DA250190C31B360344681498185206293A41350817A4D81391E2
          95B2990251B13CFED624949C4649586C17A66C52404281C884AFF2534D0864C6
          B6F01D523C2ECF8796425B89860F5001123F854C5145D6D50881700417AED05F
          3C55C596E9907F6E09C1042895D119B2D3C43D15A4A246454DA8F0FB22105D8E
          0FF315526831FA7ADC1138B57DE70E04F235CA51D8BFC7614CBC4A3510187706
          617121FA125B2B2AEB2BB7D9EB0DFC3731BE006A2209CC9A028C3246BE5D51AF
          8109D76FF0FC1FE8EBED709AB11457057D91D04FE7E2C541DBB4DF3112048224
          04545C8120E1A6226A017662DF6E9B0DB87F046BB11C29ADDECF54D8973F757B
          86866D3EFDB7CFD3EFFB2D1E98F546418E4990A44981F88A0433DE08F45B26C1
          FE69E6C388CD57F0DD3AE3E038CA95D9A71E0B75892F2DF5645966E5D98A7DD6
          22634EB0C494C3CE19B383672A74D6C20BDA2AEC6F4ACC912F9E662832660329
          36E502614C487ED33ACA8992122E29C4A215478F7147108375F6070B79B97E38
          56EEF50000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000021744558745469746C65004170706C793B4F4B3B436865636B3B4261
          72733B526962626F6E3B6463C8680000037D49444154785E4D8E7F4C94051CC6
          3FEF7B77E02073EA92742577579B684891E62AB6C4526B6013696BC9DC5A0B33
          D0322DA716D3CA94A21A8E96E976AEB654688E409B46B859CC249500E3F81588
          234EE0F875DC1DF7A3BB7BDFF7DBC16AEBD9F3D9BEFF3CCFF7C13555CB58A801
          40014CC5E5696BF638D24FBEF7EDF2D683550F7B0E5666B4969C5A5EBBEBCB65
          2F0209803A116E6438F82377A66A60385007A0E4EFB2A5BC51B1B4AEF4EC5AB9
          D476583A87AA642C7055BA47CE4A43F72752713157F67D93DE54B0DFBE04308D
          867E9E290050725F4BBDB7F8E8B29EAA86B7C4E5BF203DDEE3D23E71585AC6F6
          48E7E4C7D2E777C870A05E7E68DE277B4F668C6EDE6BCF00D4017F350A607EF5
          48DAB99CECBC9CF4343BC3E1264CAA60C282AAA8288A028A30313E852DE509EE
          0C4D72EEF26967CD17FD4F0EDE0A064DF9BBEDEB6CD6C51F3C9DF5382EFF1540
          104014216E500C2ED6DDA4F67C3BEDB79BC9C95EC3E8F8784AD28288BBADC1D3
          6C4E98652A7C2C7D2543816674430304C4885B0755E1CC99EBCC51D750F14E35
          DBCB32E91DF98DCCA5ABE8FCB36733E0500D3132EF9EAB108C7AE9ED1BA6B4AC
          969F2E39896A11CE5F68212529975D5B4A395A59C40B79CF6049D0489AAD81AA
          3C0A9854436741140FE148809AEA16CA8AAEA34C65F1E9E7F524EBEBD99A7F80
          53751FB2707118EB836642311F31C63174497C286BEE6C55D3F48971DF2088C1
          A60D6BF9BAB6849D0547D8FD520D2F3F5F822FD8C7AFCEEF58B16A11FEC82831
          3DC6A87F8868C488745C9D0C9AF5A8D2E51EF15BE72FD248B127E2F5FE8DE3FB
          FDEC28280755E1FDCFB691BF310B6FC48566C4C030F08D458984B40E4057837E
          ADAAA7CB87A0E2090EB2E491594C1A4DD45C2EC779AB0E53B287C4399384A353
          718288A8F4767B09F8F4F380069094BBDD7AB3E474869CB8B1428E5DCB90AAB6
          0DB2E59055B2B621C72EAF93134D99723C8EE3F79572A83A5336EEB439EF9A67
          990FA82A1071F7855EF9E35AC0D3EB0C010A9EF000799B56F1EEDBAFC7BF87D0
          0D411185BEEE30AD8DFE88AB2B501CF0C4FC5706DE34CC0D7F15E9AB53BF6A17
          784ED78C4AB72BF6803DDD82B6B013D5A420064CB875FABB628CB8A21DEEDBA1
          A2D6FAB11B8066480C7EE92F045000737CD6BCA736DFB77F7D616A63EE769BCC
          B0C326CF6E4D6D5B5D70FF47C9732CF700164099CE4D3373FCA76CAB43052CFF
          62065440001D884E130F19FC4FFF00FE20CB5D5DF1FFF30000000049454E44AE
          426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C00000011744558745469746C650050617573653B53746F703B3B
          8A7A3C0000012249444154785EAD91C14AC4301086A7D68AE89328EC9BE81EF5
          A6AFA04751F02D647D0341104FB23E8908227A12B6AE2D6B366B9B99D4FE2565
          1743C8C51F063EBE0993494BFF996434DEBFB97E1C7EB4958FC6C35B3814180E
          3D9C71DE4B7AF5B0D7E4FAA9992E9E1B309CF370E82DBDCB1AA067664BD9464A
          2F933B02C3F53ECD127ACDEF97DE657DF509681AAB481A2170BF6AC07B1B101B
          A1DA7E93888063DE1F604C7F1383C33E3C40A812859BC0111F1850B322B602F6
          7CC53A32A01632A2A8D06F1DFBFE1D1CDB604E95D160CFB33038F21159135B03
          8EF8D06FC4411170D4FB03D8DAE9E717CD94E978D5174549E5CCF940B283939D
          B3A3F3C1E4F862501E9EEE5EC2A1C070E8E14CE75D923FDB6CB6B5ED846EEB07
          E0FC96E3B9F3168D5F3C1F3F11AF0B16D00000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000002B44944415478DA95536D485351187ECFBDFBB4E9DACCA196B3E9B45C
          AEC034FA202CB5267E402041A1D18F2286E49FA218062D09A21F8942840499F4
          23C10821B1C414A18C0CC1E16E6998D24A9BE4D7862E75DBBDF7DCCE590AD10A
          ECBD5CCEE1DCF779DEE77DEF73902449400391A08BBDB1DBB249B3F90C8B6485
          C0B01908B008A2E81130DFE7F37A9A5AEA2ABF923C2CAD01115D29D85C5CAC28
          2B77DE566B626AB2D275AC215E0D097A3550DA395F106617566174623EE09B5F
          B872FF72510BC1F294245295828BCB9DCFD28D7A5BD141238804151225E0C94B
          13E432040A06811816A06F600A3E4FCE76B637D79C9AE1B815FA9DB537BC6A30
          9B0C35B67C134C2FF2441F44054308080F246A15D0F3DA03DCC897FA870E5B2D
          AAB8DABA2B3935DD7DB6229BF5F8C2C0324C24F96F0458C2641C00DBF50A78FC
          94E34787067251E5CDEEA682FD66BB2651072181C861016432268A0001259008
          018E90F1BE4578FED2D5884ED7F50E579459F7CC86A3ABFE2B68912D4A044FDA
          8746D04967EF7C6989357E6659D83801695317C34257E7B01F9DA8ED5ECA3F66
          895D16FE43C1DA40DFF6707E5472A9833B70D462C532E586C01279A8023E1886
          C1BE61372AB8D07ACF9AB7A35AB72D61C30A2889DFEB0757FFE01D642D73E66C
          CDCC7D9777245B2EFEB23499723408210630C6402D82050CDC9B8FC2A7818E5C
          DAB86A5F55737D8AD9549DB5370DC2D47D4802C51FBF5220E7986E88CB26B849
          F08C7CB8EB6ABBE8A0048C569B1A97517AA32D292DE5B825C70412199246298B
          58989A5DE0312C054552598489F793F06DCCF3C2D576BE926097D62F13ABD51A
          E38C858E5BB13AC3B994CC24B92159072A8D32227975390473A4E7A971EF8ACF
          3B7E7DACEBDA0372FC836045F4DB75A69A55A987ECBB63F43BABE4AAD8C312C3
          26916E8883F9EFE160A03F30E56E9E763F1A2379C1F5EBFC13749B22D6ED599B
          F50000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000002D04944415478DA8D536B489351187ECFF7EDA64DD7660EB59C4DA7A5
          B902D3E842685ED2BC54308A6246512143F24F5188414B82E847A21021452A11
          49861899265E92D4C8101C6EA5614A2B6D92B70D3575EEBB75CE4A888CD87B38
          1C38E77D9EF35E9E17098200C4103672982A5AE3D6C9D79FA291280D283A1A01
          CF01C7D9599EE9743AEC9535A5C6AFD88F177E0311390958979525C9CD33DFF2
          93FB17C546296975901F04ABFC80D04E3BDD3035BB0C43A3330BCE99D9CBF72E
          A5D7602C4348BCBF1270569EF97994469599BE57031C46AD700230781307B108
          818442C07958E8EC1D87CF63534D0D554527266DB625F24E9BCABBCA755A7551
          66B21626E6181C1FAC310A13601E085148A0BDDB0EB6C12F65D5C59925C870A5
          765B584494F5B4219EB63B3D405394D7F95F04BCC0E372006C5649E071BD8D19
          EAEF4D44C61BAD95A9BB752679881256581C0E0D2012516B081010020113F05E
          32C63907CD6D960A74B2B463C090ABDF31E541F0E4513BF862C63319B0418AE0
          6943FF203A66EE98C9C9D6074D2EB2D05CFF1AEEBBABFF0B2E909D85C3C75341
          E94F434BD3800B1D2D699D4FCE880B586411B43DEBF289E09021C55BD0B7ED36
          17CABED868DB73204ECF8BA4F0EA45B74F29641C4906C6ED81BECE012B4A2DA8
          BDAB4FDA52A8DC14EC1398988097CBE1024B4FDF6DA4CF35276C8C497C979412
          2FE67E491A57792D08210A789E0722119EE5C1F6E623FBA9B73191345CB62BBF
          AA2C5CA72D8CDD19091EA23E2480E4AF56B2F89E07EF14C0A86D0CEC831FEE58
          EA2E1413024AA188088CCEB95E171A197E302E410B02EEB35C2AF24A98889D65
          78987773F8670E46DF8FC1B761FB4B4BDD7923C6CEAF0E13AD5068023569C537
          0394EA73E131A1627598126472A937E4E5C51598C6398F8F38969C8E916BC32D
          571FE0EB1F18CBA13FC699C42C8BD867DAEEAFDA9A2F9605EC17283A14678315
          CC7CF7B8177A16C6AD5513D687C3D8CFBD3ACE3F01521932D6949C3B97000000
          0049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000002E64944415478DA8D937F48535114C7CF7D6FBF9CD3B5E5ACA95337A6
          D3E52A6CF6C308CA89030D0A54B034FF295224FBA308C4A0EC9FE88FA4204282
          4CFAC3C00821B1C414A18C14C5E95E6998DA6CB6CAA91BCDD2B9B7F75EF7CD26
          59067E2F970BF79DF3B9E79C770EE2380E78212CFE38DF5961946D919D020161
          41244A090E9730C0308E204BF7785C8E86A66BA59FB01DCBFD7644FCC93B675B
          5344FBEBF26FC8A4D2EA0C453AB93D4205DBF09EA200486532B81796616C727E
          D133BF70E9DEC5DC26EC4BF390D0ABBCF3BEBABCA77AA5DE9A9F980B1C62F0D7
          15BC69981A8E00B5C1082202011308424FDF0C7C74BADB5B1BAB4B66296A8907
          90552F2B6E1962F5D5C7B45698A5BF028B5758CEA118884E4A03020304D87ABB
          5C045DAF1C408D4ED73FA8B1D6A2A2E6C21D09FA387B654639E90C4C6343021B
          926B00D7603C446A0C2100CBB1B81C00C94A11343FA1E8B1A13E332AEF2C6DB0
          E8732A558A48A0210002208024046B00F7A00EC4F1A98080077018C08660B4E7
          3B3C7B61BB8D4E74978C9C3415EEF2A139D848BEFEDDC0A9F4EBEE481C608C18
          C1E3D6A15154DC5D345F6C2AD8EA0DBA37042CF5674340A15B0FC0692AA42474
          B48F78D1F1DA4E9FC51C11156011FC4FCB72ED5F80D582BEE9A2BC28FF421B75
          E088D1C40AC4B0197178F111D0FE000CF48CD851CED947774D59862A45826A53
          8030C4EBF282AD77E026321DBD9A199F6AEECF3A9C2164565B1A57F95F278408
          605916F8FE65832C50AFDF073FF4B599F9C4257BCB1AEB357A6D55FA1E1D0418
          BEB5391009D65382F83ED45E2C079394131CA3EFEED85ACED5F000422E4F8A4E
          29A86B51EB3479C64C2D70B84832B10084B8527CB30769167C7E06BFCCC0E45B
          277C1E773CB7B59C29E5FF72789848B93C313AD152733D4A117B5A93AA16C6C6
          2940221387425EFEB9027338E79909D792C7357165BCE3F27D7CFD03FB32E88F
          71E66396241DACDC2955A69509255187388254E36C7007D3DF02FEC5DEC5197B
          E317FBC3716CE70F8FF32FF7E032D6A733DD520000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000002B14944415478DA8D536B48D351143FF7FFFFEFA14DD78CF94A67CAD4
          3457E0A327413971A041810A96E6972245B40F412006E5A7E843521062843282
          0A461065928DC410A3426AB999898F9A69964ED768A6CEFD5F9D3B142AFCB073
          B95CB8F7FE1EE7DC7B882CCB408360D0E5BCBD365BB355731A38C64C58920E12
          88B248DC22CFF779DDBEF68E2AEB57BC27C9EB4042570A3E684957EE6F29B9A6
          898C6CCCD165B1F1117A88C3C9E0980B78606E750186173F2D79BDDE8B6D4577
          AC88E52949489582F7B5143F31C6182D258622908988A76B38F9903B25510047
          54101404B0CFBC84CF9E2FDD3D0D8F2A2786E75728015BDF5F7B2333D6D8783C
          D502F3FC0F742DC1FF413364818338653C3C75BF800F53AED60E8BB59994DF2F
          DB95644C74D6E5D4B0D3C129601806D5D84D094092804772837207DC76DDE347
          5F8FE7931A7B55BBD95858A7D76DC1C3206A30C032DCA604B45EA224008302FE
          5F3C3C7674DF24277B2B874E99CAF6F8C902841B54444BF4607D6F1B2115BDE5
          8B15A6D26D3EC11336014D53CBEAE0E1D0731F39D16CF79BF322A28252D878A0
          E56070F6BF5BF691920B5DAE0347B34D12A70A0B2CE360D1011F08C260DF9093
          149E7BD0662AC8ACD725E9C37640497CB33E700C0C5E27A6635772B767E4BF2D
          3892A310D7AB8D029BD866F01525A0FF57122470BD1A15C6DF74E5D38FA4DE5B
          DDD99A6C4CADCFCA4B83A048BFB60C4AEE5F1601F74365926498744D837BE4E3
          2D87ADA18912305A6D4A747A698B2D212DB9383B371564AC9046C58182C3B7C7
          1B022F813F20A2B20893C3D3F06DCCFDCC613B5B8558FF4633B15AAD21DA606E
          BA1AA58B3D939C91A0884DD4815AA30A595E5D5E8305CC79666276E5E7ECC4E5
          B19E4B1DB8FD1BB122F9AB9DA96775CAA1BADD91313BAB15EAA8C332C3266036
          922CF173C1C0D2C0D28CB3F3BBF3EE18DE0B6CB4F31F81AE22489B4642E30000
          000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000002D84944415478DA8D536B489361143EEFF7EDE69C7E4D99B7743A9B9A
          CB159876252A35172614686069FE895224FB11042251FE8A7E2405614628FB51
          8916449658234942C8044D9D99953A6D56DEB656735EB6EFD6FB8A86C682CEC7
          CBE1BD3CCF73CEF9CE41A2280231848DB8F39612836A83EA1448A84C44A30410
          80177934C6B3EC2BE798ABB6AED0FC05BF13C41520229E80F7981264BBAA72AE
          A994CAF21475321D11A08170BC28FC4D2DCDC0D4E22C0C383ECC399DCE8B3559
          77CD18CB1292655502DE5995DDAC0FD19B72B45920221EDF7AF16297A3932129
          48901C7C1C07968976189DB1B53C3FF7B86078607A8110D065AF4B6E2485E9CB
          8FEA4C30CD4EE2A805F8DB48863448205C1601CFC65E42EFB8B5BACE64AE44F9
          0FF2B644EBA3FA4B538A69BB6F1C288AC26AB45F0210046031B956160777ACF7
          D9A1379FD350B1A5B036539F51AA5107E24B1FD6A080A6247E0948BD7881030A
          0BB87FB1F0E45DCB4D74A2ADA0EFA4316F9B1BCDC2FF1A11619006CC3D4D83E8
          785BBE2337C014BAC4CFFF1BB1C9BB6E4BD26468353CEA7BE142C72A2DEEFD87
          0C41F31CF28B0D91378118BFB8EE8CC6299082B65A3B5D28E7C253EBEE8306A3
          2091FB27086C045EB7F0674FEA4022F0FA3868EFEDEE4719671B6A8CE94965EA
          688D5F82506523706B0856BA0FBEBA5CD0D565BD8E8CB957523726A6BD4D3F90
          22E557AA8D05FE980647C0C57940C48702FE8D22E912EC3B86DE73C3CDB63492
          B86247517D758C5E5796BC3D1E7C3C696D1164126A1D012B127A321822F4D947
          6074C07EABA7BCBB8210500C131B9C70A4AA29323E26DB90AAC36A0854720948
          250818F943F0C57AC0C3CF01CFF3CB60BBCDD1DA73A6B31063DDABC344338C36
          589B597135481D763A2631521A16A506854A0EE14C0338222671CEB3609BF8BE
          F063E4E7E58F9706EB30D883B13C5A33CE246645ECDED2ADCA90CD455245D03E
          91A223A9C3B70591E5A67C6E6FC75CFF62FDB77BB64FF8DDD2EA38FF0617C533
          CC7E0455AA0000000049454E44AE426082}
      end>
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
        ItemName = 'BarButton4'
      end
      item
        Visible = True
        ItemName = 'dxBarButton4'
      end
      item
        Visible = True
        ItemName = 'dxBarButton5'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'dxBarButton6'
      end
      item
        Visible = True
        ItemName = 'dxBarButton7'
      end
      item
        Visible = True
        ItemName = 'dxBarButton8'
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
  object StyleRepository: TcxStyleRepository
    Left = 156
    Top = 296
    PixelsPerInch = 96
    object StyleNormal: TcxStyle
    end
    object StyleComplete: TcxStyle
      AssignedValues = [svTextColor]
      TextColor = 16742656
    end
    object StyleNeedTranslation: TcxStyle
    end
    object StyleDontTranslate: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 15132390
      TextColor = clGray
    end
    object StyleHold: TcxStyle
      AssignedValues = [svColor]
      Color = 15132390
    end
    object StyleSelected: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = clHighlight
      TextColor = clHighlightText
    end
    object StyleFocused: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = clHotLight
      TextColor = clHighlightText
    end
  end
  object ImageListState: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 22282304
    ImageInfo = <
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000001B744558745469746C65005761726E696E673B4E6F7469
          6669636174696F6E3BB6E779860000024149444154785EA58F5F48536118C64F
          C5A02E9288EE2BA8816585ABFC83DA28EBA6CB6A12CC300BA2024D27589A0CB4
          ADA4DC859995D36DD1A4CD5DA428E94636D7E98FD236D79A04DBB42E14664E73
          5B46E982A7F73BD448362FA40F7E9C87F77D7EE77C87F3EA8EFE179C475B9812
          3A6BDC0F8F9410729657ECB91F1C4E8209439A823CCA7162C9DE949F4D3975F7
          DD5D69129D57F76F1A69967E9E1E7D8C90CB00CA7E7D55665AAA2E37AC295806
          9DB5FCED7C63D0AAC6AF6F9F108F05101C5081663AB64BEABF6ECC4BC0AE6EAB
          CF29766AE588935C76A51AE5443CE287B3ED349E37E4C8586799E368C84DD0A5
          906C1FAACF9D0FB98C589A0F407EE682C062D883A9B76DA0DD9CA952B2F55F87
          1BACCB12A83CBE4D64AB3DC87B9F5409C28F49072A2B140C217F9FE886C7580E
          EA0C49D337AFFBEB71D6EA03C2D57B15923A5E23A32F8F233A66C042B01BFA96
          1B30102C477D3A7A491FF8A693A06E0D7304F759452667BEB8279B9E8B730107
          621F3B11713523EA6EC5F4700B43C8C2CCDB8E49BE15AC4B4E1673B93BA776A6
          F55CDE17F49A9588BE6F4778508159470D621F1E41A3563258A6592DC22FAAF0
          F58D1A23F74B418EBFF1C48E8D9CE57C86DE7E538688CF84195B19A6FB4A0562
          3E230E498F31584ECCBF0C5CC20C7F0BD6EB85E83A97D1C1594A768527FA5588
          B8B59877DE436CCC44FF6AC3CFD028A2213F22535E2C8CF70BBBB9572A84EDD7
          30FBB21EC15E2598CBE98AC43DE6E274A4C27276AFC04AFB8E22F1538ECE1642
          4CEC5E25E23F2E2722D6131B56097344BF0168ADF1039DB8E68C000000004945
          4E44AE426082}
      end>
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
end
