inherited FormSettings: TFormSettings
  BorderStyle = bsSizeable
  Caption = 'Settings'
  ClientHeight = 597
  ClientWidth = 595
  KeyPreview = True
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  ExplicitWidth = 611
  ExplicitHeight = 635
  PixelsPerInch = 96
  TextHeight = 13
  inherited LayoutControlButtons: TdxLayoutControl
    Top = 522
    Width = 595
    Height = 75
    TabOrder = 1
    ExplicitTop = 522
    ExplicitWidth = 595
    ExplicitHeight = 75
    inherited ButtonOK: TcxButton
      Left = 411
      Top = 45
      ExplicitLeft = 411
      ExplicitTop = 45
    end
    inherited ButtonCancel: TcxButton
      Left = 492
      Top = 45
      ExplicitLeft = 492
      ExplicitTop = 45
    end
    inherited LayoutGroupButtons: TdxLayoutGroup
      Index = 1
    end
    object LayoutItemRestart: TdxLayoutLabeledItem
      Parent = LayoutGroupRestart
      CaptionOptions.Glyph.SourceDPI = 96
      CaptionOptions.Glyph.Data = {
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
      CaptionOptions.ShowAccelChar = False
      CaptionOptions.Text = 
        'Some of the changes you made will not take effect until the appl' +
        'ication has been restarted.'
      CaptionOptions.WordWrap = True
      Index = 0
    end
    object LayoutGroupRestart: TdxLayoutGroup
      Parent = LayoutControlButtonsGroup_Root
      CaptionOptions.Visible = False
      Visible = False
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      ShowBorder = False
      Index = 0
    end
    object dxLayoutSeparatorItem3: TdxLayoutSeparatorItem
      Parent = LayoutGroupRestart
      CaptionOptions.Text = 'Separator'
      Index = 1
    end
  end
  inherited LayoutControlHeader: TdxLayoutControl
    Width = 595
    TabOrder = 2
    Visible = False
    ExplicitWidth = 595
    inherited LabelHeader: TcxLabel
      ExplicitWidth = 573
      Width = 573
    end
  end
  inherited PanelMain: TPanel
    Width = 595
    Height = 483
    TabOrder = 0
    ExplicitTop = 39
    ExplicitWidth = 595
    ExplicitHeight = 483
    inherited LayoutControl: TdxLayoutControl
      Width = 579
      Height = 471
      ExplicitLeft = 8
      ExplicitWidth = 579
      ExplicitHeight = 471
      object PageControl: TcxPageControl [0]
        Left = 134
        Top = 6
        Width = 439
        Height = 459
        Color = clWhite
        ParentColor = False
        TabOrder = 1
        TabStop = False
        Properties.ActivePage = TabSheetTranslationServices
        Properties.CustomButtons.Buttons = <>
        Properties.MultiLine = True
        Properties.ShowFrame = True
        LookAndFeel.SkinName = ''
        ClientRectBottom = 458
        ClientRectLeft = 1
        ClientRectRight = 438
        ClientRectTop = 24
        object TabSheetGeneral: TcxTabSheet
          AlignWithMargins = True
          Margins.Left = 12
          Margins.Top = 12
          Margins.Right = 12
          Margins.Bottom = 8
          Caption = 'General'
          object LayoutControlGeneral: TdxLayoutControl
            Left = 0
            Top = 0
            Width = 413
            Height = 414
            Align = alClient
            ParentBackground = True
            TabOrder = 0
            Transparent = True
            LayoutLookAndFeel = LayoutSkinLookAndFeel
            OptionsItem.FocusControlOnItemCaptionClick = True
            object LabelEditingHeader: TcxLabel
              Left = 0
              Top = 56
              AutoSize = False
              Caption = 'Editing'
              Style.HotTrack = False
              Style.TextStyle = [fsBold]
              Style.TransparentBorder = False
              Properties.LineOptions.Visible = True
              Transparent = True
              Height = 17
              Width = 413
            end
            object CheckBoxEditUseProposed: TcxCheckBox
              Left = 12
              Top = 79
              Caption = 'Use Proposed status for new translations'
              ParentBackground = False
              ParentColor = False
              Style.Color = 16053234
              Style.TransparentBorder = False
              TabOrder = 3
              Transparent = True
            end
            object CheckBoxAtstart: TcxCheckBox
              Left = 12
              Top = 210
              Caption = 'Display guide dialog when the application starts'
              ParentBackground = False
              ParentColor = False
              Style.Color = 16053234
              Style.TransparentBorder = False
              TabOrder = 8
              Transparent = True
            end
            object cxLabel2: TcxLabel
              Left = 0
              Top = 187
              AutoSize = False
              Caption = 'Dialogs'
              Style.HotTrack = False
              Style.TextStyle = [fsBold]
              Style.TransparentBorder = False
              Properties.LineOptions.Visible = True
              Transparent = True
              Height = 17
              Width = 413
            end
            object ButtonDialogsSuppressReset: TcxButton
              Left = 12
              Top = 235
              Width = 197
              Height = 23
              Caption = 'Reset suppressed dialogs'
              TabOrder = 9
            end
            object LabelLanguage: TcxLabel
              Left = 0
              Top = 272
              AutoSize = False
              Caption = 'Language'
              Style.HotTrack = False
              Style.TextStyle = [fsBold]
              Style.TransparentBorder = False
              Properties.LineOptions.Visible = True
              Transparent = True
              Height = 17
              Width = 413
            end
            object LabelResourceModuleHeader: TcxLabel
              Left = 0
              Top = 0
              AutoSize = False
              Caption = 'Resource modules'
              Style.HotTrack = False
              Style.TextStyle = [fsBold]
              Style.TransparentBorder = False
              Properties.LineOptions.Visible = True
              Transparent = True
              Height = 17
              Width = 413
            end
            object CheckBoxResourceModulesIncludeVersionInfo: TcxCheckBox
              Left = 12
              Top = 23
              Hint = 
                'Include the version info of the source application into the gene' +
                'rated resource modules'
              Caption = 'Include version info'
              ParentBackground = False
              ParentColor = False
              Style.Color = 16053234
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 1
              Transparent = True
            end
            object ComboBoxSourceLanguage: TcxExtLookupComboBox
              Left = 138
              Top = 295
              RepositoryItem = DataModuleMain.EditRepositoryComboBoxItemLanguage
              Properties.ClearKey = 46
              Properties.DropDownAutoSize = True
              Properties.DropDownSizeable = True
              Style.HotTrack = False
              TabOrder = 11
              Width = 145
            end
            object ComboBoxTargetLanguage: TcxExtLookupComboBox
              Left = 138
              Top = 322
              RepositoryItem = DataModuleMain.EditRepositoryComboBoxItemLanguage
              Properties.ClearKey = 46
              Properties.DropDownAutoSize = True
              Properties.DropDownSizeable = True
              Style.HotTrack = False
              TabOrder = 12
              Width = 145
            end
            object ComboBoxApplicationLanguage: TcxExtLookupComboBox
              Left = 138
              Top = 361
              RepositoryItem = DataModuleMain.EditRepositoryComboBoxItemLanguage
              Properties.ClearKey = 46
              Properties.DropDownAutoSize = True
              Properties.DropDownSizeable = True
              Properties.OnEditValueChanged = ComboBoxApplicationLanguagePropertiesEditValueChanged
              Style.HotTrack = False
              TabOrder = 13
              Width = 145
            end
            object CheckBoxEditBiDiMode: TcxCheckBox
              Left = 12
              Top = 154
              Caption = 'BiDi mode follows edited language'
              ParentBackground = False
              ParentColor = False
              Style.Color = 16053234
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 6
              Transparent = True
            end
            object CheckBoxEditAutoApplyTranslations: TcxCheckBox
              Left = 12
              Top = 104
              Hint = 
                'When a new translation is added, apply this translation to ident' +
                'ical values in the project'
              Caption = 'Apply new translations to whole project'
              ParentBackground = False
              ParentColor = False
              Style.Color = 16053234
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 4
              Transparent = True
            end
            object CheckBoxEditAutoApplyTranslationsSimilar: TcxCheckBox
              Left = 28
              Top = 129
              Hint = 'Also apply new translations to values that are similar'
              Caption = 'Apply to similar values'
              ParentBackground = False
              ParentColor = False
              Style.Color = 16053234
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 5
              Transparent = True
            end
            object dxLayoutGroup1: TdxLayoutGroup
              AlignHorz = ahParentManaged
              AlignVert = avParentManaged
              ButtonOptions.Buttons = <>
              Hidden = True
              ItemIndex = 1
              ShowBorder = False
              Index = -1
            end
            object LayoutControlGeneralItem1: TdxLayoutItem
              Parent = LayoutControlGroupEditing
              CaptionOptions.Visible = False
              Control = LabelEditingHeader
              ControlOptions.OriginalHeight = 17
              ControlOptions.OriginalWidth = 442
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object LayoutControlGeneralItem2: TdxLayoutItem
              Parent = LayoutControlGeneralGroup2
              CaptionOptions.Visible = False
              Control = CheckBoxEditUseProposed
              ControlOptions.AutoColor = True
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 121
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object LayoutControlGroupEditing: TdxLayoutGroup
              Parent = dxLayoutGroup1
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              Offsets.Top = 8
              ButtonOptions.Buttons = <>
              Hidden = True
              ItemIndex = 1
              ShowBorder = False
              Index = 1
            end
            object LayoutControlGeneralItem12: TdxLayoutItem
              Parent = LayoutControlGeneralGroup4
              CaptionOptions.Visible = False
              Control = CheckBoxAtstart
              ControlOptions.AutoColor = True
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 121
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object LayoutControlGeneralItem13: TdxLayoutItem
              Parent = LayoutControlGroupStartup
              CaptionOptions.Visible = False
              Control = cxLabel2
              ControlOptions.OriginalHeight = 17
              ControlOptions.OriginalWidth = 442
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object LayoutControlGroupStartup: TdxLayoutGroup
              Parent = dxLayoutGroup1
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              Offsets.Top = 8
              Visible = False
              ButtonOptions.Buttons = <>
              Hidden = True
              ItemIndex = 1
              ShowBorder = False
              Index = 2
            end
            object LayoutControlGeneralGroup2: TdxLayoutGroup
              Parent = LayoutControlGroupEditing
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              Offsets.Left = 12
              ButtonOptions.Buttons = <>
              Hidden = True
              ItemIndex = 2
              ShowBorder = False
              Index = 1
            end
            object LayoutControlGeneralItem11: TdxLayoutItem
              Parent = LayoutControlGeneralGroup4
              CaptionOptions.Text = 'New Item'
              CaptionOptions.Visible = False
              Control = ButtonDialogsSuppressReset
              ControlOptions.AlignHorz = ahLeft
              ControlOptions.OriginalHeight = 23
              ControlOptions.OriginalWidth = 197
              ControlOptions.ShowBorder = False
              Index = 1
            end
            object LayoutControlGeneralGroup4: TdxLayoutGroup
              Parent = LayoutControlGroupStartup
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              Offsets.Left = 12
              ButtonOptions.Buttons = <>
              Hidden = True
              ShowBorder = False
              Index = 1
            end
            object dxLayoutItem29: TdxLayoutItem
              Parent = LayoutGroupLanguage
              CaptionOptions.Visible = False
              Control = LabelLanguage
              ControlOptions.OriginalHeight = 17
              ControlOptions.OriginalWidth = 7
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object LayoutGroupLanguage: TdxLayoutGroup
              Parent = dxLayoutGroup1
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              Offsets.Top = 8
              ButtonOptions.Buttons = <>
              Hidden = True
              ItemIndex = 1
              ShowBorder = False
              Index = 3
            end
            object dxLayoutGroup10: TdxLayoutGroup
              Parent = LayoutGroupLanguage
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              LayoutLookAndFeel = LayoutSkinLookAndFeelGroup
              ButtonOptions.Buttons = <>
              Hidden = True
              ItemIndex = 3
              ShowBorder = False
              Index = 1
            end
            object dxLayoutItem26: TdxLayoutItem
              Parent = LayoutGroupResourceModules
              CaptionOptions.Visible = False
              Control = LabelResourceModuleHeader
              ControlOptions.OriginalHeight = 17
              ControlOptions.OriginalWidth = 7
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object LayoutGroupResourceModules: TdxLayoutGroup
              Parent = dxLayoutGroup1
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              ButtonOptions.Buttons = <>
              Hidden = True
              ShowBorder = False
              Index = 0
            end
            object dxLayoutItem27: TdxLayoutItem
              Parent = dxLayoutGroup9
              CaptionOptions.Visible = False
              Control = CheckBoxResourceModulesIncludeVersionInfo
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 115
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object dxLayoutGroup9: TdxLayoutGroup
              Parent = LayoutGroupResourceModules
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              Offsets.Left = 12
              ButtonOptions.Buttons = <>
              Hidden = True
              ShowBorder = False
              Index = 1
            end
            object dxLayoutItem43: TdxLayoutItem
              Parent = dxLayoutGroup10
              AlignHorz = ahLeft
              CaptionOptions.Text = 'Default source language:'
              Control = ComboBoxSourceLanguage
              ControlOptions.OriginalHeight = 21
              ControlOptions.OriginalWidth = 145
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object dxLayoutSeparatorItem4: TdxLayoutSeparatorItem
              Parent = dxLayoutGroup10
              CaptionOptions.Text = 'Separator'
              Index = 2
            end
            object dxLayoutItem28: TdxLayoutItem
              Parent = dxLayoutGroup10
              AlignHorz = ahLeft
              CaptionOptions.Text = 'Default target language:'
              Control = ComboBoxTargetLanguage
              ControlOptions.OriginalHeight = 21
              ControlOptions.OriginalWidth = 145
              ControlOptions.ShowBorder = False
              Index = 1
            end
            object dxLayoutItem30: TdxLayoutItem
              Parent = dxLayoutGroup10
              AlignHorz = ahLeft
              CaptionOptions.Text = 'Application language:'
              Control = ComboBoxApplicationLanguage
              ControlOptions.OriginalHeight = 21
              ControlOptions.OriginalWidth = 145
              ControlOptions.ShowBorder = False
              Index = 3
            end
            object dxLayoutItem54: TdxLayoutItem
              Parent = LayoutControlGeneralGroup2
              CaptionOptions.Visible = False
              Control = CheckBoxEditBiDiMode
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 183
              ControlOptions.ShowBorder = False
              Index = 3
            end
            object dxLayoutItem55: TdxLayoutItem
              Parent = LayoutControlGeneralGroup2
              CaptionOptions.Visible = False
              Control = CheckBoxEditAutoApplyTranslations
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 206
              ControlOptions.ShowBorder = False
              Index = 1
            end
            object dxLayoutItem56: TdxLayoutItem
              Parent = LayoutControlGeneralGroup2
              CaptionOptions.Visible = False
              Offsets.Left = 16
              Control = CheckBoxEditAutoApplyTranslationsSimilar
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 127
              ControlOptions.ShowBorder = False
              Index = 2
            end
          end
        end
        object TabSheetAppearance: TcxTabSheet
          AlignWithMargins = True
          Margins.Left = 12
          Margins.Top = 12
          Margins.Right = 12
          Margins.Bottom = 8
          Caption = 'Appearance'
          ImageIndex = 5
          object LayoutControlColors: TdxLayoutControl
            Left = 0
            Top = 0
            Width = 413
            Height = 414
            Align = alClient
            ParentBackground = True
            TabOrder = 0
            Transparent = True
            LayoutLookAndFeel = LayoutSkinLookAndFeel
            DesignSize = (
              413
              414)
            object ImageComboBoxSkin: TcxImageComboBox
              Left = 45
              Top = 23
              Properties.DefaultDescription = '(none)'
              Properties.Images = ImageListSkin
              Properties.Items = <>
              Properties.LargeImages = ImageListSkinLarge
              Style.HotTrack = False
              TabOrder = 1
              Width = 164
            end
            object LabelListStyles: TcxLabel
              Left = 0
              Top = 58
              AutoSize = False
              Caption = 'Lists'
              Style.HotTrack = False
              Style.TextStyle = [fsBold]
              Style.TransparentBorder = False
              Properties.LineOptions.Visible = True
              Transparent = True
              Height = 17
              Width = 413
            end
            object GridColors: TcxGrid
              Left = 12
              Top = 99
              Width = 401
              Height = 200
              BorderStyle = cxcbsNone
              TabOrder = 3
              object GridColorsTableView: TcxGridTableView
                Navigator.Buttons.CustomButtons = <>
                OnInitEdit = GridColorsTableViewInitEdit
                DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoImmediatePost]
                DataController.Summary.DefaultGroupSummaryItems = <>
                DataController.Summary.FooterSummaryItems = <>
                DataController.Summary.SummaryGroups = <>
                DataController.OnAfterPost = GridColorsTableViewDataControllerAfterPost
                OptionsCustomize.ColumnFiltering = False
                OptionsCustomize.ColumnGrouping = False
                OptionsCustomize.ColumnMoving = False
                OptionsCustomize.ColumnSorting = False
                OptionsData.Deleting = False
                OptionsData.Inserting = False
                OptionsSelection.InvertSelect = False
                OptionsView.ColumnAutoWidth = True
                OptionsView.GridLines = glNone
                OptionsView.GroupByBox = False
                Styles.Background = StyleBackground
                Styles.Content = StyleBackground
                Styles.UseOddEvenStyles = bFalse
                Styles.Inactive = StyleBackground
                Styles.Selection = StyleBackground
                object GridColorsTableViewColumnName: TcxGridColumn
                  Caption = 'Style'
                  PropertiesClassName = 'TcxLabelProperties'
                  Properties.ShowAccelChar = False
                  Options.Focusing = False
                end
                object GridColorsTableViewColumnSample: TcxGridColumn
                  Caption = 'Sample'
                  PropertiesClassName = 'TcxLabelProperties'
                  Properties.ShowAccelChar = False
                  OnCustomDrawCell = GridColorsTableViewColumnSampleCustomDrawCell
                  BestFitMaxWidth = 60
                  Options.Focusing = False
                  Width = 100
                end
                object GridColorsTableViewColumnText: TcxGridColumn
                  Caption = 'Text'
                  DataBinding.ValueType = 'Integer'
                  PropertiesClassName = 'TdxColorEditProperties'
                  Properties.ImmediateDropDownWhenActivated = True
                  Properties.ImmediatePost = True
                  Properties.OnInitPopup = GridColorsTableViewColumnColorPropertiesInitPopup
                  OnCustomDrawCell = GridColorsTableViewColumnTextCustomDrawCell
                  Options.AutoWidthSizable = False
                end
                object GridColorsTableViewColumnBackground: TcxGridColumn
                  Caption = 'Background'
                  DataBinding.ValueType = 'Integer'
                  PropertiesClassName = 'TdxColorEditProperties'
                  Properties.ImmediateDropDownWhenActivated = True
                  Properties.ImmediatePost = True
                  Properties.OnInitPopup = GridColorsTableViewColumnColorPropertiesInitPopup
                  OnCustomDrawCell = GridColorsTableViewColumnBackgroundCustomDrawCell
                  Options.AutoWidthSizable = False
                end
                object GridColorsTableViewColumnBold: TcxGridColumn
                  Caption = 'Bold'
                  PropertiesClassName = 'TcxCheckBoxProperties'
                  Properties.AllowGrayed = True
                  Properties.ImmediatePost = True
                  Properties.NullStyle = nssInactive
                  Options.AutoWidthSizable = False
                  Width = 32
                end
              end
              object GridColorsLevel: TcxGridLevel
                GridView = GridColorsTableView
              end
            end
            object CheckBoxDisplayStatusGlyphs: TcxCheckBox
              Left = 12
              Top = 346
              Action = ActionEditStatusGlyphs
              ParentBackground = False
              ParentColor = False
              Style.Color = 16053234
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 5
              Transparent = True
            end
            object ButtonStyleReset: TcxButton
              Left = 338
              Top = 305
              Width = 75
              Height = 23
              Hint = 'Reset all styles to their default value'
              Anchors = [akTop, akRight]
              Caption = '&Reset'
              TabOrder = 4
              OnClick = ButtonStyleResetClick
            end
            object LabelApperance: TcxLabel
              Left = 0
              Top = 0
              AutoSize = False
              Caption = 'User Interface'
              Style.HotTrack = False
              Style.TextStyle = [fsBold]
              Style.TransparentBorder = False
              Properties.LineOptions.Visible = True
              Transparent = True
              Height = 17
              Width = 413
            end
            object CheckBoxStatusGlyphHint: TcxCheckBox
              Left = 28
              Top = 371
              Action = ActionEditStatusHint
              ParentBackground = False
              ParentColor = False
              Style.Color = 16053234
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 6
              Transparent = True
            end
            object LayoutControlColorsGroup_Root: TdxLayoutGroup
              AlignHorz = ahClient
              AlignVert = avTop
              ButtonOptions.Buttons = <>
              Hidden = True
              ItemIndex = 1
              ShowBorder = False
              Index = -1
            end
            object dxLayoutItem48: TdxLayoutItem
              Parent = dxLayoutGroup18
              CaptionOptions.Visible = False
              Control = LabelListStyles
              ControlOptions.OriginalHeight = 17
              ControlOptions.OriginalWidth = 413
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object dxLayoutGroup18: TdxLayoutGroup
              Parent = LayoutControlColorsGroup_Root
              CaptionOptions.Visible = False
              Offsets.Top = 8
              ButtonOptions.Buttons = <>
              ItemIndex = 1
              ShowBorder = False
              Index = 1
            end
            object dxLayoutGroup19: TdxLayoutGroup
              Parent = dxLayoutGroup18
              CaptionOptions.Visible = False
              LayoutLookAndFeel = LayoutSkinLookAndFeelGroup
              ButtonOptions.Buttons = <>
              ItemIndex = 4
              ShowBorder = False
              Index = 1
            end
            object dxLayoutItem49: TdxLayoutItem
              Parent = dxLayoutGroup19
              CaptionOptions.Text = 'Colors:'
              CaptionOptions.Layout = clTop
              Control = GridColors
              ControlOptions.OriginalHeight = 200
              ControlOptions.OriginalWidth = 250
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object dxLayoutItem50: TdxLayoutItem
              Parent = dxLayoutGroup19
              CaptionOptions.Visible = False
              Control = CheckBoxDisplayStatusGlyphs
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 115
              ControlOptions.ShowBorder = False
              Index = 3
            end
            object dxLayoutSeparatorItem5: TdxLayoutSeparatorItem
              Parent = dxLayoutGroup19
              CaptionOptions.Text = 'Separator'
              Index = 2
            end
            object dxLayoutItem51: TdxLayoutItem
              Parent = dxLayoutGroup19
              AlignHorz = ahRight
              CaptionOptions.Visible = False
              Control = ButtonStyleReset
              ControlOptions.OriginalHeight = 23
              ControlOptions.OriginalWidth = 75
              ControlOptions.ShowBorder = False
              Index = 1
            end
            object dxLayoutItem52: TdxLayoutItem
              Parent = LayoutGroupUserInterface
              CaptionOptions.Visible = False
              Control = LabelApperance
              ControlOptions.OriginalHeight = 17
              ControlOptions.OriginalWidth = 7
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object LayoutGroupUserInterface: TdxLayoutGroup
              Parent = LayoutControlColorsGroup_Root
              CaptionOptions.Visible = False
              ButtonOptions.Buttons = <>
              ShowBorder = False
              Index = 0
            end
            object LayoutGroup3: TdxLayoutGroup
              Parent = LayoutGroupUserInterface
              CaptionOptions.Visible = False
              LayoutLookAndFeel = LayoutSkinLookAndFeelGroup
              ButtonOptions.Buttons = <>
              ShowBorder = False
              Index = 1
            end
            object LayoutControlGeneralItem15: TdxLayoutItem
              Parent = LayoutGroup3
              CaptionOptions.Text = 'Style:'
              Control = ImageComboBoxSkin
              ControlOptions.AlignHorz = ahLeft
              ControlOptions.OriginalHeight = 21
              ControlOptions.OriginalWidth = 164
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object dxLayoutItem53: TdxLayoutItem
              Parent = dxLayoutGroup19
              CaptionOptions.Visible = False
              Offsets.Left = 16
              Control = CheckBoxStatusGlyphHint
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 137
              ControlOptions.ShowBorder = False
              Index = 4
            end
          end
          object cxLabel15: TcxLabel
            Left = 0
            Top = 44
            AutoSize = False
            Caption = ' '
            Style.HotTrack = False
            Height = 17
            Width = 7
          end
        end
        object TabSheetTranslationServices: TcxTabSheet
          AlignWithMargins = True
          Margins.Left = 12
          Margins.Top = 12
          Margins.Right = 12
          Margins.Bottom = 8
          Caption = 'Translators'
          ImageIndex = 5
          object LayoutControlTranslators: TdxLayoutControl
            Left = 0
            Top = 0
            Width = 413
            Height = 414
            Align = alClient
            ParentBackground = True
            TabOrder = 0
            Transparent = True
            LayoutLookAndFeel = LayoutSkinLookAndFeel
            OptionsItem.FocusControlOnItemCaptionClick = True
            object LabelTranslatorTM: TcxLabel
              Left = 0
              Top = 0
              AutoSize = False
              Caption = 'Translation Memory'
              Style.HotTrack = False
              Style.TextStyle = [fsBold]
              Style.TransparentBorder = False
              Properties.LineOptions.Visible = True
              Transparent = True
              Height = 17
              Width = 413
            end
            object CheckBoxTMLoadOnDemand: TcxCheckBox
              Left = 12
              Top = 23
              Hint = 
                'Silently load Translation Memory from disk the first time it is ' +
                'needed.'
              Caption = 'Load on demand'
              ParentBackground = False
              ParentColor = False
              Style.Color = 16053234
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 1
              Transparent = True
            end
            object LabelTransalatorMS: TcxLabel
              Left = 0
              Top = 164
              AutoSize = False
              Caption = 'Microsoft Translation Service'
              Style.HotTrack = False
              Style.TextStyle = [fsBold]
              Style.TransparentBorder = False
              Properties.LineOptions.Visible = True
              Transparent = True
              Height = 17
              Width = 413
            end
            object EditTranslatorMSAPIKey: TcxButtonEdit
              Left = 76
              Top = 187
              Properties.Buttons = <
                item
                  ImageIndex = 0
                  Hint = 'Verify API key'
                  Kind = bkGlyph
                end>
              Properties.Images = ImageList
              Properties.IncrementalSearch = False
              Properties.OnButtonClick = TextEditTranslatorMSAPIKeyPropertiesButtonClick
              Properties.OnChange = TextEditTranslatorMSAPIKeyPropertiesChange
              Style.HotTrack = False
              TabOrder = 7
              Width = 337
            end
            object CheckBoxTMBackgroundQuery: TcxCheckBox
              Left = 12
              Top = 73
              Hint = 
                'Search the Translation Memory for matching translations while yo' +
                'u work and indicate if matches are found.'
              Caption = 'Query Translation Memory in the background'
              ParentBackground = False
              ParentColor = False
              Style.Color = 16053234
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 3
              Transparent = True
            end
            object CheckBoxTMPromptToSave: TcxCheckBox
              Left = 12
              Top = 48
              Hint = 'Ask before the Translation Memory is saved'
              Caption = 'Prompt to save'
              ParentBackground = False
              ParentColor = False
              Style.Color = 16053234
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 2
              Transparent = True
            end
            object SpinEditTranslatorTerminologyMaxResult: TcxSpinEdit
              Left = 76
              Top = 129
              Hint = 'Maximum number of translations to return per term'
              Properties.Alignment.Horz = taRightJustify
              Properties.MaxValue = 20.000000000000000000
              Properties.MinValue = 1.000000000000000000
              Properties.UseLeftAlignmentOnEditing = False
              Properties.ValidationOptions = [evoShowErrorIcon]
              Style.HotTrack = False
              TabOrder = 5
              Value = 1
              Width = 77
            end
            object cxLabel1: TcxLabel
              Left = 0
              Top = 106
              AutoSize = False
              Caption = 'Microsoft Terminology Service'
              Style.HotTrack = False
              Style.TextStyle = [fsBold]
              Style.TransparentBorder = False
              Properties.LineOptions.Visible = True
              Transparent = True
              Height = 17
              Width = 413
            end
            object LayoutControlTranslatorsGroup_Root: TdxLayoutGroup
              AlignHorz = ahClient
              AlignVert = avTop
              ButtonOptions.Buttons = <>
              Hidden = True
              ItemIndex = 1
              ShowBorder = False
              Index = -1
            end
            object dxLayoutItem16: TdxLayoutItem
              Parent = LayoutGroupTranslatorTM
              CaptionOptions.Visible = False
              LayoutLookAndFeel = LayoutSkinLookAndFeelTitle
              Control = LabelTranslatorTM
              ControlOptions.OriginalHeight = 17
              ControlOptions.OriginalWidth = 416
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object LayoutGroupTranslatorTM: TdxLayoutGroup
              Parent = LayoutControlTranslatorsGroup_Root
              CaptionOptions.Visible = False
              ButtonOptions.Buttons = <>
              ItemIndex = 1
              ShowBorder = False
              Index = 0
            end
            object dxLayoutItem23: TdxLayoutItem
              Parent = dxLayoutGroup2
              CaptionOptions.Visible = False
              Control = CheckBoxTMLoadOnDemand
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 100
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object dxLayoutGroup2: TdxLayoutGroup
              Parent = LayoutGroupTranslatorTM
              CaptionOptions.Visible = False
              LayoutLookAndFeel = LayoutSkinLookAndFeelGroup
              ButtonOptions.Buttons = <>
              ShowBorder = False
              Index = 1
            end
            object dxLayoutItem24: TdxLayoutItem
              Parent = LayoutGroupTranslatorMS
              CaptionOptions.Visible = False
              Control = LabelTransalatorMS
              ControlOptions.OriginalHeight = 17
              ControlOptions.OriginalWidth = 7
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object LayoutGroupTranslatorMS: TdxLayoutGroup
              Parent = LayoutControlTranslatorsGroup_Root
              CaptionOptions.Visible = False
              Offsets.Top = 8
              ButtonOptions.Buttons = <>
              ItemIndex = 1
              ShowBorder = False
              Index = 2
            end
            object dxLayoutItem25: TdxLayoutItem
              Parent = dxLayoutGroup8
              CaptionOptions.Text = 'API key:'
              Control = EditTranslatorMSAPIKey
              ControlOptions.OriginalHeight = 21
              ControlOptions.OriginalWidth = 121
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object dxLayoutGroup8: TdxLayoutGroup
              Parent = LayoutGroupTranslatorMS
              CaptionOptions.Visible = False
              LayoutLookAndFeel = LayoutSkinLookAndFeelGroup
              ButtonOptions.Buttons = <>
              ShowBorder = False
              Index = 1
            end
            object dxLayoutItem38: TdxLayoutItem
              Parent = dxLayoutGroup2
              CaptionOptions.Visible = False
              Control = CheckBoxTMBackgroundQuery
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 100
              ControlOptions.ShowBorder = False
              Index = 2
            end
            object dxLayoutItem44: TdxLayoutItem
              Parent = dxLayoutGroup2
              CaptionOptions.Visible = False
              Control = CheckBoxTMPromptToSave
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 94
              ControlOptions.ShowBorder = False
              Index = 1
            end
            object dxLayoutItem46: TdxLayoutItem
              Parent = dxLayoutGroup17
              AlignHorz = ahLeft
              CaptionOptions.Text = 'Max results:'
              Control = SpinEditTranslatorTerminologyMaxResult
              ControlOptions.OriginalHeight = 21
              ControlOptions.OriginalWidth = 77
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object dxLayoutItem47: TdxLayoutItem
              Parent = LayoutGroupTranslatorMSTerminology
              CaptionOptions.Visible = False
              Control = cxLabel1
              ControlOptions.OriginalHeight = 17
              ControlOptions.OriginalWidth = 7
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object LayoutGroupTranslatorMSTerminology: TdxLayoutGroup
              Parent = LayoutControlTranslatorsGroup_Root
              CaptionOptions.Visible = False
              Offsets.Top = 8
              ButtonOptions.Buttons = <>
              ItemIndex = 1
              ShowBorder = False
              Index = 1
            end
            object dxLayoutGroup17: TdxLayoutGroup
              Parent = LayoutGroupTranslatorMSTerminology
              CaptionOptions.Visible = False
              LayoutLookAndFeel = LayoutSkinLookAndFeelGroup
              ButtonOptions.Buttons = <>
              ShowBorder = False
              Index = 1
            end
          end
        end
        object TabSheetFileLocations: TcxTabSheet
          AlignWithMargins = True
          Margins.Left = 12
          Margins.Top = 12
          Margins.Right = 12
          Margins.Bottom = 12
          Caption = 'Files'
          object LayoutControlFiles: TdxLayoutControl
            Left = 0
            Top = 0
            Width = 413
            Height = 410
            Align = alClient
            ParentBackground = True
            TabOrder = 0
            Transparent = True
            LayoutLookAndFeel = LayoutSkinLookAndFeel
            DesignSize = (
              413
              410)
            object cxLabel5: TcxLabel
              Left = 0
              Top = 0
              AutoSize = False
              Caption = 'File locations'
              Style.HotTrack = False
              Style.TextStyle = [fsBold]
              Style.TransparentBorder = False
              Properties.LineOptions.Visible = True
              Transparent = True
              Height = 17
              Width = 413
            end
            object GridFolders: TcxGrid
              Left = 12
              Top = 23
              Width = 401
              Height = 137
              TabOrder = 1
              object GridFoldersTableView: TcxGridTableView
                Navigator.Buttons.CustomButtons = <>
                OnCellDblClick = GridFoldersTableViewCellDblClick
                OnEditing = GridFoldersTableViewEditing
                DataController.Summary.DefaultGroupSummaryItems = <>
                DataController.Summary.FooterSummaryItems = <>
                DataController.Summary.SummaryGroups = <>
                OptionsBehavior.CellHints = True
                OptionsCustomize.ColumnFiltering = False
                OptionsCustomize.ColumnGrouping = False
                OptionsCustomize.ColumnMoving = False
                OptionsCustomize.ColumnSorting = False
                OptionsData.Deleting = False
                OptionsData.Inserting = False
                OptionsSelection.InvertSelect = False
                OptionsView.ColumnAutoWidth = True
                OptionsView.GroupByBox = False
                Styles.UseOddEvenStyles = bFalse
                Styles.OnGetContentStyle = GridFoldersTableViewStylesGetContentStyle
                object GridFoldersTableViewColumnName: TcxGridColumn
                  Caption = 'File type'
                  PropertiesClassName = 'TcxLabelProperties'
                  Properties.ShowAccelChar = False
                  Properties.ShowEndEllipsis = True
                  MinWidth = 50
                  Options.Editing = False
                  Options.Focusing = False
                  Width = 120
                end
                object GridFoldersTableViewColumnPath: TcxGridColumn
                  Caption = 'Path'
                  PropertiesClassName = 'TcxButtonEditProperties'
                  Properties.Buttons = <
                    item
                      Action = ActionFoldersModify
                      Default = True
                      Kind = bkEllipsis
                    end
                    item
                      Action = ActionFoldersExplorer
                      Glyph.SourceDPI = 96
                      Glyph.Data = {
                        424D360400000000000036000000280000001000000010000000010020000000
                        000000000000C40E0000C40E000000000000000000004298D2EF3D94D0FF3D94
                        D0FF4197D1E70000000000000000000000000000000000000000000000000000
                        000000000000000000000000000000000000000000003D94D0FFDCFDFFFFDAFB
                        FFFF358FCDFF3A92CFFF3E95D0FF4398D2AF0000000000000000000000000000
                        000000000000000000000000000000000000000000003B92CFFFD2F9FFFFB6F0
                        FFFFDCFDFFFFDFFFFFFFE5FFFFFF3E95D0FF0000000000000000000000000000
                        000000000000000000000000000000000000000000003D93CFFFC1F7FFFF51B5
                        E5FF2F89CAFF2F8ACBFF328CCCFF3992CFFF3F97D3FF000000004097D3AC3D94
                        D0FF3D94D0FF4197D1A80000000000000000000000004095D0FFA0E5FBFF48A7
                        DDFFDDFAFFFFDAF9FFFFDBFAFFFFDDFCFFFF328AC8CE000000003692D2FFDCFD
                        FFFFDAFBFFFF348DCDFF3991CEFF3D94D0FF4297D2B04397D1FF54A6D9FF82E3
                        FDFF86EBFFFF83EAFFFF85ECFFFF51B3E4FF8D8C8CFF8D8C8CFF3190D2FFCEF7
                        FFFFB5EEFEFFDBF9FFFFDDFBFFFFE2FFFFFF3D94D0FF4190C7D14397D1FF4094
                        D0FF3C92D0FF3991D0FF3B92D1FF3991CFEC00000033000000333490D1FFB7F3
                        FFFF77E0FDFF77E0FDFF76E0FEFFDAFEFFFF3B92CFFF00000027000000330000
                        0033000000338D8C8CFF000000330000002E00000000000000003C93D1FFA9F9
                        FFFF8FF0FFFF8FEFFFFF8DF0FFFFD2FFFFFF3E94D0FF00000000000000000000
                        0000000000008D8C8CFF000000000000000000000000000000003A87BCB23E93
                        CFFF3E91CEFF3E92CEFF3E92CFFF3E94D0FF3D8BC1BE00000000000000000000
                        0000000000008D8C8CFF000000000000000000000000000000003D93CFE23C93
                        D0FF3B92CFFF3D8EC7D600000033000000330000002300000000000000000000
                        0000000000008D8C8CFF000000000000000000000000000000003492D3FFDCFD
                        FFFFDAFBFFFF348DCDFF3991CEFF3D94D0FF4297D2AE00000000000000000000
                        0000000000008D8C8CFF8D8C8CFF8D8C8CFF8D8C8CFF8D8C8CFF308FD2FFCEF7
                        FFFFB5EEFEFFDBF9FFFFDDFBFFFFE2FFFFFF3D94D0FF00000000000000000000
                        00000000000000000033000000330000003300000033000000333390D1FFB7F3
                        FFFF77E0FDFF77E0FDFF76E0FEFFDAFEFFFF3B92CFFF00000000000000000000
                        00000000000000000000000000000000000000000000000000003B93D1FFA9F9
                        FFFF8FF0FFFF8FEFFFFF8DF0FFFFD2FFFFFF3E94D0FF00000000000000000000
                        00000000000000000000000000000000000000000000000000003D8BC1C04094
                        D0FF3F92CFFF3F92CEFF3E92CFFF3E94D0FF3D8BC1C000000000000000000000
                        0000000000000000000000000000000000000000000000000000000000230000
                        00330000003300000033000000330000003300000023}
                      Kind = bkGlyph
                    end>
                  Properties.OnEditValueChanged = GridFoldersTableViewColumnPathPropertiesEditValueChanged
                  OnGetCellHint = GridFoldersTableViewColumnPathGetCellHint
                  OnGetDisplayText = GridFoldersTableViewColumnPathGetDisplayText
                  MinWidth = 100
                  Width = 297
                end
                object GridFoldersTableViewColumnReadOnly: TcxGridColumn
                  DataBinding.ValueType = 'Boolean'
                  PropertiesClassName = 'TcxCheckBoxProperties'
                  Visible = False
                end
              end
              object GridFoldersLevel: TcxGridLevel
                GridView = GridFoldersTableView
              end
            end
            object ButtonFilesReset: TcxButton
              Left = 338
              Top = 166
              Width = 75
              Height = 23
              Action = ActionFolderReset
              Anchors = [akTop, akRight]
              DropDownMenu = PopupMenuFolderReset
              Kind = cxbkDropDownButton
              TabOrder = 3
            end
            object ButtonFilesModify: TcxButton
              Left = 245
              Top = 166
              Width = 75
              Height = 23
              Action = ActionFoldersModify
              Anchors = [akTop, akRight]
              TabOrder = 2
            end
            object cxLabel6: TcxLabel
              Left = 0
              Top = 203
              AutoSize = False
              Caption = 'Document file recovery'
              Style.HotTrack = False
              Style.TextStyle = [fsBold]
              Style.TransparentBorder = False
              Properties.LineOptions.Visible = True
              Transparent = True
              Height = 17
              Width = 413
            end
            object CheckBoxAutoRecovery: TcxCheckBox
              Left = 12
              Top = 226
              Caption = 'Save auto-recovery information'
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 5
              Transparent = True
            end
            object EditAutoRecoveryInterval: TcxSpinEdit
              Left = 152
              Top = 251
              Properties.Alignment.Horz = taRightJustify
              Properties.MaxValue = 60.000000000000000000
              Properties.MinValue = 1.000000000000000000
              Properties.UseLeftAlignmentOnEditing = False
              Properties.ValidationOptions = [evoShowErrorIcon]
              Style.HotTrack = False
              TabOrder = 6
              Value = 1
              Width = 53
            end
            object cxLabel10: TcxLabel
              Left = 0
              Top = 286
              Anchors = [akLeft, akTop, akRight]
              AutoSize = False
              Caption = 'Backup files'
              Style.HotTrack = False
              Style.TextStyle = [fsBold]
              Style.TransparentBorder = False
              Properties.LineOptions.Visible = True
              Transparent = True
              Height = 17
              Width = 413
            end
            object CheckBoxHistoryBackup: TcxCheckBox
              Left = 12
              Top = 334
              Caption = 'Save extra backup files'
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 9
              Transparent = True
            end
            object EditHistoryBackupMaxFiles: TcxSpinEdit
              Left = 152
              Top = 359
              Properties.Alignment.Horz = taRightJustify
              Properties.MaxValue = 100.000000000000000000
              Properties.MinValue = 1.000000000000000000
              Properties.UseLeftAlignmentOnEditing = False
              Properties.ValidationOptions = [evoShowErrorIcon]
              Style.HotTrack = False
              TabOrder = 10
              Value = 1
              Width = 53
            end
            object EditHistoryBackupMaxSize: TcxSpinEdit
              Left = 319
              Top = 359
              Properties.Alignment.Horz = taRightJustify
              Properties.DisplayFormat = '0,0 Mb'
              Properties.EditFormat = '0,0'
              Properties.MaxValue = 10000.000000000000000000
              Properties.MinValue = 1.000000000000000000
              Properties.UseLeftAlignmentOnEditing = False
              Properties.ValidationOptions = [evoShowErrorIcon]
              Style.HotTrack = False
              TabOrder = 11
              Value = 100
              Width = 78
            end
            object CheckBoxSaveBackup: TcxCheckBox
              Left = 12
              Top = 309
              Hint = 
                'Save a backup of existing files when they are replaced with newe' +
                'r files'
              Caption = 'Create backup of saved files'
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 8
              Transparent = True
            end
            object LayoutControlFilesGroup_Root: TdxLayoutGroup
              AlignHorz = ahClient
              AlignVert = avTop
              ButtonOptions.Buttons = <>
              Hidden = True
              ShowBorder = False
              Index = -1
            end
            object dxLayoutItem31: TdxLayoutItem
              Parent = dxLayoutGroup11
              CaptionOptions.Visible = False
              Control = cxLabel5
              ControlOptions.OriginalHeight = 17
              ControlOptions.OriginalWidth = 416
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object dxLayoutItem32: TdxLayoutItem
              Parent = dxLayoutGroup12
              Control = GridFolders
              ControlOptions.OriginalHeight = 137
              ControlOptions.OriginalWidth = 250
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object dxLayoutItem33: TdxLayoutItem
              Parent = dxLayoutGroup13
              AlignHorz = ahRight
              CaptionOptions.Visible = False
              Control = ButtonFilesReset
              ControlOptions.OriginalHeight = 23
              ControlOptions.OriginalWidth = 75
              ControlOptions.ShowBorder = False
              Index = 1
            end
            object dxLayoutItem34: TdxLayoutItem
              Parent = dxLayoutGroup13
              AlignHorz = ahRight
              CaptionOptions.Visible = False
              Control = ButtonFilesModify
              ControlOptions.OriginalHeight = 23
              ControlOptions.OriginalWidth = 75
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object dxLayoutGroup11: TdxLayoutGroup
              Parent = LayoutControlFilesGroup_Root
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              ButtonOptions.Buttons = <>
              Hidden = True
              ItemIndex = 3
              ShowBorder = False
              Index = 0
            end
            object dxLayoutGroup12: TdxLayoutGroup
              Parent = dxLayoutGroup11
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              LayoutLookAndFeel = LayoutSkinLookAndFeelGroup
              ButtonOptions.Buttons = <>
              Hidden = True
              ItemIndex = 1
              ShowBorder = False
              Index = 1
            end
            object dxLayoutGroup13: TdxLayoutGroup
              Parent = dxLayoutGroup12
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              ButtonOptions.Buttons = <>
              Hidden = True
              LayoutDirection = ldHorizontal
              ShowBorder = False
              Index = 1
            end
            object dxLayoutItem35: TdxLayoutItem
              Parent = LayoutGroupRecovery
              CaptionOptions.Visible = False
              Control = cxLabel6
              ControlOptions.OriginalHeight = 17
              ControlOptions.OriginalWidth = 7
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object dxLayoutItem36: TdxLayoutItem
              Parent = dxLayoutGroup14
              CaptionOptions.Visible = False
              Control = CheckBoxAutoRecovery
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 174
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object dxLayoutItem37: TdxLayoutItem
              Parent = dxLayoutGroup14
              AlignHorz = ahLeft
              CaptionOptions.Text = 'Interval (minutes):'
              Offsets.Left = 20
              Control = EditAutoRecoveryInterval
              ControlOptions.OriginalHeight = 21
              ControlOptions.OriginalWidth = 53
              ControlOptions.ShowBorder = False
              Index = 1
            end
            object LayoutGroupRecovery: TdxLayoutGroup
              Parent = dxLayoutGroup11
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              Offsets.Top = 8
              Visible = False
              ButtonOptions.Buttons = <>
              Hidden = True
              ItemIndex = 1
              ShowBorder = False
              Index = 2
            end
            object dxLayoutGroup14: TdxLayoutGroup
              Parent = LayoutGroupRecovery
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              Offsets.Left = 12
              ButtonOptions.Buttons = <>
              Hidden = True
              ItemIndex = 1
              ShowBorder = False
              Index = 1
            end
            object dxLayoutItem39: TdxLayoutItem
              Parent = LayoutGroupBackup
              CaptionOptions.Visible = False
              Control = cxLabel10
              ControlOptions.OriginalHeight = 17
              ControlOptions.OriginalWidth = 7
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object dxLayoutItem40: TdxLayoutItem
              Parent = dxLayoutGroup15
              CaptionOptions.Visible = False
              Visible = False
              Control = CheckBoxHistoryBackup
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 133
              ControlOptions.ShowBorder = False
              Index = 1
            end
            object LayoutGroupBackup: TdxLayoutGroup
              Parent = dxLayoutGroup11
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              Offsets.Top = 8
              ButtonOptions.Buttons = <>
              Hidden = True
              ItemIndex = 1
              ShowBorder = False
              Index = 3
            end
            object dxLayoutGroup15: TdxLayoutGroup
              Parent = LayoutGroupBackup
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              Offsets.Left = 12
              ButtonOptions.Buttons = <>
              Hidden = True
              ItemIndex = 2
              ShowBorder = False
              Index = 1
            end
            object dxLayoutItem41: TdxLayoutItem
              Parent = dxLayoutGroup16
              CaptionOptions.Text = 'Number of files to save:'
              Control = EditHistoryBackupMaxFiles
              ControlOptions.OriginalHeight = 21
              ControlOptions.OriginalWidth = 53
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object dxLayoutItem42: TdxLayoutItem
              Parent = dxLayoutGroup16
              CaptionOptions.Text = 'Max total file size:'
              Control = EditHistoryBackupMaxSize
              ControlOptions.OriginalHeight = 21
              ControlOptions.OriginalWidth = 78
              ControlOptions.ShowBorder = False
              Index = 2
            end
            object dxLayoutGroup16: TdxLayoutGroup
              Parent = dxLayoutGroup15
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              Offsets.Left = 20
              Visible = False
              ButtonOptions.Buttons = <>
              Hidden = True
              LayoutDirection = ldHorizontal
              ShowBorder = False
              Index = 2
            end
            object dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem
              Parent = dxLayoutGroup16
              CaptionOptions.Text = 'Empty Space Item'
              SizeOptions.Height = 10
              SizeOptions.Width = 10
              Index = 1
            end
            object dxLayoutItem45: TdxLayoutItem
              Parent = dxLayoutGroup15
              CaptionOptions.Visible = False
              Control = CheckBoxSaveBackup
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 158
              ControlOptions.ShowBorder = False
              Index = 0
            end
          end
        end
        object TabSheetProofing: TcxTabSheet
          AlignWithMargins = True
          Margins.Left = 12
          Margins.Top = 12
          Margins.Right = 12
          Margins.Bottom = 8
          Caption = 'Proofing'
          ImageIndex = 6
          object LayoutControlProofing: TdxLayoutControl
            Left = 0
            Top = 0
            Width = 413
            Height = 414
            Align = alClient
            ParentBackground = True
            TabOrder = 0
            Transparent = True
            LayoutLookAndFeel = LayoutSkinLookAndFeel
            OptionsItem.FocusControlOnItemCaptionClick = True
            DesignSize = (
              413
              414)
            object CheckBoxProofingIgnoreNumbers: TcxCheckBox
              Left = 12
              Top = 123
              Caption = 'Ignore words with numbers'
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 5
              Transparent = True
            end
            object cxLabel12: TcxLabel
              Left = 0
              Top = 0
              AutoSize = False
              Caption = 'Spell check'
              Style.HotTrack = False
              Style.TextStyle = [fsBold]
              Style.TransparentBorder = False
              Properties.LineOptions.Visible = True
              Transparent = True
              Height = 17
              Width = 396
            end
            object CheckBoxProofingIgnoreRepeatWords: TcxCheckBox
              Left = 12
              Top = 98
              Caption = 'Ignore repeated words'
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 4
              Transparent = True
            end
            object ComboBoxProofingLanguages: TcxCheckComboBox
              Left = 179
              Top = 160
              Properties.Items = <>
              Style.HotTrack = False
              TabOrder = 6
              Width = 217
            end
            object cxLabel13: TcxLabel
              Left = 0
              Top = 216
              AutoSize = False
              Caption = 'AutoCorrect'
              Style.HotTrack = False
              Style.TextStyle = [fsBold]
              Style.TransparentBorder = False
              Properties.LineOptions.Visible = True
              Transparent = True
              Height = 17
              Width = 396
            end
            object ButtonProofingEditCustomDictionary: TcxButton
              Left = 250
              Top = 187
              Width = 146
              Height = 23
              Anchors = [akTop, akRight]
              Caption = 'Edit user dictionary...'
              TabOrder = 7
              OnClick = ButtonProofingEditCustomDictionaryClick
            end
            object CheckBoxProofingSpellCheck: TcxCheckBox
              Left = 12
              Top = 23
              Caption = 'Check spelling as you type'
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 1
              Transparent = True
            end
            object CheckBoxProofingIgnoreUppercase: TcxCheckBox
              Left = 12
              Top = 48
              Caption = 'Ignore words in UPPERCASE'
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 2
              Transparent = True
            end
            object CheckBoxProofingIgnoreMixedCase: TcxCheckBox
              Left = 12
              Top = 73
              Caption = 'Ignore words in MIXed caSE'
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 3
              Transparent = True
            end
            object CheckBoxProofingCorrectSentenceCaps: TcxCheckBox
              Left = 28
              Top = 264
              Caption = 'Capitalize first letter of &sentences'
              Enabled = False
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 10
              Transparent = True
            end
            object CheckBoxProofingCorrectCapsLock: TcxCheckBox
              Left = 28
              Top = 314
              Caption = 'Correct accidental usage of cAPS &LOCK key'
              Enabled = False
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 13
              Transparent = True
            end
            object CheckBoxProofingDisableCapsLock: TcxCheckBox
              Left = 28
              Top = 339
              Caption = 'Disable Caps Lock'
              Enabled = False
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 14
              Transparent = True
            end
            object CheckBoxProofingCorrectAutoReplace: TcxCheckBox
              Left = 28
              Top = 376
              Caption = 'Correct text as you type'
              Enabled = False
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 15
              Transparent = True
            end
            object ListViewProofingAutoCorrectReplacements: TcxListView
              Left = 46
              Top = 426
              Width = 256
              Height = 85
              Anchors = [akLeft, akTop, akRight]
              ColumnClick = False
              Columns = <
                item
                  MaxWidth = 100
                  MinWidth = 100
                  Width = 100
                end
                item
                  AutoSize = True
                end>
              Enabled = False
              IconOptions.WrapText = False
              ReadOnly = True
              RowSelect = True
              ShowColumnHeaders = False
              SortType = stText
              Style.TransparentBorder = False
              TabOrder = 18
              ViewStyle = vsReport
              OnClick = ListViewProofingAutoCorrectReplacementsClick
            end
            object ButtonProofingAutoCorrectAdd: TcxButton
              Left = 308
              Top = 426
              Width = 88
              Height = 23
              Action = ActionProofingAdd
              Anchors = [akRight, akBottom]
              Enabled = False
              TabOrder = 19
            end
            object ButtonProofingAutoCorrectDelete: TcxButton
              Left = 308
              Top = 455
              Width = 88
              Height = 23
              Action = ActionProofingDelete
              Anchors = [akRight, akBottom]
              Enabled = False
              TabOrder = 20
            end
            object CheckBoxProofingCorrectAutomaticallyUseSuggestions: TcxCheckBox
              Left = 46
              Top = 517
              Caption = 'Automatically use suggestions from the spell checker'
              Enabled = False
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 21
              Transparent = True
            end
            object EditProofingAutoCorrectReplacementFrom: TcxTextEdit
              Left = 93
              Top = 401
              Enabled = False
              Properties.OnChange = EditProofingAutoCorrectReplacementFromPropertiesChange
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 16
              Width = 107
            end
            object EditProofingAutoCorrectReplacementTo: TcxTextEdit
              Left = 237
              Top = 401
              Anchors = [akLeft, akTop, akRight]
              Enabled = False
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 17
              Width = 159
            end
            object CheckBoxProofingAutoCorrect: TcxCheckBox
              Left = 12
              Top = 239
              Caption = 'Enable AutoCorrect'
              Properties.OnChange = CheckBoxProofingAutoCorrectPropertiesChange
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 9
              Transparent = True
            end
            object ButtonProofingAutoCorrectExceptions: TcxButton
              Left = 250
              Top = 264
              Width = 146
              Height = 23
              Caption = 'Exceptions...'
              Enabled = False
              TabOrder = 12
              OnClick = ButtonProofingAutoCorrectExceptionsClick
            end
            object CheckBoxProofingCorrectInitialCaps: TcxCheckBox
              Left = 28
              Top = 289
              Caption = 'Correct TWo &INitial CApitals'
              Enabled = False
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 11
              Transparent = True
            end
            object LayoutGroupProofing: TdxLayoutGroup
              AlignHorz = ahClient
              AlignVert = avTop
              CaptionOptions.Visible = False
              ButtonOptions.Buttons = <>
              Hidden = True
              ItemIndex = 1
              ShowBorder = False
              Index = -1
            end
            object dxLayoutItem5: TdxLayoutItem
              Parent = dxLayoutGroup3
              CaptionOptions.Visible = False
              Control = CheckBoxProofingIgnoreNumbers
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 121
              ControlOptions.ShowBorder = False
              Index = 4
            end
            object dxLayoutItem1: TdxLayoutItem
              Parent = dxLayoutGroup4
              CaptionOptions.Visible = False
              Control = cxLabel12
              ControlOptions.OriginalHeight = 17
              ControlOptions.OriginalWidth = 448
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object dxLayoutItem6: TdxLayoutItem
              Parent = dxLayoutGroup3
              CaptionOptions.Visible = False
              Control = CheckBoxProofingIgnoreRepeatWords
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 121
              ControlOptions.ShowBorder = False
              Index = 3
            end
            object dxLayoutGroup3: TdxLayoutGroup
              Parent = dxLayoutGroup4
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              Offsets.Left = 12
              ButtonOptions.Buttons = <>
              Hidden = True
              ShowBorder = False
              Index = 1
            end
            object dxLayoutItem7: TdxLayoutItem
              Parent = dxLayoutGroup3
              CaptionOptions.Text = 'Dictionaries:'
              Visible = False
              Control = ComboBoxProofingLanguages
              ControlOptions.AlignHorz = ahRight
              ControlOptions.AutoControlAreaAlignment = False
              ControlOptions.OriginalHeight = 21
              ControlOptions.OriginalWidth = 217
              ControlOptions.ShowBorder = False
              Index = 6
            end
            object dxLayoutGroup4: TdxLayoutGroup
              Parent = LayoutGroupProofing
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              ButtonOptions.Buttons = <>
              Hidden = True
              ShowBorder = False
              Index = 0
            end
            object dxLayoutItem9: TdxLayoutItem
              Parent = dxLayoutGroup6
              CaptionOptions.Visible = False
              Control = cxLabel13
              ControlOptions.OriginalHeight = 17
              ControlOptions.OriginalWidth = 448
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object dxLayoutGroup6: TdxLayoutGroup
              Parent = LayoutGroupProofing
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              ButtonOptions.Buttons = <>
              Hidden = True
              ItemIndex = 1
              ShowBorder = False
              Index = 1
            end
            object dxLayoutItem8: TdxLayoutItem
              Parent = dxLayoutGroup3
              CaptionOptions.Text = 'User dictionary:'
              Control = ButtonProofingEditCustomDictionary
              ControlOptions.AlignHorz = ahRight
              ControlOptions.AutoControlAreaAlignment = False
              ControlOptions.OriginalHeight = 23
              ControlOptions.OriginalWidth = 146
              ControlOptions.ShowBorder = False
              Index = 7
            end
            object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
              Parent = dxLayoutGroup3
              CaptionOptions.Text = 'Separator'
              Index = 5
            end
            object dxLayoutItem2: TdxLayoutItem
              Parent = dxLayoutGroup3
              CaptionOptions.Visible = False
              Control = CheckBoxProofingSpellCheck
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 121
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object dxLayoutItem3: TdxLayoutItem
              Parent = dxLayoutGroup3
              CaptionOptions.Visible = False
              Control = CheckBoxProofingIgnoreUppercase
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 121
              ControlOptions.ShowBorder = False
              Index = 1
            end
            object dxLayoutItem4: TdxLayoutItem
              Parent = dxLayoutGroup3
              CaptionOptions.Visible = False
              Control = CheckBoxProofingIgnoreMixedCase
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 121
              ControlOptions.ShowBorder = False
              Index = 2
            end
            object dxLayoutItem11: TdxLayoutItem
              Parent = dxLayoutAutoCreatedGroup5
              AlignHorz = ahClient
              CaptionOptions.Visible = False
              Control = CheckBoxProofingCorrectSentenceCaps
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 121
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object dxLayoutItem13: TdxLayoutItem
              Parent = LayoutGroupProofingAutoCorrect
              CaptionOptions.Visible = False
              Control = CheckBoxProofingCorrectCapsLock
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 121
              ControlOptions.ShowBorder = False
              Index = 1
            end
            object dxLayoutItem14: TdxLayoutItem
              Parent = LayoutGroupProofingAutoCorrect
              CaptionOptions.Visible = False
              Control = CheckBoxProofingDisableCapsLock
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 121
              ControlOptions.ShowBorder = False
              Index = 2
            end
            object dxLayoutGroup7: TdxLayoutGroup
              Parent = dxLayoutGroup6
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              Offsets.Left = 12
              ButtonOptions.Buttons = <>
              Hidden = True
              ItemIndex = 1
              ShowBorder = False
              Index = 1
            end
            object dxLayoutItem10: TdxLayoutItem
              Parent = LayoutGroupProofingAutoCorrect
              AlignHorz = ahLeft
              CaptionOptions.Visible = False
              Control = CheckBoxProofingCorrectAutoReplace
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 140
              ControlOptions.ShowBorder = False
              Index = 4
            end
            object dxLayoutGroup5: TdxLayoutGroup
              Parent = LayoutGroupProofingAutoCorrect
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              Offsets.Left = 18
              ButtonOptions.Buttons = <>
              Hidden = True
              ShowBorder = False
              Index = 5
            end
            object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
              Parent = dxLayoutGroup5
              LayoutDirection = ldHorizontal
              Index = 1
            end
            object dxLayoutItem18: TdxLayoutItem
              Parent = dxLayoutAutoCreatedGroup3
              AlignHorz = ahClient
              AlignVert = avTop
              Control = ListViewProofingAutoCorrectReplacements
              ControlOptions.OriginalHeight = 85
              ControlOptions.OriginalWidth = 308
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
              Parent = dxLayoutAutoCreatedGroup3
              Index = 1
            end
            object dxLayoutItem19: TdxLayoutItem
              Parent = dxLayoutAutoCreatedGroup2
              AlignVert = avTop
              CaptionOptions.Visible = False
              Control = ButtonProofingAutoCorrectAdd
              ControlOptions.OriginalHeight = 23
              ControlOptions.OriginalWidth = 88
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object dxLayoutItem20: TdxLayoutItem
              Parent = dxLayoutAutoCreatedGroup2
              AlignVert = avTop
              CaptionOptions.Visible = False
              Control = ButtonProofingAutoCorrectDelete
              ControlOptions.OriginalHeight = 23
              ControlOptions.OriginalWidth = 73
              ControlOptions.ShowBorder = False
              Index = 1
            end
            object dxLayoutItem21: TdxLayoutItem
              Parent = dxLayoutGroup5
              CaptionOptions.Visible = False
              Control = CheckBoxProofingCorrectAutomaticallyUseSuggestions
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 121
              ControlOptions.ShowBorder = False
              Index = 2
            end
            object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
              Parent = dxLayoutGroup5
              LayoutDirection = ldHorizontal
              Index = 0
            end
            object EditProofingAutoCorrectReplace: TdxLayoutItem
              Parent = dxLayoutAutoCreatedGroup1
              AlignHorz = ahClient
              AlignVert = avTop
              CaptionOptions.Text = 'Replace:'
              Control = EditProofingAutoCorrectReplacementFrom
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 114
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object dxLayoutItem17: TdxLayoutItem
              Parent = dxLayoutAutoCreatedGroup1
              AlignHorz = ahClient
              AlignVert = avTop
              CaptionOptions.Text = 'With:'
              Control = EditProofingAutoCorrectReplacementTo
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 168
              ControlOptions.ShowBorder = False
              Index = 1
            end
            object dxLayoutSeparatorItem2: TdxLayoutSeparatorItem
              Parent = LayoutGroupProofingAutoCorrect
              CaptionOptions.Text = 'Separator'
              Index = 3
            end
            object dxLayoutItem22: TdxLayoutItem
              Parent = dxLayoutGroup7
              AlignHorz = ahLeft
              CaptionOptions.Visible = False
              Control = CheckBoxProofingAutoCorrect
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 115
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object LayoutGroupProofingAutoCorrect: TdxLayoutGroup
              Parent = dxLayoutGroup7
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              Offsets.Left = 16
              ButtonOptions.Buttons = <>
              Enabled = False
              Hidden = True
              ShowBorder = False
              Index = 1
            end
            object dxLayoutItem15: TdxLayoutItem
              Parent = dxLayoutAutoCreatedGroup4
              CaptionOptions.Visible = False
              Control = ButtonProofingAutoCorrectExceptions
              ControlOptions.AlignHorz = ahRight
              ControlOptions.AutoControlAreaAlignment = False
              ControlOptions.OriginalHeight = 23
              ControlOptions.OriginalWidth = 146
              ControlOptions.ShowBorder = False
              Index = 1
            end
            object dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup
              Parent = LayoutGroupProofingAutoCorrect
              LayoutDirection = ldHorizontal
              Index = 0
            end
            object dxLayoutItem12: TdxLayoutItem
              Parent = dxLayoutAutoCreatedGroup5
              CaptionOptions.Visible = False
              Control = CheckBoxProofingCorrectInitialCaps
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 121
              ControlOptions.ShowBorder = False
              Index = 1
            end
            object dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup
              Parent = dxLayoutAutoCreatedGroup4
              AlignHorz = ahClient
              Index = 0
            end
          end
        end
        object TabSheetSystem: TcxTabSheet
          AlignWithMargins = True
          Margins.Left = 12
          Margins.Top = 12
          Margins.Right = 12
          Margins.Bottom = 8
          Caption = 'Advanced'
          ImageIndex = 5
          object LayoutControlAdvanced: TdxLayoutControl
            Left = 0
            Top = 0
            Width = 413
            Height = 414
            Align = alClient
            ParentBackground = True
            TabOrder = 0
            Transparent = True
            LayoutLookAndFeel = LayoutSkinLookAndFeel
            OptionsItem.FocusControlOnItemCaptionClick = True
            object LabelAutoUpdateIntro: TcxLabel
              AlignWithMargins = True
              Left = 12
              Top = 139
              Margins.Left = 12
              Margins.Top = 6
              Margins.Right = 12
              Margins.Bottom = 4
              Caption = 
                'The application can automatically check for, download and instal' +
                'l updates as they become available. You will always be asked to ' +
                'accept an update before it is downloaded and installed.'
              ParentColor = False
              Style.Color = 16053234
              Properties.ShowAccelChar = False
              Properties.WordWrap = True
              Transparent = True
              Width = 401
            end
            object CheckBoxSingleInstance: TcxCheckBox
              Left = 12
              Top = 23
              Caption = 'Use single instance of the application'
              Properties.OnChange = CheckBoxSingleInstancePropertiesChange
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 1
              Transparent = True
            end
            object cxLabel4: TcxLabel
              Left = 0
              Top = 56
              AutoSize = False
              Caption = 'File types'
              Style.HotTrack = False
              Style.TextStyle = [fsBold]
              Style.TransparentBorder = False
              Properties.LineOptions.Visible = True
              Transparent = True
              Height = 17
              Width = 413
            end
            object ButtonRegisterFiletypes: TcxButton
              Left = 12
              Top = 79
              Width = 253
              Height = 23
              Caption = 'Restore Translation Manager file associations'
              TabOrder = 3
              OnClick = ButtonRegisterFiletypesClick
            end
            object cxLabel11: TcxLabel
              Left = 0
              Top = 116
              AutoSize = False
              Caption = 'Auto Update'
              Style.HotTrack = False
              Style.TextStyle = [fsBold]
              Style.TransparentBorder = False
              Properties.LineOptions.Visible = True
              Transparent = True
              Height = 17
              Width = 413
            end
            object CheckBoxAutoUpdateEnabled: TcxCheckBox
              AlignWithMargins = True
              Left = 12
              Top = 188
              Margins.Left = 12
              Margins.Top = 6
              Margins.Right = 12
              Margins.Bottom = 0
              Caption = 'Check for updates every time the application starts'
              Style.HotTrack = False
              Style.TransparentBorder = False
              TabOrder = 6
              Transparent = True
            end
            object ButtonAutoUpdateNow: TcxButton
              Left = 12
              Top = 258
              Width = 197
              Height = 23
              Caption = 'Check for updates now'
              TabOrder = 8
            end
            object ButtonAutoUpdateReset: TcxButton
              Left = 12
              Top = 229
              Width = 197
              Height = 23
              Caption = 'Reset declined updates'
              TabOrder = 7
            end
            object cxLabel14: TcxLabel
              Left = 0
              Top = 0
              AutoSize = False
              Caption = 'Launch'
              Style.HotTrack = False
              Style.TextStyle = [fsBold]
              Style.TransparentBorder = False
              Properties.LineOptions.Visible = True
              Transparent = True
              Height = 17
              Width = 413
            end
            object LayoutControlAdvancedGroup_Root: TdxLayoutGroup
              AlignHorz = ahClient
              AlignVert = avTop
              CaptionOptions.Visible = False
              ButtonOptions.Buttons = <>
              Hidden = True
              ShowBorder = False
              Index = -1
            end
            object LayoutControlAdvancedItem1: TdxLayoutItem
              Parent = LayoutControlAdvancedGroup9
              CaptionOptions.Visible = False
              Control = CheckBoxSingleInstance
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 121
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object LayoutControlAdvancedItem2: TdxLayoutItem
              Parent = LayoutControlAdvancedGroup2
              CaptionOptions.Visible = False
              Control = cxLabel4
              ControlOptions.OriginalHeight = 17
              ControlOptions.OriginalWidth = 402
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object LayoutControlAdvancedItem3: TdxLayoutItem
              Parent = LayoutControlAdvancedGroup4
              CaptionOptions.Visible = False
              Control = ButtonRegisterFiletypes
              ControlOptions.AlignHorz = ahLeft
              ControlOptions.OriginalHeight = 23
              ControlOptions.OriginalWidth = 253
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object LayoutControlAdvancedGroup2: TdxLayoutGroup
              Parent = LayoutControlAdvancedGroup_Root
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              Offsets.Top = 8
              ButtonOptions.Buttons = <>
              Hidden = True
              ShowBorder = False
              Index = 1
            end
            object LayoutControlAdvancedGroup4: TdxLayoutGroup
              Parent = LayoutControlAdvancedGroup2
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              Offsets.Left = 12
              ButtonOptions.Buttons = <>
              Hidden = True
              ShowBorder = False
              Index = 1
            end
            object LayoutControlAdvancedItem6: TdxLayoutItem
              Parent = LayoutControlAdvancedGroup5
              CaptionOptions.Visible = False
              Control = cxLabel11
              ControlOptions.OriginalHeight = 17
              ControlOptions.OriginalWidth = 442
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object LayoutControlAdvancedItem7: TdxLayoutItem
              Parent = LayoutControlAdvancedGroup6
              CaptionOptions.Visible = False
              Control = LabelAutoUpdateIntro
              ControlOptions.AutoColor = True
              ControlOptions.OriginalHeight = 43
              ControlOptions.OriginalWidth = 428
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object LayoutControlAdvancedItem8: TdxLayoutItem
              Parent = LayoutControlAdvancedGroup6
              CaptionOptions.Visible = False
              Control = CheckBoxAutoUpdateEnabled
              ControlOptions.OriginalHeight = 19
              ControlOptions.OriginalWidth = 121
              ControlOptions.ShowBorder = False
              Index = 1
            end
            object LayoutControlAdvancedGroup5: TdxLayoutGroup
              Parent = LayoutControlAdvancedGroup_Root
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              Offsets.Top = 8
              Visible = False
              ButtonOptions.Buttons = <>
              Hidden = True
              ItemIndex = 1
              ShowBorder = False
              Index = 2
            end
            object LayoutControlAdvancedGroup6: TdxLayoutGroup
              Parent = LayoutControlAdvancedGroup5
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              Offsets.Left = 12
              ButtonOptions.Buttons = <>
              Hidden = True
              ItemIndex = 2
              ShowBorder = False
              Index = 1
            end
            object LayoutControlAdvancedItem9: TdxLayoutItem
              Parent = LayoutControlAdvancedGroup6
              CaptionOptions.Visible = False
              Control = ButtonAutoUpdateNow
              ControlOptions.AlignHorz = ahLeft
              ControlOptions.OriginalHeight = 23
              ControlOptions.OriginalWidth = 197
              ControlOptions.ShowBorder = False
              Index = 4
            end
            object LayoutControlAdvancedItem10: TdxLayoutItem
              Parent = LayoutControlAdvancedGroup6
              CaptionOptions.Visible = False
              Control = ButtonAutoUpdateReset
              ControlOptions.AlignHorz = ahLeft
              ControlOptions.OriginalHeight = 23
              ControlOptions.OriginalWidth = 197
              ControlOptions.ShowBorder = False
              Index = 3
            end
            object LayoutControlAdvancedItem17: TdxLayoutItem
              Parent = LayoutControlAdvancedGroup7
              CaptionOptions.Visible = False
              Control = cxLabel14
              ControlOptions.OriginalHeight = 17
              ControlOptions.OriginalWidth = 442
              ControlOptions.ShowBorder = False
              Index = 0
            end
            object LayoutControlAdvancedGroup7: TdxLayoutGroup
              Parent = LayoutControlAdvancedGroup_Root
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              ButtonOptions.Buttons = <>
              Hidden = True
              ShowBorder = False
              Index = 0
            end
            object LayoutControlAdvancedGroup9: TdxLayoutGroup
              Parent = LayoutControlAdvancedGroup7
              CaptionOptions.Text = 'Hidden Group'
              CaptionOptions.Visible = False
              Offsets.Left = 12
              ButtonOptions.Buttons = <>
              Hidden = True
              ShowBorder = False
              Index = 1
            end
            object dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
              Parent = LayoutControlAdvancedGroup6
              CaptionOptions.Text = 'Empty Space Item'
              SizeOptions.Height = 10
              SizeOptions.Width = 10
              Index = 2
            end
          end
        end
      end
      object PanelCategory: TPanel [1]
        Left = 6
        Top = 6
        Width = 122
        Height = 459
        BevelOuter = bvNone
        Color = 16053234
        FullRepaint = False
        ShowCaption = False
        TabOrder = 0
        object ButtonCategoryGeneral: TcxButton
          AlignWithMargins = True
          Left = 0
          Top = 0
          Width = 120
          Height = 25
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 2
          Margins.Bottom = 6
          Align = alTop
          Action = ActionCategoryGeneral
          OptionsImage.Margin = 8
          SpeedButtonOptions.Flat = True
          SpeedButtonOptions.Transparent = True
          TabOrder = 0
          TabStop = False
          OnEnter = ButtonCategoryEnter
        end
        object ButtonCategoryFiles: TcxButton
          AlignWithMargins = True
          Left = 0
          Top = 93
          Width = 120
          Height = 25
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 2
          Margins.Bottom = 6
          Align = alTop
          Action = ActionCategoryFiles
          OptionsImage.Margin = 8
          SpeedButtonOptions.Flat = True
          SpeedButtonOptions.Transparent = True
          TabOrder = 3
          TabStop = False
          OnEnter = ButtonCategoryEnter
        end
        object ButtonCategoryAdvanced: TcxButton
          AlignWithMargins = True
          Left = 0
          Top = 155
          Width = 120
          Height = 25
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 2
          Margins.Bottom = 6
          Align = alTop
          Action = ActionCategorySystem
          OptionsImage.Margin = 8
          SpeedButtonOptions.Flat = True
          SpeedButtonOptions.Transparent = True
          TabOrder = 5
          TabStop = False
          OnEnter = ButtonCategoryEnter
        end
        object ButtonCategoryProofing: TcxButton
          AlignWithMargins = True
          Left = 0
          Top = 124
          Width = 120
          Height = 25
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 2
          Margins.Bottom = 6
          Align = alTop
          Action = ActionCategoryProofing
          OptionsImage.Margin = 8
          SpeedButtonOptions.Flat = True
          SpeedButtonOptions.Transparent = True
          TabOrder = 4
          TabStop = False
          OnEnter = ButtonCategoryEnter
        end
        object ButtonCategoryTranslators: TcxButton
          AlignWithMargins = True
          Left = 0
          Top = 62
          Width = 120
          Height = 25
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 2
          Margins.Bottom = 6
          Align = alTop
          Action = ActionCategoryTranslators
          OptionsImage.Margin = 8
          SpeedButtonOptions.Flat = True
          SpeedButtonOptions.Transparent = True
          TabOrder = 2
          TabStop = False
          OnEnter = ButtonCategoryEnter
        end
        object ButtonCategoryAppearance: TcxButton
          AlignWithMargins = True
          Left = 0
          Top = 31
          Width = 120
          Height = 25
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 2
          Margins.Bottom = 6
          Align = alTop
          Action = ActionCategoryAppearance
          OptionsImage.Margin = 8
          SpeedButtonOptions.Flat = True
          SpeedButtonOptions.Transparent = True
          TabOrder = 1
          TabStop = False
          OnEnter = ButtonCategoryEnter
        end
      end
      inherited LayoutControlGroup_Root: TdxLayoutGroup
        AlignVert = avClient
        LayoutDirection = ldHorizontal
      end
      object LayoutItemCategories: TdxLayoutItem
        Parent = LayoutControlGroup_Root
        AlignHorz = ahLeft
        AlignVert = avClient
        CaptionOptions.Visible = False
        Control = PanelCategory
        ControlOptions.AutoColor = True
        ControlOptions.OriginalHeight = 424
        ControlOptions.OriginalWidth = 122
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object LayoutItemPages: TdxLayoutItem
        Parent = LayoutControlGroup_Root
        AlignHorz = ahClient
        AlignVert = avClient
        CaptionOptions.Visible = False
        Control = PageControl
        ControlOptions.OriginalHeight = 530
        ControlOptions.OriginalWidth = 477
        ControlOptions.ShowBorder = False
        Index = 1
      end
    end
  end
  inherited ActionList: TActionList
    Left = 36
    Top = 144
    object ActionCategoryGeneral: TAction
      Category = 'Category'
      Caption = 'General'
      Checked = True
      GroupIndex = 1
      OnExecute = ActionCategoryExecute
      OnUpdate = ActionCategoryUpdate
    end
    object ActionCategoryAppearance: TAction
      Category = 'Category'
      Caption = 'Appearance'
      GroupIndex = 1
      OnExecute = ActionCategoryExecute
      OnUpdate = ActionCategoryUpdate
    end
    object ActionCategoryTranslators: TAction
      Category = 'Category'
      Caption = 'Translators'
      GroupIndex = 1
      OnExecute = ActionCategoryExecute
      OnUpdate = ActionCategoryUpdate
    end
    object ActionCategoryFiles: TAction
      Category = 'Category'
      Caption = 'Files'
      GroupIndex = 1
      OnExecute = ActionCategoryExecute
      OnUpdate = ActionCategoryUpdate
    end
    object ActionCategoryProofing: TAction
      Category = 'Category'
      Caption = 'Proofing'
      GroupIndex = 1
      OnExecute = ActionCategoryExecute
      OnUpdate = ActionCategoryUpdate
    end
    object ActionCategorySystem: TAction
      Category = 'Category'
      Caption = 'Advanced'
      GroupIndex = 1
      OnExecute = ActionCategoryExecute
      OnUpdate = ActionCategoryUpdate
    end
    object ActionFoldersModify: TAction
      Category = 'Folders'
      Caption = '&Modify'
      Hint = 'Select folder'
      OnExecute = ActionFoldersModifyExecute
      OnUpdate = ActionFoldersModifyUpdate
    end
    object ActionFolderReset: TAction
      Category = 'Folders'
      Caption = 'Reset'
      Hint = 'Reset to the default value'
      OnExecute = ActionFolderResetExecute
      OnUpdate = ActionFolderResetUpdate
    end
    object ActionFoldersExplorer: TAction
      Category = 'Folders'
      Caption = 'ActionFoldersExplorer'
      Hint = 'Open the specified folder in Windows Explorer'
      OnExecute = ActionFoldersExplorerExecute
    end
    object ActionFolderResetAll: TAction
      Category = 'Folders'
      Caption = 'Reset all'
      Hint = 'Reset all file locations back to their defaults'
      OnExecute = ActionFolderResetAllExecute
    end
    object ActionProofingAdd: TAction
      Category = 'Proofing'
      Caption = 'Add'
      OnExecute = ActionProofingAddExecute
      OnUpdate = ActionProofingAddUpdate
    end
    object ActionProofingReplace: TAction
      Category = 'Proofing'
      Caption = 'Replace'
      OnExecute = ActionProofingReplaceExecute
      OnUpdate = ActionProofingReplaceUpdate
    end
    object ActionProofingDelete: TAction
      Category = 'Proofing'
      Caption = 'Delete'
      OnExecute = ActionProofingDeleteExecute
      OnUpdate = ActionProofingDeleteUpdate
    end
    object ActionEditStatusGlyphs: TAction
      Category = 'Edit'
      AutoCheck = True
      Caption = 'Display status indicators'
      OnExecute = ActionDummyExecute
    end
    object ActionEditStatusHint: TAction
      Category = 'Edit'
      AutoCheck = True
      Caption = 'Status indicator hints'
      Hint = 'Provide mouse over hints on status glyphs'
      OnExecute = ActionDummyExecute
      OnUpdate = ActionEditStatusHintUpdate
    end
  end
  object ImageListColorSchemesGlyphsLarge: TcxImageList
    SourceDPI = 96
    Height = 48
    Width = 48
    FormatVersion = 1
    DesignInfo = 27000872
    ImageInfo = <
      item
        Image.Data = {
          36240000424D3624000000000000360000002800000030000000300000000100
          2000000000000024000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000010000
          0002000000040000000600000008000000090000000A0000000B0000000B0000
          000A0000000A0000000800000006000000040000000200000001000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000010000000200000005000000080000
          000F000000160000001D00000022000000270000002B0000002D0000002D0000
          002B00000027000000230000001D000000160000000F00000009000000050000
          0002000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000001000000050000000B00000015000000201410
          0C42423328786A5441A48E6F56C6AB8768DFC19675F1CFA27EFBCFA27EFBC196
          75F1AB8768DF8E6F56C66A5441A44233287A14100C4300000022000000160000
          000C000000060000000200000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000100000001000000040000000A00000015000000253C2E2371795D47B2B48D
          6EE7DAB292FFE1BFA4FFE7CAB3FFEDD4C0FFF1DCCCFFF3E0D2FFF3E1D3FFF1DD
          CDFFEDD6C3FFE8CCB6FFE2C0A6FFDBB394FFB48D6EE7785C47B43C2E23730000
          0027000000170000000B00000004000000010000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          00020000000700000010000000213D2E227285664BC0D2A784FDDFBA9CFFE9CC
          B4FFF0D8C6FFF1DAC8FFF1DCCAFFF1DCCBFFF2DDCCFFF2DDCDFFF2DECEFFF2DE
          CEFFF2DECEFFF2DDCDFFF2DDCCFFF2DCCCFFEACFB9FFE0BDA0FFD2A885FD8566
          4CC23D2E22750000002400000012000000080000000200000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000030000
          0009000000161C150F486D523CA8C59975F5DCB595FFE9C9AFFFEDD0B9FFEDD2
          BCFFEED3BEFFEFD5C0FFEFD6C2FFEFD7C4FFF0D8C5FFF0D8C6FFF0D9C6FFF0D9
          C6FFF0D8C6FFF0D8C5FFF0D8C5FFEFD7C4FFEFD6C2FFEFD5C0FFEBCDB7FFDDB8
          99FFC69A78F66D523CAB1C150F4C000000190000000A00000003000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000001000000030000000A0000
          001A36281C698C684ACBD3A680FFE1BA9BFFE8C5AAFFE9C8ADFFEACAB1FFEBCC
          B3FFECCEB6FFECCFB7FFECD0B9FFEDD1BBFFEDD2BDFFEED3BEFFEED3BEFFEED3
          BEFFEED3BDFFEED3BDFFEDD2BCFFEDD1BBFFEDD1BAFFECCFB7FFEBCDB5FFEBCC
          B3FFE3C0A4FFD4A985FF8D684ACE35281C6D0000001D0000000B000000030000
          0001000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000001000000030000000A0000001A4130
          2178A47A56E1D4A57FFFE3BA99FFE4BD9DFFE5BFA0FFE6C2A4FFE7C4A8FFE8C6
          AAFFE9C8ADFFE9C9AFFFEACAB0FFEACBB2FFEBCCB4FFEBCCB4FFEBCDB5FFEBCD
          B5FFEBCDB5FFEBCCB4FFEBCCB4FFEACBB2FFEACAB0FFE9C9AFFFE9C8ADFFE8C6
          AAFFE7C4A8FFE6C2A4FFD6A985FFA67A58E34130217D0000001E0000000B0000
          0003000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000001000000020000000800000018402E1F78A97C
          56E8D2A078FFDFB18CFFE1B591FFE2B794FFE3BA98FFE4BC9CFFE4BE9FFFE5BF
          A1FFE6C1A4FFE6C3A6FFE7C4A8FFE7C5AAFFE8C6AAFFE8C7ACFFE8C7ACFFE8C7
          ACFFE8C6ACFFE8C7ACFFE8C6ABFFE7C5AAFFE7C4A8FFE6C3A6FFE6C2A4FFE5C0
          A2FFE4BE9FFFE3BB9BFFE3B998FFD5A580FFAA7C57EA402E1F7C0000001B0000
          000A000000020000000100000000000000000000000000000000000000000000
          00000000000000000000000000010000000600000014322317679E704CE1CE99
          6EFFDCA980FFDDAC84FFDEAE88FFDFB18CFFE0B390FFE1B592FFE2B896FFE3B9
          98FFE3BB9AFFE4BC9CFFE4BD9EFFE5BEA0FFE5BFA1FFE5C0A2FFE5C0A2FFE5C0
          A2FFE5C0A2FFE5C0A2FFE5BFA1FFE5BEA0FFE4BD9EFFE4BC9CFFE3BB9AFFE3B9
          98FFE2B795FFE1B592FFE0B38FFFDFB18CFFD09E75FF9E714CE23223176C0000
          0017000000070000000100000000000000000000000000000000000000000000
          00000000000000000001000000040000000E19110B43805838C9C89064FFD9A2
          76FFDAA479FFDAA57BFFDBA880FFDCAB83FFDDAC86FFDEAF89FFDFB18CFFE0B2
          8EFFE0B390FFE1B693FFE1B794FFE2B896FFE2B897FFE2B998FFE2B998FFE2B9
          98FFE2B998FFE2B998FFE2B897FFE2B896FFE1B794FFE1B693FFE0B491FFE0B3
          8FFFDFB18CFFDEAF8AFFDDAD86FFDCAB83FFDBA87FFFCA9368FF805838CC1911
          0B48000000100000000400000001000000000000000000000000000000000000
          00000000000000000001000000080000001B5F4026A3BF8556FFD59B6DFFD69D
          70FFD79F72FFD7A074FFD8A277FFD9A479FFD9A57CFFDAA77FFFDBA981FFDCAB
          84FFDCAC86FFDDAE88FFDDAF8AFFDEB08CFFDEB18CFFDEB18DFFDEB28EFFDFB2
          8EFFDEB18DFFDEB18DFFDEB18CFFDEB08CFFDDAF8AFFDDAE88FFDCAC86FFDCAB
          85FFDBA981FFDAA87FFFD9A57BFFD9A47AFFD8A277FFD7A074FFC18759FF5F40
          27A8000000200000000A00000001000000000000000000000000000000000000
          00000000000100000004000000103321146AAB7245F5CA8E5FFFD29666FFD398
          69FFD3996BFFD49A6EFFD59C70FFD59E72FFD59E73FFD6A076FFD7A278FFD7A3
          7AFFD8A57DFFD9A67FFFD9A77FFFD9A880FFDAA982FFDAAA83FFDAAA83FFDAAA
          83FFDAAA83FFDAAA83FFDAA982FFDAA881FFD9A77FFFD9A67FFFD8A57DFFD8A3
          7BFFD7A278FFD6A075FFD69F74FFD59E72FFD59C70FFD49B6EFFCC9264FFAB73
          46F5332113700000001400000005000000010000000000000000000000000000
          000000000001000000080000001C6D4626BABD7F4DFFCE905EFFCF9261FFCF93
          62FFD09565FFD19667FFD19768FFD2996AFFD29A6CFFD39B6EFFD39C6FFFD49D
          71FFD49E72FFD5A075FFD5A076FFD5A177FFD6A278FFD6A278FFD6A278FFD6A2
          78FFD6A278FFD6A278FFD6A278FFD5A177FFD5A176FFD59F74FFD49E72FFD49D
          71FFD39C70FFD39B6EFFD29A6DFFD2996BFFD19869FFD19667FFD09565FFBE82
          52FF6D4626BE000000220000000A000000010000000000000000000000000000
          0001000000030000000F2F1E1066AB6E3DFDC78753FFCB8B58FFCB8C59FFCC8E
          5CFFCD905EFFCD905FFFCE9262FFCE9363FFCF9565FFCF9567FFD09668FFD097
          69FFD0986AFFD1996CFFD1996CFFD19A6DFFD29A6EFFD29A6EFFD29A6EFFD29B
          6EFFD29B6EFFD29B6EFFD19A6DFFD19A6DFFD1996CFFD1996CFFD0986AFFD098
          6AFFD09769FFD09667FFCF9565FFCE9363FFCE9262FFCD9160FFCD905EFFC88A
          58FFAC6E3EFD2F1D0F6D00000013000000040000000100000000000000000000
          000100000006000000165C381CA9B47441FFC78651FFC88753FFC88854FFC989
          55FFC98A57FFCA8B59FFCA8D5BFFCB8E5CFFCB8F5EFFCC905FFFCC9161FFCD91
          61FFCD9263FFCD9364FFCE9465FFCE9466FFCE9566FFCE9566FFCE9566FFCE95
          67FFCE9567FFCE9567FFCE9466FFCE9466FFCE9465FFCD9364FFCD9364FFCD92
          62FFCC9161FFCC905FFFCC8F5FFFCB8E5DFFCB8D5CFFCA8C5AFFCA8B58FFC98A
          57FFB57644FF5C381BAF0000001C000000070000000100000000000000000000
          0001000000090F090434885326E3BB7B46FFC4834DFFC4844EFFC4844FFFC585
          51FFC58651FFC58652FFC68854FFC78956FFC78956FFC78A58FFC88C5AFFC88D
          5BFFC88D5BFFC98D5CFFC98E5DFFC98F5EFFC98F5EFFC98F5EFFCA8F5FFFCA8F
          5FFFCA8F5FFFCA8F5FFFC98F5EFFC98E5EFFC98E5EFFC98E5EFFC98D5CFFC88C
          5BFFC88B59FFC88B59FFC78A58FFC78956FFC68855FFC68854FFC68753FFC586
          52FFBD7E4AFF885328E60F09043B0000000C0000000200000000000000000000
          00020000000E2F1C0C69A46430FFC17F48FFC17F49FFC1804AFFC2814BFFC281
          4CFFC2824DFFC3834EFFC3834FFFC3844FFFC38450FFC48551FFC48652FFC486
          53FFC58755FFC58855FFC68957FFC68957FFC68957FFC68957FFC68957FFC689
          57FFC68957FFC68957FFC68957FFC68957FFC68957FFC58855FFC58855FFC587
          54FFC48653FFC48652FFC48552FFC48551FFC38450FFC3844FFFC3834FFFC383
          4EFFC2824DFFA56531FF2F1C0C71000000110000000300000001000000000000
          0004000000114C2C1396AA6934FFBE7B45FFBF7C46FFBF7D47FFBF7D48FFBF7E
          49FFC07E48FFC07F4BFFC0804BFFC1804BFFC1814DFFC1814EFFC1824EFFC182
          4EFFC2824FFFC28350FFC28350FFC28351FFC28351FFC28451FFC28451FFC284
          51FFC28450FFC28451FFC28451FFC28451FFC28351FFC28351FFC28350FFC283
          50FFC2824FFFC1824EFFC1824EFFC1814DFFC1814DFFC1804CFFC0804CFFC07F
          4BFFC07E49FFAB6A36FF4C2C129C000000160000000500000001000000000000
          000400000014643A17BBAF6D38FFBC7942FFBC7942FFBC7942FFBC7A44FFBD79
          44FFBD7B45FFBD7B46FFBD7C47FFBE7C47FFBE7D47FFBE7C49FFBE7E4AFFBF7E
          4AFFBF7E4AFFBF7F4BFFBF7F4CFFBF7F4CFFBF7F4CFFBF804DFFBF804DFFBF80
          4DFFBF804DFFBF804DFFBF804DFFBF804DFFBF7F4CFFBF7E4CFFBF7F4CFFBF7E
          4BFFBF7E4AFFBF7E4AFFBE7E4AFFBE7D49FFBE7D48FFBE7C46FFBD7B47FFBD7A
          46FFBD7B45FFB06E39FF643A17BF0000001A0000000600000001000000000000
          00050000001678451BD8B2703BFFBA7741FFBA7741FFBA7741FFBA7741FFBA77
          41FFBA7842FFBB7842FFBB7842FFBB7943FFBB7944FFBB7A45FFBC7A45FFBC7A
          45FFBC7B46FFBC7B47FFBC7B47FFBC7A47FFBC7C48FFBC7C48FFBC7B48FFBC7C
          48FFBC7C48FFBC7C48FFBC7C48FFBC7C48FFBC7C48FFBC7A47FFBC7B47FFBC7B
          47FFBC7B46FFBC7A45FFBC7945FFBB7A45FFBB7844FFBB7943FFBB7842FFBB78
          42FFBA7842FFB2703BFF78451BDB0000001D0000000700000001000000000000
          000500000017884D1FEDB5723CFFB8753FFFB8753FFFB8753FFFB8753FFFB874
          3FFFB8743FFFB8753FFFB8753FFFB87640FFB87640FFB97641FFB97741FFB977
          41FFB97742FFB97742FFB97843FFB97944FFB97944FFBB7845FFBB7845FFBB79
          45FFBB7945FFBB7946FFBB7A46FFBB7945FFBA7944FFB97944FFB97843FFB977
          42FFB97642FFB97641FFB97741FFB97541FFB87640FFB87640FFB8753FFFB875
          3FFFB8743FFFB5723CFF874D1FEE0000001F0000000700000001000000000000
          000500000017915321FAB5723CFFB6733DFFB6733DFFB6733DFFB6733DFFB673
          3DFFB6723DFFB6733DFFB6733DFFB6723DFFB6723DFFB6733DFFB6743EFFB674
          3EFFB77540FFB77640FFB87641FFB87642FFB87642FFB87843FFB87843FFB878
          43FFB87843FFB97844FFB97844FFB97844FFB97644FFB87743FFB87541FFB674
          3EFFB6743EFFB6733DFFB6733DFFB6733DFFB6733DFFB6733DFFB6733DFFB673
          3DFFB6733DFFB5723CFF925321FB0000001F0000000800000001000000000000
          000500000016915321FAB26F3AFFB3703BFFB3703BFFB3713BFFB3703BFFB370
          3BFFB3703BFFB3703BFFB3703BFFB3703BFFB3713CFFB4733DFFB4723DFFB473
          3EFFB4743FFFB57440FFB57340FFB57540FFB57440FFB57541FFB57441FFB574
          41FFB67542FFB67542FFB67542FFB67542FFB67542FFB67542FFB67542FFB473
          3EFFB3713BFFB3703BFFB3703BFFB3713BFFB3703BFFB3703BFFB3703BFFB370
          3BFFB3703BFFB2703AFF925321FB0000001E0000000700000001000000000000
          000500000014884E1FECAE6D38FFB16F39FFB16F39FFB16F3AFFB16F39FFB16F
          3AFFB16F39FFB16F39FFB16F39FFB2703CFFB3723DFFB3723DFFB3723FFFB373
          3EFFB37440FFB47441FFB47440FFB47541FFB47542FFB47542FFB57642FFB576
          42FFB57642FFB57643FFB57643FFB57644FFB57643FFB57643FFB57642FFB576
          43FFB3723FFFB16F39FFB16F39FFB16F39FFB16F3AFFB16F3AFFB16F39FFB16F
          3AFFB16F3AFFAE6D37FF874D1FEE0000001C0000000700000001000000000000
          00040000001178451CD6AA6834FFAF6D37FFAF6D38FFAF6D38FFAF6D37FFAF6D
          38FFAF6D37FFAF6D38FFB06E39FFB1703CFFB2723DFFB2723FFFB2733FFFB273
          40FFB27341FFB37440FFB37441FFB37542FFB37543FFB47644FFB47643FFB476
          44FFB47644FFB47644FFB47644FFB47644FFB47644FFB47644FFB47644FFB476
          44FFB37542FFB1703BFFAF6D38FFAF6D37FFAF6D38FFAF6D38FFAF6D37FFAF6D
          37FFAF6D38FFAA6833FF78451BD9000000190000000600000001000000000000
          00030000000F643917B7A5632FFFAD6B36FFAD6B36FFAD6B35FFAD6B36FFAD6B
          36FFAD6B36FFAD6C37FFAF6E3BFFB0713EFFB1713FFFB1723FFFB17340FFB273
          41FFB27441FFB27443FFB27543FFB27543FFB37544FFB37645FFB37645FFB377
          46FFB37746FFB37746FFB37746FFB37746FFB37746FFB37746FFB37746FFB377
          46FFB37645FFB27341FFAD6C37FFAD6B36FFAD6B36FFAD6B36FFAD6B36FFAD6B
          36FFAD6B36FFA5632FFF643917BC000000150000000500000000000000000000
          00030000000C4B2B118FA05F2BFFAC6934FFAC6A35FFAC6A35FFAC6934FFAC6A
          34FFAC6935FFAD6B38FFB0703FFFB0713EFFB1713FFFB17241FFB27342FFB275
          43FFB27443FFB37645FFB37545FFB37545FFB37746FFB37746FFB37747FFB477
          47FFB47747FFB47748FFB47949FFB47849FFB47849FFB47848FFB47748FFB477
          48FFB37746FFB37746FFAF6F3BFFAC6935FFAC6934FFAC6935FFAC6935FFAC69
          34FFAC6A34FFA05F2BFF4B2B1195000000110000000400000000000000000000
          0001000000082E1A0B609B5A27FFAA6833FFAA6833FFAA6832FFAA6733FFAA67
          33FFAA6833FFAD6D3AFFAF713EFFB07241FFB07340FFB07341FFB17443FFB175
          44FFB27544FFB27646FFB27746FFB37747FFB37848FFB37848FFB37849FFB379
          49FFB47849FFB47949FFB4794AFFB47949FFB4794AFFB47949FFB47949FFB379
          49FFB37949FFB37848FFAF713FFFAA6833FFAA6832FFAA6832FFAA6833FFAA68
          32FFAA6832FF9B5A27FF2E1A0B670000000D0000000200000000000000000000
          0001000000050E08032781491EDFA4632EFFA86732FFA86631FFA86632FFA866
          32FFA86632FFAD6E3CFFAE7141FFAF7242FFB07343FFB07544FFB17545FFB176
          47FFB17648FFB27748FFB27848FFB27849FFB3794AFFB3794BFFB3794BFFB37A
          4DFFB37A4DFFB47A4DFFB47A4DFFB47A4DFFB47A4DFFB47A4DFFB37A4DFFB37A
          4DFFB3794CFFB37A4BFFB17645FFA86631FFA86631FFA86631FFA86632FFA866
          31FFA4632FFF814A1EE20E080330000000080000000100000000000000000000
          0000000000030000000C5631149E9E5D29FFA6642FFFA66530FFA66430FFA665
          30FFA66530FFAD7140FFAE7243FFAE7343FFAF7445FFAF7546FFB07648FFB076
          47FFB1774AFFB2784BFFB2794CFFB2794CFFB37B4DFFB37A4EFFB37A4EFFB37B
          4FFFB37B4EFFB47C50FFB47D50FFB47C50FFB47C50FFB47D50FFB37C4FFFB37B
          4FFFB37A4EFFB37B4EFFB27A4CFFA6652FFFA66430FFA66430FFA66430FFA665
          30FF9E5D29FF563114A400000012000000050000000000000000000000000000
          000000000002000000072B180A57975725FDA3622DFFA4642EFFA4632FFFA464
          2EFFA4632EFFAD7243FFAE7444FFAE7545FFAF7547FFAF7748FFB0774AFFB078
          4CFFB1794CFFB17A4DFFB27B4EFFB27C50FFB37C50FFB37D50FFB37D51FFB37D
          51FFB37E52FFB47F52FFB47E53FFB47F52FFB47F53FFB47E53FFB37E51FFB37D
          52FFB37D51FFB37D51FFB27B4EFFA4632FFFA4632FFFA4642FFFA4642EFFA362
          2DFF975725FD2B180A5E0000000B000000020000000000000000000000000000
          000000000000000000040000000C603716AE9D5C28FFA3622DFFA3612DFFA362
          2DFFA3622DFFAC7141FFAE7547FFAF7648FFB0784AFFB0784BFFB1794CFFB17B
          4EFFB27C50FFB37C50FFB47E53FFB47F54FFB48055FFB58055FFB58055FFB581
          56FFB58157FFB58157FFB58157FFB58157FFB58157FFB58156FFB58157FFB57F
          55FFB48055FFB47F54FFB0794BFFA3622DFFA3612DFFA3622DFFA3622DFF9D5C
          28FF603716B30000001300000006000000010000000000000000000000000000
          00000000000000000002000000062C190A578F5322F29F5E29FFA1612CFFA160
          2BFFA1602CFFA86C3DFFAE7749FFAF774BFFB07A4DFFB17C50FFB27D51FFB27D
          52FFB37F55FFB37F55FFB58158FFB58259FFB6835AFFB6845BFFB6845CFFB685
          5CFFB7855DFFB7855CFFB7865DFFB7865DFFB7865DFFB6855CFFB6845CFFB683
          5AFFB6845AFFB58259FFAD7446FFA1612CFFA1602CFFA1602CFF9F5E2AFF8F53
          22F32C190A5E0000000A00000003000000000000000000000000000000000000
          00000000000000000000000000030000000A502E12939A5926FFA05F2BFFA05F
          2BFFA0602BFFA56837FFB0794EFFB17B50FFB27D52FFB37F55FFB48057FFB482
          59FFB5835AFFB6845CFFB7855EFFB7865EFFB7875FFFB88861FFB98862FFB98A
          63FFB98A63FFB98A64FFB98A63FFB98A64FFB98963FFB98963FFB98862FFB888
          61FFB7875FFFB7875FFFA86D3DFFA05F2BFFA05F2BFFA05F2BFF9A5926FF502E
          12980000000F0000000500000001000000000000000000000000000000000000
          000000000000000000000000000100000005140C052D6B3D1ABC9B5A27FF9F5E
          29FF9F5E29FFA0602CFFAC7446FFB37F55FFB48158FFB48259FFB6845BFFB786
          5EFFB78760FFB88862FFB98962FFBA8B65FFBA8B65FFBB8D68FFBB8D68FFBB8D
          69FFBB8D69FFBC8E69FFBC8D69FFBC8E69FFBC8E69FFBC8E69FFBB8D67FFBB8D
          67FFBA8B66FFB27E54FFA1612DFF9F5E2AFF9F5E29FF9B5A27FF6A3D19BF150C
          0533000000070000000200000000000000000000000000000000000000000000
          0000000000000000000000000000000000020000000629180A4F7E4A1FD89B5B
          28FF9E5D29FF9E5D29FFA56938FFB38157FFB6855EFFB78660FFB88862FFB98A
          65FFBA8B66FFBB8D68FFBC8F6AFFBC8F6BFFBC906CFFBD916EFFBD906DFFBE92
          6FFFBE9370FFBE926FFFBE9370FFBE9370FFBE9370FFBE926EFFBE926FFFBD91
          6EFFBB8D67FFA86D40FF9E5D28FF9E5D29FF9B5B28FF7F4A20DA29180A550000
          000A000000030000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000010000000200000006331F0D5F844F
          22E19B5B28FF9D5C28FF9D5C28FFA97043FFB88964FFB98B65FFBA8D67FFBB8E
          6AFFBC906CFFBD926FFFBE9270FFBF9472FFBF9573FFC09673FFC09674FFC097
          76FFC19776FFC19776FFC19776FFC19877FFC09776FFC09776FFC09776FFC096
          74FFAD774BFF9D5C28FF9D5C28FF9B5B28FF844F23E2331E0D650000000A0000
          0004000000010000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000010000000200000006341F
          0E5F814C23D89B5B28FF9C5B27FF9C5C28FFAB7347FFBB8E6AFFBD926EFFBE94
          72FFBF9673FFC09775FFC09877FFC29A79FFC29B7AFFC29B7BFFC39B7CFFC39C
          7DFFC49D7DFFC49D7DFFC49D7EFFC49D7EFFC49D7DFFC39D7DFFC29979FFAF7B
          51FF9D5C29FF9C5B27FF9B5C28FF804C23D9341F0E640000000B000000040000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000020000
          00062A190C4D6E421FBA9B5C2AFF9B5B27FF9B5B26FFA97243FFB88A65FFC198
          78FFC19A79FFC39C7CFFC39D7EFFC49E7FFFC59F80FFC5A082FFC6A183FFC6A1
          84FFC6A184FFC6A184FFC7A284FFC7A284FFC7A284FFBD9270FFAB754AFF9B5A
          26FF9B5B27FF9B5C2AFF6E421FBD2A190C520000000900000004000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          000200000005160D062B5434198F935A2BF19B5C2AFF9A5B27FFA16534FFAF7C
          53FFBD9371FFC5A183FFC6A285FFC7A386FFC8A488FFC8A589FFC9A68AFFC9A6
          8BFFC9A78BFFC9A78BFFCAA78CFFC1997AFFB2815AFFA26737FF9A5A26FF9B5C
          2AFF94592BF255341993160D062F000000070000000300000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000100000003000000062E1D0F52673F21AA9B5F2EFC9B5D2BFF9A5B
          28FF9E6130FFA97447FFB4855FFFBD9472FFC5A082FFC9A88CFFCAA98EFFC6A2
          85FFBF9776FFB68863FFAB764BFF9F6131FF9A5B28FF9B5D2BFF9B5F2EFC673F
          20AC2F1D0F560000000900000005000000020000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000010000000200000004000000062E1D0F505C3A1E998955
          2BDD9E6130FF9C5E2DFF9B5C2AFF9A5A28FF995926FF985924FF985824FF9959
          26FF9A5A27FF9B5D2AFF9C5E2CFF9E6130FF89552BDD5C3A1E9B2E1D0F530000
          0009000000050000000300000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000010000000200000003000000050F0A
          051E3220115651341C866D4525B082532DD1935E33E99E6537F99E6537F9945E
          33EA82532DD16D4525B052341C87322011570F0A052100000007000000050000
          0003000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000010000
          0002000000030000000400000005000000060000000700000007000000070000
          0007000000070000000600000005000000040000000300000002000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000001000000010000000100000002000000020000
          0001000000010000000100000001000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36240000424D3624000000000000360000002800000030000000300000000100
          2000000000000024000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000010000
          0002000000040000000600000008000000090000000A0000000B0000000B0000
          000A0000000A0000000800000006000000040000000200000001000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000010000000200000005000000080000
          000F000000160000001D00000022000000270000002B0000002D0000002D0000
          002B00000027000000230000001D000000160000000F00000009000000050000
          0002000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000001000000050000000B00000015000000200E0E
          0E422E2E2E784B4B4BA4646464C67A7A7ADF888888F1939393FB939393FB8888
          88F1797979DF646464C64A4A4AA42D2D2D7A0E0E0E4300000022000000160000
          000C000000060000000200000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000100000001000000040000000A000000150000002528282871525252B27D7D
          7DE79F9F9FFFABABABFFB6B6B6FFBEBEBEFFC7C7C7FFCCCCCCFFCDCDCDFFC8C8
          C8FFC2C2C2FFB9B9B9FFADADADFFA0A0A0FF7C7C7CE7515151B4282828730000
          0027000000170000000B00000004000000010000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          000200000007000000100000002128282872575757C0919191FDA1A1A1FFB0B0
          B0FFBCBCBCFFBEBEBEFFC1C1C1FFC2C2C2FFC4C4C4FFC4C4C4FFC5C5C5FFC5C5
          C5FFC5C5C5FFC4C4C4FFC4C4C4FFC3C3C3FFB5B5B5FFA5A5A5FF919191FD5757
          57C2272727750000002400000012000000080000000200000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000030000
          00090000001612121248454545A8818181F5969696FFA6A6A6FFACACACFFB0B0
          B0FFB3B3B3FFB6B6B6FFB8B8B8FFB9B9B9FFBBBBBBFFBCBCBCFFBDBDBDFFBDBD
          BDFFBCBCBCFFBBBBBBFFBBBBBBFFB9B9B9FFB8B8B8FFB6B6B6FFAFAFAFFF9B9B
          9BFF828282F6444444AB1111114C000000190000000A00000003000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000001000000030000000A0000
          001A21212169575757CB858585FF929292FF9B9B9BFF9F9F9FFFA4A4A4FFA6A6
          A6FFAAAAAAFFACACACFFAEAEAEFFAFAFAFFFB2B2B2FFB3B3B3FFB3B3B3FFB3B3
          B3FFB3B3B3FFB3B3B3FFB1B1B1FFB0B0B0FFAFAFAFFFACACACFFA9A9A9FFA7A7
          A7FF9D9D9DFF898989FF555555CE2020206D0000001D0000000B000000030000
          0001000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000001000000030000000A0000001A2727
          2778616161E17E7E7EFF898989FF8D8D8DFF919191FF969696FF9A9A9AFF9D9D
          9DFFA0A0A0FFA2A2A2FFA4A4A4FFA6A6A6FFA8A8A8FFA8A8A8FFA9A9A9FFA9A9
          A9FFA9A9A9FFA8A8A8FFA8A8A8FFA6A6A6FFA4A4A4FFA2A2A2FFA0A0A0FF9D9D
          9DFF9A9A9AFF969696FF858585FF626262E32626267D0000001E0000000B0000
          0003000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000001000000020000000800000018252525786060
          60E8747474FF797979FF7F7F7FFF838383FF878787FF8B8B8BFF8F8F8FFF9292
          92FF959595FF989898FF999999FF9C9C9CFF9C9C9CFF9F9F9FFF9F9F9FFF9F9F
          9FFF9E9E9EFF9E9E9EFF9D9D9DFF9C9C9CFF9A9A9AFF989898FF959595FF9292
          92FF8F8F8FFF8B8B8BFF878787FF7C7C7CFF606060EA2424247C0000001B0000
          000A000000020000000100000000000000000000000000000000000000000000
          000000000000000000000000000100000006000000141B1B1B67565656E16969
          69FF6C6C6CFF717171FF767676FF7A7A7AFF7E7E7EFF808080FF858585FF8888
          88FF8B8B8BFF8D8D8DFF8F8F8FFF919191FF929292FF939393FF939393FF9393
          93FF939393FF939393FF929292FF919191FF8F8F8FFF8D8D8DFF8B8B8BFF8787
          87FF848484FF818181FF7D7D7DFF797979FF717171FF565656E21B1B1B6C0000
          0017000000070000000100000000000000000000000000000000000000000000
          00000000000000000001000000040000000E0D0D0D43454545C9606060FF6161
          61FF646464FF666666FF6C6C6CFF6F6F6FFF737373FF767676FF7A7A7AFF7D7D
          7DFF7E7E7EFF828282FF848484FF868686FF878787FF888888FF888888FF8888
          88FF888888FF888888FF878787FF868686FF848484FF828282FF808080FF7D7D
          7DFF7A7A7AFF777777FF737373FF6F6F6FFF6B6B6BFF656565FF444444CC0D0D
          0D48000000100000000400000001000000000000000000000000000000000000
          00000000000000000001000000080000001B323232A35A5A5AFF585858FF5B5B
          5BFF5E5E5EFF616161FF636363FF666666FF696969FF6C6C6CFF6F6F6FFF7373
          73FF757575FF777777FF797979FF7B7B7BFF7C7C7CFF7D7D7DFF7D7D7DFF7E7E
          7EFF7D7D7DFF7D7D7DFF7C7C7CFF7B7B7BFF7A7A7AFF777777FF757575FF7373
          73FF6F6F6FFF6D6D6DFF686868FF676767FF636363FF616161FF5D5D5DFF3131
          31A8000000200000000A00000001000000000000000000000000000000000000
          00000000000100000004000000101919196A525252F5515151FF525252FF5555
          55FF585858FF5A5A5AFF5D5D5DFF5F5F5FFF606060FF636363FF666666FF6868
          68FF6B6B6BFF6D6D6DFF6F6F6FFF6F6F6FFF717171FF737373FF737373FF7373
          73FF737373FF737373FF717171FF707070FF6F6F6FFF6D6D6DFF6B6B6BFF6868
          68FF666666FF626262FF616161FF5F5F5FFF5D5D5DFF5A5A5AFF585858FF5252
          52F5191919700000001400000005000000010000000000000000000000000000
          000000000001000000080000001C373737BA4D4D4DFF494949FF4C4C4CFF4E4E
          4EFF515151FF535353FF555555FF575757FF595959FF5B5B5BFF5D5D5DFF5E5E
          5EFF606060FF636363FF646464FF656565FF666666FF676767FF676767FF6767
          67FF676767FF676767FF666666FF656565FF646464FF626262FF606060FF5F5F
          5FFF5D5D5DFF5B5B5BFF5A5A5AFF585858FF565656FF535353FF515151FF5151
          51FF363636BE000000220000000A000000010000000000000000000000000000
          0001000000030000000F181818664D4D4DFD434343FF434343FF454545FF4848
          48FF4A4A4AFF4B4B4BFF4E4E4EFF505050FF525252FF545454FF565656FF5757
          57FF585858FF595959FF595959FF5B5B5BFF5C5C5CFF5C5C5CFF5C5C5CFF5D5D
          5DFF5D5D5DFF5D5D5DFF5B5B5BFF5B5B5BFF5A5A5AFF595959FF585858FF5858
          58FF565656FF555555FF525252FF505050FF4E4E4EFF4D4D4DFF4A4A4AFF4949
          49FF4E4E4EFD1717176D00000013000000040000000100000000000000000000
          000100000006000000162F2F2FA9454545FF3C3C3CFF3E3E3EFF3F3F3FFF4141
          41FF434343FF454545FF474747FF494949FF4A4A4AFF4C4C4CFF4D4D4DFF4E4E
          4EFF505050FF515151FF525252FF535353FF545454FF545454FF545454FF5555
          55FF555555FF555555FF535353FF535353FF525252FF515151FF515151FF4F4F
          4FFF4D4D4DFF4C4C4CFF4B4B4BFF494949FF484848FF464646FF444444FF4242
          42FF494949FF2E2E2EAF0000001C000000070000000100000000000000000000
          00010000000908080834444444E33D3D3DFF3A3A3AFF3B3B3BFF3C3C3CFF3E3E
          3EFF3E3E3EFF3F3F3FFF424242FF434343FF444444FF464646FF484848FF4949
          49FF494949FF4A4A4AFF4B4B4BFF4D4D4DFF4D4D4DFF4D4D4DFF4D4D4DFF4D4D
          4DFF4D4D4DFF4D4D4DFF4D4D4DFF4C4C4CFF4C4C4CFF4C4C4CFF4A4A4AFF4949
          49FF474747FF474747FF464646FF434343FF424242FF424242FF414141FF3F3F
          3FFF414141FF434343E60808083B0000000C0000000200000000000000000000
          00020000000E19191969484848FF353535FF363636FF383838FF393939FF3939
          39FF3B3B3BFF3C3C3CFF3D3D3DFF3D3D3DFF3E3E3EFF3F3F3FFF414141FF4141
          41FF434343FF444444FF454545FF454545FF464646FF464646FF464646FF4646
          46FF464646FF464646FF464646FF454545FF454545FF444444FF444444FF4242
          42FF414141FF414141FF404040FF3F3F3FFF3E3E3EFF3D3D3DFF3D3D3DFF3C3C
          3CFF3B3B3BFF484848FF18181871000000110000000300000000000000000000
          00040000001128282896404040FF313131FF333333FF333333FF343434FF3535
          35FF363636FF373737FF383838FF393939FF3A3A3AFF3B3B3BFF3C3C3CFF3C3C
          3CFF3C3C3CFF3D3D3DFF3D3D3DFF3E3E3EFF3E3E3EFF3F3F3FFF3F3F3FFF3F3F
          3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF3E3E3EFF3E3E3EFF3D3D3DFF3D3D
          3DFF3C3C3CFF3C3C3CFF3C3C3CFF3B3B3BFF3A3A3AFF393939FF383838FF3737
          37FF363636FF414141FF2727279C000000160000000500000000000000000000
          000400000014343434BB3B3B3BFF313131FF313131FF313131FF323232FF3333
          33FF333333FF343434FF353535FF363636FF373737FF373737FF383838FF3939
          39FF393939FF3A3A3AFF3B3B3BFF3B3B3BFF3B3B3BFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3B3B3BFF3B3B3BFF3B3B3BFF3A3A
          3AFF393939FF393939FF383838FF373737FF373737FF363636FF353535FF3434
          34FF333333FF3C3C3CFF343434BF0000001A0000000600000000000000000000
          0005000000163E3E3ED8363636FF303030FF303030FF303030FF303030FF3030
          30FF313131FF323232FF323232FF323232FF333333FF343434FF353535FF3535
          35FF363636FF363636FF363636FF363636FF373737FF373737FF373737FF3737
          37FF373737FF373737FF373737FF373737FF373737FF363636FF363636FF3636
          36FF363636FF353535FF353535FF343434FF333333FF323232FF323232FF3232
          32FF313131FF363636FF3E3E3EDB0000001D0000000700000000000000000000
          000500000017464646ED323232FF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F
          2FFF2F2F2FFF2F2F2FFF2F2F2FFF303030FF303030FF313131FF313131FF3131
          31FF323232FF323232FF333333FF343434FF343434FF363636FF363636FF3636
          36FF363636FF363636FF363636FF363636FF353535FF343434FF333333FF3232
          32FF323232FF313131FF313131FF313131FF303030FF303030FF2F2F2FFF2F2F
          2FFF2F2F2FFF323232FF444444EE0000001F0000000700000000000000000000
          0005000000174A4A4AFA2E2E2EFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D
          2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF2E2E2EFF2E2E
          2EFF303030FF303030FF313131FF323232FF323232FF333333FF333333FF3333
          33FF333333FF343434FF343434FF343434FF343434FF333333FF313131FF2E2E
          2EFF2E2E2EFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D
          2DFF2D2D2DFF2E2E2EFF484848FB0000001F0000000800000000000000000000
          000500000016494949FA2D2D2DFF2C2C2CFF2C2C2CFF2C2C2CFF2C2C2CFF2C2C
          2CFF2C2C2CFF2C2C2CFF2C2C2CFF2C2C2CFF2D2D2DFF2E2E2EFF2E2E2EFF2F2F
          2FFF303030FF313131FF313131FF323232FF323232FF333333FF333333FF3333
          33FF333333FF333333FF333333FF333333FF333333FF333333FF333333FF2F2F
          2FFF2C2C2CFF2C2C2CFF2C2C2CFF2C2C2CFF2C2C2CFF2C2C2CFF2C2C2CFF2C2C
          2CFF2C2C2CFF2D2D2DFF474747FB0000001E0000000700000000000000000000
          000500000014434343EC2F2F2FFF2C2C2CFF2C2C2CFF2C2C2CFF2C2C2CFF2C2C
          2CFF2C2C2CFF2C2C2CFF2C2C2CFF2E2E2EFF303030FF303030FF313131FF3232
          32FF333333FF333333FF333333FF343434FF353535FF353535FF363636FF3636
          36FF363636FF363636FF363636FF373737FF363636FF363636FF363636FF3636
          36FF313131FF2C2C2CFF2C2C2CFF2C2C2CFF2C2C2CFF2C2C2CFF2C2C2CFF2C2C
          2CFF2C2C2CFF2F2F2FFF414141EE0000001C0000000700000000000000000000
          0004000000113A3A3AD6313131FF2B2B2BFF2B2B2BFF2B2B2BFF2B2B2BFF2B2B
          2BFF2B2B2BFF2B2B2BFF2D2D2DFF303030FF323232FF323232FF333333FF3333
          33FF343434FF353535FF363636FF373737FF373737FF373737FF373737FF3838
          38FF383838FF383838FF383838FF383838FF383838FF383838FF383838FF3838
          38FF373737FF2F2F2FFF2B2B2BFF2B2B2BFF2B2B2BFF2B2B2BFF2B2B2BFF2B2B
          2BFF2B2B2BFF313131FF393939D9000000190000000600000000000000000000
          00030000000F2F2F2FB7333333FF292929FF292929FF292929FF292929FF2929
          29FF292929FF2A2A2AFF2E2E2EFF313131FF323232FF333333FF343434FF3535
          35FF363636FF363636FF373737FF373737FF383838FF393939FF393939FF3A3A
          3AFF3A3A3AFF3A3A3AFF3A3A3AFF3A3A3AFF3A3A3AFF3A3A3AFF3A3A3AFF3A3A
          3AFF393939FF353535FF2A2A2AFF292929FF292929FF292929FF292929FF2929
          29FF292929FF333333FF2F2F2FBC000000150000000500000000000000000000
          00030000000C2323238F373737FF292929FF292929FF292929FF292929FF2929
          29FF292929FF2C2C2CFF333333FF343434FF353535FF363636FF373737FF3838
          38FF393939FF3A3A3AFF3B3B3BFF3B3B3BFF3B3B3BFF3C3C3CFF3C3C3CFF3D3D
          3DFF3D3D3DFF3D3D3DFF3E3E3EFF3E3E3EFF3E3E3EFF3D3D3DFF3D3D3DFF3D3D
          3DFF3C3C3CFF3C3C3CFF303030FF292929FF292929FF292929FF292929FF2929
          29FF292929FF373737FF22222295000000110000000400000000000000000000
          000100000008161616603C3C3CFF282828FF282828FF282828FF282828FF2828
          28FF282828FF303030FF353535FF363636FF373737FF383838FF3A3A3AFF3B3B
          3BFF3B3B3BFF3C3C3CFF3D3D3DFF3E3E3EFF3F3F3FFF3F3F3FFF404040FF4040
          40FF404040FF404040FF404040FF404040FF404040FF404040FF404040FF4040
          40FF404040FF3F3F3FFF353535FF282828FF282828FF282828FF282828FF2828
          28FF282828FF3B3B3BFF151515670000000D0000000200000000000000000000
          00010000000506060627373737DF2C2C2CFF272727FF272727FF272727FF2727
          27FF272727FF333333FF373737FF383838FF3A3A3AFF3A3A3AFF3C3C3CFF3D3D
          3DFF3E3E3EFF3F3F3FFF404040FF404040FF414141FF424242FF424242FF4343
          43FF434343FF444444FF444444FF444444FF444444FF444444FF434343FF4343
          43FF424242FF424242FF3C3C3CFF272727FF272727FF272727FF272727FF2727
          27FF2C2C2CFF373737E206060630000000080000000100000000000000000000
          0000000000030000000C2626269E333333FF262626FF262626FF262626FF2626
          26FF262626FF373737FF3A3A3AFF3A3A3AFF3C3C3CFF3D3D3DFF3F3F3FFF4040
          40FF414141FF424242FF434343FF444444FF454545FF454545FF454545FF4646
          46FF464646FF474747FF474747FF474747FF474747FF474747FF464646FF4646
          46FF454545FF454545FF434343FF262626FF262626FF262626FF262626FF2626
          26FF323232FF252525A400000012000000050000000000000000000000000000
          00000000000200000007131313573A3A3AFD272727FF252525FF252525FF2525
          25FF252525FF3A3A3AFF3C3C3CFF3D3D3DFF3F3F3FFF404040FF414141FF4343
          43FF444444FF454545FF464646FF474747FF484848FF494949FF494949FF4A4A
          4AFF4A4A4AFF4B4B4BFF4B4B4BFF4B4B4BFF4B4B4BFF4B4B4BFF4A4A4AFF4A4A
          4AFF494949FF494949FF464646FF252525FF252525FF252525FF252525FF2727
          27FF383838FD1212125E0000000B000000020000000000000000000000000000
          000000000000000000040000000C2C2C2CAE323232FF252525FF252525FF2525
          25FF252525FF3A3A3AFF404040FF414141FF434343FF444444FF454545FF4747
          47FF494949FF4A4A4AFF4C4C4CFF4D4D4DFF4E4E4EFF4F4F4FFF4F4F4FFF5050
          50FF515151FF515151FF515151FF515151FF515151FF515151FF515151FF4F4F
          4FFF4E4E4EFF4D4D4DFF454545FF252525FF252525FF252525FF252525FF3131
          31FF2B2B2BB30000001300000006000000010000000000000000000000000000
          0000000000000000000200000006151515573B3B3BF22A2A2AFF242424FF2424
          24FF242424FF353535FF424242FF444444FF464646FF494949FF4B4B4BFF4C4C
          4CFF4E4E4EFF4F4F4FFF525252FF525252FF545454FF555555FF565656FF5656
          56FF575757FF575757FF575757FF585858FF575757FF565656FF565656FF5454
          54FF545454FF525252FF3F3F3FFF242424FF242424FF242424FF2A2A2AFF3A3A
          3AF31515155E0000000A00000003000000000000000000000000000000000000
          00000000000000000000000000030000000A292929933A3A3AFF232323FF2323
          23FF232323FF2F2F2FFF474747FF4A4A4AFF4C4C4CFF4E4E4EFF515151FF5252
          52FF545454FF565656FF585858FF585858FF595959FF5B5B5BFF5C5C5CFF5D5D
          5DFF5D5D5DFF5E5E5EFF5E5E5EFF5E5E5EFF5D5D5DFF5D5D5DFF5C5C5CFF5B5B
          5BFF595959FF585858FF363636FF232323FF232323FF232323FF393939FF2828
          28980000000F0000000500000001000000000000000000000000000000000000
          0000000000000000000000000001000000050B0B0B2D383838BC363636FF2323
          23FF232323FF262626FF414141FF505050FF525252FF545454FF575757FF5959
          59FF5B5B5BFF5D5D5DFF5E5E5EFF606060FF616161FF636363FF636363FF6464
          64FF646464FF656565FF656565FF656565FF656565FF656565FF636363FF6363
          63FF616161FF4F4F4FFF272727FF232323FF232323FF363636FF373737BF0B0B
          0B33000000070000000200000000000000000000000000000000000000000000
          000000000000000000000000000000000002000000061818184F424242D83535
          35FF222222FF222222FF323232FF535353FF595959FF5A5A5AFF5D5D5DFF6060
          60FF616161FF636363FF666666FF666666FF676767FF696969FF696969FF6A6A
          6AFF6C6C6CFF6B6B6BFF6C6C6CFF6C6C6CFF6C6C6CFF6A6A6AFF6A6A6AFF6969
          69FF636363FF393939FF222222FF222222FF343434FF404040DA171717550000
          000A000000030000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000100000002000000061F1F1F5F4747
          47E1363636FF212121FF212121FF3D3D3DFF5F5F5FFF616161FF636363FF6565
          65FF686868FF6A6A6AFF6B6B6BFF6E6E6EFF6E6E6EFF6F6F6FFF707070FF7171
          71FF727272FF727272FF727272FF737373FF717171FF717171FF717171FF6F6F
          6FFF464646FF212121FF212121FF353535FF464646E21E1E1E650000000A0000
          0004000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000002000000062222
          225F4A4A4AD83C3C3CFF212121FF222222FF424242FF666666FF6A6A6AFF6E6E
          6EFF6F6F6FFF717171FF737373FF757575FF767676FF777777FF787878FF7979
          79FF7A7A7AFF7A7A7AFF7B7B7BFF7B7B7BFF7A7A7AFF797979FF757575FF4C4C
          4CFF232323FF212121FF3C3C3CFF484848D9212121640000000B000000040000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000020000
          00061C1C1C4D494949BA4A4A4AFF2D2D2DFF212121FF3F3F3FFF616161FF7575
          75FF767676FF797979FF7B7B7BFF7C7C7CFF7D7D7DFF7F7F7FFF808080FF8181
          81FF818181FF818181FF828282FF828282FF828282FF6D6D6DFF464646FF2121
          21FF2D2D2DFF484848FF494949BD1C1C1C520000000900000004000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          0002000000050F0F0F2B3B3B3B8F585858F1424242FF282828FF303030FF4F4F
          4FFF6E6E6EFF808080FF838383FF838383FF858585FF868686FF888888FF8989
          89FF898989FF898989FF898989FF777777FF565656FF323232FF282828FF4141
          41FF565656F23B3B3B930F0F0F2F000000070000000300000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000010000000300000006222222524B4B4BAA5E5E5EFC474747FF3030
          30FF2B2B2BFF444444FF5B5B5BFF6F6F6FFF7F7F7FFF8A8A8AFF8C8C8CFF8282
          82FF737373FF5F5F5FFF474747FF2C2C2CFF2F2F2FFF464646FF5E5E5EFC4A4A
          4AAC222222560000000900000005000000020000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000002000000040000000623232350474747996464
          64DD606060FF4E4E4EFF3E3E3EFF323232FF282828FF222222FF222222FF2828
          28FF323232FF3E3E3EFF4D4D4DFF5F5F5FFF636363DD4747479B232323530000
          0009000000050000000300000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000010000000200000003000000050C0C
          0C1E2828285640404086565656B0666666D1737373E97B7B7BF97B7B7BF97373
          73EA666666D1565656B040404087282828570C0C0C2100000007000000050000
          0003000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000010000
          0002000000030000000400000005000000060000000700000007000000070000
          0007000000070000000600000005000000040000000300000002000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000001000000010000000100000002000000020000
          0001000000010000000100000001000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36240000424D3624000000000000360000002800000030000000300000000100
          2000000000000024000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0004000000040000000400000004000000040000000400000004000000040000
          0004000000040000000400000004000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000400000004000000080000
          0010000000180000001F00000023000000270000002B0000002B000000270000
          00270000001F0000001800000010000000080000000400000004000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000400000008000000140000001F000000390000
          004F00000063000000700000007E000000860000008B0000008B000000860000
          007E00000073000000630000004F000000390000002300000014000000080000
          0004000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000400000004000000140000002B0000004C0000006A3A3837B3908F
          8DECB8B7B6FBC6C6C6FFCDCDCDFFD1D1D1FFD3D3D3FFD3D3D3FFD1D1D1FFCDCD
          CDFFC6C6C6FFB8B7B6FB8F8C8BED393837B5000000700000004F0000002E0000
          0018000000080000000400000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000040000
          000400000010000000270000004C00000078868482E7BEBDBDFDCECECEFFDADA
          DAFFE0E0E0FFE6E6E6FFEAEAEAFFEEEEEEFFF0F0F0FFF1F1F1FFEFEFEFFFEBEB
          EBFFE7E7E7FFE1E1E1FFDBDBDBFFCECECEFFBDBCBCFE858381E90000007E0000
          00510000002B0000001000000004000000040000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000004000000080000
          001C0000003B0000006D878482E8C1C1C1FFD5D5D5FFDEDEDEFFE6E6E6FFEDED
          EDFFEDEDEDFFEEEEEEFFEEEEEEFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEF
          EFFFEFEFEFFFEFEFEFFFEFEFEFFFE8E8E8FFDFDFDFFFD5D5D5FFC1C0C0FF8381
          80EA00000075000000420000001F000000080000000400000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000040000000C000000230000
          004F4C4947BCB5B4B3FCD1D1D1FFDBDBDBFFE5E5E5FFE9E9E9FFE9E9E9FFEAEA
          EAFFEBEBEBFFEBEBEBFFECECECFFECECECFFEDEDEDFFEDEDEDFFEDEDEDFFEDED
          EDFFECECECFFECECECFFECECECFFEBEBEBFFEBEBEBFFE7E7E7FFDDDDDDFFD1D1
          D1FFB3B2B2FD4B4846C200000057000000270000000C00000004000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000040000000C000000270000005B7976
          74E1C0C0C0FFD4D4D4FFDEDEDEFFE3E3E3FFE5E5E5FFE6E6E6FFE7E7E7FFE8E8
          E8FFE8E8E8FFE8E8E8FFE9E9E9FFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEA
          EAFFEAEAEAFFE9E9E9FFE9E9E9FFE9E9E9FFE8E8E8FFE7E7E7FFE7E7E7FFE1E1
          E1FFD6D6D6FFC0BFBFFF767371E4000000630000002B0000000C000000040000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000040000000C000000270000005B878482ECC5C5
          C5FFD4D4D4FFDEDEDEFFDFDFDFFFE0E0E0FFE2E2E2FFE3E3E3FFE4E4E4FFE5E5
          E5FFE5E5E5FFE6E6E6FFE6E6E6FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7
          E7FFE7E7E7FFE7E7E7FFE6E6E6FFE6E6E6FFE5E5E5FFE5E5E5FFE4E4E4FFE3E3
          E3FFE2E2E2FFD6D6D6FFC5C5C5FF83817FEE000000660000002B0000000C0000
          0004000000000000000000000000000000000000000000000000000000000000
          00000000000000000004000000080000001F0000005482807EECC4C4C4FFD2D2
          D2FFDADADAFFDBDBDBFFDCDCDCFFDEDEDEFFDFDFDFFFE0E0E0FFE0E0E0FFE1E1
          E1FFE2E2E2FFE3E3E3FFE3E3E3FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4
          E4FFE4E4E4FFE4E4E4FFE3E3E3FFE3E3E3FFE2E2E2FFE2E2E2FFE1E1E1FFE0E0
          E0FFDEDEDEFFDEDEDEFFD4D4D4FFC4C4C4FF807D7BEE0000005E000000270000
          0008000000040000000000000000000000000000000000000000000000000000
          0000000000000000000400000018000000496E6A69E0C0C0C0FFCECECEFFD6D6
          D6FFD8D8D8FFD9D9D9FFDADADAFFDBDBDBFFDCDCDCFFDDDDDDFFDEDEDEFFDEDE
          DEFFDFDFDFFFDFDFDFFFE0E0E0FFE0E0E0FFE1E1E1FFE1E1E1FFE1E1E1FFE1E1
          E1FFE1E1E1FFE0E0E0FFE0E0E0FFDFDFDFFFDFDFDFFFDEDEDEFFDEDEDEFFDDDD
          DDFFDCDCDCFFDBDBDBFFDADADAFFD1D1D1FFC0C0C0FF6B6966E4000000510000
          001C000000040000000000000000000000000000000000000000000000000000
          0000000000040000001000000035413E3BB5AAAAAAFFCACACAFFD3D3D3FFD4D4
          D4FFD4D4D4FFD6D6D6FFD7D7D7FFD8D8D8FFD9D9D9FFDADADAFFDADADAFFDBDB
          DBFFDCDCDCFFDCDCDCFFDDDDDDFFDDDDDDFFDDDDDDFFDDDDDDFFDDDDDDFFDDDD
          DDFFDDDDDDFFDDDDDDFFDDDDDDFFDCDCDCFFDCDCDCFFDBDBDBFFDBDBDBFFDADA
          DAFFD9D9D9FFD8D8D8FFD7D7D7FFD6D6D6FFCCCCCCFFA7A6A6FF3F3C3ABC0000
          003B000000100000000400000000000000000000000000000000000000000000
          0000000000040000001F0000005E949291FBC4C4C4FFD0D0D0FFD1D1D1FFD1D1
          D1FFD2D2D2FFD3D3D3FFD4D4D4FFD4D4D4FFD5D5D5FFD6D6D6FFD7D7D7FFD8D8
          D8FFD9D9D9FFD9D9D9FFD9D9D9FFD9D9D9FFDADADAFFDADADAFFDADADAFFDADA
          DAFFDADADAFFD9D9D9FFD9D9D9FFD9D9D9FFD9D9D9FFD8D8D8FFD7D7D7FFD6D6
          D6FFD6D6D6FFD4D4D4FFD4D4D4FFD3D3D3FFD2D2D2FFC6C6C6FF8F8E8DFC0000
          006A000000270000000400000000000000000000000000000000000000000000
          0004000000100000003B686461E2B9B9B9FFC9C9C9FFCDCDCDFFCECECEFFCFCF
          CFFFCFCFCFFFD0D0D0FFD1D1D1FFD1D1D1FFD2D2D2FFD3D3D3FFD3D3D3FFD4D4
          D4FFD5D5D5FFD5D5D5FFD6D6D6FFD6D6D6FFD7D7D7FFD7D7D7FFD7D7D7FFD7D7
          D7FFD7D7D7FFD6D6D6FFD6D6D6FFD5D5D5FFD5D5D5FFD4D4D4FFD4D4D4FFD3D3
          D3FFD2D2D2FFD2D2D2FFD1D1D1FFD0D0D0FFD0D0D0FFCBCBCBFFB9B9B9FF6361
          5EE6000000490000001400000004000000000000000000000000000000000000
          00040000001F0000005F908F8EFEC2C2C2FFCACACAFFCBCBCBFFCBCBCBFFCCCC
          CCFFCDCDCDFFCDCDCDFFCECECEFFCFCFCFFFCFCFCFFFD0D0D0FFD0D0D0FFD1D1
          D1FFD2D2D2FFD2D2D2FFD2D2D2FFD3D3D3FFD3D3D3FFD3D3D3FFD3D3D3FFD3D3
          D3FFD3D3D3FFD3D3D3FFD2D2D2FFD2D2D2FFD1D1D1FFD1D1D1FFD0D0D0FFD0D0
          D0FFCFCFCFFFCFCFCFFFCECECEFFCECECEFFCDCDCDFFCCCCCCFFC3C3C3FF8D8C
          8CFE000000700000002700000004000000000000000000000000000000040000
          000C000000395E5B57DFABABABFFC6C6C6FFC8C8C8FFC8C8C8FFC9C9C9FFCACA
          CAFFCACACAFFCBCBCBFFCBCBCBFFCCCCCCFFCDCDCDFFCDCDCDFFCDCDCDFFCECE
          CEFFCECECEFFCECECEFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCF
          CFFFCFCFCFFFCFCFCFFFCFCFCFFFCECECEFFCECECEFFCECECEFFCECECEFFCDCD
          CDFFCDCDCDFFCCCCCCFFCBCBCBFFCBCBCBFFCACACAFFCACACAFFC7C7C7FFACAC
          ACFF5B5855E40000004500000010000000040000000000000000000000040000
          00180000004F7F7E7DFCB5B5B5FFC5C5C5FFC6C6C6FFC6C6C6FFC7C7C7FFC7C7
          C7FFC8C8C8FFC8C8C8FFC9C9C9FFC9C9C9FFCACACAFFCACACAFFCBCBCBFFCBCB
          CBFFCBCBCBFFCCCCCCFFCCCCCCFFCCCCCCFFCCCCCCFFCCCCCCFFCCCCCCFFCCCC
          CCFFCCCCCCFFCCCCCCFFCCCCCCFFCCCCCCFFCBCBCBFFCBCBCBFFCBCBCBFFCACA
          CAFFCACACAFFCACACAFFC9C9C9FFC9C9C9FFC8C8C8FFC8C8C8FFC7C7C7FFB8B8
          B8FF7B7A79FD0000005F0000001C000000040000000000000000000000040000
          00232523209A8F8F8FFFBEBEBEFFC3C3C3FFC4C4C4FFC4C4C4FFC5C5C5FFC5C5
          C5FFC5C5C5FFC6C6C6FFC6C6C6FFC6C6C6FFC7C7C7FFC8C8C8FFC8C8C8FFC8C8
          C8FFC8C8C8FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9
          C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC8C8C8FFC8C8C8FFC7C7
          C7FFC7C7C7FFC7C7C7FFC6C6C6FFC6C6C6FFC6C6C6FFC5C5C5FFC5C5C5FFC1C1
          C1FF8E8E8EFF25221FA70000002E000000080000000000000000000000090000
          0035585450E19D9D9DFFC1C1C1FFC2C2C2FFC2C2C2FFC2C2C2FFC3C3C3FFC3C3
          C3FFC3C3C3FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC5C5C5FFC5C5C5FFC6C6
          C6FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6
          C6FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6C6FFC5C5C5FFC5C5
          C5FFC5C5C5FFC5C5C5FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC3C3C3FFC3C3
          C3FF9E9E9EFF54514EE70000003E0000000C0000000000000000000000110000
          003E706D6BF8A4A4A4FFBFBFBFFFC0C0C0FFC1C1C1FFC1C1C1FFC1C1C1FFC1C1
          C1FFC2C2C2FFC2C2C2FFC2C2C2FFC3C3C3FFC3C3C3FFC3C3C3FFC3C3C3FFC3C3
          C3FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4
          C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC3C3
          C3FFC3C3C3FFC3C3C3FFC3C3C3FFC3C3C3FFC2C2C2FFC2C2C2FFC2C2C2FFC1C1
          C1FFA6A6A6FF6B6968FA0000004F000000140000000000000000000000110000
          0049787877FEAAAAAAFFBCBCBCFFBCBCBCFFBCBCBCFFBEBEBEFFBEBEBEFFBFBF
          BFFFBFBFBFFFC0C0C0FFC0C0C0FFC0C0C0FFC1C1C1FFC1C1C1FFC1C1C1FFC1C1
          C1FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2
          C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC1C1C1FFC1C1
          C1FFC1C1C1FFC1C1C1FFC1C1C1FFC1C1C1FFC0C0C0FFBFBFBFFFBFBFBFFFBFBF
          BFFFACACACFF777675FE0000005B000000180000000000000000000000150000
          004F7E7E7EFFAFAFAFFFB9B9B9FFB9B9B9FFB9B9B9FFB9B9B9FFB9B9B9FFBBBB
          BBFFBBBBBBFFBBBBBBFFBCBCBCFFBCBCBCFFBEBEBEFFBEBEBEFFBEBEBEFFBFBF
          BFFFBFBFBFFFBFBFBFFFBFBFBFFFC0C0C0FFC0C0C0FFBFBFBFFFC0C0C0FFC0C0
          C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFBFBFBFFFBFBFBFFFBFBFBFFFBFBF
          BFFFBEBEBEFFBEBEBEFFBEBEBEFFBCBCBCFFBCBCBCFFBBBBBBFFBBBBBBFFBBBB
          BBFFAFAFAFFF7D7D7DFF000000630000001C0000000000000000000000150000
          0051828282FFB2B2B2FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6
          B6FFB6B6B6FFB6B6B6FFB8B8B8FFB8B8B8FFB8B8B8FFB9B9B9FFB9B9B9FFB9B9
          B9FFB9B9B9FFBBBBBBFFBBBBBBFFBBBBBBFFBCBCBCFFBCBCBCFFBCBCBCFFBCBC
          BCFFBEBEBEFFBEBEBEFFBCBCBCFFBCBCBCFFBBBBBBFFBBBBBBFFB9B9B9FFB9B9
          B9FFB8B8B8FFB9B9B9FFB8B8B8FFB8B8B8FFB8B8B8FFB6B6B6FFB6B6B6FFB6B6
          B6FFB2B2B2FF818181FF000000690000001C0000000000000000000000150000
          0051858585FFB2B2B2FFB3B3B3FFB3B3B3FFB3B3B3FFB3B3B3FFB3B3B3FFB3B3
          B3FFB3B3B3FFB3B3B3FFB3B3B3FFB3B3B3FFB3B3B3FFB5B5B5FFB5B5B5FFB6B6
          B6FFB6B6B6FFB8B8B8FFB8B8B8FFB8B8B8FFB9B9B9FFB9B9B9FFB9B9B9FFB9B9
          B9FFBBBBBBFFBBBBBBFFBBBBBBFFB9B9B9FFB9B9B9FFB8B8B8FFB5B5B5FFB5B5
          B5FFB3B3B3FFB3B3B3FFB3B3B3FFB3B3B3FFB3B3B3FFB3B3B3FFB3B3B3FFB3B3
          B3FFB2B2B2FF848484FF000000690000001F0000000000000000000000150000
          004F858585FFADADADFFAFAFAFFFAFAFAFFFB0B0B0FFAFAFAFFFAFAFAFFFAFAF
          AFFFAFAFAFFFAFAFAFFFAFAFAFFFB0B0B0FFB2B2B2FFB2B2B2FFB3B3B3FFB3B3
          B3FFB5B5B5FFB5B5B5FFB5B5B5FFB5B5B5FFB6B6B6FFB5B5B5FFB5B5B5FFB6B6
          B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB3B3B3FFB0B0
          B0FFAFAFAFFFAFAFAFFFB0B0B0FFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAF
          AFFFAFAFAFFF848484FF000000660000001C0000000000000000000000150000
          0049838383FFAAAAAAFFADADADFFADADADFFADADADFFADADADFFADADADFFADAD
          ADFFADADADFFADADADFFAFAFAFFFB2B2B2FFB2B2B2FFB2B2B2FFB2B2B2FFB3B3
          B3FFB5B5B5FFB5B5B5FFB5B5B5FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6
          B6FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFB6B6B6FFB8B8B8FFB2B2
          B2FFADADADFFADADADFFADADADFFADADADFFADADADFFADADADFFADADADFFADAD
          ADFFA9A9A9FF818181FF0000005F0000001C0000000000000000000000110000
          003E807F7FFFA3A3A3FFAAAAAAFFAAAAAAFFAAAAAAFFAAAAAAFFAAAAAAFFAAAA
          AAFFAAAAAAFFACACACFFAFAFAFFFB0B0B0FFB2B2B2FFB2B2B2FFB3B3B3FFB3B3
          B3FFB3B3B3FFB5B5B5FFB5B5B5FFB6B6B6FFB8B8B8FFB6B6B6FFB8B8B8FFB8B8
          B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFB5B5
          B5FFAFAFAFFFAAAAAAFFAAAAAAFFAAAAAAFFAAAAAAFFAAAAAAFFAAAAAAFFAAAA
          AAFFA3A3A3FF7D7D7DFF000000570000001800000000000000000000000C0000
          00397A7978FE9B9B9BFFA7A7A7FFA7A7A7FFA7A7A7FFA7A7A7FFA7A7A7FFA7A7
          A7FFA9A9A9FFACACACFFB0B0B0FFB0B0B0FFB2B2B2FFB2B2B2FFB3B3B3FFB3B3
          B3FFB5B5B5FFB5B5B5FFB5B5B5FFB6B6B6FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8
          B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8
          B8FFB3B3B3FFA9A9A9FFA7A7A7FFA7A7A7FFA7A7A7FFA7A7A7FFA7A7A7FFA7A7
          A7FF9B9B9BFF787776FE0000004C0000001400000000000000000000000C0000
          002E6F6C69F6959595FFA4A4A4FFA6A6A6FFA6A6A6FFA4A4A4FFA6A6A6FFA6A6
          A6FFA9A9A9FFB0B0B0FFB0B0B0FFB0B0B0FFB2B2B2FFB3B3B3FFB5B5B5FFB5B5
          B5FFB8B8B8FFB6B6B6FFB6B6B6FFB8B8B8FFB8B8B8FFB9B9B9FFB9B9B9FFB9B9
          B9FFB9B9B9FFBBBBBBFFBBBBBBFFBBBBBBFFBBBBBBFFB9B9B9FFB9B9B9FFB8B8
          B8FFB8B8B8FFADADADFFA6A6A6FFA4A4A4FFA6A6A6FFA6A6A6FFA4A4A4FFA6A6
          A6FF959595FF6C6A68F80000003E000000100000000000000000000000040000
          001F595350D98E8E8EFFA3A3A3FFA3A3A3FFA3A3A3FFA3A3A3FFA3A3A3FFA3A3
          A3FFAAAAAAFFAFAFAFFFB2B2B2FFB2B2B2FFB2B2B2FFB5B5B5FFB5B5B5FFB6B6
          B6FFB8B8B8FFB8B8B8FFB9B9B9FFB9B9B9FFB9B9B9FFBBBBBBFFBBBBBBFFBBBB
          BBFFBBBBBBFFBCBCBCFFBBBBBBFFBCBCBCFFBBBBBBFFBBBBBBFFBBBBBBFFBBBB
          BBFFB9B9B9FFB0B0B0FFA3A3A3FFA3A3A3FFA3A3A3FFA3A3A3FFA3A3A3FFA3A3
          A3FF8E8E8EFF55514DE000000032000000080000000000000000000000040000
          001426221F7E838383FF9B9B9BFFA1A1A1FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0
          A0FFACACACFFB0B0B0FFB2B2B2FFB3B3B3FFB5B5B5FFB6B6B6FFB8B8B8FFB8B8
          B8FFB9B9B9FFB9B9B9FFB9B9B9FFBBBBBBFFBCBCBCFFBCBCBCFFBEBEBEFFBEBE
          BEFFBEBEBEFFBEBEBEFFBEBEBEFFBEBEBEFFBEBEBEFFBEBEBEFFBEBEBEFFBCBC
          BCFFBCBCBCFFB6B6B6FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FF9B9B
          9BFF828282FF24201E910000001F000000040000000000000000000000000000
          000C0000002E767472FA929292FF9D9D9DFF9E9E9EFF9D9D9DFF9E9E9EFF9E9E
          9EFFAFAFAFFFB2B2B2FFB2B2B2FFB5B5B5FFB5B5B5FFB8B8B8FFB6B6B6FFB9B9
          B9FFBBBBBBFFBCBCBCFFBCBCBCFFBEBEBEFFBEBEBEFFBEBEBEFFBFBFBFFFBEBE
          BEFFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFBFBFBFFFBFBFBFFFBEBE
          BEFFBEBEBEFFBCBCBCFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9E9E9EFF9292
          92FF73716FFB0000004200000014000000000000000000000000000000000000
          00080000001C56524DD08A8A8AFF9A9A9AFF9B9B9BFF9B9B9BFF9B9B9BFF9B9B
          9BFFB2B2B2FFB3B3B3FFB5B5B5FFB6B6B6FFB8B8B8FFB9B9B9FFBBBBBBFFBBBB
          BBFFBCBCBCFFBEBEBEFFBFBFBFFFBFBFBFFFC0C0C0FFC0C0C0FFC0C0C0FFC1C1
          C1FFC1C1C1FFC1C1C1FFC1C1C1FFC1C1C1FFC1C1C1FFC0C0C0FFC0C0C0FFC0C0
          C0FFC0C0C0FFBEBEBEFF9B9B9BFF9B9B9BFF9B9B9BFF9B9B9BFF9A9A9AFF8A8A
          8AFF534E4BD70000002B00000008000000000000000000000000000000000000
          0000000000100000002E7A7877FD919191FF9A9A9AFF989898FF9A9A9AFF9A9A
          9AFFAFAFAFFFB5B5B5FFB6B6B6FFB9B9B9FFB9B9B9FFBBBBBBFFBEBEBEFFBFBF
          BFFFBFBFBFFFC1C1C1FFC1C1C1FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2
          C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2
          C2FFC1C1C1FFBBBBBBFF9A9A9AFF989898FF9A9A9AFF9A9A9AFF919191FF7877
          76FE000000450000001800000004000000000000000000000000000000000000
          0000000000080000001859534FD0898989FF949494FF979797FF979797FF9797
          97FFA9A9A9FFB8B8B8FFB9B9B9FFBCBCBCFFBFBFBFFFC0C0C0FFC0C0C0FFC1C1
          C1FFC1C1C1FFC2C2C2FFC3C3C3FFC3C3C3FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4
          C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC3C3C3FFC3C3
          C3FFC3C3C3FFB3B3B3FF979797FF979797FF979797FF949494FF888888FF5550
          4CD7000000270000000C00000000000000000000000000000000000000000000
          0000000000000000000C00000027757270F88C8C8CFF959595FF959595FF9595
          95FFA3A3A3FFBCBCBCFFBEBEBEFFC0C0C0FFC1C1C1FFC2C2C2FFC3C3C3FFC3C3
          C3FFC4C4C4FFC4C4C4FFC5C5C5FFC5C5C5FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6
          C6FFC7C7C7FFC6C6C6FFC7C7C7FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6C6FFC5C5
          C5FFC5C5C5FFA9A9A9FF959595FF959595FF959595FF8C8C8CFF716F6DF90000
          0039000000140000000400000000000000000000000000000000000000000000
          000000000000000000040000001435312D8B7F7F7FFE8E8E8EFF949494FF9494
          94FF979797FFB3B3B3FFC1C1C1FFC2C2C2FFC3C3C3FFC4C4C4FFC5C5C5FFC5C5
          C5FFC6C6C6FFC6C6C6FFC7C7C7FFC7C7C7FFC8C8C8FFC8C8C8FFC8C8C8FFC8C8
          C8FFC9C9C9FFC8C8C8FFC9C9C9FFC9C9C9FFC9C9C9FFC8C8C8FFC8C8C8FFC7C7
          C7FFC1C1C1FF989898FF949494FF949494FF8E8E8EFF7E7E7DFE342F2C970000
          001C000000080000000000000000000000000000000000000000000000000000
          00000000000000000000000000080000001858524EC6878786FF8F8F8FFF9292
          92FF929292FFA3A3A3FFC2C2C2FFC4C4C4FFC5C5C5FFC6C6C6FFC7C7C7FFC7C7
          C7FFC8C8C8FFC9C9C9FFC9C9C9FFC9C9C9FFCACACAFFCACACAFFCBCBCBFFCBCB
          CBFFCBCBCBFFCBCBCBFFCBCBCBFFCBCBCBFFCACACAFFCBCBCBFFCACACAFFC8C8
          C8FFAAAAAAFF929292FF929292FF8F8F8FFF868685FF544F4BCD000000270000
          000C000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000004000000080000001866615CD8898989FF8F8F
          8FFF919191FF919191FFAFAFAFFFC6C6C6FFC7C7C7FFC8C8C8FFC9C9C9FFC9C9
          C9FFCACACAFFCBCBCBFFCCCCCCFFCCCCCCFFCCCCCCFFCCCCCCFFCDCDCDFFCDCD
          CDFFCDCDCDFFCDCDCDFFCDCDCDFFCDCDCDFFCDCDCDFFCDCDCDFFCCCCCCFFB8B8
          B8FF919191FF919191FF8F8F8FFF898989FF635E5BDE00000027000000100000
          0004000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000004000000080000001868635ED88C8C
          8BFF8F8F8FFF8F8F8FFF919191FFB3B3B3FFC9C9C9FFCACACAFFCBCBCBFFCCCC
          CCFFCDCDCDFFCDCDCDFFCECECEFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFD0D0
          D0FFD0D0D0FFD0D0D0FFD0D0D0FFD0D0D0FFD0D0D0FFCECECEFFBEBEBEFF9191
          91FF8F8F8FFF8F8F8FFF8B8B8AFF65605DDD0000002B00000010000000040000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000400000008000000185E58
          53C38A8988FE919191FF8F8F8FFF8E8E8EFFAFAFAFFFC7C7C7FFCECECEFFCECE
          CEFFCFCFCFFFD0D0D0FFD0D0D0FFD1D1D1FFD1D1D1FFD2D2D2FFD2D2D2FFD2D2
          D2FFD2D2D2FFD2D2D2FFD2D2D2FFD2D2D2FFCBCBCBFFB5B5B5FF8E8E8EFF8F8F
          8FFF919191FF888787FE5C5652CA000000230000001000000004000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000004000000080000
          00143B37328684817FF6939393FF919191FF8E8E8EFF9D9D9DFFBFBFBFFFCBCB
          CBFFD2D2D2FFD2D2D2FFD3D3D3FFD3D3D3FFD4D4D4FFD4D4D4FFD4D4D4FFD5D5
          D5FFD5D5D5FFD5D5D5FFCECECEFFC2C2C2FFA0A0A0FF8E8E8EFF919191FF9393
          93FF83807EF83A36328F0000001C0000000C0000000400000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00040000000C0000001867615DCA8F8D8CFC969696FF929292FF8F8F8FFF9898
          98FFB2B2B2FFC4C4C4FFCBCBCBFFD1D1D1FFD5D5D5FFD6D6D6FFD2D2D2FFCDCD
          CDFFC6C6C6FFB6B6B6FF989898FF8F8F8FFF929292FF969696FF8E8D8BFD6660
          5CCE000000230000001400000008000000040000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000400000008000000100000001868625EC78E8B8AF9989898FF9898
          98FF949494FF919191FF8E8E8EFF8C8C8CFF8B8B8BFF8B8B8BFF8C8C8CFF8E8E
          8EFF919191FF949494FF989898FF989898FF8B8987FA66605CCB000000230000
          00140000000C0000000400000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000004000000080000000C000000142F2B28666F6A
          66CE8C8986F3979695FD9C9C9CFF9E9E9EFFA0A0A0FFA0A0A0FF9E9E9EFF9C9C
          9CFF979695FD8A8784F36F6965D02E2A276D0000001C000000140000000C0000
          0004000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000400000004000000080000
          000C0000001000000014000000180000001C0000001C0000001C0000001C0000
          001C0000001800000014000000100000000C0000000800000004000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000040000000400000004000000040000000800000008000000040000
          0004000000040000000400000004000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36240000424D3624000000000000360000002800000030000000300000000100
          2000000000000024000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000080000000800000008000000080000000800000008000000080000
          0008000000080000000800000008000000080000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000008000000080000
          00100000001F0000002E0000003B00000042000000490000004F0000004F0000
          0049000000490000003B0000002E0000001F0000001000000008000000080000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000800000010000000270000003B0000
          006600000086000000A0000000AF000000BE000000C6000000CB000000CB0000
          00C6000000BE000000B3000000A0000000860000006600000042000000270000
          0010000000080000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000800000008000000270000004F00000082000000A83130
          2FE97B7876FE989695FFA2A2A2FFA9A9A9FFADADADFFAFAFAFFFAFAFAFFFADAD
          ADFFA9A9A9FFA2A2A2FF989695FF797574FE302F2DEA000000AF000000860000
          00540000002E0000001000000008000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0008000000080000001F0000004900000082000000B872706DFD9C9A9AFFAAAA
          AAFFB6B6B6FFBCBCBCFFC2C2C2FFC6C6C6FFCACACAFFCCCCCCFFCDCDCDFFCBCB
          CBFFC7C7C7FFC3C3C3FFBDBDBDFFB7B7B7FFAAAAAAFF9B9999FF706D6BFE0000
          00BE000000890000004F0000001F000000080000000800000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000080000
          00100000003500000069000000AC726F6DFD9D9D9DFFB1B1B1FFBABABAFFC2C2
          C2FFC9C9C9FFC9C9C9FFCACACAFFCACACAFFCBCBCBFFCBCBCBFFCBCBCBFFCBCB
          CBFFCBCBCBFFCBCBCBFFCBCBCBFFCBCBCBFFC4C4C4FFBBBBBBFFB1B1B1FF9E9C
          9CFF6D6B69FE000000B5000000730000003B0000001000000008000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000008000000180000
          004200000086444140EE949291FFADADADFFB7B7B7FFC1C1C1FFC5C5C5FFC5C5
          C5FFC6C6C6FFC7C7C7FFC7C7C7FFC8C8C8FFC8C8C8FFC9C9C9FFC9C9C9FFC9C9
          C9FFC9C9C9FFC8C8C8FFC8C8C8FFC8C8C8FFC7C7C7FFC7C7C7FFC3C3C3FFB9B9
          B9FFADADADFF918F8FFF403D3CF1000000910000004900000018000000080000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000800000018000000490000
          0096676462FC9C9C9CFFB0B0B0FFBABABAFFBFBFBFFFC1C1C1FFC2C2C2FFC3C3
          C3FFC4C4C4FFC4C4C4FFC4C4C4FFC5C5C5FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6
          C6FFC6C6C6FFC6C6C6FFC5C5C5FFC5C5C5FFC5C5C5FFC4C4C4FFC3C3C3FFC3C3
          C3FFBDBDBDFFB2B2B2FF9D9B9BFF625F5DFD000000A00000004F000000180000
          0008000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000008000000180000004900000096706D
          6AFEA1A1A1FFB0B0B0FFBABABAFFBBBBBBFFBCBCBCFFBEBEBEFFBFBFBFFFC0C0
          C0FFC1C1C1FFC1C1C1FFC2C2C2FFC2C2C2FFC3C3C3FFC3C3C3FFC3C3C3FFC3C3
          C3FFC3C3C3FFC3C3C3FFC3C3C3FFC2C2C2FFC2C2C2FFC1C1C1FFC1C1C1FFC0C0
          C0FFBFBFBFFFBEBEBEFFB2B2B2FFA1A1A1FF6A6867FE000000A40000004F0000
          0018000000080000000000000000000000000000000000000000000000000000
          0000000000000000000000000008000000100000003B0000008D6B6867FEA0A0
          A0FFAEAEAEFFB6B6B6FFB7B7B7FFB8B8B8FFBABABAFFBBBBBBFFBCBCBCFFBCBC
          BCFFBDBDBDFFBEBEBEFFBFBFBFFFBFBFBFFFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
          C0FFC0C0C0FFC0C0C0FFC0C0C0FFBFBFBFFFBFBFBFFFBEBEBEFFBEBEBEFFBDBD
          BDFFBCBCBCFFBABABAFFBABABAFFB0B0B0FFA0A0A0FF676463FE0000009A0000
          0049000000100000000800000000000000000000000000000000000000000000
          00000000000000000000000000080000002E0000007E5B5856FC9C9C9CFFAAAA
          AAFFB2B2B2FFB4B4B4FFB5B5B5FFB6B6B6FFB7B7B7FFB8B8B8FFB9B9B9FFBABA
          BAFFBABABAFFBBBBBBFFBBBBBBFFBCBCBCFFBCBCBCFFBDBDBDFFBDBDBDFFBDBD
          BDFFBDBDBDFFBDBDBDFFBCBCBCFFBCBCBCFFBBBBBBFFBBBBBBFFBABABAFFBABA
          BAFFB9B9B9FFB8B8B8FFB7B7B7FFB6B6B6FFADADADFF9C9C9CFF555351FD0000
          0089000000350000000800000000000000000000000000000000000000000000
          000000000000000000080000001F0000005F393633EA868686FFA6A6A6FFAFAF
          AFFFB0B0B0FFB0B0B0FFB2B2B2FFB3B3B3FFB4B4B4FFB5B5B5FFB6B6B6FFB6B6
          B6FFB7B7B7FFB8B8B8FFB8B8B8FFB9B9B9FFB9B9B9FFB9B9B9FFB9B9B9FFB9B9
          B9FFB9B9B9FFB9B9B9FFB9B9B9FFB9B9B9FFB8B8B8FFB8B8B8FFB7B7B7FFB7B7
          B7FFB6B6B6FFB5B5B5FFB4B4B4FFB3B3B3FFB2B2B2FFA8A8A8FF848282FF3331
          30EE000000690000001F00000008000000000000000000000000000000000000
          000000000000000000080000003B0000009A72706FFFA0A0A0FFACACACFFADAD
          ADFFADADADFFAEAEAEFFAFAFAFFFB0B0B0FFB0B0B0FFB1B1B1FFB2B2B2FFB3B3
          B3FFB4B4B4FFB5B5B5FFB5B5B5FFB5B5B5FFB5B5B5FFB6B6B6FFB6B6B6FFB6B6
          B6FFB6B6B6FFB6B6B6FFB5B5B5FFB5B5B5FFB5B5B5FFB5B5B5FFB4B4B4FFB3B3
          B3FFB2B2B2FFB2B2B2FFB0B0B0FFB0B0B0FFAFAFAFFFAEAEAEFFA2A2A2FF6D6C
          6BFF000000A80000004900000008000000000000000000000000000000000000
          0000000000080000001F00000069524F4DFC959595FFA5A5A5FFA9A9A9FFAAAA
          AAFFABABABFFABABABFFACACACFFADADADFFADADADFFAEAEAEFFAFAFAFFFAFAF
          AFFFB0B0B0FFB1B1B1FFB1B1B1FFB2B2B2FFB2B2B2FFB3B3B3FFB3B3B3FFB3B3
          B3FFB3B3B3FFB3B3B3FFB2B2B2FFB2B2B2FFB1B1B1FFB1B1B1FFB0B0B0FFB0B0
          B0FFAFAFAFFFAEAEAEFFAEAEAEFFADADADFFACACACFFACACACFFA7A7A7FF9595
          95FF4B4947FD0000007E00000027000000080000000000000000000000000000
          0000000000080000003B0000009B6D6C6BFF9E9E9EFFA6A6A6FFA7A7A7FFA7A7
          A7FFA8A8A8FFA9A9A9FFA9A9A9FFAAAAAAFFABABABFFABABABFFACACACFFACAC
          ACFFADADADFFAEAEAEFFAEAEAEFFAEAEAEFFAFAFAFFFAFAFAFFFAFAFAFFFAFAF
          AFFFAFAFAFFFAFAFAFFFAFAFAFFFAEAEAEFFAEAEAEFFADADADFFADADADFFACAC
          ACFFACACACFFABABABFFABABABFFAAAAAAFFAAAAAAFFA9A9A9FFA8A8A8FF9F9F
          9FFF6A6969FF000000AF00000049000000080000000000000000000000000000
          000800000018000000664A4744FB878787FFA2A2A2FFA4A4A4FFA4A4A4FFA5A5
          A5FFA6A6A6FFA6A6A6FFA7A7A7FFA7A7A7FFA8A8A8FFA9A9A9FFA9A9A9FFA9A9
          A9FFAAAAAAFFAAAAAAFFAAAAAAFFABABABFFABABABFFABABABFFABABABFFABAB
          ABFFABABABFFABABABFFABABABFFABABABFFAAAAAAFFAAAAAAFFAAAAAAFFAAAA
          AAFFA9A9A9FFA9A9A9FFA8A8A8FFA7A7A7FFA7A7A7FFA6A6A6FFA6A6A6FFA3A3
          A3FF888888FF43403FFD000000780000001F0000000800000000000000000000
          00080000002E000000865D5C5BFF919191FFA1A1A1FFA2A2A2FFA2A2A2FFA3A3
          A3FFA3A3A3FFA4A4A4FFA4A4A4FFA5A5A5FFA5A5A5FFA6A6A6FFA6A6A6FFA7A7
          A7FFA7A7A7FFA7A7A7FFA8A8A8FFA8A8A8FFA8A8A8FFA8A8A8FFA8A8A8FFA8A8
          A8FFA8A8A8FFA8A8A8FFA8A8A8FFA8A8A8FFA8A8A8FFA7A7A7FFA7A7A7FFA7A7
          A7FFA6A6A6FFA6A6A6FFA6A6A6FFA5A5A5FFA5A5A5FFA4A4A4FFA4A4A4FFA3A3
          A3FF949494FF585757FF0000009B000000350000000800000000000000000000
          0008000000421D1B18D76B6B6BFF9A9A9AFF9F9F9FFFA0A0A0FFA0A0A0FFA1A1
          A1FFA1A1A1FFA1A1A1FFA2A2A2FFA2A2A2FFA2A2A2FFA3A3A3FFA4A4A4FFA4A4
          A4FFA4A4A4FFA4A4A4FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
          A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA4A4A4FFA4A4
          A4FFA3A3A3FFA3A3A3FFA3A3A3FFA2A2A2FFA2A2A2FFA2A2A2FFA1A1A1FFA1A1
          A1FF9D9D9DFF6A6A6AFF181614E1000000540000001000000000000000000000
          00120000005F413D3BFC797979FF9D9D9DFF9E9E9EFF9E9E9EFF9E9E9EFF9F9F
          9FFF9F9F9FFF9F9F9FFFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA1A1A1FFA1A1
          A1FFA2A2A2FFA2A2A2FFA2A2A2FFA2A2A2FFA2A2A2FFA2A2A2FFA2A2A2FFA2A2
          A2FFA2A2A2FFA2A2A2FFA2A2A2FFA2A2A2FFA2A2A2FFA2A2A2FFA2A2A2FFA1A1
          A1FFA1A1A1FFA1A1A1FFA1A1A1FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FF9F9F
          9FFF9F9F9FFF7A7A7AFF3B3837FD0000006D0000001800000000000000000000
          00210000006D4F4C4BFF808080FF9B9B9BFF9C9C9CFF9D9D9DFF9D9D9DFF9D9D
          9DFF9D9D9DFF9E9E9EFF9E9E9EFF9E9E9EFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F
          9FFF9F9F9FFFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0
          A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0
          A0FF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9E9E9EFF9E9E9EFF9E9E
          9EFF9D9D9DFF828282FF494747FF000000860000002700000000000000000000
          00210000007E545453FF868686FF989898FF989898FF989898FF9A9A9AFF9A9A
          9AFF9B9B9BFF9B9B9BFF9C9C9CFF9C9C9CFF9C9C9CFF9D9D9DFF9D9D9DFF9D9D
          9DFF9D9D9DFF9E9E9EFF9E9E9EFF9E9E9EFF9E9E9EFF9E9E9EFF9E9E9EFF9E9E
          9EFF9E9E9EFF9E9E9EFF9E9E9EFF9E9E9EFF9E9E9EFF9E9E9EFF9E9E9EFF9D9D
          9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9C9C9CFF9B9B9BFF9B9B
          9BFF9B9B9BFF888888FF535252FF000000960000002E00000000000000000000
          0029000000865A5A5AFF8B8B8BFF959595FF959595FF959595FF959595FF9595
          95FF979797FF979797FF979797FF989898FF989898FF9A9A9AFF9A9A9AFF9A9A
          9AFF9B9B9BFF9B9B9BFF9B9B9BFF9B9B9BFF9C9C9CFF9C9C9CFF9B9B9BFF9C9C
          9CFF9C9C9CFF9C9C9CFF9C9C9CFF9C9C9CFF9C9C9CFF9B9B9BFF9B9B9BFF9B9B
          9BFF9B9B9BFF9A9A9AFF9A9A9AFF9A9A9AFF989898FF989898FF979797FF9797
          97FF979797FF8B8B8BFF595959FF000000A00000003500000000000000000000
          0029000000895E5E5EFF8E8E8EFF929292FF929292FF929292FF929292FF9292
          92FF929292FF929292FF929292FF949494FF949494FF949494FF959595FF9595
          95FF959595FF959595FF979797FF979797FF979797FF989898FF989898FF9898
          98FF989898FF9A9A9AFF9A9A9AFF989898FF989898FF979797FF979797FF9595
          95FF959595FF949494FF959595FF949494FF949494FF949494FF929292FF9292
          92FF929292FF8E8E8EFF5D5D5DFF000000A70000003500000000000000000000
          002900000089616161FF8E8E8EFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F
          8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF919191FF9191
          91FF929292FF929292FF949494FF949494FF949494FF959595FF959595FF9595
          95FF959595FF979797FF979797FF979797FF959595FF959595FF949494FF9191
          91FF919191FF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F
          8FFF8F8F8FFF8E8E8EFF606060FF000000A70000003B00000000000000000000
          002900000086616161FF898989FF8B8B8BFF8B8B8BFF8C8C8CFF8B8B8BFF8B8B
          8BFF8B8B8BFF8B8B8BFF8B8B8BFF8B8B8BFF8C8C8CFF8E8E8EFF8E8E8EFF8F8F
          8FFF8F8F8FFF919191FF919191FF919191FF919191FF929292FF919191FF9191
          91FF929292FF929292FF929292FF929292FF929292FF929292FF929292FF8F8F
          8FFF8C8C8CFF8B8B8BFF8B8B8BFF8C8C8CFF8B8B8BFF8B8B8BFF8B8B8BFF8B8B
          8BFF8B8B8BFF8B8B8BFF606060FF000000A40000003500000000000000000000
          00290000007E5F5F5FFF868686FF898989FF898989FF898989FF898989FF8989
          89FF898989FF898989FF898989FF8B8B8BFF8E8E8EFF8E8E8EFF8E8E8EFF8E8E
          8EFF8F8F8FFF919191FF919191FF919191FF929292FF929292FF929292FF9292
          92FF929292FF949494FF949494FF949494FF949494FF949494FF929292FF9494
          94FF8E8E8EFF898989FF898989FF898989FF898989FF898989FF898989FF8989
          89FF898989FF858585FF5D5D5DFF0000009B0000003500000000000000000000
          00210000006D5C5B5BFF7F7F7FFF868686FF868686FF868686FF868686FF8686
          86FF868686FF868686FF888888FF8B8B8BFF8C8C8CFF8E8E8EFF8E8E8EFF8F8F
          8FFF8F8F8FFF8F8F8FFF919191FF919191FF929292FF949494FF929292FF9494
          94FF949494FF949494FF949494FF949494FF949494FF949494FF949494FF9494
          94FF919191FF8B8B8BFF868686FF868686FF868686FF868686FF868686FF8686
          86FF868686FF7F7F7FFF595959FF000000910000002E00000000000000000000
          001800000066565555FF777777FF838383FF838383FF838383FF838383FF8383
          83FF838383FF858585FF888888FF8C8C8CFF8C8C8CFF8E8E8EFF8E8E8EFF8F8F
          8FFF8F8F8FFF919191FF919191FF919191FF929292FF949494FF949494FF9494
          94FF949494FF949494FF949494FF949494FF949494FF949494FF949494FF9494
          94FF949494FF8F8F8FFF858585FF838383FF838383FF838383FF838383FF8383
          83FF838383FF777777FF545353FF000000820000002700000000000000000000
          0018000000544F4D4BFF717171FF808080FF828282FF828282FF808080FF8282
          82FF828282FF858585FF8C8C8CFF8C8C8CFF8C8C8CFF8E8E8EFF8F8F8FFF9191
          91FF919191FF949494FF929292FF929292FF949494FF949494FF959595FF9595
          95FF959595FF959595FF979797FF979797FF979797FF979797FF959595FF9595
          95FF949494FF949494FF898989FF828282FF808080FF828282FF828282FF8080
          80FF828282FF717171FF4B4948FF0000006D0000001F00000000000000000000
          00080000003B45423FFA6A6A6AFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F
          7FFF7F7F7FFF868686FF8B8B8BFF8E8E8EFF8E8E8EFF8E8E8EFF919191FF9191
          91FF929292FF949494FF949494FF959595FF959595FF959595FF979797FF9797
          97FF979797FF979797FF989898FF979797FF989898FF979797FF979797FF9797
          97FF979797FF959595FF8C8C8CFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F
          7FFF7F7F7FFF6A6A6AFF3F3A38FC0000005B0000001000000000000000000000
          00080000002726221FBE5F5F5FFF777777FF7D7D7DFF7C7C7CFF7C7C7CFF7C7C
          7CFF7C7C7CFF888888FF8C8C8CFF8E8E8EFF8F8F8FFF919191FF929292FF9494
          94FF949494FF959595FF959595FF959595FF979797FF989898FF989898FF9A9A
          9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A
          9AFF989898FF989898FF929292FF7C7C7CFF7C7C7CFF7C7C7CFF7C7C7CFF7C7C
          7CFF777777FF5E5E5EFF1D1A18D00000003B0000000800000000000000000000
          00000000001800000054545251FF6E6E6EFF797979FF7A7A7AFF797979FF7A7A
          7AFF7A7A7AFF8B8B8BFF8E8E8EFF8E8E8EFF919191FF919191FF949494FF9292
          92FF959595FF979797FF989898FF989898FF9A9A9AFF9A9A9AFF9A9A9AFF9B9B
          9BFF9A9A9AFF9C9C9CFF9C9C9CFF9C9C9CFF9C9C9CFF9C9C9CFF9B9B9BFF9B9B
          9BFF9A9A9AFF9A9A9AFF989898FF797979FF797979FF797979FF797979FF7A7A
          7AFF6E6E6EFF514F4EFF00000073000000270000000000000000000000000000
          0000000000100000003547433FF7666666FF767676FF777777FF777777FF7777
          77FF777777FF8E8E8EFF8F8F8FFF919191FF929292FF949494FF959595FF9797
          97FF979797FF989898FF9A9A9AFF9B9B9BFF9B9B9BFF9C9C9CFF9C9C9CFF9C9C
          9CFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9C9C9CFF9C9C
          9CFF9C9C9CFF9C9C9CFF9A9A9AFF777777FF777777FF777777FF777777FF7676
          76FF666666FF403D3AF90000004F000000100000000000000000000000000000
          0000000000000000001F00000054575555FF6D6D6DFF767676FF747474FF7676
          76FF767676FF8B8B8BFF919191FF929292FF959595FF959595FF979797FF9A9A
          9AFF9B9B9BFF9B9B9BFF9D9D9DFF9D9D9DFF9E9E9EFF9E9E9EFF9E9E9EFF9E9E
          9EFF9E9E9EFF9E9E9EFF9E9E9EFF9E9E9EFF9E9E9EFF9E9E9EFF9E9E9EFF9E9E
          9EFF9E9E9EFF9D9D9DFF979797FF767676FF747474FF767676FF767676FF6D6D
          6DFF545353FF000000780000002E000000080000000000000000000000000000
          000000000000000000100000002E4A4642F7656565FF707070FF737373FF7373
          73FF737373FF858585FF949494FF959595FF989898FF9B9B9BFF9C9C9CFF9C9C
          9CFF9D9D9DFF9D9D9DFF9E9E9EFF9F9F9FFF9F9F9FFFA0A0A0FFA0A0A0FFA0A0
          A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FF9F9F
          9FFF9F9F9FFF9F9F9FFF8F8F8FFF737373FF737373FF737373FF707070FF6464
          64FF423E3BF90000004900000018000000000000000000000000000000000000
          000000000000000000000000001800000049545150FF686868FF717171FF7171
          71FF717171FF7F7F7FFF989898FF9A9A9AFF9C9C9CFF9D9D9DFF9E9E9EFF9F9F
          9FFF9F9F9FFFA0A0A0FFA0A0A0FFA1A1A1FFA1A1A1FFA2A2A2FFA2A2A2FFA2A2
          A2FFA2A2A2FFA3A3A3FFA2A2A2FFA3A3A3FFA2A2A2FFA2A2A2FFA2A2A2FFA2A2
          A2FFA1A1A1FFA1A1A1FF858585FF717171FF717171FF717171FF686868FF504E
          4DFF000000660000002700000008000000000000000000000000000000000000
          00000000000000000000000000080000002739342FCB5C5C5BFF6A6A6AFF7070
          70FF707070FF737373FF8F8F8FFF9D9D9DFF9E9E9EFF9F9F9FFFA0A0A0FFA1A1
          A1FFA1A1A1FFA2A2A2FFA2A2A2FFA3A3A3FFA3A3A3FFA4A4A4FFA4A4A4FFA4A4
          A4FFA4A4A4FFA5A5A5FFA4A4A4FFA5A5A5FFA5A5A5FFA5A5A5FFA4A4A4FFA4A4
          A4FFA3A3A3FF9D9D9DFF747474FF707070FF707070FF6A6A6AFF5A5A59FF312E
          2AD5000000350000001000000000000000000000000000000000000000000000
          0000000000000000000000000000000000100000002E4E4845F3636362FF6B6B
          6BFF6E6E6EFF6E6E6EFF7F7F7FFF9E9E9EFFA0A0A0FFA1A1A1FFA2A2A2FFA3A3
          A3FFA3A3A3FFA4A4A4FFA5A5A5FFA5A5A5FFA5A5A5FFA6A6A6FFA6A6A6FFA7A7
          A7FFA7A7A7FFA7A7A7FFA7A7A7FFA7A7A7FFA7A7A7FFA6A6A6FFA7A7A7FFA6A6
          A6FFA4A4A4FF868686FF6E6E6EFF6E6E6EFF6B6B6BFF626261FF46423EF60000
          0049000000180000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000008000000100000002E55504DFA6565
          65FF6B6B6BFF6D6D6DFF6D6D6DFF8B8B8BFFA2A2A2FFA3A3A3FFA4A4A4FFA5A5
          A5FFA5A5A5FFA6A6A6FFA7A7A7FFA8A8A8FFA8A8A8FFA8A8A8FFA8A8A8FFA9A9
          A9FFA9A9A9FFA9A9A9FFA9A9A9FFA9A9A9FFA9A9A9FFA9A9A9FFA9A9A9FFA8A8
          A8FF949494FF6D6D6DFF6D6D6DFF6B6B6BFF656565FF504C49FB000000490000
          001F000000080000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000008000000100000002E5853
          4FFA686867FF6B6B6BFF6B6B6BFF6D6D6DFF8F8F8FFFA5A5A5FFA6A6A6FFA7A7
          A7FFA8A8A8FFA9A9A9FFA9A9A9FFAAAAAAFFABABABFFABABABFFABABABFFABAB
          ABFFACACACFFACACACFFACACACFFACACACFFACACACFFACACACFFAAAAAAFF9A9A
          9AFF6D6D6DFF6B6B6BFF6B6B6BFF676766FF534F4CFB0000004F0000001F0000
          0008000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000008000000100000
          002E57514DF1676665FF6D6D6DFF6B6B6BFF6A6A6AFF8B8B8BFFA3A3A3FFAAAA
          AAFFAAAAAAFFABABABFFACACACFFACACACFFADADADFFADADADFFAEAEAEFFAEAE
          AEFFAEAEAEFFAEAEAEFFAEAEAEFFAEAEAEFFAEAEAEFFA7A7A7FF919191FF6A6A
          6AFF6B6B6BFF6D6D6DFF656464FF504D48F4000000420000001F000000080000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000080000
          001000000027433D39C6666362FF6F6F6FFF6D6D6DFF6A6A6AFF797979FF9B9B
          9BFFA7A7A7FFAEAEAEFFAEAEAEFFAFAFAFFFAFAFAFFFB0B0B0FFB0B0B0FFB0B0
          B0FFB1B1B1FFB1B1B1FFB1B1B1FFAAAAAAFF9E9E9EFF7C7C7CFF6A6A6AFF6D6D
          6DFF6F6F6FFF63605FFF3E3935CE000000350000001800000008000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000008000000180000002E5F5A55F46D6B6AFF727272FF6E6E6EFF6B6B
          6BFF747474FF8E8E8EFFA0A0A0FFA7A7A7FFADADADFFB1B1B1FFB2B2B2FFAEAE
          AEFFA9A9A9FFA2A2A2FF929292FF747474FF6B6B6BFF6E6E6EFF727272FF6B6A
          68FF5C5652F60000004200000027000000100000000800000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000008000000100000001F0000002E615B58F36D6A69FF7474
          74FF747474FF707070FF6D6D6DFF6A6A6AFF686868FF676767FF676767FF6868
          68FF6A6A6AFF6D6D6DFF707070FF747474FF747474FF6A6866FF5D5754F50000
          0042000000270000001800000008000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000080000001000000018000000273C37
          33A467615EF6706D6AFF747372FF787878FF7A7A7AFF7C7C7CFF7C7C7CFF7A7A
          7AFF787878FF747372FF6E6B68FF65605CF7373330AC00000035000000270000
          0018000000080000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000008000000080000
          0010000000180000001F000000270000002E0000003500000035000000350000
          0035000000350000002E000000270000001F0000001800000010000000080000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000800000008000000080000000800000010000000100000
          0008000000080000000800000008000000080000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36240000424D3624000000000000360000002800000030000000300000000100
          2000000000000024000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000040000000400000004000000040000000400000004000000040000
          0004000000040000000400000004000000040000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000004000000040000
          000800000010000000180000001F00000023000000270000002B0000002B0000
          0027000000270000001F00000018000000100000000800000004000000040000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000400000008000000140000001F0000
          00390000004F00000063000000700000007E000000860000008B0000008B0000
          00860000007E00000073000000630000004F0000003900000023000000140000
          0008000000040000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000400000004000000140000002B0000004C0000006A3A38
          37B3908F8DECB8B7B6FBC6C6C6FFCDCDCDFFD1D1D1FFD3D3D3FFD3D3D3FFD1D1
          D1FFCDCDCDFFC6C6C6FFB8B7B6FB8F8C8BED393837B5000000700000004F0000
          002E000000180000000800000004000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00040000000400000010000000270000004C00000078868482E7BEBDBDFDCECE
          CEFFDADADAFFE0E0E0FFE6E6E6FFEAEAEAFFEEEEEEFFF0F0F0FFF1F1F1FFEFEF
          EFFFEBEBEBFFE7E7E7FFE1E1E1FFDBDBDBFFCECECEFFBDBCBCFE858381E90000
          007E000000510000002B00000010000000040000000400000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000040000
          00080000001C0000003B0000006D878482E8C1C1C1FFD5D5D5FFDEDEDEFFE6E6
          E6FFEDEDEDFFEDEDEDFFEEEEEEFFEEEEEEFFEFEFEFFFEFEFEFFFEFEFEFFFEFEF
          EFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFE8E8E8FFDFDFDFFFD5D5D5FFC1C0
          C0FF838180EA00000075000000420000001F0000000800000004000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000040000000C0000
          00230000004F4C4947BCB5B4B3FCD1D1D1FFDBDBDBFFE5E5E5FFE9E9E9FFE9E9
          E9FFEAEAEAFFEBEBEBFFEBEBEBFFECECECFFECECECFFEDEDEDFFEDEDEDFFEDED
          EDFFEDEDEDFFECECECFFECECECFFECECECFFEBEBEBFFEBEBEBFFE7E7E7FFDDDD
          DDFFD1D1D1FFB3B2B2FD4B4846C200000057000000270000000C000000040000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000040000000C000000270000
          005B797674E1C0C0C0FFD4D4D4FFDEDEDEFFE3E3E3FFE5E5E5FFE6E6E6FFE7E7
          E7FFE8E8E8FFE8E8E8FFE8E8E8FFE9E9E9FFEAEAEAFFEAEAEAFFEAEAEAFFEAEA
          EAFFEAEAEAFFEAEAEAFFE9E9E9FFE9E9E9FFE9E9E9FFE8E8E8FFE7E7E7FFE7E7
          E7FFE1E1E1FFD6D6D6FFC0BFBFFF767371E4000000630000002B0000000C0000
          0004000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000040000000C000000270000005B8784
          82ECC5C5C5FFD4D4D4FFDEDEDEFFDFDFDFFFE0E0E0FFE2E2E2FFE3E3E3FFE4E4
          E4FFE5E5E5FFE5E5E5FFE6E6E6FFE6E6E6FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7
          E7FFE7E7E7FFE7E7E7FFE7E7E7FFE6E6E6FFE6E6E6FFE5E5E5FFE5E5E5FFE4E4
          E4FFE3E3E3FFE2E2E2FFD6D6D6FFC5C5C5FF83817FEE000000660000002B0000
          000C000000040000000000000000000000000000000000000000000000000000
          0000000000000000000000000004000000080000001F0000005482807EECC4C4
          C4FFD2D2D2FFDADADAFFDBDBDBFFDCDCDCFFDEDEDEFFDFDFDFFFE0E0E0FFE0E0
          E0FFE1E1E1FFE2E2E2FFE3E3E3FFE3E3E3FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4
          E4FFE4E4E4FFE4E4E4FFE4E4E4FFE3E3E3FFE3E3E3FFE2E2E2FFE2E2E2FFE1E1
          E1FFE0E0E0FFDEDEDEFFDEDEDEFFD4D4D4FFC4C4C4FF807D7BEE0000005E0000
          0027000000080000000400000000000000000000000000000000000000000000
          000000000000000000000000000400000018000000496E6A69E0C0C0C0FFCECE
          CEFFD6D6D6FFD8D8D8FFD9D9D9FFDADADAFFDBDBDBFFDCDCDCFFDDDDDDFFDEDE
          DEFFDEDEDEFFDFDFDFFFDFDFDFFFE0E0E0FFE0E0E0FFE1E1E1FFE1E1E1FFE1E1
          E1FFE1E1E1FFE1E1E1FFE0E0E0FFE0E0E0FFDFDFDFFFDFDFDFFFDEDEDEFFDEDE
          DEFFDDDDDDFFDCDCDCFFDBDBDBFFDADADAFFD1D1D1FFC0C0C0FF6B6966E40000
          00510000001C0000000400000000000000000000000000000000000000000000
          000000000000000000040000001000000035413E3BB5AAAAAAFFCACACAFFD3D3
          D3FFD4D4D4FFD4D4D4FFD6D6D6FFD7D7D7FFD8D8D8FFD9D9D9FFDADADAFFDADA
          DAFFDBDBDBFFDCDCDCFFDCDCDCFFDDDDDDFFDDDDDDFFDDDDDDFFDDDDDDFFDDDD
          DDFFDDDDDDFFDDDDDDFFDDDDDDFFDDDDDDFFDCDCDCFFDCDCDCFFDBDBDBFFDBDB
          DBFFDADADAFFD9D9D9FFD8D8D8FFD7D7D7FFD6D6D6FFCCCCCCFFA7A6A6FF3F3C
          3ABC0000003B0000001000000004000000000000000000000000000000000000
          000000000000000000040000001F0000005E949291FBC4C4C4FFD0D0D0FFD1D1
          D1FFD1D1D1FFD2D2D2FFD3D3D3FFD4D4D4FFD4D4D4FFD5D5D5FFD6D6D6FFD7D7
          D7FFD8D8D8FFD9D9D9FFD9D9D9FFD9D9D9FFD9D9D9FFDADADAFFDADADAFFDADA
          DAFFDADADAFFDADADAFFD9D9D9FFD9D9D9FFD9D9D9FFD9D9D9FFD8D8D8FFD7D7
          D7FFD6D6D6FFD6D6D6FFD4D4D4FFD4D4D4FFD3D3D3FFD2D2D2FFC6C6C6FF8F8E
          8DFC0000006A0000002700000004000000000000000000000000000000000000
          000000000004000000100000003B686461E2B9B9B9FFC9C9C9FFCDCDCDFFCECE
          CEFFCFCFCFFFCFCFCFFFD0D0D0FFD1D1D1FFD1D1D1FFD2D2D2FFD3D3D3FFD3D3
          D3FFD4D4D4FFD5D5D5FFD5D5D5FFD6D6D6FFD6D6D6FFD7D7D7FFD7D7D7FFD7D7
          D7FFD7D7D7FFD7D7D7FFD6D6D6FFD6D6D6FFD5D5D5FFD5D5D5FFD4D4D4FFD4D4
          D4FFD3D3D3FFD2D2D2FFD2D2D2FFD1D1D1FFD0D0D0FFD0D0D0FFCBCBCBFFB9B9
          B9FF63615EE60000004900000014000000040000000000000000000000000000
          0000000000040000001F0000005F908F8EFEC2C2C2FFCACACAFFCBCBCBFFCBCB
          CBFFCCCCCCFFCDCDCDFFCDCDCDFFCECECEFFCFCFCFFFCFCFCFFFD0D0D0FFD0D0
          D0FFD1D1D1FFD2D2D2FFD2D2D2FFD2D2D2FFD3D3D3FFD3D3D3FFD3D3D3FFD3D3
          D3FFD3D3D3FFD3D3D3FFD3D3D3FFD2D2D2FFD2D2D2FFD1D1D1FFD1D1D1FFD0D0
          D0FFD0D0D0FFCFCFCFFFCFCFCFFFCECECEFFCECECEFFCDCDCDFFCCCCCCFFC3C3
          C3FF8D8C8CFE0000007000000027000000040000000000000000000000000000
          00040000000C000000395E5B57DFABABABFFC6C6C6FFC8C8C8FFC8C8C8FFC9C9
          C9FFCACACAFFCACACAFFCBCBCBFFCBCBCBFFCCCCCCFFCDCDCDFFCDCDCDFFCDCD
          CDFFCECECEFFCECECEFFCECECEFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCF
          CFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCECECEFFCECECEFFCECECEFFCECE
          CEFFCDCDCDFFCDCDCDFFCCCCCCFFCBCBCBFFCBCBCBFFCACACAFFCACACAFFC7C7
          C7FFACACACFF5B5855E400000045000000100000000400000000000000000000
          0004000000180000004F7F7E7DFCB5B5B5FFC5C5C5FFC6C6C6FFC6C6C6FFC7C7
          C7FFC7C7C7FFC8C8C8FFC8C8C8FFC9C9C9FFC9C9C9FFCACACAFFCACACAFFCBCB
          CBFFCBCBCBFFCBCBCBFFCCCCCCFFCCCCCCFFCCCCCCFFCCCCCCFFCCCCCCFFCCCC
          CCFFCCCCCCFFCCCCCCFFCCCCCCFFCCCCCCFFCCCCCCFFCBCBCBFFCBCBCBFFCBCB
          CBFFCACACAFFCACACAFFCACACAFFC9C9C9FFC9C9C9FFC8C8C8FFC8C8C8FFC7C7
          C7FFB8B8B8FF7B7A79FD0000005F0000001C0000000400000000000000000000
          0004000000232523209A8F8F8FFFBEBEBEFFC3C3C3FFC4C4C4FFC4C4C4FFC5C5
          C5FFC5C5C5FFC5C5C5FFC6C6C6FFC6C6C6FFC6C6C6FFC7C7C7FFC8C8C8FFC8C8
          C8FFC8C8C8FFC8C8C8FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9
          C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC8C8C8FFC8C8
          C8FFC7C7C7FFC7C7C7FFC7C7C7FFC6C6C6FFC6C6C6FFC6C6C6FFC5C5C5FFC5C5
          C5FFC1C1C1FF8E8E8EFF25221FA70000002E0000000800000000000000000000
          000900000035585450E19D9D9DFFC1C1C1FFC2C2C2FFC2C2C2FFC2C2C2FFC3C3
          C3FFC3C3C3FFC3C3C3FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC5C5C5FFC5C5
          C5FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6
          C6FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6C6FFC5C5
          C5FFC5C5C5FFC5C5C5FFC5C5C5FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC3C3
          C3FFC3C3C3FF9E9E9EFF54514EE70000003E0000000C00000000000000000000
          00110000003E706D6BF8A4A4A4FFBFBFBFFFC0C0C0FFC1C1C1FFC1C1C1FFC1C1
          C1FFC1C1C1FFC2C2C2FFC2C2C2FFC2C2C2FFC3C3C3FFC3C3C3FFC3C3C3FFC3C3
          C3FFC3C3C3FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4
          C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4
          C4FFC3C3C3FFC3C3C3FFC3C3C3FFC3C3C3FFC3C3C3FFC2C2C2FFC2C2C2FFC2C2
          C2FFC1C1C1FFA6A6A6FF6B6968FA0000004F0000001400000000000000000000
          001100000049787877FEAAAAAAFFBCBCBCFFBCBCBCFFBCBCBCFFBEBEBEFFBEBE
          BEFFBFBFBFFFBFBFBFFFC0C0C0FFC0C0C0FFC0C0C0FFC1C1C1FFC1C1C1FFC1C1
          C1FFC1C1C1FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2
          C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC1C1
          C1FFC1C1C1FFC1C1C1FFC1C1C1FFC1C1C1FFC1C1C1FFC0C0C0FFBFBFBFFFBFBF
          BFFFBFBFBFFFACACACFF777675FE0000005B0000001800000000000000000000
          00150000004F7E7E7EFFAFAFAFFFB9B9B9FFB9B9B9FFB9B9B9FFB9B9B9FFB9B9
          B9FFBBBBBBFFBBBBBBFFBBBBBBFFBCBCBCFFBCBCBCFFBEBEBEFFBEBEBEFFBEBE
          BEFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFC0C0C0FFC0C0C0FFBFBFBFFFC0C0
          C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFBFBFBFFFBFBFBFFFBFBF
          BFFFBFBFBFFFBEBEBEFFBEBEBEFFBEBEBEFFBCBCBCFFBCBCBCFFBBBBBBFFBBBB
          BBFFBBBBBBFFAFAFAFFF7D7D7DFF000000630000001C00000000000000000000
          001500000051828282FFB2B2B2FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6
          B6FFB6B6B6FFB6B6B6FFB6B6B6FFB8B8B8FFB8B8B8FFB8B8B8FFB9B9B9FFB9B9
          B9FFB9B9B9FFB9B9B9FFBBBBBBFFBBBBBBFFBBBBBBFFBCBCBCFFBCBCBCFFBCBC
          BCFFBCBCBCFFBEBEBEFFBEBEBEFFBCBCBCFFBCBCBCFFBBBBBBFFBBBBBBFFB9B9
          B9FFB9B9B9FFB8B8B8FFB9B9B9FFB8B8B8FFB8B8B8FFB8B8B8FFB6B6B6FFB6B6
          B6FFB6B6B6FFB2B2B2FF818181FF000000690000001C00000000000000000000
          001500000051858585FFB2B2B2FFB3B3B3FFB3B3B3FFB3B3B3FFB3B3B3FFB3B3
          B3FFB3B3B3FFB3B3B3FFB3B3B3FFB3B3B3FFB3B3B3FFB3B3B3FFB5B5B5FFB5B5
          B5FFB6B6B6FFB6B6B6FFB8B8B8FFB8B8B8FFB8B8B8FFB9B9B9FFB9B9B9FFB9B9
          B9FFB9B9B9FFBBBBBBFFBBBBBBFFBBBBBBFFB9B9B9FFB9B9B9FFB8B8B8FFB5B5
          B5FFB5B5B5FFB3B3B3FFB3B3B3FFB3B3B3FFB3B3B3FFB3B3B3FFB3B3B3FFB3B3
          B3FFB3B3B3FFB2B2B2FF848484FF000000690000001F00000000000000000000
          00150000004F858585FFADADADFFAFAFAFFFAFAFAFFFB0B0B0FFAFAFAFFFAFAF
          AFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFB0B0B0FFB2B2B2FFB2B2B2FFB3B3
          B3FFB3B3B3FFB5B5B5FFB5B5B5FFB5B5B5FFB5B5B5FFB6B6B6FFB5B5B5FFB5B5
          B5FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB3B3
          B3FFB0B0B0FFAFAFAFFFAFAFAFFFB0B0B0FFAFAFAFFFAFAFAFFFAFAFAFFFAFAF
          AFFFAFAFAFFFAFAFAFFF848484FF000000660000001C00000000000000000000
          001500000049838383FFAAAAAAFFADADADFFADADADFFADADADFFADADADFFADAD
          ADFFADADADFFADADADFFADADADFFAFAFAFFFB2B2B2FFB2B2B2FFB2B2B2FFB2B2
          B2FFB3B3B3FFB5B5B5FFB5B5B5FFB5B5B5FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6
          B6FFB6B6B6FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFB6B6B6FFB8B8
          B8FFB2B2B2FFADADADFFADADADFFADADADFFADADADFFADADADFFADADADFFADAD
          ADFFADADADFFA9A9A9FF818181FF0000005F0000001C00000000000000000000
          00110000003E807F7FFFA3A3A3FFAAAAAAFFAAAAAAFFAAAAAAFFAAAAAAFFAAAA
          AAFFAAAAAAFFAAAAAAFFACACACFFAFAFAFFFB0B0B0FFB2B2B2FFB2B2B2FFB3B3
          B3FFB3B3B3FFB3B3B3FFB5B5B5FFB5B5B5FFB6B6B6FFB8B8B8FFB6B6B6FFB8B8
          B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8
          B8FFB5B5B5FFAFAFAFFFAAAAAAFFAAAAAAFFAAAAAAFFAAAAAAFFAAAAAAFFAAAA
          AAFFAAAAAAFFA3A3A3FF7D7D7DFF000000570000001800000000000000000000
          000C000000397A7978FE9B9B9BFFA7A7A7FFA7A7A7FFA7A7A7FFA7A7A7FFA7A7
          A7FFA7A7A7FFA9A9A9FFACACACFFB0B0B0FFB0B0B0FFB2B2B2FFB2B2B2FFB3B3
          B3FFB3B3B3FFB5B5B5FFB5B5B5FFB5B5B5FFB6B6B6FFB8B8B8FFB8B8B8FFB8B8
          B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8
          B8FFB8B8B8FFB3B3B3FFA9A9A9FFA7A7A7FFA7A7A7FFA7A7A7FFA7A7A7FFA7A7
          A7FFA7A7A7FF9B9B9BFF787776FE0000004C0000001400000000000000000000
          000C0000002E6F6C69F6959595FFA4A4A4FFA6A6A6FFA6A6A6FFA4A4A4FFA6A6
          A6FFA6A6A6FFA9A9A9FFB0B0B0FFB0B0B0FFB0B0B0FFB2B2B2FFB3B3B3FFB5B5
          B5FFB5B5B5FFB8B8B8FFB6B6B6FFB6B6B6FFB8B8B8FFB8B8B8FFB9B9B9FFB9B9
          B9FFB9B9B9FFB9B9B9FFBBBBBBFFBBBBBBFFBBBBBBFFBBBBBBFFB9B9B9FFB9B9
          B9FFB8B8B8FFB8B8B8FFADADADFFA6A6A6FFA4A4A4FFA6A6A6FFA6A6A6FFA4A4
          A4FFA6A6A6FF959595FF6C6A68F80000003E0000001000000000000000000000
          00040000001F595350D98E8E8EFFA3A3A3FFA3A3A3FFA3A3A3FFA3A3A3FFA3A3
          A3FFA3A3A3FFAAAAAAFFAFAFAFFFB2B2B2FFB2B2B2FFB2B2B2FFB5B5B5FFB5B5
          B5FFB6B6B6FFB8B8B8FFB8B8B8FFB9B9B9FFB9B9B9FFB9B9B9FFBBBBBBFFBBBB
          BBFFBBBBBBFFBBBBBBFFBCBCBCFFBBBBBBFFBCBCBCFFBBBBBBFFBBBBBBFFBBBB
          BBFFBBBBBBFFB9B9B9FFB0B0B0FFA3A3A3FFA3A3A3FFA3A3A3FFA3A3A3FFA3A3
          A3FFA3A3A3FF8E8E8EFF55514DE0000000320000000800000000000000000000
          00040000001426221F7E838383FF9B9B9BFFA1A1A1FFA0A0A0FFA0A0A0FFA0A0
          A0FFA0A0A0FFACACACFFB0B0B0FFB2B2B2FFB3B3B3FFB5B5B5FFB6B6B6FFB8B8
          B8FFB8B8B8FFB9B9B9FFB9B9B9FFB9B9B9FFBBBBBBFFBCBCBCFFBCBCBCFFBEBE
          BEFFBEBEBEFFBEBEBEFFBEBEBEFFBEBEBEFFBEBEBEFFBEBEBEFFBEBEBEFFBEBE
          BEFFBCBCBCFFBCBCBCFFB6B6B6FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0
          A0FF9B9B9BFF828282FF24201E910000001F0000000400000000000000000000
          00000000000C0000002E767472FA929292FF9D9D9DFF9E9E9EFF9D9D9DFF9E9E
          9EFF9E9E9EFFAFAFAFFFB2B2B2FFB2B2B2FFB5B5B5FFB5B5B5FFB8B8B8FFB6B6
          B6FFB9B9B9FFBBBBBBFFBCBCBCFFBCBCBCFFBEBEBEFFBEBEBEFFBEBEBEFFBFBF
          BFFFBEBEBEFFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFBFBFBFFFBFBF
          BFFFBEBEBEFFBEBEBEFFBCBCBCFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9E9E
          9EFF929292FF73716FFB00000042000000140000000000000000000000000000
          0000000000080000001C56524DD08A8A8AFF9A9A9AFF9B9B9BFF9B9B9BFF9B9B
          9BFF9B9B9BFFB2B2B2FFB3B3B3FFB5B5B5FFB6B6B6FFB8B8B8FFB9B9B9FFBBBB
          BBFFBBBBBBFFBCBCBCFFBEBEBEFFBFBFBFFFBFBFBFFFC0C0C0FFC0C0C0FFC0C0
          C0FFC1C1C1FFC1C1C1FFC1C1C1FFC1C1C1FFC1C1C1FFC1C1C1FFC0C0C0FFC0C0
          C0FFC0C0C0FFC0C0C0FFBEBEBEFF9B9B9BFF9B9B9BFF9B9B9BFF9B9B9BFF9A9A
          9AFF8A8A8AFF534E4BD70000002B000000080000000000000000000000000000
          000000000000000000100000002E7A7877FD919191FF9A9A9AFF989898FF9A9A
          9AFF9A9A9AFFAFAFAFFFB5B5B5FFB6B6B6FFB9B9B9FFB9B9B9FFBBBBBBFFBEBE
          BEFFBFBFBFFFBFBFBFFFC1C1C1FFC1C1C1FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2
          C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2
          C2FFC2C2C2FFC1C1C1FFBBBBBBFF9A9A9AFF989898FF9A9A9AFF9A9A9AFF9191
          91FF787776FE0000004500000018000000040000000000000000000000000000
          000000000000000000080000001859534FD0898989FF949494FF979797FF9797
          97FF979797FFA9A9A9FFB8B8B8FFB9B9B9FFBCBCBCFFBFBFBFFFC0C0C0FFC0C0
          C0FFC1C1C1FFC1C1C1FFC2C2C2FFC3C3C3FFC3C3C3FFC4C4C4FFC4C4C4FFC4C4
          C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC3C3
          C3FFC3C3C3FFC3C3C3FFB3B3B3FF979797FF979797FF979797FF949494FF8888
          88FF55504CD7000000270000000C000000000000000000000000000000000000
          000000000000000000000000000C00000027757270F88C8C8CFF959595FF9595
          95FF959595FFA3A3A3FFBCBCBCFFBEBEBEFFC0C0C0FFC1C1C1FFC2C2C2FFC3C3
          C3FFC3C3C3FFC4C4C4FFC4C4C4FFC5C5C5FFC5C5C5FFC6C6C6FFC6C6C6FFC6C6
          C6FFC6C6C6FFC7C7C7FFC6C6C6FFC7C7C7FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6
          C6FFC5C5C5FFC5C5C5FFA9A9A9FF959595FF959595FF959595FF8C8C8CFF716F
          6DF9000000390000001400000004000000000000000000000000000000000000
          00000000000000000000000000040000001435312D8B7F7F7FFE8E8E8EFF9494
          94FF949494FF979797FFB3B3B3FFC1C1C1FFC2C2C2FFC3C3C3FFC4C4C4FFC5C5
          C5FFC5C5C5FFC6C6C6FFC6C6C6FFC7C7C7FFC7C7C7FFC8C8C8FFC8C8C8FFC8C8
          C8FFC8C8C8FFC9C9C9FFC8C8C8FFC9C9C9FFC9C9C9FFC9C9C9FFC8C8C8FFC8C8
          C8FFC7C7C7FFC1C1C1FF989898FF949494FF949494FF8E8E8EFF7E7E7DFE342F
          2C970000001C0000000800000000000000000000000000000000000000000000
          0000000000000000000000000000000000080000001858524EC6878786FF8F8F
          8FFF929292FF929292FFA3A3A3FFC2C2C2FFC4C4C4FFC5C5C5FFC6C6C6FFC7C7
          C7FFC7C7C7FFC8C8C8FFC9C9C9FFC9C9C9FFC9C9C9FFCACACAFFCACACAFFCBCB
          CBFFCBCBCBFFCBCBCBFFCBCBCBFFCBCBCBFFCBCBCBFFCACACAFFCBCBCBFFCACA
          CAFFC8C8C8FFAAAAAAFF929292FF929292FF8F8F8FFF868685FF544F4BCD0000
          00270000000C0000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000004000000080000001866615CD88989
          89FF8F8F8FFF919191FF919191FFAFAFAFFFC6C6C6FFC7C7C7FFC8C8C8FFC9C9
          C9FFC9C9C9FFCACACAFFCBCBCBFFCCCCCCFFCCCCCCFFCCCCCCFFCCCCCCFFCDCD
          CDFFCDCDCDFFCDCDCDFFCDCDCDFFCDCDCDFFCDCDCDFFCDCDCDFFCDCDCDFFCCCC
          CCFFB8B8B8FF919191FF919191FF8F8F8FFF898989FF635E5BDE000000270000
          0010000000040000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000400000008000000186863
          5ED88C8C8BFF8F8F8FFF8F8F8FFF919191FFB3B3B3FFC9C9C9FFCACACAFFCBCB
          CBFFCCCCCCFFCDCDCDFFCDCDCDFFCECECEFFCFCFCFFFCFCFCFFFCFCFCFFFCFCF
          CFFFD0D0D0FFD0D0D0FFD0D0D0FFD0D0D0FFD0D0D0FFD0D0D0FFCECECEFFBEBE
          BEFF919191FF8F8F8FFF8F8F8FFF8B8B8AFF65605DDD0000002B000000100000
          0004000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000004000000080000
          00185E5853C38A8988FE919191FF8F8F8FFF8E8E8EFFAFAFAFFFC7C7C7FFCECE
          CEFFCECECEFFCFCFCFFFD0D0D0FFD0D0D0FFD1D1D1FFD1D1D1FFD2D2D2FFD2D2
          D2FFD2D2D2FFD2D2D2FFD2D2D2FFD2D2D2FFD2D2D2FFCBCBCBFFB5B5B5FF8E8E
          8EFF8F8F8FFF919191FF888787FE5C5652CA0000002300000010000000040000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000040000
          0008000000143B37328684817FF6939393FF919191FF8E8E8EFF9D9D9DFFBFBF
          BFFFCBCBCBFFD2D2D2FFD2D2D2FFD3D3D3FFD3D3D3FFD4D4D4FFD4D4D4FFD4D4
          D4FFD5D5D5FFD5D5D5FFD5D5D5FFCECECEFFC2C2C2FFA0A0A0FF8E8E8EFF9191
          91FF939393FF83807EF83A36328F0000001C0000000C00000004000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000040000000C0000001867615DCA8F8D8CFC969696FF929292FF8F8F
          8FFF989898FFB2B2B2FFC4C4C4FFCBCBCBFFD1D1D1FFD5D5D5FFD6D6D6FFD2D2
          D2FFCDCDCDFFC6C6C6FFB6B6B6FF989898FF8F8F8FFF929292FF969696FF8E8D
          8BFD66605CCE0000002300000014000000080000000400000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000400000008000000100000001868625EC78E8B8AF99898
          98FF989898FF949494FF919191FF8E8E8EFF8C8C8CFF8B8B8BFF8B8B8BFF8C8C
          8CFF8E8E8EFF919191FF949494FF989898FF989898FF8B8987FA66605CCB0000
          0023000000140000000C00000004000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000004000000080000000C000000142F2B
          28666F6A66CE8C8986F3979695FD9C9C9CFF9E9E9EFFA0A0A0FFA0A0A0FF9E9E
          9EFF9C9C9CFF979695FD8A8784F36F6965D02E2A276D0000001C000000140000
          000C000000040000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000004000000040000
          00080000000C0000001000000014000000180000001C0000001C0000001C0000
          001C0000001C0000001800000014000000100000000C00000008000000040000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000400000004000000040000000400000008000000080000
          0004000000040000000400000004000000040000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36240000424D3624000000000000360000002800000030000000300000000100
          2000000000000024000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000010101080101010801010108010101080101010801010108010101080101
          0108010101080101010801010108010101080000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000001010108010101080101
          01100202021F0303032E0404043B05050542050505490606064F0606064F0505
          0549050505490404043B0303032E0202021F0101011001010108010101080000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000101010801010110030303270404043B0808
          08660B0B0B860E0E0EA0101010AF111111BE131313C6131313CB131313CB1313
          13C6111111BE101010B30E0E0EA00B0B0B860808086605050542030303270101
          0110010101080000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000101010801010108030303270606064F0B0B0B820E0E0EA86561
          5FE9BBBAB8FEDDDDDCFFEAEAEAFFF1F1F1FFF5F5F5FFF7F7F7FFF7F7F7FFF5F5
          F5FFF1F1F1FFEAEAEAFFDDDDDCFFB9B7B6FE64615FEA101010AF0B0B0B860606
          06540303032E0101011001010108000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000101
          0108010101080202021F050505490B0B0B82111111B8B2B1AFFDE1E1E1FFF2F2
          F2FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF2F2F2FFE0E0E0FFB1AFADFE1111
          11BE0B0B0B890606064F0202021F010101080101010800000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000010101080101
          011004040435080808690F0F0FACB2B0AFFDE5E5E5FFF9F9F9FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9F9F9FFE4E4
          E4FFAEADABFE101010B5090909730404043B0101011001010108000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000001010108020202180505
          05420B0B0B867A7773EED9D9D8FFF5F5F5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFF5F5F5FFD6D6D6FF787371F10C0C0C910505054902020218010101080000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000101010802020218050505490C0C
          0C96A6A4A2FCE4E4E4FFF8F8F8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFAFAFAFFE3E3E3FFA2A09EFD0E0E0EA00606064F020202180101
          0108000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000101010802020218050505490C0C0C96B1AF
          ADFEE9E9E9FFF8F8F8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFAFAFAFFE9E9E9FFABAAA8FE0E0E0EA40606064F0202
          0218010101080000000000000000000000000000000000000000000000000000
          0000000000000000000001010108010101100404043B0C0C0C8DACAAA8FEE8E8
          E8FFF6F6F6FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8FFE8E8E8FFA8A6A4FE0D0D0D9A0505
          0549010101100101010800000000000000000000000000000000000000000000
          00000000000000000000010101080303032E0A0A0A7E9A9795FCE4E4E4FFF2F2
          F2FFFAFAFAFFFCFCFCFFFDFDFDFFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFFF5F5F5FFE4E4E4FF969491FD0B0B
          0B89040404350101010800000000000000000000000000000000000000000000
          000000000000010101080202021F0707075F6D6964EACECECEFFEEEEEEFFF7F7
          F7FFF8F8F8FFF8F8F8FFFAFAFAFFFBFBFBFFFCFCFCFFFDFDFDFFFEFEFEFFFEFE
          FEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFEFEFEFFFDFDFDFFFCFCFCFFFBFBFBFFFAFAFAFFF0F0F0FFCACACAFF6A65
          61EE080808690202021F01010108000000000000000000000000000000000000
          000000000000010101080404043B0D0D0D9AB8B7B6FFE8E8E8FFF4F4F4FFF5F5
          F5FFF5F5F5FFF6F6F6FFF7F7F7FFF8F8F8FFF8F8F8FFF9F9F9FFFAFAFAFFFBFB
          FBFFFCFCFCFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFEFEFEFFFEFEFEFFFEFE
          FEFFFEFEFEFFFEFEFEFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFCFCFCFFFBFB
          FBFFFAFAFAFFFAFAFAFFF8F8F8FFF8F8F8FFF7F7F7FFF6F6F6FFEAEAEAFFB3B3
          B2FF0E0E0EA80505054901010108000000000000000000000000000000000000
          0000010101080202021F08080869928F8BFCDDDDDDFFEDEDEDFFF1F1F1FFF2F2
          F2FFF3F3F3FFF3F3F3FFF4F4F4FFF5F5F5FFF5F5F5FFF6F6F6FFF7F7F7FFF7F7
          F7FFF8F8F8FFF9F9F9FFF9F9F9FFFAFAFAFFFAFAFAFFFBFBFBFFFBFBFBFFFBFB
          FBFFFBFBFBFFFBFBFBFFFAFAFAFFFAFAFAFFF9F9F9FFF9F9F9FFF8F8F8FFF8F8
          F8FFF7F7F7FFF6F6F6FFF6F6F6FFF5F5F5FFF4F4F4FFF4F4F4FFEFEFEFFFDDDD
          DDFF8D8A87FD0A0A0A7E03030327010101080000000000000000000000000000
          0000010101080404043B0D0D0D9BB3B3B2FFE6E6E6FFEEEEEEFFEFEFEFFFEFEF
          EFFFF0F0F0FFF1F1F1FFF1F1F1FFF2F2F2FFF3F3F3FFF3F3F3FFF4F4F4FFF4F4
          F4FFF5F5F5FFF6F6F6FFF6F6F6FFF6F6F6FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7
          F7FFF7F7F7FFF7F7F7FFF7F7F7FFF6F6F6FFF6F6F6FFF5F5F5FFF5F5F5FFF4F4
          F4FFF4F4F4FFF3F3F3FFF3F3F3FFF2F2F2FFF2F2F2FFF1F1F1FFF0F0F0FFE7E7
          E7FFB1B0B0FF101010AF05050549010101080000000000000000000000000101
          01080202021808080866898581FBCFCFCFFFEAEAEAFFECECECFFECECECFFEDED
          EDFFEEEEEEFFEEEEEEFFEFEFEFFFEFEFEFFFF0F0F0FFF1F1F1FFF1F1F1FFF1F1
          F1FFF2F2F2FFF2F2F2FFF2F2F2FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3
          F3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2
          F2FFF1F1F1FFF1F1F1FFF0F0F0FFEFEFEFFFEFEFEFFFEEEEEEFFEEEEEEFFEBEB
          EBFFD0D0D0FF85817DFD090909780202021F0101010800000000000000000101
          01080303032E0B0B0B86A3A3A1FFD9D9D9FFE9E9E9FFEAEAEAFFEAEAEAFFEBEB
          EBFFEBEBEBFFECECECFFECECECFFEDEDEDFFEDEDEDFFEEEEEEFFEEEEEEFFEFEF
          EFFFEFEFEFFFEFEFEFFFF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0
          F0FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFEFEFEFFFEFEFEFFFEFEF
          EFFFEEEEEEFFEEEEEEFFEEEEEEFFEDEDEDFFEDEDEDFFECECECFFECECECFFEBEB
          EBFFDCDCDCFF9E9E9DFF0D0D0D9B040404350101010800000000000000000101
          0108050505424B4640D7B3B3B3FFE2E2E2FFE7E7E7FFE8E8E8FFE8E8E8FFE9E9
          E9FFE9E9E9FFE9E9E9FFEAEAEAFFEAEAEAFFEAEAEAFFEBEBEBFFECECECFFECEC
          ECFFECECECFFECECECFFEDEDEDFFEDEDEDFFEDEDEDFFEDEDEDFFEDEDEDFFEDED
          EDFFEDEDEDFFEDEDEDFFEDEDEDFFEDEDEDFFEDEDEDFFEDEDEDFFECECECFFECEC
          ECFFEBEBEBFFEBEBEBFFEBEBEBFFEAEAEAFFEAEAEAFFEAEAEAFFE9E9E9FFE9E9
          E9FFE5E5E5FFB2B2B2FF4A4540E1060606540101011000000000000000000101
          01120707075F837E78FCC1C1C1FFE5E5E5FFE6E6E6FFE6E6E6FFE6E6E6FFE7E7
          E7FFE7E7E7FFE7E7E7FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE9E9E9FFE9E9
          E9FFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEA
          EAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFE9E9
          E9FFE9E9E9FFE9E9E9FFE9E9E9FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE7E7
          E7FFE7E7E7FFC2C2C2FF7D7873FD0909096D0202021800000000000000000202
          02210909096D969391FFC8C8C8FFE3E3E3FFE4E4E4FFE5E5E5FFE5E5E5FFE5E5
          E5FFE5E5E5FFE6E6E6FFE6E6E6FFE6E6E6FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7
          E7FFE7E7E7FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
          E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
          E8FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE6E6E6FFE6E6E6FFE6E6
          E6FFE5E5E5FFCACACAFF908E8DFF0B0B0B860303032700000000000000000202
          02210A0A0A7E9B9B9AFFCECECEFFE0E0E0FFE0E0E0FFE0E0E0FFE2E2E2FFE2E2
          E2FFE3E3E3FFE3E3E3FFE4E4E4FFE4E4E4FFE4E4E4FFE5E5E5FFE5E5E5FFE5E5
          E5FFE5E5E5FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6
          E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE5E5
          E5FFE5E5E5FFE5E5E5FFE5E5E5FFE5E5E5FFE5E5E5FFE4E4E4FFE3E3E3FFE3E3
          E3FFE3E3E3FFD0D0D0FF9A9998FF0C0C0C960303032E00000000000000000303
          03290B0B0B86A2A2A2FFD3D3D3FFDDDDDDFFDDDDDDFFDDDDDDFFDDDDDDFFDDDD
          DDFFDFDFDFFFDFDFDFFFDFDFDFFFE0E0E0FFE0E0E0FFE2E2E2FFE2E2E2FFE2E2
          E2FFE3E3E3FFE3E3E3FFE3E3E3FFE3E3E3FFE4E4E4FFE4E4E4FFE3E3E3FFE4E4
          E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE3E3E3FFE3E3E3FFE3E3
          E3FFE3E3E3FFE2E2E2FFE2E2E2FFE2E2E2FFE0E0E0FFE0E0E0FFDFDFDFFFDFDF
          DFFFDFDFDFFFD3D3D3FFA1A1A1FF0E0E0EA00404043500000000000000000303
          03290B0B0B89A6A6A6FFD6D6D6FFDADADAFFDADADAFFDADADAFFDADADAFFDADA
          DAFFDADADAFFDADADAFFDADADAFFDCDCDCFFDCDCDCFFDCDCDCFFDDDDDDFFDDDD
          DDFFDDDDDDFFDDDDDDFFDFDFDFFFDFDFDFFFDFDFDFFFE0E0E0FFE0E0E0FFE0E0
          E0FFE0E0E0FFE2E2E2FFE2E2E2FFE0E0E0FFE0E0E0FFDFDFDFFFDFDFDFFFDDDD
          DDFFDDDDDDFFDCDCDCFFDDDDDDFFDCDCDCFFDCDCDCFFDCDCDCFFDADADAFFDADA
          DAFFDADADAFFD6D6D6FFA5A5A5FF0E0E0EA70404043500000000000000000303
          03290B0B0B89A9A9A9FFD6D6D6FFD7D7D7FFD7D7D7FFD7D7D7FFD7D7D7FFD7D7
          D7FFD7D7D7FFD7D7D7FFD7D7D7FFD7D7D7FFD7D7D7FFD7D7D7FFD9D9D9FFD9D9
          D9FFDADADAFFDADADAFFDCDCDCFFDCDCDCFFDCDCDCFFDDDDDDFFDDDDDDFFDDDD
          DDFFDDDDDDFFDFDFDFFFDFDFDFFFDFDFDFFFDDDDDDFFDDDDDDFFDCDCDCFFD9D9
          D9FFD9D9D9FFD7D7D7FFD7D7D7FFD7D7D7FFD7D7D7FFD7D7D7FFD7D7D7FFD7D7
          D7FFD7D7D7FFD6D6D6FFA8A8A8FF0E0E0EA70404043B00000000000000000303
          03290B0B0B86A9A9A9FFD1D1D1FFD3D3D3FFD3D3D3FFD4D4D4FFD3D3D3FFD3D3
          D3FFD3D3D3FFD3D3D3FFD3D3D3FFD3D3D3FFD4D4D4FFD6D6D6FFD6D6D6FFD7D7
          D7FFD7D7D7FFD9D9D9FFD9D9D9FFD9D9D9FFD9D9D9FFDADADAFFD9D9D9FFD9D9
          D9FFDADADAFFDADADAFFDADADAFFDADADAFFDADADAFFDADADAFFDADADAFFD7D7
          D7FFD4D4D4FFD3D3D3FFD3D3D3FFD4D4D4FFD3D3D3FFD3D3D3FFD3D3D3FFD3D3
          D3FFD3D3D3FFD3D3D3FFA8A8A8FF0E0E0EA40404043500000000000000000303
          03290A0A0A7EA7A7A7FFCECECEFFD1D1D1FFD1D1D1FFD1D1D1FFD1D1D1FFD1D1
          D1FFD1D1D1FFD1D1D1FFD1D1D1FFD3D3D3FFD6D6D6FFD6D6D6FFD6D6D6FFD6D6
          D6FFD7D7D7FFD9D9D9FFD9D9D9FFD9D9D9FFDADADAFFDADADAFFDADADAFFDADA
          DAFFDADADAFFDCDCDCFFDCDCDCFFDCDCDCFFDCDCDCFFDCDCDCFFDADADAFFDCDC
          DCFFD6D6D6FFD1D1D1FFD1D1D1FFD1D1D1FFD1D1D1FFD1D1D1FFD1D1D1FFD1D1
          D1FFD1D1D1FFCDCDCDFFA5A5A5FF0D0D0D9B0404043500000000000000000202
          02210909096DA4A3A3FFC7C7C7FFCECECEFFCECECEFFCECECEFFCECECEFFCECE
          CEFFCECECEFFCECECEFFD0D0D0FFD3D3D3FFD4D4D4FFD6D6D6FFD6D6D6FFD7D7
          D7FFD7D7D7FFD7D7D7FFD9D9D9FFD9D9D9FFDADADAFFDCDCDCFFDADADAFFDCDC
          DCFFDCDCDCFFDCDCDCFFDCDCDCFFDCDCDCFFDCDCDCFFDCDCDCFFDCDCDCFFDCDC
          DCFFD9D9D9FFD3D3D3FFCECECEFFCECECEFFCECECEFFCECECEFFCECECEFFCECE
          CEFFCECECEFFC7C7C7FFA1A1A1FF0C0C0C910303032E00000000000000000202
          0218080808669D9C9BFFBFBFBFFFCBCBCBFFCBCBCBFFCBCBCBFFCBCBCBFFCBCB
          CBFFCBCBCBFFCDCDCDFFD0D0D0FFD4D4D4FFD4D4D4FFD6D6D6FFD6D6D6FFD7D7
          D7FFD7D7D7FFD9D9D9FFD9D9D9FFD9D9D9FFDADADAFFDCDCDCFFDCDCDCFFDCDC
          DCFFDCDCDCFFDCDCDCFFDCDCDCFFDCDCDCFFDCDCDCFFDCDCDCFFDCDCDCFFDCDC
          DCFFDCDCDCFFD7D7D7FFCDCDCDFFCBCBCBFFCBCBCBFFCBCBCBFFCBCBCBFFCBCB
          CBFFCBCBCBFFBFBFBFFF9B9A99FF0B0B0B820303032700000000000000000202
          02180606065495928FFFB9B9B9FFC8C8C8FFCACACAFFCACACAFFC8C8C8FFCACA
          CAFFCACACAFFCDCDCDFFD4D4D4FFD4D4D4FFD4D4D4FFD6D6D6FFD7D7D7FFD9D9
          D9FFD9D9D9FFDCDCDCFFDADADAFFDADADAFFDCDCDCFFDCDCDCFFDDDDDDFFDDDD
          DDFFDDDDDDFFDDDDDDFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDDDDDDFFDDDD
          DDFFDCDCDCFFDCDCDCFFD1D1D1FFCACACAFFC8C8C8FFCACACAFFCACACAFFC8C8
          C8FFCACACAFFB9B9B9FF92908EFF0909096D0202021F00000000000000000101
          01080404043B857D7AFAB2B2B2FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7
          C7FFC7C7C7FFCECECEFFD3D3D3FFD6D6D6FFD6D6D6FFD6D6D6FFD9D9D9FFD9D9
          D9FFDADADAFFDCDCDCFFDCDCDCFFDDDDDDFFDDDDDDFFDDDDDDFFDFDFDFFFDFDF
          DFFFDFDFDFFFDFDFDFFFE0E0E0FFDFDFDFFFE0E0E0FFDFDFDFFFDFDFDFFFDFDF
          DFFFDFDFDFFFDDDDDDFFD4D4D4FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7
          C7FFC7C7C7FFB2B2B2FF7F7B75FC0707075B0101011000000000000000000101
          0108030303274C453FBEA7A7A7FFBFBFBFFFC5C5C5FFC4C4C4FFC4C4C4FFC4C4
          C4FFC4C4C4FFD0D0D0FFD4D4D4FFD6D6D6FFD7D7D7FFD9D9D9FFDADADAFFDCDC
          DCFFDCDCDCFFDDDDDDFFDDDDDDFFDDDDDDFFDFDFDFFFE0E0E0FFE0E0E0FFE2E2
          E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2
          E2FFE0E0E0FFE0E0E0FFDADADAFFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4
          C4FFBFBFBFFFA6A6A6FF49423DD00404043B0101010800000000000000000000
          000002020218060606549A9997FFB6B6B6FFC1C1C1FFC2C2C2FFC1C1C1FFC2C2
          C2FFC2C2C2FFD3D3D3FFD6D6D6FFD6D6D6FFD9D9D9FFD9D9D9FFDCDCDCFFDADA
          DAFFDDDDDDFFDFDFDFFFE0E0E0FFE0E0E0FFE2E2E2FFE2E2E2FFE2E2E2FFE3E3
          E3FFE2E2E2FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE3E3E3FFE3E3
          E3FFE2E2E2FFE2E2E2FFE0E0E0FFC1C1C1FFC1C1C1FFC1C1C1FFC1C1C1FFC2C2
          C2FFB6B6B6FF989694FF09090973030303270000000000000000000000000000
          00000101011004040435857E77F7AEAEAEFFBEBEBEFFBFBFBFFFBFBFBFFFBFBF
          BFFFBFBFBFFFD6D6D6FFD7D7D7FFD9D9D9FFDADADAFFDCDCDCFFDDDDDDFFDFDF
          DFFFDFDFDFFFE0E0E0FFE2E2E2FFE3E3E3FFE3E3E3FFE4E4E4FFE4E4E4FFE4E4
          E4FFE5E5E5FFE5E5E5FFE5E5E5FFE5E5E5FFE5E5E5FFE5E5E5FFE4E4E4FFE4E4
          E4FFE4E4E4FFE4E4E4FFE2E2E2FFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBEBE
          BEFFAEAEAEFF7F7873F90606064F010101100000000000000000000000000000
          0000000000000202021F060606549D9C9BFFB5B5B5FFBEBEBEFFBCBCBCFFBEBE
          BEFFBEBEBEFFD3D3D3FFD9D9D9FFDADADAFFDDDDDDFFDDDDDDFFDFDFDFFFE2E2
          E2FFE3E3E3FFE3E3E3FFE5E5E5FFE5E5E5FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6
          E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6
          E6FFE6E6E6FFE5E5E5FFDFDFDFFFBEBEBEFFBCBCBCFFBEBEBEFFBEBEBEFFB5B5
          B5FF9B9A99FF090909780303032E010101080000000000000000000000000000
          000000000000010101100303032E87807AF7ADADADFFB8B8B8FFBBBBBBFFBBBB
          BBFFBBBBBBFFCDCDCDFFDCDCDCFFDDDDDDFFE0E0E0FFE3E3E3FFE4E4E4FFE4E4
          E4FFE5E5E5FFE5E5E5FFE6E6E6FFE7E7E7FFE7E7E7FFE8E8E8FFE8E8E8FFE8E8
          E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE7E7
          E7FFE7E7E7FFE7E7E7FFD7D7D7FFBBBBBBFFBBBBBBFFBBBBBBFFB8B8B8FFACAC
          ACFF817B74F90505054902020218000000000000000000000000000000000000
          0000000000000000000002020218050505499A9896FFB0B0B0FFB9B9B9FFB9B9
          B9FFB9B9B9FFC7C7C7FFE0E0E0FFE2E2E2FFE4E4E4FFE5E5E5FFE6E6E6FFE7E7
          E7FFE7E7E7FFE8E8E8FFE8E8E8FFE9E9E9FFE9E9E9FFEAEAEAFFEAEAEAFFEAEA
          EAFFEAEAEAFFEBEBEBFFEAEAEAFFEBEBEBFFEAEAEAFFEAEAEAFFEAEAEAFFEAEA
          EAFFE9E9E9FFE9E9E9FFCDCDCDFFB9B9B9FFB9B9B9FFB9B9B9FFB0B0B0FF9795
          93FF080808660303032701010108000000000000000000000000000000000000
          000000000000000000000101010803030327635B53CBA3A3A2FFB2B2B2FFB8B8
          B8FFB8B8B8FFBBBBBBFFD7D7D7FFE5E5E5FFE6E6E6FFE7E7E7FFE8E8E8FFE9E9
          E9FFE9E9E9FFEAEAEAFFEAEAEAFFEBEBEBFFEBEBEBFFECECECFFECECECFFECEC
          ECFFECECECFFEDEDEDFFECECECFFEDEDEDFFEDEDEDFFEDEDEDFFECECECFFECEC
          ECFFEBEBEBFFE5E5E5FFBCBCBCFFB8B8B8FFB8B8B8FFB2B2B2FFA1A1A0FF5F57
          51D5040404350101011000000000000000000000000000000000000000000000
          0000000000000000000000000000010101100303032E87817AF3ABABAAFFB3B3
          B3FFB6B6B6FFB6B6B6FFC7C7C7FFE6E6E6FFE8E8E8FFE9E9E9FFEAEAEAFFEBEB
          EBFFEBEBEBFFECECECFFEDEDEDFFEDEDEDFFEDEDEDFFEEEEEEFFEEEEEEFFEFEF
          EFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEEEEEEFFEFEFEFFFEEEE
          EEFFECECECFFCECECEFFB6B6B6FFB6B6B6FFB3B3B3FFAAAAA9FF827B75F60505
          0549020202180000000000000000000000000000000000000000000000000000
          000000000000000000000000000001010108010101100303032E938E89FAADAD
          ADFFB3B3B3FFB5B5B5FFB5B5B5FFD3D3D3FFEAEAEAFFEBEBEBFFECECECFFEDED
          EDFFEDEDEDFFEEEEEEFFEFEFEFFFF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF1F1
          F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF0F0
          F0FFDCDCDCFFB5B5B5FFB5B5B5FFB3B3B3FFADADADFF8F8985FB050505490202
          021F010101080000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000001010108010101100303032E9691
          8BFAB0B0AFFFB3B3B3FFB3B3B3FFB5B5B5FFD7D7D7FFEDEDEDFFEEEEEEFFEFEF
          EFFFF0F0F0FFF1F1F1FFF1F1F1FFF2F2F2FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3
          F3FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF2F2F2FFE2E2
          E2FFB5B5B5FFB3B3B3FFB3B3B3FFAFAFAEFF918C88FB0606064F0202021F0101
          0108000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000001010108010101100303
          032E8F8882F1ADADACFFB5B5B5FFB3B3B3FFB2B2B2FFD3D3D3FFEBEBEBFFF2F2
          F2FFF2F2F2FFF3F3F3FFF4F4F4FFF4F4F4FFF5F5F5FFF5F5F5FFF6F6F6FFF6F6
          F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFEFEFEFFFD9D9D9FFB2B2
          B2FFB3B3B3FFB5B5B5FFACABABFF8B847FF4050505420202021F010101080000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000010101080101
          0110030303276A635CC6AAA8A6FFB7B7B7FFB5B5B5FFB2B2B2FFC1C1C1FFE3E3
          E3FFEFEFEFFFF6F6F6FFF6F6F6FFF7F7F7FFF7F7F7FFF8F8F8FFF8F8F8FFF8F8
          F8FFF9F9F9FFF9F9F9FFF9F9F9FFF2F2F2FFE6E6E6FFC4C4C4FFB2B2B2FFB5B5
          B5FFB7B7B7FFA9A7A5FF69615ACE040404350202021801010108000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000001010108020202180303032E97918EF4B3B2B1FFBABABAFFB6B6B6FFB3B3
          B3FFBCBCBCFFD6D6D6FFE8E8E8FFEFEFEFFFF5F5F5FFF9F9F9FFFAFAFAFFF6F6
          F6FFF1F1F1FFEAEAEAFFDADADAFFBCBCBCFFB3B3B3FFB6B6B6FFBABABAFFB1B1
          AFFF96908BF60505054203030327010101100101010800000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000001010108010101100202021F0303032E98948FF3B3B1B0FFBCBC
          BCFFBCBCBCFFB8B8B8FFB5B5B5FFB2B2B2FFB0B0B0FFAFAFAFFFAFAFAFFFB0B0
          B0FFB2B2B2FFB5B5B5FFB8B8B8FFBCBCBCFFBCBCBCFFB0AFADFF96908CF50505
          0542030303270202021801010108000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000010101080101011002020218030303275A54
          4EA4A09B96F6B4B2B0FFBABAB9FFC0C0C0FFC2C2C2FFC4C4C4FFC4C4C4FFC2C2
          C2FFC0C0C0FFBABAB9FFB2B0AEFF9F9A96F758524CAC04040435030303270202
          0218010101080000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000001010108010101080101
          0110020202180202021F030303270303032E0404043504040435040404350404
          0435040404350303032E030303270202021F0202021801010110010101080000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000101010801010108010101080101010801010110010101100101
          0108010101080101010801010108010101080000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end>
  end
  object ImageListColorSchemesGlyphsSmall: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 29884457
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          000000000000000000020000000800000010000000180000001D0000001E0000
          0019000000110000000800000002000000000000000000000000000000000000
          000000000003000000102B21195569513EA29E7A5CD6C29673F4C29673F49E79
          5DD769513EA42B21195800000011000000030000000000000000000000000000
          000300000013503B2A88AD8260E7D6AB88FFE1BEA1FFEACDB6FFEBCEB7FFE2C0
          A4FFD7AD8BFFAD8461E8503B2A8B000000150000000300000000000000010000
          000E4B342285B9855BF8D3A27BFFE2B896FFE4BC9CFFE5BFA1FFE5C0A2FFE5BF
          A0FFE4BC9CFFD5A680FFBB875EF94B34228A0000001000000001000000062317
          0D4D97643BE5C78D5EFFD59E72FFD7A278FFD9A67EFFDAA982FFDAA983FFDAA8
          81FFD8A57DFFD7A177FFC88F62FF97653DE723170D53000000070000000B5031
          1898B2723FFFC98A57FFCB8E5CFFCC9060FFCD9364FFCE9465FFCE9466FFCE94
          65FFCD9263FFCC9060FFCB8D5CFFB27441FF5132189E0000000E0000000E7241
          1BCEB4723CFFC07E48FFC0804BFFC1814EFFC28350FFC28451FFC28451FFC284
          51FFC28351FFC1824EFFC1814DFFB47440FF72411BD2000000130000000F8B4F
          1FF1B5723DFFB8743FFFB97641FFBB7A46FFBD7E4CFFBF8352FFBF8453FFBE82
          50FFBC7D4AFFBA7943FFB97541FFB5723DFF8B4F1FF2000000150000000D8B4F
          1FF1AF6D37FFB2713BFFB67745FFBA7F50FFBB8253FFBC8355FFBD8457FFBD85
          57FFBD8557FFB87D4CFFB3723EFFAF6D37FF8B4F1FF2000000130000000A7040
          19CAA66330FFB27342FFB98053FFBB8459FFBD885DFFBE8A61FFBF8C63FFC08C
          64FFBF8C63FFBE8A61FFB5794AFFA6632FFF70401ACE0000000E000000064B2B
          118C9C5C28FFB58054FFBB8961FFBE8F69FFC1936FFFC39672FFC49875FFC499
          76FFC49875FFC29672FFBC8C64FF9C5C28FF4B2B119300000009000000021E11
          073E804A1EDEAD784CFFC09572FFC49C7BFFC7A081FFC9A385FFCAA588FFCAA6
          88FFCAA688FFC8A385FFB3825AFF814A1EE11E11074400000004000000000000
          00043D240F70965D2FF6B88B67FFCBA98CFFCFAE94FFD1B299FFD2B49CFFD2B5
          9DFFD2B39BFFBE9575FF986032F63D230F750000000700000001000000000000
          0001000000043F26126E88562EDDB48560FFC6A285FFD4B8A2FFD6BBA5FFC9A6
          8CFFB78A66FF895730DE3F261273000000070000000100000000000000000000
          0000000000000000000221140B3A50321B86794B28C4945D31EE945D31EE794C
          28C551321B8821140B3D00000004000000010000000000000000000000000000
          0000000000000000000000000001000000020000000300000004000000040000
          0004000000030000000100000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          000000000000000000020000000800000010000000180000001D0000001E0000
          0019000000110000000800000002000000000000000000000000000000000000
          000000000003000000101C1C1C55464646A26B6B6BD6858585F4848484F46969
          69D7444444A41B1B1B5800000011000000030000000000000000000000000000
          000300000013303030886D6D6DE78D8D8DFF9B9B9BFFA7A7A7FFA8A8A8FF9C9C
          9CFF8C8C8CFF6A6A6AE82E2E2E8B000000150000000300000000000000010000
          000E28282885656565F8717171FF7D7D7DFF858585FF888888FF898989FF8787
          87FF818181FF737373FF626262F92626268A0000001000000001000000061111
          114D494949E5545454FF5A5A5AFF606060FF646464FF686868FF686868FF6666
          66FF616161FF5C5C5CFF565656FF474747E711111153000000070000000B2828
          2898464646FF414141FF454545FF484848FF4B4B4BFF4D4D4DFF4D4D4DFF4C4C
          4CFF494949FF464646FF434343FF474747FF2828289E0000000E0000000E3A3A
          3ACE393939FF343434FF373737FF383838FF3A3A3AFF3C3C3CFF3C3C3CFF3C3C
          3CFF3A3A3AFF383838FF373737FF3A3A3AFF3A3A3AD2000000130000000F4444
          44F12F2F2FFF2D2D2DFF2F2F2FFF343434FF3A3A3AFF404040FF414141FF3E3E
          3EFF383838FF303030FF2D2D2DFF2F2F2FFF454545F2000000150000000D4141
          41F12D2D2DFF2D2D2DFF383838FF434343FF464646FF494949FF4B4B4BFF4B4B
          4BFF4B4B4BFF3F3F3FFF303030FF2D2D2DFF424242F2000000130000000A3333
          33CA2E2E2EFF363636FF494949FF4F4F4FFF545454FF575757FF5A5A5AFF5B5B
          5BFF5A5A5AFF575757FF404040FF2F2F2FFF333333CE0000000E000000062020
          208C323232FF4B4B4BFF595959FF616161FF676767FF6B6B6BFF6E6E6EFF6F6F
          6FFF6E6E6EFF6A6A6AFF5C5C5CFF333333FF2121219300000009000000021010
          103E393939DE525252FF6C6C6CFF757575FF7C7C7CFF808080FF838383FF8484
          84FF838383FF7F7F7FFF616161FF3B3B3BE11010104400000004000000000000
          000426262670535353F6737373FF898989FF909090FF969696FF989898FF9999
          99FF979797FF818181FF585858F6262626750000000700000001000000000000
          0001000000042D2D2D6E5D5D5DDD818181FF939393FFA5A5A5FFA8A8A8FF9B9B
          9BFF888888FF5F5F5FDE2E2E2E73000000070000000100000000000000000000
          000000000000000000021C1C1C3A42424286616161C4777777EE777777EE6262
          62C5434343881C1C1C3D00000004000000010000000000000000000000000000
          0000000000000000000000000001000000020000000300000004000000040000
          0004000000030000000100000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000004000000100000001F0000002E00000037000000390000
          0030000000210000001000000004000000000000000000000000000000000000
          0000000000060000001F4F48438EA19891DEC3BDB9F9CFCDCCFFCFCDCCFFC2BD
          B9F9A19891DF4E48439200000021000000060000000000000000000000000000
          00060000002581766FC8C7C2BFFDD7D7D7FFE0E0E0FFE7E7E7FFE8E8E8FFE1E1
          E1FFD8D8D8FFC6C3BFFD7F766ECB000000290000000600000000000000020000
          001C756A61C5C5C4C3FFD3D3D3FFDDDDDDFFDFDFDFFFE1E1E1FFE1E1E1FFE1E1
          E1FFDFDFDFFFD5D5D5FFC6C5C4FF746961CA0000001F000000020000000C3C34
          2D83ABA6A1FDC9C9C9FFD1D1D1FFD3D3D3FFD5D5D5FFD6D6D6FFD6D6D6FFD6D6
          D6FFD4D4D4FFD3D3D3FFCACACAFFABA6A2FD3B332D8B0000000E000000166E61
          56D6B2B2B2FFC7C7C7FFC9C9C9FFCACACAFFCCCCCCFFCCCCCCFFCCCCCCFFCCCC
          CCFFCBCBCBFFCACACAFFC9C9C9FFB4B4B4FF6C6056DB0000001C0000001C7D75
          6DF6B2B2B2FFC1C1C1FFC2C2C2FFC3C3C3FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4
          C4FFC4C4C4FFC3C3C3FFC3C3C3FFB5B5B5FF7C736CF8000000250000001E8582
          7FFFB3B3B3FFB6B6B6FFB9B9B9FFBEBEBEFFC1C1C1FFC4C4C4FFC4C4C4FFC3C3
          C3FFC1C1C1FFBCBCBCFFB8B8B8FFB3B3B3FF84817EFF000000290000001A8582
          7FFFAAAAAAFFB0B0B0FFBABABAFFC2C2C2FFC3C3C3FFC4C4C4FFC4C4C4FFC4C4
          C4FFC4C4C4FFC0C0C0FFB2B2B2FFAAAAAAFF84817EFF00000025000000147C72
          6AF49D9D9DFFB4B4B4FFC2C2C2FFC4C4C4FFC6C6C6FFC7C7C7FFC8C8C8FFC8C8
          C8FFC8C8C8FFC7C7C7FFBDBDBDFF9D9D9DFF7B726AF60000001C0000000C6656
          4BCC919191FFC2C2C2FFC6C6C6FFC9C9C9FFCBCBCBFFCDCDCDFFCECECEFFCECE
          CEFFCECECEFFCDCDCDFFC8C8C8FF919191FF64564BD200000012000000043228
          206D857E78FBB9B9B9FFCCCCCCFFCFCFCFFFD1D1D1FFD3D3D3FFD4D4D4FFD4D4
          D4FFD4D4D4FFD3D3D3FFC3C3C3FF847E79FC3228207600000008000000000000
          00085A4C41AF979593FFC7C7C7FFD5D5D5FFD8D8D8FFDADADAFFDBDBDBFFDBDB
          DBFFDBDBDBFFCCCCCCFF9B9997FF594B40B50000000E00000002000000000000
          0002000000085F5145AD99928DFBC4C4C4FFD2D2D2FFDDDDDDFFDEDEDEFFD5D5
          D5FFC7C7C7FF9B948FFB5E5045B30000000E0000000200000000000000000000
          000000000000000000043930286775675CC692887FF29C9895FE9C9895FE9288
          7FF274665BC8392F286C00000008000000020000000000000000000000000000
          0000000000000000000000000002000000040000000600000008000000080000
          0008000000060000000200000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000004000000100000001F0000002E00000037000000390000
          0030000000210000001000000004000000000000000000000000000000000000
          0000000000060000001F3E38338E7A7069DE96918CF9A2A09FFFA2A09FFF9590
          8BF9797069DF3E37329200000021000000060000000000000000000000000000
          000600000025635951C8979390FDAFAFAFFFC1C1C1FFD0D0D0FFD1D1D1FFC3C3
          C3FFB1B1B1FF979390FD615850CB000000290000000600000000000000020000
          001C5A4F46C58F8D8CFFA7A7A7FFBCBCBCFFC0C0C0FFC3C3C3FFC3C3C3FFC2C2
          C2FFC0C0C0FFABABABFF908E8DFF584E46CA0000001F000000020000000C3028
          218378736FFD929292FFA4A4A4FFA7A7A7FFACACACFFAEAEAEFFAFAFAFFFAEAE
          AEFFAAAAAAFFA7A7A7FF959595FF78736FFD3028218B0000000E000000165548
          3ED6787878FF909090FF939393FF969696FF989898FF999999FF9A9A9AFF9999
          99FF989898FF969696FF939393FF797979FF52463DDB0000001C0000001C5D53
          4CF6787878FF848484FF868686FF878787FF898989FF898989FF898989FF8989
          89FF898989FF878787FF878787FF7A7A7AFF5A524BF8000000250000001E5D59
          56FF797979FF7C7C7CFF7D7D7DFF808080FF858585FF888888FF898989FF8787
          87FF838383FF7F7F7FFF7D7D7DFF797979FF5B5856FF000000290000001A5D59
          56FF737373FF777777FF7E7E7EFF858585FF878787FF888888FF8A8A8AFF8A8A
          8AFF8A8A8AFF828282FF787878FF737373FF5B5856FF00000025000000145C51
          4AF46B6B6BFF7A7A7AFF868686FF8A8A8AFF8D8D8DFF8F8F8FFF919191FF9292
          92FF919191FF8F8F8FFF808080FF6B6B6BFF5A5149F60000001C0000000C5042
          36CC626262FF858585FF8E8E8EFF939393FF989898FF9A9A9AFF9D9D9DFF9D9D
          9DFF9D9D9DFF9A9A9AFF909090FF626262FF4D4036D200000012000000042920
          186D5E5852FB7D7D7DFF999999FF9F9F9FFFA4A4A4FFA7A7A7FFA9A9A9FFA9A9
          A9FFA9A9A9FFA7A7A7FF878787FF5E5853FC291F187600000008000000000000
          0008483A2FAF686665FF8F8F8FFFACACACFFB1B1B1FFB5B5B5FFB7B7B7FFB7B7
          B7FFB6B6B6FF999999FF6A6867FF47392EB50000000E00000002000000000000
          0002000000084B3E32AD6D6660FB8A8A8AFFA6A6A6FFBBBBBBFFBEBEBEFFABAB
          ABFF8F8F8FFF6E6762FB4A3D32B30000000E0000000200000000000000000000
          000000000000000000042F261E675C4E42C66B6159F26D6966FE6D6966FE6B61
          59F25B4D42C82F251E6C00000008000000020000000000000000000000000000
          0000000000000000000000000002000000040000000600000008000000080000
          0008000000060000000200000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000004000000100000001F0000002E00000037000000390000
          0030000000210000001000000004000000000000000000000000000000000000
          0000000000060000001F4F48438EA19891DEC3BDB9F9CFCDCCFFCFCDCCFFC2BD
          B9F9A19891DF4E48439200000021000000060000000000000000000000000000
          00060000002581766FC8C7C2BFFDD7D7D7FFE0E0E0FFE7E7E7FFE8E8E8FFE1E1
          E1FFD8D8D8FFC6C3BFFD7F766ECB000000290000000600000000000000020000
          001C756A61C5C5C4C3FFD3D3D3FFDDDDDDFFDFDFDFFFE1E1E1FFE1E1E1FFE1E1
          E1FFDFDFDFFFD5D5D5FFC6C5C4FF746961CA0000001F000000020000000C3C34
          2D83ABA6A1FDC9C9C9FFD1D1D1FFD3D3D3FFD5D5D5FFD6D6D6FFD6D6D6FFD6D6
          D6FFD4D4D4FFD3D3D3FFCACACAFFABA6A2FD3B332D8B0000000E000000166E61
          56D6B2B2B2FFC7C7C7FFC9C9C9FFCACACAFFCCCCCCFFCCCCCCFFCCCCCCFFCCCC
          CCFFCBCBCBFFCACACAFFC9C9C9FFB4B4B4FF6C6056DB0000001C0000001C7D75
          6DF6B2B2B2FFC1C1C1FFC2C2C2FFC3C3C3FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4
          C4FFC4C4C4FFC3C3C3FFC3C3C3FFB5B5B5FF7C736CF8000000250000001E8582
          7FFFB3B3B3FFB6B6B6FFB9B9B9FFBEBEBEFFC1C1C1FFC4C4C4FFC4C4C4FFC3C3
          C3FFC1C1C1FFBCBCBCFFB8B8B8FFB3B3B3FF84817EFF000000290000001A8582
          7FFFAAAAAAFFB0B0B0FFBABABAFFC2C2C2FFC3C3C3FFC4C4C4FFC4C4C4FFC4C4
          C4FFC4C4C4FFC0C0C0FFB2B2B2FFAAAAAAFF84817EFF00000025000000147C72
          6AF49D9D9DFFB4B4B4FFC2C2C2FFC4C4C4FFC6C6C6FFC7C7C7FFC8C8C8FFC8C8
          C8FFC8C8C8FFC7C7C7FFBDBDBDFF9D9D9DFF7B726AF60000001C0000000C6656
          4BCC919191FFC2C2C2FFC6C6C6FFC9C9C9FFCBCBCBFFCDCDCDFFCECECEFFCECE
          CEFFCECECEFFCDCDCDFFC8C8C8FF919191FF64564BD200000012000000043228
          206D857E78FBB9B9B9FFCCCCCCFFCFCFCFFFD1D1D1FFD3D3D3FFD4D4D4FFD4D4
          D4FFD4D4D4FFD3D3D3FFC3C3C3FF847E79FC3228207600000008000000000000
          00085A4C41AF979593FFC7C7C7FFD5D5D5FFD8D8D8FFDADADAFFDBDBDBFFDBDB
          DBFFDBDBDBFFCCCCCCFF9B9997FF594B40B50000000E00000002000000000000
          0002000000085F5145AD99928DFBC4C4C4FFD2D2D2FFDDDDDDFFDEDEDEFFD5D5
          D5FFC7C7C7FF9B948FFB5E5045B30000000E0000000200000000000000000000
          000000000000000000043930286775675CC692887FF29C9895FE9C9895FE9288
          7FF274665BC8392F286C00000008000000020000000000000000000000000000
          0000000000000000000000000002000000040000000600000008000000080000
          0008000000060000000200000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          000000000000000000080202021F0303033B0505055406060663060606660505
          05570303033E0202021F00000008000000000000000000000000000000000000
          00000101010C0303033B7F7771CDCCC4BFFBE0DDDBFFEAE9E9FFEAE9E9FFE0DD
          DAFFCBC3BEFB7F7770D10303033E0101010C0000000000000000000000000101
          010C04040445B0A69FF4E2E0DEFFF3F3F3FFFCFCFCFFFFFFFFFFFFFFFFFFFDFD
          FDFFF4F4F4FFE1E0DEFFADA49DF50404044C0101010C00000000000000040303
          0335A2978FF2E1E0E0FFEFEFEFFFF9F9F9FFFBFBFBFFFDFDFDFFFDFDFDFFFDFD
          FDFFFBFBFBFFF1F1F1FFE2E1E1FF9F958DF40303033B0000000401010118695B
          4FC3C5C2BFFFE5E5E5FFEDEDEDFFEFEFEFFFF1F1F1FFF2F2F2FFF2F2F2FFF2F2
          F2FFF0F0F0FFEFEFEFFFE6E6E6FFC6C2BFFF675A4ECB0202021C0202022B9689
          7DF9CECECEFFE3E3E3FFE5E5E5FFE6E6E6FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
          E8FFE7E7E7FFE6E6E6FFE5E5E5FFD0D0D0FF93857AFA03030335030303359C94
          8CFFCECECEFFDDDDDDFFDEDEDEFFDFDFDFFFE0E0E0FFE0E0E0FFE0E0E0FFE0E0
          E0FFE0E0E0FFDFDFDFFFDFDFDFFFD1D1D1FF99918BFF0404044503030339A09E
          9CFFCFCFCFFFD2D2D2FFD5D5D5FFDADADAFFDDDDDDFFE0E0E0FFE0E0E0FFDFDF
          DFFFDDDDDDFFD8D8D8FFD4D4D4FFCFCFCFFF9F9D9BFF0404044C03030332A09E
          9CFFC6C6C6FFCCCCCCFFD6D6D6FFDEDEDEFFDFDFDFFFE0E0E0FFE0E0E0FFE0E0
          E0FFE0E0E0FFDCDCDCFFCECECEFFC6C6C6FF9F9D9BFF04040445020202279B91
          8AFFB9B9B9FFD0D0D0FFDEDEDEFFE0E0E0FFE2E2E2FFE3E3E3FFE4E4E4FFE4E4
          E4FFE4E4E4FFE3E3E3FFD9D9D9FFB9B9B9FF9A9189FF0303033501010118917E
          6FF5ADADADFFDEDEDEFFE2E2E2FFE5E5E5FFE7E7E7FFE9E9E9FFEAEAEAFFEAEA
          EAFFEAEAEAFFE9E9E9FFE4E4E4FFADADADFF8E7C6EF802020223000000085E4B
          3CACA19B96FFD5D5D5FFE8E8E8FFEBEBEBFFEDEDEDFFEFEFEFFFF0F0F0FFF0F0
          F0FFF0F0F0FFEFEFEFFFDFDFDFFFA09A96FF5C4A3CB601010110000000000101
          0110897768E6B3B1B0FFE3E3E3FFF1F1F1FFF4F4F4FFF6F6F6FFF7F7F7FFF7F7
          F7FFF7F7F7FFE8E8E8FFB6B5B4FF887565EA0202021C00000004000000000000
          0004010101108F7E6FE5B4AFACFFE0E0E0FFEEEEEEFFF9F9F9FFFAFAFAFFF1F1
          F1FFE3E3E3FFB6B1AEFF8D7B6FE90202021C0000000400000000000000000000
          0000000000000000000866584DA5A29489F3B2A9A3FFB7B4B2FFB7B4B2FFB2A9
          A3FFA09288F465574BAB01010110000000040000000000000000000000000000
          0000000000000000000000000004000000080101010C01010110010101100101
          01100101010C0000000400000000000000000000000000000000}
      end>
  end
  object ImageListSkin: TImageList
    ColorDepth = cd32Bit
    Left = 44
    Top = 184
  end
  object ImageListSkinLarge: TImageList
    ColorDepth = cd32Bit
    Height = 48
    Width = 48
    Left = 36
    Top = 232
  end
  object LayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 40
    Top = 368
    object LayoutSkinLookAndFeel: TdxLayoutSkinLookAndFeel
      GroupOptions.CaptionOptions.TextDisabledColor = clWindowText
      ItemOptions.CaptionOptions.TextDisabledColor = clWindowText
      Offsets.RootItemsAreaOffsetHorz = 0
      Offsets.RootItemsAreaOffsetVert = 0
      PixelsPerInch = 96
    end
    object LayoutSkinLookAndFeelURL: TdxLayoutSkinLookAndFeel
      ItemOptions.CaptionOptions.HotTrack = True
      ItemOptions.CaptionOptions.TextHotColor = clBlue
      Offsets.RootItemsAreaOffsetHorz = 0
      Offsets.RootItemsAreaOffsetVert = 0
      PixelsPerInch = 96
    end
    object LayoutSkinLookAndFeelTitle: TdxLayoutSkinLookAndFeel
      ItemOptions.CaptionOptions.Font.Charset = DEFAULT_CHARSET
      ItemOptions.CaptionOptions.Font.Color = clWindowText
      ItemOptions.CaptionOptions.Font.Height = -11
      ItemOptions.CaptionOptions.Font.Name = 'Segoe UI'
      ItemOptions.CaptionOptions.Font.Style = [fsBold]
      ItemOptions.CaptionOptions.UseDefaultFont = False
      Offsets.RootItemsAreaOffsetHorz = 0
      Offsets.RootItemsAreaOffsetVert = 0
      PixelsPerInch = 96
    end
    object LayoutSkinLookAndFeelGroup: TdxLayoutSkinLookAndFeel
      ItemOptions.Padding.AssignedValues = [lpavLeft]
      ItemOptions.Padding.Left = 12
      PixelsPerInch = 96
    end
    object LayoutSkinLookAndFeelStandard: TdxLayoutSkinLookAndFeel
      PixelsPerInch = 96
    end
  end
  object PopupMenuFolderReset: TPopupMenu
    Left = 44
    Top = 280
    object Reset1: TMenuItem
      Action = ActionFolderReset
      Hint = 'Reset selected file location to the default value'
    end
    object Resetall1: TMenuItem
      Action = ActionFolderResetAll
      Hint = 'Reset all file locations to their default values'
    end
  end
  object StyleRepository: TcxStyleRepository
    Left = 44
    Top = 324
    PixelsPerInch = 96
    object StyleBackground: TcxStyle
      AssignedValues = [svColor]
      Color = clWhite
    end
    object StyleDisabled: TcxStyle
      AssignedValues = [svTextColor]
      TextColor = clGrayText
    end
  end
  object ImageList: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 33030184
    ImageInfo = <
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000000F744558745469746C6500436865636B426F783B9B171E
          3B0000024749444154785E8D93CF4B545114C73FE7CD9BE764560445454591F6
          63D47E6960BF1C5D48D8A2454114812DDAD4AAFFA15DCB5A44D4AAA0B05611B4
          283221A3C4323550432A24C4C97E204E924D33EFDD9373EFA30171D1810BE75C
          CEF7C3F79E7BAF0002240097FF7F1820F281C4B5CEFE67092F9141416384CD55
          1D535C8D2A26CE4D54ECB9D8D1D4E6039EE065CE9DD845DCC9C250C043898C01
          71466FDC1FCA009E0F486414C035006A9DE83F96A0E40B050AF32B190404C900
          E3346201AE501C27B6AE20AA88C0F7E91C371F8EA05E92D6BA2A1AEA371345A6
          0C2815C6360B188331C6E68A323935C3F507C3B4B5A449ADACE26E671FE9EAB5
          846108205E0C009CB8BB7F9CCBB7FA78DAFB81B14F592B3E767437F9600977EE
          BDA6355D491054101F1B1FB07654959E81CFBCFD38C381A66A0686B33CEA9DE4
          F4F1BD647F43EFCB51B6AE8C3872B8968A8A547906CE81DA8D1555015FA77F91
          23A039B39DF6C0E7FDD41C23A313E88F2F749C6F2648A51084794DF908A131A8
          08B55B5673F2D07A9E3C1E64686296BEF11CD96F39C6DE8C70E154039595CB09
          2330501E2240142A1A29A1F1D893DEC0994281DB5DEF6868D9C9ABAE41CEB6D7
          B06ECD2A42671B83730CE0AED14E1E3B1823C2BEFA4D148A45AE743E67FF8E65
          1C6CACC1888FD5286074C135960018544B4BF048CC8BB6D158B7918424F04A53
          8F4051E227EE6031000B50F044E2572816E4074B01271011448801609C03F581
          E26CEE67CFA5ABDD195501EB82381435A0314401412DE04F7EEE05100AE00129
          20B9C87716160F050A40FE2F0C443CDF350F545E0000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000015744558745469746C6500436865636B3B4D61726B3B5469636B0B00
          C7240000026B49444154785E95924D48156B18C77F338D7A94B436816090828B
          8B94919BBE886304C5599CEA9A14145177211246AB3641F6B16861044610B408
          23C842FB2410B4ACB06BDDD55510823072532267A19E93E76BE6FDCAF3323910
          44F4303CFF7786797EFFE77978BD9B2FF6BE715DA7955F8589040CC63818345A
          99B13389D13D5EA9B87DDB05FE341EFC7B310EB89E5286824A912ABC8FDCA2F4
          F3BB9575952D48A9019C6580466B89D63FAAED4F51B11558585CA27A75259EE7
          A2748092060B2891941168AD4213EB132930323AC1FF935FA85B5FC3F12371A4
          F6114202E009A12C512AB5E2684AC931F67C7F708CB565BBB8717690AEAB5B48
          67965853E12382B003DF170855442AC9CC4C8AFEC7236C6DD9C4EE783323AF26
          A8ADDEC7A9F66EAEF49DA03D99205659812F0A148A3E165022E5FD1CD97C9EFE
          872FE9E9FA8F47AF6ED1737D809D1B4FD2D176817BC397A9AD536C68A822574C
          53EE6411225C621048842C10C880B644923BCFBA39DF7997C9E9C3FC55BF994C
          EE33E3538374FC93643E9BB2E355951510221C41488D6F013E750D352C6632DC
          7E728ED3477BC175B874AD93FD895616B2B380C618F0450E1128003C19683B93
          54826F6A9EA6E675BC1B9FE2E9EB5E1AEB9B70631962D5057C195877005FE6A3
          7B2084A628F30825D00616E51CDB7734F27CA88FBEE14F1C3BF437393F83C158
          773025C368075228029127500227BC44F3B9AF1C4CB652597E80B9F4478A4202
          7A05501405A20EA4B11DC4BC1AB02E1A6D0CE9FC1C0BB95930066F5585FD8E01
          6DECCE501680F18A3931363CF4210E267C88344CB6D605B4E5E1601081790B48
          6FA0673A017880CBEFC309D50002F0BF03C5148A5699E7E19C0000000049454E
          44AE426082}
      end>
  end
  object ColorDialog: TdxColorDialog
    Options.ColorPicker.AllowEditAlpha = False
    Options.ColorPicker.RGBHexNotation = cphnDelphi
    Options.ColorPicker.DefaultVisible = True
    Left = 102
    Top = 141
  end
end
