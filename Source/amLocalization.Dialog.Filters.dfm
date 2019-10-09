inherited FormFilters: TFormFilters
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeable
  Caption = 'Black List'
  ClientHeight = 528
  ClientWidth = 655
  OnShow = FormShow
  ExplicitWidth = 671
  ExplicitHeight = 566
  PixelsPerInch = 96
  TextHeight = 13
  inherited LayoutControlButtons: TdxLayoutControl
    Top = 480
    Width = 655
    ExplicitTop = 480
    ExplicitWidth = 655
    inherited ButtonOK: TcxButton
      Left = 488
      ExplicitLeft = 488
    end
    inherited ButtonCancel: TcxButton
      Left = 569
      ExplicitLeft = 569
    end
  end
  inherited LayoutControlHeader: TdxLayoutControl
    Width = 655
    ExplicitWidth = 655
    inherited LabelHeader: TcxLabel
      Caption = 'List of items that should never be translated'
      ExplicitWidth = 633
      Width = 633
    end
  end
  inherited PanelMain: TPanel
    Width = 655
    Height = 441
    ExplicitTop = 39
    ExplicitWidth = 655
    ExplicitHeight = 441
    inherited LayoutControl: TdxLayoutControl
      Width = 639
      Height = 429
      ExplicitLeft = 8
      ExplicitWidth = 639
      ExplicitHeight = 429
      object TreeListFilters: TcxTreeList [0]
        Left = 6
        Top = 6
        Width = 627
        Height = 417
        Bands = <
          item
          end>
        DragMode = dmAutomatic
        Navigator.Buttons.CustomButtons = <>
        OptionsBehavior.GoToNextCellOnEnter = True
        OptionsBehavior.GoToNextCellOnTab = True
        OptionsBehavior.FocusCellOnCycle = True
        OptionsCustomizing.ColumnFiltering = bFalse
        OptionsCustomizing.ColumnVertSizing = False
        OptionsCustomizing.StackedColumns = False
        OptionsData.AnsiSort = True
        OptionsSelection.MultiSelect = True
        OptionsView.CategorizedColumn = TreeListFiltersColumnGroup
        OptionsView.CheckGroups = True
        OptionsView.PaintStyle = tlpsCategorized
        OptionsView.TreeLineStyle = tllsNone
        TabOrder = 0
        OnBeginDragNode = TreeListFiltersBeginDragNode
        OnDeletion = TreeListFiltersDeletion
        OnDragOver = TreeListFiltersDragOver
        OnInitEditValue = TreeListFiltersInitEditValue
        OnKeyDown = TreeListFiltersKeyDown
        OnMoveTo = TreeListFiltersMoveTo
        OnNodeChanged = TreeListFiltersNodeChanged
        OnNodeCheckChanged = TreeListFiltersNodeCheckChanged
        object TreeListFiltersColumnGroup: TcxTreeListColumn
          PropertiesClassName = 'TcxComboBoxProperties'
          Caption.Text = 'Group'
          DataBinding.ValueType = 'String'
          Width = 140
          Position.ColIndex = 0
          Position.RowIndex = 0
          Position.BandIndex = 0
          SortOrder = soAscending
          SortIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
          OnGetDisplayText = TreeListFiltersColumnGroupGetDisplayText
          OnGetEditingProperties = TreeListFiltersColumnGroupGetEditingProperties
        end
        object TreeListFiltersColumnField: TcxTreeListColumn
          PropertiesClassName = 'TcxImageComboBoxProperties'
          Properties.Items = <
            item
              Description = 'Module'
              Value = 0
            end
            item
              Description = 'Element'
              Value = 1
            end
            item
              Description = 'Type'
              Value = 2
            end
            item
              Description = 'Name'
              Value = 3
            end
            item
              Description = 'Type and Name'
              Value = 4
            end
            item
              Description = 'Value'
              Value = 5
            end>
          BestFitMaxWidth = 200
          Caption.Text = 'Field'
          DataBinding.ValueType = 'Integer'
          Width = 100
          Position.ColIndex = 1
          Position.RowIndex = 0
          Position.BandIndex = 0
          SortOrder = soAscending
          SortIndex = 1
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
        object TreeListFiltersColumnOperator: TcxTreeListColumn
          PropertiesClassName = 'TcxImageComboBoxProperties'
          Properties.Items = <
            item
              Description = 'equals'
              Value = 0
            end
            item
              Description = 'starts with'
              Value = 1
            end
            item
              Description = 'ends with'
              Value = 2
            end
            item
              Description = 'contains'
              Value = 3
            end
            item
              Description = 'RegExp'
              Value = 4
            end>
          BestFitMaxWidth = 60
          Caption.Text = 'Operator'
          DataBinding.ValueType = 'Integer'
          Options.Sorting = False
          Width = 60
          Position.ColIndex = 2
          Position.RowIndex = 0
          Position.BandIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
        object TreeListFiltersColumnValue: TcxTreeListColumn
          PropertiesClassName = 'TcxTextEditProperties'
          Caption.Text = 'Value'
          DataBinding.ValueType = 'String'
          Options.Sorting = False
          Width = 300
          Position.ColIndex = 3
          Position.RowIndex = 0
          Position.BandIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
      end
      inherited LayoutControlGroup_Root: TdxLayoutGroup
        AlignVert = avClient
      end
      object LayoutItemFilters: TdxLayoutItem
        Parent = LayoutControlGroup_Root
        AlignVert = avClient
        CaptionOptions.Visible = False
        Control = TreeListFilters
        ControlOptions.OriginalHeight = 150
        ControlOptions.OriginalWidth = 250
        ControlOptions.ShowBorder = False
        Index = 0
      end
    end
  end
  object EditRepository: TcxEditRepository
    Left = 368
    Top = 123
    PixelsPerInch = 96
    object EditRepositoryTextItem: TcxEditRepositoryTextItem
    end
  end
end
