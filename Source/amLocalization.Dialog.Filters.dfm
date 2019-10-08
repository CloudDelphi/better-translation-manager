inherited FormFilters: TFormFilters
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeable
  Caption = 'Filters'
  ClientHeight = 528
  ClientWidth = 625
  ExplicitWidth = 641
  ExplicitHeight = 566
  PixelsPerInch = 96
  TextHeight = 13
  inherited LayoutControlButtons: TdxLayoutControl
    Top = 480
    Width = 625
    ExplicitTop = 480
    ExplicitWidth = 625
    inherited ButtonOK: TcxButton
      Left = 458
      ExplicitLeft = 458
    end
    inherited ButtonCancel: TcxButton
      Left = 539
      ExplicitLeft = 539
    end
  end
  inherited LayoutControlHeader: TdxLayoutControl
    Width = 625
    ExplicitWidth = 625
    inherited LabelHeader: TcxLabel
      Caption = 'List of items that should never be translated'
      ExplicitWidth = 603
      Width = 603
    end
  end
  inherited PanelMain: TPanel
    Width = 625
    Height = 441
    ExplicitTop = 39
    ExplicitWidth = 625
    ExplicitHeight = 441
    inherited LayoutControl: TdxLayoutControl
      Width = 609
      Height = 429
      ExplicitLeft = 8
      ExplicitWidth = 609
      ExplicitHeight = 429
      object TreeListFilters: TcxTreeList [0]
        Left = 6
        Top = 6
        Width = 597
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
          PropertiesClassName = 'TcxTextEditProperties'
          Caption.Text = 'Group'
          DataBinding.ValueType = 'String'
          Width = 100
          Position.ColIndex = 0
          Position.RowIndex = 0
          Position.BandIndex = 0
          SortOrder = soAscending
          SortIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
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
              Description = 'Value'
              Value = 4
            end>
          BestFitMaxWidth = 200
          Caption.Text = 'Field'
          DataBinding.ValueType = 'Integer'
          Width = 120
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
end
