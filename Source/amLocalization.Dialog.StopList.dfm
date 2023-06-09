inherited FormStopList: TFormStopList
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeable
  Caption = 'Stop List'
  ClientHeight = 528
  ClientWidth = 655
  OnShow = FormShow
  ExplicitWidth = 671
  ExplicitHeight = 566
  PixelsPerInch = 96
  TextHeight = 15
  inherited LayoutControlButtons: TdxLayoutControl
    Top = 473
    Width = 655
    ExplicitTop = 473
    ExplicitWidth = 655
    inherited ButtonOK: TcxButton
      Left = 485
      TabOrder = 2
      ExplicitLeft = 485
    end
    inherited ButtonCancel: TcxButton
      Left = 567
      TabOrder = 3
      ExplicitLeft = 567
    end
    object ButtonImport: TcxButton [2]
      Left = 13
      Top = 13
      Width = 75
      Height = 25
      Action = ActionImport
      TabOrder = 0
    end
    object ButtonExport: TcxButton [3]
      Left = 95
      Top = 13
      Width = 75
      Height = 25
      Action = ActionExport
      TabOrder = 1
    end
    inherited LayoutItemButtonOK: TdxLayoutItem
      Index = 2
    end
    inherited LayoutItemButtonCancel: TdxLayoutItem
      Index = 3
    end
    inherited LayoutGroupButtons: TdxLayoutGroup
      CaptionOptions.Visible = False
      ItemIndex = 1
    end
    object LayoutItemButtonImport: TdxLayoutItem
      Parent = LayoutGroupButtons
      CaptionOptions.Visible = False
      Control = ButtonImport
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object LayoutItemButtonExport: TdxLayoutItem
      Parent = LayoutGroupButtons
      CaptionOptions.Visible = False
      Control = ButtonExport
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  inherited LayoutControlHeader: TdxLayoutControl
    Width = 655
    Visible = True
    ExplicitWidth = 655
    inherited LayoutItemHeader: TdxLayoutLabeledItem
      CaptionOptions.Text = 'List of items that should never be translated'
    end
  end
  inherited LayoutControl: TdxLayoutControl
    Width = 655
    Height = 428
    ExplicitWidth = 655
    ExplicitHeight = 428
    object TreeListStopList: TcxTreeList [0]
      Left = 7
      Top = 7
      Width = 641
      Height = 414
      Bands = <
        item
        end>
      DragMode = dmAutomatic
      Navigator.Buttons.CustomButtons = <>
      OptionsBehavior.GoToNextCellOnTab = True
      OptionsBehavior.FocusCellOnCycle = True
      OptionsCustomizing.ColumnFiltering = bFalse
      OptionsCustomizing.ColumnVertSizing = False
      OptionsCustomizing.StackedColumns = False
      OptionsData.AnsiSort = True
      OptionsSelection.MultiSelect = True
      OptionsView.CategorizedColumn = TreeListStopListColumnGroup
      OptionsView.CheckGroups = True
      OptionsView.PaintStyle = tlpsCategorized
      OptionsView.TreeLineStyle = tllsNone
      ScrollbarAnnotations.CustomAnnotations = <>
      TabOrder = 0
      OnBeginDragNode = TreeListStopListBeginDragNode
      OnDeletion = TreeListStopListDeletion
      OnDragOver = TreeListStopListDragOver
      OnInitEditValue = TreeListStopListInitEditValue
      OnKeyDown = TreeListStopListKeyDown
      OnMoveTo = TreeListStopListMoveTo
      OnNodeChanged = TreeListStopListNodeChanged
      OnNodeCheckChanged = TreeListStopListNodeCheckChanged
      object TreeListStopListColumnGroup: TcxTreeListColumn
        PropertiesClassName = 'TcxComboBoxProperties'
        Properties.OnInitPopup = TreeListStopListColumnGroupPropertiesInitPopup
        Properties.OnValidate = TreeListStopListColumnGroupPropertiesValidate
        Caption.Text = 'Group'
        Width = 140
        Position.ColIndex = 0
        Position.RowIndex = 0
        Position.BandIndex = 0
        SortOrder = soAscending
        SortIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
        OnGetDisplayText = TreeListStopListColumnGroupGetDisplayText
        OnGetEditingProperties = TreeListStopListColumnGroupGetEditingProperties
      end
      object TreeListStopListColumnField: TcxTreeListColumn
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
          end
          item
            Description = 'Type, Name and Value'
            Value = 6
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
      object TreeListStopListColumnOperator: TcxTreeListColumn
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
      object TreeListStopListColumnValue: TcxTreeListColumn
        PropertiesClassName = 'TcxTextEditProperties'
        Properties.ValidateOnEnter = True
        Properties.ValidationOptions = [evoShowErrorIcon]
        Properties.OnValidate = TreeListStopListColumnValuePropertiesValidate
        Caption.Text = 'Value'
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
      Control = TreeListStopList
      ControlOptions.OriginalHeight = 150
      ControlOptions.OriginalWidth = 250
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
  inherited ActionList: TActionList
    object ActionImport: TAction
      Caption = 'Import...'
      OnExecute = ActionImportExecute
    end
    object ActionExport: TAction
      Caption = 'Export...'
      OnExecute = ActionExportExecute
    end
  end
  object EditRepository: TcxEditRepository
    Left = 368
    Top = 123
    PixelsPerInch = 96
    object EditRepositoryTextItem: TcxEditRepositoryTextItem
      Properties.ValidateOnEnter = True
      Properties.OnValidate = TreeListStopListColumnGroupPropertiesValidate
    end
  end
  object TaskDialogImport: TTaskDialog
    Buttons = <
      item
        Caption = 'Replace'
        Default = True
        CommandLinkHint = 'Clear stop list and then add the imported stop list entries'
        ModalResult = 100
      end
      item
        Caption = 'Merge'
        CommandLinkHint = 'Add the imported entries to the current stop list'
        ModalResult = 101
      end>
    Caption = 'Import Stop List'
    CommonButtons = [tcbCancel]
    Flags = [tfAllowDialogCancellation, tfUseCommandLinksNoIcon]
    RadioButtons = <>
    Text = 
      'Do you want to replace your current stop list or merge the selec' +
      'ted file into it?'
    Title = 'Replace or Merge Stop List'
    Left = 128
    Top = 135
  end
  object OpenDialogStopList: TOpenDialog
    Filter = 'Stop lists (*.xstp)|*.xspt|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 120
    Top = 224
  end
  object SaveDialogStopList: TSaveDialog
    DefaultExt = 'xstp'
    Filter = 'Stop lists (*.xstp)|*.xspt|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 232
    Top = 224
  end
end
