object DataModuleMain: TDataModuleMain
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 237
  Width = 276
  object ClientDataSetLanguages: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 64
    Top = 17
    object ClientDataSetLanguagesLocaleID: TWordField
      FieldName = 'LocaleID'
    end
    object ClientDataSetLanguagesLocaleName: TStringField
      FieldName = 'LocaleName'
      Size = 10
    end
    object ClientDataSetLanguagesLanguageName: TStringField
      FieldName = 'LanguageName'
      Size = 40
    end
    object ClientDataSetLanguagesCountryName: TStringField
      FieldName = 'CountryName'
      Size = 40
    end
  end
  object DataSourceLanguages: TDataSource
    AutoEdit = False
    DataSet = ClientDataSetLanguages
    Left = 64
    Top = 69
  end
  object GridViewRepository: TcxGridViewRepository
    Left = 60
    Top = 125
    object GridTableViewLanguages: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = DataSourceLanguages
      DataController.KeyFieldNames = 'LocaleID'
      DataController.Options = [dcoAnsiSort, dcoCaseInsensitive, dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding]
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsBehavior.IncSearchItem = GridTableViewLanguagesColumnLocaleName
      OptionsData.Deleting = False
      OptionsData.Editing = False
      OptionsData.Inserting = False
      OptionsSelection.CellSelect = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupByBox = False
      object GridTableViewLanguagesColumnLocaleID: TcxGridDBColumn
        Caption = 'Locale ID'
        DataBinding.FieldName = 'LocaleID'
      end
      object GridTableViewLanguagesColumnLocaleName: TcxGridDBColumn
        Caption = 'Locale'
        DataBinding.FieldName = 'LocaleName'
      end
      object GridTableViewLanguagesColumnLanguage: TcxGridDBColumn
        Caption = 'Language'
        DataBinding.FieldName = 'LanguageName'
        SortIndex = 0
        SortOrder = soAscending
      end
      object GridTableViewLanguagesColumnCountry: TcxGridDBColumn
        Caption = 'Country'
        DataBinding.FieldName = 'CountryName'
        Visible = False
      end
    end
    object GridTableViewTargetLanguages: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = DataSourceLanguages
      DataController.Filter.Active = True
      DataController.KeyFieldNames = 'LocaleID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      DataController.OnFilterRecord = GridTableViewTargetLanguagesDataControllerFilterRecord
      object GridTableViewTargetLanguagesLocaleID: TcxGridDBColumn
        DataBinding.FieldName = 'LocaleID'
      end
      object GridTableViewTargetLanguagesLocaleName: TcxGridDBColumn
        DataBinding.FieldName = 'LocaleName'
      end
      object GridTableViewTargetLanguagesLanguageName: TcxGridDBColumn
        DataBinding.FieldName = 'LanguageName'
        SortIndex = 0
        SortOrder = soAscending
      end
      object GridTableViewTargetLanguagesCountryName: TcxGridDBColumn
        DataBinding.FieldName = 'CountryName'
      end
    end
  end
  object LayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 192
    Top = 17
    object LayoutSkinLookAndFeel: TdxLayoutSkinLookAndFeel
      Offsets.RootItemsAreaOffsetHorz = 4
      Offsets.RootItemsAreaOffsetVert = 4
      PixelsPerInch = 96
    end
  end
  object EditRepository: TcxEditRepository
    Left = 196
    Top = 108
    PixelsPerInch = 96
    object EditRepositoryTextItem: TcxEditRepositoryButtonItem
      Properties.Buttons = <
        item
          Default = True
          Hint = 'Open in text editor'
          Kind = bkEllipsis
        end>
      Properties.ClearKey = 16392
      Properties.OnButtonClick = EditRepositoryTextItemPropertiesButtonClick
    end
  end
end
