object DataModuleTranslationMemory: TDataModuleTranslationMemory
  Height = 179
  Width = 258
  PixelsPerInch = 96
  object DataSourceTranslationMemory: TDataSource
    DataSet = TableTranslationMemory
    Left = 76
    Top = 63
  end
  object TableTranslationMemory: TFDMemTable
    AfterInsert = TableTranslationMemoryAfterModify
    AfterPost = TableTranslationMemoryAfterModify
    AfterDelete = TableTranslationMemoryAfterModify
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvStoreItems, rvSilentMode]
    ResourceOptions.StoreItems = [siMeta, siData]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 76
    Top = 24
  end
end
