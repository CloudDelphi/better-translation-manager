object DataModuleTranslationMemory: TDataModuleTranslationMemory
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 179
  Width = 258
  object TableTranslationMemory: TdxMemData
    Indexes = <>
    Persistent.Option = poNone
    SortOptions = []
    AfterInsert = TableTranslationMemoryAfterInsert
    AfterPost = TableTranslationMemoryAfterPost
    AfterDelete = TableTranslationMemoryAfterDelete
    Left = 76
    Top = 24
  end
  object DataSourceTranslationMemory: TDataSource
    DataSet = TableTranslationMemory
    Left = 76
    Top = 63
  end
end
