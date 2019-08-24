object DataModuleTranslationMemory: TDataModuleTranslationMemory
  OldCreateOrder = False
  Height = 179
  Width = 258
  object TableTranslationMemory: TdxMemData
    Indexes = <>
    Persistent.Option = poNone
    SortOptions = []
    Left = 76
    Top = 24
  end
  object DataSourceTranslationMemory: TDataSource
    DataSet = TableTranslationMemory
    Left = 76
    Top = 63
  end
end
