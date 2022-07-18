inherited FormTargetLanguage: TFormTargetLanguage
  Caption = 'Target language'
  ClientHeight = 237
  ClientWidth = 392
  ExplicitWidth = 408
  ExplicitHeight = 275
  PixelsPerInch = 96
  TextHeight = 15
  inherited LayoutControlButtons: TdxLayoutControl
    Top = 182
    Width = 392
    ExplicitTop = 182
    ExplicitWidth = 392
    inherited ButtonOK: TcxButton
      Left = 222
      ExplicitLeft = 222
    end
    inherited ButtonCancel: TcxButton
      Left = 304
      ExplicitLeft = 304
    end
  end
  inherited LayoutControlHeader: TdxLayoutControl
    Width = 392
    ExplicitWidth = 392
  end
  inherited LayoutControl: TdxLayoutControl
    Width = 392
    Height = 137
    ExplicitTop = 45
    ExplicitWidth = 392
    ExplicitHeight = 137
    object ComboBoxSourceLanguage: TcxExtLookupComboBox [0]
      Left = 68
      Top = 7
      RepositoryItem = DataModuleMain.EditRepositoryComboBoxItemLanguage
      Properties.DropDownAutoSize = True
      Properties.DropDownSizeable = True
      Properties.View = DataModuleMain.GridTableViewLanguages
      Properties.KeyFieldNames = 'LocaleName'
      Properties.ListFieldItem = DataModuleMain.GridTableViewLanguagesColumnLanguage
      Style.HotTrack = False
      TabOrder = 0
      Width = 199
    end
    object LayoutItemTargetLanguage: TdxLayoutItem
      Parent = LayoutControlGroup_Root
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Language:'
      Control = ComboBoxSourceLanguage
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 199
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
  inherited ActionList: TActionList
    inherited ActionOK: TAction
      OnUpdate = ActionOKUpdate
    end
  end
end
