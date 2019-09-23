inherited FormTargetLanguage: TFormTargetLanguage
  Caption = 'Target language'
  ClientHeight = 237
  ClientWidth = 392
  ExplicitWidth = 398
  ExplicitHeight = 265
  PixelsPerInch = 96
  TextHeight = 13
  inherited LayoutControlButtons: TdxLayoutControl
    Top = 189
    Width = 392
    inherited ButtonOK: TcxButton
      Left = 225
      ExplicitLeft = 225
    end
    inherited ButtonCancel: TcxButton
      Left = 306
      ExplicitLeft = 306
    end
  end
  inherited LayoutControlHeader: TdxLayoutControl
    Width = 392
    inherited LabelHeader: TcxLabel
      ExplicitWidth = 370
      Width = 370
    end
  end
  inherited PanelMain: TPanel
    Width = 392
    Height = 150
    ExplicitTop = 39
    ExplicitHeight = 193
    inherited LayoutControl: TdxLayoutControl
      Width = 376
      Height = 138
      ExplicitLeft = 8
      object ComboBoxSourceLanguage: TcxExtLookupComboBox [0]
        Left = 62
        Top = 6
        RepositoryItem = DataModuleMain.EditRepositoryComboBoxItemLanguage
        Properties.DropDownAutoSize = True
        Properties.DropDownSizeable = True
        Properties.View = DataModuleMain.GridTableViewLanguages
        Properties.KeyFieldNames = 'LocaleID'
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
        ControlOptions.OriginalHeight = 21
        ControlOptions.OriginalWidth = 199
        ControlOptions.ShowBorder = False
        Index = 0
      end
    end
  end
  inherited ActionList: TActionList
    inherited ActionOK: TAction
      OnUpdate = ActionOKUpdate
    end
  end
end
