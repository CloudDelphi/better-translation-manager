inherited FormSelectFileFormat: TFormSelectFileFormat
  ActiveControl = ComboBoxFileFormat
  Caption = 'File format'
  ClientHeight = 222
  ClientWidth = 384
  ExplicitWidth = 390
  ExplicitHeight = 250
  PixelsPerInch = 96
  TextHeight = 13
  inherited LayoutControlButtons: TdxLayoutControl
    Top = 174
    Width = 384
    inherited ButtonOK: TcxButton
      Left = 217
      ExplicitLeft = 217
    end
    inherited ButtonCancel: TcxButton
      Left = 298
      ExplicitLeft = 298
    end
  end
  inherited LayoutControlHeader: TdxLayoutControl
    Width = 384
    inherited LabelHeader: TcxLabel
      Caption = 'Specify the file format of the file'
      ExplicitWidth = 362
      Width = 362
    end
  end
  inherited PanelMain: TPanel
    Width = 384
    Height = 135
    ExplicitTop = 39
    ExplicitHeight = 193
    inherited LayoutControl: TdxLayoutControl
      Width = 368
      Height = 123
      ExplicitLeft = 8
      object ComboBoxFileFormat: TcxComboBox [0]
        Left = 66
        Top = 6
        Properties.DropDownListStyle = lsFixedList
        Style.HotTrack = False
        TabOrder = 0
        Width = 296
      end
      object LayoutItemFileFormat: TdxLayoutItem
        Parent = LayoutControlGroup_Root
        CaptionOptions.Text = '&File format:'
        Control = ComboBoxFileFormat
        ControlOptions.OriginalHeight = 21
        ControlOptions.OriginalWidth = 121
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
