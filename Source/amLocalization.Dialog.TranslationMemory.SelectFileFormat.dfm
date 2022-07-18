inherited FormSelectFileFormat: TFormSelectFileFormat
  ActiveControl = ComboBoxFileFormat
  Caption = 'File format'
  ClientHeight = 222
  ClientWidth = 384
  ExplicitWidth = 400
  ExplicitHeight = 260
  PixelsPerInch = 96
  TextHeight = 15
  inherited LayoutControlButtons: TdxLayoutControl
    Top = 167
    Width = 384
    ExplicitTop = 167
    ExplicitWidth = 384
    inherited ButtonOK: TcxButton
      Left = 214
      ExplicitLeft = 214
    end
    inherited ButtonCancel: TcxButton
      Left = 296
      ExplicitLeft = 296
    end
  end
  inherited LayoutControlHeader: TdxLayoutControl
    Width = 384
    ExplicitWidth = 384
    inherited LayoutItemHeader: TdxLayoutLabeledItem
      CaptionOptions.Text = 'Specify the file format of the file'
    end
  end
  inherited LayoutControl: TdxLayoutControl
    Width = 384
    Height = 122
    ExplicitTop = 45
    ExplicitWidth = 384
    ExplicitHeight = 126
    object ComboBoxFileFormat: TcxComboBox [0]
      Left = 73
      Top = 7
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 0
      Width = 304
    end
    object LayoutItemFileFormat: TdxLayoutItem
      Parent = LayoutControlGroup_Root
      CaptionOptions.Text = '&File format:'
      Control = ComboBoxFileFormat
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 121
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
