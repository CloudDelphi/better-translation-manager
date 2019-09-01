inherited FormSelectModule: TFormSelectModule
  Caption = 'Select module'
  ClientHeight = 164
  ClientWidth = 337
  ExplicitWidth = 343
  ExplicitHeight = 192
  PixelsPerInch = 96
  TextHeight = 13
  inherited LayoutControlButtons: TdxLayoutControl
    Top = 116
    Width = 337
    ExplicitTop = 109
    ExplicitWidth = 337
    inherited ButtonOK: TcxButton
      Left = 170
      ExplicitLeft = 170
    end
    inherited ButtonCancel: TcxButton
      Left = 251
      ExplicitLeft = 251
    end
  end
  inherited LayoutControlHeader: TdxLayoutControl
    Width = 337
    ExplicitWidth = 337
    inherited LabelHeader: TcxLabel
      ExplicitWidth = 315
      Width = 315
    end
  end
  inherited PanelMain: TPanel
    Width = 337
    Height = 77
    ExplicitWidth = 337
    ExplicitHeight = 45
    inherited LayoutControl: TdxLayoutControl
      Width = 321
      Height = 65
      ExplicitLeft = 8
      ExplicitTop = 4
      ExplicitWidth = 321
      ExplicitHeight = 33
      object ComboBoxModule: TcxComboBox [0]
        Left = 49
        Top = 6
        Properties.DropDownListStyle = lsEditFixedList
        Properties.Sorted = True
        Style.HotTrack = False
        TabOrder = 0
        Width = 252
      end
      object dxLayoutItem1: TdxLayoutItem
        Parent = LayoutControlGroup_Root
        CaptionOptions.Text = 'Module:'
        Control = ComboBoxModule
        ControlOptions.AlignHorz = ahLeft
        ControlOptions.OriginalHeight = 21
        ControlOptions.OriginalWidth = 252
        ControlOptions.ShowBorder = False
        Index = 0
      end
    end
  end
end
