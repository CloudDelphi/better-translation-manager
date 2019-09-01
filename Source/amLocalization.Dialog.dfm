object FormDialog: TFormDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Lorem Ipsum'
  ClientHeight = 280
  ClientWidth = 537
  Color = clWhite
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object LayoutControlButtons: TdxLayoutControl
    Left = 0
    Top = 232
    Width = 537
    Height = 48
    Align = alBottom
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = DataModuleMain.LayoutSkinLookAndFeelHeader
    object ButtonOK: TcxButton
      Left = 370
      Top = 11
      Width = 75
      Height = 25
      Action = ActionOK
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object ButtonCancel: TcxButton
      Left = 451
      Top = 11
      Width = 75
      Height = 25
      Action = ActionCancel
      Cancel = True
      ModalResult = 2
      TabOrder = 1
    end
    object LayoutControlButtonsGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object LayoutItemButtonOK: TdxLayoutItem
      Parent = LayoutGroupButtons
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = ButtonOK
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object LayoutItemButtonCancel: TdxLayoutItem
      Parent = LayoutGroupButtons
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = ButtonCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object LayoutGroupButtons: TdxLayoutGroup
      Parent = LayoutControlButtonsGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
  end
  object LayoutControlHeader: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 537
    Height = 39
    Align = alTop
    TabOrder = 1
    AutoSize = True
    LayoutLookAndFeel = DataModuleMain.LayoutSkinLookAndFeelHeader
    object LabelHeader: TcxLabel
      Left = 11
      Top = 11
      Caption = 'Lorem ipsum'
      Style.HotTrack = False
      Style.TextStyle = [fsBold]
      Style.TransparentBorder = False
      Properties.ShowAccelChar = False
      Properties.ShowEndEllipsis = True
      Properties.WordWrap = True
      Transparent = True
      Width = 515
    end
    object LayoutControlHeaderGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object LayoutItemHeader: TdxLayoutItem
      Parent = LayoutControlHeaderGroup_Root
      CaptionOptions.Visible = False
      Control = LabelHeader
      ControlOptions.OriginalHeight = 13
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
  object PanelMain: TPanel
    Left = 0
    Top = 39
    Width = 537
    Height = 193
    Align = alClient
    BevelEdges = [beTop, beBottom]
    BevelKind = bkFlat
    BevelOuter = bvNone
    Color = clWhite
    Padding.Left = 8
    Padding.Top = 4
    Padding.Right = 8
    Padding.Bottom = 4
    ParentBackground = False
    ShowCaption = False
    TabOrder = 2
    ExplicitTop = 41
    ExplicitHeight = 200
    object LayoutControl: TdxLayoutControl
      Left = 8
      Top = 4
      Width = 521
      Height = 181
      Align = alClient
      ParentBackground = True
      TabOrder = 0
      Transparent = True
      LayoutLookAndFeel = DataModuleMain.LayoutSkinLookAndFeel
      ExplicitLeft = 5
      object LayoutControlGroup_Root: TdxLayoutGroup
        AlignHorz = ahClient
        AlignVert = avTop
        ButtonOptions.Buttons = <>
        Hidden = True
        ShowBorder = False
        Index = -1
      end
    end
  end
  object ActionList: TActionList
    Left = 268
    Top = 116
    object ActionOK: TAction
      Caption = 'OK'
      OnExecute = ActionOKExecute
    end
    object ActionCancel: TAction
      Caption = 'Cancel'
      OnExecute = ActionCancelExecute
    end
  end
end
