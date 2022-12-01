object FormDialog: TFormDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Lorem Ipsum'
  ClientHeight = 295
  ClientWidth = 537
  Color = clBtnFace
  ParentFont = True
  Position = poMainFormCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 15
  object LayoutControlButtons: TdxLayoutControl
    Left = 0
    Top = 240
    Width = 537
    Height = 55
    Align = alBottom
    BevelEdges = [beTop]
    BevelInner = bvNone
    BevelKind = bkFlat
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = DataModuleMain.LayoutSkinLookAndFeelHeader
    object ButtonOK: TcxButton
      Left = 367
      Top = 13
      Width = 75
      Height = 25
      Action = ActionOK
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object ButtonCancel: TcxButton
      Left = 449
      Top = 13
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
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
  end
  object LayoutControlHeader: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 537
    Height = 45
    Align = alTop
    BevelEdges = [beBottom]
    BevelInner = bvNone
    BevelKind = bkFlat
    TabOrder = 1
    Visible = False
    AutoSize = True
    LayoutLookAndFeel = DataModuleMain.LayoutSkinLookAndFeelHeader
    object LayoutControlHeaderGroup_Root: TdxLayoutGroup
      AlignHorz = ahParentManaged
      AlignVert = avParentManaged
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object LayoutItemHeader: TdxLayoutLabeledItem
      Parent = LayoutControlHeaderGroup_Root
      LayoutLookAndFeel = DataModuleMain.LayoutSkinLookAndFeelTitle
      CaptionOptions.ShowAccelChar = False
      CaptionOptions.Text = 'Lorem Ipsum'
      CaptionOptions.WordWrap = True
      Index = 0
    end
  end
  object LayoutControl: TdxLayoutControl
    Left = 0
    Top = 45
    Width = 537
    Height = 195
    Align = alClient
    TabOrder = 2
    LayoutLookAndFeel = DataModuleMain.LayoutSkinLookAndFeel
    object LayoutControlGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avTop
      Hidden = True
      ShowBorder = False
      Index = -1
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
