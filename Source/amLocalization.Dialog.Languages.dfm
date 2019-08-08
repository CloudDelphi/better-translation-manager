object FormLanguages: TFormLanguages
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Languages'
  ClientHeight = 371
  ClientWidth = 601
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object LayoutControl: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 601
    Height = 371
    Align = alClient
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = LayoutSkinLookAndFeel
    object ComboBoxSourceLanguage: TcxExtLookupComboBox
      Left = 103
      Top = 10
      Properties.DropDownAutoSize = True
      Properties.DropDownSizeable = True
      Properties.View = DataModuleMain.GridTableViewLanguages
      Properties.KeyFieldNames = 'LocaleID'
      Properties.ListFieldItem = DataModuleMain.GridTableViewLanguagesColumnLanguage
      Style.HotTrack = False
      TabOrder = 0
      Width = 145
    end
    object ButtonOK: TcxButton
      Left = 435
      Top = 336
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 3
    end
    object ButtonCancel: TcxButton
      Left = 516
      Top = 336
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 4
    end
    object CheckListBoxLanguages: TcxCheckListBox
      Left = 103
      Top = 37
      Width = 488
      Height = 256
      Columns = 2
      EditValueFormat = cvfIndices
      Items = <
        item
          State = cbsChecked
          Text = 'DA-dk'
        end
        item
          Text = 'EN-us'
        end>
      ParentColor = True
      Sorted = True
      Style.BorderStyle = cbsNone
      Style.TransparentBorder = True
      StyleFocused.BorderStyle = cbsNone
      StyleHot.BorderStyle = cbsNone
      TabOrder = 1
    end
    object CheckBoxApplyFilter: TcxCheckBox
      Left = 103
      Top = 299
      Action = ActionApplyFilter
      Style.HotTrack = False
      TabOrder = 2
      Transparent = True
    end
    object LayoutControlGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = LayoutControlGroup_Root
      CaptionOptions.Text = '&Source language:'
      Control = ComboBoxSourceLanguage
      ControlOptions.AlignHorz = ahLeft
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 145
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = ButtonOK
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = ButtonCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = LayoutControlGroup_Root
      AlignVert = avBottom
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 4
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = LayoutControlGroup_Root
      AlignVert = avBottom
      CaptionOptions.Text = 'Separator'
      Index = 3
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = LayoutControlGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.AlignVert = tavTop
      CaptionOptions.Text = '&Target languages:'
      Control = CheckListBoxLanguages
      ControlOptions.OriginalHeight = 249
      ControlOptions.OriginalWidth = 474
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = LayoutControlGroup_Root
      CaptionOptions.ShowAccelChar = False
      CaptionOptions.Text = ' '
      Control = CheckBoxApplyFilter
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 2
    end
  end
  object LayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 68
    Top = 96
    object LayoutSkinLookAndFeel: TdxLayoutSkinLookAndFeel
      LookAndFeel.NativeStyle = False
      PixelsPerInch = 96
    end
  end
  object ActionList1: TActionList
    Left = 296
    Top = 192
    object ActionApplyFilter: TAction
      AutoCheck = True
      Caption = '&Filter target language lists with above selection'
      OnExecute = ActionApplyFilterExecute
      OnUpdate = ActionApplyFilterUpdate
    end
  end
end
