object FormNewProject: TFormNewProject
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'New project'
  ClientHeight = 193
  ClientWidth = 471
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object dxLayoutControl1: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 471
    Height = 193
    TabOrder = 0
    AutoSize = True
    object EditSourceApplication: TcxButtonEdit
      Left = 106
      Top = 10
      Properties.Buttons = <
        item
          Default = True
          Glyph.SourceDPI = 96
          Glyph.Data = {
            89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
            610000002B744558745469746C65004F70656E3B466F6C6465723B426172733B
            526962626F6E3B5374616E646172643B4C6F6164F1C3C4630000022249444154
            785EA593BD6B545110C57F6FF3242AA9B412144B3FB1500CA20663FC032C6C44
            B0B010041194147616366295805A88A058898D8D106C444D6C44123426120959
            36B8316FD9F8F2B15F6FDFBD7746B9EFAD82019B1CB81C6698397366E006AACA
            461000859CC959F327FC079DC1E1C4C381D785807EB21815A827E642DF8DB1E7
            E30F4E8BA2A0CA1F12A5F7FA281D8488F61FBAFC940C8A6DD5987A76F3DE9D4B
            FB468F5C7D53653D0470B94B426704DC2AA4DF517184380E9EBBB6DD99E1F2A9
            03DB4072BB0AA2608C1B1BB8F5E10C6032012BA00E5C422016D4D2250D0E5F1C
            24E8DE0104A00208AA8ED1BB57FA80AEBF02A9CB046C82DA26625BE00CD4CBA8
            CF1B54F21A1556AB75809E20080A800BDEDD3EA67D8343C8CA272AD3E3C40B15
            54404510C92EA7A28888E7A5F92AEAD38A38AD862675B4E28864A1C872D460FF
            F961BC786E1B71A85A108B3A8B8A0149894BDF78FB6868DEAFB05A2E11CFCCD2
            B3AB174D221AB32F51DF90DD47457C8C737E1D114769B2CCCA9A19094DDBB154
            9A632D5A66E7F1BDB42BD3D85ADC29CC9A54C04F17C40B0951B1C29772FD4598
            B62D4B73737477F7B0796B487D6602DB58F6D63B2E347391E71CB59536F1CFC6
            D7FBEFA362D86AA4B018B1E7C44992C529926AD14F50BF7BA749BD082A5EA8BA
            9010D7CD0860C24633FD88AB1D9D1C79C5647E5D72560001505401554421B55A
            FEFCA3F9184803600BB00928B01EC13FB10206489E9CDDED0036FC9D7F01FAB6
            A14B22EE620A0000000049454E44AE426082}
          Kind = bkGlyph
        end>
      Properties.OnButtonClick = EditSourceApplicationPropertiesButtonClick
      Style.BorderColor = clWindowFrame
      Style.BorderStyle = ebs3D
      Style.HotTrack = False
      Style.ButtonStyle = bts3D
      TabOrder = 0
      Width = 326
    end
    object ComboBoxSourceLanguage: TcxExtLookupComboBox
      Left = 106
      Top = 40
      Properties.DropDownAutoSize = True
      Properties.DropDownSizeable = True
      Style.BorderColor = clWindowFrame
      Style.BorderStyle = ebs3D
      Style.HotTrack = False
      Style.ButtonStyle = bts3D
      Style.PopupBorderStyle = epbsFrame3D
      TabOrder = 1
      Width = 145
    end
    object ButtonOK: TcxButton
      Left = 276
      Top = 79
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 2
    end
    object ButtonCancel: TcxButton
      Left = 357
      Top = 79
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      Default = True
      ModalResult = 2
      TabOrder = 3
    end
    object dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 3
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'Source application:'
      Control = EditSourceApplication
      ControlOptions.AlignHorz = ahLeft
      ControlOptions.OriginalHeight = 24
      ControlOptions.OriginalWidth = 326
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'Source language:'
      Control = ComboBoxSourceLanguage
      ControlOptions.AlignHorz = ahLeft
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 145
      ControlOptions.ShowBorder = False
      Index = 1
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
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 3
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'Separator'
      Index = 2
    end
  end
  object FileOpenDialogApplication: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Applications'
        FileMask = '*.exe'
      end
      item
        DisplayName = 'DLLs'
        FileMask = '*.dll'
      end
      item
        DisplayName = 'All files'
        FileMask = '*.*'
      end>
    Options = [fdoPathMustExist, fdoFileMustExist]
    Left = 176
    Top = 128
  end
end
