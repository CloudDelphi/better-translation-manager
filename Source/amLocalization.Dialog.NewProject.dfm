inherited FormNewProject: TFormNewProject
  Caption = 'New project'
  ClientHeight = 234
  ClientWidth = 471
  ExplicitWidth = 477
  ExplicitHeight = 262
  PixelsPerInch = 96
  TextHeight = 13
  inherited LayoutControlButtons: TdxLayoutControl
    Top = 186
    Width = 471
    ExplicitTop = 186
    ExplicitWidth = 471
    inherited ButtonOK: TcxButton
      Left = 304
      ExplicitLeft = 304
    end
    inherited ButtonCancel: TcxButton
      Left = 385
      ExplicitLeft = 385
    end
  end
  inherited LayoutControlHeader: TdxLayoutControl
    Width = 471
    ExplicitWidth = 471
    inherited LabelHeader: TcxLabel
      Caption = 'Create a new translation project'
      ExplicitWidth = 449
      Width = 449
    end
  end
  inherited PanelMain: TPanel
    Width = 471
    Height = 147
    ExplicitTop = 39
    ExplicitWidth = 471
    ExplicitHeight = 156
    inherited LayoutControl: TdxLayoutControl
      Width = 455
      Height = 135
      AutoSize = True
      ExplicitLeft = 8
      ExplicitTop = 4
      ExplicitWidth = 455
      ExplicitHeight = 144
      object EditSourceApplication: TcxMRUEdit [0]
        Left = 102
        Top = 6
        Properties.ButtonGlyph.SourceDPI = 96
        Properties.ButtonGlyph.Data = {
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
        Properties.OnButtonClick = EditSourceApplicationPropertiesButtonClick
        Style.HotTrack = False
        TabOrder = 0
        Width = 326
      end
      object ComboBoxSourceLanguage: TcxExtLookupComboBox [1]
        Left = 102
        Top = 36
        Properties.DropDownAutoSize = True
        Properties.DropDownSizeable = True
        Style.HotTrack = False
        TabOrder = 1
        Width = 145
      end
      inherited LayoutControlGroup_Root: TdxLayoutGroup
        ItemIndex = 1
      end
      object dxLayoutItem1: TdxLayoutItem
        Parent = LayoutControlGroup_Root
        CaptionOptions.Text = 'Source application:'
        Control = EditSourceApplication
        ControlOptions.AlignHorz = ahLeft
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 326
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutItem2: TdxLayoutItem
        Parent = LayoutControlGroup_Root
        CaptionOptions.Text = 'Source language:'
        Control = ComboBoxSourceLanguage
        ControlOptions.AlignHorz = ahLeft
        ControlOptions.OriginalHeight = 21
        ControlOptions.OriginalWidth = 145
        ControlOptions.ShowBorder = False
        Index = 1
      end
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
