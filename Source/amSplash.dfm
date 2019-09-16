object FormSplash: TFormSplash
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'Splash!'
  ClientHeight = 200
  ClientWidth = 220
  Color = clWhite
  DefaultMonitor = dmMainForm
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  PopupMode = pmExplicit
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object TimerSplash: TTimer
    Enabled = False
    Interval = 100
    OnTimer = TimerSplashTimer
    Left = 72
    Top = 56
  end
  object TimerBanner: TTimer
    Enabled = False
    Interval = 50
    OnTimer = TimerBannerTimer
    Left = 72
    Top = 104
  end
end
