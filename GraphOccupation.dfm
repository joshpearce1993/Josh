object FormGraphOccupation: TFormGraphOccupation
  Left = 0
  Top = 0
  Caption = 'Bed Occupation Graph'
  ClientHeight = 350
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ChartBedOccupation: TChart
    Left = 16
    Top = 8
    Width = 488
    Height = 273
    Title.Text.Strings = (
      'TChart')
    BottomAxis.Title.Caption = 'Time (Hours)'
    LeftAxis.Automatic = False
    LeftAxis.AutomaticMinimum = False
    LeftAxis.Title.Caption = 'Bed Occupation'
    View3D = False
    TabOrder = 0
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
    object Series1: TFastLineSeries
      Title = 'Mean Bed Occupation'
      LinePen.Color = 10708548
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series2: TFastLineSeries
      Title = 'One S.D. Above Mean'
      LinePen.Color = 3513587
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series3: TFastLineSeries
      Title = 'One S.D. Below Mean '
      LinePen.Color = 1330417
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series4: TFastLineSeries
      Title = 'Maximum Capacity'
      LinePen.Color = 11048782
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
  object ButtonPlotOccupation: TButton
    Left = 64
    Top = 304
    Width = 110
    Height = 25
    Caption = 'Plot Bed Occupation'
    TabOrder = 1
    OnClick = ButtonPlotOccupationClick
  end
  object ButtonClose: TBitBtn
    Left = 296
    Top = 304
    Width = 110
    Height = 25
    Kind = bkClose
    NumGlyphs = 2
    TabOrder = 2
  end
  object SaveDialogBedOccupancy: TSaveDialog
    Left = 208
    Top = 296
  end
end
