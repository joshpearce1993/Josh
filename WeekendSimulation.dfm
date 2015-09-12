object FormSimulation: TFormSimulation
  Left = 0
  Top = 0
  Caption = 'Simulation'
  ClientHeight = 325
  ClientWidth = 295
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelMaxBeds: TLabel
    Left = 107
    Top = 20
    Width = 127
    Height = 13
    Caption = 'Maximum Number of Beds:'
  end
  object LabelStartOccupation: TLabel
    Left = 3
    Top = 45
    Width = 231
    Height = 13
    Caption = 'Number of Occupied Beds at Start of Simulation:'
  end
  object LabelStartDischarge: TLabel
    Left = 65
    Top = 72
    Width = 169
    Height = 13
    Caption = 'Start of Discharge Interval (0..23):'
  end
  object LabelEndDischarge: TLabel
    Left = 71
    Top = 99
    Width = 163
    Height = 13
    Caption = 'End of Discharge Interval (1..24):'
  end
  object LabelReplications: TLabel
    Left = 120
    Top = 126
    Width = 114
    Height = 13
    Caption = 'Number of Replications:'
  end
  object EditMaxBeds: TEdit
    Left = 240
    Top = 17
    Width = 41
    Height = 21
    TabOrder = 0
    Text = '50'
  end
  object EditStartOccupation: TEdit
    Left = 240
    Top = 44
    Width = 41
    Height = 21
    TabOrder = 1
    Text = '0'
  end
  object EditStartDischarge: TEdit
    Left = 240
    Top = 71
    Width = 41
    Height = 21
    TabOrder = 2
    Text = '9'
  end
  object EditEndDischarge: TEdit
    Left = 240
    Top = 98
    Width = 41
    Height = 21
    TabOrder = 3
    Text = '18'
  end
  object EditReplications: TEdit
    Left = 240
    Top = 125
    Width = 41
    Height = 21
    TabOrder = 4
    Text = '100'
  end
  object ButtonLoadEdits: TButton
    Left = 120
    Top = 161
    Width = 161
    Height = 25
    Caption = 'Load Input Fields'
    TabOrder = 5
    OnClick = ButtonLoadEditsClick
  end
  object ButtonLoadFromFile: TButton
    Left = 120
    Top = 192
    Width = 161
    Height = 25
    Caption = 'Load Patient Data'
    TabOrder = 6
    OnClick = ButtonLoadFromFileClick
  end
  object ButtonRunSimulation: TButton
    Left = 120
    Top = 223
    Width = 161
    Height = 25
    Caption = 'Run the Simulation'
    TabOrder = 7
    OnClick = ButtonRunSimulationClick
  end
  object ButtonPlotOccupancy: TButton
    Left = 120
    Top = 254
    Width = 161
    Height = 25
    Caption = 'Plot Bed Occupancy'
    TabOrder = 8
    OnClick = ButtonPlotOccupancyClick
  end
  object ButtonReset: TBitBtn
    Left = 120
    Top = 285
    Width = 75
    Height = 25
    Caption = '&Reset'
    Kind = bkRetry
    NumGlyphs = 2
    TabOrder = 9
    OnClick = ButtonResetClick
  end
  object ButtonClose: TBitBtn
    Left = 206
    Top = 285
    Width = 75
    Height = 25
    Kind = bkClose
    NumGlyphs = 2
    TabOrder = 10
  end
  object OpenDialogPatientData: TOpenDialog
    Left = 32
    Top = 165
  end
  object SaveDialogPerformanceData: TSaveDialog
    Left = 32
    Top = 221
  end
end
