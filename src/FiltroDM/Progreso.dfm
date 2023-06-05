object FormProgreso: TFormProgreso
  Left = 229
  Top = 114
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Progreso...'
  ClientHeight = 25
  ClientWidth = 201
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Gauge1: TGauge
    Left = 0
    Top = 0
    Width = 201
    Height = 25
    ForeColor = clBlue
    Progress = 0
  end
end
