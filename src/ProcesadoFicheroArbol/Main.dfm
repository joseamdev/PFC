object Form1: TForm1
  Left = 270
  Top = 116
  Width = 337
  Height = 109
  Caption = 'Procesado de fichero '#225'rbol'
  Color = clBtnFace
  Constraints.MaxHeight = 109
  Constraints.MinHeight = 109
  Constraints.MinWidth = 337
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    321
    73)
  PixelsPerInch = 96
  TextHeight = 13
  object EditFichero: TEdit
    Left = 8
    Top = 8
    Width = 257
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 0
  end
  object ButtonAbrir: TButton
    Left = 272
    Top = 8
    Width = 41
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Abrir'
    TabOrder = 1
    OnClick = ButtonAbrirClick
  end
  object ButtonConvertir: TButton
    Left = 8
    Top = 40
    Width = 305
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Convertir'
    Enabled = False
    TabOrder = 2
    OnClick = ButtonConvertirClick
  end
  object OpenDialog: TOpenDialog
    Left = 280
    Top = 24
  end
end
