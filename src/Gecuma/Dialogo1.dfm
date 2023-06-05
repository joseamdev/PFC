object DialogoTitulacion: TDialogoTitulacion
  Left = 331
  Top = 143
  BorderStyle = bsDialog
  Caption = 'Selecciona la titulaci'#243'n'
  ClientHeight = 322
  ClientWidth = 472
  Color = clBtnFace
  Constraints.MaxHeight = 600
  Constraints.MaxWidth = 800
  Constraints.MinHeight = 358
  Constraints.MinWidth = 480
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  Visible = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 472
    Height = 281
    Align = alClient
    Constraints.MinHeight = 240
    Constraints.MinWidth = 360
    ItemHeight = 13
    TabOrder = 0
    OnClick = ListBox1Click
  end
  object Panel1: TPanel
    Left = 0
    Top = 281
    Width = 472
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      472
      41)
    object ButtonSiguiente: TButton
      Left = 8
      Top = 8
      Width = 457
      Height = 25
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Siguiente'
      Enabled = False
      TabOrder = 0
      OnClick = ButtonSiguienteClick
    end
  end
end
