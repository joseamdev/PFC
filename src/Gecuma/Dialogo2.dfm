object DialogoDatos: TDialogoDatos
  Left = 738
  Top = 395
  BorderStyle = bsDialog
  Caption = 'Rellena todos los campos'
  ClientHeight = 307
  ClientWidth = 422
  Color = clBtnFace
  Constraints.MaxHeight = 480
  Constraints.MaxWidth = 800
  Constraints.MinHeight = 343
  Constraints.MinWidth = 430
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 422
    Height = 266
    Align = alClient
    TabOrder = 0
    object LabelNacion: TLabel
      Left = 16
      Top = 16
      Width = 80
      Height = 16
      AutoSize = False
      Caption = 'Nacionalidad'
    end
    object LabelProvincia: TLabel
      Left = 16
      Top = 48
      Width = 80
      Height = 16
      AutoSize = False
      Caption = 'Provincia'
    end
    object LabelCP: TLabel
      Left = 16
      Top = 80
      Width = 80
      Height = 16
      AutoSize = False
      Caption = 'C'#243'digo Postal'
    end
    object LabelGenero: TLabel
      Left = 16
      Top = 112
      Width = 80
      Height = 16
      AutoSize = False
      Caption = 'G'#233'nero'
    end
    object LabelEdadInicial: TLabel
      Left = 16
      Top = 144
      Width = 80
      Height = 16
      AutoSize = False
      Caption = 'Edad de inicio'
    end
    object LabelCursoInicio: TLabel
      Left = 16
      Top = 176
      Width = 80
      Height = 16
      AutoSize = False
      Caption = 'Curso de inicio'
    end
    object LabelAcceso: TLabel
      Left = 16
      Top = 208
      Width = 80
      Height = 16
      AutoSize = False
      Caption = 'V'#237'a de acceso'
    end
    object LabelCalificacion: TLabel
      Left = 16
      Top = 240
      Width = 80
      Height = 16
      AutoSize = False
      Caption = 'Calificaci'#243'n'
    end
    object ComboBoxNacion: TComboBox
      Left = 104
      Top = 16
      Width = 305
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = ComprobarDatos
      OnExit = ComprobarDatos
    end
    object ComboBoxProvincia: TComboBox
      Left = 104
      Top = 48
      Width = 305
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnChange = ComprobarDatos
      OnExit = ComprobarDatos
    end
    object ComboBoxGenero: TComboBox
      Left = 104
      Top = 112
      Width = 305
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
      OnChange = ComprobarDatos
      OnExit = ComprobarDatos
    end
    object ComboBoxAcceso: TComboBox
      Left = 104
      Top = 208
      Width = 305
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 6
      OnChange = ComprobarDatos
      OnExit = ComprobarDatos
    end
    object EditCP: TEdit
      Left = 104
      Top = 80
      Width = 305
      Height = 21
      TabOrder = 2
      OnChange = ComprobarDatos
      OnExit = ComprobarDatos
    end
    object EditEdad: TEdit
      Left = 104
      Top = 144
      Width = 305
      Height = 21
      TabOrder = 4
      OnChange = ComprobarDatos
      OnExit = ComprobarDatos
    end
    object EditCurso: TEdit
      Left = 104
      Top = 176
      Width = 305
      Height = 21
      TabOrder = 5
      OnChange = ComprobarDatos
      OnExit = ComprobarDatos
    end
    object EditCalificacion: TEdit
      Left = 104
      Top = 240
      Width = 305
      Height = 21
      TabOrder = 7
      OnChange = ComprobarDatos
      OnExit = ComprobarDatos
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 266
    Width = 422
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      422
      41)
    object ButtonSiguiente: TButton
      Left = 8
      Top = 8
      Width = 406
      Height = 25
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Siguiente'
      Enabled = False
      TabOrder = 0
      OnClick = ButtonSiguienteClick
    end
  end
end
