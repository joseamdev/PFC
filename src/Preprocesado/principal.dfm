object Formulario_Principal: TFormulario_Principal
  Left = 194
  Top = 117
  Width = 425
  Height = 150
  Caption = 'Conversi'#243'n de fichero de datos de alumnos'
  Color = clBtnFace
  Constraints.MaxHeight = 150
  Constraints.MinHeight = 150
  Constraints.MinWidth = 425
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  DesignSize = (
    417
    123)
  PixelsPerInch = 96
  TextHeight = 13
  object BotonRadio_Alumnos: TRadioButton
    Left = 8
    Top = 80
    Width = 89
    Height = 17
    Caption = 'Alumnos'
    Checked = True
    TabOrder = 4
    TabStop = True
  end
  object BotonRadio_Asignaturas: TRadioButton
    Left = 104
    Top = 80
    Width = 89
    Height = 17
    Caption = 'Acad'#233'micos'
    TabOrder = 5
  end
  object Editor_Guardar: TEdit
    Left = 104
    Top = 40
    Width = 307
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    TabOrder = 3
  end
  object Boton_Abrir: TButton
    Left = 8
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Archivo origen'
    TabOrder = 0
    OnClick = Boton_AbrirClick
  end
  object Boton_Guardar: TButton
    Left = 8
    Top = 40
    Width = 89
    Height = 25
    Caption = 'Archivo destino'
    TabOrder = 2
    OnClick = Boton_GuardarClick
  end
  object Boton_Aceptar: TButton
    Left = 253
    Top = 80
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Convertir'
    TabOrder = 6
    OnClick = Boton_AceptarClick
  end
  object Editor_Abrir: TEdit
    Left = 104
    Top = 8
    Width = 307
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    ReadOnly = True
    TabOrder = 1
  end
  object Boton_Cancelar: TButton
    Left = 334
    Top = 80
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Salir'
    TabOrder = 7
    OnClick = Boton_CancelarClick
  end
  object DialogoAbrir: TOpenDialog
    Options = [ofFileMustExist, ofEnableSizing]
    OptionsEx = [ofExNoPlacesBar]
    Title = 'Fichero origen'
    Left = 192
    Top = 72
  end
  object DialogoGuardar: TSaveDialog
    DefaultExt = 'txt'
    FileName = 'FicheroSalida.txt'
    OptionsEx = [ofExNoPlacesBar]
    Title = 'Fichero destino'
    Left = 224
    Top = 72
  end
end
