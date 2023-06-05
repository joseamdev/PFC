object FormularioParticion: TFormularioParticion
  Left = 193
  Top = 103
  Width = 400
  Height = 163
  Caption = 'Utilidad para fragmentar y ensamblar archivos'
  Color = clBtnFace
  Constraints.MaxHeight = 163
  Constraints.MinHeight = 163
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  DesignSize = (
    392
    133)
  PixelsPerInch = 96
  TextHeight = 13
  object Label_Lineas: TLabel
    Left = 104
    Top = 80
    Width = 89
    Height = 17
    AutoSize = False
    Caption = 'N'#250'mero de l'#237'neas'
    Layout = tlCenter
  end
  object Label_num: TLabel
    Left = 104
    Top = 104
    Width = 92
    Height = 17
    AutoSize = False
    Caption = 'N'#250'mero de ficheros'
    Layout = tlCenter
  end
  object Editor_Lineas: TEdit
    Left = 200
    Top = 80
    Width = 57
    Height = 21
    AutoSize = False
    MaxLength = 6
    TabOrder = 4
    Text = '10000'
  end
  object Editor_Guardar: TEdit
    Left = 104
    Top = 40
    Width = 281
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
    Left = 264
    Top = 96
    Width = 57
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Ejecutar'
    TabOrder = 5
    OnClick = Boton_AceptarClick
  end
  object Editor_Abrir: TEdit
    Left = 104
    Top = 8
    Width = 281
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    TabOrder = 1
  end
  object Boton_Cancelar: TButton
    Left = 328
    Top = 96
    Width = 57
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Salir'
    TabOrder = 6
    OnClick = Boton_CancelarClick
  end
  object RadioButton_partir: TRadioButton
    Left = 8
    Top = 80
    Width = 81
    Height = 17
    Caption = 'Fragmentar'
    Checked = True
    TabOrder = 7
    TabStop = True
    OnClick = RadioButton_partirClick
  end
  object RadioButton_unir: TRadioButton
    Left = 8
    Top = 104
    Width = 81
    Height = 17
    Caption = 'Ensamblar'
    TabOrder = 8
    OnClick = RadioButton_unirClick
  end
  object Edit_num: TEdit
    Left = 200
    Top = 104
    Width = 57
    Height = 21
    MaxLength = 4
    TabOrder = 9
  end
  object DialogoAbrir: TOpenDialog
    Options = [ofFileMustExist, ofEnableSizing]
    OptionsEx = [ofExNoPlacesBar]
    Title = 'Fichero origen'
    Left = 320
    Top = 64
  end
  object DialogoGuardar: TSaveDialog
    DefaultExt = 'dat'
    FileName = 'FicheroSalida.dat'
    OptionsEx = [ofExNoPlacesBar]
    Title = 'Fichero destino'
    Left = 352
    Top = 64
  end
end
