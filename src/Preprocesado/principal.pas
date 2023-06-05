unit Principal;

interface

  uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
    Forms, Dialogs, StdCtrls, Math, StrUtils, Ventana_Progresion;

  const
    num_alumno = 15;
    num_asignatura = 10;
    max_nacion = 194;
    nacion : array [0..max_nacion] of string[3] = ('0  ',
      'A  ', 'AB ', 'ADN', 'AF ', 'AL ', 'AND', 'AO ', 'ARM', 'ASA', 'AUS',
      'AZ ', 'B  ', 'BAH', 'BD ', 'BDS', 'BFA', 'BG ', 'BH ', 'BHU', 'BIE',
      'BO ', 'BOH', 'BR ', 'BRU', 'BS ', 'C  ', 'CA ', 'CDN', 'CH ', 'CHE',
      'CI ', 'CO ', 'COM', 'COR', 'CR ', 'CRO', 'CS ', 'CV ', 'CY ', 'D  ',
      'DDR', 'DJ ', 'DK ', 'DOM', 'DY ', 'DZ ', 'E  ', 'EA ', 'EAK', 'EAT',
      'EAU', 'EAZ', 'EC ', 'EP ', 'ERI', 'ESL', 'ESQ', 'EST', 'ET ', 'F  ',
      'FA ', 'FJI', 'FL ', 'GA ', 'GB ', 'GCA', 'GEO', 'GH ', 'GR ', 'GU ',
      'GUE', 'GUP', 'GUY', 'H  ', 'HD ', 'HKJ', 'I  ', 'IL ', 'IND', 'IR ',
      'IRL', 'IRQ', 'IS ', 'ISA', 'J  ', 'JA ', 'KAM', 'KI ', 'KIR', 'KWT',
      'L  ', 'LAO', 'LAR', 'LEB', 'LET', 'LIT', 'LS ', 'M  ', 'MA ', 'MAC',
      'MAD', 'MAL', 'MAR', 'MC ', 'MEX', 'MG ', 'MOL', 'MS ', 'MW ', 'MYA',
      'MZ ', 'N  ', 'NA ', 'NIC', 'NIG', 'NL ', 'NP ', 'NU ', 'NZ ', 'OM ',
      'P  ', 'PA ', 'PAK', 'PAL', 'PE ', 'PI ', 'PL ', 'PN ', 'PR ', 'PY ',
      'Q  ', 'R  ', 'RA ', 'RB ', 'RC ', 'RCA', 'RCB', 'RCH', 'RH ', 'RI ',
      'RIM', 'RL ', 'RMM', 'ROK', 'RPC', 'RSM', 'RSR', 'RU ', 'RWA', 'S  ',
      'SA ', 'SCN', 'SD ', 'SDN', 'SF ', 'SGP', 'SH ', 'SME', 'SN ', 'SP ',
      'SRI', 'ST ', 'SU ', 'SWA', 'SY ', 'SYR', 'T  ', 'TAY', 'TD ', 'TG ',
      'TN ', 'TON', 'TR ', 'TT ', 'TUK', 'U  ', 'UCR', 'USA', 'UZB', 'V  ',
      'VN ', 'VS ', 'WAG', 'WAL', 'WAN', 'WD ', 'WG ', 'WL ', 'WS ', 'YM ',
      'YU ', 'YV ', 'Z  ', 'ZR ');

  type
    TCampo = array [1..2] of integer;
    TCampoAlumno = array [1..num_alumno] of TCampo;
    TCampoAsignatura = array [1..num_asignatura] of TCampo;
    TFormulario_Principal = class(TForm)
      DialogoAbrir: TOpenDialog;
      DialogoGuardar: TSaveDialog;
      BotonRadio_Alumnos: TRadioButton;
      BotonRadio_Asignaturas: TRadioButton;
      Editor_Guardar: TEdit;
      Boton_Abrir: TButton;
      Boton_Guardar: TButton;
      Boton_Aceptar: TButton;
      Editor_Abrir: TEdit;
      Boton_Cancelar: TButton;
      procedure FormCreate(Sender: TObject);
      procedure Boton_AbrirClick(Sender: TObject);
      procedure Boton_GuardarClick(Sender: TObject);
      procedure Boton_CancelarClick(Sender: TObject);
      procedure Boton_AceptarClick(Sender: TObject);
    private
      procedure Convertir_Fichero (origen, destino : string);
      procedure Convertir_Linea_Alumno (var linea : string);
      procedure Convertir_Linea_Asignatura (var linea : string);
    public
      { Public declarations }
    end;
    procedure Formatear_Fecha (var campo : string);

  var
    Formulario_Principal : TFormulario_Principal;

implementation

{$R *.dfm}

  const
//  configuración del fichero original con los datos de los alumnos
    campos_alumno : TCampoAlumno
                  = ((1, 6), (13, 3), (19, 1), (35, 2), (40, 2), (57, 11),
                    (83, 5), (96, 20), (128, 2), (133, 1), (142, 3), (159, 11),
                    (185, 4), (202, 5), (217, 4));
//  configuración del fichero original con los datos académicos
    campos_asignatura : TCampoAsignatura
                  = ((1, 6), (16, 3), (22, 1), (38, 2), (50, 3), (56, 1), (81, 4),
                    (88, 1), (118, 2), (138, 2));
//  separador de campos que se empleará en los ficheros convertidos
    separador = #32#32;

  procedure TFormulario_Principal.FormCreate(Sender: TObject);
    begin
      Editor_Guardar.Text := ExpandFileName ('FicheroSalida.txt');
    end;

  procedure TFormulario_Principal.Boton_AbrirClick(Sender: TObject);
    begin
      if DialogoAbrir.Execute then begin
        Editor_Abrir.Text := DialogoAbrir.FileName;
        if Editor_Guardar.Text = '' then
          Editor_Guardar.Text := ExpandFileName ('FicheroSalida.txt');
      end;
    end;

  procedure TFormulario_Principal.Boton_GuardarClick(Sender: TObject);
    begin
      if DialogoGuardar.Execute then
        Editor_Guardar.Text := DialogoGuardar.FileName;
    end;

  procedure TFormulario_Principal.Boton_CancelarClick(Sender: TObject);
    begin
      Formulario_Principal.Close;
    end;

  procedure TFormulario_Principal.Boton_AceptarClick(Sender: TObject);
    begin
      if not FileExists (Editor_Abrir.Text) then
        ShowMessage ('No existe el fichero origen')
      else if Editor_Guardar.Text = '' then
        ShowMessage ('Fichero destino no es válido')
      else
        Convertir_Fichero (Editor_Abrir.Text, Editor_Guardar.Text)
    end;

  procedure TFormulario_Principal.Convertir_Fichero (origen, destino : string);
    var
      fichero_origen, fichero_destino : TextFile;
      linea : string;
      long_total, pos_fichero : integer;
      asignatura : boolean;
    begin
      Formulario_Progresion.Show;
      Formulario_Progresion.Progresion (0);
      try
        FileMode := fmOpenRead;
        AssignFile (fichero_origen, origen);
        Reset (fichero_origen);
        long_total := FileSize (fichero_origen);
        FileMode := fmOpenWrite;
        AssignFile (fichero_destino, destino);
        Rewrite (fichero_destino);
        asignatura := BotonRadio_Asignaturas.Checked;
        while not SeekEof (fichero_origen) do begin
          ReadLn (fichero_origen, linea);
          pos_fichero := FilePos (fichero_origen);
          if asignatura then
            Convertir_Linea_Asignatura (linea)
          else
            Convertir_Linea_Alumno (linea);
          WriteLn (fichero_destino, linea);
          Formulario_Progresion.Progresion ((pos_fichero * 100) div long_total);
        end;
      finally
        CloseFile (fichero_origen);
        CloseFile (fichero_destino);
      end;
      Formulario_Progresion.Hide;
      ShowMessage ('¡Hecho!');
    end;

  procedure Recodifica_Nacion (var campo : string);
    var
      indice : integer;
    begin
      indice := 0;
      while (indice <= max_nacion) and (nacion[indice] <> campo) do
        inc (indice);
      if indice > max_nacion then
        indice := 0;
      campo := IntToStr (indice);
      campo := StringOfChar ('0', 3 - Length (campo)) + campo;
    end;

  procedure Formatear_Fecha (var campo : string);
    begin
      campo[3] := '/';
      campo[7] := '/';
      campo := AnsiReplaceStr (campo, 'JAN', '01');
      campo := AnsiReplaceStr (campo, 'FEB', '02');
      campo := AnsiReplaceStr (campo, 'MAR', '03');
      campo := AnsiReplaceStr (campo, 'APR', '04');
      campo := AnsiReplaceStr (campo, 'MAY', '05');
      campo := AnsiReplaceStr (campo, 'JUN', '06');
      campo := AnsiReplaceStr (campo, 'JUL', '07');
      campo := AnsiReplaceStr (campo, 'AUG', '08');
      campo := AnsiReplaceStr (campo, 'SEP', '09');
      campo := AnsiReplaceStr (campo, 'OCT', '10');
      campo := AnsiReplaceStr (campo, 'NOV', '11');
      campo := AnsiReplaceStr (campo, 'DEC', '12');
    end;

  procedure TFormulario_Principal.Convertir_Linea_Alumno (var linea: string);
    var
      indice : integer;
      campo, linea_aux : string;
    begin
//  el campo código alumno se rellena con ceros a la izquierda
      campo := Trim (MidStr (linea, campos_alumno[1, 1], campos_alumno[1, 2]));
      linea_aux := StringOfChar ('0', 6 - Length (campo)) + linea;
      linea := MidStr (linea_aux, campos_alumno[1, 1], campos_alumno[1, 2]) + separador;
//  el siguiente campo no se varía
      linea := linea + MidStr (linea_aux, campos_alumno[2, 1], campos_alumno[2, 2]) + separador;
//  el campo sección se convierte a un entero (A = 1, B = 2, ...) (2 dígitos)
      campo := MidStr (linea_aux, campos_alumno[3, 1], campos_alumno[3, 2]);
      campo := IntToStr (Ord (campo[1]) - 64);
      linea := linea + StringOfChar ('0', 2 - Length (campo)) + campo + separador;
//  los siguientes campos no se varían
      linea := linea + MidStr (linea_aux, campos_alumno[4, 1], campos_alumno[4, 2]) + separador;
      linea := linea + MidStr (linea_aux, campos_alumno[5, 1], campos_alumno[5, 2]) + separador;
//  del campo fecha de nacimiento se elimina la hora y se le de formato dd/mm/aaaa (10 caracteres)
      campo := MidStr (linea_aux, campos_alumno[6, 1], campos_alumno[6, 2]);
      Formatear_Fecha (campo);
      linea := linea + campo + separador;
//  los siguientes campos no se varían
      for indice := 7 to 10 do
        linea := linea + MidStr (linea_aux, campos_alumno[indice, 1], campos_alumno[indice, 2]) + separador;
//  del campo nacionalidad se unifica el código desconocido a un campo vacío
      campo := MidStr (linea_aux, campos_alumno[11, 1], campos_alumno[11, 2]);
      if campo = '   ' then
        campo := '0  ';
      Recodifica_Nacion (campo);
      linea := linea + campo + separador;
//  del campo fecha de matriculación se elimina la hora y se le de formato dd/mm/aaaa (10 caracteres)
      campo := MidStr (linea_aux, campos_alumno[12, 1], campos_alumno[12, 2]);
      Formatear_Fecha (campo);
      linea := linea + campo + separador;
//  del campo via de acceso se sustituye el valor 'NULL' por un cero (se reduce la longitud a 1)
      campo := MidStr (linea_aux, campos_alumno[13, 1], campos_alumno[13, 2]);
      if campo = 'NULL' then
        campo := '0';
      case campo[1] of
        '2' : campo := '1';
        'A' : campo := '2';
        'C' : campo := '3';
        'E' : campo := '4';
        'F' : campo := '5';
        'M' : campo := '6';
        'S' : campo := '7';
        'T' : campo := '8';
      else
        campo := '0';
      end;
      linea := linea + campo + separador;
//  el campo calificación de acceso se recalifica sobre 1000 en lugar de 10 (se reduce a 4 caracteres)
      campo := MidStr (linea_aux, campos_alumno[14, 1], campos_alumno[14, 2]);
      if campo = ' NULL' then
        campo := '00000';
      Delete (campo, 3, 1);
      linea := linea + campo + separador;
//  del campo fin de carrera se sustituye el valor 'NULL' por cuatro ceros
      campo := MidStr (linea_aux, campos_alumno[15, 1], campos_alumno[15, 2]);
      if campo = 'NULL' then
        campo := '0000';
      linea := linea + campo;
    end;

  procedure TFormulario_Principal.Convertir_Linea_Asignatura (var linea: string);
    var
      campo, linea_aux : string;
    begin
//  el campo código alumno se rellena con ceros a la izquierda
      campo := Trim (MidStr (linea, campos_asignatura[1, 1], campos_asignatura[1, 2]));
      linea_aux := StringOfChar ('0', 6 - Length (campo)) + linea;
      linea := MidStr (linea_aux, campos_asignatura[1, 1], campos_asignatura[1, 2]) + separador;
//  el siguiente campo no se varía
      linea := linea + MidStr (linea_aux, campos_asignatura[2, 1], campos_asignatura[2, 2]) + separador;
//  el campo sección se convierte a un entero (A = 1, B = 2, ...) (2 dígitos)
      campo := MidStr (linea_aux, campos_asignatura[3, 1], campos_asignatura[3, 2]);
      campo := IntToStr (Ord (campo[1]) - 64);
      linea := linea + StringOfChar ('0', 2 - Length (campo)) + campo + separador;
//  el siguiente campo no se varía
      linea := linea + MidStr (linea_aux, campos_asignatura[4, 1], campos_asignatura[4, 2]) + separador;
//  el campo código asignatura se rellena con ceros a la izquierda
      campo := MidStr (linea_aux, campos_asignatura[5, 1], campos_asignatura[5, 2]);
      campo := AnsiReplaceStr (campo, ' ', '0');
      linea := linea + campo + separador;
//  el siguiente campo no se varía
      linea := linea + MidStr (linea_aux, campos_asignatura[7, 1], campos_asignatura[7, 2]) + separador;
//  los campos número de convocatoria y número de matriculaciones no se varían
      linea := linea + MidStr (linea_aux, campos_asignatura[9, 1], campos_asignatura[9, 2]) + separador;
      linea := linea + MidStr (linea_aux, campos_asignatura[10, 1], campos_asignatura[10, 2]) + separador;
//  el campo convocatoria se recodifica con números en lugar de caracteres
      campo := MidStr (linea_aux, campos_asignatura[8, 1], campos_asignatura[8, 2]);
      case campo[1] of
        'F' : campo := '1';
        'J' : campo := '2';
        'R' : campo := '3';
        'S' : campo := '4';
        'D' : campo := '5';
        else campo := '0';
      end;
      linea := linea + campo + separador;
//  el campo calificación se recodifica con números en lugar de caracteres
      campo := MidStr (linea_aux, campos_asignatura[6, 1], campos_asignatura[6, 2]);
      case campo[1] of
        '9' : campo := '0';
        'A', 'N' : campo := '1';
        'S' : campo := '2';
        'E' : campo := '3';
        'D' : campo := '4';
        'C' : campo := '5';
        'P' : campo := '6';
        'T' : campo := '7';
        'B' : campo := '8';
        'M' : campo := '9';
        else campo := '0';
      end;
      linea := linea + campo;
    end;

end.


