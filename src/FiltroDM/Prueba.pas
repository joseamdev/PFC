//  El módulo Escritura.pas contiene los procedimientos y funciones encargados
//  de validar y grabar la información en el fichero de salida.

unit Prueba;


interface

  uses
    SysUtils, Tablas, Experiencias;

  procedure CrearFicherosPrueba (nombre_fichero : string; var fichero1, fichero2 : TextFile);
  procedure EscribirLineaPrueba (var fichero : TextFile; var lista : TListaExperiencia; codigo_al : integer);
  procedure CerrarFicherosPrueba (var fichero1, fichero2 : TextFile);
  function Leer_Dato (tupla : TExperiencia; atributo : TExperienciaAtributo)                             : string; overload;
  function Leer_Dato (tupla : TExperiencia; atributo : TExperienciaAsignaturaAtributo; indice : integer) : string; overload;



implementation

uses
  Variants, Controls, DateUtils, Main;





procedure CrearFicherosPrueba (nombre_fichero : string; var fichero1, fichero2 : TextFile);

begin
  FileMode := fmOpenWrite;
  AssignFile (fichero1, nombre_fichero + '1');
  AssignFile (fichero2, nombre_fichero + '2');
  Rewrite  (fichero1);
  Rewrite  (fichero2);
end;





procedure EscribirLineaPrueba (var fichero : TextFile; var lista : TListaExperiencia; codigo_al : integer);

const
  COMA = ',';

var
  numero_exp, numero_asig, indice_exp, indice_asig : integer;
  linea : string;
  atr_exp : TExperienciaAtributo;
  atr_asig : TExperienciaAsignaturaAtributo;

begin
  numero_exp := lista.Numero_Elementos - 1;
  for indice_exp := 0 to numero_exp do begin
    linea := IntToStr (codigo_al) + ' : ';
    for atr_exp := low (TExperienciaAtributo) to high (TExperienciaAtributo) do
      linea := linea + Leer_Dato (lista.Experiencia(indice_exp), atr_exp) + COMA;

    WriteLn (fichero, linea);

    numero_asig := lista.Experiencia(indice_exp).Lista_Asignatura.Numero_Elementos - 1;
    for indice_asig := 0 to numero_asig do begin
      linea := ' - ';
      for atr_asig := low (TExperienciaAsignaturaAtributo) to high (TExperienciaAsignaturaAtributo) do
        linea := linea + Leer_dato (lista.Experiencia(indice_exp), atr_asig, indice_asig) + COMA;
      WriteLn (fichero, linea);
    end;
  end;
  Flush (fichero);
end;





procedure CerrarFicherosPrueba (var fichero1, fichero2 : TextFile);
begin
  Flush (fichero1);
  Flush (fichero2);
  CloseFile (fichero1);
  CloseFile (fichero2);
end;





function Leer_Dato (tupla : TExperiencia; atributo : TExperienciaAtributo) : string;

var
  valor : integer;
  fecha : TDate;

begin
  case atributo of
    ne_centro..ne_titulacion      : begin
                                      tupla.Leer (atributo, valor);
                                      result := IntToStr (valor);
                                    end;
    ne_localidad                  : tupla.Leer (atributo, result);
    ne_nacimiento..ne_expediente  : begin
                                      tupla.Leer (atributo, fecha);
                                      result := DateToStr (fecha);
                                    end;
  else
    result := '?';
  end;
end;





function Leer_Dato (tupla : TExperiencia; atributo : TExperienciaAsignaturaAtributo; indice : integer) : string;
begin
  case atributo of
    nea_centro..nea_num_matriculas : result := IntToStr (tupla.Lista_Asignatura.Asignatura(indice).Leer (atributo));
  else
    result := '?';
  end;
end;

end.

