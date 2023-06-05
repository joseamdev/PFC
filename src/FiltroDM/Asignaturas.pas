//  El módulo 'Asignatura.pas' es el encargado del manejo del registro de datos
//  'TuplaAsignatura' que contiene la información extraída del fichero con los
//  datos de las asignaturas.

unit Asignaturas;


interface

  uses

    SysUtils;

  type

    TAtributoAsignatura = (
                          as_alumno,              as_centro,
                          as_seccion,             as_plan,
                          as_asignatura,          as_calificacion,
                          as_curso,               as_convocatoria,
                          as_num_convocatorias,   as_num_matriculas);

    TuplaAsignatura = record
                        alumno            : integer;
                        centro            : integer;
                        seccion           : integer;
                        plan              : integer;
                        asignatura        : integer;
                        calificacion      : integer;
                        curso             : integer;
                        convocatoria      : integer;
                        num_convocatorias : integer;
                        num_matriculas    : integer;
                      end;

  const

    //  almacena la [1] posición y [2] longitud de los campos de la asignatura
    campos : array [TAtributoAsignatura] of array [1..2] of integer = (
            ( 1, 6),    //  [1] código alumno
            ( 9, 3),    //  [2] centro
            (14, 2),    //  [3] sección
            (18, 2),    //  [4] plan de estudios
            (22, 3),    //  [5] código asignatura
            (44, 1),    //  [6] calificación
            (27, 4),    //  [7] curso
            (41, 1),    //  [8] convocatoria
            (33, 2),    //  [9] número de convocatorias
            (37, 2));   //  [10] número de matriculaciones


  procedure Extraer_Atributo  (linea : string; atributo : TAtributoAsignatura; var valor : integer);
  procedure Leer_Atributo     (var tupla : TuplaAsignatura; atributo : TAtributoAsignatura; var valor : integer);
  procedure Escribir_Atributo (var tupla : TuplaAsignatura; atributo : TAtributoAsignatura; valor : integer);
  procedure Extraer_Tupla     (linea : string; var tupla : TuplaAsignatura);



implementation


//  El procedimiento 'Extraer_Atributo' extrae de la línea de texto del fichero
//  de datos de las asignaturas el atributo seleccionado y lo almacena en la
//  variable 'valor'.
//  Recibe los parámetros 'linea' y 'atributo' con la línea de texto y el nombre
//  del atributo que se quiere extraer, y por referencia, la variable valor que
//  contendrá el valor del atributo.
//  Según sea el nombre del atributo que se quiere extraer se ejecuta la línea//  de código correspondiente. Se copian los caracteres de la línea siguiendo//  los datos de la tabla campos, se transforman a formato entero y se asigna el//  valor numérico resultante a la variable valor.
procedure Extraer_Atributo (linea : string; atributo : TAtributoAsignatura; var valor : integer);

begin
  case atributo of
    as_alumno             : valor := StrToIntDef (Copy (linea, campos[as_alumno,1],             campos[as_alumno,2]), 0);
    as_centro             : valor := StrToIntDef (Copy (linea, campos[as_centro,1],             campos[as_centro,2]), 0);
    as_plan               : valor := StrToIntDef (Copy (linea, campos[as_plan,1],               campos[as_plan,2]), 0);
    as_seccion            : valor := StrToIntDef (Copy (linea, campos[as_seccion,1],            campos[as_seccion,2]), 0);
    as_asignatura         : valor := StrToIntDef (Copy (linea, campos[as_asignatura,1],         campos[as_asignatura,2]), 0);
    as_calificacion       : valor := StrToIntDef (Copy (linea, campos[as_calificacion,1],       campos[as_calificacion,2]), 0);
    as_curso              : valor := StrToIntDef (Copy (linea, campos[as_curso,1],              campos[as_curso,2]), 0);
    as_convocatoria       : valor := StrToIntDef (Copy (linea, campos[as_convocatoria,1],       campos[as_convocatoria,2]), 0);
    as_num_convocatorias  : valor := StrToIntDef (Copy (linea, campos[as_num_convocatorias,1],  campos[as_num_convocatorias,2]), 0);
    as_num_matriculas     : valor := StrToIntDef (Copy (linea, campos[as_num_matriculas,1],     campos[as_num_matriculas,2]), 0);
  end;
end;





//  El procedimiento 'Leer_Atributo' lee un campo de una asignatura de una tupla.
//  Recibe como parámetros una tupla con los datos de una asignatura, el nombre
//  del atributo que se quiere leer, y por referencia, la variable que contendrá
//  el valor del atributo.
//  Dependiendo del atributo solicitado se accede al campo concreto de la tupla//  y se asigna el contenido a la variable valor.
procedure Leer_Atributo (var tupla : TuplaAsignatura; atributo : TAtributoAsignatura; var valor : integer);

begin
  case atributo of
    as_alumno             : valor := tupla.alumno;
    as_centro             : valor := tupla.centro;
    as_seccion            : valor := tupla.seccion;
    as_plan               : valor := tupla.plan;
    as_asignatura         : valor := tupla.asignatura;
    as_calificacion       : valor := tupla.calificacion;
    as_curso              : valor := tupla.curso;
    as_convocatoria       : valor := tupla.convocatoria;
    as_num_convocatorias  : valor := tupla.num_convocatorias;
    as_num_matriculas     : valor := tupla.num_matriculas;
  end;
end;





//  El procedimiento 'Escribir_Atributo' devuelve el valor del campo
//  seleccionado de una tupla de asignaturas.
//  Recibe como parámetros la tupla con los datos de la asignatura, el nombre y
//  el valor del atributo, y por referencia, la variable valor que contendrá el
//  valor del campo seleccionado.
//  Según sea el atributo seleccionado se asigna al campo de la tupla//  correspondiente el contenido del parámetro valor.

procedure Escribir_Atributo (var tupla : TuplaAsignatura; atributo : TAtributoAsignatura; valor : integer);

begin
  case atributo of
    as_alumno             : tupla.alumno := valor;
    as_centro             : tupla.centro := valor;
    as_seccion            : tupla.seccion := valor;
    as_plan               : tupla.plan := valor;
    as_asignatura         : tupla.asignatura := valor;
    as_calificacion       : tupla.calificacion := valor;
    as_curso              : tupla.curso := valor;
    as_convocatoria       : tupla.convocatoria := valor;
    as_num_convocatorias  : tupla.num_convocatorias := valor;
    as_num_matriculas     : tupla.num_matriculas := valor;
  end;
end;





//  El procedimiento 'Extraer_Tupla' rellena todos los campos de la tupla con la
//  información extraída de una línea del fichero de asignaturas.
//  Recibe como parámetros una línea de texto y, por referencia, la tupla de una
//  asignatura.
//  A cada uno de los campos de la tupla se le asigna el valor correspondiente,//  extraído de la línea del fichero de asignaturas; la posición y longitud del//  campo viene indicada en la tupla 'campos'. Los valores se convierten de
//  texto a entero.

procedure Extraer_Tupla (linea : string; var tupla : TuplaAsignatura);

begin
  tupla.alumno := StrToIntDef (Copy (linea, campos[as_alumno,1], campos[as_alumno,2]), 0);
  tupla.centro := StrToIntDef (Copy (linea, campos[as_centro,1], campos[as_centro,2]), 0);
  tupla.seccion := StrToIntDef (Copy (linea, campos[as_seccion,1], campos[as_seccion,2]), 0);
  tupla.plan := StrToIntDef (Copy (linea, campos[as_plan,1], campos[as_plan,2]), 0);
  tupla.asignatura := StrToIntDef (Copy (linea, campos[as_asignatura,1], campos[as_asignatura,2]), 0);
  tupla.calificacion := StrToIntDef (Copy (linea, campos[as_calificacion,1], campos[as_calificacion,2]), 0);
  tupla.curso := StrToIntDef (Copy (linea, campos[as_curso,1], campos[as_curso,2]), 0);
  tupla.convocatoria := StrToIntDef (Copy (linea, campos[as_convocatoria,1], campos[as_convocatoria,2]), 0);
  tupla.num_convocatorias := StrToIntDef (Copy (linea, campos[as_num_convocatorias,1], campos[as_num_convocatorias,2]), 0);
  tupla.num_matriculas := StrToIntDef (Copy (linea, campos[as_num_matriculas,1], campos[as_num_matriculas,2]), 0);
end;

end.
