//  El módulo 'Alumnos.pas' es el encargado del manejo del registro de datos
//  'TuplaAlumno' que contiene la información extraída del fichero con los datos
//  de los alumnos.

unit Alumnos;

interface

	uses
		
		Controls, SysUtils;

	
	type

		TAtributoAlumno = (
                    al_codigo,          al_centro,          al_seccion,
                    al_plan,            al_especialidad,    al_nacimiento,
                    al_codigo_postal,   al_localidad,			  al_provincia,
                    al_genero,          al_nacionalidad,    al_expediente,
                    al_acceso,          al_calificacion,    al_curso_fin);

		TuplaAlumno = record
                    codigo        : integer;
              			centro        : integer;
              			seccion       : integer;
              			plan          : integer;
              			especialidad  : integer;
              			nacimiento    : TDate;
              			codigo_postal : integer;
              			localidad     : string;
              			provincia     : integer;
              			genero        : integer;
              			nacionalidad  : integer;
              			expediente    : TDate;
              			acceso        : integer;
              			calificacion  : integer;
              			curso_fin     : integer;
              		end;


	const

		//  posición [1] y longitud [2] de los campos del alumno
		campos : array [TAtributoAlumno] of array [1..2] of integer =
			((1, 6),    //  [1] código
			(9, 3),     //  [2] centro
			(14, 2),    //  [3] sección
			(18, 2),    //  [4] plan
			(22, 2),    //  [5] especialidad
			(26, 10),   //  [6] fecha de nacimiento
			(38, 5),    //  [7] código postal
			(45, 20),   //  [8] localidad
			(67, 2),    //  [9] provincia
			(71, 1),    //  [10] género
			(74, 3),    //  [11] nacionalidad
			(79, 10),   //  [12] fecha de expediente
			(91, 1),    //  [13] vía de acceso
			(94, 4),    //  [14] calificación acceso
			(100, 4));  //  [15] curso fin de estudios
		
    //  Para los valores de fecha no existentes o nulos se emplea la constante:
		fecha_nula = 0;


	procedure Extraer_Atributo  (linea : string; atributo : TAtributoAlumno; var valor : integer);  overload;
	procedure Extraer_Atributo  (linea : string; atributo : TAtributoAlumno; var valor : string);   overload;
	procedure Extraer_Atributo  (linea : string; atributo : TAtributoAlumno; var valor : TDate);    overload;
	procedure Leer_Atributo     (var tupla : TuplaAlumno; atributo : TAtributoAlumno; var valor : integer); overload;
	procedure Leer_Atributo     (var tupla : TuplaAlumno; atributo : TAtributoAlumno; var valor : string);  overload;
	procedure Leer_Atributo     (var tupla : TuplaAlumno; atributo : TAtributoAlumno; var valor : TDate);   overload;
	procedure Escribir_Atributo (var tupla : TuplaAlumno; atributo : TAtributoAlumno; valor : integer); overload;
	procedure Escribir_Atributo (var tupla : TuplaAlumno; atributo : TAtributoAlumno; valor : string);  overload;
	procedure Escribir_Atributo (var tupla : TuplaAlumno; atributo : TAtributoAlumno; valor : TDate);   overload;
	procedure Extraer_Tupla     (linea : string; var tupla : TuplaAlumno);



implementation


//  El procedimiento 'Extraer_Atributo' es un procedimiento polimórfico.
//  Recibe como parámetros una línea del fichero y el nombre del campo que se
//  quiere extraer, y por referencia se pasa la variable valor que contendrá el
//  valor numérico en este caso.
//  Según sea el valor de atributo se copia un segmento de la cadena de texto,
//  el que corresponde al campo recibido, se transforma al tipo de dato del
//  parámetro y se almacena en valor.

procedure Extraer_Atributo (linea : string; atributo : TAtributoAlumno; var valor : integer);

begin
	case atributo of
		al_codigo        : valor := StrToIntDef (Copy (linea, campos[al_codigo,1]       , campos[al_codigo,2])       , 0);
		al_centro        : valor := StrToIntDef (Copy (linea, campos[al_centro,1]       , campos[al_centro,2])       , 0);
		al_seccion       : valor := StrToIntDef (Copy (linea, campos[al_seccion,1]      , campos[al_seccion,2])      , 0);
		al_plan          : valor := StrToIntDef (Copy (linea, campos[al_plan,1]         , campos[al_plan,2])         , 0);
		al_especialidad  : valor := StrToIntDef (Copy (linea, campos[al_especialidad,1] , campos[al_especialidad,2]) , 0);
		al_codigo_postal : valor := StrToIntDef (Copy (linea, campos[al_codigo_postal,1], campos[al_codigo_postal,2]), 0);
		al_provincia     : valor := StrToIntDef (Copy (linea, campos[al_provincia,1]    , campos[al_provincia,2])    , 0);
		al_genero        : valor := StrToIntDef (Copy (linea, campos[al_genero,1]       , campos[al_genero,2])       , 0);
		al_acceso        : valor := StrToIntDef (Copy (linea, campos[al_acceso,1]       , campos[al_acceso,2])       , 0);
		al_calificacion  : valor := StrToIntDef (Copy (linea, campos[al_calificacion,1] , campos[al_calificacion,2]) , 0);
		al_curso_fin     : valor := StrToIntDef (Copy (linea, campos[al_curso_fin,1]    , campos[al_curso_fin,2])    , 0);
	end;
end;





//  El procedimiento 'Extraer_Atributo' es un procedimiento polimórfico.
//  Recibe como parámetros una línea del fichero y el nombre del campo que se
//  quiere extraer, y por referencia se pasa la variable valor que contendrá la
//  cadena de texto en este caso.
//  Según sea el valor de atributo se copia un segmento de la cadena de texto,
//  el que corresponde al campo recibido, se transforma al tipo de dato del
//  parámetro y se almacena en valor.

procedure Extraer_Atributo (linea : string; atributo : TAtributoAlumno; var valor : string);

begin
	case atributo of
		al_localidad    : valor := Trim (Copy (linea, campos[al_localidad, 1]   , campos[al_localidad, 2]));
		al_nacionalidad : valor := Trim (Copy (linea, campos[al_nacionalidad, 1], campos[al_nacionalidad, 2]));
	end;
end;





//  El procedimiento 'Extraer_Atributo' es un procedimiento polimórfico.
//  Recibe como parámetros una línea del fichero y el nombre del campo que se
//  quiere extraer, y por referencia se pasa la variable valor que contendrá la
//  fecha en este caso.
//  Según sea el valor de atributo se copia un segmento de la cadena de texto,
//  el que corresponde al campo recibido, se transforma al tipo de dato del
//  parámetro y se almacena en valor.

procedure Extraer_Atributo (linea : string; atributo : TAtributoAlumno; var valor : TDate);

begin
	case atributo of
		al_nacimiento : valor := StrToDateDef (Copy (linea, campos[al_nacimiento,1], campos[al_nacimiento,2]), fecha_nula);
		al_expediente : valor := StrToDateDef (Copy (linea, campos[al_expediente,1], campos[al_expediente,2]), fecha_nula);
	end;
end;





//  El procedimiento 'Leer_Atributo' es un procedimiento polimórfico.
//  Recibe como parámetros una tupla con los datos de un alumno, el nombre del
//  atributo que se quiere leer, y por referencia, la variable que contendrá el
//  valor del atributo que en este caso es un entero.
//  Dependiendo del atributo solicitado se accede al campo concreto de la tupla
//  y se asigna el contenido a la variable valor.

procedure Leer_Atributo (var tupla : TuplaAlumno; atributo : TAtributoAlumno; var valor : integer);

begin
	case atributo of
		al_codigo        : valor := tupla.codigo;
		al_centro        : valor := tupla.centro;
		al_seccion       : valor := tupla.seccion;
		al_plan          : valor := tupla.plan;
		al_especialidad  : valor := tupla.especialidad;
		al_codigo_postal : valor := tupla.codigo_postal;
		al_provincia     : valor := tupla.provincia;
		al_genero        : valor := tupla.genero;
		al_nacionalidad  : valor := tupla.nacionalidad;
		al_acceso        : valor := tupla.acceso;
		al_calificacion  : valor := tupla.calificacion;
		al_curso_fin     : valor := tupla.curso_fin;
	end;
end;





//  El procedimiento 'Leer_Atributo' es un procedimiento polimórfico.
//  Recibe como parámetros una tupla con los datos de un alumno, el nombre del
//  atributo que se quiere leer, y por referencia, la variable que contendrá el
//  valor del atributo que en este caso es una cadena de texto.
//  Dependiendo del atributo solicitado se accede al campo concreto de la tupla
//  y se asigna el contenido a la variable valor.

procedure Leer_Atributo (var tupla : TuplaAlumno; atributo : TAtributoAlumno; var valor : string);

begin
	case atributo of
		al_localidad : valor := tupla.localidad;
	end;
end;





//  El procedimiento 'Leer_Atributo' es un procedimiento polimórfico.
//  Recibe como parámetros una tupla con los datos de un alumno, el nombre del
//  atributo que se quiere leer, y por referencia, la variable que contendrá el
//  valor del atributo que en este caso es una fecha.
//  Dependiendo del atributo solicitado se accede al campo concreto de la tupla
//  y se asigna el contenido a la variable valor.

procedure Leer_Atributo (var tupla : TuplaAlumno; atributo : TAtributoAlumno; var valor : TDate);

begin
	case atributo of
		al_nacimiento : valor := tupla.nacimiento;
		al_expediente : valor := tupla.expediente;
	end;
end;





//  El procedimiento 'Escribir_Atributo' es un procedimiento polimórfico.
//  Recibe como parámetros la tupla con los datos del alumno, el nombre y el
//  valor del atributo, que en este caso es un entero, y por referencia, el
//  valor del atributo.
//  Según sea el atributo seleccionado se asigna al campo de la tupla
//  correspondiente el contenido del parámetro valor.

procedure Escribir_Atributo (var tupla : TuplaAlumno; atributo : TAtributoAlumno; valor : integer);

begin
	case atributo of
		al_codigo        : tupla.codigo := valor;
		al_centro        : tupla.centro := valor;
		al_seccion       : tupla.seccion := valor;
		al_plan          : tupla.plan := valor;
		al_especialidad  : tupla.especialidad := valor;
		al_codigo_postal : tupla.codigo_postal := valor;
		al_provincia     : tupla.provincia := valor;
		al_genero        : tupla.genero := valor;
		al_nacionalidad  : tupla.nacionalidad := valor;
		al_acceso        : tupla.acceso := valor;
		al_calificacion  : tupla.calificacion := valor;
		al_curso_fin     : tupla.curso_fin := valor;
	end;
end;





//  El procedimiento 'Escribir_Atributo' es un procedimiento polimórfico.
//  Recibe como parámetros la tupla con los datos del alumno, el nombre y el
//  valor del atributo, que en este caso es una cadena de texto, y por
//  referencia, el valor del atributo.
//  Según sea el atributo seleccionado se asigna al campo de la tupla
//  correspondiente el contenido del parámetro valor.

procedure Escribir_Atributo (var tupla : TuplaAlumno; atributo : TAtributoAlumno; valor : string);

begin
	case atributo of
		al_localidad : tupla.localidad := valor;
	end;
end;





//  El procedimiento 'Escribir_Atributo' es un procedimiento polimórfico.
//  Recibe como parámetros la tupla con los datos del alumno, el nombre y el
//  valor del atributo, que en este caso es una fecha, y por referencia, el
//  valor del atributo.
//  Según sea el atributo seleccionado se asigna al campo de la tupla
//  correspondiente el contenido del parámetro valor.

procedure Escribir_Atributo (var tupla : TuplaAlumno; atributo : TAtributoAlumno; valor : TDate);

begin
	case atributo of
		al_nacimiento : tupla.nacimiento := valor;
		al_expediente : tupla.expediente := valor;
	end;
end;





//  El procedimiento 'Extraer_Tupla' rellena todos los campos de la tupla con la
//  información extraída de una línea del fichero de alumnos.
//  Recibe como parámetros una línea de texto y, por referencia, la tupla de un
//  alumno.
//  A cada uno de los campos de la tupla se le asigna el valor correspondiente,
//  extraído de la línea del fichero de alumno; la posición y longitud del campo
//  viene indicada en la tupla campos. Para los valores entero y fecha se
//  realiza una conversión de texto a entero o fecha según corresponda. Para el
//  campo localidad, que es el único de tipo texto, se recortan los espacios en
//  blanco al principio y al final de la cadena si los hubiera.

procedure Extraer_Tupla (linea : string; var tupla : TuplaAlumno);

begin
		tupla.codigo        := StrToIntDef  (Copy (linea, campos[al_codigo,1]       , campos[al_codigo,2])       , 0);
		tupla.centro        := StrToIntDef  (Copy (linea, campos[al_centro,1]       , campos[al_centro,2])       , 0);
		tupla.seccion       := StrToIntDef  (Copy (linea, campos[al_seccion,1]      , campos[al_seccion,2])      , 0);
		tupla.plan          := StrToIntDef  (Copy (linea, campos[al_plan,1]         , campos[al_plan,2])         , 0);
		tupla.especialidad  := StrToIntDef  (Copy (linea, campos[al_especialidad,1] , campos[al_especialidad,2]) , 0);
		tupla.nacimiento    := StrToDateDef (Copy (linea, campos[al_nacimiento,1]   , campos[al_nacimiento,2])   , fecha_nula);
		tupla.codigo_postal := StrToIntDef  (Copy (linea, campos[al_codigo_postal,1], campos[al_codigo_postal,2]), 0);
		tupla.localidad     := Trim         (Copy (linea, campos[al_localidad,1]    , campos[al_localidad,2]));
		tupla.provincia     := StrToIntDef  (Copy (linea, campos[al_provincia,1]    , campos[al_provincia,2])    , 0);
		tupla.genero        := StrToIntDef  (Copy (linea, campos[al_genero,1]       , campos[al_genero,2])       , 0);
		tupla.nacionalidad  := StrToIntDef  (Copy (linea, campos[al_nacionalidad,1] , campos[al_nacionalidad,2]) , 0);
		tupla.expediente    := StrToDateDef (Copy (linea, campos[al_expediente,1]   , campos[al_expediente,2])   , fecha_nula);
		tupla.acceso        := StrToIntDef  (Copy (linea, campos[al_acceso,1]       , campos[al_acceso,2])       , 0);
		tupla.calificacion  := StrToIntDef  (Copy (linea, campos[al_calificacion,1] , campos[al_calificacion,2]) , 0);
		tupla.curso_fin     := StrToIntDef  (Copy (linea, campos[al_curso_fin,1]    , campos[al_curso_fin,2])    , 0);
	end;

end.
