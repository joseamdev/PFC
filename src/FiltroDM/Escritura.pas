//  El módulo Escritura.pas contiene los procedimientos y funciones encargados
//  de validar y grabar la información en el fichero de salida.

unit Escritura;


interface

  uses
    SysUtils, Classes, Variants, Controls, DateUtils, Main, Tablas, Experiencias;

  type
    TAtributoNominal = (nan_titulacion, nan_especialidad, nan_codigo_postal, nan_nacionalidad);
    TListaValoresAtributos = array [TAtributoNominal] of TStringList;


  procedure CrearFicheros       (nombre_fichero : string; var fichero, fichero_temp : TextFile; var lista_valores : TListaValoresAtributos);
  procedure EscribirLineaDatos  (var fichero : TextFile; var lista : TListaExperiencia; tabla_s : TTablaSeleccion; tabla_f : TTablaFiltrado; var contador : integer; var lista_valores : TListaValoresAtributos);
  procedure EscribirCabecera    (var fichero : TextFile; tabla_s : TTablaSeleccion; tabla_f : TTablaFiltrado; var lista_valores : TListaValoresAtributos);
  procedure PasarDatos          (var fichero, fichero_temp : TextFile);
  procedure CerrarFicheros      (var fichero, fichero_temp : TextFile; var lista_valores : TListaValoresAtributos);
  function Cumple_Condiciones   (tupla : TExperiencia; tabla_f : TTablaFiltrado; clase : TAtributoClase) : boolean;
  function Leer_Dato            (tupla : TExperiencia; atributo : TAtributoSeleccion)                   : integer; overload;
  function Leer_Dato            (tupla : TExperiencia; atributo : TAtributoSeleccion; indice : integer) : integer; overload;
  function Leer_Dato            (tupla : TExperiencia; atributo : TAtributoClase)                       : integer; overload;
  function Conversion_Dato      (atributo : TAtributoSeleccion; valor : integer) : string; overload;
  function Conversion_Dato      (atributo : TAtributoClase;     valor : integer) : string; overload;
  procedure AnyadirValores      (tupla : TExperiencia; var lista_valores : TListaValoresAtributos);
  procedure DefinicionAtributo  (atributo : TAtributoSeleccion; lista_valores : TListaValoresAtributos; var linea : string);

implementation


const

  COMA = ',';

  FICHERO_TEMPORAL_DATOS = '$$$datos_salida.temp';

  //  Se crean como constantes unas tablas que almacenan las líneas de texto que
  //  se usarán como comentarios o definición de variables en la cabecera del
  //  fichero.

  //  Tabla de comentarios de texto correspondientes a cada atributo seleccionado.
  com_atr_sel  : array [TAtributoSeleccion] of string
                 =  ('% - Titulación.',
                     '% - Especialidad.',
                     '% - Edad de inicio de la carrera.',
                     '% - Edad final de la carrera.',
                     '% - Género.',
                     '% - Código postal.',
                     '% - Provincia.',
                     '% - Nacionalidad.',
                     '% - Expediente.',
                     '% - Vía de acceso a la Universidad.',
                     '% - Calificación de acceso.',
                     '% - Curso de finalización.',
                     '% - Duración de los estudios.',
                     '% - Número de aprobados.',
                     '% - Número de notables.',
                     '% - Número de sobresalientes.',
                     '% - Número de matrículas de honor.',
                     '% - Número de suspensos.',
                     '% - Número de no presentados.',
                     '% - Nota media final.',
                     '% - Calificación de la asignatura.',
                     '% - Curso de la asignatura.',
                     '% - Convocatoria de la asignatura.',
                     '% - Número de convocatoria de la asignatura.',
                     '% - Número de matrículas de la asignatura.');

  //  Tabla de comentarios de texto correspondientes a cada atributo filtrado.
	com_atr_filt : array [TAtributoFiltrado] of string
                 =  ('% - Género: ',
                     '% - Nacionalidad: ',
                     '% - Provincia: ',
                     '% - Vía de acceso: ',
                     '% - Estudios finalizados: ',
                     '% - Asignatura: ');

  //  Tabla de comentarios de texto correspondientes a la clase.
  com_clase    : array [TAtributoClase] of string
                 =  ('% - Código postal.',
                     '% - Provincia.',
                     '% - Género.',
                     '% - Nacionalidad.',
                     '% - Vía de acceso.',
                     '% - Duración de los estudios.',
                     '% - Titulación.');

  //  Tabla de definiciones de los atributos.
  def_atributo : array [TAtributoSeleccion] of string
                  =  ('@attribute TITULACION {',
                      '@attribute ESPECIALIDAD {',
                      '@attribute EDAD_INICIO integer',
                      '@attribute EDAD_FINAL integer',
                      '@attribute GENERO {H, M}',
                      '@attribute CODIGO_POSTAL {',
                      '@attribute PROVINCIA {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52}',
                      '@attribute NACIONALIDAD {',
                      '@attribute EXPEDIENTE integer',
                      '@attribute ACCESO {2ciclo, adaptacion, COU, erasmus, FP, mayor25, selectividad, titulado}',
                      '@attribute CALIFICACION integer',
                      '@attribute CURSO_FIN integer',
                      '@attribute DURACION {1anyo, 2anyos, 3anyos, 4anyos, 5anyos, 6anyos, 7anyos, 8anyos, 9anyos, 10anyos, mas_de_10}',
                      '@attribute NUM_APROBADOS integer',
                      '@attribute NUM_NOTABLES integer',
                      '@attribute NUM_SOBRESALIENTES integer',
                      '@attribute NUM_MATRICULAS_HONOR integer',
                      '@attribute NUM_SUSPENSOS integer',
                      '@attribute NUM_NO_PRESENTADOS integer',
                      '@attribute NOTA_MEDIA integer',
                      '@attribute ASIG_CALIFICACION {SC, NP, suspenso, equivalencia, adaptada, convalidada, aprobado, notable, sobresaliente, MH}',
                      '@attribute ASIG_CURSO date "yyyy"',
                      '@attribute ASIG_CONVOCATORIA {Feb, Jun, Jun_ext, Sep, Dic}',
                      '@attribute ASIG_NUM_CONVOCATORIA {ninguna, una, dos, tres, cuatro, cinco, mas_de_5}',
                      '@attribute ASIG_NUM_MATRICULAS {ninguna, una, dos, tres, cuatro, cinco, mas_de_5}');

  //  Tabla de definiciones de la clase.
  def_clase    : array [TAtributoClase] of string
                  =  ('@attribute CODIGO_POSTAL {',
                      '@attribute PROVINCIA {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52}',
                      '@attribute GENERO {H, M}',
                      '@attribute NACIONALIDAD {',
                      '@attribute ACCESO {2ciclo, adaptacion, COU, erasmus, FP, mayor25, selectividad, titulado}',
                      '@attribute DURACION {1anyo, 2anyos, 3anyos, 4anyos, 5anyos, 6anyos, 7anyos, 8anyos, 9anyos, 10anyos, mas_de_10}',
                      '@attribute TITULACION {');



//  El procedimiento 'CrearFichero' crea el fichero de salida con las
//  experiencias a la par que crea el fichero temporal que contendrá los datos.
//  También se crea la lista de valores nominales a medida.

procedure CrearFicheros (nombre_fichero : string; var fichero, fichero_temp : TextFile; var lista_valores : TListaValoresAtributos);

var
  atr : TAtributoNominal;

begin
  //  Se establece el modo del fichero en modo escritura. Se asigna un puntero al
  //  nombre del fichero pasado como parámetro y se crea en el disco.
  FileMode := fmOpenWrite;
  AssignFile (fichero, nombre_fichero);
  Rewrite    (fichero);

  //  Se asigna un puntero al fichero temporal de datos y se crea en el disco.
  //  Se escribe el indicador de comienzo de líneas de datos.
  AssignFile  (fichero_temp, FICHERO_TEMPORAL_DATOS);
  Rewrite     (fichero_temp);
	WriteLn     (fichero_temp, '@data');

  //  Se crea la lista que contendrá los valores de los atributos nominales
  //  recogidos de las experiencias. Los valores estarán ordenados y no habrá
  //  valores repetidos.
  for atr := Low (TAtributoNominal) to High (TAtributoNominal) do begin
    lista_valores[atr]            := TStringList.Create;
    lista_valores[atr].Sorted     := true;
    lista_valores[atr].Duplicates := dupIgnore;
  end;
end;





//  El procedimiento 'EscribirCabecera' escribe las definiciones del fichero de
//  salida con las experiencias.

procedure EscribirCabecera (var fichero : TextFile; tabla_s : TTablaSeleccion; tabla_f : TTablaFiltrado; var lista_valores : TListaValoresAtributos);

var
  atr : TAtributoSeleccion;
  indice, numero, centro, seccion, plan, asignatura : integer;
  texto_atr, texto_asig, s : string;

begin
  //  Al inicio del fichero se escriben los comentarios.
  WriteLn (fichero, '% Conjunto de experiencias generado a partir de la base de datos de los alumnos de la UMA');
  WriteLn (fichero, '%');

  //  Se escriben los comentarios que indican las opciones de filtrado.
  WriteLn (fichero, '% Este fichero incluye las experiencias de:');
  if tabla_f.Leer (naf_genero)       > 0 then WriteLn (fichero, com_atr_filt[naf_genero]       + FormFiltroDM.ComboBoxGenero.Text);
  if tabla_f.Leer (naf_nacionalidad) > 0 then WriteLn (fichero, com_atr_filt[naf_nacionalidad] + FormFiltroDM.ComboBoxNacion.Text);
  if tabla_f.Leer (naf_provincia)    > 0 then WriteLn (fichero, com_atr_filt[naf_provincia]    + FormFiltroDM.ComboBoxProvincia.Text);
  if tabla_f.Leer (naf_acceso)       > 0 then WriteLn (fichero, com_atr_filt[naf_acceso]       + FormFiltroDM.ComboBoxAcceso.Text);
  if tabla_f.Leer (naf_finalizado)   > 0 then WriteLn (fichero, com_atr_filt[naf_finalizado]   + FormFiltroDM.ComboBoxFinalizado.Text);
  if tabla_f.Leer (naf_asignatura)   > 0 then
  begin
    case tabla_f.Leer(naf_asignatura) of
      1 :
        begin
          WriteLn (fichero, com_atr_filt[naf_asignatura] + 'Asignatura aprobada:');
          WriteLn (fichero, '%   - ' + FormFiltroDM.EditAsigAprobada.Text);
        end;
      2 :
        begin
          WriteLn (fichero, com_atr_filt[naf_asignatura] + 'Estudios cursados:');
          numero := FormFiltroDM.ListBoxFiltAsig.Items.Count - 1;
          for indice := 0 to numero do
            WriteLn (fichero, '%   - ' + FormFiltroDM.ListBoxFiltAsig.Items[indice]);
        end;
    end;
  end;
  if (tabla_f.Leer (naf_genero)     = 0) and (tabla_f.Leer (naf_nacionalidad) = 0) and
     (tabla_f.Leer (naf_provincia)  = 0) and (tabla_f.Leer (naf_acceso)       = 0) and
     (tabla_f.Leer (naf_finalizado) = 0) and (tabla_f.Leer (naf_asignatura)   = 0) then
        WriteLn (fichero, '% - Todas las experiencias.');
  WriteLn (fichero, '%');

  //  Se escriben en el fichero los comentarios sobre los atributos seleccionados.
  WriteLn (fichero, '% Los atributos seleccionados son:');
  for atr := low (TAtributoSeleccion) to nas_nota_media do
	   if tabla_s.Leer_Seleccion (atr) then
       WriteLn (fichero, com_atr_sel[atr]);
  if FormFiltroDM.ListBoxSelAsig2.Enabled then begin
    for atr := nas_asig_calificacion to high (TAtributoSeleccion) do
      if tabla_s.Leer_Seleccion(atr) then
        WriteLn (fichero, com_atr_sel[atr]);
    numero := FormFiltroDM.ListBoxSelAsig2.Items.Count - 1;
    for indice := 0 to numero do
      WriteLn (fichero, '%   - ' + FormFiltroDM.ListBoxSelAsig2.Items[indice]);
  end;

  WriteLn (fichero, '%');

  //  Se escribe en el fichero el comentario sobre el atributo seleccionado como
  //  la clase.
  WriteLn (fichero, '% El atributo seleccionado como clase es:');
  WriteLn (fichero, com_clase[tabla_s.Leer_Clase]);

  //  Escritura de la cabecera de datos del fichero.
  //  Primeramente el nombre de la relación.
  WriteLn (fichero, '');
  WriteLn (fichero, '@relation GECUMA');
  WriteLn (fichero, '');

  //  Escritura de los atributos en la cabecera de datos.
  for atr := low (TAtributoSeleccion) to nas_nota_media do
    if tabla_s.Leer_Seleccion (atr) then begin
      DefinicionAtributo (atr, lista_valores, s);
      WriteLn (fichero, def_atributo[atr] + s);
    end;
  //  Escritura de los atributos de las asignaturas en la cabecera de datos.
  if FormFiltroDM.ListBoxSelAsig2.Enabled then begin
    numero := tabla_s.Lista_Asignatura.Numero_Elementos - 1;
    for indice := 0 to numero do begin
      centro     := tabla_s.Lista_Asignatura.Leer_Centro(indice);
      seccion    := tabla_s.Lista_Asignatura.Leer_Seccion(indice);
      plan       := tabla_s.Lista_Asignatura.Leer_Plan(indice);
      asignatura := tabla_s.Lista_Asignatura.Leer_Asignatura(indice);

      //  Se comprimen los códigos de centro, sección, plan y asignatura en un solo
      //  texto de 3 + 2 + 2 + 3 caracteres.
      texto_asig := IntToStr (centro);
      while Length (texto_asig) < 3 do
        texto_asig := '0' + texto_asig;
      s := IntToStr (seccion);
      if Length (s) = 1 then
        s := '0' + s;
      texto_asig := texto_asig + s;
      s := IntToStr (plan);
      if Length (s) = 1 then
        s := '0' + s;
      texto_asig := texto_asig + s;
      s := IntToStr (asignatura);
      if Length (s) = 1 then
        s := '0' + s;
      texto_asig := texto_asig + s + '_';

      for atr := nas_asig_calificacion to high (TAtributoSeleccion) do
        if tabla_s.Leer_Seleccion (atr) then begin
          texto_atr := def_atributo[atr];
          Insert (texto_asig, texto_atr, 17);
          WriteLn (fichero, texto_atr);
        end;
    end;
  end;

  //  Escritura de la variable clase en la cabecera de datos.
  WriteLn (fichero, def_clase[tabla_s.Leer_Clase]);
	WriteLn (fichero, '');

  //  Se vacía la caché en el disco.
  Flush (fichero);
end;





//  El procedimiento 'EscribirLineaDatos' prepara el contenido de la lista de
//  experiencias y lo convierte a una línea de texto que se escribirá en el
//  fichero de salida.

procedure EscribirLineaDatos (var fichero : TextFile; var lista : TListaExperiencia; tabla_s : TTablaSeleccion; tabla_f : TTablaFiltrado; var contador : integer; var lista_valores : TListaValoresAtributos);

var
  numero, numero_asig, valor, centro, seccion, plan, asignatura, indice, indice_asig : integer;
  linea : string;
  atr_s : TAtributoSeleccion;
  coincidencia : boolean;

begin
  //  El número de elementos de la lista de experiencias se guarda en la variable
  //  numero. Mientras haya elementos en la lista se va recorriendo la misma y
  //  eliminando el elemento analizado.
  numero := lista.Numero_Elementos;
  while numero > 0 do begin
    dec (numero);
    //  A la variable de texto linea se le asigna una cadena vacía; en ella se irá
    //  almacenando el contenido que se escribirá en el fichero.
    linea := '';
    //  Se comprueba si la experiencia cumple las condiciones pasadas como parámetro
    //  en la tabla de filtrado.
    if Cumple_Condiciones (lista.Experiencia(0), tabla_f, tabla_s.Leer_Clase) then begin
      //  Si se cumplen las condiciones de filtrado entonces se comienza la escritura
      //  de los atributos.
      //  Se recorre la lista de atributos, se comprueba si está seleccionado y en
      //  caso afirmativo se añade a la variable linea. Cada uno de los atributos irá
      //  separado por comas.
      for atr_s := low (TAtributoSeleccion) to nas_nota_media do begin
        if tabla_s.Leer_Seleccion (atr_s) then begin
          valor := Leer_Dato (lista.Experiencia(0), atr_s);
          linea := linea + Conversion_Dato (atr_s, valor) + COMA;
        end;
      end;
      //  Igualmente se trabaja con los atributos de la lista de asignaturas, que se
      //  irán añadiendo a la variable auxiliar linea separados por comas.
      if FormFiltroDM.ListBoxSelAsig2.Enabled then begin
        numero_asig := tabla_s.Lista_Asignatura.Numero_Elementos - 1;
        for indice := 0 to numero_asig do begin
          centro     := tabla_s.Lista_Asignatura.Leer_Centro(indice);
          seccion    := tabla_s.Lista_Asignatura.Leer_Seccion(indice);
          plan       := tabla_s.Lista_Asignatura.Leer_Plan(indice);
          asignatura := tabla_s.Lista_Asignatura.Leer_Asignatura(indice);
          indice_asig := 0;
          lista.Experiencia(0).Lista_Asignatura.Buscar (centro, seccion, plan, asignatura, indice_asig, coincidencia);
          for atr_s := nas_asig_calificacion to high (TAtributoSeleccion) do begin
            if tabla_s.Leer_Seleccion (atr_s) then begin
              if coincidencia then
                valor := Leer_Dato (lista.Experiencia(0), atr_s, indice_asig)
              else
                valor := 0;
              linea := linea + Conversion_Dato (atr_s, valor) + COMA;
            end;
          end;
        end;
      end;
      //  Como último atributo se escribe el valor de la clase.
      valor := Leer_Dato (lista.Experiencia(0), tabla_s.Leer_Clase);
      linea := linea + Conversion_Dato (tabla_s.Leer_Clase, valor);
      //  La variable linea, que contiene los valores de una experiencia, se escribe
      //  en el fichero. Se incrementa la variable contador que lleva la cuenta del
      //  número de experiencias que se almacenan en el fichero.
      WriteLn (fichero, linea);
      //  Se añaden a la lista de valores los de los atributos de la tupla.
      AnyadirValores (lista.Experiencia(0), lista_valores);
      //  Para finalizar se vacía la caché en el fichero y se elimina la
      //  experiencia de la lista antes de volver a iterar.
      inc (contador);
      Flush (fichero);
    end;
    lista.Eliminar(0);
  end;
end;





//  El procedimiento 'PasarDatos' se encarga de transpasar la información de las
//  experiencias contenidas en el fichero temporal al fichero de salida.

procedure PasarDatos (var fichero, fichero_temp : TextFile);

var
  linea_datos : string;
  
begin
  CloseFile (fichero_temp);
  FileMode := fmOpenRead;
  AssignFile (fichero_temp, FICHERO_TEMPORAL_DATOS);
  Reset (fichero_temp);
  while not SeekEof (fichero_temp) do begin
    ReadLn (fichero_temp, linea_datos);
    WriteLn (fichero, linea_datos);
  end;
end;





//  El procedimiento 'CerrarFicheros' vacía el contenido de la caché de disco y
//  cierra los ficheros. Borra también el fichero temporal y la lista de valores.

procedure CerrarFicheros (var fichero, fichero_temp : TextFile; var lista_valores : TListaValoresAtributos);

var
  atr : TAtributoNominal;
  
begin
  Flush (fichero);
  Flush (fichero_temp);
  CloseFile (fichero);
  CloseFile (fichero_temp);
  DeleteFile (FICHERO_TEMPORAL_DATOS);
  for atr := Low (TAtributoNominal) to High (TAtributoNominal) do
    lista_valores[atr].Free;
end;





//  La función CumpleCondiciones comprueba los valores de la experiencia pasada
//  como parámetro y los compara con la tabla de filtrado. Se devuelve el valor
//  de función true si todos los atributos coinciden. También se comprueba que
//  el valor de la clase no sea nulo.

function Cumple_Condiciones (tupla : TExperiencia; tabla_f : TTablaFiltrado; clase : TAtributoClase) : boolean;

var
  valor_tupla, valor_filtro, num_elem_1, num_elem_2, indice, indice2, centro, seccion, plan, asignatura : integer;

begin
  //  Se pone true como valor por defecto para la función. A partir de ahí se
  //  empieza a comprobar uno a uno todos los atributos de la experiencia si
  //  coinciden con los datos reflejados en la tabla de filtrado.
  result := true;

  //  Si el género está definido y no corresponde con el valor de la tabla de
  //  filtrado entonces se termina la función con el comando exit y se devuelve
  //  como valor de la función false.
  tupla.Leer (ne_genero, valor_tupla);
  valor_filtro := tabla_f.Leer(naf_genero);
  if (valor_filtro > 0) and (valor_tupla <> valor_filtro) then begin
    result := false;
    exit;
  end;

  //  Si la nacionalidad está definida entonces se comprueba si coincide la
  //  selección hecha en la tabla de filtrado con el valor de la experiencia. Si
  //  está seleccionada la nacionalidad española (código 47) o extranjera (otro
  //  código) y el valor del atributo de la experiencia no coincide entonces se
  //  devuelve false.
  tupla.Leer (ne_nacionalidad, valor_tupla);
  valor_filtro := tabla_f.Leer(naf_nacionalidad);
  if valor_filtro > 0 then
  begin
    case valor_filtro of
      1 :
        if valor_tupla <> 47 then begin
          result := false;
          exit;
        end;
      2 :
        if valor_tupla = 47 then begin
          result := false;
          exit;
        end;
    end;
  end;

  //  Si la provincia está definida entonces se comprueba si coincide con el valor
  //  de la experiencia. Si se marcó la provincia de Málaga (código 29) u otra
  //  provincia (códigos restantes) y el valor no coincide con la selección
  //  entonces la función devuelve el valor false.
  tupla.Leer (ne_provincia, valor_tupla);
  valor_filtro := tabla_f.Leer(naf_provincia);
  if valor_filtro > 0 then
  begin
    case valor_filtro of
      1 :
        if valor_tupla <> 29 then
        begin
          result := false;
          exit;
        end;
      2 :
        if valor_tupla = 29 then
        begin
          result := false;
          exit;
        end;
    end;
  end;

  //  Si la vía de acceso está definida entonces se compara el valor marcado con
  //  el valor de la experiencia. Si no coinciden se devuelve false.
  tupla.Leer (ne_acceso, valor_tupla);
  valor_filtro := tabla_f.Leer(naf_acceso);
  if (valor_filtro > 0) and (valor_filtro <> valor_tupla) then begin
    result := false;
    exit;
  end;

  //  Si está definido el estado de los estudios (finalizado o cursando) entonces
  //  se comprueba el año de finalización de estudios. Si el alumno está cursando
  //  estudios y el año es positivo, o el alumno finalizó y el año es igual a cero
  //  entonces significa que no hay coincidencia y la función devuelve false.
  valor_filtro := tabla_f.Leer(naf_finalizado);
  if valor_filtro > 0 then begin
    tupla.Leer (ne_curso_fin, valor_tupla);
    case valor_filtro of
      1 : if valor_tupla > 0 then begin
              result := false;
              exit;
          end;
      2 : if valor_tupla = 0 then begin
              result := false;
              exit;
          end;
    end;
  end;

  //  Si no está definido un valor para el atributo de clase entonces no cumple
  //  las condiciones para guardarse la experiencia.
  case clase of
    nac_codigo_postal : tupla.Leer (ne_codigo_postal, valor_tupla);
    nac_provincia     : tupla.Leer (ne_provincia, valor_tupla);
    nac_genero        : tupla.Leer (ne_genero, valor_tupla);
    nac_nacionalidad  : tupla.Leer (ne_nacionalidad, valor_tupla);
    nac_acceso        : tupla.Leer (ne_acceso, valor_tupla);
    nac_duracion      : tupla.Leer (ne_duracion_carrera, valor_tupla);
    nac_titulacion    : tupla.Leer (ne_titulacion, valor_tupla);
  end;
  if valor_tupla = 0 then begin
    result := false;
    exit;
  end;

  //  Si la opción de asignatura está definida (asignatura aprobada o estudios
  //  cursados) entonces se pasa a comprobar los valores de la experiencia con los
  //  de la tabla de filtrado.
  valor_filtro := tabla_f.Leer (naf_asignatura);
  if (valor_filtro > 0) then begin
    case valor_filtro of
      //  Si está marcada la opción de asignatura aprobada entonces se busca en la
      //  lista de asignaturas de la experiencia el código de la asignatura
      //  seleccionado en la tabla de filtrado. Por defecto se cambia el valor que
      //  devolverá la función a false. Si se encuentra una coincidencia entonces se
      //  comprueba la calificación de la asignatura, y si tiene un valor mayor que 2
      //  (la asignatura está aprobada) entonces se devuelve true.
      1 : begin
            num_elem_1  := tupla.Lista_Asignatura.Numero_Elementos;
            centro      := tabla_f.Lista_Asignatura.Leer_Centro(0);
            seccion     := tabla_f.Lista_Asignatura.Leer_Seccion(0);
            plan        := tabla_f.Lista_Asignatura.Leer_Plan(0);
            asignatura  := tabla_f.Lista_Asignatura.Leer_Asignatura(0);
            indice      := 0;
            result      := false;

            while (indice < num_elem_1) do begin
              tupla.Lista_Asignatura.Buscar (centro, seccion, plan, asignatura, indice, result);
              if result then begin
                valor_tupla := tupla.Lista_Asignatura.Asignatura(indice).Leer(nea_calificacion);
                if (valor_tupla > 2) then begin
                  result := true;
                  exit;
                end;
              end;
              inc (indice);
            end;
          end;
      //  Si está marcada la opción de estudios cursados entonces se comprueba que hay
      //  elementos en la lista de asignaturas de la tabla de filtrado. Si es así
      //  entonces se comprueba que hay elementos en la lista de asignaturas de la
      //  experiencia. En caso negativo se devuelve el valor false. Si existen
      //  elementos entonces se recorre la lista y se comprueba si éstos están
      //  contenidos en la lista de asignaturas de la tabla de filtrado. En el momento
      //  en que no haya una ocurrencia se devuelve false. Si todos los elementos están
      //  en la lista se devuelve true.
      2 :
        begin
          result     := false;
          num_elem_1 := tabla_f.Lista_Asignatura.Numero_Elementos;
          num_elem_2 := tupla.Lista_Asignatura.Numero_Elementos;

          if (num_elem_1 = 0) or (num_elem_2 = 0) then
            exit;

          dec (num_elem_1);
          for indice := 0 to num_elem_1 do begin
            centro      := tabla_f.Lista_Asignatura.Leer_Centro(indice);
            seccion     := tabla_f.Lista_Asignatura.Leer_Seccion(indice);
            plan        := tabla_f.Lista_Asignatura.Leer_Plan(indice);
            asignatura  := tabla_f.Lista_Asignatura.Leer_Asignatura(indice);
            indice2     := 0;

            if (asignatura > 0) then
              tupla.Lista_Asignatura.Buscar(centro, seccion, plan, asignatura, indice2, result)
            else if (plan > 0) then
              tupla.Lista_Asignatura.Buscar(centro, seccion, plan, indice2, result)
            else if (seccion > 0) then
              tupla.Lista_Asignatura.Buscar(centro, seccion, indice2, result)
            else if (centro > 0) then
              tupla.Lista_Asignatura.Buscar(centro, indice2, result);         

            if not result then
              exit;
          end;
        end;
    end;
  end;
end;





//  La función Leer_Dato recoge como parámetro una experiencia y un tipo de
//  atributo. Lee dicho atributo de la experiencia y devuelve el valor. Se trata
//  de una función polimórfica.

function Leer_Dato (tupla : TExperiencia; atributo : TAtributoSeleccion) : integer;

var
  fecha : TDate;

begin
  result := 0;
  case atributo of
    nas_titulacion            : tupla.Leer (ne_titulacion, result);
    nas_especialidad          : tupla.Leer (ne_especialidad, result);
    nas_edad_inicio           : tupla.Leer (ne_edad_inicial, result);
    nas_edad_final            : tupla.Leer (ne_edad_final, result);
    nas_genero                : tupla.Leer (ne_genero, result);
    nas_codigo_postal         : tupla.Leer (ne_codigo_postal, result);
    nas_provincia             : tupla.Leer (ne_provincia, result);
    nas_nacionalidad          : tupla.Leer (ne_nacionalidad, result);
    nas_expediente            : begin
                                  tupla.Leer (ne_expediente, fecha);
                                  result := YearOf (fecha);
                                end;
    nas_acceso                : tupla.Leer (ne_acceso, result);
    nas_calificacion          : tupla.Leer (ne_calificacion, result);
    nas_curso_fin             : tupla.Leer (ne_curso_fin, result);
    nas_duracion              : tupla.Leer (ne_duracion_carrera, result);
    nas_num_aprobados         : tupla.Leer (ne_num_aprobados, result);
    nas_num_notables          : tupla.Leer (ne_num_notables, result);
    nas_num_sobresalientes    : tupla.Leer (ne_num_sobresalientes, result);
    nas_num_matriculas_honor  : tupla.Leer (ne_num_matriculas_honor, result);
    nas_num_suspensos         : tupla.Leer (ne_num_suspensos, result);
    nas_num_no_presentados    : tupla.Leer (ne_num_no_presentados, result);
    nas_nota_media            : tupla.Leer (ne_nota_media, result);
  end;
end;





//  La función Leer_Dato recoge como parámetro una experiencia, un tipo de
//  atributo y un índice. Lee dicho atributo de la asignatura apuntada por
//  indice de la experiencia y devuelve el valor.

function Leer_Dato (tupla : TExperiencia; atributo : TAtributoSeleccion; indice : integer) : integer;

begin
  result := 0;
  case atributo of
    nas_asig_calificacion        : result := tupla.Lista_Asignatura.Asignatura(indice).Leer (nea_calificacion);
    nas_asig_curso               : result := tupla.Lista_Asignatura.Asignatura(indice).Leer (nea_curso);
    nas_asig_convocatoria        : result := tupla.Lista_Asignatura.Asignatura(indice).Leer (nea_convocatoria);
    nas_asig_num_convocatoria    : result := tupla.Lista_Asignatura.Asignatura(indice).Leer (nea_num_convocatorias);
    nas_asig_num_matriculaciones : result := tupla.Lista_Asignatura.Asignatura(indice).Leer (nea_num_matriculas);
  end;
end;





//  La función Leer_Dato recoge como parámetro una experiencia y un tipo de
//  atributo. Lee dicho atributo de la experiencia y devuelve el valor.

function Leer_Dato (tupla : TExperiencia; atributo : TAtributoClase) : integer;

begin
  result := 0;
  case atributo of
    nac_codigo_postal : tupla.Leer (ne_codigo_postal,    result);
    nac_provincia     : tupla.Leer (ne_provincia,        result);
    nac_genero        : tupla.Leer (ne_genero,           result);
    nac_nacionalidad  : tupla.Leer (ne_nacionalidad,     result);
    nac_acceso        : tupla.Leer (ne_acceso,           result);
    nac_duracion      : tupla.Leer (ne_duracion_carrera, result);
    nac_titulacion    : tupla.Leer (ne_titulacion,       result);
  end;
end;





//  La función Conversion_Dato recoge el tipo de atributo y el valor de dicho
//  atributo y prepara la cadena de texto que se escribirá como campo en el
//  fichero de salida. Se trata de una función polimórfica.

function Conversion_Dato (atributo : TAtributoSeleccion; valor : integer) : string;

begin
  case atributo of
    nas_titulacion               : if valor = 0 then
                                      result := '?'
                                   else
                                      result := IntToStr (valor);
    nas_especialidad             : if valor = 0 then
                                      result := '?'
                                   else
                                      result := IntToStr (valor);
    nas_edad_inicio              : result := IntToStr (valor);
    nas_edad_final               : result := IntToStr (valor);
    nas_genero                   : case valor of
                                      1  : result := 'H';
                                      2  : result := 'M';
                                   else
                                      result := '?';
                                   end;
    nas_codigo_postal            : if valor = 0 then
                                      result := '?'
                                   else
                                      result := IntToStr (valor);
    nas_provincia                : case valor of
                                      1..52 : result := IntToStr (valor);
                                   else
                                      result := '?';
                                   end;
    nas_nacionalidad             : if valor = 0 then
                                      result := '?'
                                   else
                                      result := IntToStr (valor);
    nas_expediente               : case valor of
                                      0 : result := '?';
                                   else
                                      result := IntToStr (valor);
                                   end;
    nas_acceso                   : case valor of
                                      1 : result := '2ciclo';
                                      2 : result := 'adaptacion';
                                      3 : result := 'COU';
                                      4 : result := 'erasmus';
                                      5 : result := 'FP';
                                      6 : result := 'mayor25';
                                      7 : result := 'selectividad';
                                      8 : result := 'titulado';
                                   else
                                      result := '?';
                                   end;
    nas_calificacion             : result := IntToStr (valor);
    nas_curso_fin                : case valor of
                                      0 : result := '?';
                                   else
                                      result := IntToStr (valor);
                                   end;
    nas_duracion                 : case valor of
                                      0 : result := '?';
                                      1 : result := '1anyo';
                                      2..10 : result := IntToStr (valor) + 'anyos';
                                   else
                                      result := 'mas_de_10';
                                   end;
    nas_num_aprobados            : result := IntToStr (valor);
    nas_num_notables             : result := IntToStr (valor);
    nas_num_sobresalientes       : result := IntToStr (valor);
    nas_num_matriculas_honor     : result := IntToStr (valor);
    nas_num_suspensos            : result := IntToStr (valor);
    nas_num_no_presentados       : result := IntToStr (valor);
    nas_nota_media               : result := IntToStr (valor);
    nas_asig_calificacion        : case valor of
                                      0 : result := 'SC';
                                      1 : result := 'NP';
                                      2 : result := 'suspenso';
                                      3 : result := 'equivalencia';
                                      4 : result := 'adaptada';
                                      5 : result := 'convalidada';
                                      6 : result := 'aprobado';
                                      7 : result := 'notable';
                                      8 : result := 'sobresaliente';
                                      9 : result := 'MH';
                                   end;
    nas_asig_curso               : case valor of
                                      0 : result := '?';
                                   else
                                      result := IntToStr (valor);
                                   end;
    nas_asig_convocatoria        : case valor of
                                      1 : result := 'Feb';
                                      2 : result := 'Jun';
                                      3 : result := 'Jun_ext';
                                      4 : result := 'Sep';
                                      5 : result := 'Dic';
                                   else
                                      result := '?';
                                   end;
    nas_asig_num_convocatoria    : case valor of
                                      0 : result := 'ninguna';
                                      1 : result := 'una';
                                      2 : result := 'dos';
                                      3 : result := 'tres';
                                      4 : result := 'cuatro';
                                      5 : result := 'cinco';
                                   else
                                      result := 'mas_de_5';
                                   end;
    nas_asig_num_matriculaciones : case valor of
                                      0 : result := 'ninguna';
                                      1 : result := 'una';
                                      2 : result := 'dos';
                                      3 : result := 'tres';
                                      4 : result := 'cuatro';
                                      5 : result := 'cinco';
                                   else
                                      result := 'mas_de_5';
                                   end;
  end;
end;





//  La función Conversion_Dato recoge el tipo de atributo y el valor de dicho
//  atributo y prepara la cadena de texto que se escribirá como campo en el
//  fichero de salida. Se trata de una función polimórfica.

function Conversion_Dato (atributo : TAtributoClase; valor : integer) : string;

begin
  case atributo of
    nac_codigo_postal : if valor = 0 then
                          result := '?'
                        else
                          result := IntToStr (valor);
    nac_provincia     : case valor of
                          1..52 : result := IntToStr (valor);
                        else
                          result := '?';
                        end;
    nac_genero        : case valor of
                          1  : result := 'H';
                          2  : result := 'M';
                        else
                          result := '?';
                        end;
    nac_nacionalidad  : if valor = 0 then
                          result := '?'
                        else
                          result := IntToStr (valor);
    nac_acceso        : case valor of
                          1 : result := '2ciclo';
                          2 : result := 'adaptacion';
                          3 : result := 'COU';
                          4 : result := 'erasmus';
                          5 : result := 'FP';
                          6 : result := 'mayor25';
                          7 : result := 'selectividad';
                          8 : result := 'titulado';
                        else
                          result := '?';
                        end;
    nac_duracion      : case valor of
                          0 : result := '?';
                          1 : result := '1anyo';
                          2..10 : result := IntToStr (valor) + 'anyos';
                        else
                          result := 'mas_de_10';
                        end;
    nac_titulacion    : if valor = 0 then
                          result := '?'
                        else
                          result := IntToStr (valor);
  end;
end;





//  El procedimiento 'AnyadirValores' lee los valores de la tupla y los añade
//  a la lista de valores nominales. Si el valor es igual a cero (desconocido)
//  no se añade a la lista.

procedure AnyadirValores (tupla : TExperiencia; var lista_valores : TListaValoresAtributos);

var
  valor : integer;

begin
  tupla.Leer (ne_titulacion, valor);
  if valor > 0 then
    lista_valores[nan_titulacion].Append(IntToStr (valor));

  tupla.Leer (ne_especialidad, valor);
  if valor > 0 then
    lista_valores[nan_especialidad].Append(IntToStr (valor));

  tupla.Leer (ne_codigo_postal, valor);
  if valor > 0 then
    lista_valores[nan_codigo_postal].Append(IntToStr (valor));

  tupla.Leer (ne_nacionalidad, valor);
  if valor > 0 then
    lista_valores[nan_nacionalidad].Append(IntToStr (valor));
end;





//  El procedimiento 'DefinicionAtributo' devuelve la lista de valores del
//  atributo en una cadena de texto separados por comas.

procedure DefinicionAtributo (atributo : TAtributoSeleccion; lista_valores : TListaValoresAtributos; var linea : string);

var
  numero, indice : integer;

begin
  linea := '';
  case atributo of
    nas_titulacion    :  begin
                          numero := lista_valores[nan_titulacion].Count - 1;
                          for indice := 0 to numero do
                            linea := linea + lista_valores[nan_titulacion].Strings[indice] + COMA;
                          if numero >= 0 then
                            Delete (linea, Length (linea), 1);
                          linea := linea + '}'
                         end;
    nas_especialidad  :  begin
                          numero := lista_valores[nan_especialidad].Count - 1;
                          for indice := 0 to numero do
                            linea := linea + lista_valores[nan_especialidad].Strings[indice] + COMA;
                          if numero >= 0 then
                            Delete (linea, Length (linea), 1);
                          linea := linea + '}'
                         end;
    nas_codigo_postal :  begin
                          numero := lista_valores[nan_codigo_postal].Count - 1;
                          for indice := 0 to numero do
                            linea := linea + lista_valores[nan_codigo_postal].Strings[indice] + COMA;
                          if numero >= 0 then
                            Delete (linea, Length (linea), 1);
                          linea := linea + '}'
                         end;
    nas_nacionalidad  :  begin
                          numero := lista_valores[nan_nacionalidad].Count - 1;
                          for indice := 0 to numero do
                            linea := linea + lista_valores[nan_nacionalidad].Strings[indice] + COMA;
                          if numero >= 0 then
                            Delete (linea, Length (linea), 1);
                          linea := linea + '}'
                         end;
  end;
end;


end.

