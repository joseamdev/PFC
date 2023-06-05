//  El m�dulo 'Compactar.pas' re�ne los procedimientos encargados de tratar la
//  informaci�n de los alumnos y las asignaturas, de modo que �stos queden
// resumidos en una tupla y libres de incongruencias o errores.

unit Compactar;

interface

	uses

		Experiencias, Tablas, Controls, DateUtils, SysUtils, Classes;


	type

		TCalificacion = (no_presentado, suspenso, aprobado, notable, sobresaliente, matricula);


	function  Contar_Calificaciones             (asig : TListaExperienciaAsignatura; calif : TCalificacion) : integer;
	procedure Completar_Atributos               (exp : TExperiencia);
	function  Numero_Titulaciones               (lista_exp : TListaExperiencia) : integer;
	procedure Separar_Asignaturas_Titulaciones  (exp_origen, exp_destino : TExperiencia; centro, seccion : integer);
	procedure Separar_Titulaciones              (var lista_origen, lista_tit : TListaExperiencia);
  procedure Eliminar_Asignaturas_Repetidas    (lista_asig : TListaExperienciaAsignatura);
	procedure Aplicar_Adaptaciones              (lista_exp : TListaExperiencia; tabla_adaptacion : TTablaAdaptacion);
	procedure Fundir_Asignaturas                (lista_asig : TListaExperienciaAsignatura; indice_1, indice_2 : integer);
	procedure Fundir_Asignaturas_Adaptadas      (lista_asig : TListaExperienciaAsignatura; indice_1, indice_2 : integer);
  procedure Eliminar_Incoherencias_Atributo   (lista_exp : TListaExperiencia; atributo : TExperienciaAtributo);
  procedure Eliminar_Incoherencias            (lista_exp : TListaExperiencia);
	procedure Principal                         (tabla_adaptacion : TTablaAdaptacion; var lista_exp : TListaExperiencia);





implementation



//  El procedimiento 'Principal' se encarga de separar de una lista de
//  experiencias aquellas que pertenezcan a diferentes titulaciones y tratar
//  s�lo aquellas experiencias que contengan la informaci�n de un alumno y una
//  titulaci�n. Se rellenan atributos, se fusiona la informaci�n y se eliminan
//  incoherencias.

procedure Principal (tabla_adaptacion : TTablaAdaptacion; var lista_exp : TListaExperiencia);

var
  numero_exp, numero_tit, indice_exp, indice_tit : integer;
	lista_compactada, lista_tit : TListaExperiencia;

begin
  //  En 'numero_exp' se guarda el n�mero de elementos que hay en la lista de
  //  experiencias lista_exp. Seg�n si hay un solo elemento o si hay m�s de uno
  //  se realizan diferentes acciones.
  numero_exp := lista_exp.Numero_Elementos;

  //  si no hay experiencias en la lista se sale del procedimiento.
  
  if (numero_exp = 0) then
    exit

  //  Si �nicamente hay una l�nea de datos por alumno entonces los �nico que se
  //  hace es completar los atributos y aplicar los cuadros de adaptaci�n.

  else if (numero_exp = 1) then begin

    Aplicar_Adaptaciones (lista_exp, tabla_adaptacion);
    Completar_Atributos (lista_exp.Experiencia (0));

  //  Si hay m�s de un elemento significa que hay m�s de una entrada con el
  //  mismo c�digo de alumno. Entonces se pueden dar dos casos: que todas las
  //  experiencias pertenezcan a la misma titulaci�n o que haya m�s de una
  //  titulaci�n para el mismo alumno.

  end else if (numero_exp > 1) then begin

    //  En el primer caso, se recorre la lista de experiencias con un bucle. Se
    //  completan los atributos cuyos valores est�n en funci�n de los atributos
    //  conocidos. Se extrae el n�mero de  asignaturas y se comprueba que no sea
    //  nulo. Si hay asignaturas dentro de la experiencia se aplican las
    //  adaptaciones necesarias para fusionar de diferentes planes de estudio.
    //  Una vez se termina de recorrer la lista de experiencias se eliminan las
    //  incoherencias. Por �ltimo se eliminan todas las experiencias de la lista
    //  excepto la primera, donde han quedado fusionados todos los datos.

  	numero_tit := Numero_Titulaciones (lista_exp);

    if numero_tit = 1 then begin

      Aplicar_Adaptaciones   (lista_exp, tabla_adaptacion);
   	Eliminar_Incoherencias (lista_exp);
      Completar_Atributos    (lista_exp.Experiencia (0));

      for indice_exp := 1 to (numero_exp - 1) do
        lista_exp.Eliminar(1);

    //  En el segundo caso se crea la lista auxiliar lista_compactada donde se
    //  almacenan los alumnos con igual c�digo y diferente titulaci�n con la
    //  informaci�n ya compactada. Se crea un bucle para recorrer cada una de
    //  las titulaciones del alumno y tratarlas por separado. Se crea la lista
    //  auxiliar 'lista_tit' donde se trasladan las experiencias de la
    //  titulaci�n a tratar en esa iteraci�n. Se guarda en 'numero_exp' el
    //  n�mero de l�neas repetidas del alumno de una titulaci�n concreta.
    //  Posteriormente se realizan las mismas operaciones que se daban en el
    //  primer caso. Se empieza con otro bucle interno que recorre la lista de
    //  experiencias de una misma titulaci�n. Se completan los atributos. Se
    //  comprueba que el n�mero de asignaturas sea mayor que cero; si se cumple
    //  la condici�n se aplican los cuadros de adaptaci�n a todas las
    //  asignaturas. Al finalizar el bucle se eliminan las incoherencias. Antes
    //  de iniciar la siguiente iteraci�n del bucle externo se inserta el
    //  resultado guardado en 'lista_tit' en la lista 'lista_compactada', y se
    //  libera 'lista_tit' de memoria.

    end else begin
      lista_compactada := TListaExperiencia.Create;
      for indice_tit := 1 to numero_tit do begin

        lista_tit := TListaExperiencia.Create;

        Separar_Titulaciones    (lista_exp, lista_tit);
        Aplicar_Adaptaciones    (lista_tit, tabla_adaptacion);
   	  Eliminar_Incoherencias  (lista_tit);
        Completar_Atributos     (lista_tit.Experiencia (0));

   	  lista_compactada.Insertar (lista_tit.Experiencia(0));
   	  lista_tit.Free;
   	end;
      lista_exp.Free;
   	lista_exp := lista_compactada;
    end;
  end;

end;





//  La funci�n 'Contar_Calificaciones' cuenta el n�mero de calificaciones con
//  valor 'calif' dentro de la lista de experiencias 'asig'.
function Contar_Calificaciones (asig : TListaExperienciaAsignatura; calif : TCalificacion) : integer;
var
	valor, numero, i, contador : integer;

begin
	valor := 0;

  //  Seg�n sea el valor de 'calif', la variable valor toma el valor num�rico
  //  correspondiente. Se guarda el n�mero de elementos de 'asig' en la variable
  //  'numero' y se pone la variable 'contador' a cero.
	case calif of
		no_presentado : valor := 1;
		suspenso      : valor := 2;
		aprobado      : valor := 6;
		notable       : valor := 7;
		sobresaliente : valor := 8;
		matricula     : valor := 9;
	end;

	numero   := asig.Numero_Elementos;
	contador := 0;

  //  Entonces se empieza a recorrer la 'lista asig' y se comprueba si el
  //  valor de la calificaci�n de la asignatura coincide con el de la variable
  //  valor. Si se cumple la condici�n se incrementa la variable 'contador'. Una
  //  vez finalizado el bucle en 'contador' estar� almacenado el n�mero de
  //  asignaturas con la calificaci�n seleccionada.

	for i := 0 to numero - 1 do
		if asig.Asignatura(i).Leer(nea_calificacion) = valor then
			inc (contador);

	result := contador;
end;





//  El procedimiento 'Completar_Atributos' se encarga de rellenar los atributos
//  cuyos valores son dependientes de otros atributos de la experiencia.
procedure Completar_Atributos (exp : TExperiencia);

var
	fecha1, fecha2 : TDate;
	anyo, edad, duracion, numero, numero_a, numero_n, numero_s, numero_m : integer;

begin
  //  En primer lugar se leen las fechas de nacimiento y la de matriculaci�n de
  //  la experiencia 'exp' pasada como par�metro. Si los valores no son nulos se
  //  restan para calcular la edad del alumno cuando inicio sus estudios y se
  //  guarda en la variable 'edad'; en caso contrario se guarda el valor cero.
  //  Despu�s se escribe la edad del alumno en la experiencia 'exp'.
	exp.Leer (ne_nacimiento, fecha1);	exp.Leer (ne_expediente, fecha2);

	if (fecha1 > 0) and (fecha2 > 0) then
  	edad := YearsBetween (fecha2, fecha1)
  else
		edad := 0;

	exp.Escribir (ne_edad_inicial, edad);

  //  Para calcular la duraci�n de la carrera se lee el curso de finalizaci�n.
  //  Si tanto el a�o de inicio como el de fin de estudios no son nulos se
  //  calcula la diferencia entre ambos y se almacena el resultado en la
  //  variable duracion; si alguno de los valores es nulo entonces duracion ser�
  //  igual a cero. En el atributo correspondiente de la experiencia 'exp' se
  //  actualiza el valor.
	exp.Leer (ne_curso_fin, anyo);
	if (anyo > 0) and (YearOf(fecha2) > 0) then
		duracion := anyo - YearOf (fecha2)
	else
		duracion := 0;

	exp.Escribir (ne_duracion_carrera, duracion);

  //  La edad de finalizaci�n de estudios se calcula a partir de la edad de
  //  inicio y la duraci�n de los estudios. Si ambos valores no son nulos
  //  entonces se suman y se guarda el valor en edad; en caso contrario se
  //  asigna a la edad el valor cero. El resultado se escribe en el par�metro
  //  correspondiente de la variable 'exp'.
	if (duracion > 0) and (edad > 0) then
		edad := edad + duracion
	else
		edad := 0;

	exp.Escribir (ne_edad_final, edad);

  //  El n�mero de asignaturas no presentadas se calcula llamando al
  //  procedimiento 'Contar_Calificaciones'. El resultado se almacena en la
  //  variable 'numero'. El valor se guarda luego en el atributo correspondiente
  //  de la experiencia 'exp'.
  numero := Contar_Calificaciones (exp.Lista_Asignatura, no_presentado);
	exp.Escribir (ne_num_no_presentados, numero);

  //  El n�mero de asignaturas suspensas se calcula llamando al procedimiento
  //  'Contar_Calificaciones'. El resultado se almacena en la variable 'numero'.
  //  El valor se guarda luego en el atributo correspondiente de la experiencia 'exp'.
	numero := Contar_Calificaciones (exp.Lista_Asignatura, suspenso);
	exp.Escribir (ne_num_suspensos, numero);

  //  El n�mero de asignaturas con aprobado se calcula llamando al procedimiento
  //  'Contar_Calificaciones'. El resultado se almacena en la variable 'numero'.
  //  El valor se guarda luego en el atributo correspondiente de la experiencia 'exp'.
	numero_a := Contar_Calificaciones (exp.Lista_Asignatura, aprobado);
	exp.Escribir (ne_num_aprobados, numero);

  //  El n�mero de asignaturas con notable se calcula llamando al procedimiento
  //  'Contar_Calificaciones'. El resultado se almacena en la variable 'numero'.
  //  El valor se guarda luego en el atributo correspondiente de la experiencia 'exp'.
	numero_n := Contar_Calificaciones (exp.Lista_Asignatura, notable);
	exp.Escribir (ne_num_notables, numero);

  //  El n�mero de asignaturas con sobresaliente se calcula llamando al
  //  procedimiento 'Contar_Calificaciones'. El resultado se almacena en la
  // variable 'numero'. El valor se guarda luego en el atributo correspondiente
  // de la experiencia 'exp'.
	numero_s := Contar_Calificaciones (exp.Lista_Asignatura, sobresaliente);
	exp.Escribir (ne_num_sobresalientes, numero);

  //  El n�mero de asignaturas con matr�cula de honor se calcula llamando al
  //  procedimiento 'Contar_Calificaciones'. El resultado se almacena en la
  //  variable 'numero'. El valor se guarda luego en el atributo correspondiente
  //  de la experiencia 'exp'.
  numero_m := Contar_Calificaciones (exp.Lista_Asignatura, matricula);
	exp.Escribir (ne_num_matriculas_honor, numero);

  //  La nota media se calcula multiplicando cien por el n�mero de matr�culas de
  //  honor por cuatro m�s el n�mero de sobresalientes por tres m�s el n�mero de
  //  notables por dos m�s el n�mero de aprobados. El resultado se guarda en la
  //  variable 'numero'. Si es distinto de cero se calcula la nota media
  //  mediante la f�rmula siguiente: se divide el valor de la variable 'numero'
  //  por la suma del n�mero asignaturas aprobadas (aprobados, notables,
  //  sobresalientes y matr�culas de honor). El resultado se almacena en la
  //  variable 'numero' y se guarda en el atributo correspondiente de la
  //  experiencia 'exp'.
  numero := ((numero_m * 4) + (numero_s * 3) + (numero_n * 2) + numero_a) * 100;
  if numero <> 0 then
    numero := numero div (numero_m + numero_s + numero_n + numero_a);
	exp.Escribir (ne_nota_media, numero);

  //  El c�digo de la titulaci�n es la suma del c�digo del centro y el de secci�n.
  //  Para facilitar ciertas comprobaciones se almacenan en �ste par�metro los
  //  dos como un �nico c�digo. Para ello se multiplica el c�digo del centro por
  //  cien (secci�n solo tiene dos d�gitos) y se le suma el c�digo de la secci�n.
  //  El resultado se almacena en la experiencia 'exp'.

	exp.Leer (ne_centro, numero_n);
	exp.Leer (ne_seccion, numero_s);
	numero := (numero_n * 100) + numero_s;
	exp.Escribir (ne_titulacion, numero);
end;





//  La funci�n 'Numero_Titulaciones' calcula y devuelve el n�mero de
//  titulaciones distintas dentro de una lista de experiencias.
function Numero_Titulaciones (lista_exp : TListaExperiencia) : integer;
var
	numero_exp, indice_exp, centro, seccion : integer;
  lista_sin_rep : TStringList;

begin
  //  En 'numero_elementos' se guarda el n�mero de experiencias contenidas en
  //  la lista 'lista_exp'. Se crea la lista 'lista_sin_rep' para guardar los
  //  c�digos de las titulaciones de la lista de experiencias que sean
  //  diferentes. La lista ser� ordenada e ignorar� duplicados; de esta
  //  manera se obtendr� una lista sin elementos repetidos y ordenados.
	numero_exp               := lista_exp.Numero_Elementos - 1;  lista_sin_rep            := TStringList.Create;
  lista_sin_rep.Sorted     := true;
  lista_sin_rep.Duplicates := dupIgnore;

  //  Mediante un bucle se recorre la lista de experiencias 'lista_exp'. En
  //  cada iteraci�n se lee el c�digo del centro y de la secci�n y se guarda
  //  en la lista 'lista_sin_rep'. Al finalizar el proceso, en 'lista_sin_rep'
  //  se tiene una lista de elementos �nicos. El n�mero de elementos de esta
  //  lista indica el n�mero de titulaciones diferentes existentes en la
  //  lista de experiencias. Se libera de memoria la lista lista_sin_rep y
  //  se devuelve el n�mero de elementos de la misma.

	for indice_exp := 0 to numero_exp do begin
		lista_exp.Experiencia (indice_exp).Leer (ne_centro, centro);
		lista_exp.Experiencia (indice_exp).Leer (ne_seccion, seccion);
    lista_sin_rep.Append  (IntToStr ((centro * 100) + seccion));
	end;
	result := lista_sin_rep.Count;
  lista_sin_rep.Free;
end;





//  El procedimiento 'Separar_Asignaturas_Titulaciones' divide las
//  asignaturas que no coinciden con la titulaci�n origen. �sta se
//  corresponde con los c�digos pasados como par�metros: centro y seccion.
//  Todas las que difieran pasan a la experiencia destino.
procedure Separar_Asignaturas_Titulaciones (exp_origen, exp_destino : TExperiencia; centro, seccion : integer);
var
	indice_asig, numero_asig, centro_origen, seccion_origen : integer;

begin
  //  En la variable 'numero_asig' se guarda el n�mero de asignaturas de la
  //  experiencia origen exp_origen. La variable 'indice_asig' se pone a cero, y la
  //  variable de control coincidencia se pone a true.
	numero_asig  := exp_origen.Lista_Asignatura.Numero_Elementos;
	indice_asig  := 0;

  //  Se inicia un bucle que recorrer� la lista de asignaturas de la
  //  experiencia origen. Iterar� mientras no se llegue al final de la lista.
  //  En 'centro_origen' y 'seccion_origen' se escriben los c�digos del
  //  centro y secci�n de la asignatura apuntada por 'indice_asig'. Se comparan
  //  los c�digos y si coinciden se inserta en la experiencia 'exp_destino' y se
  //  elimina de 'exp_origen'. Por �ltimo se incrementa la variable 'indice'.
  while (indice_asig < numero_asig) do begin    centro_origen  := exp_origen.Lista_Asignatura.Asignatura(indice_asig).Leer(nea_centro);
    seccion_origen := exp_origen.Lista_Asignatura.Asignatura(indice_asig).Leer(nea_seccion);
    if (centro_origen <> centro) or (seccion_origen <> seccion) then begin
  		exp_destino.Lista_Asignatura.Insertar (exp_origen.Lista_Asignatura.Asignatura (indice_asig));
  		exp_origen.Lista_Asignatura.Eliminar (indice_asig);
  		dec (numero_asig);
    end else
      inc (indice_asig);
  end;

end;




//  El procedimiento 'Separar_Titulaciones' divide las titulaciones en dos
//  listas. La lista 'lista_tit' se queda con la serie experiencias de la
//  misma titulaci�n que se encuentren al comienzo de lista. En 'lista' se
//  almacena el resto. Si est� vac�a no se realiza la divisi�n de
//  asignaturas.
procedure Separar_Titulaciones (var lista_origen, lista_tit : TListaExperiencia);

var
	numero_exp, indice_exp, centro_origen, seccion_origen, centro_tit, seccion_tit : integer;

begin
  //  En la variable 'numero_exp' se guarda el n�mero de elementos de la lista de
  //  experiencias 'lista_origen'. Si 'numero_exp' es igual a cero, significa que no hay
  //  experiencias en la lista, por lo que el procedimiento no realiza
  //  ninguna operaci�n. Si numero es mayor que cero entonces comienza el
  //  proceso de divisi�n.
	numero_exp := lista_origen.Numero_Elementos;  if numero_exp = 0 then
    exit;

  //  La primera experiencia se traslada sin comprobar, ya que servir� como
  //  referencia. Se inserta la primera experiencia en 'lista_tit'. Se elimina
  //  de la lista origen 'lista_origen'. Se leen los c�digos del centro y secci�n de
  //  la experiencia y se guardan en las variables 'centro_origen' y 'seccion_origen'. La
  //  variable 'indice_exp' se pone a cero. La variable 'numero_exp' se decrementa en
  //  uno; se hab�a eliminado previamente un elemento de la lista de
  //  experiencias de origen.
	lista_tit.Insertar (lista_origen.Experiencia (0));	lista_origen.Eliminar (0);
	lista_tit.Experiencia (0).Leer (ne_centro, centro_tit);
	lista_tit.Experiencia (0).Leer (ne_seccion, seccion_tit);
  indice_exp := 0;
	dec (numero_exp);

  //  Mientras haya elementos en la lista de experiencias 'lista_origen' se va
  //  iterando. Se leen los c�digos del centro y la secci�n del elemento
  //  apuntado por indice. Si coinciden con los c�digos de la primera
  //  experiencia de la lista, almacenada en 'lista_tit', se inserta la
  //  experiencia en dicha lista y se elimina de la lista origen lista. Una
  //  vez eliminada la variable numero se decrementa en uno. Si no coinciden
  //  entonces se incrementa la variable indice en uno.
  //  Si en la lista de experiencias lista todav�a quedan elementos entonces  //  se trasladan las asignaturas en que coincidan el centro y la secci�n,
  //  de la lista 'lista_origen' a la lista 'lista_tit'.

	while (indice_exp < numero_exp) do begin
    lista_origen.Experiencia(indice_exp).Leer (ne_centro, centro_origen);
    lista_origen.Experiencia(indice_exp).Leer (ne_seccion, seccion_origen);
  	if (centro_origen = centro_tit) and (seccion_origen = seccion_tit) then begin
			lista_tit.Insertar (lista_origen.Experiencia (indice_exp));
			lista_origen.Eliminar (indice_exp);
			dec (numero_exp);
		end else
      inc (indice_exp);
	end;
  if lista_origen.Numero_Elementos > 0 then
    Separar_Asignaturas_Titulaciones (lista_tit.Experiencia (0), lista_origen.Experiencia (0), centro_tit, seccion_tit);
end;





//  El procedimiento 'Eliminar_Asignaturas_Repetidas' recorre la lista 'lista_asig'
//  buscando elementos repetidos. Cuando encuentra otra asignatura de igual c�digo
//  compara los datos de ambas y elimina el elemento innecesario.

procedure Eliminar_Asignaturas_Repetidas (lista_asig : TListaExperienciaAsignatura);

var
  numero_asig, indice_asig, indice_repe : integer;
  centro, seccion, plan, asignatura : integer;
  coincidencia : boolean;

begin
	indice_asig := 0;
  numero_asig := lista_asig.Numero_Elementos;

  if numero_asig = 0 then
    exit;

  repeat
    centro      := lista_asig.Asignatura(indice_asig).Leer (nea_centro);
    seccion     := lista_asig.Asignatura(indice_asig).Leer (nea_seccion);
    plan        := lista_asig.Asignatura(indice_asig).Leer (nea_plan);
    asignatura  := lista_asig.Asignatura(indice_asig).Leer (nea_asignatura);
    indice_repe := indice_asig + 1;

    lista_asig.Buscar (centro, seccion, plan, asignatura, indice_repe, coincidencia);
   	if coincidencia then begin
   		Fundir_Asignaturas (lista_asig, indice_asig, indice_repe);
   		dec (numero_asig);
   	end else
  	  inc (indice_asig);

  until (indice_asig >= numero_asig);
end;





//  El procedimiento 'Aplicar_Adaptaciones' funde las asignaturas de una
//  experiencia, les aplica el cuadro de adaptaciones correspondientes y elimina
//  las entradas repetidas.

procedure Aplicar_Adaptaciones (lista_exp : TListaExperiencia; tabla_adaptacion : TTablaAdaptacion);

var
	numero_exp, numero_asig, indice_exp, indice_asig, indice_adaptacion : integer;
  centro, seccion, plan, asignatura, aux : integer;
  lista_asig : TListaExperienciaAsignatura;
	coincidencia : boolean;

begin
  //  En 'numero_exp' se guarda el n�mero de elementos de la lista de experiencias.
  numero_exp := lista_exp.Numero_Elementos;

  //  Se recorren todas las experiencias de la lista. Se extrae la lista de
  //  asignaturas.
  for indice_exp := 0 to (numero_exp - 1) do begin
    lista_asig  := lista_exp.Experiencia(indice_exp).Lista_Asignatura;
    numero_asig := lista_asig.Numero_Elementos;
    //  Si la lista de asignaturas no est� vac�a:
    if numero_asig > 0 then begin

      Eliminar_Asignaturas_Repetidas (lista_asig);

      // Se aplican las adaptaciones de las asignaturas

      numero_asig := lista_asig.Numero_Elementos;
    	for indice_asig := 0 to (numero_asig - 1) do begin
        //  En primer lugar se leen los c�digos del centro, secci�n, plan de estudios y asignatura.
    		centro            := lista_asig.Asignatura(indice_asig).Leer (nea_centro);
    		seccion           := lista_asig.Asignatura(indice_asig).Leer (nea_seccion);
    		plan              := lista_asig.Asignatura(indice_asig).Leer (nea_plan);
    		asignatura        := lista_asig.Asignatura(indice_asig).Leer (nea_asignatura);

        indice_adaptacion := 0;

        //  Mientras haya coincidencias en el c�digo de la asignatura se van
        //  aplicando las adaptaciones.
        repeat
    			tabla_adaptacion.Buscar (centro, seccion, plan, asignatura, indice_adaptacion, coincidencia);

    			if coincidencia then begin
    				aux := tabla_adaptacion.Leer_Centro2 (indice_adaptacion);
    				lista_asig.Asignatura(indice_asig).Escribir (nea_centro, aux);

    				aux := tabla_adaptacion.Leer_Seccion2 (indice_adaptacion);
    				lista_asig.Asignatura(indice_asig).Escribir (nea_seccion, aux);

    				aux := tabla_adaptacion.Leer_Plan2 (indice_adaptacion);
    				lista_asig.Asignatura(indice_asig).Escribir (nea_plan, aux);

    				aux := tabla_adaptacion.Leer_Asignatura2 (indice_adaptacion);
    				lista_asig.Asignatura(indice_asig).Escribir (nea_asignatura, aux);
    			end;

    			inc (indice_adaptacion);
    		until not coincidencia;
    	end;

      // Se eliminan las entradas repetidas tras las adaptaciones

      Eliminar_Asignaturas_Repetidas (lista_asig);
    end;
  end;

end;





//  El procedimiento 'Fundir_Asignaturas' compara los valores de las dos
//  asignaturas de la lista apuntadas por los �ndices pasados como par�metros,
//  guardando el resultado en la primera de ellas.

procedure Fundir_Asignaturas (lista_asig : TListaExperienciaAsignatura; indice_1, indice_2 : integer);

var
	calificacion_1, calificacion_2, convocatoria_1, convocatoria_2,
	curso_1, curso_2, num_convocatoria_1, num_convocatoria_2, num_matricula_1, num_matricula_2 : integer;

begin
  //  En primer lugar se leen las calificaciones, cursos, convocatorias, n�mero
  //  de convocatorias y n�mero de matr�culas de las dos asignaturas pasadas
  //  como par�metros. Se guardan los valores en variables locales para
  //  posteriores comprobaciones.
  //  El resultado de fundir las dos asignatura se guardar� en la primera de ellas.
	calificacion_1     := lista_asig.Asignatura (indice_1).Leer (nea_calificacion);
	calificacion_2     := lista_asig.Asignatura (indice_2).Leer (nea_calificacion);
	curso_1            := lista_asig.Asignatura (indice_1).Leer (nea_curso);
	curso_2            := lista_asig.Asignatura (indice_2).Leer (nea_curso);
	convocatoria_1     := lista_asig.Asignatura (indice_1).Leer (nea_convocatoria);
	convocatoria_2     := lista_asig.Asignatura (indice_2).Leer (nea_convocatoria);
	num_convocatoria_1 := lista_asig.Asignatura (indice_1).Leer (nea_num_convocatorias);
	num_convocatoria_2 := lista_asig.Asignatura (indice_2).Leer (nea_num_convocatorias);
	num_matricula_1    := lista_asig.Asignatura (indice_1).Leer (nea_num_matriculas);
	num_matricula_2    := lista_asig.Asignatura (indice_2).Leer (nea_num_matriculas);

  //  Se comparan los valores de las dos asignaturas.
	if ((calificacion_1 <> calificacion_2) and (curso_1 < curso_2)) or
		 ((calificacion_1 <  calificacion_2) and (curso_1 = curso_2)) then
		   calificacion_1 := calificacion_2;

	if ((convocatoria_1 <> convocatoria_2) and (curso_1 < curso_2)) or
		 ((convocatoria_1 <  convocatoria_2) and (curso_1 = curso_2)) then
       convocatoria_1 := convocatoria_2;

	if curso_1 < curso_2 then
		curso_1 := curso_2;

	if num_convocatoria_2 > num_convocatoria_1 then
		num_convocatoria_1 := num_convocatoria_2;

	if num_matricula_2 > num_matricula_1 then
		num_matricula_1 := num_matricula_2;

  //  Una vez terminadas las comprobaciones se guardan los valores de calificaci�n,
  //  curso, convocatoria, n�mero de convocatorias y n�mero de matr�culas en la
  //  asignatura apuntada por el primer �ndice y se elimina la asignatura
  //  apuntada por el segundo.

	lista_asig.Asignatura (indice_1).Escribir (nea_calificacion, calificacion_1);
	lista_asig.Asignatura (indice_1).Escribir (nea_curso, curso_1);
	lista_asig.Asignatura (indice_1).Escribir (nea_convocatoria, convocatoria_1);
	lista_asig.Asignatura (indice_1).Escribir (nea_num_convocatorias, num_convocatoria_1);
	lista_asig.Asignatura (indice_1).Escribir (nea_num_matriculas, num_matricula_1);
	lista_asig.Eliminar (indice_2);
end;





//  El procedimiento 'Fundir_Asignaturas_Adaptadas' compara los valores de las
//  dos asignaturas de la lista apuntadas por los �ndices pasados como
//  par�metros, guardando el resultado en la primera de ellas.

procedure Fundir_Asignaturas_Adaptadas (lista_asig : TListaExperienciaAsignatura; indice_1, indice_2 : integer);

var
	calificacion_1, calificacion_2, convocatoria_1, convocatoria_2,
	curso_1, curso_2, num_convocatoria_1, num_convocatoria_2, num_matricula_1, num_matricula_2 : integer;

begin
  //  Primero se leen los valores de calificaci�n, curso, convocatoria, n�mero
  //  de convocatorias y n�mero de matr�culas de las asignaturas de la lista
  //  apuntadas por los �ndices y se guardan en variables locales.
	calificacion_1     := lista_asig.Asignatura (indice_1).Leer (nea_calificacion);
	calificacion_2     := lista_asig.Asignatura (indice_2).Leer (nea_calificacion);
	curso_1            := lista_asig.Asignatura (indice_1).Leer (nea_curso);
	curso_2            := lista_asig.Asignatura (indice_2).Leer (nea_curso);
	convocatoria_1     := lista_asig.Asignatura (indice_1).Leer (nea_convocatoria);
	convocatoria_2     := lista_asig.Asignatura (indice_2).Leer (nea_convocatoria);
	num_convocatoria_1 := lista_asig.Asignatura (indice_1).Leer (nea_num_convocatorias);
	num_convocatoria_2 := lista_asig.Asignatura (indice_2).Leer (nea_num_convocatorias);
	num_matricula_1    := lista_asig.Asignatura (indice_1).Leer (nea_num_matriculas);
	num_matricula_2    := lista_asig.Asignatura (indice_2).Leer (nea_num_matriculas);

  //  Se inician las comprobaciones de las dos asignaturas.
	if calificacion_1 < calificacion_2 then calificacion_1 := calificacion_2;

	if curso_1        < curso_2        then curso_1        := curso_2;

	if convocatoria_1 < convocatoria_2 then convocatoria_1 := convocatoria_2;

	num_convocatoria_1 := num_convocatoria_1 + num_convocatoria_2;
	num_matricula_1    := num_matricula_1    + num_matricula_2;

  //  Se sobrescriben el resultado de las comprobaciones en la asignatura
  //  apuntada por el primer �ndice y se elimina la asignatura apuntada por el
  //  segundo.

	lista_asig.Asignatura (indice_1).Escribir (nea_calificacion, calificacion_1);
	lista_asig.Asignatura (indice_1).Escribir (nea_curso, curso_1);
	lista_asig.Asignatura (indice_1).Escribir (nea_convocatoria, convocatoria_1);
	lista_asig.Asignatura (indice_1).Escribir (nea_num_convocatorias, num_convocatoria_1);
	lista_asig.Asignatura (indice_1).Escribir (nea_num_matriculas, num_matricula_1);
	lista_asig.Eliminar (indice_2);
end;





//  El procedimiento 'Eliminar_Incoherencias_Atributo' comprueba que la primera
//  entrada de la lista de experiencias contenga valores v�lidos para el
//  atributo, si no es as� se busca en las siguientes experiencias de la lista
//  un valor v�lido. La primera ocurrencia sustituir� el valor incorrecto de la
//  primera experiencia.

procedure Eliminar_Incoherencias_Atributo (lista_exp : TListaExperiencia; atributo : TExperienciaAtributo);

var
  valor_entero, indice_exp, numero_exp : integer;
  valor_fecha : TDate;
  valor_texto : string;
  coincidencia : boolean;

begin
  case atributo of
    ne_centro..ne_titulacion :
              begin
                numero_exp := lista_exp.Numero_Elementos;
               	lista_exp.Experiencia(0).Leer (atributo, valor_entero);

               	if valor_entero = 0 then begin
               		indice_exp := 1;
                  coincidencia := false;
                  while (indice_exp < numero_exp) and (not coincidencia) do begin
                  	lista_exp.Experiencia (indice_exp).Leer (atributo, valor_entero);
                  	coincidencia := (valor_entero <> 0);
                    inc (indice_exp);
                  end;
                  if coincidencia then
                    lista_exp.Experiencia (0).Escribir (atributo, valor_entero);
                end;
              end;
    ne_localidad :
              begin
                numero_exp := lista_exp.Numero_Elementos;
               	lista_exp.Experiencia(0).Leer (atributo, valor_texto);

               	if valor_texto = '' then begin
               		indice_exp := 1;
                  coincidencia := false;
                  while (indice_exp < numero_exp) and (not coincidencia) do begin
                  	lista_exp.Experiencia (indice_exp).Leer (atributo, valor_texto);
                  	coincidencia := not (valor_texto = '');
                    inc (indice_exp);
                  end;
                  if coincidencia then
                    lista_exp.Experiencia (0).Escribir (atributo, valor_texto);
                end;
              end;
    ne_nacimiento..ne_expediente :
              begin
                numero_exp := lista_exp.Numero_Elementos;
               	lista_exp.Experiencia(0).Leer (atributo, valor_fecha);

               	if valor_fecha = 0 then begin
               		indice_exp := 1;
                  coincidencia := false;
                  while (indice_exp < numero_exp) and (not coincidencia) do begin
                  	lista_exp.Experiencia (indice_exp).Leer (atributo, valor_fecha);
                  	coincidencia := (valor_fecha <> 0);
                    inc (indice_exp);
                  end;
                  if coincidencia then
                    lista_exp.Experiencia (0).Escribir (atributo, valor_fecha);
                end;
              end;

  end;
end;





//  El procedimiento 'Eliminar_Incoherencias' comprueba que la primera entrada
//  de la lista de experiencias contenga valores v�lidos en todos sus atributos.
//  Se realiza el proceso mediante llamadas a 'Eliminar_Incoherencias_Atributo'.

procedure Eliminar_Incoherencias (lista_exp : TListaExperiencia);

begin
  //  S�lo se modifican los atributos del alumno.
  //  Los campos: c�digo, centro, secci�n, plan, especialidad no var�an.
  //  En los campos: nacimiento, c.p., localidad, provincia, g�nero, nacionalidad,
  //  expediente, acceso, calificaci�n y a�o finalizaci�n se elige la primera
  //  ocurrencia, salvo que sea nula, en ese caso se elige la segunda, y as�
  //  sucesivamente.

  Eliminar_Incoherencias_Atributo (lista_exp, ne_codigo_postal);
  Eliminar_Incoherencias_Atributo (lista_exp, ne_provincia);
  Eliminar_Incoherencias_Atributo (lista_exp, ne_genero);
  Eliminar_Incoherencias_Atributo (lista_exp, ne_nacionalidad);
  Eliminar_Incoherencias_Atributo (lista_exp, ne_acceso);
  Eliminar_Incoherencias_Atributo (lista_exp, ne_calificacion);
  Eliminar_Incoherencias_Atributo (lista_exp, ne_curso_fin);
  Eliminar_Incoherencias_Atributo (lista_exp, ne_localidad);
  Eliminar_Incoherencias_Atributo (lista_exp, ne_nacimiento);
  Eliminar_Incoherencias_Atributo (lista_exp, ne_expediente);

end;


end.
