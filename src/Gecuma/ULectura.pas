unit ULectura;





interface




uses
   SysUtils, StrUtils, Dialogs,
   UClaseArbol, UClaseAtributo;



const

   NOMBRE_ATRIBUTO : array [TAtributo] of string = (
      'TITULACION',
      'EDAD_INICIO',
      'GENERO',
      'CODIGO_POSTAL',
      'PROVINCIA',
      'NACIONALIDAD',
      'EXPEDIENTE',
      'ACCESO',
      'CALIFICACION',
      'DURACION');

   TEXTO_COMPARADOR : array [TComparador] of string = (
      '=',
      '>',
      '<=');

   NUM_VALORES = 21;

   TEXTO_VALOR : array [1..NUM_VALORES] of record texto : string ; numero : integer end = (
      (texto:'H';             numero:1),
      (texto:'M';             numero:2),
      (texto:'2ciclo';        numero:1),
      (texto:'adaptacion';    numero:2),
      (texto:'COU';           numero:3),
      (texto:'erasmus';       numero:4),
      (texto:'FP';            numero:5),
      (texto:'mayor25';       numero:6),
      (texto:'selectividad';  numero:7),
      (texto:'titulado';      numero:8),
      (texto:'1anyo';         numero:1),
      (texto:'2anyos';        numero:2),
      (texto:'3anyos';        numero:3),
      (texto:'4anyos';        numero:4),
      (texto:'5anyos';        numero:5),
      (texto:'6anyos';        numero:6),
      (texto:'7anyos';        numero:7),
      (texto:'8anyos';        numero:8),
      (texto:'9anyos';        numero:9),
      (texto:'10anyos';       numero:10),
      (texto:'mas_de_10';     numero:11));


type
   TErrorLectura = (NO_error, E_fichero_no_existe, E_fichero_vacio, E_formato_no_valido, E_atributo_no_valido, E_comparador_no_valido);



   procedure LecturaArbol        (nombre : string; var arbol : TArbol);

   procedure AbrirFichero        (nombre : string; var fichero : TextFile; var error : TErrorLectura);
   procedure LeerLineaEficiencia (var fichero : TextFile; var arbol : TArbol; var error : TErrorLectura);
   procedure LeerLineaDatos      (var fichero : TextFile; var tupla, tupla_clase : TRegNodo; var profundidad : integer; var error : TErrorLectura);
   procedure DecodificarLinea    (var linea : string; var tupla : TRegNodo; var profundidad : integer; var error : TErrorLectura);
   procedure DecodificarClase    (var linea : string; var tupla : TRegNodo; var error : TErrorLectura);
   procedure ExtraerProfundidad  (var linea : string; var profundidad : integer; var error : TErrorLectura);
   procedure ExtraerAtributo     (var linea : string; var tupla : TRegNodo; var error : TErrorLectura);
   procedure ExtraerComparador   (var linea : string; var tupla : TRegNodo; var error : TErrorLectura);
   procedure ExtraerValor        (var linea : string; var tupla : TRegNodo; var error : TErrorLectura);
   procedure ExtraerClase        (var linea : string; var tupla : TRegNodo; var error : TErrorLectura);
   procedure ConvertirAtributo   (nombre : string; var atr : TAtributo; var error : TErrorLectura);
   procedure ConvertirComparador (nombre : string; var comp : TComparador; var error : TErrorLectura);
   procedure ConvertirValor      (nombre : string; var valor : integer; var error : TErrorLectura);

   procedure InsertarNodo        (var arbol : TArbol; var tupla : TRegNodo; profundidad : integer; var error : TErrorLectura);
   procedure InsertarNodoRec     (nodo  : TNodo;  var tupla : TRegNodo; profundidad : integer; var error : TErrorLectura);

   procedure CerrarFichero       (var fichero : TextFile);

   procedure MostrarMensajeError (error : TErrorLectura; texto : string);

   function TamFichero (nombre : string) : longint;


   


implementation





//  En este procedimiento se realiza toda la operación de lectura del fichero y
//  se traspasa la información al árbol de datos.

procedure LecturaArbol (nombre : string; var arbol : TArbol);

var
   tupla, tupla_clase : TRegNodo;
   fichero : TextFile;
   profundidad : integer;
   error : TErrorLectura;

begin
   error := NO_error;

   arbol.Free;

   AbrirFichero (nombre, fichero, error);
   if error <> NO_error then begin
      MostrarMensajeError (error, nombre);
      exit;
   end;

   arbol := TArbol.Create;

   LeerLineaEficiencia (fichero, arbol, error);
   if error <> NO_error then begin
      CerrarFichero (fichero);
      arbol.Free;
      MostrarMensajeError (error, nombre);
      exit;
   end;

   tupla := TRegNodo.Create;
   tupla_clase := TRegNodo.Create;

   while not SeekEof (fichero) do begin
      tupla_clase.EscribirComparador (c_mayor);
      LeerLineaDatos (fichero, tupla, tupla_clase, profundidad, error);
      if error <> NO_error then begin
         CerrarFichero (fichero);
         tupla.Free;
         tupla_clase.Free;
         arbol.Free;
         MostrarMensajeError (error, nombre);
         exit;
      end;

      InsertarNodo (arbol, tupla, profundidad, error);
      if error <> NO_error then begin
         CerrarFichero (fichero);
         tupla.Free;
         tupla_clase.Free;
         arbol.Free;
         MostrarMensajeError (error, nombre);
         exit;
      end;

      if tupla_clase.LeerComparador = c_igual then begin
         InsertarNodo (arbol, tupla_clase, profundidad + 1, error);
         if error <> NO_error then begin
            CerrarFichero (fichero);
            tupla.Free;
            tupla_clase.Free;
            arbol.Free;
            MostrarMensajeError (error, nombre);
            exit;
         end;
      end;
   end;
   tupla.Free;
   tupla_clase.Free;
   CerrarFichero (fichero);
end;





procedure AbrirFichero (nombre : string; var fichero : TextFile; var error : TErrorLectura);

begin
   FileMode := fmOpenRead;
   if not FileExists (nombre) then begin
      error := e_fichero_no_existe;
      exit;
   end;

   AssignFile (fichero, nombre);
   Reset (fichero);
   if TamFichero (nombre) = 0 then begin
      error := e_fichero_vacio;
      CloseFile (fichero);
   end;
end;





procedure LeerLineaEficiencia (var fichero : TextFile; var arbol : TArbol; var error : TErrorLectura);

var
   linea : string;
   valor : integer;

begin
   ReadLn (fichero, linea);
   valor := StrToIntDef (linea, 0);
   arbol.EscribirEficiencia (valor);
end;





procedure LeerLineaDatos (var fichero : TextFile; var tupla, tupla_clase : TRegNodo; var profundidad : integer; var error : TErrorLectura);

var
   linea : string;

begin
   ReadLn (fichero, linea);

   DecodificarLinea (linea, tupla, profundidad, error);
   if error <> NO_error then
      exit;
   if linea <> '' then
      DecodificarClase (linea, tupla_clase, error);
end;





procedure DecodificarLinea (var linea : string; var tupla : TRegNodo; var profundidad : integer; var error : TErrorLectura);

begin
   ExtraerProfundidad (linea, profundidad, error);
   if error <> NO_error then
      exit;

   ExtraerAtributo (linea, tupla, error);
   if error <> NO_error then
      exit;

   ExtraerComparador (linea, tupla, error);
   if error <> NO_error then
      exit;

   if tupla.LeerAtributo = atr_duracion then
      ExtraerClase (linea, tupla, error)
   else
      ExtraerValor (linea, tupla, error);
end;





procedure DecodificarClase (var linea : string; var tupla : TRegNodo; var error : TErrorLectura);

begin
   tupla.EscribirAtributo (atr_duracion);
   tupla.EscribirComparador (c_igual);
   ExtraerClase (linea, tupla, error);
end;





procedure ExtraerProfundidad (var linea : string; var profundidad : integer; var error : TErrorLectura);

var
   posicion : integer;

begin
   profundidad := 1;
   posicion := Pos ('|', linea);
   while (posicion > 0) do begin
      inc (profundidad);
      Delete (linea, 1, posicion);
      posicion := Pos ('|', linea);
   end;
   linea := Trim (linea);
end;





procedure ExtraerAtributo (var linea : string; var tupla : TRegNodo; var error : TErrorLectura);

var
   posicion : integer;
   palabra : string;
   atributo : TAtributo;

begin
   posicion := Pos (' ', linea);
   palabra := LeftStr (linea, (posicion - 1));
   Delete (linea, 1, posicion);
   ConvertirAtributo (palabra, atributo, error);
   if error = NO_error then
      tupla.EscribirAtributo (atributo);
end;





procedure ExtraerComparador (var linea : string; var tupla : TRegNodo; var error : TErrorLectura);

var
   posicion : integer;
   palabra : string;
   comp : TComparador;

begin
   posicion := Pos (' ', linea);
   palabra := LeftStr (linea, (posicion - 1));
   Delete (linea, 1, posicion);
   ConvertirComparador (palabra, comp, error);
   if error = NO_error then
      tupla.EscribirComparador (comp);
end;





procedure ExtraerValor (var linea : string; var tupla : TRegNodo; var error : TErrorLectura);

var
   posicion, valor : integer;
   palabra : string;

begin
   posicion := Pos (': ', linea);
   if posicion = 0 then begin
      palabra := Trim (linea);
      linea := '';
   end
   else begin
      palabra := LeftStr (linea, (posicion - 1));
      Delete (linea, 1, (posicion + 1));
   end;
   ConvertirValor (palabra, valor, error);
   if error = NO_error then
      tupla.EscribirValor(valor);
end;





procedure ExtraerClase (var linea : string; var tupla : TRegNodo; var error : TErrorLectura);

var
   posicion, valor : integer;
   palabra : string;

begin
   posicion := Pos (' ', linea);
   palabra := LeftStr (linea, (posicion - 1));
   linea := '';
   ConvertirValor (palabra, valor, error);
   if error = NO_error then
      tupla.EscribirValor(valor);
end;





procedure ConvertirAtributo (nombre : string; var atr : TAtributo; var error : TErrorLectura);

var
   indice : integer;

begin
   indice := AnsiIndexText (nombre, NOMBRE_ATRIBUTO);
   if indice >= 0 then
      atr := TAtributo (indice)
   else
      error := E_atributo_no_valido;
end;





procedure ConvertirComparador (nombre : string; var comp : TComparador; var error : TErrorLectura);

var
   indice : integer;

begin
   indice := AnsiIndexText (nombre, TEXTO_COMPARADOR);
   if indice >= 0 then
      comp := TComparador (indice)
   else
      error := E_comparador_no_valido;
end;




procedure ConvertirValor (nombre : string; var valor : integer; var error : TErrorLectura);

var
   indice : integer;
   coincidencia : boolean;

begin
   indice := 0;
   repeat
      inc (indice);
      coincidencia := AnsiCompareStr (TEXTO_VALOR[indice].texto, nombre) = 0;
   until coincidencia or (indice = NUM_VALORES);
   if coincidencia then
      valor := TEXTO_VALOR [indice].numero
   else
      valor := StrToIntDef (nombre, 0);
end;





procedure InsertarNodo (var arbol : TArbol; var tupla : TRegNodo; profundidad : integer; var error : TErrorLectura);

var
   numero : integer;

begin
   if profundidad = 1 then
      arbol.InsertarHijo (tupla)
   else begin
      numero := arbol.NumHijos;
      InsertarNodoRec (arbol.LeerHijo (numero - 1), tupla, profundidad, error);
   end;
end;





procedure InsertarNodoRec (nodo : TNodo; var tupla : TRegNodo; profundidad : integer; var error : TErrorLectura);

var
   numero : integer;

begin
   if (profundidad - 1) = nodo.LeerNivel then
      nodo.InsertarHijo (tupla)
   else begin
      numero := nodo.NumHijos;
      InsertarNodoRec (nodo.LeerHijo (numero - 1), tupla, profundidad, error);
   end;
end;





procedure CerrarFichero (var fichero : TextFile);

begin
   CloseFile (fichero);
end;





procedure MostrarMensajeError (error : TErrorLectura; texto : string);

begin
   case error of
      E_fichero_no_existe : ShowMessage ('ERROR: El fichero de datos "' + texto + '" no existe');
      E_fichero_vacio     : ShowMessage ('ERROR: El fichero de datos "' + texto + '" está vacío');
      E_formato_no_valido : ShowMessage ('ERROR: El formato del fichero "' + texto + '" no es correcto');
      E_atributo_no_valido : ShowMessage ('ERROR: Nombre de atributo no coincide');
      E_comparador_no_valido : ShowMessage ('ERROR: Formato del texto no coincide');
   end;
end;





function TamFichero (nombre : string) : longint;

var
   fichero : file of Byte;

begin
  AssignFile(fichero, nombre);
  Reset(fichero);
  TamFichero := FileSize (fichero);
  CloseFile (fichero);
end;





end.
