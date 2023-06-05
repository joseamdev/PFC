unit UTablas;


interface


uses
   StrUtils, SysUtils, Dialogs, Math;

   
const
    SEPARADOR        = #32#32;
    TAM_SEPARADOR    = 2;     // número de caracteres del separador
    TAM_CENTRO       = 3;     // número de caracteres del código del centro
    TAM_SECCION      = 2;     // número de caracteres del código de la sección
    TAM_NACIONALIDAD = 3;     // número de caracteres del código de la nacionalidad
    TAM_PROVINCIA    = 2;     // número de caracteres del código de la provincia
    TAM_CALIFICACION = 1;     // número de caracteres del código de la calificación
    TAM_CONVOCATORIA = 1;     // número de caracteres del código de la convocatoria
    TAM_ACCESO       = 1;     // número de caracteres del código de la vía de acceso


type

   // Es la tabla base para todas las demás tablas

   TTabla = class
      private
         numero : integer;
      public
         constructor Create;
         destructor  Destroy; override;
         function    NumeroElementos : integer;
   end;

   // Tablas con los nombres de Vía de acceso, Provincia y Nacionalidad

   TTablaSimple = class (TTabla)
      private
         tamano_codigo : integer;
         codigo : array of integer;
         nombre : array of string;
      public
         constructor Create (tam : integer);
         destructor  Destroy; override;
         function    LeerCodigo  (indice : integer) : integer;
         function    LeerNombre  (indice : integer) : string;
         function    NombreValor (valor : integer) : string;
         procedure   CargarTabla (nombre_fichero : string);
   end;

   // Tablas con los nombres de Titulacion

   TTablaTitulacion = class (TTablaSimple)
      private
         seccion : array of integer;
      public
         constructor Create;
         destructor  Destroy; override;
         function    LeerSeccion   (indice : integer) : integer;
         function    NombreSeccion (cen, sec : integer) : string;
         procedure   CargarTabla   (nombre_fichero : string);
   end;



implementation





{ TTabla }





constructor TTabla.Create;

begin
   inherited Create;
   numero := 0;
end;





destructor TTabla.Destroy;

begin
   inherited Destroy;
end;





function TTabla.NumeroElementos: integer;

begin
   result := numero;
end;





{ TTablaSimple }





constructor TTablaSimple.Create (tam : integer);

begin
   inherited Create;
   tamano_codigo := tam;
end;





function TTablaSimple.LeerCodigo (indice : integer) : integer;

begin
   if indice >= numero then
      result := 0
   else
      result := codigo[indice];
end;





function TTablaSimple.LeerNombre(indice : integer) : string;

begin
   if indice >= numero then
      result := ''
   else
      result := nombre[indice];
end;





procedure TTablaSimple.CargarTabla (nombre_fichero : string);

var
   fichero : TextFile;
   linea : string;

begin
   FileMode := fmOpenRead;
   AssignFile (fichero, nombre_fichero);
   Reset (fichero);
   while not SeekEof (fichero) do begin
      ReadLn (fichero, linea);
      inc (numero);
      SetLength (codigo, numero);
      SetLength (nombre, numero);
      codigo[numero-1] := StrToIntDef (LeftStr (linea, tamano_codigo), 0);
      Delete (linea, 1, tamano_codigo + TAM_SEPARADOR);
      nombre[numero-1] := linea;
   end;
   CloseFile (fichero);
end;





destructor TTablaSimple.Destroy;

begin
   inherited Destroy;
end;





function TTablaSimple.NombreValor(valor: integer): string;

var
   coincidencia : boolean;
   indice : integer;

begin
   coincidencia := false;
   indice := 0;
   while (indice < numero) and not coincidencia do begin
      coincidencia := (valor = codigo[indice]);
      inc (indice);
   end;
   dec (indice);
   if coincidencia then
      result := LeerNombre (indice)
   else
      result := '¿?';
end;





{ TTablaTitulacion }





constructor TTablaTitulacion.Create;

begin
   inherited Create (3);
end;





function TTablaTitulacion.LeerSeccion (indice : integer) : integer;

begin
   result := seccion[indice];
end;





procedure TTablaTitulacion.CargarTabla (nombre_fichero : string);

var
   fichero : TextFile;
   linea : string;

begin
   FileMode := fmOpenRead;
   AssignFile (fichero, nombre_fichero);
   Reset (fichero);
   while not SeekEof (fichero) do begin
      ReadLn (fichero, linea);
      inc (numero);
      SetLength (codigo, numero);
      SetLength (seccion, numero);
      SetLength (nombre, numero);
      codigo[numero-1] := StrToIntDef (LeftStr (linea, TAM_CENTRO), 0);
      Delete (linea, 1, TAM_CENTRO + TAM_SEPARADOR);
      seccion[numero-1] := StrToIntDef (LeftStr (linea, TAM_SECCION), 0);
      Delete (linea, 1, TAM_SECCION + TAM_SEPARADOR);
      nombre[numero-1] := linea;
   end;
   CloseFile (fichero);
end;




destructor TTablaTitulacion.Destroy;

begin
   inherited Destroy;
end;





function TTablaTitulacion.NombreSeccion(cen, sec: integer): string;

var
   coincidencia : boolean;
   indice : integer;

begin
   coincidencia := false;
   indice := 0;
   while (indice < numero) and not coincidencia do begin
      coincidencia := (cen  = codigo[indice]) and
                      (sec  = seccion[indice]);
      inc (indice);
   end;
   dec (indice);
   if coincidencia then
      result := LeerNombre (indice)
   else
      result := '¿?';
end;





end.
