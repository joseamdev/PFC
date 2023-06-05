unit UEjecucion;



interface


uses

   Dialogs, ULectura, UClaseArbol, UClaseAtributo, UDatos;


type
   TErrorEjecucion = (NO_error, E_nodo_no_existe, E_arbol_no_existe);


   procedure Calcular        (arbol : TArbol; var tupla : TRegDatos);

   procedure RecorrerArbol   (arbol : TArbol; var tupla : TRegDatos; var error : TErrorEjecucion);
   procedure RecorrerNodo    (nodo : TNodo; var tupla : TRegDatos; var error : TErrorEjecucion);
   function  CompararNodo    (regnodo : TRegNodo; tupla : TRegDatos) : boolean;
   procedure MostrarMensajeError (error : TErrorEjecucion);



implementation





procedure Calcular (arbol : TArbol; var tupla : TRegDatos);

var
   error : TErrorEjecucion;

begin
   error := NO_error;
   if arbol = NIL then begin
      error := E_arbol_no_existe;
      MostrarMensajeError (error);
      exit
   end;
   RecorrerArbol (arbol, tupla, error);
   MostrarMensajeError (error);
end;





procedure RecorrerArbol (arbol : TArbol; var tupla : TRegDatos; var error : TErrorEjecucion);

var
   indice, numero : integer;

begin
   indice := 0;
   numero := arbol.NumHijos;
   while (indice < numero) and (not CompararNodo (arbol.LeerHijo(indice).LeerNodo, tupla)) do
      inc (indice);
   if indice < numero then
      RecorrerNodo (arbol.LeerHijo(indice), tupla, error)
   else
      error := E_nodo_no_existe;
end;





procedure RecorrerNodo (nodo : TNodo; var tupla : TRegDatos; var error : TErrorEjecucion);

var
   indice, numero : integer;

begin
   indice := 0;
   numero := nodo.NumHijos;
   if (numero = 0) then
      EscribirDatosAlumno (tupla, atr_duracion, nodo.LeerNodo.LeerValor)
   else if (numero = 1) and nodo.LeerHijo(indice).EsHoja then
      EscribirDatosAlumno (tupla, atr_duracion, nodo.LeerHijo(indice).LeerNodo.LeerValor)
   else begin
      while (indice < numero) and (not CompararNodo (nodo.LeerHijo(indice).LeerNodo, tupla)) do
         inc (indice);
      if indice < numero then
         RecorrerNodo (nodo.LeerHijo(indice), tupla, error)
      else
         error := E_nodo_no_existe;
   end;
end;





function CompararNodo (regnodo : TRegNodo; tupla : TRegDatos) : boolean;

var
   valornodo, valortupla : integer;

begin
   valornodo  := regnodo.LeerValor;
   valortupla := LeerDatosAlumno(tupla, regnodo.LeerAtributo);
   case regnodo.LeerComparador of
      c_igual      : result := (valortupla =  valornodo);
      c_mayor      : result := (valortupla >  valornodo);
      c_menorigual : result := (valortupla <= valornodo);
   else
      result := false;
   end;
   if regnodo.LeerAtributo = atr_duracion then
      result := true;
end;





procedure MostrarMensajeError (error : TErrorEjecucion);

begin
   case error of
      E_nodo_no_existe : ShowMessage ('ERROR: Datos insuficientes en el árbol.');
      E_arbol_no_existe : ShowMessage ('ERROR: El árbol de datos no fue creado.');
   end;
end;





end.
