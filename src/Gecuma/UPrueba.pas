unit UPrueba;


interface

uses
   UClaseAtributo, UClaseArbol, SysUtils;

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

   procedure MostrarArbol  (var arbol : TArbol);
   procedure RecorrerArbol (var arbol : TArbol);
   procedure RecorrerNodo  (nodo : TNodo);
   procedure EscribirLinea (var nodo : TNodo);




implementation


var
   fichero : TextFile;



procedure MostrarArbol (var arbol : TArbol);

begin
   FileMode := fmOpenWrite;
   AssignFile (fichero, 'arbol.txt');
   ReWrite (fichero);

   RecorrerArbol (arbol);

   CloseFile (fichero);
end;





procedure EscribirLinea (var nodo : TNodo);

var
   atributo : TAtributo;
   comp : TComparador;
   valor, nivel : integer;
   linea : string;

begin
   nivel := nodo.LeerNivel;
   atributo := nodo.LeerNodo.LeerAtributo;
   comp := nodo.LeerNodo.LeerComparador;
   valor := nodo.LeerNodo.LeerValor;
   linea := StringOfChar (' ', nivel * 2) +
            '|_' +
            NOMBRE_ATRIBUTO [atributo] + ' ' +
            TEXTO_COMPARADOR [comp] + ' ' +
            IntToStr (valor);
   WriteLn (fichero, linea);
end;




procedure RecorrerArbol (var arbol : TArbol);

var
   indice, numero : integer;

begin
   indice := 0;
   numero := arbol.NumHijos;
   while (indice < numero) do begin
      RecorrerNodo (arbol.LeerHijo(indice));
      inc (indice);
   end;
end;





procedure RecorrerNodo (nodo : TNodo);

var
   indice, numero : integer;

begin
   indice := 0;
   numero := nodo.NumHijos;
   EscribirLinea (nodo);
   while (indice < numero) do begin
      RecorrerNodo (nodo.LeerHijo(indice));
      inc (indice);
   end;
end;




end.
