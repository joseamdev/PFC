unit UDatos;



interface


uses
   StrUtils, Dialogs, UClaseAtributo;


type
   TRegDatos = array [TAtributo] of integer;


   function  LeerDatosAlumno     (tupla : TRegDatos; atributo : TAtributo) : integer;
   procedure EscribirDatosAlumno (var tupla : TRegDatos; atributo : TAtributo; valor : integer);



implementation





function LeerDatosAlumno (tupla : TRegDatos; atributo : TAtributo) : integer;

begin
   result := tupla[atributo];
end;





procedure EscribirDatosAlumno (var tupla : TRegDatos; atributo : TAtributo; valor : integer);

begin
   tupla[atributo] := valor;
end;





end.
