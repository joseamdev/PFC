unit UClaseAtributo;


interface


uses
   StrUtils, Dialogs;


type

   TAtributo = (
      atr_titulacion,
      atr_edad_inicio,
      atr_genero,
      atr_codigo_postal,
      atr_provincia,
      atr_nacionalidad,
      atr_expediente,
      atr_acceso,
      atr_calificacion,
      atr_duracion);


   TComparador = (c_igual, c_mayor, c_menorigual);


   TRegNodo = class
      private
         nodo_atributo : TAtributo;
         nodo_comp     : TComparador;
         nodo_valor    : integer;
      public
         constructor Create;
         destructor Destroy; override;
         function LeerAtributo      : TAtributo;
         function LeerComparador    : TComparador;
         function LeerValor         : integer;
         procedure EscribirAtributo   (atributo : TAtributo);
         procedure EscribirComparador (comparador : TComparador);
         procedure EscribirValor      (valor : integer);

   end;





implementation





constructor TRegNodo.Create;

begin
   inherited Create;
end;





destructor TRegNodo.Destroy;

begin
   inherited Destroy;
end;





function TRegNodo.LeerAtributo : TAtributo;

begin
   result := nodo_atributo;
end;





function TRegNodo.LeerComparador : TComparador;

begin
   result := nodo_comp;
end;





function TRegNodo.LeerValor : integer;

begin
   result := nodo_valor;
end;





procedure TRegNodo.EscribirAtributo (atributo : TAtributo);

begin
   nodo_atributo := atributo;
end;





procedure TRegNodo.EscribirComparador (comparador : TComparador);

begin
   nodo_comp := comparador;
end;





procedure TRegNodo.EscribirValor (valor : integer);

begin
   nodo_valor := valor;
end;





end.
