unit UClaseArbol;


interface


uses
   UClaseAtributo;


type

   TNodo = class
      private
         nodo      : TRegNodo;
         nivel     : integer;
         num_hijos : integer;
         hijos     : array of TNodo;
      public
         constructor Create;
         destructor  Destroy; override;
         function    LeerNodo  : TRegNodo;
         function    LeerNivel : integer;
         function    NumHijos  : integer;
         function    EsHoja    : boolean;
         procedure   EscribirNodo (reg : TRegNodo);
         function    LeerHijo     (i : integer) : TNodo;
         procedure   InsertarHijo (reg : TRegNodo);
         procedure   EliminarHijo (i : integer);
   end;

   TArbol = class
      private
         eficiencia : integer;
         num_hijos  : integer;
         nodos      : array of TNodo;
      public
         constructor Create;
         destructor  Destroy; override;
         function    LeerEficiencia : integer;
         function    NumHijos       : integer;
         function    LeerHijo           (i : integer) : TNodo;
         procedure   EscribirEficiencia (valor : integer);
         procedure   InsertarHijo       (reg : TRegNodo);
         procedure   EliminarHijo       (i : integer);
   end;



implementation





{ TNodo }





constructor TNodo.Create;

begin
   inherited Create;
   num_hijos := 0;
   SetLength (hijos, 0);
end;





destructor TNodo.Destroy;

begin
   while num_hijos > 0 do begin
      dec (num_hijos);
      hijos[num_hijos].Free;
   end;
   SetLength (hijos, 0);
   inherited Destroy;
end;





function TNodo.LeerNodo : TRegNodo;

begin
   result := nodo;
end;





function TNodo.LeerNivel : integer;

begin
   result := nivel;
end;





function TNodo.NumHijos: integer;

begin
   result := num_hijos;
end;





function TNodo.EsHoja : boolean;

begin
   result := num_hijos = 0;
end;





procedure TNodo.EscribirNodo (reg : TRegNodo);

begin
   nodo := reg;
end;





function TNodo.LeerHijo (i : integer) : TNodo;

begin
   if i < num_hijos then
      result := hijos[i]
   else
      result := NIL;
end;





procedure TNodo.InsertarHijo (reg : TRegNodo);

begin
   inc (num_hijos);
   SetLength (hijos, num_hijos);
   hijos[num_hijos-1] := TNodo.Create;
   hijos[num_hijos-1].nodo := TRegNodo.Create;
   hijos[num_hijos-1].nodo.EscribirAtributo (reg.LeerAtributo);
   hijos[num_hijos-1].nodo.EscribirComparador (reg.LeerComparador);
   hijos[num_hijos-1].nodo.EscribirValor (reg.LeerValor);
   hijos[num_hijos-1].nivel := nivel + 1;
end;





procedure TNodo.EliminarHijo (i : integer);

begin
   if i < num_hijos then begin
      hijos[i].Free;
      dec (num_hijos);
      while i < num_hijos do begin
         hijos[i] := hijos[i+1];
         inc (i);
      end;
      SetLength (hijos, num_hijos);
   end;
end;





{ TArbol }





constructor TArbol.Create;

begin
   inherited Create;
   num_hijos := 0;
   SetLength (nodos, 0);
end;





destructor TArbol.Destroy;

begin
   while num_hijos > 0 do begin
      dec (num_hijos);
      nodos[num_hijos].Free;
   end;
   SetLength (nodos, 0);
   inherited Destroy;
end;





function TArbol.LeerEficiencia : integer;

begin
   result := eficiencia;
end;





function TArbol.NumHijos: integer;

begin
   result := num_hijos;
end;





function TArbol.LeerHijo (i : integer) : TNodo;

begin
   if i < num_hijos then
      result := nodos[i]
   else
      result := NIL;
end;





procedure TArbol.EscribirEficiencia (valor : integer);

begin
   eficiencia := valor;
end;





procedure TArbol.InsertarHijo (reg : TRegNodo);

begin
   inc (num_hijos);
   SetLength (nodos, num_hijos);
   nodos[num_hijos-1] := TNodo.Create;
   nodos[num_hijos-1].nodo := TRegNodo.Create;
   nodos[num_hijos-1].nodo.EscribirAtributo (reg.LeerAtributo);
   nodos[num_hijos-1].nodo.EscribirComparador (reg.LeerComparador);
   nodos[num_hijos-1].nodo.EscribirValor (reg.LeerValor);
   nodos[num_hijos-1].nivel := 1;
end;





procedure TArbol.EliminarHijo (i : integer);

begin
   if i < num_hijos then begin
      nodos[i].Free;
      dec (num_hijos);
      while i < num_hijos do begin
         nodos[i] := nodos[i+1];
         inc (i);
      end;
      SetLength (nodos, num_hijos);
   end;
end;





end.
