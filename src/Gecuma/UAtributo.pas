unit UAtributo;


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
      atr_duracion,
      atr_num_aprobados,
      atr_num_notables,
      atr_num_sobresalientes,
      atr_num_matriculas_honor,
      atr_num_suspensos,
      atr_num_no_presentados);


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
         procedure BuscarAtributo   (nombre : string; var atr : TAtributo; var error : boolean);
         procedure BuscarComparador (nombre : string; var comp : TComparador; var error : boolean);

   end;


const

   NOMBRE_ATRIBUTO : array [TAtributo] of string = (
      'TITULACION',
      'EDAD_INICIO',
      'GENERO:',
      'CODIGO_POSTAL',
      'PROVINCIA',
      'NACIONALIDAD',
      'EXPEDIENTE',
      'ACCESO:',
      'CALIFICACION',
      'DURACION',
      'NUM_APROBADOS',
      'NUM_NOTABLES',
      'NUM_SOBRESALIENTES',
      'NUM_MATRICULAS_HONOR',
      'NUM_SUSPENSOS',
      'NUM_NO_PRESENTADOS');

   TEXTO_COMPARADOR : array [TComparador] of string = (
      '=',
      '>',
      '<=');



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





procedure TRegNodo.BuscarAtributo (nombre : string; var atr : TAtributo; var error : boolean);
var
   i : integer;
begin
   error := false;
   i := AnsiIndexText (nombre, NOMBRE_ATRIBUTO);
   if i >= 0 then
      atr := TAtributo (i)
   else begin
      ShowMessage ('ERROR: Nombre de atributo no coincide');
      error := true;
   end;
end;


procedure TRegNodo.BuscarComparador (nombre : string; var comp : TComparador; var error : boolean);
var
   i : integer;
begin
   error := false;
   i := AnsiIndexText (nombre, TEXTO_COMPARADOR);
   if i >= 0 then
      comp := TComparador (i)
   else begin
      ShowMessage ('ERROR: Formato del texto no coincide');
      error := true;
   end;
end;


end.
