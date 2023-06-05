unit Tablas;

interface

  uses
    Main;

  const
    SEPARADOR = #32#32;
    TAM_SEPARADOR = 2;    // número de caracteres del separador
    TAM_CENTRO = 3;       // número de caracteres del código del centro
    TAM_SECCION = 2;      // número de caracteres del código de la sección
    TAM_PLAN = 2;         // número de caracteres del código del plan
    TAM_ASIGNATURA = 3;   // número de caracteres del código de la asignatura
    TAM_NACIONALIDAD = 3;  // número de caracteres del código de la nacionalidad
    TAM_PROVINCIA = 2;    // número de caracteres del código de la provincia
    TAM_CALIFICACION = 1; // número de caracteres del código de la calificación
    TAM_CONVOCATORIA = 1; // número de caracteres del código de la convocatoria
    TAM_ACCESO = 1;       // número de caracteres del código de la vía de acceso

  type

    // Es la tabla base para todas las demás tablas

    TTabla = class
      private
        numero : integer;
      public
        constructor Create;
        destructor Destroy; override;
        function Numero_Elementos : integer;
    end;

    // Tablas con los nombres de Calificación, Convocatoria, Vía de acceso, Provincia, Centro y Nacionalidad

    TTablaSimple = class (TTabla)
      private
        tamano_codigo : integer;
        codigo : array of integer;
        nombre : array of string;
      public
        constructor Create (tam : integer);
        destructor Destroy; override;
        function Leer_Codigo (indice : integer) : integer;
        function Leer_Nombre (indice : integer) : string;
        function Nombre_Valor (valor : integer) : string;
        procedure Cargar_Tabla (nombre_fichero : string);
    end;

    // Tablas con los nombres de Titulación, Plan y Asignatura

    TTablaTitulacion = class (TTablaSimple)
      private
        seccion : array of integer;
      public
        constructor Create;
        destructor Destroy; override;
        function Leer_Seccion (indice : integer) : integer;
        function Nombre_Seccion (cen, sec : integer) : string;
        procedure Cargar_Tabla (nombre_fichero : string);
    end;

    TTablaPlan = class (TTablaTitulacion)
      private
        plan : array of integer;
      public
        constructor Create;
        destructor Destroy; override;
        function Leer_Plan (indice : integer) : integer;
        function Nombre_Plan (cen, sec, pl : integer) : string;
        procedure Cargar_Tabla (nombre_fichero : string);
    end;

    TTablaNombreAsignatura = class (TTablaPlan)
      private
        asignatura : array of integer;
      public
        constructor Create;
        destructor Destroy; override;
        function Leer_Asignatura (indice : integer) : integer;
        function Nombre_Asig (cen, sec, pl, asig : integer) : string;
        procedure Cargar_Tabla (nombre_fichero : string);
    end;

    // Tabla de Asignatura

    TTablaAsignatura = class (TTabla)
      private
        centro      : array of integer;
        seccion     : array of integer;
        plan        : array of integer;
        asignatura  : array of integer;
      public
        constructor Create;
        destructor  Destroy; override;
        function    Leer_Centro (indice : integer) : integer;
        function    Leer_Seccion (indice : integer) : integer;
        function    Leer_Plan (indice : integer) : integer;
        function    Leer_Asignatura (indice : integer) : integer;
        function    Leer_Indice (indice : integer) : string;
        procedure   Insertar_Indice (s : string);
        procedure   Insertar_Asignatura (c, s, p, a : integer);
        procedure   Eliminar;
        procedure   Eliminar_Asignatura (indice : integer);
    end;

    // Tabla de Adaptación

    TTablaAdaptacion = class (TTablaAsignatura)
      private
        centro2 : array of integer;
        seccion2 : array of integer;
        plan2 : array of integer;
        asignatura2 : array of integer;
      public
        constructor Create;
        destructor Destroy; override;
        function Leer_Centro2 (indice : integer) : integer;
        function Leer_Seccion2 (indice : integer) : integer;
        function Leer_Plan2 (indice : integer) : integer;
        function Leer_Asignatura2 (indice : integer) : integer;
        procedure Buscar (c, s, p, a : integer; var indice : integer; var coincidencia : boolean);
        procedure Cargar_Tabla (nombre_fichero : string);
    end;

    //  Tabla con los datos de filtrado de atributos

    TAtributoFiltrado =  (naf_genero,
                          naf_nacionalidad,
                          naf_provincia,
                          naf_acceso,
                          naf_finalizado,
                          naf_asignatura);

    TTablaFiltrado = class
      private
        lista_atr   : array [TAtributoFiltrado] of integer;
        lista_asig  : TTablaAsignatura;
      public
        constructor Create;
        destructor  Destroy; override;
        function    Leer (atributo : TAtributoFiltrado) : integer;
        procedure   Escribir (atributo : TAtributoFiltrado; valor : integer);
        function    Lista_Asignatura : TTablaAsignatura;
        procedure   Leer_Componentes;
        procedure   Escribir_Componentes;
    end;

    //  Tabla con los datos de selección de atributos

    TAtributoSeleccion = (nas_titulacion,               nas_especialidad,
                          nas_edad_inicio,              nas_edad_final,
                          nas_genero,                   nas_codigo_postal,
                          nas_provincia,                nas_nacionalidad,
                          nas_expediente,               nas_acceso,
                          nas_calificacion,             nas_curso_fin,
                          nas_duracion,                 nas_num_aprobados,
                          nas_num_notables,             nas_num_sobresalientes,
                          nas_num_matriculas_honor,     nas_num_suspensos,
                          nas_num_no_presentados,       nas_nota_media,
                          nas_asig_calificacion,        nas_asig_curso,
                          nas_asig_convocatoria,        nas_asig_num_convocatoria,
                          nas_asig_num_matriculaciones);

    TAtributoClase = (nac_codigo_postal,    nac_provincia,
                      nac_genero,           nac_nacionalidad,
                      nac_acceso,           nac_duracion,
                      nac_titulacion);

    TTablaSeleccion = class
      private
        clase : TAtributoClase;
        lista_atr : array [TAtributoSeleccion] of boolean;
        lista_asig : TTablaAsignatura;
      public
        constructor Create;
        destructor Destroy; override;
        function Leer_Seleccion (atributo : TAtributoSeleccion) : boolean;
        procedure Escribir_Seleccion (atributo : TAtributoSeleccion; valor : boolean);
        function Leer_Clase : TAtributoClase;
        procedure Escribir_Clase (valor : TAtributoClase);
        function Lista_Asignatura : TTablaAsignatura;
        procedure Leer_Componentes;
        procedure Escribir_Componentes;
    end;

    // Tabla con la configuración del programa

    TFicheroConfiguracion =  (nfc_alumno,
                              nfc_academico,
                              nfc_experiencia,
                              nfc_asignatura,
                              nfc_centro,
                              nfc_plan,
                              nfc_titulacion,
                              nfc_adaptacion,
                              nfc_calificacion,
                              nfc_convocatoria,
                              nfc_nacion,
                              nfc_provincia,
                              nfc_acceso);

    TTablaConfiguracion = class
      private
        lista_fich : array [TFicheroConfiguracion] of string;
        filtro : TTablaFiltrado;
        seleccion : TTablaSeleccion;
      public
        constructor Create;
        destructor Destroy; override;
        procedure Cargar (nombre_fichero : string);
        procedure Guardar (nombre_fichero : string);
        function Tabla_Filtrado : TTablaFiltrado;
        function Tabla_Seleccion : TTablaSeleccion;
        procedure Leer_Componentes;
        procedure Escribir_Componentes;
    end;


implementation


  uses
    StrUtils, SysUtils, Dialogs, Math;

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

  function TTabla.Numero_Elementos: integer;
    begin
      result := numero;
    end;

{ TTablaSimple }

  constructor TTablaSimple.Create (tam : integer);
    begin
      inherited Create;
      tamano_codigo := tam;
    end;

  function TTablaSimple.Leer_Codigo (indice : integer) : integer;
    begin
      if indice >= numero then
        result := 0
      else
        result := codigo[indice];
    end;

  function TTablaSimple.Leer_Nombre(indice : integer) : string;
    begin
      if indice >= numero then
        result := ''
      else
        result := nombre[indice];
    end;

  procedure TTablaSimple.Cargar_Tabla (nombre_fichero : string);
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

function TTablaSimple.Nombre_Valor(valor: integer): string;
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
    result := Leer_Nombre (indice)
  else
    result := '¿?';
end;

{ TTablaTitulacion }

  constructor TTablaTitulacion.Create;
    begin
      inherited Create (3);
    end;

  function TTablaTitulacion.Leer_Seccion (indice : integer) : integer;
    begin
      result := seccion[indice];
    end;

  procedure TTablaTitulacion.Cargar_Tabla (nombre_fichero : string);
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

function TTablaTitulacion.Nombre_Seccion(cen, sec: integer): string;
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
    result := Leer_Nombre (indice)
  else
    result := '¿?';
end;

{ TTablaPlan }

function TTablaPlan.Leer_Plan (indice : integer) : integer;
    begin
      result := plan[indice];
    end;

  procedure TTablaPlan.Cargar_Tabla (nombre_fichero: string);
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
        SetLength (plan, numero);
        SetLength (nombre, numero);
        codigo[numero-1] := StrToIntDef (LeftStr (linea, TAM_CENTRO), 0);
        Delete (linea, 1, TAM_CENTRO + TAM_SEPARADOR);
        seccion[numero-1] := StrToIntDef (LeftStr (linea, TAM_SECCION), 0);
        Delete (linea, 1, TAM_SECCION + TAM_SEPARADOR);
        plan[numero-1] := StrToIntDef (LeftStr (linea, TAM_PLAN), 0);
        Delete (linea, 1, TAM_PLAN + TAM_SEPARADOR);
        nombre[numero-1] := linea;
      end;
      CloseFile (fichero);
    end;

  constructor TTablaPlan.Create;
    begin
      inherited Create;
    end;

  destructor TTablaPlan.Destroy;
    begin
      inherited Destroy;
    end;

function TTablaPlan.Nombre_Plan (cen, sec, pl: integer): string;
  var
    coincidencia : boolean;
    indice : integer;
begin
  coincidencia := false;
  indice := 0;
  while (indice < numero) and not coincidencia do begin
    coincidencia := (cen  = codigo[indice]) and
                    (sec  = seccion[indice]) and
                    (pl   = plan[indice]);
      inc (indice);
  end;
  dec (indice);
  if coincidencia then
    result := Leer_Nombre (indice)
  else
    result := '¿?';
end;

{ TTablaNombreAsignatura }

  function TTablaNombreAsignatura.Leer_Asignatura (indice : integer) : integer;
    begin
      result := asignatura[indice];
    end;

  procedure TTablaNombreAsignatura.Cargar_Tabla(nombre_fichero: string);
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
        SetLength (plan, numero);
        SetLength (asignatura, numero);
        SetLength (nombre, numero);
        codigo[numero-1] := StrToIntDef (LeftStr (linea, TAM_CENTRO), 0);
        Delete (linea, 1, TAM_CENTRO + TAM_SEPARADOR);
        seccion[numero-1] := StrToIntDef (LeftStr (linea, TAM_SECCION), 0);
        Delete (linea, 1, TAM_SECCION + TAM_SEPARADOR);
        plan[numero-1] := StrToIntDef (LeftStr (linea, TAM_PLAN), 0);
        Delete (linea, 1, TAM_PLAN + TAM_SEPARADOR);
        asignatura[numero-1] := StrToIntDef (LeftStr (linea, TAM_ASIGNATURA), 0);
        Delete (linea, 1, TAM_ASIGNATURA + TAM_SEPARADOR);
        nombre[numero-1] := linea;
      end;
      CloseFile (fichero);
    end;

  constructor TTablaNombreAsignatura.Create;
    begin
      inherited Create;
    end;

  destructor TTablaNombreAsignatura.Destroy;
    begin
      inherited Destroy;
    end;

  function TTablaNombreAsignatura.Nombre_Asig (cen, sec, pl, asig : integer) : string;
    var
      coincidencia : boolean;
      indice : integer;
    begin
      coincidencia := false;
      indice := 0;
      while (indice < numero) and not coincidencia do begin
        coincidencia := (cen  = codigo[indice]) and
                        (sec  = seccion[indice]) and
                        (pl   = plan[indice]) and
                        (asig = asignatura[indice]);
        inc (indice);
      end;
      dec (indice);
      if coincidencia then
        result := Leer_Nombre (indice)
      else
        result := '¿?';
    end;

{ TTablaAsignatura }

  function TTablaAsignatura.Leer_Centro (indice : integer) : integer;
    begin
      result := centro[indice];
    end;

  function TTablaAsignatura.Leer_Plan (indice : integer) : integer;
    begin
      result := plan[indice];
    end;

  function TTablaAsignatura.Leer_Seccion (indice : integer) : integer;
    begin
      result := seccion[indice];
    end;

  function TTablaAsignatura.Leer_Asignatura (indice : integer) : integer;
    begin
      result := asignatura[indice];
    end;

  function TTablaAsignatura.Leer_Indice (indice: integer): string;
    var
      s, sol : string;
    begin
      sol := IntToStr (centro[indice]);
      while Length (sol) < 3 do
        sol := '0' + sol;

      s := IntToStr (seccion[indice]);
      if Length (s) = 1 then
        s := '0' + s;
      sol := sol + '-' + s;

      s := IntToStr (plan[indice]);
      if Length (s) = 1 then
        s := '0' + s;
      sol := sol + '-' + s;

      s := IntToStr (asignatura[indice]);
      while Length (s) < 3 do
        s := '0' + s;
      sol := sol + '-' + s;

      result := sol;
    end;

  procedure TTablaAsignatura.Insertar_Indice (s : string);
    var
      cen, sec, pl, asig : integer;
    begin
      cen  := StrToIntDef (MidStr (s,  1, 3), 0);
      sec  := StrToIntDef (MidStr (s,  5, 2), 0);
      pl   := StrToIntDef (MidStr (s,  8, 2), 0);
      asig := StrToIntDef (MidStr (s, 11, 3), 0);
      Insertar_Asignatura (cen, sec, pl, asig);
    end;

  procedure TTablaAsignatura.Insertar_Asignatura (c, s, p, a: integer);
    var
      i, indice : integer;
    begin
      indice := 0;
      while (indice < numero) and
          ((centro[indice] <  c) or
          ((centro[indice] = c) and (seccion[indice] < s)) or
          ((centro[indice] = c) and (seccion[indice] = s) and (plan[indice] < p)) or
          ((centro[indice] = c) and (seccion[indice] = s) and (plan[indice] = p) and (asignatura[indice] < a))) do
        inc (indice);
      inc (numero);
      SetLength (centro, numero);
      SetLength (seccion, numero);
      SetLength (plan, numero);
      SetLength (asignatura, numero);
      for i := indice to (numero - 2) do begin
        centro[i+1] := centro[i];
        seccion[i+1] := seccion[i];
        plan[i+1] := plan[i];
        asignatura[i+1] := asignatura[i];
      end;
      centro[indice] := c;
      seccion[indice] := s;
      plan[indice] := p;
      asignatura[indice] := a;
    end;

  procedure TTablaAsignatura.Eliminar_Asignatura (indice: integer);
    var
      i : integer;
    begin
      dec (numero);
      for i := indice to numero do begin
        centro[i] := centro[i+1];
        seccion[i] := seccion[i+1];
        plan[i] := plan[i+1];
        asignatura[i] := asignatura[i+1];
      end;
      SetLength (centro, numero);
      SetLength (seccion, numero);
      SetLength (plan, numero);
      SetLength (asignatura, numero);
    end;

  constructor TTablaAsignatura.Create;
    begin
      inherited Create;
    end;

  destructor TTablaAsignatura.Destroy;
    begin
      SetLength (centro,     0);
      SetLength (seccion,    0);
      SetLength (plan,       0);
      SetLength (asignatura, 0);
      inherited Destroy;
    end;

  procedure TTablaAsignatura.Eliminar;
    begin
      SetLength (centro,     0);
      SetLength (seccion,    0);
      SetLength (plan,       0);
      SetLength (asignatura, 0);
      numero     := 0;
    end;

{ TTablaAdaptacion }

  function TTablaAdaptacion.Leer_Centro2 (indice: integer): integer;
    begin
      result := centro2[indice];
    end;

  function TTablaAdaptacion.Leer_Seccion2 (indice: integer): integer;
    begin
      result := seccion2[indice];
    end;

  function TTablaAdaptacion.Leer_Plan2 (indice: integer): integer;
    begin
      result := plan2[indice];
    end;

  function TTablaAdaptacion.Leer_Asignatura2 (indice: integer): integer;
    begin
      result := asignatura2[indice];
    end;

  procedure TTablaAdaptacion.Cargar_Tabla (nombre_fichero: string);
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
        SetLength (centro, numero);
        SetLength (seccion, numero);
        SetLength (plan, numero);
        SetLength (asignatura, numero);
        SetLength (centro2, numero);
        SetLength (seccion2, numero);
        SetLength (plan2, numero);
        SetLength (asignatura2, numero);
        centro[numero-1] := StrToIntDef (LeftStr (linea, TAM_CENTRO), 0);
        Delete (linea, 1, TAM_CENTRO + TAM_SEPARADOR);
        seccion[numero-1] := StrToIntDef (LeftStr (linea, TAM_SECCION), 0);
        Delete (linea, 1, TAM_SECCION + TAM_SEPARADOR);
        plan[numero-1] := StrToIntDef (LeftStr (linea, TAM_PLAN), 0);
        Delete (linea, 1, TAM_PLAN + TAM_SEPARADOR);
        asignatura[numero-1] := StrToIntDef (LeftStr (linea, TAM_ASIGNATURA), 0);
        Delete (linea, 1, TAM_ASIGNATURA + TAM_SEPARADOR);
        centro2[numero-1] := StrToIntDef (LeftStr (linea, TAM_CENTRO), 0);
        Delete (linea, 1, TAM_CENTRO + TAM_SEPARADOR);
        seccion2[numero-1] := StrToIntDef (LeftStr (linea, TAM_SECCION), 0);
        Delete (linea, 1, TAM_SECCION + TAM_SEPARADOR);
        plan2[numero-1] := StrToIntDef (LeftStr (linea, TAM_PLAN), 0);
        Delete (linea, 1, TAM_PLAN + TAM_SEPARADOR);
        asignatura2[numero-1] := StrToIntDef (LeftStr (linea, TAM_ASIGNATURA), 0);
      end;
      CloseFile (fichero);
    end;

  procedure TTablaAdaptacion.Buscar (c, s, p, a : integer; var indice: integer; var coincidencia: boolean);
    var
      num : integer;
    begin
      num := Length (centro);
      coincidencia := false;
      while (indice < num) and (centro[indice] < c) do
        inc (indice);
      if (indice = num) or (centro[indice] > c) then
        exit;
      while (indice < num) and (centro[indice] = c) and (seccion[indice] < s) do
        inc (indice);
      if (indice = num) or (centro[indice] > c) or (seccion[indice] > s) then
        exit;
      while (indice < num) and (centro[indice] = c) and (seccion[indice] = s) and (plan[indice] < p) do
        inc (indice);
      if (indice = num) or (centro[indice] > c) or (seccion[indice] > s) or (plan[indice] > p) then
        exit;
      while (indice < num) and (centro[indice] = c) and (seccion[indice] = s) and (plan[indice] = p) and (asignatura[indice] < a) do
        inc (indice);
      coincidencia := (indice < num) and (centro[indice] = c) and (seccion[indice] = s) and (plan[indice] = p) and (asignatura[indice] = a);
    end;

  constructor TTablaAdaptacion.Create;
    begin
      inherited Create;
    end;

  destructor TTablaAdaptacion.Destroy;
    begin
      inherited Destroy;
    end;

{ TTablaFiltrado }

  constructor TTablaFiltrado.Create;
    var
    	i : TAtributoFiltrado;
    begin
      inherited Create;
      for i := Low (TAtributoFiltrado) to High (TAtributoFiltrado) do
      	lista_atr [i] := 0;
      lista_asig := TTablaAsignatura.Create;
    end;

  destructor TTablaFiltrado.Destroy;
    begin
      lista_asig.Free;
      inherited Destroy;
    end;

  function TTablaFiltrado.Leer (atributo : TAtributoFiltrado) : integer;
    begin
      result := lista_atr [atributo];
    end;

  procedure TTablaFiltrado.Escribir (atributo : TAtributoFiltrado; valor : integer);
    begin
      lista_atr [atributo] := valor;
    end;

  function TTablaFiltrado.Lista_Asignatura : TTablaAsignatura;
    begin
      result := lista_asig;
    end;

  procedure TTablaFiltrado.Escribir_Componentes;
    var
      i : integer;
    begin
      FormFiltroDM.ComboBoxGenero.ItemIndex     := lista_atr[naf_genero];
      FormFiltroDM.ComboBoxNacion.ItemIndex     := lista_atr[naf_nacionalidad];
      FormFiltroDM.ComboBoxProvincia.ItemIndex  := lista_atr[naf_provincia];
      FormFiltroDM.ComboBoxAcceso.ItemIndex     := lista_atr[naf_acceso];
      FormFiltroDM.ComboBoxFinalizado.ItemIndex := lista_atr[naf_finalizado];
      FormFiltroDM.ComboBoxListaAsig.ItemIndex  := lista_atr[naf_asignatura];
      FormFiltroDM.ListBoxFiltAsig.Items.Clear;
      FormFiltroDM.EditAsigAprobada.Clear;
      case lista_atr[naf_asignatura] of
        1 : begin
              FormFiltroDM.EditAsigAprobada.Text := lista_asig.Leer_Indice(0);
            end;
        2 : begin
              for i := 0 to (lista_asig.numero - 1) do
                FormFiltroDM.ListBoxFiltAsig.Items.Append (lista_asig.Leer_Indice(i));
            end;
      end;
    end;

  procedure TTablaFiltrado.Leer_Componentes;
    var
      i : integer;
    begin
      lista_atr[naf_genero]       := FormFiltroDM.ComboBoxGenero.ItemIndex;
      lista_atr[naf_nacionalidad] := FormFiltroDM.ComboBoxNacion.ItemIndex;
      lista_atr[naf_provincia]    := FormFiltroDM.ComboBoxProvincia.ItemIndex;
      lista_atr[naf_acceso]       := FormFiltroDM.ComboBoxAcceso.ItemIndex;
      lista_atr[naf_finalizado]   := FormFiltroDM.ComboBoxFinalizado.ItemIndex;
      lista_atr[naf_asignatura]   := FormFiltroDM.ComboBoxListaAsig.ItemIndex;
      lista_asig.Eliminar;
      case FormFiltroDM.ComboBoxListaAsig.ItemIndex of
        1 : begin
              lista_asig.Insertar_Indice (FormFiltroDM.EditAsigAprobada.Text);
            end;
        2 : begin
              for i := 0 to (FormFiltroDM.ListBoxFiltAsig.Count - 1) do
                lista_asig.Insertar_Indice (FormFiltroDM.ListBoxFiltAsig.Items.Strings[i]);
            end;
      end;
    end;

{ TTablaSeleccion }

  constructor TTablaSeleccion.Create;
    begin
      inherited Create;
      lista_asig := TTablaAsignatura.Create;
    end;

  destructor TTablaSeleccion.Destroy;
    begin
      lista_asig.Free;
      inherited Destroy;
    end;

  function TTablaSeleccion.Leer_Seleccion (atributo: TAtributoSeleccion): boolean;
    begin
      result := lista_atr [atributo];
    end;

  procedure TTablaSeleccion.Escribir_Seleccion (atributo: TAtributoSeleccion; valor: boolean);
    begin
      lista_atr [atributo] := valor;
    end;

  function TTablaSeleccion.Leer_Clase : TAtributoClase;
    begin
      result := clase;
    end;

  procedure TTablaSeleccion.Escribir_Clase (valor: TAtributoClase);
    begin
      clase := valor;
    end;

  function TTablaSeleccion.Lista_Asignatura : TTablaAsignatura;
    begin
      result := lista_asig;
    end;

  procedure TTablaSeleccion.Escribir_Componentes;
    var
      i : integer;
    begin
      FormFiltroDM.ComboBoxClase.ItemIndex := Integer (clase);
      for i := 0 to Integer (High (TAtributoSeleccion)) do
        FormFiltroDM.CheckListBoxSeleccion.Checked[i] := lista_atr[TAtributoSeleccion(i)];
      FormFiltroDM.ListBoxSelAsig2.Items.Clear;
      for i := 0 to lista_asig.numero - 1 do
        FormFiltroDM.ListBoxSelAsig2.Items.Append(lista_asig.Leer_Indice(i));
    end;

  procedure TTablaSeleccion.Leer_Componentes;
    var
      i : integer;
    begin
      clase := TAtributoClase (FormFiltroDM.ComboBoxClase.ItemIndex);
      for i := 0 to Integer (High (TAtributoSeleccion)) do
        lista_atr[TAtributoSeleccion(i)] := FormFiltroDM.CheckListBoxSeleccion.Checked[i];
      lista_asig.Eliminar;
      for i := 0 to (FormFiltroDM.ListBoxSelAsig2.Count - 1) do
        lista_asig.Insertar_Indice (FormFiltroDM.ListBoxSelAsig2.Items.Strings[i]);
    end;

{ TTablaConfiguracion }

  constructor TTablaConfiguracion.Create;
    begin
      inherited Create;
      filtro := TTablaFiltrado.Create;
      seleccion := TTablaSeleccion.Create;
    end;

  destructor TTablaConfiguracion.Destroy;
    begin
      filtro.Free;
      seleccion.Free;
      inherited Destroy;
    end;

  procedure TTablaConfiguracion.Cargar (nombre_fichero: string);
    var
      fichero : TextFile;
      linea : string;
      cen, sec, pl, asig, numero, i : integer;
      atr_c : TFicheroConfiguracion;
      atr_f : TAtributoFiltrado;
      atr_s : TAtributoSeleccion;
    begin
      FileMode := fmOpenRead;
      AssignFile (fichero, nombre_fichero);
      Reset (fichero);
      // Se carga la lista de ficheros de configuración
      for atr_c := Low (TFicheroConfiguracion) to High (TFicheroConfiguracion) do
        ReadLn (fichero, lista_fich[atr_c]);
      // Se cargan los datos de filtrado
      for atr_f := Low (TAtributoFiltrado) to High (TAtributoFiltrado) do
        if not SeekEof (fichero) then begin
          ReadLn (fichero, linea);
          filtro.lista_atr [atr_f] := StrToIntDef (linea, 0);
        end;
      if not SeekEof (fichero) then begin
        ReadLn (fichero, linea);
        numero := StrToIntDef (linea, 0); end
      else
        numero := 0;
      for i := 1 to numero do
        if not SeekEof (fichero) then begin
          ReadLn (fichero, linea);
          cen := StrToIntDef (LeftStr (linea, TAM_CENTRO), 0);
          Delete (linea, 1, TAM_CENTRO + TAM_SEPARADOR);
          sec := StrToIntDef (LeftStr (linea, TAM_SECCION), 0);
          Delete (linea, 1, TAM_SECCION + TAM_SEPARADOR);
          pl := StrToIntDef (LeftStr (linea, TAM_PLAN), 0);
          Delete (linea, 1, TAM_PLAN + TAM_SEPARADOR);
          asig := StrToIntDef (LeftStr (linea, TAM_ASIGNATURA), 0);
          filtro.lista_asig.Insertar_Asignatura (cen, sec, pl, asig);
        end;
      // Se cargan los datos de selección
      if not SeekEof (fichero) then begin
        ReadLn (fichero, linea);
        seleccion.clase := TAtributoClase (StrToIntDef (linea, 0));
      end;
      for atr_s := Low (TAtributoSeleccion) to High (TAtributoSeleccion) do
        if not SeekEof (fichero) then begin
          ReadLn (fichero, linea);
          seleccion.lista_atr[atr_s] := StrToBoolDef (linea, false);
        end;
      if not SeekEof (fichero) then begin
        ReadLn (fichero, linea);
        numero := StrToIntDef (linea, 0);
      end;
      for i := 1 to numero do
        if not SeekEof (fichero) then begin
          ReadLn (fichero, linea);
          cen := StrToIntDef (LeftStr (linea, TAM_CENTRO), 0);
          Delete (linea, 1, TAM_CENTRO + TAM_SEPARADOR);
          sec := StrToIntDef (LeftStr (linea, TAM_SECCION), 0);
          Delete (linea, 1, TAM_SECCION + TAM_SEPARADOR);
          pl := StrToIntDef (LeftStr (linea, TAM_PLAN), 0);
          Delete (linea, 1, TAM_PLAN + TAM_SEPARADOR);
          asig := StrToIntDef (LeftStr (linea, TAM_ASIGNATURA), 0);
          seleccion.lista_asig.Insertar_Asignatura (cen, sec, pl, asig);
        end;
      CloseFile (fichero);
    end;

  procedure TTablaConfiguracion.Guardar (nombre_fichero: string);
    var
      fichero : TextFile;
      linea, campo : string;
      atr_c : TFicheroConfiguracion;
      atr_f : TAtributoFiltrado;
      atr_s : TAtributoSeleccion;
      i, numero : integer;
    begin
      FileMode := fmOpenWrite;
      AssignFile (fichero, nombre_fichero);
      Rewrite (fichero);
      // Se guarda la lista de ficheros de configuración
      for atr_c := Low (TFicheroConfiguracion) to High (TFicheroConfiguracion) do
        WriteLn (fichero, lista_fich[atr_c]);
      // Se guardan los datos de filtrado
      for atr_f := Low (TAtributoFiltrado) to High (TAtributoFiltrado) do
				WriteLn (fichero, IntToStr (filtro.lista_atr [atr_f]));
      numero := filtro.lista_asig.Numero_Elementos;
      WriteLn (fichero, IntToStr (numero));
      for i := 0 to (numero - 1) do begin
        campo := IntToStr (filtro.lista_asig.Leer_Centro(i));
        linea := DupeString ('0', TAM_CENTRO - Length(campo)) + campo + SEPARADOR;
        campo := IntToStr (filtro.lista_asig.Leer_Seccion(i));
        linea := linea + DupeString ('0', TAM_SECCION - Length(campo)) + campo + SEPARADOR;
        campo := IntToStr (filtro.lista_asig.Leer_Plan(i));
        linea := linea + DupeString ('0', TAM_PLAN - Length(campo)) + campo + SEPARADOR;
        campo := IntToStr (filtro.lista_asig.Leer_Asignatura(i));
        linea := linea + DupeString ('0', TAM_ASIGNATURA - Length(campo)) + campo;
        WriteLn (fichero, linea);
      end;
      // Se guardan los datos de selección
      WriteLn (fichero, IntToStr (Ord (seleccion.clase)));
      for atr_s := Low (TAtributoSeleccion) to High (TAtributoSeleccion) do
        WriteLn (fichero, BoolToStr (seleccion.lista_atr[atr_s], false));
      numero := seleccion.lista_asig.Numero_Elementos;
      WriteLn (fichero, IntToStr (numero));
      for i := 0 to (numero - 1) do begin
        campo := IntToStr (seleccion.lista_asig.Leer_Centro(i));
        linea := DupeString ('0', TAM_CENTRO - Length(campo)) + campo + SEPARADOR;
        campo := IntToStr (seleccion.lista_asig.Leer_Seccion(i));
        linea := linea + DupeString ('0', TAM_SECCION - Length(campo)) + campo + SEPARADOR;
        campo := IntToStr (seleccion.lista_asig.Leer_Plan(i));
        linea := linea + DupeString ('0', TAM_PLAN - Length(campo)) + campo + SEPARADOR;
        campo := IntToStr (seleccion.lista_asig.Leer_Asignatura(i));
        linea := linea + DupeString ('0', TAM_ASIGNATURA - Length(campo)) + campo;
        WriteLn (fichero, linea);
      end;
      CloseFile (fichero);
    end;

  function TTablaConfiguracion.Tabla_Filtrado: TTablaFiltrado;
    begin
      result := filtro;
    end;

  function TTablaConfiguracion.Tabla_Seleccion: TTablaSeleccion;
    begin
      result := seleccion;
    end;

  procedure TTablaConfiguracion.Escribir_Componentes;
    begin
      FormFiltroDM.LabeledEditAlumno.Text        := lista_fich[nfc_alumno];
      FormFiltroDM.LabeledEditAcademico.Text     := lista_fich[nfc_academico];
      FormFiltroDM.LabeledEditExperiencia.Text   := lista_fich[nfc_experiencia];
      FormFiltroDM.LabeledEditAsignatura.Text    := lista_fich[nfc_asignatura];
      FormFiltroDM.LabeledEditCentro.Text        := lista_fich[nfc_centro];
      FormFiltroDM.LabeledEditCalificacion.Text  := lista_fich[nfc_calificacion];
      FormFiltroDM.LabeledEditConvocatoria.Text  := lista_fich[nfc_convocatoria];
      FormFiltroDM.LabeledEditTitulacion.Text    := lista_fich[nfc_titulacion];
      FormFiltroDM.LabeledEditNacion.Text        := lista_fich[nfc_nacion];
      FormFiltroDM.LabeledEditPlan.Text          := lista_fich[nfc_plan];
      FormFiltroDM.LabeledEditProvincia.Text     := lista_fich[nfc_provincia];
      FormFiltroDM.LabeledEditAcceso.Text        := lista_fich[nfc_acceso];
      FormFiltroDM.LabeledEditAdaptacion.Text    := lista_fich[nfc_adaptacion];
      filtro.Escribir_Componentes;
      seleccion.Escribir_Componentes;
    end;

  procedure TTablaConfiguracion.Leer_Componentes;
    begin
      lista_fich[nfc_alumno]        := FormFiltroDM.LabeledEditAlumno.Text;
      lista_fich[nfc_academico]     := FormFiltroDM.LabeledEditAcademico.Text;
      lista_fich[nfc_experiencia]   := FormFiltroDM.LabeledEditExperiencia.Text;
      lista_fich[nfc_asignatura]    := FormFiltroDM.LabeledEditAsignatura.Text;
      lista_fich[nfc_centro]        := FormFiltroDM.LabeledEditCentro.Text;
      lista_fich[nfc_calificacion]  := FormFiltroDM.LabeledEditCalificacion.Text;
      lista_fich[nfc_convocatoria]  := FormFiltroDM.LabeledEditConvocatoria.Text;
      lista_fich[nfc_titulacion]    := FormFiltroDM.LabeledEditTitulacion.Text;
      lista_fich[nfc_nacion]        := FormFiltroDM.LabeledEditNacion.Text;
      lista_fich[nfc_plan]          := FormFiltroDM.LabeledEditPlan.Text;
      lista_fich[nfc_provincia]     := FormFiltroDM.LabeledEditProvincia.Text;
      lista_fich[nfc_acceso]        := FormFiltroDM.LabeledEditAcceso.Text;
      lista_fich[nfc_adaptacion]    := FormFiltroDM.LabeledEditAdaptacion.Text;
      filtro.Leer_Componentes;
      seleccion.Leer_Componentes;
    end;
end.
