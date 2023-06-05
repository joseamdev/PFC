//  El módulo 'Experiencias' se encarga del manejo de los objetos
//  'TExperienciaAsignatura', 'TListaExperienciaAsignatura', 'TExperiencia',
//  'TListaExperiecia', encargados de almacenar la información de las
//  experiencias con los datos de los alumnos y asignaturas.

unit Experiencias;


interface

  uses
    Controls;

  type
    TExperienciaAtributo = (ne_centro,
                            ne_seccion,
                            ne_plan,
                            ne_especialidad,
                            ne_edad_inicial,
                            ne_edad_final,
                            ne_codigo_postal,
                            ne_provincia,
                            ne_genero,
                            ne_nacionalidad,
                            ne_acceso,
                            ne_calificacion,
                            ne_curso_fin,
                            ne_duracion_carrera,
                            ne_num_aprobados,
                            ne_num_notables,
                            ne_num_sobresalientes,
                            ne_num_matriculas_honor,
                            ne_num_suspensos,
                            ne_num_no_presentados,
                            ne_nota_media,
                            ne_titulacion,
                            ne_localidad,
                            ne_nacimiento,
                            ne_expediente);

    TExperienciaAtributoEntero = ne_centro..ne_titulacion;

    TExperienciaAsignaturaAtributo = (nea_centro,
                                      nea_seccion,
                                      nea_plan,
                                      nea_asignatura,
                                      nea_calificacion,
                                      nea_curso,
                                      nea_convocatoria,
                                      nea_num_convocatorias,
                                      nea_num_matriculas);

    TExperienciaAsignatura = class
      private
        lista_atr : array [TExperienciaAsignaturaAtributo] of integer;
      public
        constructor Create; overload;
        constructor Create (cen, sec, pl, asig, cal, cur, conv, nconv, nmat : integer); overload;
        destructor  Destroy; override;
        function    Leer (atributo : TExperienciaAsignaturaAtributo) : integer;
        procedure   Escribir (atributo : TExperienciaAsignaturaAtributo; valor : integer);
    end;

    TListaExperienciaAsignatura = class
      private
        numero : integer;
        lista : array of TExperienciaAsignatura;
      public
        constructor Create;
        destructor Destroy; override;
        procedure Buscar (centro : integer; var indice: integer; var coincidencia : boolean); overload;
        procedure Buscar (centro, seccion : integer; var indice: integer; var coincidencia : boolean); overload;
        procedure Buscar (centro, seccion, plan : integer; var indice: integer; var coincidencia : boolean); overload;
        procedure Buscar (centro, seccion, plan, asignatura : integer; var indice: integer; var coincidencia : boolean); overload;
        procedure Insertar (cen, sec, pl, asig, cal, cur, conv, nconv, nmat : integer); overload;
        procedure Insertar (asig : TExperienciaAsignatura); overload;
        procedure Eliminar (indice : integer);
        function  Asignatura (indice : integer) : TExperienciaAsignatura;
        function  Numero_Elementos : integer;
        function  Clonar : TListaExperienciaAsignatura;
    end;

    TExperiencia = class
      private
        lista_atr  : array [TExperienciaAtributoEntero] of integer;
        nacimiento : TDate;
        localidad  : string;
        expediente : TDate;
        lista_asig : TListaExperienciaAsignatura;
      public
        constructor Create; overload;
        constructor Create (cen, sec, pl, esp, cp, pr, gen, nac, acc, cal, fin,
                            dur, eini, efin, nap, nnot, nsob, nmh, nsus, nnp, nm,
                            tit : integer; fnac, exp : TDate; loc : string); overload;
        destructor  Destroy; override;
        procedure   Leer (atributo : TExperienciaAtributo; var valor : integer); overload;
        procedure   Leer (atributo : TExperienciaAtributo; var valor : string); overload;
        procedure   Leer (atributo : TExperienciaAtributo; var valor : TDate); overload;
        procedure   Escribir (atributo :TExperienciaAtributo; valor : integer); overload;
        procedure   Escribir (atributo :TExperienciaAtributo; valor : string); overload;
        procedure   Escribir (atributo :TExperienciaAtributo; valor : TDate); overload;
        function    Lista_Asignatura : TListaExperienciaAsignatura;
    end;

    TListaExperiencia = class
      private
        numero : integer;
        lista : array of TExperiencia;
      public
        constructor Create;
        destructor Destroy; override;
        procedure Buscar (centro, seccion : integer; var indice: integer; var coincidencia : boolean);
        procedure Insertar (cen, sec, pl, esp, cp, pr, gen, nac, acc, cal, fin,
          dur, eini, efin, nap, nnot, nsob, nmh, nsus, nnp, nm, tit : integer;
          fnac, exp : TDate; loc : string); overload;
        procedure Insertar (exp : TExperiencia); overload;
        procedure Eliminar (indice : integer);
        function Experiencia (indice : integer) : TExperiencia;
        function Numero_Elementos : integer;
        function Clonar : TListaExperiencia;
    end;



implementation





{ TExperiencia }





constructor TExperiencia.Create;

var
 	i : TExperienciaAtributoEntero;

begin
  inherited Create;
  for i := Low (TExperienciaAtributoEntero) to High (TExperienciaAtributoEntero) do
   	lista_atr[i] := 0;
  nacimiento := 0;
  localidad := '';
  expediente := 0;
  lista_asig := TListaExperienciaAsignatura.Create;
end;





constructor TExperiencia.Create(cen, sec, pl, esp, cp, pr, gen, nac, acc, cal, fin, dur, eini, efin, nap, nnot, nsob, nmh, nsus, nnp, nm, tit: integer; fnac, exp: TDate; loc: string);

begin
  inherited Create;
  lista_atr [ne_centro]               := cen;
  lista_atr [ne_seccion]              := sec;
  lista_atr [ne_plan]                 := pl;
  lista_atr [ne_especialidad]         := esp;
  lista_atr [ne_codigo_postal]        := cp;
  lista_atr [ne_provincia]            := pr;
  lista_atr [ne_genero]               := gen;
  lista_atr [ne_nacionalidad]         := nac;
  lista_atr [ne_acceso]               := acc;
  lista_atr [ne_calificacion]         := cal;
  lista_atr [ne_curso_fin]            := fin;
  lista_atr [ne_duracion_carrera]     := dur;
  lista_atr [ne_edad_inicial]         := eini;
  lista_atr [ne_edad_final]           := efin;
  lista_atr [ne_num_aprobados]        := nap;
  lista_atr [ne_num_notables]         := nnot;
  lista_atr [ne_num_sobresalientes]   := nsob;
  lista_atr [ne_num_matriculas_honor] := nmh;
  lista_atr [ne_num_suspensos]        := nsus;
  lista_atr [ne_num_no_presentados]   := nnp;
  lista_atr [ne_nota_media]           := nm;
  lista_atr [ne_titulacion]           := tit;
  nacimiento                          := fnac;
  expediente                          := exp;
  localidad                           := loc;
  lista_asig := TListaExperienciaAsignatura.Create;
end;





destructor TExperiencia.Destroy;

begin
  lista_asig.Free;
  inherited Destroy;
end;





procedure TExperiencia.Leer (atributo : TExperienciaAtributo; var valor : integer);

begin
  valor := lista_atr [atributo];
end;





procedure TExperiencia.Leer (atributo : TExperienciaAtributo; var valor : string);

begin
  case atributo of
    ne_localidad : valor := localidad;
  end;
end;





procedure TExperiencia.Leer (atributo:  TExperienciaAtributo; var valor : TDate);

begin
  case atributo of
    ne_nacimiento : valor := nacimiento;
    ne_expediente : valor := expediente;
  end;
end;





procedure TExperiencia.Escribir (atributo : TExperienciaAtributo; valor : integer);

begin
  lista_atr[atributo] := valor;
end;





procedure TExperiencia.Escribir (atributo: TExperienciaAtributo; valor: string);

begin
  case atributo of
    ne_localidad : localidad := valor;
  end;
end;





procedure TExperiencia.Escribir (atributo: TExperienciaAtributo; valor: TDate);

begin
  case atributo of
    ne_nacimiento : nacimiento := valor;
    ne_expediente : expediente := valor;
  end;
end;





function TExperiencia.Lista_Asignatura : TListaExperienciaAsignatura;

begin
  result := lista_asig;
end;





{ TListaExperiencia }





constructor TListaExperiencia.Create;

begin
  inherited Create;
  SetLength (lista, 0);
  numero := 0;
end;





destructor TListaExperiencia.Destroy;

var
  i : integer;

begin
  for i := 0 to (numero - 1) do
    lista[i].Free;
  SetLength (lista, 0);
  inherited Destroy;
end;





procedure TListaExperiencia.Buscar (centro, seccion : integer; var indice: integer; var coincidencia : boolean);

begin
  coincidencia := false;
  while (indice < numero) and (not coincidencia) do begin
    coincidencia := (centro = lista[indice].lista_atr[ne_centro]) and
                    (seccion = lista[indice].lista_atr[ne_seccion]);
    inc (indice);
  end;
  if coincidencia then
    dec (indice);
end;





procedure TListaExperiencia.Insertar (cen, sec, pl, esp, cp, pr, gen, nac, acc, cal, fin,
          dur, eini, efin, nap, nnot, nsob, nmh, nsus, nnp, nm, tit : integer;
          fnac, exp : TDate; loc : string);

begin
  inc (numero);
  SetLength (lista, numero);
  lista[numero - 1] := TExperiencia.Create (cen, sec, pl, esp, cp, pr, gen, nac, acc, cal,
                                            fin, dur, eini, efin, nap, nnot, nsob, nmh,
                                            nsus, nnp, nm, tit, fnac, exp, loc);
end;





procedure TListaExperiencia.Insertar (exp : TExperiencia);

begin
  inc (numero);
  SetLength (lista, numero);
  lista[numero - 1] := TExperiencia.Create (
                              exp.lista_atr[ne_centro],
                              exp.lista_atr[ne_seccion],
                              exp.lista_atr[ne_plan],
                              exp.lista_atr[ne_especialidad],
                              exp.lista_atr[ne_codigo_postal],
                              exp.lista_atr[ne_provincia],
                              exp.lista_atr[ne_genero],
                              exp.lista_atr[ne_nacionalidad],
                              exp.lista_atr[ne_acceso],
                              exp.lista_atr[ne_calificacion],
                              exp.lista_atr[ne_curso_fin],
                              exp.lista_atr[ne_duracion_carrera],
                              exp.lista_atr[ne_edad_inicial],
                              exp.lista_atr[ne_edad_final],
                              exp.lista_atr[ne_num_aprobados],
                              exp.lista_atr[ne_num_notables],
                              exp.lista_atr[ne_num_sobresalientes],
                              exp.lista_atr[ne_num_matriculas_honor],
                              exp.lista_atr[ne_num_suspensos],
                              exp.lista_atr[ne_num_no_presentados],
                              exp.lista_atr[ne_nota_media],
                              exp.lista_atr[ne_titulacion],
                              exp.nacimiento,
                              exp.expediente,
                              exp.localidad);
  lista[numero - 1].lista_asig := exp.Lista_Asignatura.Clonar;
end;





procedure TListaExperiencia.Eliminar (indice : integer);

var
  i : integer;

begin
  for i := indice to (numero - 2) do
    lista[i] := lista[i+1];
  SetLength (lista, numero - 1);
  dec (numero);
end;





function TListaExperiencia.Experiencia (indice : integer): TExperiencia;

begin
  if (lista <> NIL) and (indice < numero) then
    result := lista [indice]
  else
    result := NIL;
end;





function TListaExperiencia.Numero_Elementos : integer;

begin
  result := numero;
end;





function TListaExperiencia.Clonar : TListaExperiencia;

var
  copia : TListaExperiencia;
  i : integer;

begin
  copia := TListaExperiencia.Create;
  for i := 0 to (numero - 1) do begin
    copia.Insertar (lista[i].lista_atr[ne_centro],
                    lista[i].lista_atr[ne_seccion],
                    lista[i].lista_atr[ne_plan],
                    lista[i].lista_atr[ne_especialidad],
                    lista[i].lista_atr[ne_codigo_postal],
                    lista[i].lista_atr[ne_provincia],
                    lista[i].lista_atr[ne_genero],
                    lista[i].lista_atr[ne_nacionalidad],
                    lista[i].lista_atr[ne_acceso],
                    lista[i].lista_atr[ne_calificacion],
                    lista[i].lista_atr[ne_curso_fin],
                    lista[i].lista_atr[ne_duracion_carrera],
                    lista[i].lista_atr[ne_edad_inicial],
                    lista[i].lista_atr[ne_edad_final],
                    lista[i].lista_atr[ne_num_aprobados],
                    lista[i].lista_atr[ne_num_notables],
                    lista[i].lista_atr[ne_num_sobresalientes],
                    lista[i].lista_atr[ne_num_matriculas_honor],
                    lista[i].lista_atr[ne_num_suspensos],
                    lista[i].lista_atr[ne_num_no_presentados],
                    lista[i].lista_atr[ne_nota_media],
                    lista[i].lista_atr[ne_titulacion],
                    lista[i].nacimiento,
                    lista[i].expediente,
                    lista[i].localidad);
    copia.Experiencia(i).lista_asig := self.Experiencia(i).Lista_Asignatura.Clonar;
  end;
  result := copia;
end;





{ TExperienciaAsignatura }





constructor TExperienciaAsignatura.Create;

var
  i : TExperienciaAsignaturaAtributo;

begin
  inherited Create;
  for i := Low (TExperienciaAsignaturaAtributo) to High (TExperienciaAsignaturaAtributo) do
    lista_atr[i] := 0;
end;





constructor TExperienciaAsignatura.Create (cen, sec, pl, asig, cal, cur, conv, nconv, nmat : integer);

begin
  inherited Create;
  lista_atr[nea_centro]             := cen;
  lista_atr[nea_seccion]            := sec;
  lista_atr[nea_plan]               := pl;
  lista_atr[nea_asignatura]         := asig;
  lista_atr[nea_calificacion]       := cal;
  lista_atr[nea_curso]              := cur;
  lista_atr[nea_convocatoria]       := conv;
  lista_atr[nea_num_convocatorias]  := nconv;
  lista_atr[nea_num_matriculas]     := nmat;
end;





destructor TExperienciaAsignatura.Destroy;

begin
  inherited Destroy;
end;





function TExperienciaAsignatura.Leer (atributo : TExperienciaAsignaturaAtributo) : integer;

begin
  result := lista_atr[atributo];
end;





procedure TExperienciaAsignatura.Escribir (atributo : TExperienciaAsignaturaAtributo; valor : integer);

begin
  lista_atr[atributo] := valor;
end;





{ TListaExperienciaAsignatura }





constructor TListaExperienciaAsignatura.Create;

begin
  inherited Create;
  SetLength (lista, 0);
  numero := 0;
end;





destructor TListaExperienciaAsignatura.Destroy;

var
  i : integer;

begin
  for i := 0 to (numero - 1) do
    lista[i].Free;
  SetLength (lista, 0);
  inherited Destroy;
end;





procedure TListaExperienciaAsignatura.Buscar (centro : integer; var indice : integer; var coincidencia : boolean);

begin
  coincidencia := false;
  while (indice < numero) and not coincidencia do begin
    coincidencia := (centro = lista[indice].lista_atr[nea_centro]);
    inc (indice);
  end;
  if coincidencia then
    dec (indice);
end;





procedure TListaExperienciaAsignatura.Buscar (centro, seccion : integer; var indice : integer; var coincidencia : boolean);

begin
  coincidencia := false;
  while (indice < numero) and (not coincidencia) do begin
    coincidencia := (centro = lista[indice].lista_atr[nea_centro]) and (seccion = lista[indice].lista_atr[nea_seccion]);
    inc (indice);
  end;
  if coincidencia then
    dec (indice);
end;





procedure TListaExperienciaAsignatura.Buscar (centro, seccion, plan : integer; var indice : integer; var coincidencia : boolean);

begin
  coincidencia := false;
  while (indice < numero) and not coincidencia do begin
    coincidencia := (centro = lista[indice].lista_atr[nea_centro]) and
                    (seccion = lista[indice].lista_atr[nea_seccion]) and
                    (plan = lista[indice].lista_atr[nea_plan]);
    inc (indice);
  end;
  if coincidencia then
    dec (indice);
end;





procedure TListaExperienciaAsignatura.Buscar (centro, seccion, plan, asignatura : integer; var indice : integer; var coincidencia : boolean);

begin
  coincidencia := false;
  while ((indice < numero) and (not coincidencia)) do begin
    coincidencia := (centro     = lista[indice].lista_atr[nea_centro])    and
                    (seccion    = lista[indice].lista_atr[nea_seccion])   and
                    (plan       = lista[indice].lista_atr[nea_plan])      and
                    (asignatura = lista[indice].lista_atr[nea_asignatura]);
    inc (indice);
  end;
  if coincidencia then
    dec (indice);
end;





procedure TListaExperienciaAsignatura.Insertar (cen, sec, pl, asig, cal, cur, conv, nconv, nmat : integer);

begin
  inc (numero);
  SetLength (lista, numero);
  lista[numero-1] := TExperienciaAsignatura.Create (cen, sec, pl, asig, cal, cur, conv, nconv, nmat);
end;





procedure TListaExperienciaAsignatura.Insertar (asig : TExperienciaAsignatura);

begin
  inc (numero);
  SetLength (lista, numero);
  lista[numero-1] := TExperienciaAsignatura.Create (asig.lista_atr[nea_centro],
                                                    asig.lista_atr[nea_seccion],
                                                    asig.lista_atr[nea_plan],
                                                    asig.lista_atr[nea_asignatura],
                                                    asig.lista_atr[nea_calificacion],
                                                    asig.lista_atr[nea_curso],
                                                    asig.lista_atr[nea_convocatoria],
                                                    asig.lista_atr[nea_num_convocatorias],
                                                    asig.lista_atr[nea_num_matriculas]);
end;





procedure TListaExperienciaAsignatura.Eliminar (indice : integer);

var
  i : integer;

begin
  dec (numero);
  for i := indice to (numero - 1) do
    lista[i] := lista[i + 1];
  SetLength (lista, numero);
end;





function TListaExperienciaAsignatura.Asignatura (indice: integer): TExperienciaAsignatura;

begin
  result := lista[indice];
end;





  function TListaExperienciaAsignatura.Numero_Elementos: integer;
    begin
      result := numero;
    end;

  function TListaExperienciaAsignatura.Clonar : TListaExperienciaAsignatura;
    var
      i : integer;
      copia : TListaExperienciaAsignatura;
    begin
      copia := TListaExperienciaAsignatura.Create;
      for i := 0 to (numero - 1) do begin
        copia.Insertar (lista[i].lista_atr[nea_centro],
                        lista[i].lista_atr[nea_seccion],
                        lista[i].lista_atr[nea_plan],
                        lista[i].lista_atr[nea_asignatura],
                        lista[i].lista_atr[nea_calificacion],
                        lista[i].lista_atr[nea_curso],
                        lista[i].lista_atr[nea_convocatoria],
                        lista[i].lista_atr[nea_num_convocatorias],
                        lista[i].lista_atr[nea_num_matriculas]);
      end;
      result := copia;
    end;

end.
