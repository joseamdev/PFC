//  El m�dulo Main.pas es el m�dulo principal del programa.

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, CheckLst, Buttons, Menus, StrUtils,
  ShellAPI;

type
  TFormFiltroDM = class(TForm)
    PageControl1:               TPageControl;
    TabSheet1:                  TTabSheet;
    TabSheet2:                  TTabSheet;
    TabSheet3:                  TTabSheet;
    OpenDialog1:                TOpenDialog;
    SaveDialog1:                TSaveDialog;
    CheckListBoxSeleccion:      TCheckListBox;
    ComboBoxClase:              TComboBox;
    LabelClase:                 TLabel;
    LabelAtributos:             TLabel;
    ComboBoxGenero:             TComboBox;
    ComboBoxNacion:             TComboBox;
    ComboBoxProvincia:          TComboBox;
    ComboBoxAcceso:             TComboBox;
    ComboBoxTitulacion:         TComboBox;
    ComboBoxFinalizado:         TComboBox;
    ListBoxSelAsig1:            TListBox;
    ListBoxSelAsig2:            TListBox;
    LabelAsignaturas:           TLabel;
    LabelTitulacion:            TLabel;
    GroupBoxFiltAsig:           TGroupBox;
    ComboBoxCentro:             TComboBox;
    ComboBoxSeccion:            TComboBox;
    ComboBoxPlan:               TComboBox;
    ComboBoxAsignatura:         TComboBox;
    ListBoxFiltAsig:            TListBox;
    ButtonFiltInsertar:         TButton;
    ButtonFiltEliminar:         TButton;
    ButtonSelInsertar:          TButton;
    ButtonSelEliminar:          TButton;
    LabelGenero:                TLabel;
    LabelNacion:                TLabel;
    LabelProvincia:             TLabel;
    LabelAcceso:                TLabel;
    LabelFinalizado:            TLabel;
    LabeledEditConfiguracion:   TLabeledEdit;
    LabeledEditAlumno:          TLabeledEdit;
    LabeledEditAcademico:       TLabeledEdit;
    LabeledEditExperiencia:     TLabeledEdit;
    LabeledEditAsignatura:      TLabeledEdit;
    LabeledEditCentro:          TLabeledEdit;
    LabeledEditCalificacion:    TLabeledEdit;
    LabeledEditConvocatoria:    TLabeledEdit;
    LabeledEditTitulacion:      TLabeledEdit;
    LabeledEditNacion:          TLabeledEdit;
    LabeledEditPlan:            TLabeledEdit;
    LabeledEditProvincia:       TLabeledEdit;
    LabeledEditAcceso:          TLabeledEdit;
    LabeledEditAdaptacion:      TLabeledEdit;
    ButtonAbrirConfiguracion:   TButton;
    ButtonGuardarConfiguracion: TButton;
    ButtonAlumno:               TButton;
    ButtonAcademico:            TButton;
    ButtonExperiencia:          TButton;
    ButtonAdaptacion:           TButton;
    ButtonAsignatura:           TButton;
    ButtonCentro:               TButton;
    ButtonTitulacion:           TButton;
    ButtonPlan:                 TButton;
    ButtonAcceso:               TButton;
    ButtonCalificacion:         TButton;
    ButtonConvocatoria:         TButton;
    ButtonNacion:               TButton;
    ButtonProvincia:            TButton;
    ComboBoxListaAsig:          TComboBox;
    LabelAsignatura:            TLabel;
    EditAsigAprobada:           TEdit;
    LabelAsignaturaAprobada:    TLabel;
    LabelAsignaturasCursadas:   TLabel;
    Button1:                    TButton;
    Button2:                    TButton;
    procedure FormCreate                      (Sender: TObject);
    procedure FormDestroy                     (Sender: TObject);
    procedure Ejecutar1Click                  (Sender: TObject);
    procedure ComboBoxCentroSelect            (Sender: TObject);
    procedure ComboBoxSeccionSelect           (Sender: TObject);
    procedure ComboBoxPlanSelect              (Sender: TObject);
    procedure ComboBoxTitulacionSelect        (Sender: TObject);
    procedure ButtonSelInsertarClick          (Sender: TObject);
    procedure ButtonSelEliminarClick          (Sender: TObject);
    procedure ComboBoxListaAsigSelect         (Sender: TObject);
    procedure ButtonFiltInsertarClick         (Sender: TObject);
    procedure ButtonFiltEliminarClick         (Sender: TObject);
    procedure ButtonClick                     (Sender: TObject);
    procedure CargarConfiguracion;
    procedure ButtonGuardarConfiguracionClick (Sender: TObject);
    procedure ButtonExperienciaClick          (Sender: TObject);
    procedure CheckListBoxSeleccionClick      (Sender: TObject);
    procedure Escribir_Nombres_Lista_Filt;
    procedure Escribir_Nombres_Lista_Sel;
    procedure Button2Click                    (Sender: TObject);
    procedure Eliminar_Atributo_Repetido;
    procedure LiberarMemoria;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormFiltroDM: TFormFiltroDM;



implementation

{$R *.dfm}

uses
  Tablas, Lectura, Compactar, Experiencias, Alumnos, Asignaturas, Escritura, Progreso;


//  Se hace uso de once variables globales. En ellas se almacenan los datos de
//  configuraci�n y las tablas con las cadenas de texto correspondientes a
//  diversos datos acad�micos.

var
  tabla_configuracion : TTablaConfiguracion;
  tabla_centro        : TTablaSimple;
  tabla_titulacion    : TTablaTitulacion;
  tabla_asignatura    : TTablaNombreAsignatura;
  tabla_plan          : TTablaPlan;
  tabla_adaptacion    : TTablaAdaptacion;
  tabla_calificacion  : TTablaSimple;
  tabla_convocatoria  : TTablaSimple;
  tabla_provincia     : TTablaSimple;
  tabla_nacion        : TTablaSimple;
  tabla_acceso        : TTablaSimple;



procedure TFormFiltroDM.LiberarMemoria;
begin
   SetProcessWorkingSetSize(GetCurrentProcess, $FFFFFFFF, $FFFFFFFF);
end;


//  El procedimiento 'CargarConfiguracion' crea los objetos 'tabla' que son
//  globales al m�dulo a partir de los datos de configuraci�n.
procedure TFormFiltroDM.CargarConfiguracion;

var
  i, num : integer;
  s, s2  : string;

begin
  // Se leen los datos de configuraci�n del disco.
  tabla_configuracion := TTablaConfiguracion.Create;
  tabla_configuracion.Cargar (LabeledEditConfiguracion.Text);
  // Se actualizan los componentes.
  tabla_configuracion.Escribir_Componentes;
  // Se crea la tabla con los nombres de los centros de la UMA.
  tabla_centro := TTablaSimple.Create (3);
  tabla_centro.Cargar_Tabla(LabeledEditCentro.Text);
  //  En el componente ComboBoxCentro se cargan los textos de la tabla junto con
  //  el c�digo para facilitar la identificaci�n del centro.
  num := tabla_centro.Numero_Elementos;
  FormFiltroDM.ComboBoxCentro.Clear;
  for i := 0 to (num - 1) do begin
    Str (tabla_centro.Leer_Codigo(i):3, s);
    s := StringReplace (s, ' ', '0', [rfReplaceAll]) + ': ' + tabla_centro.Leer_Nombre(i);
    FormFiltroDM.ComboBoxCentro.Items.Append(s);
  end;
  // Se crea la tabla con los nombres de las titulaciones de la UMA.
  tabla_titulacion := TTablaTitulacion.Create;
  tabla_titulacion.Cargar_Tabla (LabeledEditTitulacion.Text);
  //  En el componente 'ComboBoxTitulacion' se cargan los textos de la tabla
  //  junto con los c�digos y secci�n.
  num := tabla_titulacion.Numero_Elementos - 1;
  for i := 0 to num do begin
    Str (tabla_titulacion.Leer_Codigo(i):3, s);
    Str (tabla_titulacion.Leer_Seccion(i):2, s2);
    s :=  StringReplace (s, ' ', '0', [rfReplaceAll]) +
          '-' +
          StringReplace (s2, ' ', '0', [rfReplaceAll]) +
          ': ' +
          tabla_titulacion.Leer_Nombre(i);
    FormFiltroDM.ComboBoxTitulacion.Items.Append(s);
  end;
  //  Se crean las tablas con los nombres de los planes de estudio, los
  //  nombres de las asignaturas, los cuadros de adaptaci�n, las calificaciones,
  //  las convocatorias, los nombres de las provincias, los nombres de los pa�ses,
  //  y los tipos de acceso a la universidad.
  tabla_plan := TTablaPlan.Create;
  tabla_plan.Cargar_Tabla(LabeledEditPlan.Text);
  tabla_asignatura := TTablaNombreAsignatura.Create;
  tabla_asignatura.Cargar_Tabla (LabeledEditAsignatura.Text);
  tabla_adaptacion := TTablaAdaptacion.Create;
  tabla_adaptacion.Cargar_Tabla (LabeledEditAdaptacion.Text);
  tabla_calificacion := TTablaSimple.Create (1);
  tabla_calificacion.Cargar_Tabla (LabeledEditCalificacion.Text);
  tabla_convocatoria := TTablaSimple.Create (1);
  tabla_convocatoria.Cargar_Tabla (LabeledEditConvocatoria.Text);
  tabla_provincia := TTablaSimple.Create (2);
  tabla_provincia.Cargar_Tabla (LabeledEditProvincia.Text);
  tabla_nacion := TTablaSimple.Create (3);
  tabla_nacion.Cargar_Tabla (LabeledEditNacion.Text);
  tabla_acceso := TTablaSimple.Create (1);
  tabla_acceso.Cargar_Tabla (LabeledEditAcceso.Text);
  //  Se marca la opci�n de filtrado por asignatura en 'ComboBoxListaAsig'.
  ComboBoxListaAsigSelect (ComboBoxListaAsig);
  //  Se marcan los atributos seleccionados en 'CheckListBoxSeleccion'.
  CheckListBoxSeleccionClick (CheckListBoxSeleccion);
  //  Por �ltimo se llama a los procedimientos 'Escribir_Nombres_Lista_Filt' y
  //  'Escribir_Nombres_Lista_Sel' que escriben los nombres de las asignaturas
  //  filtradas y las asignaturas seleccionadas en los componentes
  //  'ListBoxFiltAsig' y 'ListBoxSelAsig2' respectivamente.
  Escribir_Nombres_Lista_Filt;
  Escribir_Nombres_Lista_Sel;
end;





// El procedimiento FormCreate carga la configuraci�n por defecto mediante una
//  llamada al procedimiento CargarConfiguracion.

procedure TFormFiltroDM.FormCreate(Sender: TObject);
begin
  CargarConfiguracion;
end;





//  El procedimiento FormDestroy se encarga de liberar de memoria, justo antes
//  de cerrar la aplicaci�n, las variables globales, que no son m�s que las
//  tablas con las cadenas de texto y configuraci�n del programa.

procedure TFormFiltroDM.FormDestroy(Sender: TObject);
begin
  tabla_configuracion.Free;
  tabla_centro.Free;
  tabla_titulacion.Free;
  tabla_asignatura.Free;
  tabla_plan.Free;
  tabla_adaptacion.Free;
  tabla_calificacion.Free;
  tabla_convocatoria.Free;
  tabla_provincia.Free;
  tabla_nacion.Free;
  tabla_acceso.Free;
end;





//  El procedimiento 'EjecutarClick' realiza toda la labor principal del
//  programa, desde la apertura de ficheros, filtrado y por �ltimo la escritura
//  de los ficheros de salida.

procedure TFormFiltroDM.Ejecutar1Click (Sender: TObject);

var
  lista_experiencias : TListaExperiencia;
  fich_alumno, fich_asig, fich_exp, fich_temp : TextFile;
  buffer_alumno, buffer_asignatura, nombre : string;
  error, fin : boolean;
  contador, contador_total, codigo_alumno : integer;
  lista_valores : TListaValoresAtributos;
  tam_alumno, tam_asig, pos_alumno, pos_asig : integer;

begin
  FormProgreso.Gauge1.Progress := 0;
  FormProgreso.Visible := true;
  FormFiltroDM.Enabled := false;
  //  En primer lugar se reinician las variables locales (el contador, las
  //  variables de control fin y error, y las l�neas de texto auxiliares).
  fin                := false;
  buffer_alumno      := '';
  buffer_asignatura  := '';
  contador           := 0;
  contador_total     := 0;
  codigo_alumno      := 0;
  //  Se crea el objeto que contendr� la lista de experiencias.
  lista_experiencias := TListaExperiencia.Create;
  //  A partir de las componentes de la aplicaci�n se actualiza la configuraci�n
  //  del programa.
  tabla_configuracion.Leer_Componentes;
  //  Se elimina el atributo de clase repetido si lo hubiera.
  Eliminar_Atributo_Repetido;
  //  Se abren los ficheros de datos de alumnos y asignaturas.
  Lectura.Inicializar (LabeledEditAlumno.Text, LabeledEditAcademico.Text, fich_alumno, fich_asig, error);
  if error then
    ShowMessage ('ERROR: Fallo al abrir los ficheros de datos')
  else begin
    error :=  (ExtractFileDir (LabeledEditExperiencia.Text) <> '') and
              not DirectoryExists (ExtractFileDir (LabeledEditExperiencia.Text));
    if error then
      ShowMessage ('ERROR: Fallo al crear el fichero de experiencias');
  end;
  //  Si hubo un error de apertura de ficheros salta un mensaje de aviso y se
  //  termina el procedimiento.
  if not error then begin
    //  Una vez abiertos los ficheros, se asigna a la variable nombre el nombre
    //  del fichero en el que se guardar�n las experiencias generadas.
    nombre := LabeledEditExperiencia.Text;
    tam_alumno := FileSize (fich_alumno);
    tam_asig := FileSize (fich_asig);
    //  Se crea la cabecera del fichero de experiencias.
    Escritura.CrearFicheros (nombre, fich_exp, fich_temp, lista_valores);
    //  Mientras haya datos en los ficheros de entrada (asignaturas y alumnos)
    //  se van compactando y filtrando la informaci�n.
    while not fin do begin
      Lectura.Principal (fich_alumno, fich_asig, lista_experiencias, buffer_alumno, buffer_asignatura, codigo_alumno, fin);
      LiberarMemoria;
      //  Antes de guardar las experiencias en fichero de salida se comprueba
      //  que la lista no est� vac�a.
      pos_alumno := FilePos (fich_alumno);
      pos_asig := FilePos (fich_asig);
      FormProgreso.Gauge1.Progress := (100 * (pos_alumno + pos_asig)) div (tam_asig + tam_alumno);
      if lista_experiencias.Numero_Elementos > 0 then begin
        Compactar.Principal (tabla_adaptacion, lista_experiencias);
        Escritura.EscribirLineaDatos (fich_temp, lista_experiencias, tabla_configuracion.Tabla_Seleccion, tabla_configuracion.Tabla_Filtrado, contador, lista_valores);
        inc (contador_total);
      end;
    end;
    Escritura.EscribirCabecera (fich_exp, tabla_configuracion.Tabla_Seleccion, tabla_configuracion.Tabla_Filtrado, lista_valores);
    Escritura.PasarDatos (fich_exp, fich_temp);
    LiberarMemoria;
    //  Una vez que se llega al final del fichero, se cierran los ficheros de
    //  entrada (alumnos y asignaturas) y de salida (experiencias).
    Lectura.Finalizar (fich_alumno, fich_asig);
    Escritura.CerrarFicheros (fich_exp, fich_temp, lista_valores);
    //  Se imprime un mensaje que indica que ha concluido el proceso y el n�mero
    //  de experiencias generadas.
    FormProgreso.Gauge1.Progress := 100;
    ShowMessage ('_____Proceso terminado_____' + chr(13) + chr(13) + 'Experiencias analizadas: ' + IntToStr (contador_total) + chr(13) + 'Experiencias guardadas: ' + IntToStr (contador));
  end;
  lista_experiencias.Free;
  FormFiltroDM.Enabled := true;
  FormProgreso.Visible := false;
  LiberarMemoria;
end;





//  El procedimiento 'ComboBoxCentroSelect' carga en el componente
//  'ComboBoxSeccion' las titulaciones pertenecientes al centro seleccionado en
//  'ComboBoxCentro'.

procedure TFormFiltroDM.ComboBoxCentroSelect(Sender: TObject);

var
  i, cod, num : integer;
  cod_str, tit_str : string;
  l : TStringList;

begin
  //  Se crea una lista de cadenas de texto en la variable 'l'. En 'num' se
  //  almacena el tope del bucle que ser� el n�mero de elementos de
  //  'tabla_titulacion' menos uno.

  l := TStringList.Create;
  num := tabla_titulacion.Numero_Elementos - 1;
  //  Se extraen los tres caracteres de la izquierda del centro seleccionado,
  //  que forman el c�digo del centro, y se almacena en 'cod_str'.
  cod_str := LeftStr (ComboBoxCentro.Text, 3);
  //  El texto se pasa a entero y se guarda en la variable 'cod'.
  cod := StrToInt (cod_str);
  //  La variable 'cod_str' se concatena con la cadena de texto � - � que
  //  servir� de separador con el c�digo de secci�n.
  cod_str := cod_str + '-';
  //  Se recorre la lista de titulaciones mediante un bucle, si la titulaci�n
  //  pertenece al centro seleccionado, se lee su c�digo, se sustituyen los
  //  espacios por ceros, se a�ade la cadena �: �, se concatena con el nombre de
  //  la titulaci�n y se a�ade a la lista de cadenas 'l'.
  for i := 0 to num do begin
    if (tabla_titulacion.Leer_Codigo(i) = cod) then begin
      Str (tabla_titulacion.Leer_Seccion(i):2, tit_str);
      tit_str := StringReplace (tit_str, ' ', '0', [rfReplaceAll]) + ': ';
      l.Append (cod_str +
                tit_str +
                tabla_titulacion.Leer_Nombre(i));
    end;
  end;
  //  Una vez terminado, todas las titulaciones del centro seleccionado estar�n
  //  almacenadas en la variable 'l'.
  //  Se eliminan los elementos de 'ComboBoxPlan', 'ComboBoxAsignatura' y
  //  'ComboBoxSeccion' que contienen los datos de titulaciones, planes y
  //  asignaturas de una selecci�n anterior.
  ComboBoxPlan.Items.Clear;
  ComboBoxAsignatura.Items.Clear;
  ComboBoxSeccion.Items.Clear;
  //  Por �ltimo, se carga la nueva lista de titulaciones en 'ComboBoxSeccion' y
  //  se libera de memoria la variable 'l'.
  ComboBoxSeccion.Items.AddStrings(l);
  l.Free;
end;





//  El procedimiento 'ComboBoxSeccionSelect' carga en el componente
//  'ComboBoxPlan' los planes de estudio pertenecientes a la titulaci�n
//  seleccionada en 'ComboBoxSeccion'.

procedure TFormFiltroDM.ComboBoxSeccionSelect(Sender: TObject);

var
  i, cod, tit, num : integer;
  cod_str, tit_str, pl_str : string;
  l : TStringList;

begin
  //  Se crea una lista de cadenas de texto en la variable 'l'.
  l := TStringList.Create;
  //  En 'num' se almacena el tope del bucle que ser� el n�mero de elementos de
  //  'tabla_plan' menos uno.
  num := tabla_plan.Numero_Elementos - 1;
  //  Se extraen los tres caracteres de la izquierda del nombre de la titulaci�n
  //  seleccionada, que forman el c�digo del centro, y se almacena en 'cod_str'.
  cod_str := LeftStr (ComboBoxSeccion.Text, 3);
  //  Se extraen dos caracteres desde la quinta posici�n del nombre de la
  //  titulaci�n, que forman el c�digo de la titulaci�n, y se almacena en
  //  'tit_str'.
  tit_str := MidStr (ComboBoxSeccion.Text, 5, 2);
  //  Las variables de texto se pasan a entero y se guardan en la variable 'cod'
  //  y 'tit' respectivamente.
  cod := StrToInt (cod_str);
  tit := StrToInt (tit_str);
  //  La variable 'cod_str' se concatena con la cadena de texto � - � m�s la
  //  variable 'tit_str' y el separador � - � que servir�n de cabecera para el
  //  c�digo del plan.
  cod_str := cod_str + '-' + tit_str + '-';
  //  Se recorre la lista de planes mediante un bucle, si el plan de estudios
  //  pertenece a la titulaci�n  seleccionada, se lee su c�digo, se sustituyen
  //  los espacios por ceros, se a�ade la cadena �: �, se concatena con el
  //  nombre del plan y se a�ade a la lista de cadenas 'l'.
  for i := 0 to num do begin
    if (tabla_plan.Leer_Codigo(i) = cod) and (tabla_plan.Leer_Seccion(i) = tit) then begin
      Str (tabla_plan.Leer_Plan(i):2, pl_str);
      pl_str := StringReplace (pl_str, ' ', '0', [rfReplaceAll]) + ': ';
      l.Append (cod_str +
                pl_str +
                tabla_plan.Leer_Nombre(i));
    end;
  end;
  //  Una vez terminado, todos los planes de estudio de la titulaci�n
  //  seleccionada estar�n almacenados en la variable 'l'.
  //  Se eliminan los elementos de 'ComboBoxPlan' y 'ComboBoxAsignatura' que
  //  contienen los datos de planes y asignaturas de una selecci�n anterior.
  ComboBoxAsignatura.Items.Clear;
  ComboBoxPlan.Items.Clear;
  //  Por �ltimo, se carga la nueva lista de planes de estudio en 'ComboBoxPlan'
  //  y se libera de memoria la variable 'l'.
  ComboBoxPlan.Items.AddStrings(l);
  l.Free;
end;





//  El procedimiento 'ComboBoxPlanSelect' carga en el componente
//  'ComboBoxAsignatura' las asignaturas pertenecientes al plan de estudios
//  seleccionado en 'ComboBoxPlan'.

procedure TFormFiltroDM.ComboBoxPlanSelect(Sender: TObject);

var
  i, cod, tit, pl, num : integer;
  cod_str, tit_str, pl_str, asig_str : string;
  l : TStringList;

begin
  //  Se crea una lista de cadenas de texto en la variable 'l'.
  l := TStringList.Create;
  //  En 'num' se almacena el tope del bucle que ser� el n�mero de elementos de
  //  'tabla_asignatura' menos uno.
  num := tabla_asignatura.Numero_Elementos - 1;
  //  Se extraen los tres caracteres de la izquierda del plan de estudios
  //  seleccionado, que forman el c�digo del centro, y se almacena en 'cod_str'.
  cod_str := LeftStr (ComboBoxPlan.Text, 3);
  //  Se extraen dos caracteres, desde la quinta posici�n, del nombre del plan
  //  de estudios, que forman el c�digo de la titulaci�n, y se almacena en
  //  'tit_str'.
  tit_str := MidStr (ComboBoxPlan.Text, 5, 2);
  //  Se extraen dos caracteres, desde la octava posici�n, del plan
  //  seleccionado, que forman el c�digo del plan de estudios, y se almacena en
  //  'pl_str'.
  pl_str  := MidStr (ComboBoxPlan.Text, 8, 2);
  //  Los c�digos en formato texto se pasan a su valor entero y se guardan en
  //  las variables 'cod', 'tit' y 'pl' respectivamente.
  cod := StrToInt (cod_str);
  tit := StrToInt (tit_str);
  pl  := StrToInt (pl_str);
  //  La variable 'cod_str' se concatena con la cadena de texto � - � m�s la
  //  variable 'tit_str', otro separador � - �, la variable 'pl_str' y un �ltimo
  //  separador � - �; el resultado se almacena en 'cod_str' y servir� de
  //  cabecera para el c�digo de la asignatura.
  cod_str := cod_str + '-' + tit_str + '-' + pl_str + '-';
  //  Se recorre la lista de asignaturas mediante un bucle, si la asignatura
  //  pertenece al plan de estudios seleccionado, se lee su c�digo, se
  //  sustituyen los espacios por ceros, se a�ade la cadena �: �, se concatena
  //  con el nombre de la titulaci�n y se a�ade a la lista de cadenas 'l'.
  for i := 0 to num do begin
    if (tabla_asignatura.Leer_Codigo(i) = cod) and (tabla_asignatura.Leer_Seccion(i) = tit) and (tabla_asignatura.Leer_Plan(i) = pl) then begin
      Str (tabla_asignatura.Leer_Asignatura(i):3, asig_str);
      asig_str := StringReplace (asig_str, ' ', '0', [rfReplaceAll]) + ': ';
      l.Append (cod_str +
                asig_str +
                tabla_asignatura.Leer_Nombre(i));
    end;
  end;
  //  Una vez terminado, todas las asignaturas del plan de estudios seleccionado
  //  estar�n almacenadas en la variable 'l'.
  //  Se eliminan los elementos de 'ComboBoxAsignatura' que contiene los datos
  //  de asignaturas de una selecci�n anterior.
  ComboBoxAsignatura.Items.Clear;
  //  Por �ltimo, se carga la nueva lista de asignaturas en 'ComboBoxAsignatura'
  //  y se libera de memoria la variable 'l'.
  ComboBoxAsignatura.Items.AddStrings(l);
  l.Free;
end;





//  El procedimiento 'ComboBoxTitulacionSelect' carga en el componente
//  'ListBoxSelAsig1' la lista de asignaturas de la titulaci�n seleccionada.

procedure TFormFiltroDM.ComboBoxTitulacionSelect (Sender: TObject);

var
  i, cod, tit, num : integer;
  cod_str, tit_str, pl_str, asig_str : string;

begin
  //  Se eliminan los datos de 'ListBoxSelAsig1'.
  ListBoxSelAsig1.Items.Clear;
  //  En 'num' se almacena el n�mero de asignaturas menos uno para usarlo como
  //  valor final del bucle.
  num     := tabla_asignatura.Numero_Elementos - 1;
  //  En 'cod_str' y 'tit_str' se almacenan el c�digo del centro y de la secci�n
  //  en formato texto extra�dos del componente 'ComboBoxTitulacion'.
  cod_str := LeftStr (ComboBoxTitulacion.Text, 3);
  tit_str := MidStr (ComboBoxTitulacion.Text, 5, 2);
  //  A las variables 'cod' y 'tit' se les asigna el valor num�rico de 'cod_str'
  //  y 'tit_str'.
  cod     := StrToInt (cod_str);
  tit     := StrToInt (tit_str);
  //  La variable 'cod_str' se concatena con el car�cter �-� m�s la variable
  //  'tit_str' y se termina con otro car�cter �-�; el resultado se almacena en
  //  la propia variable 'cod_str'.
  cod_str := cod_str + '-' + tit_str + '-';
  //  Una vez terminada la asignaci�n de valores a la variables se inicia el
  //  bucle que recorre los elementos de la 'tabla tabla_asignatura'. Si el
  //  c�digo del centro y la secci�n de la asignatura analizada coinciden con
  //  los seleccionados (almacenados en 'cod' y 'tit') se procede a leer el
  //  c�digo del plan y asignatura y se guardan en formato texto en 'pl_str' y
  //  'asig_str'. Posteriormente en estas variables se reemplazan los caracteres
  //  en blanco por ceros y se concatenan separados por un gui�n.
  //  Por �ltimo se completa con el c�digo del centro y la titulaci�n, el nombre
  //  de la asignatura y se a�ade el resultado a 'ListBoxSelAsig1'.
  for i := 0 to num do begin
    if (tabla_asignatura.Leer_Codigo(i) = cod) and (tabla_asignatura.Leer_Seccion(i) = tit) then begin
      Str (tabla_asignatura.Leer_Plan(i):2, pl_str);
      Str (tabla_asignatura.Leer_Asignatura(i):3, asig_str);
      pl_str   := StringReplace (pl_str, ' ', '0', [rfReplaceAll]) + '-';
      asig_str := pl_str + StringReplace (asig_str, ' ', '0', [rfReplaceAll]) + ': ';
      ListBoxSelAsig1.Items.Append (cod_str +
                                    asig_str +
                                    tabla_asignatura.Leer_Nombre(i));
    end;
  end;
end;





//  El procedimiento 'ButtonSelInsertarClick' se activa cuando se intenta
//  insertar una asignatura de 'ListBoxSelAsig1' a 'ListBoxSelAsig2'.

procedure TFormFiltroDM.ButtonSelInsertarClick (Sender: TObject);

var
  numero, indice : integer;
  seleccionado, repetido : boolean;
  cadena : string;

begin
  //  En la variable 'numero' se almacena el n�mero de elementos de
  //  'ListBoxSelAsig1' menos uno, la cual se usar� como valor final del bucle.
  numero := ListBoxSelAsig1.Count - 1;  //  La lista 'ListBoxSelAsig1' se recorre mediante un bucle. En la variable
  //  booleana seleccionado se guarda si la asignatura est� marcada; en repetido
  //  si el elemento se encuentra en 'ListBoxSelAsig2'. Si est� marcado y no
  //  est� repetido se a�ade a 'ListBoxSelAsig2'.
  for indice := 0 to numero do
  begin
    seleccionado := ListBoxSelAsig1.Selected [indice];
    cadena := ListBoxSelAsig1.Items[indice];
    repetido := ListBoxSelAsig2.Items.IndexOf (cadena) >= 0;
    if seleccionado and not repetido then
      ListBoxSelAsig2.Items.Append(cadena);
  end;
end;





//  El procedimiento 'ButtonSelEliminarClick' borra de la lista
//  'ListBoxSelAsig2' el elemento seleccionado cuando se pulsa el bot�n
//  eliminar.

procedure TFormFiltroDM.ButtonSelEliminarClick (Sender: TObject);

begin
  ListBoxSelAsig2.DeleteSelected;
end;





//  El procedimiento 'ComboBoxListaAsigSelect' se encarga de activar o
//  desactivar componentes seg�n la opci�n seleccionada en 'ComboBoxListaAsig'.

procedure TFormFiltroDM.ComboBoxListaAsigSelect (Sender: TObject);

begin
  case ComboBoxListaAsig.ItemIndex of
    //  Si la opci�n seleccionada es �todas las asignaturas� (caso 0), todos los
    //  componentes se desactivan ya que no habr� filtrado alguno.
    0 : begin
          EditAsigAprobada.Enabled   := false;
          ListBoxFiltAsig.Enabled    := false;
          GroupBoxFiltAsig.Enabled   := false;
          ButtonFiltEliminar.Enabled := false;
          ButtonFiltInsertar.Enabled := false;
          ComboBoxAsignatura.Enabled := false;
          ComboBoxCentro.Enabled     := false;
          ComboBoxPlan.Enabled       := false;
          ComboBoxSeccion.Enabled    := false;
        end;
    //  En caso de seleccionar �asignatura aprobada� (case 1) se activan todos
    //  los componentes excepto 'ListBoxFiltAsig'.
    1 : begin
          EditAsigAprobada.Enabled   := true;
          ListBoxFiltAsig.Enabled    := false;
          GroupBoxFiltAsig.Enabled   := true;
          ButtonFiltEliminar.Enabled := true;
          ButtonFiltInsertar.Enabled := true;
          ComboBoxAsignatura.Enabled := true;
          ComboBoxCentro.Enabled     := true;
          ComboBoxPlan.Enabled       := true;
          ComboBoxSeccion.Enabled    := true;
        end;
    //  Por �ltimo, si se elige la opci�n �asignaturas cursadas� (caso 2) se
    //  activan todos los componentes excepto 'EditAsigAprobada'.
    2 : begin
          EditAsigAprobada.Enabled   := false;
          ListBoxFiltAsig.Enabled    := true;
          GroupBoxFiltAsig.Enabled   := true;
          ButtonFiltEliminar.Enabled := true;
          ButtonFiltInsertar.Enabled := true;
          ComboBoxAsignatura.Enabled := true;
          ComboBoxCentro.Enabled     := true;
          ComboBoxPlan.Enabled       := true;
          ComboBoxSeccion.Enabled    := true;
        end;
  end;
end;





//  El procedimiento 'ButtonFiltInsertarClick' a�ade la asignatura seleccionada
//  a 'EditAsigAprobada' o 'ListBoxFiltAsig' seg�n sea la opci�n seleccionada en
//  'ComboBoxListaAsig'.

procedure TFormFiltroDM.ButtonFiltInsertarClick (Sender: TObject);

begin
  case ComboBoxListaAsig.ItemIndex of
    //  En caso de que el elemento seleccionado en 'ComboBoxListaAsig' sea
    //  �asignatura aprobada� (caso 1) se carga en el cuadro de texto
    //  'EditAsigAprobada' la asignatura seleccionada en 'ComboBoxAsignatura'.
    1 : begin
          EditAsigAprobada.Text := ComboBoxAsignatura.Items.Strings [ComboBoxAsignatura.ItemIndex];
        end;
    //  En caso de ser �asignaturas cursadas� (case 2) se dan cuatro
    //  posibilidades; seg�n si los componentes 'ComboBox' est�n vac�os o no.
    2 : begin
          //  Si ComboBoxAsignatura contiene una asignatura, se comprueba que no
          //  est� ya contenida en 'ListBoxFiltAsig'; en caso negativo se a�ade
          //  la asignatura seleccionada.
          if ComboBoxAsignatura.ItemIndex > -1 then begin
            if ListBoxFiltAsig.Items.IndexOf (ComboBoxAsignatura.Items.Strings [ComboBoxAsignatura.ItemIndex]) < 0 then
              ListBoxFiltAsig.Items.Append (ComboBoxAsignatura.Items.Strings [ComboBoxAsignatura.ItemIndex]);
          end
          //  Si no hay seleccionada una asignatura pero s� un plan de estudios
          //  de una titulaci�n, se comprueba que no est� en 'ListBoxFiltAsig';
          //  en caso negativo se a�ade el plan seleccionado.
          else if ComboBoxPlan.ItemIndex > - 1 then begin
            if ListBoxFiltAsig.Items.IndexOf (ComboBoxPlan.Items.Strings [ComboBoxPlan.ItemIndex]) < 0 then
              ListBoxFiltAsig.Items.Append (ComboBoxPlan.Items.Strings [ComboBoxPlan.ItemIndex]);
          end
          //  Si hay seleccionada una titulaci�n en 'ComboBoxSeccion' pero no
          //  hay ni plan ni asignatura seleccionada entonces se comprueba si ya
          //  est� en ListBosFiltAsig, si no es as� se a�ade al componente.
          else if ComboBoxSeccion.ItemIndex > - 1 then begin
            if ListBoxFiltAsig.Items.IndexOf (ComboBoxSeccion.Items.Strings [ComboBoxSeccion.ItemIndex]) < 0 then
              ListBoxFiltAsig.Items.Append (ComboBoxSeccion.Items.Strings [ComboBoxSeccion.ItemIndex]);
          end
          //  Por �ltimo, si hay seleccionado un centro en 'ComboBoxCentro' pero
          //  no hay selecci�n en los tres componentes anteriores (secci�n, plan,
          //  asignatura) entonces se comprueba si ya existe en 'ListBoxFiltAsig',
          //  en caso de que no, se a�ade a la lista.
          else if ComboBoxCentro.ItemIndex > - 1 then begin
            if ListBoxFiltAsig.Items.IndexOf (ComboBoxCentro.Items.Strings [ComboBoxCentro.ItemIndex]) < 0 then
              ListBoxFiltAsig.Items.Append (ComboBoxCentro.Items.Strings [ComboBoxCentro.ItemIndex]);
          end;
        end;
  end;
end;





//  El procedimiento 'ButtonFiltEliminarClick' limpia el contenido del
//  componente seleccionado.
//  Si 'ComboBoxListaAsig' tiene seleccionada la opci�n �asignatura aprobada�
//  (caso 1) entonces se borra el texto de 'EditAsigAprobada'.
//  Si en ComboBoxListAsig est� seleccionada la opci�n �asignaturas cursadas�
//  (caso 2) entonces se borra el contenido de la lista 'ListBoxFiltAsig'.

procedure TFormFiltroDM.ButtonFiltEliminarClick (Sender: TObject);

begin
  case ComboBoxListaAsig.ItemIndex of
    1 : EditAsigAprobada.Clear;
    2 : ListBoxFiltAsig.DeleteSelected;
  end;
end;





//  El procedimiento 'ButtonClick' se encarga de abrir el di�logo de apertura de
//  fichero seg�n el bot�n �Abrir� que se puls�.

procedure TFormFiltroDM.ButtonClick (Sender: TObject);

begin
  //  Si el bot�n pulsado es 'ButtonAbrirConfiguraci�n' entonces la extensi�n
  //  por defecto ser� �.cfg� y se filtrar�n todos los archivos que no tengan
  //  esa extensi�n.
  if Sender = ButtonAbrirConfiguracion then begin
    OpenDialog1.DefaultExt := 'cfg';
    OpenDialog1.Filter := 'Archivos de configuraci�n (*.CFG)|*.CFG';
  end
  //  En otro caso, la extensi�n por defecto utilizada ser� �.dat� y se
  //  filtrar�n todos los archivos excepto aquellos que tengan dicha extensi�n.
  else begin
    OpenDialog1.DefaultExt := 'dat';
    OpenDialog1.Filter := 'Archivos de datos (*.DAT)|*.DAT';
  end;
  //  Cuando se pulsa el bot�n se abre el componente di�logo de apertura de
  //  archivo. Si el fichero existe y no hubo error al abrir se contin�an las
  //  comprobaciones. Seg�n sea el bot�n pulsado se carga en el cuadro de texto
  //  correspondiente el nombre del fichero seleccionado en el di�logo.
  //  Adem�s, en  el caso del fichero de configuraci�n se llama al procedimiento
  //  'CargarConfiguracion' para actualizar los componentes. Si el fichero no
  //  existe se muestra un mensaje de error.
  if OpenDialog1.Execute then
    if FileExists (OpenDialog1.FileName) then begin
      if Sender = ButtonAbrirConfiguracion then begin
        LabeledEditConfiguracion.Text := OpenDialog1.FileName;
        CargarConfiguracion;
      end
      else if Sender = ButtonAlumno        then   LabeledEditAlumno.Text        := OpenDialog1.FileName
      else if Sender = ButtonAcademico     then   LabeledEditAcademico.Text     := OpenDialog1.FileName
      else if Sender = ButtonAdaptacion    then   LabeledEditAdaptacion.Text    := OpenDialog1.FileName
      else if Sender = ButtonAsignatura    then   LabeledEditAsignatura.Text    := OpenDialog1.FileName
      else if Sender = ButtonCentro        then   LabeledEditCentro.Text        := OpenDialog1.FileName
      else if Sender = ButtonTitulacion    then   LabeledEditTitulacion.Text    := OpenDialog1.FileName
      else if Sender = ButtonPlan          then   LabeledEditPlan.Text          := OpenDialog1.FileName
      else if Sender = ButtonAcceso        then   LabeledEditAcceso.Text        := OpenDialog1.FileName
      else if Sender = ButtonCalificacion  then   LabeledEditCalificacion.Text  := OpenDialog1.FileName
      else if Sender = ButtonConvocatoria  then   LabeledEditConvocatoria.Text  := OpenDialog1.FileName
      else if Sender = ButtonNacion        then   LabeledEditNacion.Text        := OpenDialog1.FileName
      else if Sender = ButtonProvincia     then   LabeledEditProvincia.Text     := OpenDialog1.FileName;
    end
    else
      ShowMessage ('Fichero no existe');
end;





//  El procedimiento 'ButtonGuardarConfiguracion' guarda el estado de los
//  componentes en el fichero seleccionado.

procedure TFormFiltroDM.ButtonGuardarConfiguracionClick (Sender: TObject);

begin
  //  Primero se selecciona la extensi�n por defecto �.cfg� y el filtro de
  //  archivos �*.cfg�. Si no produce un error al abrir el di�logo se leen los
  //  componentes y se actualizan las tablas en tabla_configuracion. Despu�s se
  //  guarda el contenido de la misma en el fichero seleccionado. El nombre del
  //  fichero se actualiza en el cuadro de texto 'LabeledEditConfiguracion'.
  SaveDialog1.DefaultExt := 'cfg';
  SaveDialog1.Filter := 'Archivos de configuraci�n (*.CFG)|*.cfg';
  if SaveDialog1.Execute then begin
    tabla_configuracion.Leer_Componentes;
    tabla_configuracion.Guardar (SaveDialog1.FileName);
    LabeledEditConfiguracion.Text := SaveDialog1.FileName;
  end;
end;





//  El procedimiento 'ButtonExperienciaClick' selecciona el nombre del fichero
//  de salida.

procedure TFormFiltroDM.ButtonExperienciaClick( Sender: TObject);

begin
  //  En primer lugar se selecciona la extensi�n por defecto �.arff� y el filtro
  //  de fichero �*.arff�. Se llama al procedimiento 'SaveDialog'. Si la
  //  ejecuci�n del di�logo fue correcta, se comprueba la extensi�n en
  //  min�sculas; si coincide se carga el nombre del fichero en
  //  'LabeledEditExperiencia'. Si no coincide se le a�ade la extensi�n e
  //  igualmente se carga el nombre en el componente mencionado anteriormente.
  SaveDialog1.DefaultExt := 'arff';
  SaveDialog1.Filter := 'Ficheros de experiencias de WEKA (*.ARFF)|*.arff';
  if SaveDialog1.Execute then begin
    if LowerCase (ExtractFileExt (SaveDialog1.FileName)) = '.arff' then
      LabeledEditExperiencia.Text := SaveDialog1.FileName
    else
      LabeledEditExperiencia.Text := SaveDialog1.FileName + '.arff';
  end;
end;





//  El procedimiento 'CheckListBoxSeleccionClick' completa el estado de los
//  atributos de selecci�n de las asignaturas y habilita en consonancia los
//  componentes seleccionados.

procedure TFormFiltroDM.CheckListBoxSeleccionClick(Sender: TObject);

begin
  //  En primer lugar se comprueba si est�n marcadas las casillas de
  //  'CheckListBoxSeleccionClick' correspondientes a �calificaci�n de la
  //  asignatura�, �curso de la asignatura�, �convocatoria de la asignatura�,
  //  �n�mero de convocatorias de la asignatura� y �n�mero de matriculas de la
  //  asignatura�.
  //  Si no est� marcada ninguna de las casillas entonces se desactivan los
  //  componentes.
  if not CheckListBoxSeleccion.Checked[20] and
     not CheckListBoxSeleccion.Checked[21] and
     not CheckListBoxSeleccion.Checked[22] and
     not CheckListBoxSeleccion.Checked[23] and
     not CheckListBoxSeleccion.Checked[24] then begin
        ComboBoxTitulacion.Enabled  := false;
        ListBoxSelAsig1.Enabled     := false;
        ButtonSelInsertar.Enabled   := false;
        ButtonSelEliminar.Enabled   := false;
        ListBoxSelAsig2.Enabled     := false;
  end
  //  Si alguna de las casillas est� marcada entonces se activan todos los
  //  componentes anteriores.
  else begin
        ComboBoxTitulacion.Enabled  := true;
        ListBoxSelAsig1.Enabled     := true;
        ButtonSelInsertar.Enabled   := true;
        ButtonSelEliminar.Enabled   := true;
        ListBoxSelAsig2.Enabled     := true;
  end;
end;





//  El procedimiento 'Escribir_Nombres_Lista_Filt' completa el nombre de la
//  asignatura, plan de estudios, titulaci�n y centro a partir del c�digo en
//  'ListaBoxFiltAsig'.

procedure TFormFiltroDM.Escribir_Nombres_Lista_Filt;

var
  numero, indice, centro, seccion, plan, asignatura : integer;
  s : string;

begin
  //  Primero se guarda en la variable numero el n�mero de elementos de
  //  'ListBoxFiltAsig' menos uno; se utilizar� como valor final del bucle.
  numero := ListBoxFiltAsig.Items.Count - 1;
  //  El bucle recorre la lista de elementos, lee el contenido y lo separa del
  //  siguiente modo: los tres primeros caracteres se almacenan en la variable
  //  centro, en seccion se guardan los dos caracteres que comienzan en la
  //  posici�n cinco de la cadena de texto, en plan se guardan otros dos
  //  caracteres leyendo desde la posici�n ocho, y en asignatura se guardan
  //  los tres caracteres que hay desde la posici�n once.
  for indice := 0 to numero do begin
    s := ListBoxFiltAsig.Items.Strings[indice];
    centro     := StrToIntDef (MidStr (s,  1, 3), 0);
    seccion    := StrToIntDef (MidStr (s,  5, 2), 0);
    plan       := StrToIntDef (MidStr (s,  8, 2), 0);
    asignatura := StrToIntDef (MidStr (s, 11, 3), 0);
    //  Estos n�meros en formato texto se convierten a su valor num�rico para
    //  poder tratar la informaci�n.
    //  Si asignatura no est� vac�o significa que la l�nea se corresponde a una
    //  asignatura; se busca el nombre en tabla_asignatura y se a�ade a la
    //  variable de texto s junto con el separador �: � que ir� entre el c�digo
    //  y el nombre. Despu�s esta variable se carga en la l�nea del componente
    //  que est� indexado en es momento.
    if asignatura > 0 then
      s := s + ': ' + tabla_asignatura.Nombre_Asig(centro, seccion, plan, asignatura)
    //  Al igual que se hace con la asignatura se act�a con plan, seccion y
    //  centro; se comprueba que no est� vac�o para ver el tipo de dato con el
    //  que se va a trabajar, se busca el nombre en la tabla correspondiente,
    //  se recorta la parte del c�digo que no corresponda, se incluye el
    //  separador �: � y se concatena con el nombre.
    else if plan > 0 then
      s := LeftStr (s, 9) + ': ' + tabla_plan.Nombre_Plan (centro, seccion, plan)
    else if seccion > 0 then
      s := LeftStr (s, 6) + ': ' + tabla_titulacion.Nombre_Seccion (centro, seccion)
    else
      s := LeftStr (s, 3) + ': ' + tabla_centro.Nombre_Valor (centro);
    //  Por �ltimo se sobrescribe la l�nea de 'ListBoxFiltAsig'.
    ListBoxFiltAsig.Items.Strings[indice] := s;
  end;
end;





//  El procedimiento 'Escribir_Nombres_Lista_Sel' lee el c�digo de las
//  asignaturas contenidas en 'ListBoxSelAsig2' y les a�ade el nombre.

procedure TFormFiltroDM.Escribir_Nombres_Lista_Sel;

var
  numero, indice, centro, seccion, plan, asignatura : integer;
  s : string;

begin
  //  Primero se almacena en la variable numero el n�mero de elementos de
  //  'ListBoxSelAsig2' y se le resta uno. Este valor se utiliza como valor
  //  final en el bucle que recorrer� la lista.
  numero := ListBoxSelAsig2.Items.Count - 1;
  //  El bucle empieza leyendo el elemento indicado de la lista y se asigna a la
  //  cadena s. �sta se utiliza para extraer los c�digos del centro, secci�n,
  //  plan y asignatura; luego se pasan a valor entero. Las posiciones de la
  //  cadena s est�n definidas del siguiente modo: las posiciones del uno al
  //  tres pertenecen al centro, cinco y seis pertenecen a la secci�n, las ocho
  //  y nueve al plan de estudios, y de la once a la trece a la asignatura.
  for indice := 0 to numero do begin
    s := ListBoxSelAsig2.Items.Strings[indice];
    centro     := StrToIntDef (MidStr (s,  1, 3), 0);
    seccion    := StrToIntDef (MidStr (s,  5, 2), 0);
    plan       := StrToIntDef (MidStr (s,  8, 2), 0);
    asignatura := StrToIntDef (MidStr (s, 11, 3), 0);
    //  Con el c�digo completo de la asignatura se busca el nombre en la tabla
    //  'tabla_asignatura'. �ste se concatena con el c�digo y un separador (�: �).
    s := s + ': ' + tabla_asignatura.Nombre_Asig(centro, seccion, plan, asignatura);
    //  La cadena resultante machaca la existente en 'ListBoxSelAsig2'.
    ListBoxSelAsig2.Items.Strings[indice] := s;
  end;
end;





//  El procedimiento 'Button2Click' abre el fichero de ayuda en HTML mediante
//  una llamada al programa por defecto.

procedure TFormFiltroDM.Button2Click(Sender: TObject);

begin
  ShellExecute (Handle,'open',pChar('ayuda.html'),nil,'.',SW_SHOW);
end;





//  El procedimiento 'Eliminar_Atributo_Repetido' se encarga de eliminar el
//  atributo de la experiencia si coincide con el de la clase. En otro caso
//  no hace nada.

procedure TFormFiltroDM.Eliminar_Atributo_Repetido;

begin
  case tabla_configuracion.Tabla_Seleccion.Leer_Clase of
    nac_codigo_postal : tabla_configuracion.Tabla_Seleccion.Escribir_Seleccion (nas_codigo_postal,  false);
    nac_provincia     : tabla_configuracion.Tabla_Seleccion.Escribir_Seleccion (nas_provincia,      false);
    nac_genero        : tabla_configuracion.Tabla_Seleccion.Escribir_Seleccion (nas_genero,         false);
    nac_nacionalidad  : tabla_configuracion.Tabla_Seleccion.Escribir_Seleccion (nas_nacionalidad,   false);
    nac_acceso        : tabla_configuracion.Tabla_Seleccion.Escribir_Seleccion (nas_acceso,         false);
    nac_duracion      : tabla_configuracion.Tabla_Seleccion.Escribir_Seleccion (nas_duracion,       false);
    nac_titulacion    : tabla_configuracion.Tabla_Seleccion.Escribir_Seleccion (nas_titulacion,     false);
  end;
end;


end.
