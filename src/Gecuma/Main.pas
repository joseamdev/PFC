unit Main;


interface


uses
   Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, ExtCtrls,
   ComCtrls, DateUtils, UDatos, UClaseArbol;

   
type
   TFormGecUMA = class (TForm)
      ButtonOK          : TButton;
      EditResultado     : TEdit;
      LabelResultado    : TLabel;
      PanelDatos        : TPanel;
      LabelTitulacion   : TLabel;
      LabelCP           : TLabel;
      LabelProvincia    : TLabel;
      LabelNacion       : TLabel;
      LabelCursoInicio  : TLabel;
      LabelAcceso       : TLabel;
      LabelCalificacion : TLabel;
      LabelEdadInicial  : TLabel;
      LabelGenero       : TLabel;
      EditCP            : TEdit;
      EditCursoInicio   : TEdit;
      EditCalificacion  : TEdit;
      EditEdadInicial   : TEdit;
      EditTitulacion    : TEdit;
      EditProvincia     : TEdit;
      EditNacion        : TEdit;
      EditViaAcceso     : TEdit;
      EditGenero        : TEdit;
      PanelResultados   : TPanel;
      PanelBotones      : TPanel;
    LabelEficiencia: TLabel;
      procedure FormCreate    (Sender: TObject);
      procedure ButtonOKClick (Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
   private
      { Private declarations }
   public
      { Public declarations }
   end;


var
   FormGecUMA : TFormGecUMA;
   tupla_alumno : TRegDatos;
   arbol : TArbol;



implementation


{$R *.dfm}


uses
   Dialogo1, Dialogo2, UTablas, UClaseAtributo;


var
   tabla_titulacion    : TTablaTitulacion;
   tabla_provincia     : TTablaSimple;
   tabla_nacion        : TTablaSimple;
   tabla_acceso        : TTablaSimple;





// Al abrir el programa se cargan las tablas en memoria y se actualizan los
// componentes.

procedure TFormGecUMA.FormCreate(Sender: TObject);

var
   indice, numero : integer;
   codigo1, codigo2 : string;
   lista_ordenada : TStringList;

begin

   tabla_titulacion := TTablaTitulacion.Create;
   tabla_titulacion.CargarTabla ('Tabla_titulaciones.dat');

   tabla_provincia := TTablaSimple.Create (2);
   tabla_provincia.CargarTabla ('Tabla_provincias.dat');

   tabla_nacion := TTablaSimple.Create (3);
   tabla_nacion.CargarTabla ('Tabla_nacionalidades.dat');

   tabla_acceso := TTablaSimple.Create (1);
   tabla_acceso.CargarTabla ('Tabla_vías_acceso.dat');

   numero := tabla_titulacion.NumeroElementos - 1;
   for indice := 0 to numero do begin
      Str (tabla_titulacion.LeerCodigo(indice):3, codigo1);
      Str (tabla_titulacion.LeerSeccion(indice):2, codigo2);
      codigo1 := StringReplace (codigo1, ' ', '0', [rfReplaceAll]) +
                 '-' +
                 StringReplace (codigo2, ' ', '0', [rfReplaceAll]) +
                 ': ' +
                 tabla_titulacion.LeerNombre(indice);
      DialogoTitulacion.ListBox1.Items.Append(codigo1);
   end;

   lista_ordenada := TStringList.Create;
   lista_ordenada.Sorted := true;

   numero := tabla_provincia.NumeroElementos - 1;
   for indice := 0 to numero do begin
      Str (tabla_provincia.LeerCodigo(indice):2, codigo2);
      codigo1 := tabla_provincia.LeerNombre(indice) +
                 ' [' +
                 StringReplace (codigo2, ' ', '0', [rfReplaceAll]) +
                 ']';
      lista_ordenada.Append(codigo1);
   end;
   numero := lista_ordenada.Count - 1;
   for indice := 0 to numero do
      DialogoDatos.ComboBoxProvincia.Items.Append(lista_ordenada.Strings[indice]);

   lista_ordenada.Clear;

   numero := tabla_nacion.NumeroElementos - 1;
   for indice := 1 to numero do begin
      Str (tabla_nacion.LeerCodigo(indice):3, codigo2);
      codigo1 := tabla_nacion.LeerNombre(indice) +
                 ' [' +
                 StringReplace (codigo2, ' ', '0', [rfReplaceAll]) +
                 ']';
      lista_ordenada.Append(codigo1);
   end;
   numero := lista_ordenada.Count - 1;
   for indice := 0 to numero do
      DialogoDatos.ComboBoxNacion.Items.Append(lista_ordenada.Strings[indice]);

   lista_ordenada.Clear;

   numero := tabla_acceso.NumeroElementos - 1;
   for indice := 0 to numero do begin
      Str (tabla_acceso.LeerCodigo(indice):1, codigo2);
      codigo1 := tabla_acceso.LeerNombre(indice) +
                 ' [' +
                 codigo2 +
                 ']';
      lista_ordenada.Append(codigo1);
   end;
   numero := lista_ordenada.Count - 1;
   for indice := 0 to numero do
      DialogoDatos.ComboBoxAcceso.Items.Append(lista_ordenada.Strings[indice]);

   DialogoDatos.ComboBoxGenero.Items.Append('Hombre [1]');
   DialogoDatos.ComboBoxGenero.Items.Append('Mujer [2]');

end;





procedure TFormGecUMA.ButtonOKClick (Sender : TObject);

begin
   FormGecUMA.Visible := false;
   DialogoTitulacion.Visible := true;
end;





procedure TFormGecUMA.FormClose(Sender: TObject; var Action: TCloseAction);

begin
   DialogoTitulacion.Close;
end;





procedure TFormGecUMA.FormShow(Sender: TObject);

var
   codigo : integer;

begin
   codigo := LeerDatosAlumno (tupla_alumno, atr_titulacion);
   EditTitulacion.Text := tabla_titulacion.NombreSeccion (codigo div 100, codigo mod 100);
   EditNacion.Text := tabla_nacion.NombreValor (LeerDatosAlumno (tupla_alumno, atr_nacionalidad));
   EditProvincia.Text := tabla_provincia.NombreValor (LeerDatosAlumno (tupla_alumno, atr_provincia));
   EditCP.Text := IntToStr (LeerDatosAlumno (tupla_alumno, atr_codigo_postal));
   codigo := LeerDatosAlumno (tupla_alumno, atr_genero);
   case codigo of
      1 : EditGenero.Text := 'Hombre';
      2 : EditGenero.Text := 'Mujer';
   else
      EditGenero.Text := '¿?';
   end;
   EditEdadInicial.Text := IntToStr (LeerDatosAlumno (tupla_alumno, atr_edad_inicio));
   EditCursoInicio.Text := IntToStr (LeerDatosAlumno (tupla_alumno, atr_expediente));
   EditViaAcceso.Text := tabla_acceso.NombreValor (LeerDatosAlumno (tupla_alumno, atr_acceso));
   codigo := LeerDatosAlumno (tupla_alumno, atr_calificacion);
   EditCalificacion.Text := IntToStr (codigo div 100) + '.' + IntToStr (codigo mod 100);
   EditResultado.Text := IntToStr (LeerDatosAlumno (tupla_alumno, atr_duracion));
   LabelEficiencia.Caption := IntToStr (arbol.LeerEficiencia div 10000) +
                              '.' +
                              IntToStr ((arbol.LeerEficiencia mod 10000) div 100) +
                              '%';
end;





end.
