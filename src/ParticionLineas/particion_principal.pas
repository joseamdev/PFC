unit particion_principal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Math, StrUtils;

type
  TFormularioParticion = class(TForm)
    DialogoAbrir: TOpenDialog;
    DialogoGuardar: TSaveDialog;
    Editor_Lineas: TEdit;
    Editor_Guardar: TEdit;
    Boton_Abrir: TButton;
    Boton_Guardar: TButton;
    Boton_Aceptar: TButton;
    Editor_Abrir: TEdit;
    Boton_Cancelar: TButton;
    Label_Lineas: TLabel;
    RadioButton_partir: TRadioButton;
    RadioButton_unir: TRadioButton;
    Edit_num: TEdit;
    Label_num: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Boton_AbrirClick(Sender: TObject);
    procedure Boton_GuardarClick(Sender: TObject);
    procedure Boton_CancelarClick(Sender: TObject);
    procedure Boton_AceptarClick(Sender: TObject);
    procedure RadioButton_partirClick(Sender: TObject);
    procedure RadioButton_unirClick(Sender: TObject);
  private
    procedure PartirArchivo (origen, destino : string; numero : integer);
    procedure UnirArchivo (origen, destino : string);
    function FicheroDestino (nombre : string; numero : integer) : string;
    function Log10Integer (numero : integer) : integer;
  public
    { Public declarations }
  end;

var
  FormularioParticion: TFormularioParticion;

implementation

{$R *.dfm}

//
// Expande el nombre del fichero destino al inicio del programa
//
procedure TFormularioParticion.FormCreate(Sender: TObject);
begin
  Editor_Guardar.Text := ExpandFileName ('FicheroSalida.dat');
end;

//
// Abre una ventana diálogo para el fichero origen
//
procedure TFormularioParticion.Boton_AbrirClick(Sender: TObject);
begin
  if DialogoAbrir.Execute then
    Editor_Abrir.Text := DialogoAbrir.FileName;
end;

//
// Abre una ventana diálogo para el fichero destino
//
procedure TFormularioParticion.Boton_GuardarClick(Sender: TObject);
begin
  if DialogoGuardar.Execute then
    Editor_Guardar.Text := DialogoGuardar.FileName;
end;

//
// Activa el editor de núm. de líneas si se activa la opción 'Fragmentar'
//
procedure TFormularioParticion.RadioButton_partirClick(Sender: TObject);
begin
  Label_Lineas.Enabled := true;
  Editor_Lineas.Enabled := true;
end;

//
// Desactiva el editor de núm. de líneas si se activa la opción 'Ensamblar'
//
procedure TFormularioParticion.RadioButton_unirClick(Sender: TObject);
begin
  Label_Lineas.Enabled := false;
  Editor_Lineas.Enabled := false;
end;

//
// Cierra el programa si se pulsa el botón 'Cancelar'
//
procedure TFormularioParticion.Boton_CancelarClick(Sender: TObject);
begin
  FormularioParticion.Close;
end;

//
// Realiza la operación marcada (fragmentar/ensamblar) si se pulsa el botón 'Aceptar'
//
procedure TFormularioParticion.Boton_AceptarClick(Sender: TObject);
var
  numlin : integer;
begin
  if not FileExists (Editor_Abrir.Text) then
    ShowMessage ('No existe el fichero origen')
  else if Editor_Guardar.Text = '' then
    ShowMessage ('El fichero destino no es válido')
  else if RadioButton_Partir.Checked then begin
    numlin := StrToIntDef (Editor_Lineas.Text, 0);
    if numlin <= 0 then
      ShowMessage ('El número de líneas no es válido')
    else
      PartirArchivo (Editor_Abrir.Text, Editor_Guardar.Text, numlin)
  end
  else
    UnirArchivo (Editor_Abrir.Text, Editor_Guardar.Text);
end;

//
// Parte el fichero origen en fragmentos de un número determinado de líneas
//
procedure TFormularioParticion.PartirArchivo (origen, destino: string; numero: integer);
var
  fichero_origen, fichero_destino : TextFile;
  cont, cont2, num : integer;
  linea : string;
begin
  try
    AssignFile (fichero_origen, origen);
    FileMode := fmOpenRead;
    Reset (fichero_origen);
    FileMode := fmOpenWrite;
    num := StrToIntDef (Edit_num.Text, 0);
    if num = 0 then
      num := High (num);
    cont := 1;
    while (not SeekEof (fichero_origen)) and (cont <= num) do begin
      try
        AssignFile (fichero_destino, FicheroDestino (destino, cont));
        Rewrite (fichero_destino);
        cont2 := 0;
        while (not SeekEof (fichero_origen)) and (cont2 < numero) do begin
          ReadLn (fichero_origen, linea);
          WriteLn (fichero_destino, linea);
          inc (cont2);
        end;
      finally
        CloseFile (fichero_destino);
      end;
      inc (cont);
    end;
  finally
    CloseFile (fichero_origen);
  end;
  ShowMessage ('hecho');
end;


//
// Funde varios ficheros en uno
//
procedure TFormularioParticion.UnirArchivo(origen, destino: string);
begin
  showmessage ('en preparación');
end;


//
// Da formato al nombre del fichero destino
//
function TFormularioParticion.FicheroDestino (nombre: string; numero: integer): string;
var
  num_digitos, long_extension : integer;
  extension : string;
begin
  num_digitos := 5 - Log10Integer (numero);
  extension := ExtractFileExt (nombre);
  long_extension := Length (extension);
  Delete (nombre, Length (nombre) + 1 - long_extension, long_extension);
  result := nombre + '.' + StringOfChar ('0', num_digitos) + IntToStr (numero) + extension;
end;

//
// Devuelve el logaritmo en base 10 de un número determinado
//
function TFormularioParticion.Log10Integer (numero: integer): integer;
begin
  result := Trunc (Log10 (numero));
end;

end.


