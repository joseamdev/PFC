unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, StrUtils;

type
  TForm1 = class(TForm)
    OpenDialog: TOpenDialog;
    EditFichero: TEdit;
    ButtonAbrir: TButton;
    ButtonConvertir: TButton;
    procedure ButtonAbrirClick(Sender: TObject);
    procedure ButtonConvertirClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


const
   EXTENSION = '.arb';
   TEMPORAL = '.temp';


var
  Form1: TForm1;



implementation



{$R *.dfm}



procedure TForm1.ButtonAbrirClick(Sender: TObject);

begin
   if OpenDialog.Execute then begin
      EditFichero.Text := OpenDialog.FileName;
      ButtonConvertir.Enabled := true;
   end;
end;





procedure TForm1.ButtonConvertirClick(Sender: TObject);

var
   fichero, fichero_temporal, fichero_salida : TextFile;
   nombre, linea, valor_str : string;
   posicion : integer;
   es_arbol : boolean;

begin
   nombre := EditFichero.Text;

   FileMode := fmOpenRead;
   AssignFile (fichero, nombre);
   Reset (fichero);

   FileMode := fmOpenWrite;
   AssignFile (fichero_temporal, nombre + TEMPORAL);
   Rewrite (fichero_temporal);

   AssignFile (fichero_salida, nombre + EXTENSION);
   Rewrite (fichero_salida);

   es_arbol := false;

   repeat
      ReadLn (fichero, linea);
      posicion := Pos ('tree', linea);
      if posicion > 0 then begin
         ReadLn (fichero, linea);
         posicion := Pos ('-----', linea);
         es_arbol := posicion > 0;
      end;
   until (SeekEof (fichero)) or es_arbol;

   repeat
      ReadLn (fichero, linea);
      posicion := Pos ('=', linea) + Pos ('>', linea) + Pos ('|', linea);
      if posicion > 0 then
         WriteLn (fichero_temporal, linea)
      else if Pos (':', linea) = 1 then
         WriteLn (fichero_temporal, 'DURACION =' + RightStr (linea, Length (linea) - 1))
      else
         es_arbol := false;
   until (SeekEof (fichero)) or (not es_arbol);

   CloseFile (fichero_temporal);

   while (not SeekEof (fichero)) and (Pos ('Correctly Classified Instances', linea) = 0) do
      ReadLn (fichero, linea);

   CloseFile (fichero);

   if Pos ('Correctly Classified Instances', linea) > 0 then
      valor_str := LeftStr (RightStr (linea, 9), 2) + MidStr (RightStr (linea, 9), 4, 4)
   else
      valor_str := '0';

   WriteLn (fichero_salida, valor_str);

   FileMode := fmOpenRead;
   AssignFile (fichero_temporal, nombre + TEMPORAL);
   Reset (fichero_temporal);

   while not SeekEof (fichero_temporal) do begin
      ReadLn (fichero_temporal, linea);
      WriteLn (fichero_salida, linea);
   end;

   CloseFile (fichero_temporal);
   CloseFile (fichero_salida);

   DeleteFile (nombre + TEMPORAL);
end;





end.
