program FiltroDM;

uses
  Forms,
  Main in 'Main.pas' {FormFiltroDM},
  Alumnos in 'Alumnos.pas',
  Asignaturas in 'Asignaturas.pas',
  Compactar in 'Compactar.pas',
  Escritura in 'Escritura.pas',
  Experiencias in 'Experiencias.pas',
  Lectura in 'Lectura.pas',
  Tablas in 'Tablas.pas',
  Progreso in 'Progreso.pas' {FormProgreso};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormFiltroDM, FormFiltroDM);
  Application.CreateForm(TFormProgreso, FormProgreso);
  Application.Run;
end.
