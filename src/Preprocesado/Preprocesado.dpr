program Preprocesado;

uses
  Forms,
  Principal in 'principal.pas' {Formulario_Principal},
  Ventana_Progresion in 'ventana_progresion.pas' {Formulario_Progresion};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm (TFormulario_Principal, Formulario_Principal);
  Application.CreateForm (TFormulario_Progresion, Formulario_Progresion);
  Application.Run;
end.
