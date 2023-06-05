program ParticionLineas;

uses
  Forms,
  particion_principal in 'particion_principal.pas' {FormularioParticion};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormularioParticion, FormularioParticion);
  Application.Run;
end.
