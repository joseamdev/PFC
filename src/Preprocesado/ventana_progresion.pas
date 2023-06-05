unit Ventana_Progresion;

interface

  uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
    Forms, Dialogs, Gauges, StdCtrls;

  type
    TFormulario_Progresion = class(TForm)
      Gauge1: TGauge;
      Label1: TLabel;
      private
        { Private declarations }
      public
        procedure Progresion (numero : integer);
      end;

  var
    Formulario_Progresion : TFormulario_Progresion;

implementation

{$R *.dfm}

  procedure TFormulario_Progresion.Progresion(numero: integer);
    begin
      Gauge1.Progress := numero;
    end;

end.
