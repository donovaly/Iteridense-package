program IteridenseTest;

{$mode objfpc}{$H+}{$R+}{$Q+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, TAChartLazarusPkg, lazcontrols, IteridenseTestUnit;

{$R *.res}

begin
  RequireDerivedFormResource:= True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainF, MainF);
  Application.Run;
end.

