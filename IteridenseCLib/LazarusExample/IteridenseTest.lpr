program IteridenseTest;

{$mode objfpc}{$H+}{$R+}{$Q+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, TAChartLazarusPkg, lazcontrols, IteridenseTestMain, ChartDataHandling,
  ceAxisDlg, ceBrushFrame, ceFontFrame, ceLegendDlg, cePenFrame, cePointerFrame,
  ceSeriesDlg, ceShapeBrushPenMarginsFrame, ceTitleFootDlg, ceUtils,
  ceAxisFrame;

{$R *.res}

begin
  RequireDerivedFormResource:= True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

