program IteridenseClustering;

{$mode objfpc}{$H+}{$R+}{$Q+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, TAChartLazarusPkg, lazcontrols, IteridenseClusteringMain, ChartDataHandling,
  ceAxisDlg, ceBrushFrame, ceFontFrame, ceLegendDlg, cePenFrame, cePointerFrame,
  ceSeriesDlg, ceShapeBrushPenMarginsFrame, ceTitleFootDlg, ceUtils, ceAxisFrame,
  AboutForm;

{$R *.res}

begin
  RequireDerivedFormResource:= True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutFormF, AboutFormF);
  Application.Run;
end.

