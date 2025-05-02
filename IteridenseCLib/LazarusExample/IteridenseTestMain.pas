unit IteridenseTestMain;

{$mode objfpc}{$H+}{$R+}{$Q+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  EditBtn, ExtCtrls, ComCtrls, Spin, Menus, Grids, Math, CTypes,
  TATransformations, TATools, TAGraph, TASeries, TAChartAxis, TALegend, TATextElements,
  TATypes, TAChartUtils, SpinEx,
  FileInfo, StrUtils, Streamex, Generics.Collections, Types;

const
  MAX_DIMENSIONS = 256;

type

  // C-compatible tensor struct
  CTensor = record
    data: Pointer;         // void* data
    ndims: Int64;          // int64_t ndims
    dims: array[0..MAX_DIMENSIONS-1] of csize_t;
  end;

  // C-compatible array struct
  CArray = record
    data: Pointer;         // void* data
    length: csize_t;
  end;

  // C-compatible IteridenseResult struct
  IteridenseResultC = record
    clusterTensor : CTensor;
    countTensor : CTensor;
    numOfClusters : Int64;
    finalResolution : Int64;
    assignments : CArray;
    clusterDensities : CArray;
    clusterSizes : CArray;
  end;
  PIteridenseResultC = ^IteridenseResultC;

  TInitJulia = procedure(argc: Integer; argv: PPAnsiChar); cdecl;
  TShutdownJulia = procedure(retcode: Integer); cdecl;
  // allocates and computes the IteridenseResult
  // returns a pointer to a heap-allocated IteridenseResultC
  // caller must free with IteridenseFree
  // returns nil on failure
  TIteridenseClustering = function(
    const dataMatrix: PDouble; // const double* dataMatrix
    nrows, ncols: cint64;      // int64_t
    minClusterSize: cint64;
    startResolution: cint64;
    density: cdouble;
    stopResolution: cint64;
    minClusters: cint64;
    minClusterDensity: cdouble;
    noDiagonals: cint64;       // int64_t (0 or 1)
    useDensity: cint64;
    useClusters: cint64
    ): PIteridenseResultC; cdecl;
  // frees memory allocated by TIteridenseClustering
  // returns 0 on success, -1 if ptr is nil
  TIteridenseFree = function(pointer: PIteridenseResultC): Integer; cdecl;

  TIntArray = array of Int64;
  TDoubleArray = array of Double;

  { TMainForm }

  TMainForm = class(TForm)
    AutoscaleMI: TMenuItem;
    AxisClickTool: TAxisClickTool;
    ChangeBackColorMI: TMenuItem;
    ChartAxisTransformDim1: TChartAxisTransformations;
    ContextChartPM: TPopupMenu;
    IteridenseResultTS: TTabSheet;
    FinalResolutionLE: TLabeledEdit;
    ClusterResultSG: TStringGrid;
    NoDiagonalsCB: TCheckBox;
    IteridenseBevelBottomB: TBevel;
    IteridenseBevelTopB: TBevel;
    Separator1MI: TMenuItem;
    UseDensityRB: TRadioButton;
    UseClustersRB: TRadioButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MinClustersSE: TSpinEdit;
    StartResolutionSE: TSpinEdit;
    MinClusterSizeSE: TSpinEdit;
    StopResolutionSE: TSpinEdit;
    TopLine: TConstantLine;
    BottomLine: TConstantLine;
    LeftLine: TConstantLine;
    RightLine: TConstantLine;
    ChartAxisTransformValues: TChartAxisTransformations;
    ChartToolset: TChartToolset;
    ColorDialog: TColorDialog;
    DataPointClickTool: TDataPointClickTool;
    DataPointCrosshairTool: TDataPointCrosshairTool;
    DataPointHintTool: TDataPointHintTool;
    DataPointMarksClickTool: TDataPointMarksClickTool;
    DensityFSE: TFloatSpinEdit;
    MinClusterDensityFSE: TFloatSpinEdit;
    FloatSpinEdit3: TFloatSpinEdit;
    FloatSpinEdit4: TFloatSpinEdit;
    FloatSpinEdit5: TFloatSpinEdit;
    FloatSpinEdit6: TFloatSpinEdit;
    IteridenseBB: TBitBtn;
    LoadedActionFileL: TLabel;
    LoadedDataFileM: TMemo;
    OpenBB: TBitBtn;
    LegendClickTool: TLegendClickTool;
    LineDragTool: TDataPointDragTool;
    MethodsPC: TPageControl;
    SaveBB: TBitBtn;
    OpenDialog: TOpenDialog;
    PanDragTool: TPanDragTool;
    PanMouseWheelTool: TPanMouseWheelTool;
    RectangleSelectionTool: TUserDefinedTool;
    SaveDialog: TSaveDialog;
    DataC: TChart;
    IteridenseTS: TTabSheet;
    DBSCANTS: TTabSheet;
    KMeansTS: TTabSheet;
    TitleFootClickTool: TTitleFootClickTool;
    ValuesAutoScaleAxisTransform: TAutoScaleAxisTransform;
    ValuesLinearTransform: TLinearAxisTransform;
    ZoomDragTool: TZoomDragTool;
    ZoomMouseWheelTool: TZoomMouseWheelTool;
    procedure AutoscaleMIClick(Sender: TObject);
    procedure AxisClickToolClick(ASender: TChartTool; Axis: TChartAxis;
      AHitInfo: TChartAxisHitTests);
    procedure ChangeBackColorMIClick(Sender: TObject);
    procedure DataPointHintToolHint(ATool: TDataPointHintTool;
      const APoint: TPoint; var AHint: String);
    procedure DataPointHintToolHintPosition(ATool: TDataPointHintTool;
      var APoint: TPoint);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure IteridenseBBClick(Sender: TObject);
    function CArrayToTIntArray(cArray: CArray): TIntArray;
    function CArrayToTDoubleArray(cArray: CArray): TDoubleArray;
    function CTensorToTDoubleArray(tensor: CTensor): TDoubleArray;
    procedure LegendClickToolClick(ASender: TChartTool; ALegend: TChartLegend);
    procedure OpenBBClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames{%H-}: array of String);
    procedure TitleFootClickToolClick(ASender: TChartTool; ATitle: TChartTitle);
    procedure UseDensityRBChange(Sender: TObject);
  private
  public
  end;

const
  DLLName : string = 'IteridenseCLib.dll';
var
  MainForm : TMainForm;
  Version : string = '';
  LibHandle : THandle;
  IteridenseClustering : TIteridenseClustering;
  IteridenseFree : TIteridenseFree;
  InitJulia : TInitJulia;
  ShutdownJulia : TShutdownJulia;
  DropfileName : string = ''; // name of dropped CSV file
  InNameData : string = ''; // name of loaded data CSV file
  DataArray : Array of Array of double; // array that holds the data to be clustered
  // filename to store appearance
  const AppearanceFile : string = 'Appearance-IteridenseTest.ini';
  // filename with default appearance
  const AppearanceDefault : string = 'Appearance-IteridenseTest.default';

implementation

{$R *.lfm}

uses
  ChartDataHandling;


procedure TMainForm.FormCreate(Sender: TObject);
var
  iniFile : string;
  FileVerInfo: TFileVersionInfo;
  i : integer;
  args: array[0..1] of PAnsiChar;
begin
  try
    FileVerInfo:= TFileVersionInfo.Create(nil);
    FileVerInfo.ReadFileInfo;
    Version:= FileVerInfo.VersionStrings.Values['ProductVersion'];
  finally
    FileVerInfo.Free;
  end;
  MainForm.Caption:= Application.Title + ' ' + Version;
  DefaultFormatSettings.DecimalSeparator:= '.'; // we use English numbers

  // explicitly set there because the IDE always
  // stores initial values with trailing LineEnding
  LoadedDataFileM.Text:= 'None';

  // initialize chart transformation before we can draw in chart, otherwise there
  // would be an error when sensor data is load before chart is shown first time
  DataC.Prepare;

  // setup the chart
  TopLine.Position:= Infinity;
  BottomLine.Position:= -Infinity;
  LeftLine.Position:= -Infinity;
  RightLine.Position:= Infinity;
  // due to a bug in TAChart the preset title size is not taken on
  // high-DPI screens, therefore explicitly set it on start
  DataC.Title.Font.Size:= 11;
  // make the bars' DatapointDragtool react only on the bars, not the data points
  LineDragTool.AffectedSeries:= Format('%d;%d;%d;%d',
   [TopLine.Index, BottomLine.Index, LeftLine.Index, RightLine.Index]);

  // load the current chart appearance settings
  // we load from the same folder than the program .exe
  iniFile:= ExtractFilePath(Application.ExeName) + AppearanceFile;
  if FileExists(iniFile) then
    ChartData.LoadAppearance(iniFile);

  // load the Iteridense DLL
  LibHandle:= LoadLibrary(PChar(DLLName));
  if LibHandle <> 0 then
  begin
    // testing the Julia start/stop functions
    InitJulia:= TInitJulia(GetProcAddress(LibHandle, 'init_julia'));
    if not Assigned(InitJulia) then
    begin
      MessageDlg('The function "init_julia" is not found in the DLL', mtError, [mbOK], 0);
      FreeLibrary(LibHandle);
    end;
    ShutdownJulia:= TShutdownJulia(GetProcAddress(LibHandle, 'shutdown_julia'));
    if not Assigned(ShutdownJulia) then
    begin
      MessageDlg('The function "shutdown_julia" is not found in the DLL', mtError, [mbOK], 0);
      FreeLibrary(LibHandle);
    end;
    // testing the struct functions
    IteridenseClustering:= TIteridenseClustering(GetProcAddress(LibHandle, 'IteridenseClustering'));
    if not Assigned(IteridenseClustering) then
    begin
      MessageDlg('The function "IteridenseClustering" is not found in the DLL', mtError, [mbOK], 0);
      FreeLibrary(LibHandle);
    end;
    IteridenseFree:= TIteridenseFree(GetProcAddress(LibHandle, 'IteridenseFree'));
    if not Assigned(IteridenseFree) then
    begin
      MessageDlg('The function "IteridenseFree" is not found in the DLL', mtError, [mbOK], 0);
      FreeLibrary(LibHandle);
    end;
  end
  else
  begin
    MessageDlg('DLL could not be loaded', mtError, [mbOK], 0);
  end;

  // initialize Julia runtime
  args[0] := PAnsiChar(AnsiString('MyProgram'));
  args[1] := nil;
  InitJulia(1, @args[0]);

end;


procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  iniFile : string;
begin
  // shutdown Julia runtime with exit code 0 (success)
  ShutdownJulia(0);
  // unload the DLL
  FreeLibrary(LibHandle);

  // save the current chart appearance settings
  // we write into the same folder than the program .exe
  iniFile:= ExtractFilePath(Application.ExeName) + AppearanceFile;
  ChartData.SaveAppearance(iniFile);
end;


procedure TMainForm.FormDropFiles(Sender: TObject;
const
  FileNames: array of String);
begin
  DropfileName:= FileNames[0];
  OpenBBClick(Sender);
  DropfileName:= '';
end;


procedure TMainForm.UseDensityRBChange(Sender: TObject);
begin
  if UseDensityRB.Checked then
  begin
    DensityFSE.Enabled:= true;
    MinClustersSE.Enabled:= false;
  end
  else
  begin
    DensityFSE.Enabled:= false;
    MinClustersSE.Enabled:= true;
  end;
end;


// function to convert a c-array to an array of Int64
function TMainForm.CArrayToTIntArray(cArray: CArray): TIntArray;
var
  count, i: LongInt;
  arrayPointer: PInt64;
begin
  result:= [];
  Setlength(result, cArray.length);
  arrayPointer:= PInt64(cArray.data);
  for i:= 0 to High(result) do
    result[i]:= arrayPointer[i];
end;

// function to convert a c-array to an array of Double
function TMainForm.CArrayToTDoubleArray(cArray: CArray): TDoubleArray;
var
  count, i: LongInt;
  arrayPointer: PDouble;
begin
  result:= [];
  Setlength(result, cArray.length);
  arrayPointer:= PDouble(cArray.data);
  for i:= 0 to High(result) do
    result[i]:= arrayPointer[i];
end;


// function to convert a c-tensor to an array of Double
function TMainForm.CTensorToTDoubleArray(tensor: CTensor): TDoubleArray;
var
  count, i: LongInt;
  dataPointer: PDouble;
begin
  // we have to manually count because tensor.dims is MAX_DIMENSIONS
  // as defined but we need only the ones that are not zero
  count:= 1;
  for i:= 0 to tensor.ndims - 1 do
    count:= count * tensor.dims[i];
  result:= [];
  SetLength(result, count);
  dataPointer:= PDouble(tensor.data);
  for i:= 0 to count - 1 do
    result[i]:= dataPointer[i];
end;


procedure TMainForm.IteridenseBBClick(Sender: TObject);
var
  dataPointer : PDouble;
  counter, rows, columns, val, i, count, idx,
    numOfClusters, assignmentColumn : LongInt;
  randomNumber : Double;
  textLine : String;
  iteridenseResult : PIteridenseResultC;
  inputArray : array of Double;
  assignmentsArray, clusterSizesArray : TIntArray;
  clusterDensitiesArray : TDoubleArray;
  nrows, ncols, minClusterSize, startResolution, stopResolution,
  minClusters, noDiagonals, useDensity, useClusters : cint64;
  density, minClusterDensity: cdouble;
  Series : array of TLineSeries;
begin
  // we must transform the DataArray to a contiguous 1D array in
  // column-major order to make it accessible for Julia
  nrows:= Length(DataArray);
  ncols:= Length(DataArray[0]) - 1; // don't take the assignment column into account
  SetLength(inputArray, nrows * ncols);
  counter:= 0;
  for columns:= 0 to ncols-1 do
    for rows:= 0 to nrows-1 do
      begin
        inputArray[counter]:= DataArray[rows, columns];
        inc(counter);
      end;

  minClusterSize:= MinClusterSizeSE.Value;
  startResolution:= StartResolutionSE.Value;
  density:= DensityFSE.Value;
  stopResolution:= StopResolutionSE.Value;
  minClusters:= MinClustersSE.Value;
  minClusterDensity:= MinClusterDensityFSE.Value;
  noDiagonals:= NoDiagonalsCB.Checked.ToInteger;
  useDensity:= UseDensityRB.Checked.ToInteger;
  useClusters:= UseClustersRB.Checked.ToInteger;

  iteridenseResult:= IteridenseClustering(
    @inputArray[0], // pointer to first element
    nrows,
    ncols,
    minClusterSize,
    startResolution,
    density,
    stopResolution,
    minClusters,
    minClusterDensity,
    noDiagonals,
    useDensity,
    useClusters
  );

  if iteridenseResult = nil then
  begin
    MessageDlg('Failed to create iteridenseResult', mtError, [mbOK], 0);
    exit;
  end;

  // fixme check case no cluster found
  if (iteridenseResult^.assignments.data = nil)
    or (iteridenseResult^.assignments.length = 0) then
  begin
    MessageDlg('Clustering failed, no assignments available', mtError, [mbOK], 0);
    exit;
  end;

  // convert c-arrays to Pascal arrays
  assignmentsArray:= CArrayToTIntArray(iteridenseResult^.assignments);
  clusterDensitiesArray:= CArrayToTDoubleArray(iteridenseResult^.clusterDensities);
  clusterSizesArray:= CArrayToTIntArray(iteridenseResult^.clusterSizes);

  FinalResolutionLE.Text:= IntToStr(iteridenseResult^.finalResolution);
  // fill the arrays to ClusterResultSG
  ClusterResultSG.RowCount:= Length(clusterSizesArray) + 1; // +1 for header row
  i:= Length(clusterSizesArray);
  for i:= 0 to High(clusterSizesArray) do
  begin
    ClusterResultSG.Cells[0, i+1] := IntToStr(i+1);
    ClusterResultSG.Cells[1, i+1] := IntToStr(clusterSizesArray[i]);
    ClusterResultSG.Cells[2, i+1] := Format('%.3g', [clusterDensitiesArray[i]]);
  end;

  numOfClusters:= iteridenseResult^.numOfClusters;

  // add assignmentsArray as column to DataArray
  assignmentColumn:= length(DataArray[0])-1;
  for i:= 0 to High(DataArray) do
    DataArray[i][assignmentColumn]:= assignmentsArray[i];

  // plot the data
  // first delete existing data
  for i := DataC.SeriesCount - 1 downto 0 do
  begin
    DataC.Series[i].Free;
  end;

  // create a series for each cluster
  SetLength(Series, numOfClusters+1);
  for i:= 0 to numOfClusters do
  begin
    Series[i]:= TLineSeries.Create(DataC);
    Series[i].ShowLines:= False; // points only
    Series[i].Pointer.Visible:= true;
    Series[i].Pointer.Brush.Color:= colorPalette[i]; // assign unique color for each cluster
    Series[i].Pointer.Style:= psCircle; // circles for the points
    Series[i].Title:= IntToStr(i);
    DataC.AddSeries(Series[i]);
  end;

  // add data points to the correct series
  assignmentColumn:= Length(DataArray[0]) - 1;
  for i:= 0 to high(DataArray) do
  begin
    idx := round(DataArray[i, assignmentColumn]); // clusterID
    Series[idx].AddXY(DataArray[i, 0], DataArray[i, 1]);
  end;


  // free the allocated struct
  if IteridenseFree(iteridenseResult) <> 0 then
    MessageDlg('Warning: failed to free iteridenseResult', mtError, [mbOK], 0);

end;


procedure TMainForm.OpenBBClick(Sender: TObject);
var
  DummyString, firstLine, secondLine : string;
  fileSuccess : Byte = 0;
  MousePointer : TPoint;
  i, clusterCount, assignmentColumn : Integer;
  Series : TLineSeries;
begin
  MousePointer:= Mouse.CursorPos; // store mouse position
  DummyString:= '';
  // assure the input tab is visible
  if MethodsPC.ActivePage = IteridenseResultTS then
    MethodsPC.ActivePage:= IteridenseTS;

  if DropfileName <> '' then // a file was dropped into the program
    fileSuccess:= ChartData.ReadData(DropfileName)
  else
  begin
    DummyString:= ChartData.OpenHandling('', '.csv');
    if DummyString = '' then
      exit; // user aborted the loading
    fileSuccess:= ChartData.ReadData(DummyString);
  end;

  if fileSuccess <> 1 then
  begin
    if fileSuccess < 2 then // only output message if there was not already an error message
      MessageDlgPos('Error while attempting to open file',
       mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
    exit;
  end
  else
  begin
    if DropfileName <> '' then
      InNameData:= DropfileName
    else
      InNameData:= OpenDialog.FileName;
    SaveDialog.FileName:= ''; // will be re-set in TChartData.SaveHandling()
    // show the full path as tooltip
    if DropfileName <> '' then
      LoadedDataFileM.Hint:= DropfileName
    else
      LoadedDataFileM.Hint:= DummyString;
    // display file name without suffix
    DummyString:= ExtractFileName(InNameData);
    SetLength(DummyString, Length(DummyString) - 4);
    LoadedDataFileM.Color:= clActiveCaption;
    LoadedDataFileM.Text:= DummyString;

    // disable saving, will be re-enabled by GererateCommand
    SaveBB.Enabled:= False;
  end; // else if fileSuccess

  // plot the data
  // first delete existing data
  for i := DataC.SeriesCount - 1 downto 0 do
  begin
    DataC.Series[i].Free;
  end;

  // add a column with zeros to DataArray to store there lates the assignments
  assignmentColumn:= length(DataArray[0]);
  SetLength(DataArray, high(DataArray)+1, assignmentColumn+1);
  for i:= 0 to high(DataArray) do
    DataArray[i][assignmentColumn]:= 0;

  // create a plot series
  Series:= TLineSeries.Create(DataC);
  Series.ShowLines:= False; // points only
  Series.Pointer.Visible:= True;
  Series.Pointer.Brush.Color:= colorPalette[0]; // assign unique color for each cluster
  Series.Pointer.Style:= psCircle; // circles for the points
  Series.Title:= '0';
  DataC.AddSeries(Series);

  // add data points to the series
  // fixme: allow to select what column to plot
  for i:= 0 to high(DataArray) do
    Series.AddXY(DataArray[i, 0], DataArray[i, 1]);

  // finally enable the clustering button
  IteridenseBB.Enabled:= true;
end;


procedure TMainForm.DataPointHintToolHint(ATool: TDataPointHintTool;
  const APoint: TPoint; var AHint: String);
begin
  ChartData.CDDataPointHintToolHint(ATool, APoint, AHint);
end;

procedure TMainForm.DataPointHintToolHintPosition(ATool: TDataPointHintTool;
  var APoint: TPoint);
begin
  ChartData.CDDataPointHintToolHintPosition(ATool, APoint);
end;

procedure TMainForm.AutoscaleMIClick(Sender: TObject);
begin
  ChartData.CDAutoscaleMIClick(Sender);
end;

procedure TMainForm.ChangeBackColorMIClick(Sender: TObject);
begin
  ChartData.CDChangeBackColorMIClick(Sender);
end;

procedure TMainForm.LegendClickToolClick(ASender: TChartTool;
  ALegend: TChartLegend);
begin
  ChartData.CDLegendClickToolClick(ASender, ALegend);
end;

procedure TMainForm.AxisClickToolClick(ASender: TChartTool; Axis: TChartAxis;
  AHitInfo: TChartAxisHitTests);
begin
  ChartData.CDAxisClickToolClick(ASender, Axis, AHitInfo);
end;

procedure TMainForm.TitleFootClickToolClick(ASender: TChartTool;
  ATitle: TChartTitle);
begin
  ChartData.CDTitleFootClickToolClick(ASender, ATitle);
end;


end.

