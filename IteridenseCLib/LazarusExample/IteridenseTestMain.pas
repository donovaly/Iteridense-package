unit IteridenseTestMain;

{$mode objfpc}{$H+}{$R+}{$Q+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  EditBtn, ExtCtrls, ComCtrls, Spin, Menus, Grids, ComboEx, Math, CTypes,
  SpinEx, FileInfo, StrUtils, Streamex, Generics.Collections, Types,
  TATransformations, TATools, TAGraph, TASeries, TAChartAxis, TALegend,
  TATextElements, TATypes,
  AboutForm;

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

  // C-compatible DBSCANResult struct
  DBSCANResultC = record
    numOfClusters : Int64;
    assignments : CArray;
    clusterSizes : CArray;
  end;
  PDBSCANResultC = ^DBSCANResultC;

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
  // allocates and computes the IteridenseResult
  // returns a pointer to a heap-allocated IteridenseResultC
  // caller must free with IteridenseFree
  // returns nil on failure
  TDBSCANClustering = function(
    const dataMatrix: PDouble; // const double* dataMatrix
    nrows, ncols: cint64;      // int64_t
    radius: cdouble;
    minNeighbors: cint64;
    minClusterSize: cint64
    ): PDBSCANResultC; cdecl;
  // frees memory allocated by TIteridenseClustering
  // returns 0 on success, -1 if ptr is nil
  TDBSCANFree = function(pointer: PDBSCANResultC): Integer; cdecl;

  ClusterMethods = (Ideridense, DBSCAN, KMeans, none);
  TIntArray = array of Int64;
  TDoubleArray = array of Double;

  { TMainForm }

  TMainForm = class(TForm)
    AboutMI: TMenuItem;
    AutoscaleMI: TMenuItem;
    AxisClickTool: TAxisClickTool;
    FlipB: TButton;
    ChangeBackColorMI: TMenuItem;
    ChartAxisTransformDim1: TChartAxisTransformations;
    DBSCANBevelB: TBevel;
    RadiusEpsFSEL: TLabel;
    MinClusterSizeDBscanL: TLabel;
    MinNeighborsL: TLabel;
    MinClusterSizeDBscanSE: TSpinEdit;
    PlotSelectionCCB: TCheckComboBox;
    DataSelectionCCB: TCheckComboBox;
    ContextChartPM: TPopupMenu;
    FileMI: TMenuItem;
    DimensionSelectionGB: TGroupBox;
    ClusterResultTS: TTabSheet;
    FinalResolutionLE: TLabeledEdit;
    ClusterResultSG: TStringGrid;
    DataSelectionL: TLabel;
    PlotSelectionL: TLabel;
    OpenCsvMI: TMenuItem;
    MainMenu: TMainMenu;
    SaveMI: TMenuItem;
    SavePlotMI: TMenuItem;
    MiscellaneousMI: TMenuItem;
    NoDiagonalsCB: TCheckBox;
    IteridenseBevelBottomB: TBevel;
    IteridenseBevelTopB: TBevel;
    ResetChartAppearanceMI: TMenuItem;
    SaveCsvMI: TMenuItem;
    Separator1MI: TMenuItem;
    MinNeighborsSE: TSpinEdit;
    UseDensityRB: TRadioButton;
    UseClustersRB: TRadioButton;
    DensityL: TLabel;
    MinClusterDensityL: TLabel;
    MinClusterSizeIterIdenseL: TLabel;
    StartResolutionL: TLabel;
    StopResolutionL: TLabel;
    MinClustersL: TLabel;
    MinClustersSE: TSpinEdit;
    StartResolutionSE: TSpinEdit;
    MinClusterSizeIterIdenseSE: TSpinEdit;
    StopResolutionSE: TSpinEdit;
    ChartAxisTransformValues: TChartAxisTransformations;
    ChartToolset: TChartToolset;
    ColorDialog: TColorDialog;
    DataPointClickTool: TDataPointClickTool;
    DataPointCrosshairTool: TDataPointCrosshairTool;
    DataPointHintTool: TDataPointHintTool;
    DataPointMarksClickTool: TDataPointMarksClickTool;
    DensityFSE: TFloatSpinEdit;
    MinClusterDensityFSE: TFloatSpinEdit;
    RadiusEpsFSE: TFloatSpinEdit;
    ClusteringBB: TBitBtn;
    LoadedActionFileL: TLabel;
    LoadedDataFileM: TMemo;
    OpenCsvBB: TBitBtn;
    LegendClickTool: TLegendClickTool;
    LineDragTool: TDataPointDragTool;
    MethodsPC: TPageControl;
    SavePlotBB: TBitBtn;
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
    procedure AboutMIClick(Sender: TObject);
    procedure AutoscaleMIClick(Sender: TObject);
    procedure AxisClickToolClick(ASender: TChartTool; Axis: TChartAxis;
      AHitInfo: TChartAxisHitTests);
    procedure ChangeBackColorMIClick(Sender: TObject);
    procedure DataPointHintToolHint(ATool: TDataPointHintTool;
      const APoint: TPoint; var AHint: String);
    procedure DataPointHintToolHintPosition(ATool: TDataPointHintTool;
      var APoint: TPoint);
    procedure DataSelectionCCBItemChange(Sender: TObject; AIndex: Integer);
    procedure FlipBClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ClusteringBBClick(Sender: TObject);
    function CArrayToTIntArray(cArray: CArray): TIntArray;
    function CArrayToTDoubleArray(cArray: CArray): TDoubleArray;
    function CTensorToTDoubleArray(tensor: CTensor): TDoubleArray;
    procedure LegendClickToolClick(ASender: TChartTool; ALegend: TChartLegend);
    procedure MethodsPCChange(Sender: TObject);
    procedure OpenCsvBBClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames{%H-}: array of String);
    procedure PlotSelectionCCBItemChange(Sender: TObject; AIndex: Integer);
    procedure ResetChartAppearanceMIClick(Sender: TObject);
    procedure SaveCsvMIClick(Sender: TObject);
    procedure SavePlotBBClick(Sender: TObject);
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
  DBSCANClustering : TDBSCANClustering;
  DBSCANFree : TDBSCANFree;
  InitJulia : TInitJulia;
  ShutdownJulia : TShutdownJulia;
  DropfileName : string = ''; // name of dropped CSV file
  InNameData : string = ''; // name of loaded data CSV file
  DataArray : Array of Array of double; // array that holds the data to be clustered
  DataHeader : string; // header line of InNameData
  DataColumnSeparator : Char; // column separator of the CSV file
  DataTextColumns : array of array of String; // to store text columns
  DataTextColumnsIndices : array of Int64; // to store the index of the text columns
  UsedClusteringMethod : ClusterMethods = none;
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
  // due to a bug in TAChart the preset title size is not taken on
  // high-DPI screens, therefore explicitly set it on start
  DataC.Title.Font.Size:= 11;

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
    // now DBSCAN
    DBSCANClustering:= TDBSCANClustering(GetProcAddress(LibHandle, 'DBSCANClustering'));
    if not Assigned(DBSCANClustering) then
    begin
      MessageDlg('The function "DBSCANClustering" is not found in the DLL', mtError, [mbOK], 0);
      FreeLibrary(LibHandle);
    end;
    DBSCANFree:= TDBSCANFree(GetProcAddress(LibHandle, 'DBSCANFree'));
    if not Assigned(DBSCANFree) then
    begin
      MessageDlg('The function "DBSCANFree" is not found in the DLL', mtError, [mbOK], 0);
      FreeLibrary(LibHandle);
    end;
  end
  else
  begin
    MessageDlg('DLL could not be loaded', mtError, [mbOK], 0);
    exit;
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
  // and unload the DLL
  if LibHandle <> 0 then
  begin
    ShutdownJulia(0);
    FreeLibrary(LibHandle);
  end;

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
  OpenCsvBBClick(Sender);
  DropfileName:= '';
end;


procedure TMainForm.ResetChartAppearanceMIClick(Sender: TObject);
var
  defaultFile : string;
  MousePointer : TPoint;
begin
  defaultFile:= ExtractFilePath(Application.ExeName) + AppearanceDefault;
  if not FileExists(defaultFile) then
  begin
    MousePointer:= Mouse.CursorPos; // store mouse position
    MessageDlgPos('The file "' + AppearanceDefault
                  + '" is not in the same folder as this program.'
                  + LineEnding + 'The appearance cannot be reset.',
                  mtError, [mbOK], 0 , MousePointer.X, MousePointer.Y);
    exit;
  end;
  ChartData.LoadAppearance(defaultFile);
end;


procedure TMainForm.FlipBClick(Sender: TObject);
begin
  ChartData.FlipBClick(Sender);
end;


procedure TMainForm.SaveCsvMIClick(Sender: TObject);
begin
  ChartData.CDSaveCsvMIClick(Sender);
end;


procedure TMainForm.SavePlotBBClick(Sender: TObject);
begin
  ChartData.CDSavePlotBBClick(Sender, MainForm.DataC.Name);
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
  count, i: Int64;
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
  count, i: Int64;
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
  count, i: Int64;
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


procedure TMainForm.ClusteringBBClick(Sender: TObject);
var
  dataPointer : PDouble;
  counter, row, column, i, assignmentColumn : Int64;
  iteridenseResult : PIteridenseResultC;
  DBSCANResult : PDBSCANResultC;
  inputArray : array of Double;
  dimensionIndices : array of Int64;
  assignmentsArray, clusterSizesArray : TIntArray;
  clusterDensitiesArray : TDoubleArray;
  rows, columns, minClusterSize, startResolution, stopResolution,
  minClusters, noDiagonals, useDensity, useClusters : cint64;
  density, minClusterDensity: cdouble;
begin
  assignmentsArray:= [];
  // we must transform the DataArray to a contiguous 1D array in
  // column-major order to make it accessible for Julia
  rows:= Length(DataArray);
  // determine the number of dimensions to be clustered from the combobox
  columns:= 0;
  SetLength(dimensionIndices, DataSelectionCCB.Items.Count);
  for i:= 0 to DataSelectionCCB.Items.Count-1 do
  begin
    if (DataSelectionCCB.Checked[i]) then
    begin
      dimensionIndices[i]:= 1;
      inc(columns);
    end;
  end;

  SetLength(inputArray, rows * columns);
  counter:= 0;
  for column:= 0 to Length(DataArray[0])-1-1 do // -1 to not take the assignment column into account
  begin
    // omit the text columns and only use selected dimensions
    if (DataTextColumnsIndices[column] = 0) and (dimensionIndices[column] = 1) then
    begin
      for row:= 0 to rows-1 do
        begin
          inputArray[counter]:= DataArray[row, column];
          inc(counter);
        end;
    end;
  end;
  //minClusterSize:= MinClusterSizeIterIdenseSE.Value;
  startResolution:= StartResolutionSE.Value;
  density:= DensityFSE.Value;
  stopResolution:= StopResolutionSE.Value;
  minClusters:= MinClustersSE.Value;
  minClusterDensity:= MinClusterDensityFSE.Value;
  noDiagonals:= NoDiagonalsCB.Checked.ToInteger;
  useDensity:= UseDensityRB.Checked.ToInteger;
  useClusters:= UseClustersRB.Checked.ToInteger;

  // we execute the clustering according to the currently open methods tab
  if MethodsPC.ActivePage.Caption = 'Iteridense' then
  begin
    iteridenseResult:= IteridenseClustering(
      @inputArray[0], // pointer to first element
      rows,
      columns,
      MinClusterSizeIterIdenseSE.Value,
      startResolution,
      density,
      stopResolution,
      minClusters,
      minClusterDensity,
      noDiagonals,
      useDensity,
      useClusters );
    // test if there is a result
    if iteridenseResult = nil then
    begin
      MessageDlg('Failed to create iteridenseResult', mtError, [mbOK], 0);
      exit;
    end;
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
    // free the allocated struct
    if IteridenseFree(iteridenseResult) <> 0 then
      MessageDlg('Warning: failed to free iteridenseResult', mtError, [mbOK], 0);
  end
  else if MethodsPC.ActivePage.Caption = 'DBSCAN' then
  begin
    DBSCANResult:= DBSCANClustering(
      @inputArray[0], // pointer to first element
      rows,
      columns,
      RadiusEpsFSE.Value,
      MinNeighborsSE.Value,
      MinClusterSizeDBscanSE.Value );
    // test if there is a result
    if DBSCANResult = nil then
    begin
      MessageDlg('Failed to create DBSCANResult', mtError, [mbOK], 0);
      exit;
    end;
    if (DBSCANResult^.assignments.data = nil)
      or (DBSCANResult^.assignments.length = 0) then
    begin
      MessageDlg('Clustering failed, no assignments available', mtError, [mbOK], 0);
      exit;
    end;
    // convert c-arrays to Pascal arrays
    assignmentsArray:= CArrayToTIntArray(DBSCANResult^.assignments);
    clusterSizesArray:= CArrayToTIntArray(DBSCANResult^.clusterSizes);
    // fill the arrays to ClusterResultSG
    ClusterResultSG.RowCount:= Length(clusterSizesArray) + 1; // +1 for header row
    i:= Length(clusterSizesArray);
    for i:= 0 to High(clusterSizesArray) do
    begin
      ClusterResultSG.Cells[0, i+1] := IntToStr(i+1);
      ClusterResultSG.Cells[1, i+1] := IntToStr(clusterSizesArray[i]);
      ClusterResultSG.Cells[2, i+1] := '-';
    end;
    // free the allocated struct
    if DBSCANFree(DBSCANResult) <> 0 then
      MessageDlg('Warning: failed to free iteridenseResult', mtError, [mbOK], 0);
  end;

  // add assignmentsArray as column to DataArray
  assignmentColumn:= length(DataArray[0])-1;
  for i:= 0 to High(DataArray) do
    DataArray[i][assignmentColumn]:= assignmentsArray[i];

  // plot the data
  ChartData.CDPlotSelectionCCBItemChange(Sender);

  // save the used clustering method
  UsedClusteringMethod:= ClusterMethods(MainForm.MethodsPC.ActivePage.TabIndex);

  // enable saving
  SaveCsvMI.Enabled:= true;
  SavePlotBB.Enabled:= true;
  SavePlotMI.Enabled:= true;
end;


procedure TMainForm.OpenCsvBBClick(Sender: TObject);
var
  DummyString, firstLine, secondLine : string;
  fileSuccess : Byte = 0;
  MousePointer : TPoint;
  i, clusterCount, assignmentColumn, count, dim1, dim2 : Integer;
  Series : TLineSeries;
begin
  MousePointer:= Mouse.CursorPos; // store mouse position
  DummyString:= '';
  // assure the input tab is visible
  if MethodsPC.ActivePage = ClusterResultTS then
    MethodsPC.ActivePage:= IteridenseTS;

  if DropfileName <> '' then // a file was dropped into the program
  begin
    // empty the selection boxes
    MainForm.PlotSelectionCCB.Items.Clear;
    MainForm.DataSelectionCCB.Items.Clear;
    // read the content into DataHeader and DataArray
    fileSuccess:= ChartData.ReadData(DropfileName)
  end
  else
  begin
    DummyString:= ChartData.OpenHandling('', '.csv');
    if DummyString = '' then
      exit; // user aborted the loading
    MainForm.PlotSelectionCCB.Items.Clear;
    MainForm.DataSelectionCCB.Items.Clear;
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
    SaveDialog.FileName:= ''; // will be re-set in ChartData.SaveHandling()
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
  end; // else if fileSuccess

  // add a column with zeros to DataArray to store there later the assignments
  assignmentColumn:= length(DataArray[0]);
  SetLength(DataArray, high(DataArray)+1, assignmentColumn+1);
  for i:= 0 to high(DataArray) do
    DataArray[i][assignmentColumn]:= 0;
  // we must also adapt DataTextColumnsIndices accordingly
  Setlength(DataTextColumnsIndices, assignmentColumn+1);

  // plot the data
  ChartData.CDPlotSelectionCCBItemChange(Sender);

  // enable/disable opbjects
  ClusteringBB.Enabled:= true;
  FlipB.Enabled:= true;
  SaveCsvMI.Enabled:= false;
  SavePlotBB.Enabled:= true;
  SavePlotMI.Enabled:= true;
  // reset method
  UsedClusteringMethod:= ClusterMethods.none;
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

procedure TMainForm.DataSelectionCCBItemChange(Sender: TObject; AIndex: Integer);
var
  i : Int64;
begin
  // check if there is at least one selected dimension
  for i:= 0 to MainForm.DataSelectionCCB.Items.Count-1 do
  begin
    if (MainForm.DataSelectionCCB.Checked[i]) then
    begin
      MainForm.ClusteringBB.Enabled:= true;
      MainForm.ClusteringBB.Hint:= 'click to start the Iteridense clustering';
      exit;
    end;
  end;
  // no dimension, thus disable clustering
  MainForm.ClusteringBB.Enabled:= false;
  MainForm.ClusteringBB.Hint:= 'no dimensions selected to be clustered';
end;

procedure TMainForm.PlotSelectionCCBItemChange(Sender: TObject; AIndex: Integer);
begin
  ChartData.CDPlotSelectionCCBItemChange(Sender);
end;

procedure TMainForm.AutoscaleMIClick(Sender: TObject);
begin
  ChartData.CDAutoscaleMIClick(Sender);
end;

procedure TMainForm.AboutMIClick(Sender: TObject);
begin
 with AboutFormF do
 begin
  // set version number
  NameL.Caption:= Application.Title + ' version ';
  VersionNumberL.Caption:= Version;
  Caption:= Application.Title;
  // open the dialog
  ShowModal;
 end;
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

procedure TMainForm.MethodsPCChange(Sender: TObject);
begin
  // disable Clustering button when the results tab is active
 if MethodsPC.ActivePage.Caption = 'Clustering Result' then
   ClusteringBB.Enabled:= false
 else
   ClusteringBB.Enabled:= true;
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

