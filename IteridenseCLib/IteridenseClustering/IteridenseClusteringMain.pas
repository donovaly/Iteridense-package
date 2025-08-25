unit IteridenseClusteringMain;

{$mode objfpc}{$H+}{$R+}{$Q+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  Math, ExtCtrls, ComCtrls, Spin, Menus, Grids, ComboEx, CTypes, SpinEx,
  FileInfo, Types, TATransformations, TATools, TAGraph, TASeries,
  TAChartAxis, TALegend, TATextElements, AboutForm;

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

  // C-compatible KMeansResult struct
  KMeansResultC = record
    numOfClusters : Int64;
    assignments : CArray;
    clusterSizes : CArray;
    clusterCenters : CTensor;
  end;
  PKMeansResultC = ^KMeansResultC;

  TInitJulia = procedure(argc: Integer; argv: PPAnsiChar); cdecl;
  TShutdownJulia = procedure(retcode: Integer); cdecl;
  // allocates and computes the IteridenseResult
  // returns a pointer to a heap-allocated IteridenseResultC
  // caller must free with IteridenseFree
  // returns nil on failure
  TIteridenseClustering = function(
    const dataMatrix: PDouble;  // const double* dataMatrix
    nrows, ncols: cint64;       // int64_t
    density: cdouble;
    minClusters: cint64;
    minClusterSize: cint64;
    startResolution: cint64;
    stopResolution: cint64;
    minClusterDensity: cdouble;
    useDensity: cint64;         // int64_t (0 or 1)
    useClusters: cint64;
    noDiagonals: cint64
    ): PIteridenseResultC; cdecl;
  // frees memory allocated by TIteridenseClustering
  // returns 0 on success, -1 if ptr is nil
  TIteridenseFree = function(pointer: PIteridenseResultC): Integer; cdecl;

  // like for IteridenseResult
  TDBSCANClustering = function(
    const dataMatrix: PDouble; // const double* dataMatrix
    nrows, ncols: cint64;
    radius: cdouble;
    minNeighbors: cint64;
    minClusterSize: cint64
    ): PDBSCANResultC; cdecl;
  // frees memory allocated by TDBSCANClustering
  TDBSCANFree = function(pointer: PDBSCANResultC): Integer; cdecl;

  // like for IteridenseResult
  TKMeansClustering = function(
    const dataMatrix: PDouble; // const double* dataMatrix
    nrows, ncols: cint64;
    numOfClusters: cint64;
    maxIter: cint64;
    tolerance: cdouble
    ): PKMeansResultC; cdecl;
  // frees memory allocated by TKMeansClustering
  TKMeansFree = function(pointer: PKMeansResultC): Integer; cdecl;

  // calls Julia's garbage collection
  // returns in every case 0
  TGarbageCollection = function(): Integer; cdecl;

  // returns size of currently available memory in bytes
  TFreeMemoryInBytes = function(): UInt64; cdecl;

  ClusterMethods = (Ideridense, DBSCAN, KMeans, none);
  TIntArray = array of Int64;
  TDoubleArray = array of Double;

  { TMainForm }

  TMainForm = class(TForm)
    AboutMI: TMenuItem;
    AutoscaleMI: TMenuItem;
    AxisClickTool: TAxisClickTool;
    NumDataPointsLE: TLabeledEdit;
    LabelHintB: TButton;
    FlipTB: TToggleBox;
    IteridenseSliderGB: TGroupBox;
    LoadedDataFileLE: TLabeledEdit;
    ProportionalMI: TMenuItem;
    RadiusTB: TTrackBar;
    ChangeBackColorMI: TMenuItem;
    ChartAxisTransformDim1: TChartAxisTransformations;
    DBSCANBevelB: TBevel;
    SliderStartSE: TSpinEdit;
    SliderStopSE: TSpinEdit;
    ToleranceFSX: TFloatSpinEditEx;
    KMeansBevelB: TBevel;
    MaxIterationsL: TLabel;
    MaxIterationsSE: TSpinEdit;
    NumberClustersL: TLabel;
    NumberClustersSE: TSpinEdit;
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
    ToleranceL: TLabel;
    DensityTB: TTrackBar;
    IteridenseSliderTB: TTrackBar;
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
    DataPointCrosshairTool: TDataPointCrosshairTool;
    DataPointHintTool: TDataPointHintTool;
    DensityFSE: TFloatSpinEdit;
    MinClusterDensityFSE: TFloatSpinEdit;
    RadiusFSE: TFloatSpinEdit;
    ClusteringBB: TBitBtn;
    OpenCsvBB: TBitBtn;
    LegendClickTool: TLegendClickTool;
    MethodsPC: TPageControl;
    SavePlotBB: TBitBtn;
    OpenDialog: TOpenDialog;
    PanDragTool: TPanDragTool;
    PanMouseWheelTool: TPanMouseWheelTool;
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
    procedure DataSelectionCCBItemChange(Sender: TObject; AIndex{%H-}: Integer);
    procedure DensityFSEChange(Sender: TObject);
    procedure DensityTBChange(Sender: TObject);
    procedure FlipTBChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction{%H-}: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ClusteringBBClick(Sender: TObject);
    function CArrayToTIntArray(cArray: CArray): TIntArray;
    function CArrayToTDoubleArray(cArray: CArray): TDoubleArray;
    function CTensorToTDoubleArray(tensor: CTensor): TDoubleArray;
    procedure ProportionalMIClick(Sender: TObject);
    procedure RadiusFSEChange(Sender: TObject);
    procedure UpdateLabelHintBPosition;
    procedure IteridenseSliderTBChange(Sender: TObject);
    procedure IteridenseSliderTBMouseDown(Sender: TObject;
      Button{%H-}: TMouseButton; Shift{%H-}: TShiftState; X{%H-}, Y{%H-}: Integer);
    procedure IteridenseSliderTBMouseMove(Sender: TObject; Shift: TShiftState;
      X{%H-}, Y{%H-}: Integer);
    procedure IteridenseSliderTBMouseUp(Sender: TObject; Button{%H-}: TMouseButton;
      Shift{%H-}: TShiftState; X{%H-}, Y{%H-}: Integer);
    procedure LegendClickToolClick(ASender: TChartTool; ALegend: TChartLegend);
    procedure MethodsPCChange(Sender: TObject);
    procedure OpenCsvBBClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames{%H-}: array of String);
    procedure PlotSelectionCCBItemChange(Sender: TObject; AIndex{%H-}: Integer);
    procedure RadiusTBChange(Sender: TObject);
    procedure ResetChartAppearanceMIClick(Sender: TObject);
    procedure SaveCsvMIClick(Sender: TObject);
    procedure SavePlotBBClick(Sender: TObject);
    procedure SliderStartSEEditingDone(Sender: TObject);
    procedure SliderStopSEEditingDone(Sender: TObject);
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
  KMeansClustering : TKMeansClustering;
  KMeansFree : TKMeansFree;
  GarbageCollection : TGarbageCollection;
  FreeMemoryInBytes : TFreeMemoryInBytes;
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
  SliderPosition : Integer; // to know in what direction a slider was moved
  SliderIncrement : Double; // to store the slider increment
  IderidenseDensity : Double; // to store the density for Ideridense
  IderidenseDensityBySlider : Boolean = false; // for signal handling
  DBSCANRadius : Double; // to store the radius for DBSCAN
  DBSCANRadiusBySlider : Boolean = false; // for signal handling
  KMeansClusterCenters : Array of Array of double; // store the K-means cluster center coordinates
  // filename to store appearance
  const AppearanceFile : string = 'Appearance-IteridenseClustering.ini';
  // filename with default appearance
  const AppearanceDefault : string = 'Appearance-IteridenseClustering.default';

implementation

{$R *.lfm}

uses
  ChartDataHandling;


procedure TMainForm.FormCreate(Sender: TObject);
var
  iniFile : string;
  FileVerInfo : TFileVersionInfo;
  args : array[0..1] of PAnsiChar;
  errorDialog : TForm;
  DLLLoadingError : Boolean;
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

  // initialize chart transformation
  DataC.Prepare;

  // initializations
  SliderPosition:= DensityTB.Position;
  IderidenseDensity:= DensityFSE.Value;
  DBSCANRadius:= RadiusFSE.Value;

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
  DLLLoadingError:= false;
  LibHandle:= LoadLibrary(PChar(DLLName));
  if LibHandle <> 0 then
  begin
    // testing the Julia start/stop functions
    InitJulia:= TInitJulia(GetProcAddress(LibHandle, 'init_julia'));
    if not Assigned(InitJulia) then
    begin
      errorDialog:= CreateMessageDialog('The function "init_julia" is not found in the DLL',
                                        mtError, [mbOK]);
      FreeLibrary(LibHandle);
      DLLLoadingError:= true;
    end;
    ShutdownJulia:= TShutdownJulia(GetProcAddress(LibHandle, 'shutdown_julia'));
    if not Assigned(ShutdownJulia) then
    begin
      errorDialog:= CreateMessageDialog('The function "shutdown_julia" is not found in the DLL',
                                        mtError, [mbOK]);
      FreeLibrary(LibHandle);
      DLLLoadingError:= true;
    end;
    // testing general Julia functions
    GarbageCollection:= TGarbageCollection(GetProcAddress(LibHandle, 'GarbageCollection'));
    if not Assigned(GarbageCollection) then
    begin
      errorDialog:= CreateMessageDialog('The function "GarbageCollection" is not found in the DLL',
                                        mtError, [mbOK]);
      FreeLibrary(LibHandle);
      DLLLoadingError:= true;
    end;
    FreeMemoryInBytes:= TFreeMemoryInBytes(GetProcAddress(LibHandle, 'FreeMemoryInBytes'));
    if not Assigned(FreeMemoryInBytes) then
    begin
      errorDialog:= CreateMessageDialog('The function "FreeMemoryInBytes" is not found in the DLL',
                                        mtError, [mbOK]);
      FreeLibrary(LibHandle);
      DLLLoadingError:= true;
    end;
    // testing the struct functions
    IteridenseClustering:= TIteridenseClustering(GetProcAddress(LibHandle, 'IteridenseClustering'));
    if not Assigned(IteridenseClustering) then
    begin
      errorDialog:= CreateMessageDialog(
                      'The function "IteridenseClustering" is not found in the DLL',
                      mtError, [mbOK]);
      FreeLibrary(LibHandle);
      DLLLoadingError:= true;
    end;
    IteridenseFree:= TIteridenseFree(GetProcAddress(LibHandle, 'IteridenseFree'));
    if not Assigned(IteridenseFree) then
    begin
      errorDialog:= CreateMessageDialog('The function "IteridenseFree" is not found in the DLL',
                                        mtError, [mbOK]);
      FreeLibrary(LibHandle);
      DLLLoadingError:= true;
    end;
    // now DBSCAN
    DBSCANClustering:= TDBSCANClustering(GetProcAddress(LibHandle, 'DBSCANClustering'));
    if not Assigned(DBSCANClustering) then
    begin
      errorDialog:= CreateMessageDialog('The function "DBSCANClustering" is not found in the DLL',
                                        mtError, [mbOK]);
      FreeLibrary(LibHandle);
      DLLLoadingError:= true;
    end;
    DBSCANFree:= TDBSCANFree(GetProcAddress(LibHandle, 'DBSCANFree'));
    if not Assigned(DBSCANFree) then
    begin
      errorDialog:= CreateMessageDialog('The function "DBSCANFree" is not found in the DLL',
                                        mtError, [mbOK]);
      FreeLibrary(LibHandle);
      DLLLoadingError:= true;
    end;
    // now K-means
    KMeansClustering:= TKMeansClustering(GetProcAddress(LibHandle, 'KMeansClustering'));
    if not Assigned(KMeansClustering) then
    begin
      errorDialog:= CreateMessageDialog('The function "KMeansClustering" is not found in the DLL',
                                        mtError, [mbOK]);
      FreeLibrary(LibHandle);
      DLLLoadingError:= true;
    end;
    KMeansFree:= TKMeansFree(GetProcAddress(LibHandle, 'KMeansFree'));
    if not Assigned(KMeansFree) then
    begin
      errorDialog:= CreateMessageDialog('The function "KMeansFree" is not found in the DLL',
                                        mtError, [mbOK]);
      FreeLibrary(LibHandle);
      DLLLoadingError:= true;
    end;
  end
  else
  begin
    errorDialog:= CreateMessageDialog('IteridenseClustering could be started because'
                  + LineEnding
                  + 'the DLL "IteridenseCLib.dll" could not be loaded', mtError, [mbOK]);
    DLLLoadingError:= true;
  end;

  if DLLLoadingError then
  begin
    try
      errorDialog.Position:= poScreenCenter;
      errorDialog.FormStyle:= fsStayOnTop;   // to keep it above other windows
      errorDialog.ShowModal;
    finally
      errorDialog.Free;
    end;
    Application.Terminate;
  end;

  // initialize Julia runtime
  args[0]:= PAnsiChar(AnsiString('MyProgram'));
  args[1]:= nil;
  InitJulia(1, @args[0]);

  // assure that MethodsPC will have 2 header lines with each 2 tabs
  // we therefore set its TabWidth a bit below half of its Width
  MethodsPC.TabWidth:= MethodsPC.Width div 2 - 3;

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
    DensityTB.Enabled:= true;
    MinClustersSE.Enabled:= false;
  end
  else
  begin
    DensityFSE.Enabled:= false;
    DensityTB.Enabled:= false;
    MinClustersSE.Enabled:= true;
  end;
end;


// Note: the following 2 functions CArrayToTIntArray and CArrayToTDoubleArray
// cannot be merged as of FreePascal 3.2 since single generic functions are not yet supported.
// As they are supported in Delphi, this might be supported with future Freepascal versions.
// function to convert a C-array to an array of Int64
function TMainForm.CArrayToTIntArray(cArray: CArray): TIntArray;
var
  i: Int64;
  arrayPointer: PInt64;
begin
  result:= [];
  Setlength(result, cArray.length);
  arrayPointer:= PInt64(cArray.data);
  for i:= 0 to High(result) do
    result[i]:= arrayPointer[i];
end;

// function to convert a C-array to an array of Double
function TMainForm.CArrayToTDoubleArray(cArray: CArray): TDoubleArray;
var
  i: Int64;
  arrayPointer: PDouble;
begin
  result:= [];
  Setlength(result, cArray.length);
  arrayPointer:= PDouble(cArray.data);
  for i:= 0 to High(result) do
    result[i]:= arrayPointer[i];
end;


// function to convert a C-tensor to an array of Double
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
  counter, row, column, i, k, assignmentColumn, nrows, ncols, decision : Int64;
  iteridenseResult : PIteridenseResultC;
  DBSCANResult : PDBSCANResultC;
  kMeansResult : PKMeansResultC;
  inputArray : array of Double;
  dimensionIndices : array of Int64;
  assignmentsArray, clusterSizesArray : TIntArray;
  clusterDensitiesArray, clusterCentersArray : TDoubleArray;
  rows, columns : cint64;
  necessaryRAM, availableRAM : float;
  tempString : String;
begin
  // initialization
  SetLength(assignmentsArray, Length(DataArray));
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
  for column:= 0 to Length(DataArray[0])-1-1 do // -1 to not take assignment column into account
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

  // we execute the clustering according to the currently open methods tab:

  // Iteridense
  if MethodsPC.ActivePage.Caption = 'Iteridense' then
  begin

    // warn if there is not enough RAM available
    // * 2 because we have to create 2 tensors, the count tensor and the cluster tensor
    // * 4 because we use Float32 which has 4 bytes
    necessaryRAM:= power(StopResolutionSE.Value, columns) * 2 * 4;
    availableRAM:= FreeMemoryInBytes();
    // 95% of the availableRAM because we need space for
    // for Julia and other tasks the OS might perform
    if 0.95*availableRAM < necessaryRAM then
    begin
      necessaryRAM:= (1/0.475*necessaryRAM) / availableRAM;
      // we only round smaller numbers
      if necessaryRAM < 1e8 then
        necessaryRAM:= roundTo(necessaryRAM, -2);
      decision:= QuestionDlg('Warning', 'The clustering might stop early or fail!' + LineEnding
                 + LineEnding + 'The currently available RAM is '
                 + FloatToStr(trunc(availableRAM/1e6)) + ' MB.' + LineEnding
                 + 'To perform a clustering in ' + IntToStr(columns) + ' dimensions '
                 + 'with a maximal resolution of ' + IntToStr(StopResolutionSE.Value)
                 + ' would require ' + FloatToStr(necessaryRAM)
                 + ' times more RAM.' + LineEnding + LineEnding
                 + 'Do you like to try to cluster anyway?',
                 mtWarning, [mrYes, 'Yes', mrNo, 'No', 'IsDefault'], 0);
      if decision = mrNo then
        exit;
    end;

    // the clustering
    iteridenseResult:= IteridenseClustering(
      @inputArray[0], // pointer to first element
      rows,
      columns,
      DensityFSE.Value,
      MinClustersSE.Value,
      MinClusterSizeIterIdenseSE.Value,
      StartResolutionSE.Value,
      StopResolutionSE.Value,
      MinClusterDensityFSE.Value,
      UseDensityRB.Checked.ToInteger,
      UseClustersRB.Checked.ToInteger,
      NoDiagonalsCB.Checked.ToInteger );
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
      // free the allocated struct
      if IteridenseFree(iteridenseResult) <> 0 then
        MessageDlg('Warning: failed to free iteridenseResult', mtError, [mbOK], 0);
      exit;
    end;
    // convert c-arrays to Pascal arrays
    assignmentsArray:= CArrayToTIntArray(iteridenseResult^.assignments);
    clusterDensitiesArray:= CArrayToTDoubleArray(iteridenseResult^.clusterDensities);
    clusterSizesArray:= CArrayToTIntArray(iteridenseResult^.clusterSizes);

    if iteridenseResult^.finalResolution = 1 then
    begin
      // then no clustering was performed
      QuestionDlg('Error', 'Due to lack of RAM, no clustering could be performed.',
                   mtError, [mbOK, 'OK'], 0);
      if IteridenseFree(iteridenseResult) <> 0 then
        MessageDlg('Warning: failed to free iteridenseResult', mtError, [mbOK], 0);
      exit;
    end;

    FinalResolutionLE.Text:= IntToStr(iteridenseResult^.finalResolution);
    // fill the arrays to ClusterResultSG
    ClusterResultSG.RowCount:= Length(clusterSizesArray) + 1; // +1 for header row
    ClusterResultSG.Columns.Items[2].Title.Caption:= 'Density';
    for i:= 0 to High(clusterSizesArray) do
    begin
      ClusterResultSG.Cells[0, i+1]:= IntToStr(i+1);
      ClusterResultSG.Cells[1, i+1]:= IntToStr(clusterSizesArray[i]);
      ClusterResultSG.Cells[2, i+1]:= Format('%.3g', [clusterDensitiesArray[i]]);
    end;
    // free the allocated struct
    if IteridenseFree(iteridenseResult) <> 0 then
      MessageDlg('Warning: failed to free iteridenseResult', mtError, [mbOK], 0);
  end

  // DBSCAN
  else if MethodsPC.ActivePage.Caption = 'DBSCAN' then
  begin
    DBSCANResult:= DBSCANClustering(
      @inputArray[0], // pointer to first element
      rows,
      columns,
      RadiusFSE.Value,
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
     ClusterResultSG.Columns.Items[2].Title.Caption:= '';
    for i:= 0 to High(clusterSizesArray) do
    begin
      ClusterResultSG.Cells[0, i+1]:= IntToStr(i+1);
      ClusterResultSG.Cells[1, i+1]:= IntToStr(clusterSizesArray[i]);
      ClusterResultSG.Cells[2, i+1]:= '';
    end;
    // free the allocated struct
    if DBSCANFree(DBSCANResult) <> 0 then
      MessageDlg('Warning: failed to free DBSCANResult', mtError, [mbOK], 0);
  end

  // K-Means
  else if MethodsPC.ActivePage.Caption = 'K-Means' then
  begin
    kMeansResult:= KMeansClustering(
      @inputArray[0], // pointer to first element
      rows,
      columns,
      NumberClustersSE.Value,
      MaxIterationsSE.Value,
      ToleranceFSX.Value );
    // test if there is a result
    if kMeansResult = nil then
    begin
      MessageDlg('Failed to create kMeansResult', mtError, [mbOK], 0);
      exit;
    end;
    if (kMeansResult^.assignments.data = nil)
      or (kMeansResult^.assignments.length = 0) then
    begin
      MessageDlg('Clustering failed, no assignments available', mtError, [mbOK], 0);
      exit;
    end;
    // convert c-arrays to Pascal arrays
    assignmentsArray:= CArrayToTIntArray(kMeansResult^.assignments);
    clusterSizesArray:= CArrayToTIntArray(kMeansResult^.clusterSizes);
    clusterCentersArray:= CTensorToTDoubleArray(kMeansResult^.clusterCenters);
    // for convert clusterSizesArray to a matrix
    nrows:= kMeansResult^.clusterCenters.dims[0];
    ncols:= kMeansResult^.clusterCenters.dims[1];
    SetLength(KMeansClusterCenters, nrows, ncols);
    counter:= 0;
    // the output is column-major
    for columns:= 0 to ncols-1 do
      for rows:= 0 to nrows-1 do
        begin
          KMeansClusterCenters[rows][columns]:= clusterCentersArray[counter];
          inc(counter);
        end;
    // fill the arrays to ClusterResultSG
    ClusterResultSG.RowCount:= Length(clusterSizesArray) + 1; // +1 for header row
    ClusterResultSG.Columns.Items[2].Title.Caption:= 'Center at';
    for i:= 0 to High(clusterSizesArray) do
    begin
      ClusterResultSG.Cells[0, i+1]:= IntToStr(i+1);
      ClusterResultSG.Cells[1, i+1]:= IntToStr(clusterSizesArray[i]);
      tempString:= '';
      for k:= 0 to High(KMeansClusterCenters[0]) do
      begin
        if k = 0 then
          tempString:= tempString + '(';
        tempString:= tempString + Format('%.2f', [KMeansClusterCenters[i][k]]);
        if k < High(KMeansClusterCenters[0]) then
          tempString:= tempString + ', ';
        if k = High(KMeansClusterCenters[0]) then
          tempString:= tempString + ')';
      end;
      ClusterResultSG.Cells[2, i+1]:= tempString;
    end;
    // free the allocated struct
    if KMeansFree(kMeansResult) <> 0 then
      MessageDlg('Warning: failed to free kMeansResult', mtError, [mbOK], 0);
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
  DummyString : string;
  fileSuccess : Byte = 0;
  MousePointer : TPoint;
  i, assignmentColumn, counter : Int64;
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
      LoadedDataFileLE.Hint:= DropfileName
    else
      LoadedDataFileLE.Hint:= DummyString;
    // display file name without suffix
    DummyString:= ExtractFileName(InNameData);
    SetLength(DummyString, Length(DummyString) - 4);
    LoadedDataFileLE.Color:= clActiveCaption;
    LoadedDataFileLE.Text:= DummyString;
  end; // else if fileSuccess

  // add a column with zeros to DataArray to store there later the assignments
  assignmentColumn:= length(DataArray[0]);
  SetLength(DataArray, high(DataArray)+1, assignmentColumn+1);
  for i:= 0 to high(DataArray) do
    DataArray[i][assignmentColumn]:= 0;
  // we must also adapt DataTextColumnsIndices accordingly
  Setlength(DataTextColumnsIndices, assignmentColumn+1);
  // output number of data points
  NumDataPointsLE.Text:= IntToStr(high(DataArray)+1);

  // plot the data
  ChartData.CDPlotSelectionCCBItemChange(Sender);

  // uncheck all but maximal the first 2 items in PlotSelectionCCB
  // we do that first here since the change will trigger
  // ChartData.CDPlotSelectionCCBItemChange and we first needed to have a plot
  counter:= 0;
  for i:= 0 to PlotSelectionCCB.Items.Count-1 do
  begin
    // select maximal 2 for the plot as we don't allow 3D plots
    if (PlotSelectionCCB.ItemEnabled[i] = false) or (counter > 1) then
      PlotSelectionCCB.Checked[i]:= false;
    if (PlotSelectionCCB.ItemEnabled[i] = true) and (counter < 2) then
      inc(counter);
  end;

  // enable/disable objects
  ClusteringBB.Enabled:= true;
  FlipTB.Enabled:= true;
  SaveCsvMI.Enabled:= false;
  SavePlotBB.Enabled:= true;
  SavePlotMI.Enabled:= true;
  DensityTB.Enabled:= true;
  RadiusTB.Enabled:= true;
  if MethodsPC.ActivePage.Caption = 'Iteridense' then
    IteridenseSliderGB.Enabled:= true;
  // reset cluster method
  UsedClusteringMethod:= ClusterMethods.none;
  // reset density
  // as resetting the slider triggers DensityTBChange, set SliderIncrement to zero
  SliderIncrement:= 0;
  DensityTB.Position:= DensityTB.Min;
  DensityFSE.Value:= 1.1;
  // reset resolution
  // the slider must be set first because it will change the others
  IteridenseSliderTB.Position:= 2;
  StartResolutionSE.Value:= 2;
  StopResolutionSE.Value:= 100;
  FinalResolutionLE.Text:= '';
  // propose a minimal cluster size of 5 % of the total clusters
  MinClusterSizeIterIdenseSE.Value:= Trunc(0.05 * Length(DataArray));
  MinClusterSizeDBscanSE.Value:= Trunc(0.05 * Length(DataArray));
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
  // display the first enabled and checked item in DataSelectionCCB
  for i:= 0 to MainForm.DataSelectionCCB.Items.Count-1 do
  begin
    if (DataSelectionCCB.ItemEnabled[i])
     and (DataSelectionCCB.Checked[i]) then
    begin
      DataSelectionCCB.ItemIndex:= i;
      break;
    end;
  end;
  // check if there is at least one selected dimension
  for i:= 0 to DataSelectionCCB.Items.Count-1 do
  begin
    if (DataSelectionCCB.Checked[i]) then
    begin
      // enable ClusteringBB according to the MethodsPC state
      MethodsPCChange(Sender);
      ClusteringBB.Hint:= 'click to start the Iteridense clustering';
      exit;
    end;
  end;
  // no dimension, thus disable clustering
  ClusteringBB.Enabled:= false;
  ClusteringBB.Hint:= 'no dimensions selected to be clustered';
  IteridenseSliderGB.Enabled:= false;
end;

procedure TMainForm.DensityFSEChange(Sender: TObject);
var
  increment : Double;
begin
  if IderidenseDensityBySlider then
     exit;
  increment:= SliderIncrement;
  // as changing slider position triggers DensityTBChange, set SliderIncrement to zero
  SliderIncrement:= 0;
  // if increased, set position to Min otherwise to Max
  if DensityFSE.Value > IderidenseDensity then
    DensityTB.Position:= DensityTB.Min;
  if DensityFSE.Value < IderidenseDensity then
    DensityTB.Position:= DensityTB.Max;
  // save new density
  IderidenseDensity:= DensityFSE.Value;
  SliderIncrement:= increment;
end;

procedure TMainForm.IteridenseSliderTBChange(Sender: TObject);
begin
  StartResolutionSE.Value:= IteridenseSliderTB.Position;
  StopResolutionSE.Value:= IteridenseSliderTB.Position;
  // cluster only if allowed
  if ClusteringBB.Enabled then
    ClusteringBBClick(Sender);
end;

procedure TMainForm.IteridenseSliderTBMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LabelHintB.Visible:= true;
  UpdateLabelHintBPosition;
end;

procedure TMainForm.IteridenseSliderTBMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    UpdateLabelHintBPosition;
  end;
end;

procedure TMainForm.IteridenseSliderTBMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LabelHintB.Visible:= false;
end;

procedure TMainForm.UpdateLabelHintBPosition;
var
  mousePointer : TPoint;
begin
  // update button caption with current position
  LabelHintB.Caption:= IntToStr(IteridenseSliderTB.Position);

  // convert to screen coordinates
  mousePointer:= Mouse.CursorPos;;
  mousePointer:= MainForm.ScreenToClient(mousePointer);

  // position label above the thumb
  LabelHintB.Left:= mousePointer.X - LabelHintB.Width div 2;
  LabelHintB.Top:= IteridenseSliderGB.Top + 8 - LabelHintB.Height div 2;
end;

procedure TMainForm.DensityTBChange(Sender: TObject);
var
  sign : Integer;
begin
  // the idea is to trunc(density), divide it by 10 and map this to the slider as increment
  // and perform a clustering on every slider movement

  // at first determine the movement direction by setting the sign of increment
  sign:= DensityTB.Position - SliderPosition;
  // only set new increment if previous position was 1
  if (sign > 0) and (DensityTB.Position = 2) then
  begin
    SliderIncrement:= Trunc(DensityFSE.Value) / 10;
    // we can also have the case that DensityFSE.Value < 0
    if SliderIncrement = 0.0 then
       SliderIncrement:= 0.1;
  end;

  // cluster only if there was actually a change (not on e.g. slider resets)
  if SliderIncrement > 0 then
  begin
    IderidenseDensityBySlider:= true;
    DensityFSE.Value:= DensityFSE.Value + sign * SliderIncrement;
    IderidenseDensity:= DensityFSE.Value;
    IderidenseDensityBySlider:= false;
    ClusteringBBClick(Sender);
  end;
  // save new position
  SliderPosition:= DensityTB.Position;
end;

procedure TMainForm.FlipTBChange(Sender: TObject);
begin
  ChartData.CDFlipTBChange(Sender);
end;

procedure TMainForm.RadiusFSEChange(Sender: TObject);
var
  increment : Double;
begin
  if DBSCANRadiusBySlider then
     exit;
  increment:= SliderIncrement;
  // as changing slider position triggers DensityTBChange, set SliderIncrement to zero
  SliderIncrement:= 0;
  // if increased, set position to Min otherwise to Max
  if RadiusFSE.Value > DBSCANRadius then
    RadiusTB.Position:= RadiusTB.Min;
  if RadiusFSE.Value < DBSCANRadius then
    RadiusTB.Position:= RadiusTB.Max;
  // save new radius
  DBSCANRadius:= RadiusFSE.Value;
  SliderIncrement:= increment;
end;

procedure TMainForm.RadiusTBChange(Sender: TObject);
var
  sign : Integer;
begin
  // the idea is to trunc(density), divide it by 10 and map this to the slider as increment
  // and perform a clustering on every slider movement

  // at first determine the movement direction by setting the sign of increment
  sign:= RadiusTB.Position - SliderPosition;
  // only set new increment if previous position was 1
  if (sign > 0) and (RadiusTB.Position = 2) then
  begin
    SliderIncrement:= Trunc(RadiusFSE.Value) / 10;
    // since the radius could be < 0, set the increment to at least 0.1
    if SliderIncrement = 0.0 then
      SliderIncrement:= 0.1;
  end;

  // cluster only if there was actually a change (not on e.g. slider resets)
  if SliderIncrement > 0 then
  begin
    DBSCANRadiusBySlider:= true;
    RadiusFSE.Value:= RadiusFSE.Value + sign * SliderIncrement;
    DBSCANRadius:= RadiusFSE.Value;
    DBSCANRadiusBySlider:= false;
    ClusteringBBClick(Sender);
  end;
  // save new position
  SliderPosition:= RadiusTB.Position;
end;

procedure TMainForm.PlotSelectionCCBItemChange(Sender: TObject; AIndex: Integer);
begin
  ChartData.CDPlotSelectionCCBItemChange(Sender);
end;

procedure TMainForm.SliderStartSEEditingDone(Sender: TObject);
begin
  // assure it is below minimum and set slider accordingly
  if SliderStartSE.Value >= SliderStopSE.Value then
     SliderStartSE.Value:= SliderStopSE.Value - 1;
  IteridenseSliderTB.Min:= SliderStartSE.Value;
end;

procedure TMainForm.SliderStopSEEditingDone(Sender: TObject);
begin
  // assure it is below minimum and set slider accordingly
  if SliderStopSE.Value <= SliderStartSE.Value then
     SliderStopSE.Value:= SliderStartSE.Value + 1;
  IteridenseSliderTB.Max:= SliderStopSE.Value;
end;

procedure TMainForm.AutoscaleMIClick(Sender: TObject);
begin
  ChartData.CDAutoscaleMIClick(Sender);
end;

procedure TMainForm.ProportionalMIClick(Sender: TObject);
begin
  ChartData.CDProportionalMIClick(Sender);
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
    // only enable if data was loaded
    if Length(DataArray) > 0 then
      ClusteringBB.Enabled:= true;
  // reset sliders
  SliderIncrement:= 0;
  DensityTB.Position:= 1;
  RadiusTB.Position:= 1;
  // enable iteridense slider
  if MethodsPC.ActivePage.Caption = 'Iteridense' then
   IteridenseSliderGB.Enabled:= true
  else
   IteridenseSliderGB.Enabled:= false;
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

