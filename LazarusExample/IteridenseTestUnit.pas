unit IteridenseTestUnit;

{$mode objfpc}{$H+}{$R+}{$Q+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  EditBtn, ExtCtrls, ComCtrls, Spin, Math, CTypes,
  TATransformations, TATools, TAGraph, TASeries, TAChartAxis, TALegend, TATextElements,
  TATypes, TAChartUtils,
  FileInfo, StrUtils, Streamex, Generics.Collections;

const
  MAX_DIMS = 256;

type

  // C-compatible tensor struct
  CTensor = record
    data : Pointer;       // void* data
    ndims : cint;         // int ndims
    dims : array[0..MAX_DIMS-1] of csize_t; // size_t dims[MAX_DIMS]
  end;

  // C-compatible IteridenseResult struct
  IteridenseResultC = record
    clusterTensor : CTensor;
    countTensor : CTensor;
    numOfClusters : cint;
    finalResolution : cint;
    assignments : CTensor;
    clusterDensities : CTensor;
    clusterSizes : CTensor;
  end;
  PIteridenseResultC = ^IteridenseResultC;

  TInitJulia = procedure(argc: Integer; argv: PPAnsiChar); cdecl;
  TShutdownJulia = procedure(retcode: Integer); cdecl;
  // allocates and computes the IteridenseResult
  // returns a pointer to a heap-allocated IteridenseResultC
  // caller must free with IteridenseFree
  // returns nil on failure
  TIteridenseClustering = function(): PIteridenseResultC; cdecl;
  // frees memory allocated by TIteridenseClustering
  // returns 0 on success, -1 if ptr is nil
  TIteridenseFree = function(pointer: PIteridenseResultC): Integer; cdecl;
  { TMainF }

  TMainF = class(TForm)
    AxisClickTool: TAxisClickTool;
    TopLine: TConstantLine;
    BottomLine: TConstantLine;
    LeftLine: TConstantLine;
    RightLine: TConstantLine;
    ChartAxisTransformTemp: TChartAxisTransformations;
    ChartAxisTransformValues: TChartAxisTransformations;
    ChartToolset: TChartToolset;
    ColorDialog: TColorDialog;
    DataPointClickTool: TDataPointClickTool;
    DataPointCrosshairTool: TDataPointCrosshairTool;
    DataPointHintTool: TDataPointHintTool;
    DataPointMarksClickTool: TDataPointMarksClickTool;
    FloatSpinEdit1: TFloatSpinEdit;
    FloatSpinEdit2: TFloatSpinEdit;
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
    TempAutoScaleAxisTransform: TAutoScaleAxisTransform;
    TitleFootClickTool: TTitleFootClickTool;
    ValuesAutoScaleAxisTransform: TAutoScaleAxisTransform;
    ZoomDragTool: TZoomDragTool;
    ZoomMouseWheelTool: TZoomMouseWheelTool;
    function FontStylesToString(FontStyles: TFontStyles): string;
    function StringToFontStyles(s: string): TFontStyles;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure IteridenseBBClick(Sender: TObject);
    procedure OpenBBClick(Sender: TObject);
    function ReadData(InName: string): Boolean;
    function OpenHandling(InName: string; FileExt: string): string;
    function SaveHandling(InName: string; FileExt: string): string;
    procedure SaveAppearance(iniFile: string);
    procedure LoadAppearance(iniFile: string);
    procedure FormDropFiles(Sender: TObject; const FileNames{%H-}: array of String);
  private
  public
  end;

var
  MainF : TMainF;
  Version : string = '';
  LibHandle : THandle;
  IteridenseClustering : TIteridenseClustering;
  IteridenseFree : TIteridenseFree;
  InitJulia : TInitJulia;
  ShutdownJulia : TShutdownJulia;
  DLLPath : String;
  DropfileName : string = ''; // name of dropped CSV file
  InNameData : string = ''; // name of loaded data CSV file
  DataArray : Array of Array of double; // array that holds the data to be clustered
  ClusteredDataArray : Array of Array of double; // array that holds the clustered data
  // filename to store appearance
  const AppearanceFile : string = 'Appearance-IteridenseTest.ini';
  // filename with default appearance
  const AppearanceDefault : string = 'Appearance-IteridenseTest.default';
  // a palette with distinguishable colors
  colorPalette: array[0..2] of TColor = (
    $7F7F7F,      // RGB(127,127,127)
    $00FF7F,     // RGB(0,127,255)
    $7F007F      // RGB(127,127,127)
  );

implementation

{$R *.lfm}

procedure TMainF.FormCreate(Sender: TObject);
var
  iniFile : string;
  FileVerInfo: TFileVersionInfo;
  i : integer;
begin
  try
    FileVerInfo:= TFileVersionInfo.Create(nil);
    FileVerInfo.ReadFileInfo;
    Version:= FileVerInfo.VersionStrings.Values['ProductVersion'];
  finally
    FileVerInfo.Free;
  end;
  MainF.Caption:= Application.Title + ' ' + Version;
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
  //if FileExists(iniFile) then
  //  LoadAppearance(iniFile)

end;


procedure TMainF.FormDropFiles(Sender: TObject;
const
  FileNames: array of String);
begin
  DropfileName:= FileNames[0];
  OpenBBClick(Sender);
  DropfileName:= '';
end;


procedure TMainF.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  iniFile : string;
begin
  // shutdown Julia runtime with exit code 0 (success)
  //ShutdownJulia(0);
  // unload the DLL
  //FreeLibrary(LibHandle);
  // save the current chart appearance settings
  // we write into the same folder than the program .exe
  iniFile:= ExtractFilePath(Application.ExeName) + AppearanceFile;
  //SaveAppearance(iniFile);
end;



function TMainF.FontStylesToString(FontStyles: TFontStyles): string;
begin
  result := '';
  if fsBold in FontStyles then
   result:= result + IntToStr(Ord(fsBold)) + ',';
  if fsItalic in FontStyles then
   result:= result + IntToStr(Ord(fsItalic)) + ',';
  if fsUnderline in FontStyles then
   result:= result + IntToStr(Ord(fsUnderline)) + ',';
  if fsStrikeOut in FontStyles then
   result:= result + IntToStr(Ord(fsStrikeOut)) + ',';
  RemoveTrailingChars(result, [',']);
end;


procedure TMainF.IteridenseBBClick(Sender: TObject);
var
  dataPointer : PDouble;
  val, i, count : LongInt;
  randomNumber : Double;
  textLine : String;
  iteridenseResult : PIteridenseResultC;
begin

  iteridenseResult:= IteridenseClustering();
  if iteridenseResult = nil then
  begin
    MessageDlg('Failed to create iteridenseResult', mtError, [mbOK], 0);
    exit;
  end;

  if (iteridenseResult^.clusterTensor.data = nil)
    or (iteridenseResult^.clusterTensor.ndims = 0) then
  begin
    //TextOutputM.Lines.Add('No clusterTensor data available.');
    exit;
  end;

  // calculate total number of elements (product of dims)
  count:= 1;
  for i:= 0 to iteridenseResult^.clusterTensor.ndims - 1 do
    count:= count * Integer(iteridenseResult^.clusterTensor.dims[i]);
  //if count < 3 then
    //TextOutputM.Lines.Add('clusterTensor has less than 3 elements.');
  // cast data pointer to PDouble
  dataPointer := PDouble(iteridenseResult^.clusterTensor.data);

  // output first 3 elements (or fewer if not enough)
  for i:= 0 to min(2, count - 1) do
  begin
    textLine:= Format('clusterTensor[%d] = %.6f', [i+1, dataPointer[i]]);
    //TextOutputM.Lines.Add(textLine);
  end;

  // free the allocated struct
  if IteridenseFree(iteridenseResult) <> 0 then
    MessageDlg('Warning: failed to free iteridenseResult', mtError, [mbOK], 0);
end;


procedure TMainF.OpenBBClick(Sender: TObject);
var
  DummyString, firstLine, secondLine : string;
  FileSuccess : Boolean = false;
  MousePointer : TPoint;
  i, idx, clusterCount, newColumns : Integer;
  Series : array of TLineSeries;
begin
  MousePointer:= Mouse.CursorPos; // store mouse position
  DummyString:= '';

  if DropfileName <> '' then // a file was dropped into the program
    FileSuccess:= ReadData(DropfileName)
  else
  begin
    DummyString:= OpenHandling('', '.csv');
    if DummyString = '' then
      exit; // user aborted the loading
    FileSuccess:= ReadData(DummyString);
  end;

  if not FileSuccess then
  begin
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
    SaveDialog.FileName:= ''; // will be re-set in SaveHandling()
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
  end; // else if not FileSuccess

  // plot the data
  // first delete existing data
  for i := DataC.SeriesCount - 1 downto 0 do
  begin
    DataC.Series[i].Free;
  end;
  clusterCount:= 3;
  Randomize; // Initialize random number generator
  // add a column with random numbers 0, 1 or 2
  newColumns:= length(DataArray[0]) + 1;
  SetLength(DataArray, high(DataArray), newColumns);
  for i:= 0 to high(DataArray) do
    DataArray[i][newColumns-1]:= Random(clusterCount);


  // Create a series for each cluster
  SetLength(Series, clusterCount);
  for i:= 0 to clusterCount - 1 do
  begin
    Series[i]:= TLineSeries.Create(DataC);
    Series[i].ShowLines:= False; // Points only
    Series[i].Pointer.Visible:= True;
    Series[i].Pointer.Brush.Color:= colorPalette[i]; // Assign a unique color for each cluster
    DataC.AddSeries(Series[i]);
  end;

  // Add data points to the correct series
  for i:= 0 to high(DataArray) do
  begin
    idx := round(DataArray[i, 2]); // clusterID
    Series[idx].AddXY(DataArray[i, 0], DataArray[i, 1]);
  end;
end;


function TMainF.ReadData(InName: string): Boolean;
var
  columnSeparator : char;
  firstLine, secondLine, textLine : string;
  OpenFileStream : TFileStream;
  LineReader : TStreamReader;
  StringArray : TStringArray;
  rowCounter : integer = 0;
  MousePointer : TPoint;
  List : specialize TList<TStringArray>;
  i, j, columns : integer;
begin
  Result:= false;
  MousePointer:= Mouse.CursorPos; // store mouse position
  // read the file into an array
  try
    OpenFileStream:= TFileStream.Create(InName, fmOpenRead or fmShareDenyNone);
  except
    on EFOpenError do
    begin
      MessageDlgPos('Data file is used by another program and cannot be opened.',
                 mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
      exit;
    end;
  end;
 try
  LineReader:= TStreamReader.Create(OpenFileStream);

  // read the first header line to get the data names and the column separator
  LineReader.ReadLine(firstLine);
  // read second line to check for column separator
  LineReader.ReadLine(secondLine);
  inc(rowCounter);
  // we assume the second line is the first one with data, therefore we
  // parse the second line for the first character that is a potential column separator
  // we detect for these possible separators: ',', ';', #9, '|', ' '
  for i:= 1 to Length(secondLine) do
  begin
    if (secondLine[i] = ',') or (secondLine[i] = ' ') or (secondLine[i] = #9)
      or (secondLine[i] = ';') or (secondLine[i] = '|') then
    begin
      columnSeparator:= secondLine[i];
      break;
    end
  end;
  // if there are the same number of columnSeparator in first and second line we found the separator
  // otherwise we have to parse the second line
  if firstLine.CountChar(columnSeparator) <> secondLine.CountChar(columnSeparator) then
  begin
    for i:= 1 to Length(firstLine) do
    begin
      if (firstLine[i] = ',') or (firstLine[i] = ' ') or (firstLine[i] = #9)
        or (secondLine[i] = ';') or (secondLine[i] = '|') then
      begin
        columnSeparator:= firstLine[i];
        break;
      end
    end;
  end;
  // if still not found, we issue an  error and stop
  if firstLine.CountChar(columnSeparator) <> secondLine.CountChar(columnSeparator) then
  begin
    MessageDlg('The column separator of the data file could not be determined', mtError, [mbOK], 0);
    exit;
  end;

  // set the chart axis titles according to the header
  StringArray:= firstLine.Split(columnSeparator);
  DataC.AxisList[0].Title.Caption:= StringArray[0];
  DataC.AxisList[1].Title.Caption:= StringArray[1];
  // secondLine is the first row of the StringArray
  StringArray:= secondLine.Split(columnSeparator);
 try
  List:= specialize TList<TStringArray>.Create;
  List.Add(StringArray);
  // read file until end
  while not LineReader.Eof do
  begin
    LineReader.ReadLine(textLine);
    StringArray:= textLine.Split(columnSeparator);
    List.Add(StringArray);
    inc(rowCounter);
  end;

  // convert the string array to an array of double
  columns:= length(StringArray);
  setlength(DataArray, rowCounter, columns);
  for i:= 0 to List.Count-1 do
    for j:= 0 to columns-1 do
      begin
        if not TryStrToFloat(List[i][j], DataArray[i][j]) then
        begin
          MessageDlg('The value in column ' + IntToStr(i) + ' of line ' + IntToStr(j)
            + ' of the CSV file could not be converted to a number', mtError, [mbOK], 0);
          exit;
        end;
      end;
 finally
   List.Free;
 end;

  Result:= true;

 finally
  LineReader.Free;
  OpenFileStream.Free;
 end;

end;

function TMainF.OpenHandling(InName: string; FileExt: string): string;
// handles the open dialog
var
 OutNameTemp : string;
begin
  result:= '';
  if FileExt = '.csv' then
  begin
    OpenDialog.Filter:= 'Data file (*.csv)|*.csv';
    OpenDialog.Title:= 'Open data file';
  end;
  // propose a file name
  if (InName <> '') and (OpenDialog.FileName = '') then
    OpenDialog.FileName:= ExtractFileName(InName);
  // empty existing dialog file name if the extension does not match
  if ExtractFileExt(OpenDialog.FileName) <> FileExt then
    OpenDialog.FileName:= '';
  if OpenDialog.FileName <> '' then
    OpenDialog.FileName:= ExtractFileName(OpenDialog.FileName);
  if OpenDialog.Execute then
  begin
    OutNameTemp:= OpenDialog.FileName;
    // add file extension if it is missing
    if (ExtractFileExt(OutNameTemp) <> FileExt) then
      Insert(FileExt, OutNameTemp, Length(OutNameTemp) + 1);
    if not FileExists(OutNameTemp) then
    begin
      MessageDlg('The file does not exist!', mtError, [mbOK], 0);
      result:= '';
      exit;
    end;
    result:= OutNameTemp;
    // store last used name
    OpenDialog.FileName:= ExtractFileName(OutNameTemp);
  end
  else // was not executed for some reason
    result:= '';
end;


function TMainF.SaveHandling(InName: string; FileExt: string): string;
// handles the save dialog
var
  YesNo : integer;
  OutNameTemp, DialogText : string;
  MousePointer : TPoint;
begin
  // initialize
  MousePointer:= Mouse.CursorPos;
  result:= '';

  if FileExt = '.csv' then
  begin
    SaveDialog.Filter:= 'Table (*.csv)|*.csv';
    SaveDialog.Title:= 'Save data as';
  end;
  // clear filename if the extension does not fit
  if (SaveDialog.FileName <> '')
   and (ExtractFileExt(SaveDialog.FileName) <> FileExt) then
    SaveDialog.FileName:= '';
  // propose a file name
  if ((InName <> '') and (SaveDialog.FileName = ''))
   or ((InName <> '') and (SaveDialog.FileName <> '')
     and (ExtractFileExt(InName) <> ExtractFileExt(SaveDialog.FileName))) then
    SaveDialog.FileName:= ExtractFileName(InName);
  if SaveDialog.Execute then
  begin
    OutNameTemp:= SaveDialog.FileName;
    // add file extension if it is missing
    if (ExtractFileExt(OutNameTemp) <> FileExt) then
     Insert(FileExt, OutNameTemp, Length(OutNameTemp) + 1);

    if FileExists(OutNameTemp) then
    begin
      if FileExt = '.csv' then
        DialogText:= 'Do you want to overwrite the existing file';

      with CreateMessageDialog
        (DialogText + LineEnding
             + ExtractFileName(OutNameTemp) + ' ?',
             mtWarning, [mbYes]+[mbNo]) do
      try
        ActiveControl:= FindComponent('NO') as TWinControl;
        YesNo:= ShowModal;
      finally
        free;
      end;
      if YesNo = mrNo then // if No
      begin
        SaveHandling(InName, FileExt);
        exit;
      end
      else // if Yes
      begin
        result:= OutNameTemp;
        // store last used name
        SaveDialog.FileName:= ExtractFileName(OutNameTemp);
        exit;
      end;
    end; // end if FileExists

    result:= OutNameTemp;
    // store last used name
    SaveDialog.FileName:= ExtractFileName(OutNameTemp);
  end
  else // the user canceled the dialog
  begin
    if FileExists(InName) and (SaveDialog.FileName = InName) then
      result:= 'canceled';
  end;

end;


procedure TMainF.SaveAppearance(iniFile: string);
var
 i : integer;
 Chart : TChart;
 Axis : TChartAxis;
 Series : TLineSeries;
 tempStr : string;
 List : TStringList;
begin

try
 List:= TStringList.Create;

 Chart:= MainF.DataC;
 List.Add('Chart SIXCH');

 // Axes
 for i:= 0 to Chart.AxisList.Count-1 do
 begin
  Axis:= Chart.AxisList[i];
  List.Add('Axis ' + IntToStr(i));
  // purposely don't store the axis visibility and caption since
  // this can lead to confusion
  // Axis title
  WriteStr(tempStr, Axis.Title.Alignment);
  List.Add('Title.Alignment ' + tempStr);
  List.Add('Title.LabelFont.Name ' + Axis.Title.LabelFont.Name);
  List.Add('Title.LabelFont.Size ' + IntToStr(Axis.Title.LabelFont.Size));
  List.Add('Title.LabelFont.Color ' + ColorToString(Axis.Title.LabelFont.Color));
  List.Add('Title.LabelFont.Style ' + FontStylesToString(Axis.Title.LabelFont.Style));
  List.Add('Title.LabelFont.Orientation ' + IntToStr(Axis.Title.LabelFont.Orientation));
  List.Add('Title.Distance ' + IntToStr(Axis.Title.Distance));
  WriteStr(tempStr, Axis.Title.Shape);
  List.Add('Title.Shape ' + tempStr);
  List.Add('Title.LabelBrush.Color ' + ColorToString(Axis.Title.LabelBrush.Color));
  WriteStr(tempStr, Axis.Title.LabelBrush.Style);
  List.Add('Title.LabelBrush.Style ' + tempStr);
  List.Add('Title.Frame.Visible ' + BoolToStr(Axis.Title.Frame.Visible));
  List.Add('Title.Frame.Color ' + ColorToString(Axis.Title.Frame.Color));
  WriteStr(tempStr, Axis.Title.Frame.Style);
  List.Add('Title.Frame.Style ' + tempStr);
  List.Add('Title.Frame.Width ' + IntToStr(Axis.Title.Frame.Width));
  List.Add('Title.Margins.Left ' + IntToStr(Axis.Title.Margins.Left));
  List.Add('Title.Margins.Top ' + IntToStr(Axis.Title.Margins.Top));
  List.Add('Title.Margins.Right ' + IntToStr(Axis.Title.Margins.Right));
  List.Add('Title.Margins.Bottom ' + IntToStr(Axis.Title.Margins.Bottom));

  // Axis range
  // purposely don't store the range since this depends on the actual values
  List.Add('Inverted ' + BoolToStr(Axis.Inverted));

  // Tick labels
  List.Add('Marks.Visible ' + BoolToStr(Axis.Marks{%H-}.Visible));
  List.Add('Marks.Format ' + Axis.Marks{%H-}.Format);
  List.Add('Marks.Distance ' + IntToStr(Axis.Marks{%H-}.Distance));
  List.Add('TickLength ' + IntToStr(Axis.TickLength));
  List.Add('TickInnerLength ' + IntToStr(Axis.TickInnerLength));
  List.Add('TickColor ' + ColorToString(Axis.TickColor));
  List.Add('Marks.LabelFont.Name ' + Axis.Marks{%H-}.LabelFont.Name);
  List.Add('Marks.LabelFont.Size ' + IntToStr(Axis.Marks{%H-}.LabelFont.Size));
  List.Add('Marks.LabelFont.Color ' + ColorToString(Axis.Marks{%H-}.LabelFont.Color));
  List.Add('Marks.LabelFont.Style ' + FontStylesToString(Axis.Marks{%H-}.LabelFont.Style));
  WriteStr(tempStr, Axis.Marks{%H-}.Shape);
  List.Add('Marks.Shape ' + tempStr);
  List.Add('Marks.LabelBrush.Color ' + ColorToString(Axis.Marks{%H-}.LabelBrush.Color));
  WriteStr(tempStr, Axis.Marks{%H-}.LabelBrush.Style);
  List.Add('Marks.LabelBrush.Style ' + tempStr);
  List.Add('Marks.Frame.Visible ' + BoolToStr(Axis.Marks{%H-}.Frame.Visible));
  List.Add('Marks.Frame.Color ' + ColorToString(Axis.Marks{%H-}.Frame.Color));
  WriteStr(tempStr, Axis.Marks{%H-}.Frame.Style);
  List.Add('Marks.Frame.Style ' + tempStr);
  List.Add('Marks.Frame.Width ' + IntToStr(Axis.Marks{%H-}.Frame.Width));
  List.Add('Marks.Margins.Left ' + IntToStr(Axis.Marks{%H-}.Margins.Left));
  List.Add('Marks.Margins.Top ' + IntToStr(Axis.Marks{%H-}.Margins.Top));
  List.Add('Marks.Margins.Right ' + IntToStr(Axis.Marks{%H-}.Margins.Right));
  List.Add('Marks.Margins.Bottom ' + IntToStr(Axis.Marks{%H-}.Margins.Bottom));

  // Grid
  List.Add('Grid.Visible ' + BoolToStr(Axis.Grid.Visible));
  WriteStr(tempStr, Axis.Grid.Style);
  List.Add('Grid.Style ' + tempStr);
  List.Add('Grid.Width ' + IntToStr(Axis.Grid.Width));
  List.Add('Grid.Color ' + ColorToString(Axis.Grid.Color));

  // Frame
  List.Add('Frame.Visible ' + BoolToStr(Chart.Frame.Visible));
  WriteStr(tempStr, Chart.Frame.Style);
  List.Add('Frame.Style ' + tempStr);
  List.Add('Frame.Width ' + IntToStr(Chart.Frame.Width));
  List.Add('Frame.Color ' + ColorToString(Chart.Frame.Color));

  // Arrow
  List.Add('Arrow.Visible ' + BoolToStr(Axis.Arrow.Visible));
  List.Add('Arrow.BaseLength ' + IntToStr(Axis.Arrow.BaseLength));
  List.Add('Arrow.Length ' + IntToStr(Axis.Arrow.Length));
  List.Add('Arrow.Width ' + IntToStr(Axis.Arrow.Width));
 end;

 // Background
 List.Add('BackColor ' + ColorToString(Chart.BackColor));

 // Legend
 // purposely don't store the legend visibility
 WriteStr(tempStr, Chart.Legend.Alignment);
 List.Add('Legend.Alignment ' + tempStr);
 List.Add('Legend.ColumnCount ' + IntToStr(Chart.Legend.ColumnCount));
 List.Add('Legend.Inverted ' + BoolToStr(Chart.Legend.Inverted));
 WriteStr(tempStr, Chart.Legend.ItemFillOrder);
 List.Add('Legend.ItemFillOrder ' + tempStr);
 List.Add('Legend.MarginX ' + IntToStr(Chart.Legend.MarginX));
 List.Add('Legend.MarginY ' + IntToStr(Chart.Legend.MarginY));
 List.Add('Legend.Spacing ' + IntToStr(Chart.Legend.Spacing));
 List.Add('Legend.SymbolWidth ' + IntToStr(Chart.Legend.SymbolWidth));
 List.Add('Legend.UseSidebar ' + BoolToStr(Chart.Legend.UseSidebar));
 // Legend Brush
 List.Add('Legend.BackgroundBrush.Color '
  + ColorToString(Chart.Legend.BackgroundBrush.Color));
 WriteStr(tempStr, Chart.Legend.BackgroundBrush.Style);
 List.Add('Legend.BackgroundBrush.Style ' + tempStr);
 // Legend Font
 List.Add('Legend.Font.Color ' + ColorToString(Chart.Legend.Font.Color));
 List.Add('Legend.Font.Name ' + Chart.Legend.Font.Name);
 List.Add('Legend.Font.Orientation ' + IntToStr(Chart.Legend.Font.Orientation));
 List.Add('Legend.Font.Size ' + IntToStr(Chart.Legend.Font.Size));
 List.Add('Legend.Font.Style ' + FontStylesToString(Chart.Legend.Font.Style));
 // Legend Frame
 List.Add('Legend.Frame.Color ' + ColorToString(Chart.Legend.Frame.Color));
 WriteStr(tempStr, Chart.Legend.Frame.Style);
 List.Add('Legend.Frame.Style ' + tempStr);
 List.Add('Legend.Frame.Visible ' + BoolToStr(Chart.Legend.Frame.Visible));
 List.Add('Legend.Frame.Width ' + IntToStr(Chart.Legend.Frame.Width));

 // Series
 for i:= 0 to Chart.SeriesCount-5 do // omit the TConstantLines
 begin
  Series:= (Chart.Series[i]) as TLineSeries;
  List.Add('LineSeries ' + Series.Name);
  // we don't store the Title since this is set via the .def file
  // also don't show the Active state for the same reason

  // Legend
  List.Add('Legend.Visible ' + BoolToStr(Series.Legend.Visible));
  WriteStr(tempStr, Series.Legend.Multiplicity);
  List.Add('Legend.Multiplicity ' + tempStr);
  // Marks
  WriteStr(tempStr, Series.Marks.Style);
  List.Add('Marks.Style ' + tempStr);
  List.Add('Marks.Format ' + Series.Marks.Format);
  List.Add('Marks.LabelBrush.Color ' + ColorToString(Series.Marks.LabelBrush.Color));
  List.Add('Marks.LabelFont.Color ' + ColorToString(Series.Marks.LabelFont.Color));
  List.Add('Marks.Visible ' + BoolToStr(Series.Marks.Visible));
  // Lines
  List.Add('ShowLines ' + BoolToStr(Series.ShowLines));
  List.Add('SeriesColor ' + ColorToString(Series.SeriesColor));
  WriteStr(tempStr, Series.LinePen.Style);
  List.Add('LinePen.Style ' + tempStr);
  List.Add('LinePen.Width ' + IntToStr(Series.LinePen.Width));
  // Points
  List.Add('ShowPoints ' + BoolToStr(Series.ShowPoints));
  List.Add('Pointer.Brush.Color ' + ColorToString(Series.Pointer.Brush.Color));
  WriteStr(tempStr, Series.Pointer.Brush.Style);
  List.Add('Pointer.Brush.Style ' + tempStr);
  List.Add('Pointer.HorizSize ' + IntToStr(Series.Pointer.HorizSize));
  List.Add('Pointer.Pen.Color ' + ColorToString(Series.Pointer.Pen.Color));
  WriteStr(tempStr, Series.Pointer.Pen.Style);
  List.Add('Pointer.Pen.Style ' + tempStr);
  List.Add('Pointer.Pen.Width ' + IntToStr(Series.Pointer.Pen.Width));
  WriteStr(tempStr, Series.Pointer.Style);
  List.Add('Pointer.Style ' + tempStr);
 end;

 // Title
 // purposely don't store the title text since this can cause issues
 // when another user starts the program for another measurement
 WriteStr(tempStr, Chart.Title.Alignment);
 List.Add('Title.Alignment ' + tempStr);
 WriteStr(tempStr, Chart.Title.Shape);
 List.Add('Title.Shape ' + tempStr);
 List.Add('Title.Wordwrap ' + BoolToStr(Chart.Title.Wordwrap));
 // Title Brush
 List.Add('Title.Brush.Color '
  + ColorToString(Chart.Title.Brush.Color));
 WriteStr(tempStr, Chart.Title.Brush.Style);
 List.Add('Title.Brush.Style ' + tempStr);
 // Title Font
 List.Add('Title.Font.Color ' + ColorToString(Chart.Title.Font.Color));
 List.Add('Title.Font.Name ' + Chart.Title.Font.Name);
 List.Add('Title.Font.Orientation ' + IntToStr(Chart.Title.Font.Orientation));
 List.Add('Title.Font.Size ' + IntToStr(Chart.Title.Font.Size));
 List.Add('Title.Font.Style ' + FontStylesToString(Chart.Title.Font.Style));
 // Title Frame
 List.Add('Title.Frame.Color ' + ColorToString(Chart.Title.Frame.Color));
 WriteStr(tempStr, Chart.Title.Frame.Style);
 List.Add('Title.Frame.Style ' + tempStr);
 List.Add('Title.Frame.Visible ' + BoolToStr(Chart.Title.Frame.Visible));
 List.Add('Title.Frame.Width ' + IntToStr(Chart.Title.Frame.Width));
 // Title Margins
 List.Add('Title.Margins.Left ' + IntToStr(Chart.Title.Margins.Left));
 List.Add('Title.Margins.Top ' + IntToStr(Chart.Title.Margins.Top));
 List.Add('Title.Margins.Right ' + IntToStr(Chart.Title.Margins.Right));
 List.Add('Title.Margins.Bottom ' + IntToStr(Chart.Title.Margins.Bottom));

 // save the list
 List.SaveToFile(iniFile);

finally
 List.Free;
end;

end;


function TMainF.StringToFontStyles(s: string): TFontStyles;
var
 i : integer;
begin
 result:= [];
 for i:= 1 to WordCount(s, [',']) do
  result:= result + [TFontStyle(StrToInt(ExtractWord(i, s, [','])))];
end;


procedure TMainF.LoadAppearance(iniFile: string);
var
 i, m : integer;
 Chart : TChart;
 Axis : TChartAxis;
 Series : TLineSeries;
 List : TStringList;
 tempAlignment : TAlignment;
 tempShape : TChartLabelShape;
 tempBrushStyle : TBrushStyle;
 tempStyle : TPenStyle;
 tempLegendAlignment : TLegendAlignment;
 tempFillOrder : TLegendItemFillOrder;
 tempMultiplicity : TLegendMultiplicity;
 tempMarksStyle : TSeriesMarksStyle;
 tempPointerStyle : TSeriesPointerStyle;
 Abool : Boolean;
begin

 try
  List:= TStringList.Create;
  List.LoadFromFile(iniFile);
  m:= 0;

  // now read the chart properties
  Chart:= (MainF.FindComponent(
           Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length))
           as TChart);
  // Axes
  for i:= 0 to Chart.AxisList.Count-1 do
  begin
   inc(m);
   Axis:= Chart.AxisList[StrToInt(
           Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length)
          )];
   // Axis title
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempAlignment);
   Axis.Title.Alignment:= tempAlignment;
   inc(m);
   Axis.Title.LabelFont.Name:=
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length);
   inc(m);
   Axis.Title.LabelFont.Size:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Title.LabelFont.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Title.LabelFont.Style:= StringToFontStyles(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Title.LabelFont.Orientation:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Title.Distance:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempShape);
   Axis.Title.Shape:= tempShape;
   inc(m);
   Axis.Title.LabelBrush.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempBrushStyle);
   Axis.Title.LabelBrush.Style:= tempBrushStyle;
   inc(m);
   Axis.Title.Frame.Visible:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Title.Frame.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempStyle);
   Axis.Title.Frame.Style:= tempStyle;
   inc(m);
   Axis.Title.Frame.Width:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Title.Margins.Left:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Title.Margins.Top:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Title.Margins.Right:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Title.Margins.Bottom:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   // Axis range
   inc(m);
   Axis.Inverted:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   // Tick labels
   inc(m);
   Axis.Marks{%H-}.Visible:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Marks{%H-}.Format:= Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length);
   inc(m);
   Axis.Marks{%H-}.Distance:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.TickLength:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.TickInnerLength:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.TickColor:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Marks{%H-}.LabelFont.Name:=
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length);
   inc(m);
   Axis.Marks{%H-}.LabelFont.Size:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Marks{%H-}.LabelFont.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Marks{%H-}.LabelFont.Style:= StringToFontStyles(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempShape);
   Axis.Marks{%H-}.Shape:= tempShape;
   inc(m);
   Axis.Marks{%H-}.LabelBrush.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempBrushStyle);
   Axis.Marks{%H-}.LabelBrush.Style:= tempBrushStyle;
   inc(m);
   Axis.Marks{%H-}.Frame.Visible:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Marks{%H-}.Frame.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempStyle);
   Axis.Marks{%H-}.Frame.Style:= tempStyle;
   inc(m);
   Axis.Marks{%H-}.Frame.Width:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Marks{%H-}.Margins.Left:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Marks{%H-}.Margins.Top:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Marks{%H-}.Margins.Right:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Marks{%H-}.Margins.Bottom:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   // Grid
   inc(m);
   Axis.Grid.Visible:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempStyle);
   Axis.Grid.Style:= tempStyle;
   inc(m);
   Axis.Grid.Width:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Grid.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   // Frame
   inc(m);
   Chart.Frame.Visible:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempStyle);
   Chart.Frame.Style:= tempStyle;
   inc(m);
   Chart.Frame.Width:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Chart.Frame.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   // Arrow
   inc(m);
   Axis.Arrow.Visible:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Arrow.BaseLength:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Arrow.Length:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Arrow.Width:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  end;

  // Background
  inc(m);
  Chart.BackColor:= StringToColor(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));

  // Legend
  inc(m);
  ReadStr(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
   tempLegendAlignment);
  Chart.Legend.Alignment:= tempLegendAlignment;
  inc(m);
  Chart.Legend.ColumnCount:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Legend.Inverted:= StrToBool(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  ReadStr(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
   tempFillOrder);
  Chart.Legend.ItemFillOrder:= tempFillOrder;
  inc(m);
  Chart.Legend.MarginX:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Legend.MarginY:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Legend.Spacing:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Legend.SymbolWidth:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Legend.UseSidebar:= StrToBool(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  // Legend Brush
  inc(m);
  Chart.Legend.BackgroundBrush.Color:= StringToColor(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  ReadStr(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
   tempBrushStyle);
  Chart.Legend.BackgroundBrush.Style:= tempBrushStyle;
  // Legend Font
  inc(m);
  Chart.Legend.Font.Color:= StringToColor(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Legend.Font.Name:= Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length);
  inc(m);
  Chart.Legend.Font.Orientation:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Legend.Font.Size:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Legend.Font.Style:= StringToFontStyles(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  // Legend Frame
  inc(m);
  Chart.Legend.Frame.Color:= StringToColor(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  ReadStr(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
   tempStyle);
  Chart.Legend.Frame.Style:= tempStyle;
  inc(m);
  Chart.Legend.Frame.Visible:= StrToBool(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Legend.Frame.Width:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));

  // Series
  for i:= 0 to Chart.SeriesCount-5 do // omit the TConstantLines
  begin
   inc(m);
   Series:= (MainF.FindComponent(
             Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length))
             as TLineSeries);

   // Legend
   inc(m);
   Series.Legend.Visible:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempMultiplicity);
   Series.Legend.Multiplicity:= tempMultiplicity;
   // Marks
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempMarksStyle);
   Series.Marks.Style:= tempMarksStyle;
   inc(m);
   Series.Marks.Format:= Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length);
   inc(m);
   Series.Marks.LabelBrush.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Series.Marks.LabelFont.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Series.Marks.Visible:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   // Lines
   inc(m);
   Series.ShowLines:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Series.SeriesColor:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempStyle);
   Series.LinePen.Style:= tempStyle;
   inc(m);
   Series.LinePen.Width:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   // Points
   inc(m);
   Series.ShowPoints:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Series.Pointer.Brush.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempBrushStyle);
   Series.Pointer.Brush.Style:= tempBrushStyle;
   inc(m);
   Series.Pointer.HorizSize:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Series.Pointer.Pen.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempStyle);
   Series.Pointer.Pen.Style:= tempStyle;
   inc(m);
   Series.Pointer.Pen.Width:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempPointerStyle);
   Series.Pointer.Style:= tempPointerStyle;
  end;

  // Title
  inc(m);
  ReadStr(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
   tempAlignment);
  Chart.Title.Alignment:= tempAlignment;
  inc(m);
  ReadStr(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
   tempShape);
  Chart.Title.Shape:= tempShape;
  inc(m);
  Chart.Title.Wordwrap:= StrToBool(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  // Title Brush
  inc(m);
  Chart.Title.Brush.Color:= StringToColor(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  ReadStr(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
   tempBrushStyle);
  Chart.Title.Brush.Style:= tempBrushStyle;
  // Title Font
  inc(m);
  Chart.Title.Font.Color:= StringToColor(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Title.Font.Name:= Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length);
  inc(m);
  Chart.Title.Font.Orientation:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Title.Font.Size:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Title.Font.Style:= StringToFontStyles(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  // Title Frame
  inc(m);
  Chart.Title.Frame.Color:= StringToColor(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  ReadStr(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
   tempStyle);
  Chart.Title.Frame.Style:= tempStyle;
  inc(m);
  Chart.Title.Frame.Visible:= StrToBool(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Title.Frame.Width:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  // Title Margins
  inc(m);
  Chart.Title.Margins.Left:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Title.Margins.Top:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Title.Margins.Right:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Title.Margins.Bottom:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));

 finally
  List.Free;
 end;
end;



end.

