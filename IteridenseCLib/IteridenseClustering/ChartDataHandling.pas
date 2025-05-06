unit ChartDataHandling;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, EditBtn, ExtCtrls, ComCtrls, Spin, Math, CTypes,
  SpinEx, FileInfo, StrUtils, Streamex, Generics.Collections,
  TATools, TAGraph, TASeries, TAChartAxis, TALegend, TATextElements,
  TATypes, TAChartUtils, TADrawerSVG,
  IteridenseClusteringMain;

type
  TChartData = class
    constructor create;

  private

  public
    function FontStylesToString(FontStyles: TFontStyles): string;
    function StringToFontStyles(s: string): TFontStyles;
    function ReadData(InName: string): Byte;
    procedure FlipBClick(Sender: TObject);
    procedure CDPlotSelectionCCBItemChange(Sender: TObject);
    procedure CDDataPointHintToolHint(ATool: TDataPointHintTool; const APoint: TPoint;
                                    var AHint: String);
    procedure CDDataPointHintToolHintPosition(ATool: TDataPointHintTool; var APoint: TPoint);
    procedure CDAutoscaleMIClick(Sender: TObject);
    procedure CDChangeBackColorMIClick(Sender: TObject);
    procedure CDLegendClickToolClick(Sender: TChartTool; Legend: TChartLegend);
    procedure CDAxisClickToolClick(Sender: TChartTool;
                                    AnAxis: TChartAxis; HitInfo: TChartAxisHitTests);
    procedure CDTitleFootClickToolClick(Sender: TChartTool; Title: TChartTitle);
    function OpenHandling(InName: string; FileExt: string): string;
    function SaveHandling(InName: string; FileExt: string): string;
    function CDSavePlotBBClick(Sender: TObject; ChartName: string): Boolean;
    procedure CDSaveCsvMIClick(Sender: TObject);
    procedure SaveAppearance(iniFile: string);
    procedure LoadAppearance(iniFile: string);

    class var
     fileSuccess : Boolean; // if file operation was successful
     HeaderStrings : array [1..6] of string; // string for the output file header
     wasZoomDragged : Boolean; // if the user previously zoomDragged

  end;

var
  ChartData : TChartData;
  // filename to store appearance
  const AppearanceFile : string = 'Appearance-IteridenseClustering.ini';
  // filename with default appearance
  const AppearanceDefault : string = 'Appearance-IteridenseClustering.default';
  // a palette with distinguishable colors
  colorPalette: array[0..14] of TColor = (
    $ED9464, // RGB(100, 149, 237)
    $109FFF, // RGB(255, 159, 18)
    $38C88E, // RGB(142, 200, 58)
    $3531FF, // RGB(255, 49, 53)
    $F981FF, // RGB(255, 129, 249)
    $000000, // RGB(255, 108, 99)
    $A09FFF, // RGB(255, 159, 168)
    $2DECFF, // RGB(255, 236, 45)
    $FFEA4A, // RGB(74, 238, 255)
    $2121FF, // RGB(255, 33, 37)
    $F39EB1, // RGB(177, 158, 243)
    $6BDBF9, // RGB(249, 219, 98)
    $D662FF, // RGB(255, 98, 215)
    $009FFF, // RGB(255, 159, 0)
    $43FFFF  // RGB(255, 255, 67)
  );


implementation

uses
  // ChartEditing units
  ceTitleFootDlg, ceLegendDlg, ceSeriesDlg, ceAxisDlg, ceAxisFrame;

constructor TChartData.create;
begin
end;


function TChartData.ReadData(InName: string): Byte;
var
  firstLine, secondLine, textLine : string;
  OpenFileStream : TFileStream;
  LineReader : TStreamReader;
  StringArray : TStringArray;
  MousePointer : TPoint;
  List : specialize TList<TStringArray>;
  i, k, m, columns, columnCounter, counter : Int64;
begin
  result:= 0; // initialized as failed
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
  DataHeader:= firstLine; // store the header for later usage
  // read second line to check for column separator
  LineReader.ReadLine(secondLine);
  // we assume the second line is the first one with data, therefore we
  // parse the second line for the first character that is a potential column separator
  // we detect for these possible separators: ',', ';', #9, '|', ' '
  for i:= 1 to Length(secondLine) do
  begin
    if (secondLine[i] = ',') or (secondLine[i] = ' ') or (secondLine[i] = #9)
      or (secondLine[i] = ';') or (secondLine[i] = '|') then
    begin
      // also test the case  that there are spaces around the separators (e.g. ' , ')
      if (secondLine[i] = ' ') and (i < Length(secondLine))
        and ((secondLine[i+1] = ',') or (secondLine[i+1] = ';') or (secondLine[i+1] = '|')) then
        DataColumnSeparator:= secondLine[i+1]
      else
        DataColumnSeparator:= secondLine[i];
      break;
    end
  end;
  // if there are the same number of DataColumnSeparator in first and second line
  // we found the separator, otherwise we have to parse the second line
  if firstLine.CountChar(DataColumnSeparator) <> secondLine.CountChar(DataColumnSeparator) then
  begin
    for i:= 1 to Length(firstLine) do
    begin
      if (firstLine[i] = ',') or (firstLine[i] = ' ') or (firstLine[i] = #9)
        or (secondLine[i] = ';') or (secondLine[i] = '|') then
      begin
        DataColumnSeparator:= firstLine[i];
        break;
      end
    end;
  end;
  // if still not found, we issue an  error and stop
  if firstLine.CountChar(DataColumnSeparator) <> secondLine.CountChar(DataColumnSeparator) then
  begin
    MessageDlg('The column separator of the data file could not be determined', mtError, [mbOK], 0);
    result:= 2; // means error, but known error
    exit;
  end;

  // the file opening succeeded and we can delete the existing plot and results
  MainForm.ClusterResultSG.RowCount:= 1; // deletes the results table but not the header
  for i:= MainForm.DataC.SeriesCount - 1 downto 0 do
      MainForm.DataC.Series[i].Free;
  // set the chart axis titles according to the header
  StringArray:= firstLine.Split(DataColumnSeparator);
  columns:= length(StringArray);
  MainForm.DataC.AxisList[1].Title.Caption:= StringArray[0]; // AxisList[1] is x-axis
  MainForm.DataC.AxisList[0].Title.Caption:= StringArray[1];
  // fill the CheckComboBoxes
  for i:= 0 to High(StringArray) do
  begin
    // in case there is a leading space, remove it
    if (High(StringArray[i]) > 0) and (StringArray[i][1] = ' ') then
      StringArray[i]:= Trim(StringArray[i]);
    MainForm.DataSelectionCCB.AddItem(StringArray[i], cbChecked);
    MainForm.PlotSelectionCCB.AddItem(StringArray[i], cbChecked);
  end;
  // set the first CheckComboBoxes item to be displayed
  MainForm.DataSelectionCCB.ItemIndex:= 0;
  MainForm.PlotSelectionCCB.ItemIndex:= 0;

  // read file, thereby secondLine is the first row of the StringArray
  StringArray:= secondLine.Split(DataColumnSeparator);
  try
    List:= specialize TList<TStringArray>.Create;
    List.Add(StringArray);
    // read file until end
    while not LineReader.Eof do
    begin
      LineReader.ReadLine(textLine);
      StringArray:= textLine.Split(DataColumnSeparator);
      // often the last lines of files are empty, contain whitespace etc. therefore we only
      // read if a line contains the same number of column separators as the header line
      if length(StringArray) = columns then
        List.Add(StringArray);
    end;

    // setup DataTextColumnsIndices
    Setlength(DataTextColumnsIndices, columns);
    // reset all indices
    FillChar(DataTextColumnsIndices[0],
             Length(DataTextColumnsIndices) * SizeOf(DataTextColumnsIndices[0]), 0);

    // convert the string list to an array of double
    columnCounter:= 0;
    SetLength(DataArray, List.Count, columns);
    for i:= 0 to List.Count-1 do
    begin
      for k:= 0 to columns-1 do
        begin
          if not TryStrToFloat(List[i][k], DataArray[i][k]) then
          begin
            if DataTextColumnsIndices[k] = 0 then
            begin
              result:= 2; // means error, but known error
              // uncheck this column for the clustering
              MainForm.PlotSelectionCCB.ItemEnabled[k]:= false;
              // we will later uncheck since unchecking will trigger PlotSelectionCCB()
              // and we first need the reading to be complete before we can plot
              MainForm.DataSelectionCCB.ItemEnabled[k]:= false;
              MainForm.DataSelectionCCB.Checked[k]:= false;
              // store current column to be able to later save it untouched
              DataTextColumnsIndices[k]:= 1;
            end;
            // set value to zero just to have a value
            DataArray[i][k]:= 0.0;
            inc(columnCounter);
           end;
        end;
    end;
    if result = 2 then
    begin
      // setup DataTextColumns
      SetLength(DataTextColumns, List.Count, columnCounter);
      columnCounter:= 0;
      for k:= 0 to columns-1 do
      begin
        if DataTextColumnsIndices[k] = 1 then
        begin
          for i:= 0 to List.Count-1 do
            DataTextColumns[i][columnCounter]:= List[i][k];
          inc(columnCounter);
        end;
      end;
    end;

  finally
    List.Free;
  end;

  result:= 1; // no errors

 finally
   LineReader.Free;
   OpenFileStream.Free;
 end;

end;


function TChartData.OpenHandling(InName: string; FileExt: string): string;
// handles the open dialog
var
 OutNameTemp : string;
begin
  result:= '';
  if FileExt = '.csv' then
  begin
    MainForm.OpenDialog.Filter:= 'Data file (*.csv)|*.csv';
    MainForm.OpenDialog.Title:= 'Open data file';
  end;
  // propose a file name
  if (InName <> '') and (MainForm.OpenDialog.FileName = '') then
    MainForm.OpenDialog.FileName:= ExtractFileName(InName);
  // empty existing dialog file name if the extension does not match
  if ExtractFileExt(MainForm.OpenDialog.FileName) <> FileExt then
    MainForm.OpenDialog.FileName:= '';
  if MainForm.OpenDialog.FileName <> '' then
    MainForm.OpenDialog.FileName:= ExtractFileName(MainForm.OpenDialog.FileName);
  if MainForm.OpenDialog.Execute then
  begin
    OutNameTemp:= MainForm.OpenDialog.FileName;
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
    MainForm.OpenDialog.FileName:= ExtractFileName(OutNameTemp);
  end
  else // was not executed for some reason
    result:= '';
end;


function TChartData.SaveHandling(InName: string; FileExt: string): string;
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
    MainForm.SaveDialog.Filter:= 'Table (*.csv)|*.csv';
    MainForm.SaveDialog.Title:= 'Save clustered data as';
  end
  else if FileExt = '.svg' then
  begin
    MainForm.SaveDialog.Filter:= 'Vector graphics (*.svg)|*.svg';
    MainForm.SaveDialog.Title:= 'Save plot as';
  end;
  // clear filename if the extension does not fit
  if (MainForm.SaveDialog.FileName <> '')
   and (ExtractFileExt(MainForm.SaveDialog.FileName) <> FileExt) then
    MainForm.SaveDialog.FileName:= '';
  // propose a file name
  if ((InName <> '') and (MainForm.SaveDialog.FileName = ''))
   or ((InName <> '') and (MainForm.SaveDialog.FileName <> '')
     and (ExtractFileExt(InName) <> ExtractFileExt(MainForm.SaveDialog.FileName))) then
    MainForm.SaveDialog.FileName:= ExtractFileName(InName);
  if MainForm.SaveDialog.Execute then
  begin
    OutNameTemp:= MainForm.SaveDialog.FileName;
    // add file extension if it is missing
    if (ExtractFileExt(OutNameTemp) <> FileExt) then
     Insert(FileExt, OutNameTemp, Length(OutNameTemp) + 1);

    if FileExists(OutNameTemp) then
    begin
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
        MainForm.SaveDialog.FileName:= ExtractFileName(OutNameTemp);
        exit;
      end;
    end; // end if FileExists

    result:= OutNameTemp;
    // store last used name
    MainForm.SaveDialog.FileName:= ExtractFileName(OutNameTemp);
  end
  else // the user canceled the dialog
  begin
    result:= 'canceled';
  end;

end;


function TChartData.CDSavePlotBBClick(Sender: TObject; ChartName: string): Boolean;
var
 OutNameHelp, ScreenOutName, tempString : string;
begin
  result:= false;
  // propose a file name
  OutNameHelp:= MainForm.LoadedDataFileM.Text;
  if UsedClusteringMethod <> none then
  begin
    Str(UsedClusteringMethod, tempString);
    OutNameHelp:= OutNameHelp + '-clustered-' + tempString;
  end;
  ScreenOutName:= SaveHandling(OutNameHelp, '.svg'); // opens file dialog

  if ScreenOutName <> '' then
  begin
    (MainForm.FindComponent(ChartName) as TChart).SaveToSVGFile(ScreenOutName);
    result:= true;
  end
  else
    result:= false;

end;


procedure TChartData.CDSaveCsvMIClick(Sender: TObject);
var
 textLine, CSVOutName, OutNameHelp, tempString, columnSeparator : string;
 saveFileStream : TFileStream;
 i, k, count : Int64;
 MousePointer : TPoint;
 lineBytes: TBytes;
 stringArray : TStringArray;
begin
  MousePointer:= Mouse.CursorPos; // store mouse position

  // propose a file name
  OutNameHelp:= MainForm.LoadedDataFileM.Text;
  if UsedClusteringMethod <> none then
  begin
    Str(UsedClusteringMethod, tempString);
    OutNameHelp:= OutNameHelp + '-clustered-' + tempString;
  end;
  CSVOutName:= ChartData.SaveHandling(OutNameHelp, '.csv'); // opens file dialog
  if CSVOutName <> '' then
  begin
    if FileExists(CSVOutName) then
    begin
      try
        saveFileStream:= TFileStream.Create(CSVOutName, fmOpenReadWrite);
      except
        on EFOpenError do
        begin
          MessageDlgPos('CSV file is used by another program and cannot be opened.',
                   mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
          exit;
        end;
      end;
      // delete its content
      saveFileStream.Size:= 0;
    end
    else
    begin
      try
        SaveFileStream:= TFileStream.Create(CSVOutName, fmCreate);
      except
        on EFOpenError do
        begin
          MessageDlgPos('CSV file could not be created.' + LineEnding +
                   'Probably you don''t have write access to the specified folder.',
                   mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
          exit;
        end;
      end;
    end;
  end; // end if CSVOutName <> ''

  // Some data files use a space around the DataColumnSeparator and we have
  // to respect this. Therefore check this from the DataHeader.
  columnSeparator:= DataColumnSeparator;
  stringArray:= DataHeader.Split(DataColumnSeparator);
  // test for space after separator
  if stringArray[1][1] = ' ' then
    columnSeparator:= columnSeparator + ' ';
  // test for space before separator
  if stringArray[0][High(stringArray[0])] = ' ' then
    columnSeparator:= ' ' + columnSeparator;

  try
    // write the header line and add there the column 'cluster'
    // as it might have Unicode characters, output as Unicode
    textLine:= DataHeader + columnSeparator + 'cluster' + LineEnding;
    lineBytes:= BytesOf(UTF8Encode(textLine));
    saveFileStream.WriteBuffer(lineBytes[0], Length(lineBytes));

    // write the data
    for i:= 0 to High(DataArray) do
    begin
      textLine:= '';
      count:= 0;
      for k:= 0 to High(DataArray[i]) do
      begin
        // we take care of the text columns
        if DataTextColumnsIndices[k] = 0 then
          textLine:= textLine + FloatToStr(DataArray[i][k])
        else
        begin
          textLine:= textLine + DataTextColumns[i][count];
          inc(count);
        end;
        if k < High(DataArray[i]) then
          textLine:= textLine + columnSeparator;
      end;
      textLine:= textLine + LineEnding;
      lineBytes:= BytesOf(UTF8Encode(textLine));
      saveFileStream.WriteBuffer(lineBytes[0], Length(lineBytes));
    end;

  finally
    saveFileStream.Free;
  end;

end;


procedure TChartData.FlipBClick(Sender: TObject);
var
  series, i : Int64;
  lineSeries : TLineSeries;
  tempValue : Double;
  tempString : String;
begin
  // change for all series x and y
  for series:= 0 to MainForm.DataC.Series.Count-1 do
  begin
    lineSeries:= MainForm.DataC.Series[series] as TLineSeries;
    for i:= 0 to lineSeries.Count-1 do
    begin
      tempValue:= lineSeries.XValue[i];
      lineSeries.XValue[i]:= lineSeries.YValue[i];
      lineSeries.YValue[i]:= tempValue;
    end;
  end;
  // flip axis decription
  tempString:= MainForm.DataC.AxisList[0].Title.Caption;
  MainForm.DataC.AxisList[0].Title.Caption:= MainForm.DataC.AxisList[1].Title.Caption;
  MainForm.DataC.AxisList[1].Title.Caption:= tempString;
end;


// event functions -------------------------------------------------------

// CDPlotSelectionCCBItemChange is the central procedure that plots the data
procedure TChartData.CDPlotSelectionCCBItemChange(Sender: TObject);
var
  i, count, dim1, dim2, numOfClusters, assignmentColumn, clusterNumber : Int64;
  Series : array of TLineSeries;
  StringArray : TStringArray;
begin
  // replot using the first 2 selected dimensions
  count:= 0;
  dim1:= -1; dim2:= -1;
  for i:= 0 to MainForm.PlotSelectionCCB.Items.Count-1 do
  begin
    if not MainForm.PlotSelectionCCB.ItemEnabled[i] then
      continue;
    if (MainForm.PlotSelectionCCB.Checked[i]) and (count = 0) then
    begin
      dim1:= i;
      inc(count);
    end
    else if (MainForm.PlotSelectionCCB.Checked[i]) and (count = 1) then
    begin
      dim2:= i;
      break;
    end;
  end;
  // add data to plot when there is at least one usable dimensions
  if dim1 > -1 then
  begin
    // first delete
    for i:= MainForm.DataC.SeriesCount - 1 downto 0 do
      MainForm.DataC.Series[i].Free;
    // set the chart axis titles according to the header
    StringArray:= DataHeader.Split(DataColumnSeparator);
    MainForm.DataC.AxisList[1].Title.Caption:= StringArray[dim1];

    // create a plot series
    numOfClusters:= MainForm.ClusterResultSG.RowCount-1;
    SetLength(Series, numOfClusters+1);
    for i:= 0 to numOfClusters do // we need one extra for the unclusteed points
    begin
      Series[i]:= TLineSeries.Create(MainForm.DataC);
      Series[i].ShowLines:= False; // points only
      Series[i].Pointer.Visible:= true;
      // assign unique color for each cluster
      Series[i].Pointer.Brush.Color:= colorPalette[i mod Length(colorPalette)];
      Series[i].Pointer.Style:= psCircle; // circles for the points
      Series[i].Title:= IntToStr(i);
      MainForm.DataC.AddSeries(Series[i]);
    end;

    // add data points to the according cluster number series
    assignmentColumn:= Length(DataArray[0]) - 1;
    for i:= 0 to high(DataArray) do
    begin
      clusterNumber:= round(DataArray[i, assignmentColumn]);
      if dim2 > -1 then
        Series[clusterNumber].AddXY(DataArray[i, dim1], DataArray[i, dim2])
      else
        Series[clusterNumber].AddXY(DataArray[i, dim1], 0.0);
    end;
    // set axis title and enable Flip button
    if dim2 > -1 then
    begin
      MainForm.DataC.AxisList[0].Title.Caption:= StringArray[dim2];
      MainForm.FlipB.Enabled:= true;
    end
    else
    begin
      MainForm.DataC.AxisList[0].Title.Caption:= '';
      MainForm.FlipB.Enabled:= false;
    end;

    // If there are center points available in ClusterResultSG (not only if
    // in KMeansClusterCenters) we have k-Means results.
    // k-Means does not have a '0' cluster as all points are always in a cluster,
    // thus we can draw into that series.
    if (MainForm.ClusterResultSG.Columns.Items[2].Title.Caption = 'Center at')
     and (MainForm.ClusterResultSG.RowCount > 1) then // more than the header line
    begin
      Series[0].Pointer.Brush.Color:= clGray;
      Series[0].Pointer.Style:= psFullStar; // stars for the points
      Series[0].Pointer.HorizSize:= 8;
      Series[0].Pointer.VertSize:= 8;
      Series[0].Title:= 'centers';
      for i:= 0 to numOfClusters-1 do
      begin
        if Length(KMeansClusterCenters[0]) > 0 then
          Series[0].AddXY(KMeansClusterCenters[i][0], KMeansClusterCenters[i][1])
        else
          Series[0].AddXY(KMeansClusterCenters[i][0], 0.0);
      end;
    end;
  end
  else
  begin
    MainForm.DataC.AxisList[1].Title.Caption:= 'Dimension 1';
    MainForm.DataC.AxisList[0].Title.Caption:= 'Dimension 2';
  end;

  // display the first enabled and checked item in PlotSelectionCCB
  for i:= 0 to MainForm.PlotSelectionCCB.Items.Count-1 do
  begin
    if (MainForm.PlotSelectionCCB.ItemEnabled[i])
     and (MainForm.PlotSelectionCCB.Checked[i]) then
    begin
      MainForm.PlotSelectionCCB.ItemIndex:= i;
      break;
    end;
  end;

end;


procedure TChartData.CDDataPointHintToolHint(
  ATool: TDataPointHintTool; const APoint: TPoint; var AHint: String);
var
  SeriesName : string;
  dim1, dim2 : double;
  currentSeries, otherSeries : TBasicChartSeries;
  i : integer;
begin
  otherSeries:= nil;
  currentSeries:= ATool.Series;

  SeriesName:= ATool.Series.Name;
  dim1:= MainForm.DataC.AxisList[1].GetTransform.GraphToAxis(
          ATool.NearestGraphPoint.X);
  dim2:= MainForm.DataC.AxisList[0].GetTransform.GraphToAxis(
         ATool.NearestGraphPoint.Y);

  // set the hint text to the cluster number and the coordinates
  // In ClusterResultSG we stored the coordinated a %.2f since the space there is
  // limited. Therefore we must read out that values also as %.2f to avoid rounding errors.
  if (currentSeries as TLineSeries).Title = 'centers' then
    AHint:= (currentSeries as TLineSeries).Title + '; '
            + Format('%.2f', [dim1]) + ', ' + Format('%.2f', [dim2])
  else
    AHint:= (currentSeries as TLineSeries).Title + '; '
            + Format('%.3g', [dim1]) + ', ' + Format('%.3g', [dim2]);
end;


procedure TChartData.CDDataPointHintToolHintPosition(
 ATool: TDataPointHintTool; var APoint: TPoint);
var
  series : TLineSeries;
  x, y, HintWidth, HintHeight : Integer;
  rect : TRect;
  HintWindow : THintWindow;
  HintText : string = '';
  SeriesName : string;
// moves the hint text above the cursor and center it horizontally to cursor
begin
  series:= ATool.Series as TLineSeries;
  SeriesName:= ATool.Series.Name;
  // get image coordinates of the point
  x:= MainForm{%H-}.DataC.XGraphToImage(series.ListSource[ATool.PointIndex]^.X);
  // all series are connected to the left axis
  y:= MainForm{%H-}.DataC.YGraphToImage(
       MainForm.DataC.AxisList[0].GetTransform.AxisToGraph(
        series.ListSource[ATool.PointIndex]^.Y));

  // get hint text - just call the event handler of OnHint
  MainForm.DataPointHintToolHint(ATool, APoint, HintText);

  HintWindow:= THintWindow.Create(nil);
  try
    rect:= HintWindow.CalcHintRect(MainForm.DataC.Width, HintText, nil);
    HintWidth:= rect.Right - rect.Left;  // hint window width
    HintHeight:= rect.Bottom - rect.Top; // hint window height
  finally
    HintWindow.Free;
  end;

  // center hint horizontally relative to data point
  APoint.x:= x - HintWidth div 2;
  // move hint 10 pixels above the "High" data point
  APoint.y:= y - HintHeight - 10;
  // hint coordinates are relative to screen
  APoint:= MainForm.DataC.ClientToScreen(APoint);
end;


procedure TChartData.CDAutoscaleMIClick(Sender: TObject);
begin
 MainForm.DataC.AxisList[0].Range.UseMax:= false;
 MainForm.DataC.AxisList[0].Range.UseMin:= false;
 MainForm.DataC.AxisList[1].Range.UseMax:= false;
 MainForm.DataC.AxisList[1].Range.UseMin:= false;
 // for the x-axis also te extent must be set
 MainForm.DataC.Extent.UseXMax:= false;
 MainForm.DataC.Extent.UseXMin:= false;
end;


procedure TChartData.CDChangeBackColorMIClick(Sender: TObject);
begin
  // start with current color
  MainForm.ColorDialog.Color:= MainForm.DataC.BackColor;
  if MainForm.ColorDialog.Execute then
   MainForm.DataC.BackColor:= MainForm.ColorDialog.Color;
end;


procedure TChartData.CDLegendClickToolClick(Sender: TChartTool;
  Legend: TChartLegend);
var
 editor : TChartLegendEditor;
begin
 editor:= TChartLegendEditor.Create(nil);
 try
  editor.Prepare(Legend, 'Edit chart legend');
  editor.ShowModal;
 finally
  editor.Free;
 end;
end;


procedure TChartData.CDAxisClickToolClick(Sender: TChartTool;
  AnAxis: TChartAxis; HitInfo: TChartAxisHitTests);
var
  page : TChartAxisEditorPage;
  editor : TChartAxisEditor;
begin
 Unused(Sender);
 if (ahtTitle in HitInfo) then
  page:= aepTitle
 else if (ahtLabels in HitInfo) then
  page:= aepLabels
 else if (ahtLine in HitInfo) then
  page:= aepLine
 else if (ahtGrid in HitInfo) then
  page:= aepGrid
 else
  exit;
 editor:= TChartAxisEditor.Create(nil);
 try
  editor.Prepare(AnAxis, 'Edit chart axis "%s"');
  editor.Page:= page;
  editor.ShowModal; // shows the dialog
  // Info: calling the dialog triggers ZoomDragToolAfterMouseUp
  // keep that in mind
 finally
  editor.Free;
 end;
end;


procedure TChartData.CDTitleFootClickToolClick(Sender: TChartTool;
  Title: TChartTitle);
var
 editor : TChartTitleFootEditor;
begin
 editor:= TChartTitleFootEditor.Create(nil);
 try
  editor.Prepare(Title, 'Edit chart title');
  editor.ShowModal;
 finally
  editor.Free;
 end;
end;


// chart appearance functions --------------------------------------------
function TChartData.FontStylesToString(FontStyles: TFontStyles): string;
begin
  result:= '';
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


function TChartData.StringToFontStyles(s: string): TFontStyles;
var
 i : integer;
begin
 result:= [];
 for i:= 1 to WordCount(s, [',']) do
  result:= result + [TFontStyle(StrToInt(ExtractWord(i, s, [','])))];
end;


procedure TChartData.SaveAppearance(iniFile: string);
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

 Chart:= MainForm.DataC;
 List.Add('Chart ' + Chart.Name);

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


procedure TChartData.LoadAppearance(iniFile: string);
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
  Chart:= (MainForm.FindComponent(
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
   Series:= (MainForm.FindComponent(
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

