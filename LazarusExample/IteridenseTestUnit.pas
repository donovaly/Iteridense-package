unit IteridenseTestUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  EditBtn, ExtCtrls, Math, ctypes;

const
  MAX_DIMS = 256;

type

  // C-compatible tensor struct
  CTensor = record
    data: Pointer;       // void* data
    ndims: cint;         // int ndims
    dims: array[0..MAX_DIMS-1] of csize_t;  // size_t dims[MAX_DIMS]
  end;

  // C-compatible IteridenseResult struct
  IteridenseResultC = record
    clusterTensor: CTensor;
    countTensor: CTensor;
    numOfClusters: cint;
    finalResolution: cint;
    assignments: CTensor;
    clusterDensities: CTensor;
    clusterSizes: CTensor;
  end;
  PIteridenseResultC = ^IteridenseResultC;

  TInitJulia = procedure(argc: Integer; argv: PPAnsiChar); cdecl;
  TShutdownJulia = procedure(retcode: Integer); cdecl;
  // allocates and computes the IteridenseResult
  // returns a pointer to a heap-allocated IteridenseResultC
  // caller must free with IteridenseFree
  // returns nil on failure
  TIteridenseClustering = function():PIteridenseResultC; cdecl;
  // frees memory allocated by TIteridenseClustering
  // returns 0 on success, -1 if ptr is nil
  TIteridenseFree = function(pointer: PIteridenseResultC): Integer; cdecl;
  { TMainF }

  TMainF = class(TForm)
    GetRandomBB: TBitBtn;
    TextOutputM: TMemo;
    RandomOutLE: TLabeledEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure GetRandomBBClick(Sender: TObject);
  private
  public
  end;

var
  MainF : TMainF;
  LibHandle : THandle;
  IteridenseClustering : TIteridenseClustering;
  IteridenseFree : TIteridenseFree;
  InitJulia : TInitJulia;
  ShutdownJulia : TShutdownJulia;
  DLLPath : String;

implementation

{$R *.lfm}

procedure TMainF.FormCreate(Sender: TObject);
var
  args: array[0..1] of PAnsiChar;
begin
  LibHandle:= LoadLibrary(PChar('IteridenseCLib.dll'));
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
  args[0]:= PAnsiChar(AnsiString('MyProgram'));
  args[1]:= nil;
  InitJulia(1, @args[0]);
end;

procedure TMainF.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // shutdown Julia runtime with exit code 0 (success)
  ShutdownJulia(0);
  // unload the DLL
  FreeLibrary(LibHandle);
end;

procedure TMainF.GetRandomBBClick(Sender: TObject);
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
    TextOutputM.Lines.Add('No clusterTensor data available.');
    Exit;
  end;

  // calculate total number of elements (product of dims)
  count:= 1;
  for i:= 0 to iteridenseResult^.clusterTensor.ndims - 1 do
    count:= count * Integer(iteridenseResult^.clusterTensor.dims[i]);
  if count < 3 then
    TextOutputM.Lines.Add('clusterTensor has less than 3 elements.');
  // cast data pointer to PDouble
  dataPointer := PDouble(iteridenseResult^.clusterTensor.data);

  // output first 3 elements (or fewer if not enough)
  for i:= 0 to min(2, count - 1) do
  begin
    textLine:= Format('clusterTensor[%d] = %.6f', [i+1, dataPointer[i]]);
    TextOutputM.Lines.Add(textLine);
  end;

  // free the allocated struct
  if IteridenseFree(iteridenseResult) <> 0 then
    MessageDlg('Warning: failed to free iteridenseResult', mtError, [mbOK], 0);
end;

end.

