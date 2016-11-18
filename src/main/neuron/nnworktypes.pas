unit NNWorkTypes;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TLayerType = (NNW_ALL, NNW_INPUT, NNW_HIDDEN, NNW_OUTPUT);

  DType = double;
  PDType = ^DType;
  TVarArrayOfDType = array of DType;
  PVarArrayOfDType = ^TVarArrayOfDType;

  TVarPointerArray = array of pointer;
  PVarPointerArray = ^TVarPointerArray;

  { TNNTrainData }

  TNNTrainData = record
    // some record variables
    Refs: integer;
    Iterations: integer;
    // tarin params
    MSE_max: DType;
    eta: DType;
    // pointers to datastructures
    Data: PVarArrayOfDType;
    desired: PVarArrayOfDType;
    output: PVarArrayOfDType;
    output_weight_delta: PVarArrayOfDType;
    hidden_weight_delta: PVarArrayOfDType;
  end;

  PNNTrainData = ^TNNTrainData;
  TNNTrainProc = function(const J: integer; const TrainData: PNNTrainData): DType of
    object;

implementation

end.

