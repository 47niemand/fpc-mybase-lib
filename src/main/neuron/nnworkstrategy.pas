unit NNWorkStrategy;

{$mode delphi}

interface

uses
  Classes, SysUtils, NNWork, NNWorkTypes;

type

  { TNNWorkTrainStrategyDefault }

  TNNWorkTrainStrategyDefault = class(TNNWorkTrainStrategy)
  public
    function DoTrain(const NN: TNNWork; const ATrainData: TNNTrainData): DType;
      override;
    class function GetInstance: TNNWorkTrainStrategy; override;
    destructor Destroy; override;
  end;


implementation

{ TNNWorkTrainStrategyDefault }

// Training routine for the network. Uses data as input, compares output with
// desired output, computes errors, adjusts weights attached to each node,
// then repeats until the mean squared error at the output is less than
// max_MSE. The learning rate is eta.

function TNNWorkTrainStrategyDefault.DoTrain(const NN: TNNWork;
  const ATrainData: TNNTrainData): DType;
var
  //  output, output_weight_delta, hidden_weight_delta: array of DType;
  //  MSE, MSE_max: DType; // slight speed enhancement
  J, K: integer;
  N: integer;
  C: cardinal;
  _MSE_max_: DType; // slight speed enhancement
  MSE: DType;
begin
  N := 0;
  C := 0;
  _MSE_max_ := ATrainData.MSE_max;
  with NN do
    while True do
    begin
      // run & compute MSE
      for J := 0 to HiddenSize - 1 do
        RunHidden(J, @ATrainData);
      MSE := 0;
      for K := 0 to OutputSize - 1 do
        MSE := MSE + RunTrainOutput(K, @ATrainData);

      N := N + 1;
      if (GetTickCount - C) > 1000 then
      begin
        writeln(format('MSE = %g, N = %d', [MSE, N]));
        C := GetTickCount;
      end;

      if (MSE < _MSE_max_) then
        break;
      // And the hidden layer error terms
      for J := 0 to HiddenSize - 1 do
        TrainHiddenErrorTerms(J, @ATrainData);
      //adjusts weights
      for K := 0 to OutputSize - 1 do
        TrainAdjustOutput(K, @ATrainData);
      for J := 0 to HiddenSize - 1 do
        TrainAdjustHidden(J, @ATrainData);
      if Abort or (_MSE_max_ = 0) then
        break;
    end;
  PNNTrainData(@ATrainData)^.Iterations := N;
  writeln(format('DONE - MSE = %g, N = %d', [MSE, N]));
  Result := MSE;
end;

var
  StrategySinglentonDefault: TNNWorkTrainStrategy = nil;

class function TNNWorkTrainStrategyDefault.GetInstance: TNNWorkTrainStrategy;
begin
  if not Assigned(StrategySinglentonDefault) then
  begin
    Result := TNNWorkTrainStrategyDefault.Create;
    if InterlockedCompareExchange(pointer(StrategySinglentonDefault),
      pointer(Result), nil) <> nil then
      FreeAndNil(Result);
  end;
  Result := StrategySinglentonDefault;
end;

destructor TNNWorkTrainStrategyDefault.Destroy;
begin
  InterlockedCompareExchange(pointer(StrategySinglentonDefault),
    nil, pointer(self));
  inherited Destroy;
end;

initialization

finalization
  FreeAndNil(StrategySinglentonDefault);
end.









