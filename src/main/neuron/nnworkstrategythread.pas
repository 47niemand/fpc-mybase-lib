unit NNWorkStrategyThread;

{$mode delphi}

interface

uses
  Classes, SysUtils, NNWorkTypes, NNWork;

type
  { TNNWorkTrainSingleThraed }

  TNNWorkTrainMultiThraed = class(TNNWorkTrainStrategy)
  public
    function DoTrain(const NN: TNNWork; const ATrainData: TNNTrainData): DType;
      override;
    class function GetInstance: TNNWorkTrainStrategy; override;
  end;



implementation

uses NNWorkUtils, syncobjs;

const
  MAX_USE_CPU_COUNT = 4;

var
  StrategySiglentonMultiThread: TNNWorkTrainStrategy = nil;

{ TNNWorkTrainMultiThraed }

// Training routine for the network. Uses data as input, compares output with
// desired output, computes errors, adjusts weights attached to each node,
// then repeats until the mean squared error at the output is less than
// max_MSE. The learning rate is eta.

function TNNWorkTrainMultiThraed.DoTrain(const NN: TNNWork;
  const ATrainData: TNNTrainData): DType;
var
  MSE: DType;
  _MSE_max_: DType; // slight speed enhancement
  I, J, K: integer;
  N: integer;
  EA: array[0..4] of TEvent;
  E: TEvent;
  P: TVarPointerArray;
  C: cardinal;
begin
  for I := 0 to High(EA) do
    EA[I] := TEvent.Create(nil, False, False, '');

  SetLength(P, MAX_USE_CPU_COUNT);
  N := 0;
  C := 0;
  _MSE_max_ := ATrainData.MSE_max;

  with NN, ATrainData do
    while True do
    begin
      // run & compute MSE
      J := 0;
      E := EA[0];
      E.ResetEvent;
      NNThreadTrain(TrainHiddenRun, @ATrainData, J, HiddenSize - 1, E,
        MAX_USE_CPU_COUNT, nil);
      repeat
        if E.WaitFor(INFINITE) <> wrSignaled then
          raise Exception.Create('process error');
      until ATrainData.Refs = 0;

      MSE := 0.0;
      K := 0;
      E := EA[1];
      E.ResetEvent;
      NNThreadTrain(TrainOutPutRun, @ATrainData, K, OutputSize - 1, E,
        MAX_USE_CPU_COUNT, @p);
      repeat
        if E.WaitFor(INFINITE) <> wrSignaled then
          raise Exception.Create('process error');
      until ATrainData.Refs = 0;

      for I := 0 to MAX_USE_CPU_COUNT - 1 do
        MSE := MSE + NNThreadResult(P[I]);

      N := N + 1;
      if (GetTickCount - C) > 1000 then
      begin
        writeln(format('MSE = %g, N = %d', [MSE, N]));
        C := GetTickCount;
      end;

      if (MSE < _MSE_max_{^}) then
        break;
      // And the hidden layer error terms
      J := 0;
      E := EA[2];
      E.ResetEvent;
      NNThreadTrain(TrainHiddenErrorTerms, @ATrainData, J, HiddenSize - 1, E,
        MAX_USE_CPU_COUNT, nil);
      repeat
        if E.WaitFor(INFINITE) <> wrSignaled then
          raise Exception.Create('process error');
      until ATrainData.Refs = 0;

      //adjusts weights
      K := 0;
      E := EA[3];
      E.ResetEvent;
      NNThreadTrain(TrainAdjustOutput, @ATrainData, K, OutputSize - 1,
        E, MAX_USE_CPU_COUNT, nil);
      repeat
        if E.WaitFor(INFINITE) <> wrSignaled then
          raise Exception.Create('process error');
      until ATrainData.Refs = 0;

      J := 0;
      E := EA[4];
      E.ResetEvent;
      NNThreadTrain(TrainAdjustHidden, @ATrainData, J, HiddenSize - 1,
        E, MAX_USE_CPU_COUNT, nil);
      repeat
        if E.WaitFor(INFINITE) <> wrSignaled then
          raise Exception.Create('process error');
      until ATrainData.Refs = 0;

      if (_MSE_max_ = 0) or (Abort) then
        break;
    end;

  for I := 0 to High(EA) do
    FreeAndNil(EA[I]);

  writeln(format('DONE - MSE = %g, N = %d', [MSE, N]));
  Result := MSE;
end;


class function TNNWorkTrainMultiThraed.GetInstance: TNNWorkTrainStrategy;
begin
  if not Assigned(StrategySiglentonMultiThread) then
  begin
    Result := TNNWorkTrainMultiThraed.Create;
    if InterlockedCompareExchange(pointer(StrategySiglentonMultiThread),
      pointer(Result), nil) <> nil then
      FreeAndNil(Result);
  end;
  Result := StrategySiglentonMultiThread;
end;


initialization

finalization
  FreeAndNil(StrategySiglentonMultiThread);
end.

