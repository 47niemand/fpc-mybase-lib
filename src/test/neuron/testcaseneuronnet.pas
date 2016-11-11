unit TestCaseNeuronNet;

{$mode delphi}

interface

uses
  Classes, Forms, SysUtils, fpcunit, testutils, testregistry,
  NNWork, NNWorkUtils, utimers, NNWorkTypes, FuzzyUtils;

type

  { TTestCaseNeuronBasics }

  TTestCaseNeuronBasics = class(TTestCase)
  published
    procedure TestNNWork_BASIC;
    procedure TestNNWork_BASIC_MultiCPU;
    procedure TestNNWork_LARGE;
    procedure TestNNWork_LARGE_MultiCPU;
    procedure TestLoadSave;
  end;

implementation



const
  www1: array[0..2, 0..1] of double =
    ((0.499999999767169, 0.468620060477406), (-0.361048467224464,
    0.297419034875929), (
    0.227078732801601, -0.171654418576509));

  www2: array[0..1, 0..2] of double =
    ((0.181308728875592, 0.338204534724355, 0.127761641284451),
    (0.0743262325413525, 0.417987840482965, 0.0252059353515506));




procedure TTestCaseNeuronBasics.TestNNWork_BASIC;
var
  w: Tnnwork;
  C: int64;
  I, o: TVarArrayOfDType;
  K: integer;
begin
  RandSeed := 0;
  w := Tnnwork.Create(2, 3, 2);
  try
    //  w.SaveAsCode('d:\net.txt');

    SetLength(I, w.InputSize);
    SetLength(o, w.OutputSize);

    I[0] := 0.1;
    I[1] := 0.9;
    o[0] := 0.9;
    o[1] := 0.1;


    with w do
    begin
      hiddennodes.nodes[0].output := 0;
      hiddennodes.nodes[0].weights[0] := 0.499999999767169;
      hiddennodes.nodes[0].weights[1] := 0.468620060477406;
      hiddennodes.nodes[1].output := 0;
      hiddennodes.nodes[1].weights[0] := -0.361048467224464;
      hiddennodes.nodes[1].weights[1] := 0.297419034875929;
      hiddennodes.nodes[2].output := 0;
      hiddennodes.nodes[2].weights[0] := 0.227078732801601;
      hiddennodes.nodes[2].weights[1] := -0.171654418576509;
      outputnodes.nodes[0].output := 0;
      outputnodes.nodes[0].weights[0] := 0.181308728875592;
      outputnodes.nodes[0].weights[1] := 0.338204534724355;
      outputnodes.nodes[0].weights[2] := 0.127761641284451;
      outputnodes.nodes[1].output := 0;
      outputnodes.nodes[1].weights[0] := 0.0743262325413525;
      outputnodes.nodes[1].weights[1] := 0.417987840482965;
      outputnodes.nodes[1].weights[2] := 0.0252059353515506;

    end;


    C := timer_GetTick;
    W.Train(I, o, 0.001, 0.01);
    Writeln('TIME: ', tickToTimeStr(timer_GetTick - C));

  finally
    w.Free;
  end;

end;

procedure TTestCaseNeuronBasics.TestNNWork_BASIC_MultiCPU;
var
  w: Tnnwork;
  C: int64;
  I, o: TVarArrayOfDType;
  K: integer;
begin
  RandSeed := 0;
  w := Tnnwork.Create(2, 3, 2);
  try
    //  w.SaveAsCode('d:\net.txt');

    SetLength(I, w.InputSize);
    SetLength(o, w.OutputSize);

    I[0] := 0.1;
    I[1] := 0.9;
    o[0] := 0.9;
    o[1] := 0.1;


    with w do
    begin
      hiddennodes.nodes[0].output := 0;
      hiddennodes.nodes[0].weights[0] := 0.499999999767169;
      hiddennodes.nodes[0].weights[1] := 0.468620060477406;
      hiddennodes.nodes[1].output := 0;
      hiddennodes.nodes[1].weights[0] := -0.361048467224464;
      hiddennodes.nodes[1].weights[1] := 0.297419034875929;
      hiddennodes.nodes[2].output := 0;
      hiddennodes.nodes[2].weights[0] := 0.227078732801601;
      hiddennodes.nodes[2].weights[1] := -0.171654418576509;
      outputnodes.nodes[0].output := 0;
      outputnodes.nodes[0].weights[0] := 0.181308728875592;
      outputnodes.nodes[0].weights[1] := 0.338204534724355;
      outputnodes.nodes[0].weights[2] := 0.127761641284451;
      outputnodes.nodes[1].output := 0;
      outputnodes.nodes[1].weights[0] := 0.0743262325413525;
      outputnodes.nodes[1].weights[1] := 0.417987840482965;
      outputnodes.nodes[1].weights[2] := 0.0252059353515506;

    end;


    C := timer_GetTick;
    w.TrainT(I, o, 0.001, 0.01);
    Writeln('TIME: ', tickToTimeStr(timer_GetTick - C));

  finally
    w.Free;
  end;

end;


procedure TTestCaseNeuronBasics.TestNNWork_LARGE;
var
  w: Tnnwork;
  C: int64;
  I, o: TVarArrayOfDType;
  K: integer;
begin
  RandSeed := 0;
  w := Tnnwork.Create(16 * 16, 256, 16 * 16);
  try
    SetLength(I, w.InputSize);
    SetLength(o, w.OutputSize);

    for K := 0 to length(I) - 1 do
      I[K] := FuzzyFalse(0.01);
    for K := 0 to length(o) - 1 do
      o[K] := FuzzyTrue(0.01);

    Writeln('ONE THREAD');
    C := timer_GetTick;
    w.Train(I, o, 0.001, 0.01);
    Writeln('TIME: ', tickToTimeStr(timer_GetTick - C));
  finally
    w.Free;
  end;
end;

procedure TTestCaseNeuronBasics.TestNNWork_LARGE_MultiCPU;
var
  w: Tnnwork;
  C: int64;
  I, o: TVarArrayOfDType;
  K: integer;
begin
  RandSeed := 0;
  w := Tnnwork.Create(16 * 16, 256, 16 * 16);
  try
    SetLength(I, w.InputSize);
    SetLength(o, w.OutputSize);

    for K := 0 to length(I) - 1 do
      I[K] := FuzzyFalse(0.01);
    for K := 0 to length(o) - 1 do
      o[K] := FuzzyTrue(0.01);

    Writeln('MULTI THREAD');
    C := timer_GetTick;
    w.TrainT(I, o, 0.001, 0.01);
    Writeln('TIME: ', tickToTimeStr(timer_GetTick - C));
  finally
    w.Free;
  end;
end;

procedure TTestCaseNeuronBasics.TestLoadSave;
var
  w, c: Tnnwork;
  I, J, K: integer;
begin
  w := Tnnwork.Create(16 * 16, 256, 16 * 16);
  try
    W.Save('test.net');
    AssertEquals(True, FileExists('test.net'));
    c := w.Load('test.net');
    try
      AssertEquals(w.HiddenSize, c.HiddenSize);
      AssertEquals(w.OutputSize, c.OutputSize);
      AssertEquals(w.InputSize, c.InputSize);

      with w do
        for I := 0 to HiddenSize - 1 do
          for J := 0 to HiddenNodes.GetWeights - 1 do
            AssertEquals('', HiddenNodes.nodes[I].weights[J],
              c.HiddenNodes.nodes[I].weights[J], 1e-16);
      with w do
        for I := 0 to OutputSize - 1 do
          for J := 0 to OutputNodes.GetWeights - 1 do
            AssertEquals('', OutputNodes.nodes[I].weights[J],
              c.OutputNodes.nodes[I].weights[J], 1e-16);

    finally
      c.Free;
    end;
  finally
    w.Free;
  end;
end;



initialization

  RegisterTest(TTestCaseNeuronBasics);
end.



















