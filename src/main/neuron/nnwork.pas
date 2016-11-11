unit NNWork;

interface

uses NNWorkTypes, NNWorkUtils;

type
  { TNeuron }

  TNeuron = class
  public
    weights: array of DType;
    output: DType;
    constructor Create(ASize: integer);
    destructor Destroy; override;
    function Clone: TNeuron;
  end;

  { TNNLayer }

  TNNLayer = class
  protected
    size: integer;
    weights: integer;
  public
    nodes: array of TNeuron;
    function GetWeights: integer;
    constructor Create(const Dimension, NumWeights: integer);
    // Number of nodes in the layer.
    destructor Destroy; override;
    function Clone: TNNLayer;
  end;

  // This class implements a simple three-layer backpropagation network.

  { TNNWork }

  TNNWork = class
  private
    FAbort: boolean;
    // statistic variables
    FTrainAdjustHidden: integer;
    FTrainAdjustOutput: integer;
    FTrainHiddenErrorTerms: integer;
    FTrainHiddenRun: integer;
    FTrainOutPutRun: integer;
  protected
    FInputSize: integer;
    FOutputSize: integer;
    FHiddenSize: integer;
    FOutputNodes: TNNLayer;
    FHiddenNodes: TNNLayer;
    FLast_MSE: DType;
    function TrainAdjustHidden(const J: integer; const TrainData: PNNTrainData): DType;
    function TrainAdjustOutput(const K: integer; const TrainData: PNNTrainData): DType;
    function TrainHiddenErrorTerms(const J: integer;
      const TrainData: PNNTrainData): DType;
    function TrainHiddenRun(const J: integer; const TrainData: PNNTrainData): DType;
    function TrainOutPutRun(const K: integer; const TrainData: PNNTrainData): DType;
    function DoTrain(const ATrainData: TNNTrainData): DType;
  public
    property Last_MSE: DType read FLast_MSE;
    property OutputNodes: TNNLayer read FOutputNodes;
    property HiddenNodes: TNNLayer read FHiddenNodes;
    property InputSize: integer read FInputSize;
    property OutputSize: integer read FOutputSize;
    property HiddenSize: integer read FHiddenSize;
    // Initialise with the dimensions of the network (input, hidden, output)
    constructor Create(const AInput, AHidden, AOutput: integer);
    destructor Destroy; override;
    // returns dims of network - argument is either ALL, INPUT, HIDDEN or OUTPUT
    // (see above). ALL gives total nodes (useful to see if network is empty).
    function GetLayerSize(const ALayer: TLayerType): integer;
    // Training args are input, desired output, minimum error, learning rate
    procedure Train(const Data: TVarArrayOfDType; const desired: TVarArrayOfDType;
      const max_MSE, eta: DType);
    // Trian with multi-thread optimizations
    procedure TrainT(const Data: TVarArrayOfDType; const desired: TVarArrayOfDType;
      const max_MSE, eta: DType);
    // Run args are input data, output
    procedure Run(const AData: TVarArrayOfDType; var AResult: TVarArrayOfDType);
    // Set Output nerurons
    procedure SetOutput(const desired: TVarArrayOfDType);
    // Run args are input data, output, return MSE between result and output nodes values
    function RunE(const AData: TVarArrayOfDType; var AResult: TVarArrayOfDType): DType;
    //TODO
    class function Load(const AFileName: string): TNNWork;
    procedure Save(const AFileName: string);
    function SaveAsCode(const AFileName: string): integer;
    procedure Abort;
    function Clone: TNNWork;
  end;


function Sigmoid(const AData: DType): DType;

implementation

{ TNNWork }

uses
  Classes, syncobjs, Math, SysUtils, uVariantUtils;

function Sigmoid(const AData: DType): DType;
begin
  Result := 1 / (1 + exp(-AData));
end;

function printafloat(a: array of DType): string;
var
  I: integer;
begin
  Result := '';
  for I := 0 to Length(a) - 1 do
    Result := Result + format('%g; ', [a[I]]);
end;


{ TNNLayer }

function TNNLayer.GetWeights: integer;
begin
  Result := weights;
end;

constructor TNNLayer.Create(const Dimension, NumWeights: integer);
var
  J, I: integer;
begin
  size := Dimension;
  weights := NumWeights;
  setlength(nodes, size);
  for J := 0 to size - 1 do
  begin
    nodes[J] := TNeuron.Create(weights);
    for I := 0 to weights - 1 do
      nodes[J].weights[I] := 0.5 - random / 1;
  end;
end;

destructor TNNLayer.Destroy;
var
  I: integer;
begin
  for I := 0 to size - 1 do
    FreeAndNil(nodes[I]);
  SetLength(nodes, 0);
  inherited Destroy;
end;

function TNNLayer.Clone: TNNLayer;
var
  J, I: integer;
begin
  TObject(Result) := TNNLayer.newinstance;
  Result.weights := weights;
  Result.size := size;
  setlength(Result.nodes, size);
  for J := 0 to size - 1 do
    Result.nodes[J] := nodes[J].Clone;
end;

{ TNeuron }

constructor TNeuron.Create(ASize: integer);
begin
  SetLength(weights, ASize);
end;

destructor TNeuron.Destroy;
begin
  SetLength(weights, 0);
  inherited Destroy;
end;

function TNeuron.Clone: TNeuron;
var
  I: integer;
begin
  TObject(Result) := TNeuron.newinstance;
  SetLEngth(Result.weights, Length(weights));
  Result.output := output;
  for I := 0 to Length(weights) - 1 do
    Result.weights[I] := weights[I];
end;

{ TNNWork }

constructor TNNWork.Create(const AInput, AHidden, AOutput: integer);
begin
  inherited Create;
  FInputSize := AInput;
  FHiddenSize := AHidden;
  FOutputSize := AOutput;
  FHiddenNodes := TNNLayer.Create(HiddenSize, InputSize);
  FOutputNodes := TNNLayer.Create(OutputSize, HiddenSize);
end;

destructor TNNWork.Destroy;
begin
  FreeAndNil(FOutputNodes);
  FreeAndNil(FHiddenNodes);
  inherited Destroy;
end;

function TNNWork.GetLayerSize(const ALayer: TLayerType): integer;
begin
  case ALayer of
    NNW_ALL:
      Result := InputSize + HiddenSize + OutputSize;
    NNW_INPUT:
      Result := InputSize;
    NNW_HIDDEN:
      Result := HiddenSize;
    NNW_OUTPUT:
      Result := OutputSize;
    else
      raise Exception.CreateFmt('Warning: no such layer %d', [Ord(ALayer)]);
  end;
end;

class function TNNWork.Load(const AFileName: string): TNNWork;
var
  T: TextFile;
  S: string;
  J, K, N, I: integer;
  D: DType;
  I_, H_, O_: integer;
  NN: TNNWork;
  L: TLayerType;
begin
  Result := nil;
  AssignFile(T, AFileName);
  Reset(T);
  try
    N := 0;
    NN := nil;
    while not EOF(T) do
      try
        Read(t, D);
        Inc(N);
        if N = 1 then
          I_ := Round(D)
        else
        if N = 2 then
          H_ := Round(D)
        else
        if N = 3 then
          O_ := Round(D);
        if N = 3 then
        begin
          NN := TNNWork.Create(I_, H_, O_);
          J := 0;
          K := 0;
          L := NNW_HIDDEN;
        end;
        if N > 3 then
        begin

          if L = NNW_HIDDEN then
          begin
            if K >= NN.HiddenNodes.weights then
            begin
              K := 0;
              Inc(J);
            end;
            if J >= NN.HiddenSize then
            begin
              L := NNW_OUTPUT;
              J := 0;
              K := 0;
            end;
            if L = NNW_HIDDEN then
            begin
              NN.HiddenNodes.nodes[J].weights[K] := D;
              Inc(K);
            end;
          end;

          if L = NNW_OUTPUT then
          begin
            if K >= NN.OutputNodes.weights then
            begin
              K := 0;
              Inc(J);
            end;
            if J >= NN.OutputSize then
            begin
              Result := NN;
              NN := nil;
              Break;
            end;
            if L = NNW_OUTPUT then
            begin
              NN.OutputNodes.nodes[J].weights[K] := D;
              Inc(K);
            end;
          end;

        end;
      except
        Read(T, s);
      end;
  finally
    if Assigned(NN) then
      FreeAndNil(NN);
    CloseFile(T);
  end;
end;

procedure TNNWork.Save(const AFileName: string);
var
  T: TextFile;
  I, J: integer;
begin
  Assign(T, AFileName);
  Rewrite(T);
  try
    Writeln(T, '# neuron net');
    Writeln(T, InputSize, ' ', HiddenSize, ' ', OutputSize);
    Writeln(T, '# hidden layer');
    for I := 0 to HiddenSize - 1 do
    begin
      for J := 0 to HiddenNodes.weights - 1 do
        Write(T, HiddenNodes.nodes[I].weights[J], ' ');
      writeln(T);
    end;
    Writeln(T, '# output layer');
    for I := 0 to OutputSize - 1 do
    begin
      for J := 0 to OutputNodes.weights - 1 do
        Write(T, OutputNodes.nodes[I].weights[J], ' ');
      writeln(T);
    end;
    Writeln(T, '#end');
  finally
    Close(T);
  end;
end;

function TNNWork.SaveAsCode(const AFileName: string): integer;
var
  O: TStringList;
  I, J: integer;
begin
  O := TStringList.Create;
  O.Add(format('//constructor TYouNNwork.Create;', []));
  O.Add(format('//hidden_nodes.Free;', []));
  O.Add(format('//output_nodes.Free;', []));
  O.Add(format('inherited Create(%d,%d,%d);', [InputSize, HiddenSize, OutputSize]));
  for I := 0 to HiddenSize - 1 do
  begin
    O.Add(format('hidden_nodes.nodes[%d].output:=%g ;',
      [I, HiddenNodes.nodes[I].output]));
    for J := 0 to HiddenNodes.weights - 1 do
      O.Add(format('hidden_nodes.nodes[%d].weights[%d]:=%g;',
        [I, J, HiddenNodes.nodes[I].weights[J]]));
  end;
  for I := 0 to OutputSize - 1 do
  begin
    O.Add(format('output_nodes.nodes[%d].output:=%g;',
      [I, OutputNodes.nodes[I].output]));
    for J := 0 to OutputNodes.weights - 1 do
      O.Add(format('output_nodes.nodes[%d].weights[%d]:=%g;',
        [I, J, OutputNodes.nodes[I].weights[J]]));
  end;
  O.SaveToFile(AFileName);
  O.Free;
end;

procedure TNNWork.Abort;
begin
  FAbort := True;
end;

function TNNWork.Clone: TNNWork;
begin
  TObject(Result) := TNNWork.newinstance;
  Result.FTrainAdjustHidden := 0;
  Result.FTrainAdjustOutput := 0;
  Result.FTrainHiddenErrorTerms := 0;
  Result.FTrainHiddenRun := 0;
  Result.FTrainOutPutRun := 0;
  Result.FOutputSize := OutputSize;
  Result.FHiddenSize := HiddenSize;
  Result.FInputSize := InputSize;
  Result.FOutputNodes := OutputNodes.Clone;
  Result.FHiddenNodes := HiddenNodes.Clone;
end;

procedure TNNWork.Run(const AData: TVarArrayOfDType; var AResult: TVarArrayOfDType);
var
  I, J, K: integer;
  sum: DType;
begin
  if (InputSize = 0) or (HiddenSize = 0) or (OutputSize = 0) then
    raise Exception.Create('Warning: stupid dimensions. No action taken.');
  if Length(AData) < InputSize then
    raise Exception.Create('Warning: length(data)<input_size. No action taken.');
  if Length(AResult) < OutputSize then
    raise Exception.Create('Warning: length(data)<input_size. No action taken.');
  for J := 0 to HiddenSize - 1 do
  begin
    sum := 0;
    for I := 0 to InputSize - 1 do
      sum := sum + HiddenNodes.nodes[J].weights[I] * AData[I];
    HiddenNodes.nodes[J].output := Sigmoid(sum);
  end;
  for K := 0 to OutputSize - 1 do
  begin
    sum := 0;
    for J := 0 to HiddenSize - 1 do
      sum := sum + OutputNodes.nodes[K].weights[J] * HiddenNodes.nodes[J].output;
    AResult[K] := Sigmoid(sum);
  end;
end;

function TNNWork.RunE(const AData: TVarArrayOfDType; var AResult: TVarArrayOfDType): DType;
var
  I, J, K: integer;
  r, sum: DType;
begin
  Result := 0.0;
  if (InputSize = 0) or (HiddenSize = 0) or (OutputSize = 0) then
    raise Exception.Create('Warning: stupid dimensions. No action taken.');
  if Length(AData) < InputSize then
    raise Exception.Create('Warning: length(data)<input_size. No action taken.');
  if Length(AResult) < OutputSize then
    raise Exception.Create('Warning: length(data)<input_size. No action taken.');
  for J := 0 to HiddenSize - 1 do
  begin
    sum := 0;
    for I := 0 to InputSize - 1 do
      sum := sum + HiddenNodes.nodes[J].weights[I] * AData[I];
    HiddenNodes.nodes[J].output := Sigmoid(sum);
  end;
  for K := 0 to OutputSize - 1 do
  begin
    sum := 0;
    for J := 0 to HiddenSize - 1 do
      sum := sum + OutputNodes.nodes[K].weights[J] * HiddenNodes.nodes[J].output;
    r := Sigmoid(sum);
    AResult[K] := r;
    Result := Result + sqr(OutputNodes.nodes[K].output - r);
  end;
end;


// Training routine for the network. Uses data as input, compares output with
// desired output, computes errors, adjusts weights attached to each node,
// then repeats until the mean squared error at the output is less than
// max_MSE. The learning rate is eta.

function TNNWork.TrainHiddenRun(const J: integer; const TrainData: PNNTrainData): DType;
var
  sum: DType;
  I: integer;
begin
  InterLockedIncrement(FTrainHiddenRun);
  with TrainData^ do
  begin
    sum := 0;
    with HiddenNodes do
      for I := 0 to InputSize - 1 do
        sum := sum + nodes[J].weights[I] * Data[I];
    HiddenNodes.nodes[J].output := Sigmoid(sum);
  end;
end;

function TNNWork.TrainOutPutRun(const K: integer; const TrainData: PNNTrainData): DType;
var
  delta, sum: DType;
  J: integer;
begin
  InterLockedIncrement(FTrainOutPutRun);
  with TrainData^ do
  begin
    sum := 0;
    with OutputNodes do
      for J := 0 to HiddenSize - 1 do
        sum := sum + nodes[K].weights[J] * HiddenNodes.nodes[J].output;
    Output[K] := Sigmoid(sum);
    delta := Desired[K] - Output[K];
    Output_weight_delta[K] := delta * (Output[K] * (1 - Output[K]));
    Result := sqr(delta);
  end;
end;

function TNNWork.TrainAdjustOutput(const K: integer;
  const TrainData: PNNTrainData): DType;
var
  J: integer;
  e: DType;
begin
  InterLockedIncrement(FTrainAdjustOutput);
  with TrainData^ do
  begin
    e := Eta^;
    with OutputNodes do
      for J := 0 to HiddenSize - 1 do
        nodes[K].weights[J] :=
          nodes[K].weights[J] + e * Output_weight_delta[K] *
          HiddenNodes.nodes[J].output;
  end;
end;

function TNNWork.TrainHiddenErrorTerms(const J: integer;
  const TrainData: PNNTrainData): DType;
var
  K: integer;
  sum: DType;
begin
  InterLockedIncrement(FTrainHiddenErrorTerms);
  with TrainData^ do
  begin
    sum := 0;
    with OutputNodes do
      for K := 0 to OutputSize - 1 do
        sum := sum + Output_weight_delta[K] * nodes[K].weights[J];
    with HiddenNodes do
      hidden_weight_delta[J] :=
        sum * nodes[J].output * (1 - nodes[J].output);
  end;
end;

function TNNWork.TrainAdjustHidden(const J: integer;
  const TrainData: PNNTrainData): DType;
var
  I: integer;
  e: DType;
begin
  InterLockedIncrement(FTrainAdjustHidden);
  with TrainData^ do
  begin
    e := Eta^;
    with HiddenNodes do
      for I := 0 to InputSize - 1 do
        nodes[J].weights[I] :=
          nodes[J].weights[I] + e * Hidden_weight_delta[J] * Data[I];
  end;
end;

function TNNWork.DoTrain(const ATrainData: TNNTrainData): DType;
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
  _MSE_max_ := ATrainData.MSE_max^;
  with ATrainData do
    while True do
    begin
      // run & compute MSE
      for J := 0 to HiddenSize - 1 do
      begin
        Assert(True);
        TrainHiddenRun(J, @ATrainData);
      end;
      MSE := 0;
      for K := 0 to OutputSize - 1 do
      begin
        Assert(True);
        MSE := MSE + TrainOutPutRun(K, @ATrainData);
      end;

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
      begin
        Assert(True);
        TrainHiddenErrorTerms(J, @ATrainData);
      end;

      //adjusts weights
      for K := 0 to OutputSize - 1 do
      begin
        Assert(True);
        TrainAdjustOutput(K, @ATrainData);
      end;
      for J := 0 to HiddenSize - 1 do
      begin
        Assert(True);
        TrainAdjustHidden(J, @ATrainData);
      end;
      if FAbort or (_MSE_max_ = 0) then
        break;
    end;
  writeln(format('DONE - MSE = %g, N = %d', [MSE, N]));
  Result := MSE;
end;


procedure TNNWork.Train(const Data: TVarArrayOfDType; const desired: TVarArrayOfDType;
  const max_MSE, eta: DType);
var
  output, output_weight_delta, hidden_weight_delta: array of DType;
  {MSE, }MSE_max: DType; // slight speed enhancement
  //  J, K: integer;
  //  N: integer;
  TrainData: TNNTrainData;
  //  C: cardinal;
begin
  FAbort := False;
  MSE_max := max_MSE * 2.0;
  //  N := 0;
  if (InputSize = 0) or (HiddenSize = 0) or (OutputSize = 0) then
    raise Exception.Create('Warning: stupid dimensions. No action taken.');
  SetLength(output, OutputSize);
  SetLength(output_weight_delta, OutputSize);
  SetLength(hidden_weight_delta, HiddenSize);
  TrainData.Refs := 0;
  TrainData.Eta := @eta;
  TrainData.MSE_max := @MSE_max;
  TrainData.Data := @Data;
  TrainData.Hidden_weight_delta := @hidden_weight_delta;
  TrainData.Output_weight_delta := @output_weight_delta;
  TrainData.Desired := @desired;
  TrainData.Output := @output;
  FLast_MSE := DoTrain(TrainData);
  setlength(output, 0);
  setlength(output_weight_delta, 0);
  setlength(hidden_weight_delta, 0);
  Writeln('TrainAdjustHidden ', FTrainAdjustHidden);
  Writeln('TrainAdjustOutput ', FTrainAdjustOutput);
  Writeln('TrainHiddenErrorTerms ', FTrainHiddenErrorTerms);
  Writeln('TrainHiddenRun ', FTrainHiddenRun);
  Writeln('TrainOutPutRun ', FTrainOutPutRun);
end;

const
  MAX_USE_CPU_COUNT = 4;
  HALF_USE_CPU_COUNT = 2;


procedure TNNWork.TrainT(const Data: TVarArrayOfDType; const desired: TVarArrayOfDType;
  const max_MSE, eta: DType);

var
  output, output_weight_delta, hidden_weight_delta: array of DType;
  MSE, MSE_max: DType; // slight speed enhancement
  I, J, K: integer;
  N: integer;
  D: TNNTrainData;
  EA: array[0..4] of TEvent;
  E: TEvent;
  P: TVarPointerArray;
  C: cardinal;
begin
  FAbort := False;
  for I := 0 to High(EA) do
    EA[I] := TEvent.Create(nil, False, False, '');

  SetLength(P, MAX_USE_CPU_COUNT);
  MSE_max := max_MSE * 2.0;
  N := 0;
  if (InputSize = 0) or (HiddenSize = 0) or (OutputSize = 0) then
    raise Exception.Create('Warning: stupid dimensions. No action taken.');
  SetLength(output, OutputSize);
  SetLength(output_weight_delta, OutputSize);
  SetLength(hidden_weight_delta, HiddenSize);
  D.Refs := 0;
  D.Eta := @eta;
  D.MSE_max := @MSE_max;
  D.Data := @Data;
  D.Hidden_weight_delta := @hidden_weight_delta;
  D.Output_weight_delta := @output_weight_delta;
  D.Desired := @desired;
  D.Output := @output;
  C := 0;
  while True do
  begin
    // run & compute MSE
    J := 0;
    E := EA[0];
    E.ResetEvent;
    NNThreadTrain(TrainHiddenRun, @D, J, HiddenSize - 1, E, MAX_USE_CPU_COUNT, nil);
    repeat
      if E.WaitFor(INFINITE) <> wrSignaled then
        raise Exception.Create('process error');
    until D.Refs = 0;

    MSE := 0.0;
    K := 0;
    E := EA[1];
    E.ResetEvent;
    NNThreadTrain(TrainOutPutRun, @D, K, OutputSize - 1, E, MAX_USE_CPU_COUNT, @p);
    repeat
      if E.WaitFor(INFINITE) <> wrSignaled then
        raise Exception.Create('process error');
    until D.Refs = 0;

    for I := 0 to MAX_USE_CPU_COUNT - 1 do
      MSE := MSE + NNThreadResult(P[I]);

    N := N + 1;
    if (GetTickCount - C) > 1000 then
    begin
      writeln(format('MSE = %g, N = %d', [MSE, N]));
      C := GetTickCount;
    end;

    if (MSE < MSE_max) then
      break;
    // And the hidden layer error terms
    J := 0;
    E := EA[2];
    E.ResetEvent;
    NNThreadTrain(TrainHiddenErrorTerms, @D, J, HiddenSize - 1, E,
      MAX_USE_CPU_COUNT, nil);
    repeat
      if E.WaitFor(INFINITE) <> wrSignaled then
        raise Exception.Create('process error');
    until D.Refs = 0;

    //adjusts weights
    K := 0;
    E := EA[3];
    E.ResetEvent;
    NNThreadTrain(TrainAdjustOutput, @D, K, OutputSize - 1, E, MAX_USE_CPU_COUNT, nil);
    repeat
      if E.WaitFor(INFINITE) <> wrSignaled then
        raise Exception.Create('process error');
    until D.Refs = 0;

    J := 0;
    E := EA[4];
    E.ResetEvent;
    NNThreadTrain(TrainAdjustHidden, @D, J, HiddenSize - 1, E, MAX_USE_CPU_COUNT, nil);
    repeat
      if E.WaitFor(INFINITE) <> wrSignaled then
        raise Exception.Create('process error');
    until D.Refs = 0;

    if (max_MSE = 0) or (FAbort) then
      break;
  end;
  FLast_MSE := MSE;
  writeln(format('DONE - MSE = %g, N = %d', [MSE, N]));
  setlength(output, 0);
  setlength(output_weight_delta, 0);
  setlength(hidden_weight_delta, 0);
  Writeln('TrainAdjustHidden ', FTrainAdjustHidden);
  Writeln('TrainAdjustOutput ', FTrainAdjustOutput);
  Writeln('TrainHiddenErrorTerms ', FTrainHiddenErrorTerms);
  Writeln('TrainHiddenRun ', FTrainHiddenRun);
  Writeln('TrainOutPutRun ', FTrainOutPutRun);
  for I := 0 to High(EA) do
    FreeAndNil(EA[I]);
end;

procedure TNNWork.SetOutput(const desired: TVarArrayOfDType);
var
  K: integer;
begin
  Assert(Assigned(desired));
  for K := 0 to OutputSize - 1 do
  begin
    if K >= Length(desired) then
      Break;
    OutputNodes.nodes[K].output := desired[K];
  end;
end;

end.
