unit NNWork;

{$mode delphi}

interface

uses NNWorkTypes, NNLayer, NNNeuron;

type
  TNNWorkTrainStrategy = class;
  TNNWorkTrainStrategyClass = class of TNNWorkTrainStrategy;

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
    procedure SetTrainStrategy(AValue: TNNWorkTrainStrategyClass);
  protected
    FInputSize: integer;
    FOutputSize: integer;
    FHiddenSize: integer;
    FOutputNodes: TNNLayer;
    FHiddenNodes: TNNLayer;
    FLast_MSE: DType;
    FTrainStrategy: TNNWorkTrainStrategy;
  public
    property Abort: boolean read FAbort;
    function TrainAdjustHidden(const J: integer; const TrainData: PNNTrainData): DType;
    function TrainAdjustOutput(const K: integer; const TrainData: PNNTrainData): DType;
    function TrainHiddenErrorTerms(const J: integer;
      const TrainData: PNNTrainData): DType;
    function TrainHiddenRun(const J: integer; const TrainData: PNNTrainData): DType;
    function TrainOutPutRun(const K: integer; const TrainData: PNNTrainData): DType;
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
    // Run args are input data, output
    procedure Run(const AData: TVarArrayOfDType; var AResult: TVarArrayOfDType);
    // Set Output nerurons
    procedure SetOutput(const desired: TVarArrayOfDType);
    // Run args are input data, output, return MSE between result and output nodes values
    function RunE(const AData: TVarArrayOfDType; var AResult: TVarArrayOfDType): DType;
    class function Load(const AFileName: string): TNNWork;
    procedure Save(const AFileName: string);
    procedure Cancel;
    function Clone: TNNWork;
    property TrainStrategy: TNNWorkTrainStrategyClass write SetTrainStrategy;
  end;

  { TNNWorkTrainStrategy }

  TNNWorkTrainStrategy = class(TObject)
  public
    function DoTrain(const NN: TNNWork; const ATrainData: TNNTrainData): DType;
      virtual; abstract;
    class function GetInstance: TNNWorkTrainStrategy; virtual; abstract;
  end;


function Sigmoid(const AData: DType): DType;

var
  DefaultTrainStrategy: TNNWorkTrainStrategyClass = nil;

implementation

{ TNNWork }

uses
  Classes, SysUtils, NNWorkStrategy;

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


{ TNNWork }

constructor TNNWork.Create(const AInput, AHidden, AOutput: integer);
begin
  inherited Create;
  FTrainStrategy := DefaultTrainStrategy.GetInstance;
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

procedure TNNWork.Train(const Data: TVarArrayOfDType; const desired: TVarArrayOfDType;
  const max_MSE, eta: DType);
var
  output, output_weight_delta, hidden_weight_delta: array of DType;
  MSE_max: DType; // slight speed enhancement
  TrainData: TNNTrainData;
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
  TrainData.Eta := eta;
  TrainData.MSE_max := MSE_max;
  TrainData.Data := @Data;
  TrainData.hidden_weight_delta := @hidden_weight_delta;
  TrainData.output_weight_delta := @output_weight_delta;
  TrainData.desired := @desired;
  TrainData.output := @output;
  Write(format('Train using strategy %p ', [pointer(FTrainStrategy)]));
  Writeln(format('%s', [FTrainStrategy.ClassName]));
  FLast_MSE := FTrainStrategy.DoTrain(Self, TrainData);
  setlength(output, 0);
  setlength(output_weight_delta, 0);
  setlength(hidden_weight_delta, 0);
  Writeln('TrainAdjustHidden ', FTrainAdjustHidden);
  Writeln('TrainAdjustOutput ', FTrainAdjustOutput);
  Writeln('TrainHiddenErrorTerms ', FTrainHiddenErrorTerms);
  Writeln('TrainHiddenRun ', FTrainHiddenRun);
  Writeln('TrainOutPutRun ', FTrainOutPutRun);
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
            if K >= NN.HiddenNodes.GetWeights then
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
            if K >= NN.OutputNodes.GetWeights then
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
      for J := 0 to HiddenNodes.GetWeights - 1 do
        Write(T, HiddenNodes.nodes[I].weights[J], ' ');
      writeln(T);
    end;
    Writeln(T, '# output layer');
    for I := 0 to OutputSize - 1 do
    begin
      for J := 0 to OutputNodes.GetWeights - 1 do
        Write(T, OutputNodes.nodes[I].weights[J], ' ');
      writeln(T);
    end;
    Writeln(T, '#end');
  finally
    Close(T);
  end;
end;

procedure TNNWork.Cancel;
begin
  FAbort := True;
end;

function TNNWork.Clone: TNNWork;
begin
  TObject(Result) := TNNWork.newinstance;
  Result.FTrainStrategy := Self.FTrainStrategy;
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

function TNNWork.RunE(const AData: TVarArrayOfDType;
  var AResult: TVarArrayOfDType): DType;
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
    output[K] := Sigmoid(sum);
    delta := desired[K] - output[K];
    output_weight_delta[K] := delta * (output[K] * (1 - output[K]));
    Result := sqr(delta);
  end;
end;

function TNNWork.TrainAdjustOutput(const K: integer;
  const TrainData: PNNTrainData): DType;
var
  J: integer;
begin
  InterLockedIncrement(FTrainAdjustOutput);
  with TrainData^ do
    with OutputNodes do
      for J := 0 to HiddenSize - 1 do
        nodes[K].weights[J] :=
          nodes[K].weights[J] + eta * output_weight_delta[K] *
          HiddenNodes.nodes[J].output;
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
        sum := sum + output_weight_delta[K] * nodes[K].weights[J];
    with HiddenNodes do
      hidden_weight_delta[J] :=
        sum * nodes[J].output * (1 - nodes[J].output);
  end;
end;

procedure TNNWork.SetTrainStrategy(AValue: TNNWorkTrainStrategyClass);
begin
  FTrainStrategy := AValue.GetInstance;
end;

function TNNWork.TrainAdjustHidden(const J: integer;
  const TrainData: PNNTrainData): DType;
var
  I: integer;
begin
  InterLockedIncrement(FTrainAdjustHidden);
  with TrainData^ do
    with HiddenNodes do
      for I := 0 to InputSize - 1 do
        nodes[J].weights[I] :=
          nodes[J].weights[I] + Eta * hidden_weight_delta[J] * Data[I];
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

initialization
  DefaultTrainStrategy := TNNWorkTrainStrategyDefault;

end.
