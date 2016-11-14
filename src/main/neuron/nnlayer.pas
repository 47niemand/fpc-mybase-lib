unit NNLayer;

{$mode delphi}

interface

uses
  Classes, SysUtils, NNNeuron, NNWorkTypes;

type
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


implementation

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


end.

