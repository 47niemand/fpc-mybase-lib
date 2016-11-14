unit NNNeuron;

{$mode delphi}

interface

uses
  Classes, SysUtils, NNWorkTypes;

type
  { TNeuron }

  TNeuron = class
  public
    weights: array of DType;
    output: DType;
    constructor Create(const ASize: integer);
    destructor Destroy; override;
    function Clone: TNeuron;
  end;

implementation

{ TNeuron }

constructor TNeuron.Create(const ASize: integer);
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
  SetLength(Result.weights, Length(weights));
  Result.output := output;
  for I := 0 to Length(weights) - 1 do
    Result.weights[I] := weights[I];
end;


end.

