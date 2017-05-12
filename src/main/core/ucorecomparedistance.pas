unit uCoreCompareDistance;

interface

type
  TVarMatix = array of array of integer;

procedure LevenshteinCore(const S, T: string; var D: TVarMatix);
function LevenshteinScore(const S, T: string): double;
function LevenshteinDistance(const S, T: string): integer;

function Tanimoto(const S, T: string): double;

implementation

uses Math;

function CreateVarMatix(const rows, cols: integer): TVarMatix;
var
  I, J: Integer;
begin
  SetLength(Result, rows);
  for I := 0 to rows - 1 do
  begin
    SetLength(Result[I], cols);
    for J := 0 to cols - 1 do
      Result[I, J] := 0;
  end;
end;

function Tanimoto(const S, T: string): double;
var
  a, b, c: integer;
  sym: char;
begin
  a := Length(S);
  b := Length(T);

  if (a = 0) or (b = 0) then
  begin
    Result := 0.0;
    exit;
  end;

  c := 0;
  for sym in S do
    if pos(sym, T) > 0 then
      Inc(c);

  Result := c / (a + b - c);
  if Result > 1 then
    Result := 1.0 / Result;
end;

procedure LevenshteinCore(const S, T: string; var D: TVarMatix);
var
  I, J, M, N: integer;
  score: array[1..3] of integer;

  function SubstitutionCost(I, J: integer): integer;
  begin
    if S[I] = T[J] then
      Result := 0
    else
      Result := 1;
  end;

begin
  M := length(S); //i
  N := length(T); //j

  D := CreateVarMatix(M + 1, N + 1);

  for I := 1 to M do
    D[I, 0] := I;

  for J := 1 to N do
    D[0, J] := J;

  for I := 1 to M do
    for J := 1 to N do
    begin
      score[1] := D[I - 1, J] + 1; // deletion
      score[2] := D[I, J - 1] + 1; // insertion
      score[3] := D[I - 1, J - 1] + SubstitutionCost(I, J); // substitution
      D[I, J] := min(min(score[1], score[2]), score[3]);
    end;

end;

function LevenshteinScore(const S, T: string): double;
var
  M, N: integer;
  D: TVarMatix;

  function DistanceWeight(): double;
  var
    I, J: integer;
    A, B: string;
  begin
    A := '';
    B := '';
    I := M;
    J := N;

    while (I > 0) and (J > 0) do
      if D[I, J] = D[I - 1, J] + 1 then
        I := I - 1
      else
      if D[I, J] = D[I, J - 1] + 1 then
        J := J - 1
      else
      begin
        A := S[I] + A;
        B := T[J] + B;
        I := I - 1;
        J := J - 1;
      end;

    Result := Tanimoto(A, B);
  end;

begin

  M := length(S); //i
  N := length(T); //j

  if (M = 0) or (N = 0) then
  begin
    Result := 0.0;
    exit;
  end;

  LevenshteinCore(S, T, D);

  Assert((M + N - D[M, N]) > 0);
  Result := 1 - D[M, N] / (M + N - D[M, N]);
  if Result < 0 then
    Result := 0
  else
  if Result < 1 then
    Result := Result * DistanceWeight();
end;

function LevenshteinDistance(const S, T: string): integer;
var
  M, N: integer;
  D: TVarMatix;
begin

  M := length(S); //i
  N := length(T); //j

  if (M = 0) or (N = 0) then
  begin
    Result := 0;
    exit;
  end;

  LevenshteinCore(S, T, D);

  Result := D[M, N];
end;

end.
