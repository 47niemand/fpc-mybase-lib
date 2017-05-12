unit TestCaseCore;

{$mode delphi}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  { TTestCaseCore }

  TTestCaseCore = class(TTestCase)
  published
    procedure TestTest;
  end;

implementation

uses Math, uCoreCompareDistance;

{ TTestCaseCore }

procedure LevenshteinRestore(const S, T: string; const D: TVarMatix; var A, B: string);
var
  I, J, N: integer;
begin
  A := '';
  B := '';
  I := Length(S);
  J := Length(T);

  Assert(Length(D) = (I + 1));
  for N := 0 to I do
    Assert(Length(D[I]) = (J + 1));

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
end;

procedure TTestCaseCore.TestTest;

  procedure check(const a, b: string);
  var
    LC, LS: double;
    LD: integer;
    Ta: double;
    D: TVarMatix;
    S, T: string;
  begin
    LS := LevenshteinScore(a, b);
    LD := LevenshteinDistance(a, b);
    Ta := Tanimoto(A, B);
    LevenshteinCore(a, b, d);
    if LD <> 0 then
      LC := Min(Length(a), Length(b) - LD) / LD
    else
      LC := 1.0;
    LevenshteinRestore(A, B, D, S, T);

    Writeln(format('"%S"?"%S" = '#9' LS=%d%%(%d%%) '#9' LD=%d '#9' T=%d%%',
      [A, B, Round(LS * 100), Round(Lc * Ta * 100), LD, Round(Ta * 100)]));

  end;

var
  N: TStrings;
  S1, S2: string;

begin
  N := TStringList.Create;
  try

    N.add('AA5511KI');
    N.add('A5511KI');
    N.add('AA551K');
    N.add('AB5135KB');
    N.add('ABKB');
    N.add('AA');
    N.Add('BBBB');
    N.Add('AABB');
    N.Add('AAABB');
    N.Add('AACBB');

    check('', '');

    for S1 in N do
      for S2 in N do
        check(S1, S2);

  finally
    N.Free;
  end;

end;

initialization

  RegisterTest(TTestCaseCore);
end.







