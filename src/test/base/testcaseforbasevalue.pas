unit TestCaseForBaseValue;

{$mode delphi}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestCaseForBaseValue }

  TTestCaseForBaseValue = class(TTestCase)
  published
    procedure TestBaseValueInitalization;
    procedure TestBaseVariantCompare;
    procedure TestBaseValueCompare;
    procedure TestBaseValueSetGet;
    procedure TestBaseValueEvents;
    procedure TestBaseValueLeaks;
  end;

implementation

uses variants, uBaseTestUtils, uBaseConsts, uBaseObserver,
  uBaseInterface, uBaseValue;

procedure TTestCaseForBaseValue.TestBaseValueInitalization;
var
  V: IBaseValue;
begin
  V := nil;
  AssertEquals('should be any values', 0, GetBaseValueCounter);
  V := NewValue;
  AssertEquals('should be assigned by NewValue', True, Assigned(V));
  AssertEquals('one value created', 1, GetBaseValueCounter);
  V := nil;
  //  AssertEquals('value should be destoryed', 0, GetBaseValueCounter);
  V := NewValue(null);
  AssertEquals('should be assigned by NewValue(params)', True, Assigned(V));
  AssertEquals('one value created', 1, GetBaseValueCounter);
  V := nil;
  //  AssertEquals('value should be destoryed', 0, GetBaseValueCounter);
  V := NewImmutableValue(1);
  AssertEquals('should be assigned by NewValueReadonly(params)', True, Assigned(V));
  AssertEquals('one value created', 1, GetBaseValueCounter);
  V := nil;
end;

procedure TTestCaseForBaseValue.TestBaseVariantCompare;
var
  A: variant;
  B: variant;
  Res: TVariantRelationship;
begin
  //numbers
  A := 1;
  B := 1;
  res := VarCompare(a, b);
  AssertEquals('A=B', True, Res = vrEqual);
  A := 1;
  B := 2;
  res := VarCompare(a, b);
  AssertEquals('A<B', True, Res = vrLessThan);
  A := 2;
  B := 1;
  res := VarCompare(a, b);
  AssertEquals('A>B', True, Res = vrGreaterThan);
  //strings
  A := 'test';
  B := 'test';
  res := VarCompare(a, b);
  AssertEquals('string with same data is equal', True, Res = vrEqual);
  res := VarCompare(a, a);
  AssertEquals('compare with same is always equal', True, Res = vrEqual);
  A := 'aaaa';
  B := 'aaab';
  res := VarCompare(a, b);
  AssertEquals('by char comare', True, Res = vrLessThan);
  A := 'test';
  B := 'a';
  res := VarCompare(a, b);
  AssertEquals('the longest is aways greater', True, Res = vrGreaterThan);
  A := 'Test';
  B := 'test';
  res := VarCompare(a, b);
  AssertEquals('case sentecive', True, Res = vrLessThan);
  // varArrays
  a := VarArrayOf([1, 'test', 3]);
  b := VarArrayOf([1, 'test', 3]);
  res := VarCompare(a, a);
  AssertEquals(True, Res = vrEqual);
  res := VarCompare(a, b);
  AssertEquals(True, Res = vrNotEqual);
  a := VarArrayOf([1, 'test', 3]);
  b := VarArrayOf([1, 'test', 3, '4']);
  res := VarCompare(a, b);
  AssertEquals('second array is greater we got a < b', True, Res = vrLessThan);
  a := null;
  b := Unassigned;
  res := VarCompare(a, b);
  AssertEquals('null <> Unassigned', True, Res = vrNotEqual);
  a := null;
  b := null;
  res := VarCompare(a, b);
  AssertEquals('null = null', True, Res = vrEqual);
  a := Unassigned;
  b := Unassigned;
  res := VarCompare(a, b);
  AssertEquals('Unassigned = Unassigned', True, Res = vrEqual);
  a := null;
  b := '';
  res := VarCompare(a, b);
  AssertEquals('any compared with null or unasigned should be notEqual',
    True, Res = vrNotEqual);
  a := Unassigned;
  b := 4;
  res := VarCompare(a, b);
  AssertEquals(True, Res = vrNotEqual);
  //mixed
  a := 1;
  b := '1';
  res := VarCompare(a, b);
  AssertEquals('numbers as string can be compared to numbers', True, Res = vrEqual);
  a := 1.1;
  b := FloatToStrF(1.1, ffGeneral, -1, 0, BaseFormatSettings);
  res := VarCompare(a, b);
  AssertEquals('numbers as string can be compared to numbers', True, Res = vrEqual);
  a := 1 + 1e-8;
  b := 1 + 1e-16;
  res := VarCompare(a, b);
  AssertEquals('double prcession', True, Res = vrGreaterThan);
  a := 1 + 1e-16;
  b := 1 + 1e-16;
  res := VarCompare(a, b);
  AssertEquals('double prcession', True, Res = vrEqual);
  a := '1.0';
  b := '1';
  res := VarCompare(a, b);
  AssertEquals('strings compare as string', True, Res <> vrEqual);
  a := FloatToStrF(1 + 1e-14, ffGeneral, -1, 0, BaseFormatSettings);
  b := 1 + 1e-13;
  AssertEquals('variant should be double type', vardouble, VarType(B));
  res := VarCompare(a, b);
  AssertEquals('double 1e-13 prcession', True, Res = vrEqual);
  a := FloatToStrF(1 + 1e-13, ffGeneral, -1, 0, BaseFormatSettings);
  b := 1 + 1e-6;
  AssertEquals('A should be string', True, VarIsStr(a));
  AssertEquals('B should be double', vardouble, VarType(B));
  res := VarCompare(a, b);
  AssertEquals('double 1e-13 prcession', True, Res <> vrEqual);
  //TODO dates, mixed types with different argument orders.....
end;

procedure TTestCaseForBaseValue.TestBaseValueCompare;
var
  V, Q: IBaseValue;
  A: variant;
  Res: TVariantRelationship;
begin
  V := NewValue(1);
  Res := V.CompareTo(V);
  AssertEquals(True, Res = vrEqual);
  A := V; //convert interface to variant
  Res := V.CompareTo(A);
  AssertEquals(True, Res = vrEqual);
  Res := V.CompareTo(1);
  AssertEquals(True, Res = vrEqual);
  Res := V.CompareTo(2);
  AssertEquals(True, Res = vrLessThan);
  Res := V.CompareTo(0);
  AssertEquals(True, Res = vrGreaterThan);
  Q := NewValue(5);
  Res := V.CompareTo(Q);
  AssertEquals(True, Res = vrLessThan);
  Q.Value := 1;
  Res := V.CompareTo(Q);
  AssertEquals(True, Res = vrEqual);
  //  I := TInterfacedObject.Create;
  //  Res := V.CompareTo(I);
  //  AssertEquals(True, Res = vrNotEqual);
end;

procedure TTestCaseForBaseValue.TestBaseValueSetGet;
var
  V, Q: IBaseValue;
  A, B: variant;
  Res: TVariantRelationship;
  I: integer;
begin
  // simple set/get/compare
  V := NewValue(1);
  Q := NewValue(2);
  A := V.GetValue;
  AssertEquals(1, A);
  V.Value := 2;
  B := V.Value;
  AssertEquals(2, B);
  Q.Value := 1;
  V.Value := 2;
  Res := V.CompareTo(Q);
  AssertEquals(True, Res <> vrEqual);
  Q.Value := 2;
  B := 2;
  Res := Q.CompareTo(B);
  AssertEquals(True, Res = vrEqual);
  for I := 0 to 999999 do
  begin
    A := TestData;
    V.Value := A;
    B := V.Value;
    AssertEquals('consistency check', True, VarCompare(A, B) = vrEqual);
  end;
  Writeln('BaseValueCollisions ', GetBaseValueCollisions);
  Writeln('BaseValueLongestCollision ', GetBaseValueLongestCollision);
  Writeln('BaseValueMaxGetQueue ', GetBaseValueMaxGetQueue);
  Writeln('BaseValueMaxSetQueue ', GetBaseValueMaxSetQueue);
  Writeln('BaseValueReadSkipCount ', GetBaseValueReadSkipCount);
  Writeln('BaseValueReadLongestSkip ', GetBaseValueReadLongestSkip);
  Writeln('BaseValueNewCounter ', GetBaseValueNewCounter);
  Writeln('BaseValueDisposeCounter ', GetBaseValueDisposeCounter);
  Writeln('BaseValueCounter ', GetBaseValueCounter);
end;


var
  FTestObserverLastOperation: TBaseObservedOperation = booCustom;
  FTestObserverCounter: integer = 0;
  FTestObserverThrowError: boolean = False;

type

  { TTestBaseValueObserver }

  TTestBaseValueObserver = class(TBaseObserver)
    procedure Update(const Subject: IUnknown;
      const AOperation: TBaseObservedOperation; const {%H-}Data: IUnknown); override;
  end;

{ TTestObserver }

procedure TTestBaseValueObserver.Update(const Subject: IUnknown;
  const AOperation: TBaseObservedOperation; const Data: IUnknown);
var
  V, D: IBaseValue;
  A: variant;
begin
  Inc(FTestObserverCounter);
  FTestObserverLastOperation := AOperation;

  D := CastBaseValue(Subject);
  // passed param should be IBaseValue

  V := CastBaseValue(Subject);
  a := V.Value;
  Assert(Assigned(@A));
  // hint: don't try to change value on call back
  // you can throw exception for prevent changing
  if FTestObserverThrowError then
    raise Exception.Create('test exception');
end;


procedure TTestCaseForBaseValue.TestBaseValueEvents;
var
  V: IBaseValue;
  S: IBaseSubject;
  O: IBaseObserver;
  N: IBaseVersionCounter;
  Res: longint;
  X: integer;
begin
  FTestObserverCounter := 0;
  FTestObserverThrowError := False;
  V := NewValue(0);
  Res := V.QueryInterface(IBaseSubject, S);
  AssertEquals('should support IBaseSubject', S_OK, Res);
  O := TTestBaseValueObserver.Create;
  S.Attach(O);
  AssertEquals('no notifications', 0, FTestObserverCounter);
  V.Value := 'test';
  AssertEquals('has notification', 1, FTestObserverCounter);
  V.Value := 'test';
  AssertEquals('next notification', 2, FTestObserverCounter);
  V.Value := pi();
  AssertEquals('next notification', 3, FTestObserverCounter);
  AssertEquals('check operation', True, FTestObserverLastOperation = booChange);

  N := CastBaseVersionCounter(V);
  X := N.Version;
  V.Value := 'last value';
  AssertEquals('next notification', 4, FTestObserverCounter);
  AssertEquals('version check', X + 1, N.Version);
  AssertEquals('version=updates', FTestObserverCounter, N.Version);

  FTestObserverThrowError := True;
  try
    V.Value := 'new value';
    Fail('shoud be exceptipn');
  except
    AssertEquals('notfiy called', 5, FTestObserverCounter);
  end;
  AssertEquals('no version change', X + 1, N.Version);
end;

procedure TTestCaseForBaseValue.TestBaseValueLeaks;
begin
  AssertEquals('all values should be destoryed', 0, GetBaseValueCounter);
  AssertEquals('check new=dispose', GetBaseValueNewCounter,
    GetBaseValueDisposeCounter);
  AssertEquals('check observers', 0, GetBaseObserverCounter);
  AssertEquals('check subjects', 0, GetBaseSubjectCounter);
end;

initialization

  RegisterTest(TTestCaseForBaseValue);
end.
