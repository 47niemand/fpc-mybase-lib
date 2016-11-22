unit TestCaseForBValueConcurency;

{$mode delphi}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, uBaseInterface;

var
  G: TInterfacedObject;
  Gi: IUnknown;

type
  { TRunner }

  TRunner = class(TThread)
  protected
    FError: boolean;
    FErrorMessage: string;
    FCount: integer;
    FValue: IBaseValue;
    FActive: boolean;
    procedure Stop;
    procedure Execute; override;
  public
    constructor Create(const AValue: IBaseValue);
    destructor Destroy; override;
  end;

  { TTestCaseForBValueConcurency }

  TTestCaseForBValueConcurency = class(TTestCase)
  private
  published
    procedure TestMurmur;
    procedure TestBaseValueConcurency;
    procedure TestBaseValueLeaks;
  end;

implementation

uses contnrs, variants, uBaseValue, uBaseTestUtils, uBaseObserver;


{ TRunner }

procedure TRunner.Stop;
begin
  FActive := False;
  if (not Finished) and Suspended then
    Resume;
end;

procedure TRunner.Execute;
var
  s: variant;
  b: boolean;
  K: integer;
begin
  Inc(FCount);
  try
    Writeln(format('[%x] thread execute...', [Handle]));
    while FActive do
    begin
      Inc(FCount);
      K := Random(2);
      //Writeln(format('%x START Runner %d', [Handle, K]));
      case K of
        1:
        begin
          s := FValue.Value;
          b := Validate(s);
          Assert(b, 'data corrupted');
        end;
        0:
          FValue.Value := TestData;
      end;
      //       Writeln(format('%x FINISH Runner %d', [Handle, K]));
      //            sleep(1);
    end;

  except
    on e: Exception do
    begin
      FError := True;
      FErrorMessage := e.ClassName + ' ' + e.Message;
      Writeln(format('%x ERROR %s', [Handle, FErrorMessage]));
      raise;
    end;
  end;
end;

constructor TRunner.Create(const AValue: IBaseValue);
begin
  FValue := AValue;
  Priority := tpTimeCritical;
  FActive := True;
  inherited Create(False);
end;

destructor TRunner.Destroy;
begin
  FActive := False;
  if (not Finished) and Suspended then
    Resume;
  WaitFor;
  Assert(Finished);
  FValue := nil;
  if FError then
    Writeln(format('[%x] thread error = %s', [Handle, FErrorMessage]));
  Writeln(format('[%x] thread destroy Count = %d', [Handle, FCount]));
  inherited Destroy;
end;



procedure TTestCaseForBValueConcurency.TestMurmur;
var
  x: variant;
begin
  x := AddMurmur2('');
  AssertEquals(True, Validate(x));
  x := AddMurmur2('1');
  AssertEquals(True, Validate(x));
  x := TestData;
  AssertEquals(True, Validate(x));
  x := TestData;
  AssertEquals(True, Validate(x));
  x := TestData;
  AssertEquals(True, Validate(x));

  x := TestString;
  AssertEquals(True, VarIsStr(x));
  AssertEquals(True, Validate(x));
  x := TestTime;
  AssertEquals(vardate, VarType(x));
  AssertEquals(True, Validate(x));
  x := TestInteger;
  AssertEquals(True, VarIsOrdinal(x));
  AssertEquals(True, Validate(x));
  x := TestDouble;
  AssertEquals(True, VarIsFloat(x));
  AssertEquals(True, Validate(x));
end;

procedure TTestCaseForBValueConcurency.TestBaseValueConcurency;
var
  T: TObjectList;
  V: IBaseValue;
  I: integer;
  C: integer;
  P: pointer;
begin
  Gi := nil;
  G := TInterfacedObject.Create;
  Gi := G;
  V := NewValue(TestData);
  t := TObjectList.Create(True);
  // try creae 64 threads
  for I := 0 to 7 do
    t.Add(TRunner.Create(V));
  Sleep(8000); //wait for run
  for P in T do
    TRunner(P).Stop;
  Sleep(1000); //waith for stop
  C := 0;
  for P in T do
  begin
    AssertEquals(TRunner(P).FErrorMessage, False, TRunner(P).FError);
    AssertEquals('Thread still runing', True, TRunner(P).Finished);
    C := C + TRunner(P).FCount;
  end;
  T.Free;
  Writeln('Total cycles ', c);
  Writeln('BaseValueCollisions ', GetBaseValueCollisions);
  Writeln('BaseValueLongestCollision ', GetBaseValueLongestCollision);
  Writeln('BaseValueMaxGetQueue ', GetBaseValueMaxGetQueue);
  Writeln('BaseValueMaxSetQueue ', GetBaseValueMaxSetQueue);
  Writeln('BaseValueReadSkipCount ', GetBaseValueReadSkipCount);
  Writeln('BaseValueReadLongestSkip ', GetBaseValueReadLongestSkip);
  Writeln('BaseValueNewCounter ', GetBaseValueNewCounter);
  Writeln('BaseValueDisposeCounter ', GetBaseValueDisposeCounter);
end;

procedure TTestCaseForBValueConcurency.TestBaseValueLeaks;
begin
  AssertEquals('all values should be destoryed', 0, GetBaseValueCounter);
  AssertEquals('check new=dispose', GetBaseValueNewCounter,
    GetBaseValueDisposeCounter);
  AssertEquals('check observers', 0, GetBaseObserverCounter);
  AssertEquals('check subjects', 0, GetBaseSubjectCounter);
end;

initialization

  RegisterTest(TTestCaseForBValueConcurency);
end.
