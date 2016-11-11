unit TestCaseForListsAndSets;

{$mode delphi}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, ubaselist,
  ubaseset, uBaseListHelper;

var
  FTestCounter: integer = 0;


type

  { TTestCaseForListsAndSets }

  TTestCaseForListsAndSets = class(TTestCase)
  private
  published
    procedure TestComparasion;
    procedure TestListHelper;
    procedure TestLists;
    procedure TestSets;
    procedure TestLeak;
  end;

  { ITestIntf
    declare test interface
    }

  ITestIntf = interface
    ['{F4D9EEF7-0564-4637-9D12-6921943BE55F}']
    // test interface with some method
    function GetValue: integer;
  end;

  // reslove generics for fture usage

  // tihs comparasion function type
  // it shoud compare two objects with specified interface
  TTestLeftComparsion = TLeftComparison<ITestIntf>;

  // this is list interface of our ITestInterface
  ITestList = IBaseList<ITestIntf>;

  // this is set interface of our ITestInterface
  ITestSet = IBaseSet<ITestIntf>;


  TListHelperTest = TListHelper<ITestIntf>;

  // Implement Classe

  { TTestClass simple implementation of ITestInterface }

  TTestClass = class(TInterfacedObject, ITestIntf)
    FTestValue: integer;
    function GetValue: integer;
    function TestCompare(ARightItem: ITestIntf): integer; //for test
    constructor Create(const AValue: integer);
    destructor Destroy; override;
    class function New(const AValue: integer): ITestIntf;
  end;

  { TTestList is our list is it neseesary override GetComparisonFunction method }

  TTestList = class(TBaseList<ITestIntf>, ITestList)
  protected
    function GetComparisonFunction: TTestLeftComparsion; override;
  end;

  { TTestSet our set is it neseesary override GetComparisonFunction method  }
  TTestSet = class(TBaseSet<ITestIntf>, ITestSet)
  protected
    function GetComparisonFunction: TTestLeftComparsion; override;
  end;


implementation

uses Math;

// comparsion function
function LeftTestComparison(const Left: ITestIntf; var Value): integer;
var
  Right: ITestIntf absolute Value;
  L, R: integer;
begin
  Assert(Assigned(Left));
  Assert(Assigned(Right));
  R := Right.GetValue;
  L := Left.GetValue;
  Result := CompareValue(L, R);
end;

{ TTestSet }

function TTestSet.GetComparisonFunction: TTestLeftComparsion;
begin
  Result := LeftTestComparison;
end;

{ TTestList }

function TTestList.GetComparisonFunction: TTestLeftComparsion;
begin
  Result := LeftTestComparison;
end;


{ TTestClass }

function TTestClass.GetValue: integer;
begin
  Result := FTestValue;
end;

function TTestClass.TestCompare(ARightItem: ITestIntf): integer;
begin
  Result := CompareValue(GetValue, ARightItem.GetValue);
end;

constructor TTestClass.Create(const AValue: integer);
begin
  FTestValue := AValue;
  Inc(FTestCounter);
end;

destructor TTestClass.Destroy;
begin
  inherited Destroy;
  Dec(FTestCounter);
end;

class function TTestClass.New(const AValue: integer): ITestIntf;
begin
  Result := TTestClass.Create(AValue);
end;

{ TTestCaseForListsAndSets }

procedure TTestCaseForListsAndSets.TestComparasion;
var
  A, B, C: ITestIntf;
  R: integer;
begin
  A := TTestClass.New(1);
  B := TTestClass.New(1);
  C := TTestClass.New(2);

  // A=B, A<C && A<>C, B<C && B<>C

  R := LeftTestComparison(A, B);
  AssertEquals(0, R);

  R := LeftTestComparison(B, A);
  AssertEquals(0, R);

  R := LeftTestComparison(A, C);
  AssertEquals(True, R < 0);

  R := LeftTestComparison(C, A);
  AssertEquals(True, R > 0);

  R := LeftTestComparison(B, C);
  AssertEquals(True, R < 0);

  R := LeftTestComparison(C, B);
  AssertEquals(True, R > 0);
end;

procedure TTestCaseForListsAndSets.TestListHelper;
var
  L: TList;
  A: array[0..9] of ITestIntf;
  O: array[0..9] of integer;
  I, J: integer;
  B: boolean;
begin
  for I := 0 to 9 do
    A[I] := TTestClass.New(I);

  //  pesudo-random order
  O[0] := 2;
  O[1] := 6;
  O[2] := 9;
  O[3] := 0;
  O[4] := 7;
  O[5] := 5;
  O[6] := 3;
  O[7] := 1;
  O[8] := 8;
  O[9] := 4;

  L := TList.Create;
  for I := 0 to 9 do
  begin
    B := TListHelperTest.BinarySearch(L, A[O[I]], J, LeftTestComparison);
    AssertEquals(False, B);
    L.Insert(J, Pointer(A[O[I]]));
  end;

  for I := 0 to 9 do
    AssertEquals(A[I].GetValue, ITestIntf(L.Items[I]).GetValue);

  for I := 1 to L.Count - 1 do
    AssertEquals(True, ITestIntf(L.Items[I - 1]).GetValue <=
      ITestIntf(L.Items[I]).GetValue);

  L.Free;
end;

procedure TTestCaseForListsAndSets.TestLists;
var
  L: ITestList;
  I: integer;
  B: boolean;
  N: ITestIntf;
begin
  L := TTestList.Create;
  L.Add(TTestClass.New(0));
  L.Add(TTestClass.New(1));
  L.Add(TTestClass.New(1));
  L.Add(TTestClass.New(2));
  AssertEquals('items in list', 4, L.Count);
  AssertEquals('check values by it index', 0, L.Item[0].GetValue);
  AssertEquals('check values by it index', 1, L.Item[1].GetValue);
  AssertEquals('check values by it index', 1, L.Item[2].GetValue);
  AssertEquals('check values by it index', 2, L.Item[3].GetValue);

  N := TTestClass.New(3);
  B := L.Find(N, I);
  AssertEquals('seach of item not in list', False, B);

  B := L.Find(TTestClass.New(1), I);
  AssertEquals('search for item in list', True, B);
  AssertEquals(1, I);
  B := L.FindEx(I + 1, TTestClass.New(1), I);
  AssertEquals(True, B);
  AssertEquals(2, I);
  B := L.FindEx(I + 1, TTestClass.New(1), I);
  AssertEquals(False, B);

  L.Exchange(0, 1);
  AssertEquals('check exchange', 1, L.Item[0].GetValue);
  AssertEquals('check exchange', 0, L.Item[1].GetValue);
  B := L.Find(TTestClass.New(0), I);
  AssertEquals('search for item in list', True, B);
  AssertEquals('search for item in list', 1, I);
  L.Delete(1);
  AssertEquals('items in list', 3, L.Count);
  B := L.Find(TTestClass.New(0), I);
  AssertEquals(False, B);

  //TODO: Iterators
end;

procedure TTestCaseForListsAndSets.TestSets;
var
  S: ITestSet;
  ItemA, ItemB: ITestIntf;
  ItemA_, ItemA1, ItemB1: ITestIntf;
  B: boolean;

begin
  S := TTestSet.Create;
  ItemA := TTestClass.New(1);
  B := S.Add(ItemA);
  AssertEquals('add new item', True, B);
  ItemB := TTestClass.New(2);
  B := S.Add(ItemB);
  AssertEquals('add new item', True, B);
  AssertEquals(2, S.Count);



  ItemA1 := TTestClass.New(1);
  ItemB1 := TTestClass.New(2);
  AssertEquals('test comprasion function 1=1', 0, LeftTestComparison(ItemA, ItemA1));
  AssertEquals('test comprasion function 2=2', 0, LeftTestComparison(ItemB, ItemB1));
  AssertEquals('test comprasion function 1<>2', True,
    LeftTestComparison(ItemA, ItemB1) <> 0);
  AssertEquals('test comprasion function 1<>2', True,
    LeftTestComparison(ItemA1, ItemB) <> 0);
  AssertEquals('test comprasion function 1<>2', True,
    LeftTestComparison(ItemA1, ItemB1) <> 0);


  B := S.Add(ItemA1);
  AssertEquals('add existing item', False, B);
  B := S.Add(ItemB1);
  AssertEquals('add existing item', False, B);
  AssertEquals(2, S.Count);

  B := S.Add(TTestClass.New(1));
  AssertEquals('existing item ', False, B);
  B := S.Add(TTestClass.New(2));
  AssertEquals('existing item ', False, B);
  AssertEquals(2, S.Count);

  B := S.Add(TTestClass.New(3));
  AssertEquals('add new item', True, B);
  B := S.Add(TTestClass.New(4));
  AssertEquals(True, B);
  AssertEquals('add new item', 4, S.Count);

  AssertEquals('check item value', 1, ItemA.GetValue);
  AssertNotSame(Pointer(ItemA), Pointer(ItemA1));

  ItemA_ := S.Find(ItemA1);
  AssertSame('this mean that value of item A1 is equal ' +
    'to A2 by LeftTestComparison comparing', Pointer(ItemA), Pointer(ItemA_));
  ItemA_ := nil;
  ItemA_ := S.Remove(ItemA1);
  AssertEquals('remove item', 3, S.Count);
  AssertSame(Pointer(ItemA), Pointer(ItemA_));

  ItemA1 := S.Find(TTestClass.New(1));
  AssertNull(Pointer(ItemA1));

  S.Clear;
  AssertEquals(0, S.Count);
end;

procedure TTestCaseForListsAndSets.TestLeak;
begin
  AssertEquals(0, FTestCounter);
end;



initialization

  RegisterTest(TTestCaseForListsAndSets);
end.
