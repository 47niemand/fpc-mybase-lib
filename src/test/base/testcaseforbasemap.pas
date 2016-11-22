unit TestCaseForBaseMap;

{$mode delphi}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestCaseForBaseMap }

  TTestCaseForBaseMap = class(TTestCase)
  published
    procedure TestBaseMap;
    procedure TestBaseEntry2;
    procedure TestBaseMapEvents;
    procedure TestBaseMapWeakEvents;
    procedure TestBaseMapLeaks;
  end;

implementation

uses variants, uBaseObserver, uBaseTestUtils, uBaseInterface, uBaseEntry,
  uBaseValue, uBaseMap;

procedure TTestCaseForBaseMap.TestBaseMap;
var
  M: IBaseMap;
  E: IBaseEntry;
  v: variant;
begin
  M := NewMap;
  // set/get
  AssertEquals(0, M.Size);
  M['test'] := 1;
  AssertEquals(1, M.Size);
  v := M['test'];
  AssertEquals(True, VarCompare(1, v) = vrEqual);
  M['test'] := 2;
  v := M['test'];
  AssertEquals(True, VarCompare(2, v) = vrEqual);
  AssertEquals(1, M.Size);
  // find
  E := M.Find('test');
  AssertNotNull(E);
  AssertEquals('test', E.Name);
  v := E.Value;
  AssertEquals(True, VarCompare(2, v) = vrEqual);
  E.Value := '3';
  v := M['test'];
  AssertEquals(True, VarCompare('3', v) = vrEqual);
  v := E.Value;
  AssertEquals(True, VarCompare('3', v) = vrEqual);
end;


procedure TTestCaseForBaseMap.TestBaseEntry2;
const
  CTestSize = 1000;
var
  M: IBaseMap;
  EE, E: IBaseEntry;
  X, Q: integer;
  S: TStrings;
  T: string;
begin
  M := NewMap;
  Writeln('Create Map');
  while M.Size < CTestSize do
    M.Values[TestString(10, 10)] :=
      TestData//    Writeln('**** check values ', GetBaseValueCounter);
  //    Writeln('**** check enties ', GetBaseEntryCounter);
  ;
  Writeln('**** check values ', GetBaseValueCounter);
  Writeln('**** check enties ', GetBaseEntryCounter);

  AssertEquals(CTestSize, M.Size);

  Writeln('Clear Map');
  M.Clear;

  Writeln('**** check values ', GetBaseValueCounter);
  Writeln('**** check enties ', GetBaseEntryCounter);

  Writeln('Recreate Map');
  while M.Size < CTestSize do
    M.Values[TestString(10, 10)] :=
      TestData//    Writeln('**** check values ', GetBaseValueCounter);
  //    Writeln('**** check enties ', GetBaseEntryCounter);
  ;

  Writeln('**** check values ', GetBaseValueCounter);
  Writeln('**** check enties ', GetBaseEntryCounter);

  AssertEquals(CTestSize, M.Size);

  Writeln('Validate map data');
  S := TStringList.Create;
  try
    x := 0;
    q := 0;
    Writeln('Check iterator');
    for E in M do
    begin
      AssertEquals(True, Validate(E.Value));
      AssertEquals(True, Validate(E.Name));
      S.Add(E.Name);
      Writeln(E.Name);
      E.Value := null;
      Inc(X);
      Q := 0;
      for EE in M do
        if not Validate(EE.Value) then
          Inc(Q);
      AssertEquals(X, Q);
    end;
    Writeln('Check iterator and removed items');
    for T in S do
    begin
      E := M.Remove(T);
      AssertEquals(T, E.Name);
      //    Writeln('**** check values ', GetBaseValueCounter);
      //    Writeln('**** check enties ', GetBaseEntryCounter);
      E := nil;
    end;
    Writeln('**** check values ', GetBaseValueCounter);
    Writeln('**** check enties ', GetBaseEntryCounter);

    AssertEquals(0, M.Size);

  finally
    S.Free;
  end;
end;


var
  TestObserverCounter: integer = 0;


type

  { TTestObserver }

  TTestObserver = class(TBaseObserver)
  public
    procedure Update(const Subject: IUnknown; const AOperation: TBaseObservedOperation;
      const Data: IUnknown); override;
  end;

{ TTestObserver }

var
  FTestCounter: integer = 0;

procedure TTestObserver.Update(const Subject: IUnknown;
  const AOperation: TBaseObservedOperation; const Data: IUnknown);
begin
  Inc(FTestCounter);
  Writeln(format(' RECIVED UPDATE %s [%d] %s',
    [GetSafeIntfName(Subject), Ord(AOperation), GetSafeIntfName(Data)]));
end;

var
  EE: IBaseEntry = nil;

procedure TTestCaseForBaseMap.TestBaseMapEvents;
var
  O: IBaseObserver;
  M: IBaseMap;
  E: IBaseEntry;
begin
  FTestCounter := 0;
  O := TTestObserver.Create;
  M := NewMap;
  CastBaseSubject(M).Attach(O);
  M['test'] := pi; // changing
  AssertEquals(1, FTestCounter);
  M['test'] := pi; // not changeing
  AssertEquals(1, FTestCounter);
  M['date'] := now;
  AssertEquals(2, FTestCounter);
  E := M.Find('test');
  E.Value := 'test'; // e is part of M
  AssertEquals(3, FTestCounter);
  EE := nil;
  EE := M.Remove('test'); // extract entry and store it globaly,
  AssertSame(pointer(E), pointer(EE));
  AssertEquals(4, FTestCounter); // remove operation generated boodelete event
  M.Clear; // clear generate free event for all items in map
  AssertEquals(5, FTestCounter);
  EE.Value := 'passport';
  AssertEquals('passport', EE.Value);
  AssertEquals(5, FTestCounter); // events not occured
  E := nil;
  O := nil;
  M := nil;
end;

procedure TTestCaseForBaseMap.TestBaseMapWeakEvents;
begin
  writeln('WeaklyInterfacedObjectCounter ', GetWeaklyInterfacedObjectCounter);
  writeln('WeakRefCounter ', GetWeakRefCounter);
  AssertEquals(0, GetBaseMapCounter);
  AssertEquals(True, Assigned(EE));
  AssertEquals('passport', EE.Value);
  EE.Value := 'ok';
  AssertEquals('ok', EE.Value);
  EE := nil;
end;

procedure TTestCaseForBaseMap.TestBaseMapLeaks;
begin
  AssertEquals('check enumerators', 0, GetBaseMapEnumeratorCounter);
  AssertEquals('check maps', 0, GetBaseMapCounter);
  AssertEquals('check values', 0, GetBaseValueCounter);
  AssertEquals('check enties', 0, GetBaseEntryCounter);
  AssertEquals('check observers', 0, GetBaseObserverCounter);
  AssertEquals('check subjects', 0, GetBaseSubjectCounter);
  writeln('WeaklyInterfacedObjectCounter ', GetWeaklyInterfacedObjectCounter);
  writeln('WeakRefCounter ', GetWeakRefCounter);
end;

initialization

  RegisterTest(TTestCaseForBaseMap);
end.
