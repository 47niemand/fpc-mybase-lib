unit TestCaseForBaseMapConcurency;

{$mode delphi}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestCaseForBaseMapConcurency }

  TTestCaseForBaseMapConcurency = class(TTestCase)
  published
    procedure TestMapConcurency;
    procedure TestLeak;
  end;

implementation

uses uBaseValue, uBaseEntry, contnrs, variants, uBaseObserver,
  uBaseInterface, uBaseMap, uBaseTestUtils;

var
  FEventsObserverCounter: integer = 0;

type

  { TMapEventsObserver }

  TMapEventsObserver = class(TBaseObserver)
  public
    procedure Update(const Subject: IUnknown;
      const AOperation: TBaseObservedOperation; const Data: IUnknown); override;
  end;

  { TRunner }

  TRunner = class(TThread)
  protected
    FName: string;
    C: integer;
    FEntry: IBaseEntry;
    FCount: integer;
    FMap: IBaseMap;
    FActive: boolean;
    FObserver: IBaseObserver;
    FError: boolean;
    FErrorMsg: string;
    procedure Execute; override;
  public
    procedure Stop;
    constructor Create(const AName: string; AMap: IBaseMap);
    destructor Destroy; override;
  end;

{ TRunner }

procedure TRunner.Stop;
begin
  Writeln(format('[%x] try stop', [Handle]));
  FActive := False;
end;


procedure TRunner.Execute;
var
  V: variant;
  K: string;
  E: IBaseEntry;
begin
  try
    WriteLn(format('[%x] run...', [Handle]));
    while FActive do
    begin
      Inc(FCount);
      K := TestString(3, 3);
      C := Random(6);
      case C of
        0: FMap.Values[K] := TestData;
        1:
        begin
          v := FMap.Values[K];
          if not VarIsEmpty(v) then
            Assert(Validate(v), 'validate ' + VarToStrDef(v, '<unknown>'));
        end;
        2:
        begin
          E := FMap.Find(K);
          if Assigned(E) then
            FEntry := E;
        end;
        3:
          if Assigned(FEntry) then
            FEntry.SetValue(TestData);
        4:
        begin
          E := FMap.Remove(K);
          if Assigned(E) then
            FEntry := E;
        end;
        5:
          FEntry := nil;
      end;
    end;
  except
    on E: Exception do
    begin
      Writeln('Operation -> ', C, ' -> ', E.ClassName, '->', E.Message);
      FError := True;
      FErrorMsg := format('%s->%s at operation %d', [E.ClassName, E.message, C]);
    end;
  end;
  WriteLn(format('[%x] finish', [Handle]));
  FEntry := nil;
end;

constructor TRunner.Create(const AName: string; AMap: IBaseMap);
begin
  FName := AName;
  FMap := AMap;
  FActive := True;
  FreeOnTerminate := False;
  inherited Create(False);
  WriteLn(format('[%x] create', [Handle]));
end;

destructor TRunner.Destroy;
begin
  FActive := False;
  if Suspended then
    Resume;
  WaitFor;
  Assert(Finished);
  FMap := nil;
  FObserver := nil;
  if FError then
    Writeln(format('[%x] thread error  = %s', [Handle, FErrorMsg]));
  Writeln(format('[%x] thread destroy Count = %d', [Handle, FCount]));
  inherited Destroy;
end;

{ TMapEventsObserver }

procedure TMapEventsObserver.Update(const Subject: IUnknown;
  const AOperation: TBaseObservedOperation; const Data: IUnknown);
var
  M: IBaseMap;
  E: IBaseEntry;
begin
  InterLockedIncrement(FEventsObserverCounter);
  //M := CastBaseMap(STMapEventsObserverubject);
  //if Assigned(Data) then
  //begin
  //  E := CastBaseEntry(Data);
  //  writeln(format('[%x] ', [GetCurrentThreadId]), 'map op = ', Ord(AOperation),
  //    ' entry = ', e.Name,
  //    ' value = ', VarToStrDef(e.Value, '<unknown>'));
  //end
  //else
  //  writeln(format('[%x] ', [GetCurrentThreadId]), 'map op = ',
  //    Ord(AOperation), ' <nil>');
end;



{ TTestCaseForBaseMapConcurency }

procedure TTestCaseForBaseMapConcurency.TestMapConcurency;
var
  M: IBaseMap;
  R: Pointer;
  O: IBaseObserver;
  C: integer;
  L: TObjectList;
  I: integer;
begin
  FEventsObserverCounter := 0;
  L := TObjectList.Create(True);
  M := NewMap;
  O := TMapEventsObserver.Create;
  CastBaseSubject(M).Attach(O);
  for I := 0 to 7 do
    L.Add(TRunner.Create(IntToStr(I), M));
  sleep(60000);
  for R in L do
    (TObject(R) as TRunner).Stop;
  sleep(1000);

  C := 0;
  for R in L do
  begin
    AssertEquals(format('[%x] still active', [(TObject(R) as TRunner).Handle]),
      True, (TObject(R) as TRunner).Finished);
    C := C + (TObject(R) as TRunner).FCount;
  end;

  Writeln('TOTAL Cycles: ', c);
  Writeln('TOTAL Updates: ', FEventsObserverCounter);

  L.Free;
end;

procedure TTestCaseForBaseMapConcurency.TestLeak;
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

  RegisterTest(TTestCaseForBaseMapConcurency);
end.
