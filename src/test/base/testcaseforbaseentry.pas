unit TestCaseForBaseEntry;

{$mode delphi}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestCaseForBaseEntry }

  TTestCaseForBaseEntry = class(TTestCase)
  private
  published
    procedure TestBaseEntry;
    procedure TestBaseEntryVerions;
    procedure TestBaseEntryEvents;
    procedure TestBaseEntryLeaks;
  end;

implementation

uses variants, uBaseTestUtils, uBaseInterface, uBaseEntry, uBaseValue, uBaseObserver;

procedure TTestCaseForBaseEntry.TestBaseEntry;
var
  E: IBaseEntry;
  V: variant;
begin
  E := NewEntry('test');
  AssertEquals('test', e.Name);
  E.Value := 1;
  AssertEquals(1, e.Value);
  E.Value := 2;
  V := E.Value;
  AssertEquals(2, V);
end;

procedure TTestCaseForBaseEntry.TestBaseEntryVerions;
var
  E: IBaseEntry;
  V: IBaseVersionCounter;
  Res: longint;
begin
  E := NewEntry('test');
  Res := E.QueryInterface(IBaseVersionCounter, V);
  AssertEquals('should supprot IVersionCounter', S_OK, Res);
  AssertEquals('check version', 0, V.Version);
  AssertEquals('name', 'test', E.Name);
  AssertEquals('check value', True, VarCompare(Unassigned, E.Value) = vrEqual);
  E.Value := 1;
  AssertEquals('check version', 1, V.Version);
  AssertEquals('check value', 1, E.Value);
  E.Value := 1;
  AssertEquals('check version', 1, V.Version);
  AssertEquals('check value', 1, E.Value);
end;

var
  FTestObserverCounter: integer = 0;

type

  { TTestBaseEventObserver }

  TTestBaseEventObserver = class(TBaseObserver)
    procedure Update(const Subject: IUnknown;
      const AOperation: TBaseObservedOperation; const Data: IUnknown); override;
  end;

{ TTestBaseEventObserver }

procedure TTestBaseEventObserver.Update(const Subject: IUnknown;
  const AOperation: TBaseObservedOperation; const Data: IUnknown);
begin
  Inc(FTestObserverCounter);
  Writeln(format(' RECIVED UPDATE %s [%d] %s',
    [GetSafeIntfName(Subject), Ord(AOperation), GetSafeIntfName(Data)]));
end;


procedure TTestCaseForBaseEntry.TestBaseEntryEvents;
var
  E: IBaseEntry;
  O: IBaseObserver;
begin
  O := TTestBaseEventObserver.Create;
  E := NewEntry('test');
  FTestObserverCounter := 0;
  CastBaseSubject(E).Attach(O);
  E.Value := 'new value';
  E.Value := 'new value 2';
  AssertEquals('Events', 2, FTestObserverCounter);
  writeln('WeaklyInterfacedObjectCounter ', GetWeaklyInterfacedObjectCounter);
  writeln('WeakRefCounter ', GetWeakRefCounter);
  writeln('enties ', GetBaseEntryCounter);
  writeln('observers ', GetBaseObserverCounter);
  writeln('subjects ', GetBaseSubjectCounter);
  writeln('values ', GetBaseValueCounter);
  E.Value := 'new value 3';

end;

procedure TTestCaseForBaseEntry.TestBaseEntryLeaks;
begin
  writeln('WeaklyInterfacedObjectCounter ', GetWeaklyInterfacedObjectCounter);
  writeln('WeakRefCounter ', GetWeakRefCounter);
  AssertEquals('check enties', 0, GetBaseEntryCounter);
  AssertEquals('check observers', 0, GetBaseObserverCounter);
  AssertEquals('check values ', 0, GetBaseValueCounter);
  AssertEquals('check subjects', 0, GetBaseSubjectCounter);

end;



initialization

  RegisterTest(TTestCaseForBaseEntry);
end.


