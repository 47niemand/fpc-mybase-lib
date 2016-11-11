unit TestCaseBaseInterface;

{$mode delphi}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestCaseBaseInterface }

  TTestCaseBaseInterface = class(TTestCase)
  published
    procedure TestBaseInterface;
    procedure TestBaseWeakRefs;
    procedure TestRefsLeaks;
  end;

implementation

uses uBaseInterface;

procedure TTestCaseBaseInterface.TestBaseInterface;
var
  BaseVersionCounter: IBaseVersionCounter;
  WeakRef: IWeakRef;
  Weakly: IWeakly;
  BaseClonable: IBaseClonable;
  BaseObserver: IBaseObserver;
  BaseSubject: IBaseSubject;
  BaseObjectReference: IBaseObjectReference;
  BaseValue: IBaseValue;
  BaseEntry: IBaseEntry;
  BaseValueEnumerator: IBaseValueEnumerator;
  BaseEntryEnumerator: IBaseEntryEnumerator;
  //EnumerableBaseValue: IEnumerableBaseValue;
  //EnumerableBaseEntry: IEnumerableBaseEntry;
  BaseMap: IBaseMap;
begin
  BaseVersionCounter := nil;
  WeakRef := nil;
  Weakly := nil;
  BaseClonable := nil;
  BaseObserver := nil;
  BaseSubject := nil;
  BaseObjectReference := nil;
  BaseValue := nil;
  BaseEntry := nil;
  BaseValueEnumerator := nil;
  BaseEntryEnumerator := nil;
  //EnumerableBaseValue := nil;
  //EnumerableBaseEntry := nil;
  BaseMap := nil;
  //just check
  AssertSame(nil, BaseVersionCounter);
  AssertSame(nil, WeakRef);
  AssertSame(nil, Weakly);
  AssertSame(nil, BaseClonable);
  AssertSame(nil, BaseObserver);
  AssertSame(nil, BaseSubject);
  AssertSame(nil, BaseObjectReference);
  AssertSame(nil, BaseValue);
  AssertSame(nil, BaseEntry);
  AssertSame(nil, BaseValueEnumerator);
  AssertSame(nil, BaseEntryEnumerator);
  //AssertSame(nil, EnumerableBaseValue);
  //AssertSame(nil, EnumerableBaseEntry);
  AssertSame(nil, BaseMap);
  AssertEquals('should not be any weakRef', 0, GetWeakRefCounter);
  AssertEquals('should not be any WeaklyObjects', 0, GetWeaklyInterfacedObjectCounter);
end;


var
  WeakObjectDestoryed: integer = 0;

type

  { TWeakObject }

  TWeakObject = class(TWeaklyInterfacedObject)
    destructor Destroy; override;
  end;

{ TWeakObject }

destructor TWeakObject.Destroy;
begin
  inherited Destroy;
  Inc(WeakObjectDestoryed);
end;

procedure TTestCaseBaseInterface.TestBaseWeakRefs;
var
  I: IUnknown;
  T: TObject;

  procedure WeakTest;
  var
    W: IWeakRef;
    J: IUnknown;
  begin
    W := SafeGetWeakRef(I);
    AssertEquals('should be one weakrefs', 1, GetWeakRefCounter);
    // retriwing of weakref shound not cange RefCount of point interfae
    AssertEquals('should has two refs', 1, TInterfacedObject(T).RefCount);
    AssertEquals('has weak interface', True, Assigned(W));
    AssertEquals('weak object is alive', True, W.IsAlive);
    J := W.Get;
    AssertEquals('should has three refs', 2, TInterfacedObject(T).RefCount);
    AssertSame('has weak object is point to I', Pointer(I), Pointer(J));
    J := nil; // decrase refs
    AssertEquals('should has two refs', 1, TInterfacedObject(T).RefCount);
    AssertEquals('should be one weakRef', 1, GetWeakRefCounter); // it's W
    AssertEquals('T shoud not be destroyed', 0, WeakObjectDestoryed);
    I := nil; //T should be destroyed
    AssertEquals('T shoud be destroyed', 1, WeakObjectDestoryed);
    // with access to T we should catch access violation
    AssertEquals('weak object is dead', False, W.IsAlive);
    J := W.Get;
    AssertSame('weak object is dead', nil, Pointer(J));
    J := nil;
    AssertEquals('should be one weakRef', 1, GetWeakRefCounter);
    W := nil;
    T := nil;
  end;

var
  E: IWeakly;
  P: IWeakRef;
  Res: longint;
  A, B, C: Pointer;
begin
  AssertEquals('should not be any weakRef', 0, GetWeakRefCounter);
  T := TWeakObject.Create;
  WeakObjectDestoryed := 0;
  AssertEquals('instanse of TInterfacedObject', True, T is TInterfacedObject);
  AssertEquals('newly created object', 0, TInterfacedObject(T).RefCount);
  I := TInterfacedObject(T);
  AssertEquals('should has one refs', 1, TInterfacedObject(T).RefCount);
  Res := I.QueryInterface(IWeakly, E);
  AssertEquals('should has two refs', 2, TInterfacedObject(T).RefCount);
  AssertEquals('should has IWeakly interface', True, Res = S_OK);

  AssertEquals('should be any weakrefs', 0, GetWeakRefCounter);
  P := E.WeakRef; // get ref
  B := Pointer(P); // we need only pointer to check
  P := nil; // and clean up
  AssertEquals('should has two refs', 2, TInterfacedObject(T).RefCount);
  AssertEquals('should be one weakrefs', 1, GetWeakRefCounter);
  P := E.WeakRef;
  C := Pointer(P);
  P := nil;
  AssertEquals('should has two refs', 2, TInterfacedObject(T).RefCount);
  AssertEquals('should be one weakrefs', 1, GetWeakRefCounter);
  P := SafeGetWeakRef(I); // another way to get weak refs if interface supports it
  A := Pointer(P);
  P := nil;
  AssertEquals('should has two refs', 2, TInterfacedObject(T).RefCount);
  AssertEquals('should be one weakrefs', 1, GetWeakRefCounter);

  //cleanup
  A := nil;
  B := nil;
  C := nil;

  AssertSame('check weak link interface A=B', A, B);
  AssertSame('check weak link interface C=B', C, B);

  E := nil; // we don't need this (RefCount - 1)

  AssertEquals('should has one refs', 1, TInterfacedObject(T).RefCount);
  WeakTest;

  //i don't know why but w will be destoryed after leave method
  //next assertion is not working
  //AssertEquals('should not be any weakRef', 0, GetWeakRefCounter);
end;

procedure TTestCaseBaseInterface.TestRefsLeaks;
begin
  AssertEquals('should not be any weakRef', 0, GetWeakRefCounter);
  AssertEquals('should not be any WeaklyObjects', 0, GetWeaklyInterfacedObjectCounter);
end;

initialization

  RegisterTest(TTestCaseBaseInterface);
end.











