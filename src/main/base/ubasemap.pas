{
Copyright (C) 2016 Dmitry Muza <dmitry.muza@gmail.com>

This source is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2 of the License.

This code is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

A copy of the GNU General Public License is available on the World Wide Web
at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA.
}

unit uBaseMap;

{$mode delphi}
{$i BaseOptions.inc}

interface

uses
  Classes, SysUtils, uBaseInterface, uBaseObserver, uBaseEntry;

type
  IEnumerableBaseEntry = IEnumerable<IBaseEntry>;

function NewMap: IBaseMap;
function NewBaseIntfMap: IBaseMap; //TODO: create tests

function GetBaseMapCounter: integer;
function GetMapObserverCounter: integer;
function GetBaseMapEnumeratorCounter: integer;




implementation

uses Variants, uBaseListHelper, uBaseValue, uBaseConsts;

var
  BaseMapCounter: integer = 0;
  MapObserverCounter: integer = 0;
  BaseMapEnumeratorCounter: integer = 0;


type

  { TBaseIntfMap }

  TBaseIntfMap = class(TInterfacedObject, IBaseMap)
  private
    FList: TInterfaceList;
    FNames: TStrings;
    function InternalIndex(const Name: string): integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Append(const Map: IBaseMap); //TODO: do
    procedure Clear;
    function Exists(const Name: string): boolean;
    function Find(const Name: string): IBaseEntry;
    function GetEnumerator: IBaseEntryEnumerator; //TODO: do
    function GetValue(const Name: string): variant;
    function Remove(const Name: string): IBaseEntry;
    procedure SetValue(const Name: string; const Value: variant);
    function Size: integer;
  end;

  { TBaseMap }

  TMapObserver = class;

  TBaseMap = class(TInterfacedObject, IBaseMap, IEnumerableBaseEntry,
    IBaseObjectReference, IBaseVersionCounter, IBaseSubject)
  private
    FMapObserver: IBaseObserver;
    FMapSubject: IBaseSubject;
    procedure InitSubject;
  protected
    FList: TThreadList;
    FVersion: integer;
    procedure InternalClear;
    function CheckExists(const Item: IBaseEntry): boolean;
    function GetMapObserver: IBaseObserver;
    procedure OnUpdate(const Item: IBaseEntry; const AOperation: TBaseObservedOperation;
      const NewValue: IBaseValue); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Exists(const AItem: string): boolean;
    function Remove(const AItem: string): IBaseEntry;
    function Find(const AItem: string): IBaseEntry;
    function GetEnumerator: IBaseEntryEnumerator;
    function GetObject: TObject;
    function GetValue(const AItem: string): variant;
    function Size: integer;
    function Version: integer;
    procedure Append(const AParams: IBaseMap);
    procedure Attach(const Observer: IUnknown);
    procedure Clear;
    procedure Detach(const Observer: IUnknown);
    procedure Notify(const Operation: TBaseObservedOperation; const Data: IUnknown);
    procedure SetValue(const AItem: string; const AValue: variant);
  published
    property Values[AItem: string]: variant read GetValue write SetValue; default;
  end;

  { TMapObserver }

  TMapObserver = class(TBaseObserver)
  private
    FOwner: TBaseMap;
  public
    constructor Create(const AOwner: TBaseMap);
    destructor Destroy; override;
    procedure Update(const Subject: IInterface; const AOperation: TBaseObservedOperation;
      const Data: IInterface); override;
  end;

  { TBaseMapEnumerator }

  TBaseMapEnumerator = class(TInterfacedObject, IBaseEntryEnumerator)
  private
    FPosition: integer;
    FList: TList;
    FMap: TBaseMap;
  public
    constructor Create(const AMap: TBaseMap);
    destructor Destroy; override;
    function GetCurrent: IBaseEntry;
    function MoveNext: boolean;
    procedure Reset;
    property Current: IBaseEntry read GetCurrent;
  end;

function NewMap: IBaseMap;
begin
  Result := TBaseMap.Create;
end;

function NewBaseIntfMap: IBaseMap;
begin
  Result := TBaseIntfMap.Create;
end;

function GetBaseMapCounter: integer;
begin
  Result := BaseMapCounter;
end;

function GetMapObserverCounter: integer;
begin
  Result := MapObserverCounter;
end;

function GetBaseMapEnumeratorCounter: integer;
begin
  Result := BaseMapEnumeratorCounter;
end;

{ TBaseIntfMap }

function TBaseIntfMap.InternalIndex(const Name: string): integer;
begin
  Result := FNames.IndexOf(Name);
  if Result >= 0 then
    Result := integer(FNames.Objects[Result]);
end;

constructor TBaseIntfMap.Create;
begin
  inherited Create;
  FList := TInterfaceList.Create;
  FNames := TStringList.Create;
end;

destructor TBaseIntfMap.Destroy;
begin
  FreeAndNil(FNames);
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TBaseIntfMap.Append(const Map: IBaseMap);
begin
  raise EBaseIntfException.Create(SNotImplemented);
end;

procedure TBaseIntfMap.Clear;
begin
  FList.Lock;
  try
    FList.Clear;
    FNames.Clear;
  finally
    FList.Unlock;
  end;
end;

function TBaseIntfMap.Exists(const Name: string): boolean;
begin
  FList.Lock;
  Result := InternalIndex(Name) >= 0;
  FList.Unlock;
end;

function TBaseIntfMap.Find(const Name: string): IBaseEntry;
var
  I: integer;
begin
  Result := nil;
  try
    FList.Lock;
    I := InternalIndex(Name);
    if I >= 0 then
      Result := NewImmutableEntry(Name, FList.Items[I]);
  finally
    FList.Unlock;
  end;
end;

function TBaseIntfMap.GetEnumerator: IBaseEntryEnumerator;
begin
  Result := nil;
  raise Exception.Create(SNotImplemented);
end;

function TBaseIntfMap.GetValue(const Name: string): variant;
var
  I: integer;
begin
  FList.Lock;
  try
    I := InternalIndex(Name);
    if I >= 0 then
      Result := FList.Items[I]
    else
      Result := Unassigned;
  finally
    FList.Unlock;
  end;
end;


function TBaseIntfMap.Remove(const Name: string): IBaseEntry;
var
  I, J: integer;
begin
  Result := nil;
  FList.Lock;
  try
    I := FNames.IndexOf(Name);
    if I >= 0 then
    begin
      J := integer(FNames.Objects[I]);
      Result := NewImmutableEntry(FNames.Strings[I], FList.Items[J]);
      FNames.Delete(I);
      FList.Delete(J);
    end;
  finally
    FList.Unlock;
  end;
end;

procedure TBaseIntfMap.SetValue(const Name: string; const Value: variant);
var
  I, J: integer;
begin
  FList.Lock;
  try
    I := FNames.IndexOf(Name);
    if I >= 0 then
    begin
      J := integer(FNames.Objects[I]);
      FList.Items[J] := CastVarToIntf(Value);
    end
    else
    begin
      J := FList.Add(CastVarToIntf(Value));
      I := FNames.AddObject(Name, TObject(J));
    end;
  finally
    FList.Unlock;
  end;
end;

function TBaseIntfMap.Size: integer;
begin
  Result := FList.Count;
end;

{ TParamsObserver }

procedure TMapObserver.Update(const Subject: IInterface;
  const AOperation: TBaseObservedOperation; const Data: IInterface);
var
  E: IBaseEntry;
  V: IBaseValue;
begin
{$IFDEF WITHLOG}
  Log(Self, FOwner, 'Update[%d](%p, %p)', [Ord(AOperation), pointer(Subject),
    pointer(Data)]);
{$ENDIF}
  if Assigned(FOwner) and Assigned(Subject) then
    if Subject.QueryInterface(IBaseEntry, E) = S_OK then
      if Assigned(Data) then
      begin
        V := CastBaseValue(Data);
        FOwner.OnUpdate(E, AOperation, V);
        V := nil;
      end
      else
        FOwner.OnUpdate(E, AOperation, nil);
end;

constructor TMapObserver.Create(const AOwner: TBaseMap);
begin
  inherited Create;
  InterLockedIncrement(MapObserverCounter);
  FOwner := AOwner;
{$IFDEF WITHLOG}
  Log(Self, FOwner, 'Create Observer', []);
{$ENDIF}
end;

destructor TMapObserver.Destroy;
begin
  inherited Destroy;
  InterLockedDecrement(MapObserverCounter);
{$IFDEF WITHLOG}
  Log(Self, FOwner, 'Destroy', []);
{$ENDIF}
end;

{ TBaseParamsEnumerator }

constructor TBaseMapEnumerator.Create(const AMap: TBaseMap);
var
  L: TList;
begin
  inherited Create;
  InterLockedIncrement(BaseMapEnumeratorCounter);
  AMap._AddRef;
  FMap := AMap;
  FList := TList.Create;
  L := AMap.FList.LockList;
  try
    FList.AddList(L);
  finally
    AMap.FList.UnlockList;
  end;
  FPosition := -1;
{$IFDEF WITHLOG}
  Log(Self, AMap, 'Create Enumerator for %d items', [FList.Count]);
{$ENDIF}
end;

destructor TBaseMapEnumerator.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
  FMap._Release;
  InterLockedDecrement(BaseMapEnumeratorCounter);
{$IFDEF WITHLOG}
  Log(Self, nil, 'Destory Enumerator', []);
{$ENDIF}
end;

function TBaseMapEnumerator.GetCurrent: IBaseEntry;
var
  L: TList;
begin
  Result := IBaseEntry(FList.List^[FPosition]);
  L := FMap.FList.LockList;
  try
    if L.IndexOf(pointer(Result)) < 0 then
      raise EBaseConcurrencyError.Create(SConcurrencyError);
  finally
    FMap.FList.UnlockList;
  end;
end;

function TBaseMapEnumerator.MoveNext: boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

procedure TBaseMapEnumerator.Reset;
begin
  FPosition := -1;
end;

{ Binnary Search Helpers }

type
  TListHelperBaseEntry = TListHelper<IBaseEntry>;

function LeftBaseEntryComparison(const Left: IBaseEntry; var Value): integer;
var
  Right: IBaseEntry absolute Value;
  S: string;
begin
  Assert(Assigned(@Value));
  S := Right.Name;
  Result := CompareStr(Left.Name, S);
end;

function LeftBaseEntryKeyComparison(const Left: IBaseEntry; var Value): integer;
var
  Right: string absolute Value;
begin
  Assert(Assigned(@Value));
  Result := CompareStr(Left.Name, Right);
end;

{ TBaseMap }

procedure TBaseMap.OnUpdate(const Item: IBaseEntry;
  const AOperation: TBaseObservedOperation; const NewValue: IBaseValue);
begin
{$IFDEF WITHLOG}
  Log(Self, nil, 'OnUpdate[%d](%p, %p)', [Ord(AOperation), pointer(Item),
    pointer(NewValue)]);
{$ENDIF}
  //  if AOperation in [booChange, booDeleteItem, booFree] then
  //    Assert(CheckExists(Item), 'item not exists on map');
  if AOperation in [booChange, booAddItem] then
    InterLockedIncrement(FVersion);
  if Assigned(FMapSubject) then
    if Assigned(NewValue) then
      FMapSubject.Notify(AOperation, NewImmutableEntry(Item.Name, NewValue.GetValue))
    else
      FMapSubject.Notify(AOperation, NewImmutableEntry(Item.Name, Item.Value));
end;

procedure TBaseMap.InternalClear;
var
  L: TList;
  Idx: integer;
begin
  L := FList.LockList;
  try
    for Idx := 0 to L.Count - 1 do
    begin
      CastBaseSubject(IBaseEntry(L.List^[Idx])).Detach(GetMapObserver);
      IBaseEntry(L.List^[Idx]) := nil;
    end;
    L.Clear;
  finally
    FList.UnlockList;
  end;
end;

function TBaseMap.CheckExists(const Item: IBaseEntry): boolean;
var
  L: TList;
begin
  L := FList.LockList;
  try
    Result := L.IndexOf(pointer(Item)) >= 0;
  finally
    FList.UnlockList;
  end;
end;

function TBaseMap.GetMapObserver: IBaseObserver;
var
  O: IBaseObserver;
begin
  if not Assigned(FMapObserver) then
  begin
    O := TMapObserver.Create(Self);
    O._AddRef;
    if InterlockedCompareExchange(pointer(FMapObserver), pointer(O), nil) <> nil then
    begin
      O._Release;
      O := nil;
      Assert(Assigned(FMapObserver));
    end
    else
      Assert(pointer(FMapObserver) = pointer(O));
  end;
  Result := FMapObserver;
end;

procedure TBaseMap.InitSubject;
var
  L: IBaseSubject;
begin
  L := TBaseSubject.Create(Self);
  L._AddRef;
  if InterlockedCompareExchange(pointer(FMapSubject), pointer(L), nil) <> nil then
  begin
    Assert(Assigned(FMapSubject));
    L._Release;
    L := nil;
  end
  else
    Assert(pointer(L) = pointer(FMapSubject));
end;

procedure TBaseMap.Attach(const Observer: IUnknown);
begin
  if not Assigned(FMapSubject) then
    InitSubject;
  Assert(Assigned(FMapSubject));
  FMapSubject.Attach(Observer);
end;

procedure TBaseMap.Detach(const Observer: IUnknown);
begin
  if Assigned(FMapSubject) then
    FMapSubject.Detach(Observer);
end;

procedure TBaseMap.Notify(const Operation: TBaseObservedOperation; const Data: IUnknown);
begin
  if Assigned(FMapSubject) then
    FMapSubject.Notify(Operation, Data);
end;

function TBaseMap.Version: integer;
begin
  Result := FVersion;
end;

function TBaseMap.Exists(const AItem: string): boolean;
var
  L: TList;
  Idx: integer;
begin
  L := FList.LockList;
  try
    Result := TListHelperBaseEntry.BinarySearch(L, (@AItem)^, Idx,
      LeftBaseEntryKeyComparison);
  finally
    FList.UnlockList;
  end;
end;

function TBaseMap.Remove(const AItem: string): IBaseEntry;
var
  L: TList;
  Idx: integer;
begin
  L := FList.LockList;
  try
    if TListHelperBaseEntry.BinarySearch(L, (@AItem)^, Idx,
      LeftBaseEntryKeyComparison) then
    begin
      Result := IBaseEntry(L.List^[Idx]);
      CastBaseSubject(Result).Detach(GetMapObserver);
      IBaseEntry(L.List^[Idx]) := nil;
      L.Delete(Idx);
    end
    else
      Result := nil
  finally
    FList.UnlockList;
  end;
  Notify(booDeleteItem, Result);
{$IFDEF WITHLOG}
  Log(Self, nil, 'Remove[%s] -> %p', [AItem, pointer(Result)]);
{$ENDIF}
end;

function TBaseMap.GetObject: TObject;
begin
  Result := Self;
end;

function TBaseMap.GetEnumerator: IBaseEntryEnumerator;
begin
  Result := TBaseMapEnumerator.Create(Self);
end;

function TBaseMap.GetValue(const AItem: string): variant;
var
  L: TList;
  Idx: integer;
begin
  L := FList.LockList;
  try
    if TListHelperBaseEntry.BinarySearch(L, (@AItem)^, Idx,
      LeftBaseEntryKeyComparison) then
      Result := IBaseEntry(L.List^[Idx]).Value
    else
      Result := Unassigned;
  finally
    FList.UnlockList;
  end;
{$IFDEF WITHLOG}
  Log(Self, nil, 'GET[%s] -> "%s"',
    [AItem, Copy(VarToStrDef(Result, '<undef>'), 1, 20)]);
{$ENDIF}
end;

procedure TBaseMap.SetValue(const AItem: string; const AValue: variant);
var
  L: TList;
  Idx: integer;
  E: IBaseEntry;
  IsNew: boolean;
  S: IBaseSubject;
begin
{$IFDEF WITHLOG}
  Log(Self, nil, 'SET[%s] <- "%s"',
    [AItem, Copy(VarToStrDef(AValue, '<undef>'), 1, 20)]);
{$ENDIF}
  IsNew := False;
  S := nil;
  L := FList.LockList;
  try
    IsNew := not TListHelperBaseEntry.BinarySearch(L, (@AItem)^,
      Idx, LeftBaseEntryKeyComparison);
    if IsNew then
    begin
      E := NewEntry(AItem, AValue);
      L.Insert(Idx, nil);
      IBaseEntry(L.List^[Idx]) := E;
      S := CastBaseSubject(E);
      S.Attach(GetMapObserver);
    end
    else
      E := IBaseEntry(L.List^[Idx]);
  finally
    FList.UnlockList;
  end;
  if IsNew then
  begin
    Assert(Assigned(S));
    Assert(Assigned(E));
    try
      S.Notify(booAddItem, NewImmutableValue(AValue));
    except
      S.Detach(GetMapObserver);
      L := FList.LockList;
      try
        Idx := L.IndexOf(pointer(E));
        if Idx > 0 then
        begin
          IBaseEntry(L.List^[Idx]) := nil;
          L.Delete(Idx);
        end;
      finally
        FList.UnlockList;
      end;
      raise;
    end;
  end
  else
    E.SetValue(AValue);
end;

function TBaseMap.Size: integer;
begin
  Result := FList.LockList.Count;
  FList.UnlockList;
end;

procedure TBaseMap.Append(const AParams: IBaseMap);
var
  I: IBaseObjectReference;
  N: IBaseEntry;
  P: TBaseMap;
begin
  I := CastBaseObjectReference(AParams);
  P := I.GetObject as TBaseMap;
  for N in P do
    Values[N.Name] := N.Value;
end;

procedure TBaseMap.Clear;
var
  L, T: TList;
  Idx: integer;
begin
  T := TList.Create;
  try
    L := FList.LockList;
    try
      T.AddList(L);
    finally
      FList.UnlockList;
    end;
    for Idx := 0 to T.Count - 1 do
    begin
      Notify(booFree, IBaseEntry(T.List^[Idx]));
      FList.Remove(T.List^[Idx]); //remove pointer from FList
      CastBaseSubject(IBaseEntry(T.List^[Idx])).Detach(GetMapObserver);
      IBaseEntry(T.List^[Idx]) := nil;
    end;
    Assert(Size = 0);
  finally
    T.Free;
  end;
end;

constructor TBaseMap.Create;
begin
  inherited Create;
  InterLockedIncrement(BaseMapCounter);
  FList := TThreadList.Create;
{$IFDEF WITHLOG}
  Log(Self, nil, 'Create', []);
{$ENDIF}
end;

function TBaseMap.Find(const AItem: string): IBaseEntry;
var
  L: TList;
  Idx: integer;
begin
  L := FList.LockList;
  try
    if TListHelperBaseEntry.BinarySearch(L, (@AItem)^, Idx,
      LeftBaseEntryKeyComparison) then
      Result := IBaseEntry(L.List^[Idx])
    else
      Result := nil;
  finally
    FList.UnlockList;
  end;
{$IFDEF WITHLOG}
  Log(Self, nil, 'Find[%s] -> %p', [AItem, pointer(Result)]);
{$ENDIF}
end;

destructor TBaseMap.Destroy;
begin
  InternalClear;
  FreeAndNil(FList);
  if Assigned(FMapObserver) then
  begin
    Assert(FMapObserver._AddRef = 2);
    Assert(FMapObserver._Release = 1);
  end;
  FMapObserver := nil;
  inherited Destroy;
  InterLockedDecrement(BaseMapCounter);
{$IFDEF WITHLOG}
  Log(Self, nil, 'Destroy', []);
{$ENDIF}
end;

end.
