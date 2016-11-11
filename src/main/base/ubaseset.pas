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

unit uBaseSet;

{$mode delphi}
{$I BaseOptions.inc}

interface

uses
  Classes, SysUtils, uBaseListHelper;

type

  { IBaseSet Interface to collection that contains no duplicate element.
    Interfac provide methods for add, remove and search items in set.
    }
  IBaseSet<T> = interface
    function GetCount: integer;
    function Add(const Item: T): boolean;
    function Remove(const Item: T): T;
    function Exists(const Item: T): boolean;
    function Find(const Item: T): T;
    property Count: integer read GetCount;
    function GetEnumerator: IEnumerator<T>;
    procedure Clear;
  end;

  { TBaseSet abstract generic class with basic implementation
    of IBaseSet interface.
    Method GetComparisonFunction should be overriden. Retrurned function
    will used for comparisons while adding new items.
    The behavior of a set is not specified if the value of an objectis changed.
    }
  TBaseSet<T> = class(TInterfacedObject, IBaseSet<T>, IEnumerable<T>)
  private
    FLeftComparison: TLeftComparison<T>;
  protected
    FList: TThreadList;
    function InternalRemove(const L: TList; const Idx: integer): T;
    procedure InternalAdd(const L: TList; Idx: integer; const AItem: T);
    function InternalSearch(const Instance: TList; const Value: T;
      out FoundIndex: integer): boolean;
    procedure OnUpdateInternal(const {%H-}L: TList; const {%H-}Item: T;
      const {%H-}AOperation: TBaseListOperation); virtual;
    function GetComparisonFunction: TLeftComparison<T>; virtual;
      abstract; {Warning successors sbould implement this}
  public
    function GetCount: integer;
    function Add(const Item: T): boolean;
    function Find(const Item: T): T;
    function Remove(const Item: T): T;
    function Exists(const Item: T): boolean;
    property Count: integer read GetCount;
    function GetEnumerator: IEnumerator<T>; virtual;
    function GetObject: TObject;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;

  { TBaseSetEnumerator }

  TBaseSetEnumerator<T> = class(TInterfacedObject, IEnumerator<T>)
  protected
    FPosition: integer;
    FList: TList;
  public
    constructor Create(const ASet: TBaseSet<T>);
    function GetCurrent: T; virtual;
    function MoveNext: boolean;
    property Current: T read GetCurrent;
    procedure Reset;
    destructor Destroy; override;
  end;


implementation

uses Variants, uBaseConsts;

{ TBaseSet<T> }

function TBaseSet<T>.GetCount: integer;
begin
  Result := FList.LockList.Count;
  FList.UnlockList;
end;

function TBaseSet<T>.Find(const Item: T): T;
var
  Idx: integer;
  L: TList;
begin
  L := FList.LockList;
  try
    if InternalSearch(L, Item, Idx) then
    begin
      Result := T(L.Items[Idx]);
      OnUpdateInternal(L, Result, bloCustom);
    end
    else
      Result := nil;
  finally
    FList.UnlockList;
  end;
end;

procedure TBaseSet<T>.InternalAdd(const L: TList; Idx: integer; const AItem: T);
begin
  OnUpdateInternal(L, AItem, bloAddItem);
  L.Insert(Idx, nil);
  T(L.List^[Idx]) := AItem;
  Assert(L.Items[Idx] = Pointer(AItem));
end;

function TBaseSet<T>.Add(const Item: T): boolean;
var
  Idx: integer;
  L: TList;
begin
  Result := False;
  L := FList.LockList;
  try
    if not InternalSearch(L, Item, Idx) then
    begin
      InternalAdd(L, Idx, Item);
      Result := True;
    end;
  finally
    FList.UnlockList;
  end;
end;

function TBaseSet<T>.InternalSearch(const Instance: TList; const Value: T;
  out FoundIndex: integer): boolean;
var
  E: T;
begin
  E := Value;
  Result := TListHelper<T>.BinarySearch(Instance, E, FoundIndex, FLeftComparison);
end;

function TBaseSet<T>.InternalRemove(const L: TList; const Idx: integer): T;
begin
  Result := T(L.Items[Idx]);
  OnUpdateInternal(L, Result, bloDeleteItem);
  T(L.List^[Idx]) := nil;
  L.Delete(Idx);
end;

function TBaseSet<T>.Remove(const Item: T): T;
var
  Idx: integer;
  L: TList;
begin
  L := FList.LockList;
  try
    if InternalSearch(L, Item, Idx) then
      Result := InternalRemove(L, Idx)
    else
      Result := nil;
  finally
    FList.UnlockList;
  end;
end;

function TBaseSet<T>.Exists(const Item: T): boolean;
var
  Idx: integer;
  L: TList;
begin
  L := FList.LockList;
  try
    Result := InternalSearch(L, Item, Idx);
  finally
    FList.UnlockList;
  end;
end;

procedure TBaseSet<T>.Clear;
var
  L: TList;
  Idx: integer;
begin
  L := FList.LockList;
  try
    for Idx := 0 to L.Count - 1 do
    begin
      OnUpdateInternal(L, T(L.List^[Idx]), bloFree);
      T(L.List^[Idx]) := nil;
    end;
    FList.Clear;
  finally
    FList.UnlockList;
  end;
end;

procedure TBaseSet<T>.OnUpdateInternal(const {%H-}L: TList; const {%H-}Item: T;
  const {%H-}AOperation: TBaseListOperation);
begin
  // notihng in this scoup
end;

constructor TBaseSet<T>.Create;
begin
  inherited Create;
  FList := TThreadList.Create;
  FLeftComparison := GetComparisonFunction;
end;

destructor TBaseSet<T>.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TBaseSet<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TBaseSetEnumerator<T>.Create(Self);
end;

function TBaseSet<T>.GetObject: TObject;
begin
  Result := Self;
end;

{ TBaseSetEnumerator<T> }

constructor TBaseSetEnumerator<T>.Create(const ASet: TBaseSet<T>);
begin
  inherited Create;
  FList := ASet.FList.LockList;
  ASet.FList.UnlockList;
  FPosition := -1;
end;

function TBaseSetEnumerator<T>.GetCurrent: T;
begin
  Result := T(FList.Items[FPosition]);
end;

function TBaseSetEnumerator<T>.MoveNext: boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

procedure TBaseSetEnumerator<T>.Reset;
begin
  FPosition := -1;
end;

destructor TBaseSetEnumerator<T>.Destroy;
begin
  inherited Destroy;
end;

end.
