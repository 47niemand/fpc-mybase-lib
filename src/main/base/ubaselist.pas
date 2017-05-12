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

unit uBaseList;

{$mode delphi}
{$I BaseOptions.inc}

interface

uses
  Classes, SysUtils, uBaseListHelper;

type

  { EBaseListIdxOutOfBounds }

  EBaseListIdxOutOfBounds = class(EListError)
    constructor Create(const Idx: integer);
  end;


  { IBaseList generic interface provide methods for access elements
    by thier index.  Interface allow search for elements
    in the list.
    }
  IBaseList<T> = interface
    function Add(const Item: T): integer;
    function Remove(const Index: integer): T;
    procedure Append(const L: IBaseList<T>);
    function Find(const Item: T; out Index: integer): boolean;
    function FindEx(const StartIdx: integer; const Item: T; out Index: integer): boolean;
    function Exist(const AItem: T): boolean;
    function Get(const Index: integer): T;
    function GetCount: integer;
    function GetEnumerator: IEnumerator<T>;
    procedure Clear;
    procedure Delete(const Index: integer);
    procedure Exchange(const Index1, Index2: integer);
    procedure Put(const Index: integer; const Item: T);
    procedure SetCount(const ACount: integer);
    property Count: integer read GetCount write SetCount;
    property Item[Index: integer]: T read Get write Put;
  end;

  { TBaseList - abstract generic class and basic implementation
    of IBaseList interface. Successors of this class should
    define generic type T and override abstract method GetComparisonFunction
    rwith neccessary for searching
    }
  TBaseList<T> = class(TInterfacedObject, IBaseList<T>, IEnumerable<T>)
  private
    FList: TThreadList;
    FLeftComparison: TLeftComparison<T>;
  protected
    procedure OnUpdateInternal(const {%H-}L: TList; const {%H-}AIndex: integer;
      const {%H-}AOperation: TBaseListOperation; const {%H-}NewValue: T); virtual;
    function GetComparisonFunction: TLeftComparison<T>; virtual;
      abstract; {Warning successors should implement this}
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const AItem: T): integer;
    function Remove(const AIndex: integer): T;
    procedure Append(const S: IBaseList<T>);
    function Exist(const AItem: T): boolean;
    function Find(const AItem: T; out AIndex: integer): boolean;
    function FindEx(const AStartIdx: integer; const AItem: T;
      out AIndex: integer): boolean;
    function First: T;
    function Get(const AIndex: integer): T;
    function GetCount: integer;
    function GetEnumerator: IEnumerator<T>; virtual;
    function Last: T;
    procedure Clear;
    procedure Delete(const AIndex: integer);
    procedure Exchange(const AIndex1, AIndex2: integer);
    procedure Put(const AIndex: integer; const AItem: T);
    procedure SetCount(const ACount: integer);
    property Count: integer read GetCount write SetCount;
    property Item[Index: integer]: T read Get write Put; default;
  end;

  { TBaseListEnumerator }

  TBaseListEnumerator<T> = class(TInterfacedObject, IEnumerator<T>)
  protected
    FPosition: integer;
    FList: TList;
  public
    constructor Create(const AList: TBaseList<T>);
    function GetCurrent: T; virtual;
    function MoveNext: boolean;
    property Current: T read GetCurrent;
    procedure Reset;
    destructor Destroy; override;
  end;

implementation

uses uBaseConsts;


{ TBaseList }

function TBaseList<T>.GetCount: integer;
begin
  Result := FList.LockList.Count;
  FList.UnlockList;
end;

function TBaseList<T>.Find(const AItem: T; out AIndex: integer): boolean;
begin
  Result := FindEx(0, AItem, AIndex);
end;

function TBaseList<T>.FindEx(const AStartIdx: integer; const AItem: T;
  out AIndex: integer): boolean;
var
  L: TList;
  E: T;
begin
  L := FList.LockList;
  try
    if (AStartIdx < 0) or (AStartIdx >= L.Count) then
      raise EBaseListIdxOutOfBounds.Create(AStartIdx);

    E := AItem;
    AIndex := TListHelper<T>.IndexOf(L, E, FLeftComparison, AStartIdx,
      L.Count - AStartIdx);
    Result := AIndex >= 0;
  finally
    FList.UnlockList;
  end;
end;

procedure TBaseList<T>.Put(const AIndex: integer; const AItem: T);
var
  L: TList;
begin
  L := FList.LockList;
  try
    if (AIndex < 0) or (AIndex >= L.Count) then
      raise EBaseListIdxOutOfBounds.Create(AIndex);
    OnUpdateInternal(L, AIndex, bloChange, AItem);
    T(L.List^[AIndex]) := AItem;
  finally
    FList.UnlockList;
  end;
end;

function TBaseList<T>.Get(const AIndex: integer): T;
var
  L: TList;
begin
  L := FList.Locklist;
  try
    if (AIndex < 0) or (AIndex >= L.Count) then
      raise EBaseListIdxOutOfBounds.Create(AIndex);
    Result := T(L.List^[AIndex]);
  finally
    FList.UnlockList;
  end;
end;

function TBaseList<T>.Last: T;
var
  L: TList;
begin
  L := FList.Locklist;
  try
    if L.Count = 0 then
      raise EListError.Create(SListEmpty);
    Result := T(L.List^[L.Count - 1]);
  finally
    FList.UnlockList;
  end;
end;

function TBaseList<T>.First: T;
begin
  Result := Get(0);
end;

procedure TBaseList<T>.Append(const S: IBaseList<T>);
var
  L: TList;
  M, N, I: integer;
  V: T;
begin
  L := FList.Locklist;
  try
    N := L.Count;
    M := S.Count;
    L.Count := L.Count + M;
    for I := 0 to M - 1 do
    begin
      V := S.Get(I);
      OnUpdateInternal(L, I + N, bloAddItem, V);
      T(L.List^[I + N]) := V;
      V := nil;
    end;
  finally
    FList.UnlockList;
  end;
end;

function TBaseList<T>.Remove(const AIndex: integer): T;
var
  L: TList;
begin
  L := FList.Locklist;
  try
    if (AIndex < 0) or (AIndex >= L.Count) then
      raise EBaseListIdxOutOfBounds.Create(AIndex);
    OnUpdateInternal(L, AIndex, bloDeleteItem, nil);
    Result := T(L.List^[AIndex]);
    L.Delete(AIndex);
  finally
    FList.UnlockList;
  end;
end;

function TBaseList<T>.Add(const AItem: T): integer;
var
  L: TList;
begin
  L := FList.Locklist;
  try
    OnUpdateInternal(L, L.Count, bloAddItem, AItem);
    Result := L.Add(nil);
    T(L.List^[Result]) := AItem;
  finally
    FList.UnlockList;
  end;
end;

procedure TBaseList<T>.Delete(const AIndex: integer);
begin
  Remove(AIndex);
end;

destructor TBaseList<T>.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

constructor TBaseList<T>.Create;
begin
  inherited Create;
  FList := TThreadList.Create;
  FLeftComparison := GetComparisonFunction;
  if not Assigned(FLeftComparison) then
    FLeftComparison := @DefaultComparisonFunction;
end;

procedure TBaseList<T>.Exchange(const AIndex1, AIndex2: integer);
var
  L: TList;
begin
  L := FList.Locklist;
  try
    if (AIndex1 < 0) or (AIndex1 >= L.Count) then
      raise EBaseListIdxOutOfBounds.Create(AIndex1);
    if (AIndex2 < 0) or (AIndex2 >= L.Count) then
      raise EBaseListIdxOutOfBounds.Create(AIndex1);
    L.Exchange(AIndex1, AIndex2);
  finally
    FList.UnlockList;
  end;
end;

procedure TBaseList<T>.SetCount(const ACount: integer);
var
  L: TList;
  Idx: integer;
begin
  L := FList.LockList;
  try
    for Idx := ACount to L.Count do
    begin
      OnUpdateInternal(L, Idx, bloDeleteItem, nil);
      T(L.List^[Idx]) := nil;
    end;
    L.Count := ACount;
  finally
    FList.UnlockList;
  end;
end;

procedure TBaseList<T>.Clear;
var
  Idx: integer;
  L: TList;
begin
  L := FList.Locklist;
  try
    for Idx := 0 to L.Count - 1 do
    begin
      OnUpdateInternal(L, Idx, bloFree, nil);
      T(L.List^[Idx]) := nil;
    end;
    FList.Clear;
  finally
    FList.UnlockList;
  end;
end;

function TBaseList<T>.Exist(const AItem: T): boolean;
var
  Idx: integer;
begin
  Result := Find(AItem, Idx);
end;

function TBaseList<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TBaseListEnumerator<T>.Create(Self);
end;

procedure TBaseList<T>.OnUpdateInternal(const {%H-}L: TList;
  const {%H-}AIndex: integer; const {%H-}AOperation: TBaseListOperation;
  const {%H-}NewValue: T);
begin
  // nothing
end;

{ TBaseListEnumerator }

constructor TBaseListEnumerator<T>.Create(const AList: TBaseList<T>);
begin
  inherited Create;
  FList := AList.FList.LockList;
  AList.FList.UnlockList;
  FPosition := -1;
end;

function TBaseListEnumerator<T>.GetCurrent: T;
begin
  Result := T(FList.Items[FPosition]);
end;

function TBaseListEnumerator<T>.MoveNext: boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

procedure TBaseListEnumerator<T>.Reset;
begin
  FPosition := -1;
end;

destructor TBaseListEnumerator<T>.Destroy;
begin
  inherited Destroy;
end;

{ EBaseListIdxOutOfBounds }

constructor EBaseListIdxOutOfBounds.Create(const Idx: integer);
begin
  inherited CreateFmt(SListOutOfBoundsFmt, [Idx]);
end;


end.
