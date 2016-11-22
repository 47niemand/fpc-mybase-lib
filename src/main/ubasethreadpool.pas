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

unit uBaseThreadPool;

{$mode delphi}

interface

uses
  Classes, SysUtils, uBaseTypes;

type

  { EBasePoolError }

  EBasePoolError = class(EBaseException);

  // forward declaration
  TBasePoolQueue = class;

  { TBasePoolObject }

  TBasePoolObject = class(TThread)
  protected
    FOwnPool: TBasePoolQueue;
  public
    constructor Create(const APool: TBasePoolQueue); virtual;
    destructor Destroy; override;
  end;

  TBasePoolObjectClass = class of TBasePoolObject;

  { TBasePoolQueue }

  TBasePoolQueue = class(TObject)
  private
    FAllocatedCount: integer;
    FThreadPool: TThreadList;
  protected
    FPoolSize: integer;
    procedure FreePool;
    procedure ThreadsSync;
  public
    class function GetPoolObjectClass: TBasePoolObjectClass; virtual; abstract;
    constructor Create; virtual;
    destructor Destroy; override;
    function GetAllocatedCount: integer;
    function GetAvaibleCount: integer;
    function GetPoolSize: integer;
    function Pop: TBasePoolObject;
    function Remove(const T: TBasePoolObject): boolean;
    procedure Push(const T: TBasePoolObject);
  end;


implementation

uses uBaseConsts;

{ TBasePoolObject }

constructor TBasePoolObject.Create(const APool: TBasePoolQueue);
begin
  FOwnPool := APool;
  inherited Create(True);
end;

destructor TBasePoolObject.Destroy;
begin
  inherited Destroy;
end;

{ TBasePoolQueue }

procedure TBasePoolQueue.FreePool;
var
  L: TList;
begin
  ThreadsSync;
  while FAllocatedCount > 0 do
  begin
    L := FThreadPool.LockList;
    Assert(FAllocatedCount = L.Count);
    TObject(L.Last).Free;
    FThreadPool.UnlockList;
  end;
end;

procedure TBasePoolQueue.ThreadsSync;
var
  L: TList;
  C: integer;
begin
  //TODO: possible dead lock if thread dont finish it wokr
  repeat
    L := FThreadPool.LockList;
    C := L.Count;
    FThreadPool.UnlockList;
  until C = FAllocatedCount;
end;

constructor TBasePoolQueue.Create;
begin
  inherited Create;
  FThreadPool := TThreadList.Create;
end;

destructor TBasePoolQueue.Destroy;
begin
  FreePool;
  inherited Destroy;
  Assert(FAllocatedCount = 0);
  FreeAndNil(FThreadPool);
end;

function TBasePoolQueue.Pop: TBasePoolObject;
var
  L: TList;
begin
  Result := nil;
  L := FThreadPool.LockList;
  try
    if L.Count > 0 then
    begin
      pointer(Result) := L.Last;
      L.Delete(L.Count - 1);
    end
    else
    if FAllocatedCount < FPoolSize then
    begin
      Inc(FAllocatedCount);
      Result := GetPoolObjectClass.Create(Self);
    end
    else
      raise EBasePoolError.Create(SOutOfResources);
  finally
    FThreadPool.UnlockList;
  end;
end;

procedure TBasePoolQueue.Push(const T: TBasePoolObject);
begin
  FThreadPool.Add(pointer(T));
end;

function TBasePoolQueue.Remove(const T: TBasePoolObject): boolean;
var
  L: TList;
begin
  L := FThreadPool.LockList;
  Result := L.Remove(pointer(T)) >= 0;
  if Result then
    Dec(FAllocatedCount);
  Assert(FAllocatedCount = L.Count);
  FThreadPool.UnlockList;
end;

function TBasePoolQueue.GetPoolSize: integer;
begin
  Result := FPoolSize;
end;

function TBasePoolQueue.GetAllocatedCount: integer;
begin
  Result := FAllocatedCount;
end;

function TBasePoolQueue.GetAvaibleCount: integer;
var
  C: integer;
begin
  C := FThreadPool.LockList.Count;
  Result := FPoolSize - FAllocatedCount + C;
  FThreadPool.UnlockList;
end;


end.

