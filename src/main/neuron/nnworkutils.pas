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

unit NNWorkUtils;

{$mode delphi}

interface

uses
  Classes, SysUtils, syncobjs, NNWorkTypes;

procedure NNThreadTrain(const AProc: TNNTrainProc; const ATrainData: PNNTrainData;
  var ALoopVariable: integer; const ALimit: integer; var FinishEvent: TEvent;
  const AThreadsCount: integer; const Res: PVarPointerArray);

function NNThreadResult(const P: pointer): DType;
procedure NNTrainSync;

procedure SaveArrayOfDType(const D: TVarArrayOfDType; const FileName: string);
function LoadArrayOfDType(const FileName: string): TVarArrayOfDType;


implementation


uses uBaseThreadPool;


//Object Pooling
const
  MaxThreadPoolSize = 16;


type

  { TTrainThread }

  TTrainThread = class(TBasePoolObject)
  protected
    sum: DType;
    NeedResult: boolean;
    TrainData: PNNTrainData;
    SharedLoopVar: PInteger;
    Proc: TNNTrainProc;
    Limit: integer;
    Finish: TEvent;
    StartEvent: TEvent;
    FWorking: boolean;
    procedure Execute; override;
  public
    constructor Create(const APool: TBasePoolQueue); override;
    destructor Destroy; override;
  end;

type

  { TMyThreadPool }

  TMyThreadPool = class(TBasePoolQueue)
    class function GetPoolObjectClass: TBasePoolObjectClass; override;
    constructor Create; override;
  end;

var
  MyPool: TBasePoolQueue;


procedure NNThreadTrain(const AProc: TNNTrainProc; const ATrainData: PNNTrainData;
  var ALoopVariable: integer; const ALimit: integer; var FinishEvent: TEvent;
  const AThreadsCount: integer; const Res: PVarPointerArray);
var
  P: TTrainThread;
  I: integer;
begin
  I := 0;

  while I < AThreadsCount do
  begin
    P := TTrainThread(MyPool.Pop);
    InterLockedIncrement(ATrainData^.Refs);
    with P do
    begin
      Proc := AProc;
      sum := 0.0;
      SharedLoopVar := @ALoopVariable;
      Limit := ALimit;
      TrainData := ATrainData;
      Finish := FinishEvent;
      NeedResult := Assigned(Res);
    end;
    if Assigned(Res) then
      Res[I] := P
    else
      P.StartEvent.SetEvent;
    Inc(I);
  end;

  if Assigned(Res) then
    for I := 0 to AThreadsCount - 1 do
      TTrainThread(Res[I]).StartEvent.SetEvent;
end;

function NNThreadResult(const P: pointer): DType;
begin
  Result := TTrainThread(P).sum;
end;

procedure NNTrainSync;
begin
  TMyThreadPool(MyPool).ThreadsSync;
end;

procedure SaveArrayOfDType(const D: TVarArrayOfDType; const FileName: string);
var
  T: TextFile;
  V: DType;
begin
  Assign(T, FileName);
  Rewrite(T);
  try
    Writeln(T, '# TArrayOfDType');
    WriteLn(T, Length(D));
    for V in D do
      Write(T, V, ' ');
    WriteLn(T);
    WriteLn(T, '# end');
  finally
    CloseFile(T);
  end;
end;

function LoadArrayOfDType(const FileName: string): TVarArrayOfDType;
var
  T: TextFile;
  V: DType;
  K, N: integer;
  S: string;
begin
  Assign(T, FileName);
  Rewrite(T);
  try
    N := 0;
    while not EOF(T) do
      try
        Read(T, V);
        Inc(N);
        if N = 1 then
        begin
          SetLength(Result, Round(V));
          K := 0;
        end;
        if N > 1 then
        begin
          if K >= Length(Result) then
            Break;
          Result[K] := V;
          Inc(K);
        end;
      except
        Read(T, S);
        Write(S);
      end;
  finally
    CloseFile(T);
  end;
end;

{ TMyThreadPool }

class function TMyThreadPool.GetPoolObjectClass: TBasePoolObjectClass;
begin
  Result := TTrainThread;
end;

constructor TMyThreadPool.Create;
begin
  inherited Create;
  FPoolSize := MaxThreadPoolSize;
end;


{ TTrainThread }

procedure TTrainThread.Execute;
var
  I: integer;
  S: DType;
begin
  while StartEvent.WaitFor(INFINITE) = wrSignaled do
  begin
    if Terminated then
      break;
    FWorking := True;
    S := 0.0;
    if NeedResult then
    begin
      I := InterLockedIncrement(SharedLoopVar^) - 1;
      while I <= Limit do
      begin
        S := S + Proc(I, TrainData);
        I := InterLockedIncrement(SharedLoopVar^) - 1;
      end;
    end
    else
    begin
      I := InterLockedIncrement(SharedLoopVar^) - 1;
      while I <= Limit do
      begin
        Proc(I, TrainData);
        I := InterLockedIncrement(SharedLoopVar^) - 1;
      end;
    end;
    sum := s;
    InterLockedDecrement(TrainData^.Refs);
    FWorking := False;
    if Assigned(FOwnPool) then
      FOwnPool.Push(Self);
    if Assigned(Finish) then
      Finish.SetEvent;
  end;
end;

constructor TTrainThread.Create(const APool: TBasePoolQueue);
begin
  StartEvent := TEvent.Create(nil, False, False, '');
  inherited Create(APool);
  Start;
end;

destructor TTrainThread.Destroy;
begin
  Terminate;
  if Suspended then
    Start;
  StartEvent.SetEvent;
  WaitFor;
  if Assigned(FOwnPool) then
    FOwnPool.Remove(Self);
  inherited Destroy;
  FreeAndNil(StartEvent);
end;

procedure FreePool;
begin
  TMyThreadPool(MyPool).FreePool;
end;

initialization
  MyPool := TMyThreadPool.Create;

finalization
  FreeAndNil(MyPool);
end.
