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

unit uBaseObserver;

{$mode delphi}
{$I BaseOptions.inc}

interface

uses
  Classes, SysUtils, uBaseInterface;

type

  { TBaseObserver }

  TBaseObserver = class(TWeaklyInterfacedObject, IBaseObserver)
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Update(const Subject: IUnknown; const AOperation: TBaseObservedOperation;
      const Data: IUnknown); virtual; abstract;
  end;

  { TBaseSubject }
  TBaseSubject = class(TInterfacedObject, IBaseSubject)
  private
    FController: pointer;
    FNeedGC: boolean;
    FObservers: IInterfaceList;
    procedure GarbageCollect;
  protected
    procedure AttachObserver(AObserver: IBaseObserver);
    procedure AttachWeak(AWeakRef: IWeakRef);
    procedure DetachObserver(AObserver: IBaseObserver);
    procedure DetachWeak(AWeakRef: IWeakRef);
    procedure DoNotify(const Target: IBaseObserver; AOperation: TBaseObservedOperation;
      const Data: IUnknown); virtual;
    procedure InitObserver;
  public
    constructor Create(const AController: IUnknown);
    destructor Destroy; override;
    function Listeners: integer;
    procedure Attach(const AObserver: IUnknown); overload;
    procedure Detach(const AObserver: IUnknown); overload;
    procedure Notify(const AOperation: TBaseObservedOperation; const Data: IUnknown);
  end;


function GetBaseObserverCounter: integer;
function GetBaseSubjectCounter: integer;

implementation

uses uBaseConsts;

var
  BaseObserverCounter: integer = 0;
  BaseSubjectCounter: integer = 0;
  BaseSubjectAttach: integer = 0;
  BaseSubjectDeach: integer = 0;

function GetBaseObserverCounter: integer;
begin
  Result := BaseObserverCounter;
end;

function GetBaseSubjectCounter: integer;
begin
  Result := BaseSubjectCounter;
end;

{ TBaseObserver }

procedure TBaseObserver.AfterConstruction;
begin
  inherited AfterConstruction;
  InterLockedIncrement(BaseObserverCounter);
end;

destructor TBaseObserver.Destroy;
begin
  inherited Destroy;
  InterLockedDecrement(BaseObserverCounter);
end;

{ TBaseSubject }

procedure TBaseSubject.DoNotify(const Target: IBaseObserver;
  AOperation: TBaseObservedOperation; const Data: IUnknown);
begin
{$IFDEF WITHLOG}
  Log(Self, SafeBaseObjectReference(Target), 'DoNotify[%d](%p,%p)',
    [Ord(AOperation), pointer(Target), pointer(Data)]);
{$ENDIF}
  Target.Update(IInterface(FController), AOperation, Data);
end;


procedure TBaseSubject.InitObserver;
var
  I: IInterfaceList;
begin
  I := TInterfaceList.Create;
  I._AddRef;
  if InterlockedCompareExchange(pointer(FObservers), pointer(I), nil) <> nil then
  begin
    Assert(Assigned(FObservers));
    I._Release;
    I := nil;
  end
  else
    Assert(pointer(FObservers) = pointer(I));
end;

procedure TBaseSubject.AttachWeak(AWeakRef: IWeakRef);
var
  I: IInterface;
  O: IBaseObserver;
begin
  Assert(Assigned(FObservers));
  O := CastBaseObserver(AWeakRef.Get);
  Assert(Assigned(O));
  I := CastInterface(AWeakRef);
  FObservers.Lock;
  try
    if FObservers.IndexOf(I) < 0 then
    begin
      FObservers.Add(I);
      InterLockedIncrement(BaseSubjectAttach);
    end;
  finally
    FObservers.Unlock;
  end;
end;

procedure TBaseSubject.AttachObserver(AObserver: IBaseObserver);
var
  I: IInterface;
begin
  Assert(Assigned(FObservers));
  I := CastInterface(AObserver);
  FObservers.Lock;
  try
    if FObservers.IndexOf(I) < 0 then
    begin
      FObservers.Add(I);
      InterLockedIncrement(BaseSubjectAttach);
    end;
  finally
    FObservers.Unlock;
  end;
end;

procedure TBaseSubject.DetachWeak(AWeakRef: IWeakRef);
var
  I: IInterface;
  O: IBaseObserver;
  Res: integer;
begin
  Assert(Assigned(FObservers));
  O := CastBaseObserver(AWeakRef.Get);
  Assert(Assigned(O));
  I := CastInterface(AWeakRef);
  Res := FObservers.Remove(I);
  if Res >= 0 then
    InterLockedDecrement(BaseSubjectDeach);
  Assert(Res >= 0);
end;

procedure TBaseSubject.DetachObserver(AObserver: IBaseObserver);
var
  I: IInterface;
  Res: integer;
begin
  Assert(Assigned(FObservers));
  I := CastInterface(AObserver);
  Res := FObservers.Remove(I);
  if Res >= 0 then
    InterLockedDecrement(BaseSubjectDeach);
  Assert(Res >= 0);
end;

procedure TBaseSubject.Attach(const AObserver: IUnknown);
var
  W: IWeakly;
  R: IWeakRef;
  O: IBaseObserver;
begin
  if not Assigned(FObservers) then
    InitObserver;
  Assert(Assigned(FObservers));

  if FNeedGC then
    GarbageCollect;
  if AObserver.QueryInterface(IWeakRef, R) = S_OK then
    AttachWeak(R)
  else
  if AObserver.QueryInterface(IBaseObserver, O) = S_OK then
  begin
    if O.QueryInterface(IWeakly, W) = S_OK then
      AttachWeak(W.WeakRef)
    else
      AttachObserver(O);
  end
  else
    raise EBaseIntfException.Create(SCantDoDueInterface);
end;

procedure TBaseSubject.Detach(const AObserver: IUnknown);
var
  W: IWeakly;
  R: IWeakRef;
  O: IBaseObserver;
begin
  if Assigned(FObservers) then
  begin
    if FNeedGC then
      GarbageCollect;
    if AObserver.QueryInterface(IWeakRef, R) = S_OK then
      DetachWeak(R)
    else
    if AObserver.QueryInterface(IBaseObserver, O) = S_OK then
    begin
      if O.QueryInterface(IWeakly, W) = S_OK then
        DetachWeak(W.WeakRef)
      else
        DetachObserver(O);
    end
    else
      raise EBaseIntfException.Create(SCantDoDueInterface);
  end;
end;

procedure TBaseSubject.GarbageCollect;
var
  I: integer;
begin
  if FObservers <> nil then
    repeat
      I := FObservers.Remove(nil);
    until I < 0;
  FNeedGC := False;
end;

procedure TBaseSubject.Notify(const AOperation: TBaseObservedOperation;
  const Data: IUnknown);
var
  K: integer;
  I: IInterface;
  O: IBaseObserver;
  W: IWeakRef;
begin
{$IFDEF WITHLOG}
  Log(Self, nil, 'Notify[%d](%p)',
    [Ord(AOperation), pointer(Data)]);
{$ENDIF}
  if Assigned(FObservers) then
  begin
    FObservers.Lock; //TODO: slove possible deadlock
    try
      for K := 0 to FObservers.Count - 1 do
      begin
        I := FObservers.Items[K];
        if Assigned(I) then
        begin
          if I.QueryInterface(IWeakRef, W) = S_OK then
          begin
            if W.IsAlive and (W.Get.QueryInterface(IBaseObserver, O) = S_OK) then
            begin
              DoNotify(O, AOperation, Data);
              Continue;
            end
            else
            if not W.IsAlive then
            begin
              FObservers.Items[K] := nil;
              FNeedGC := True;
              Continue;
            end
            else
              raise EBaseIntfException.CreateFmt(SWeakPointErrorFmt, ['IBaseObserver']);
          end
          else
          begin
            O := CastBaseObserver(I);
            DoNotify(O, AOperation, Data);
            Continue;
          end;
        end
        else
          Assert(FNeedGC = True);
      end;
    finally
      FObservers.Unlock;
    end;
  end;
end;

constructor TBaseSubject.Create(const AController: IUnknown);
begin
  inherited Create;
  InterLockedIncrement(BaseSubjectCounter);
  FController := pointer(AController);
  {$IFDEF WITHLOG}
  Log(Self, nil, 'Create Subject for %p', [pointer(FController)]);
  {$ENDIF}
end;

destructor TBaseSubject.Destroy;
begin
  if Assigned(FObservers) then
  begin
    FObservers.Clear;
    FObservers := nil;
  end;
  inherited Destroy;
  InterLockedDecrement(BaseSubjectCounter);
end;

function TBaseSubject.Listeners: integer;
begin
  if Assigned(FObservers) then
    Result := FObservers.Count
  else
    Result := 0;
end;

end.





