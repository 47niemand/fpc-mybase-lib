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

unit uBaseEntry;

{$mode delphi}
{$i BaseOptions.inc}
{$undef WITHLOG}

interface

uses
  Classes, SysUtils, uBaseInterface;

function NewImmutableEntry(AName: string; AValue: variant): IBaseEntry; overload;
function NewEntry(AName: string; AValue: variant): IBaseEntry; overload;
function NewEntry(AName: string): IBaseEntry; overload;
function CloneEntry(const E: IBaseEntry): IBaseEntry;

function GetBaseEntryCounter: integer;

implementation

uses Variants, uBaseConsts, uBaseObserver, uBaseValue;

var
  BaseEntryCounter: integer = 0;

type

  { TBaseImmutableEntry }

  TBaseImmutableEntry = class(TWeaklyInterfacedObject, IBaseEntry)
  private
    FName: string;
    FValue: variant;
  public
    function GetName: string;
    function GetValue: variant;
    procedure SetValue(const Value: variant);
    constructor Create(const AName: string; const AValue: variant);
    destructor Destroy; override;
  published
    property Name: string read GetName;
    property Value: variant read GetValue write SetValue;
  end;

  { TBaseEntry }

  TBaseEntry = class(TWeaklyInterfacedObject, IBaseEntry, IBaseClonable,
    IBaseSubject, IBaseVersionCounter)
  private
    FName: string;
    FSubject: IBaseSubject;
    FBaseValue: IBaseValue;
    FVersion: integer;
    procedure InitSubject; inline;
  protected
    procedure OnChange(const NewValue: variant); virtual;
  public
    constructor Create(AName: string); overload;
    constructor Create(AName: string; AValue: variant);
    destructor Destroy; override;
    function Clone: IInterface;
    function GetName: string;
    function GetValue: variant;
    function Version: integer;
    procedure Attach(const AObserver: IUnknown);
    procedure Detach(const AObserver: IUnknown);
    procedure Notify(const AOperation: TBaseObservedOperation; const AData: IUnknown);
    procedure SetValue(const AValue: variant);
  published
    property Name: string read GetName;
    property Value: variant read GetValue write SetValue;
  end;

function NewImmutableEntry(AName: string; AValue: variant): IBaseEntry;
begin
  Result := TBaseImmutableEntry.Create(AName, AValue);
end;

function NewEntry(AName: string; AValue: variant): IBaseEntry;
begin
  Result := TBaseEntry.Create(AName, AValue);
end;

function NewEntry(AName: string): IBaseEntry;
begin
  Result := TBaseEntry.Create(AName);
end;

function CloneEntry(const E: IBaseEntry): IBaseEntry;
var
  I: IBaseClonable;
begin
  Result := nil;
  if E.QueryInterface(IBaseClonable, I) = S_OK then
  begin
    if I.Clone.QueryInterface(IBaseEntry, Result) <> S_OK then
      raise EBaseException.Create(SCloneError);
  end
  else
    Result := NewEntry(E.Name, E.Value);
end;

function GetBaseEntryCounter: integer;
begin
  Result := BaseEntryCounter;
end;

{ TBaseImmutableEntry }

function TBaseImmutableEntry.GetName: string;
begin
  Result := FName;
end;

function TBaseImmutableEntry.GetValue: variant;
begin
  Result := FValue;
end;

procedure TBaseImmutableEntry.SetValue(const Value: variant);
begin
  Assert(Assigned(@Value));
  raise EPropReadOnly.Create(SPropReadOnly);
end;

constructor TBaseImmutableEntry.Create(const AName: string; const AValue: variant);
begin
  inherited Create;
  InterLockedIncrement(BaseEntryCounter);
  FName := AName;
  FValue := AValue;
end;

destructor TBaseImmutableEntry.Destroy;
begin
  inherited Destroy;
  InterLockedDecrement(BaseEntryCounter);
end;

{ TBaseEntry }

procedure TBaseEntry.SetValue(const AValue: variant);
begin
  if FBaseValue.CompareTo(AValue) = vrEqual then
    exit;
  InterLockedIncrement(FVersion);
  OnChange(AValue);
  FBaseValue.SetValue(AValue);
end;

function TBaseEntry.GetName: string;
begin
  Result := FName;
end;

function TBaseEntry.GetValue: variant;
begin
  Result := FBaseValue.Value;
end;

procedure TBaseEntry.OnChange(const NewValue: variant);
begin
{$IFDEF WITHLOG}
  Log(Self, nil, '[%s] OnChange "%s" -> "%s"',
    [FName, Copy(VarToStrDef(FBaseValue.Value, '<undef>'), 1, 20),
    Copy(VarToStrDef(NewValue, '<undef>'), 1, 20)]);
{$ENDIF}
  if Assigned(FSubject) then
    Notify(booChange, NewImmutableValue(NewValue));
end;

procedure TBaseEntry.InitSubject;
var
  S: IBaseSubject;
begin
  S := TBaseSubject.Create(Self);
  if InterlockedCompareExchange(Pointer(FSubject), Pointer(S), nil) <> nil then
  begin
    S := nil;
    Assert(Assigned(FSubject));
  end
  else
  begin
    S._AddRef;
    Assert(Pointer(FSubject) = Pointer(S));
  end;
end;

procedure TBaseEntry.Attach(const AObserver: IUnknown);
begin
  if not Assigned(FSubject) then
    InitSubject;
  FSubject.Attach(AObserver);
end;

procedure TBaseEntry.Detach(const AObserver: IUnknown);
begin
  if Assigned(FSubject) then
    FSubject.Detach(AObserver);
end;

procedure TBaseEntry.Notify(const AOperation: TBaseObservedOperation;
  const AData: IUnknown);
begin
  if Assigned(FSubject) then
    FSubject.Notify(AOperation, AData);
end;

function TBaseEntry.Version: integer;
begin
  Result := FVersion;
end;

function TBaseEntry.Clone: IInterface;
begin
  Result := TBaseEntry.Create(Name, Value);
end;

constructor TBaseEntry.Create(AName: string; AValue: variant);
begin
  inherited Create;
  InterLockedIncrement(BaseEntryCounter);
  FName := AName;
  FBaseValue := NewValue(AValue);
{$IFDEF WITHLOG}
  Log(Self, nil, '[%s] Create -> "%s"',
    [Name, Copy(VarToStrDef(FBaseValue.Value, '<undef>'), 1, 20)]);
{$ENDIF}
end;

constructor TBaseEntry.Create(AName: string);
begin
  inherited Create;
  InterLockedIncrement(BaseEntryCounter);
  FName := AName;
  FBaseValue := NewValue(Unassigned);
{$IFDEF WITHLOG}
  Log(Self, nil, '[%s] Create -> "%s"',
    [Name, Copy(VarToStrDef(FBaseValue.Value, '<undef>'), 1, 20)]);
{$ENDIF}
end;

destructor TBaseEntry.Destroy;
begin
  FBaseValue := nil;
  FSubject := nil;
  inherited Destroy;
  InterLockedDecrement(BaseEntryCounter);
{$IFDEF WITHLOG}
  Log(Self, nil, '[%s] Destroy', [FName]);
{$ENDIF}
end;

end.
