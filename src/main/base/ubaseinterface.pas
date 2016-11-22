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

unit uBaseInterface;

{$mode delphi}
{$i BaseOptions.inc}


interface

uses
  Classes, SysUtils, Variants, uBaseTypes, uBaseConsts;

type

  { EBaseIntfException }

  EBaseIntfException = class(Exception);

  { EBaseIntfNotSupported }

  EBaseIntfNotSupported = class(EBaseIntfException)
    constructor CreateMsg(const msg: string);
  end;

  { EBasePropReadOnly }

  EBasePropReadOnly = class(EBaseIntfException);

  { EBaseConcurrencyError }

  EBaseConcurrencyError = class(EBaseIntfException);

  { IBaseVersionCounter }

  IBaseVersionCounter = interface
    ['{8482E6D3-D8D3-48A5-AFD3-29BCEAE78A52}']
    function Version: integer;
  end;



  { IWeakRef }

  IWeakRef = interface
    ['{04E01ADC-8137-4524-AE83-58A69536D7E3}']
    function Get: IUnknown;
    function IsAlive: boolean;
    procedure _Clean;
  end;

  { IWeakly }

  IWeakly = interface
    ['{74917189-7FA9-41D6-AF59-F99C5C0F383D}']
    function WeakRef: IWeakRef;
  end;

  { IHash }

  IHash = interface
    ['{03766DB7-8AB2-4879-9C6D-B9524D7E7DF7}']
    function GetHash: int64;
  end;

  { IBaseClonable }

  IBaseClonable = interface
    ['{097392DE-A90D-4322-A7C4-E404CFF58A68}']
    function Clone: IInterface;
  end;

  { IBaseObserver }

  TBaseObservedOperation = (booAddItem, booChange, booDeleteItem, booCustom, booFree);

  IBaseObserver = interface
    ['{5F908860-9A24-458A-A39C-51AC0634555C}']
    procedure Update(const Subject: IUnknown; const Operation: TBaseObservedOperation;
      const Data: IUnknown);
  end;

  IBaseSubject = interface
    ['{4EAC7887-49AD-411C-90FF-102446F19E84}']
    procedure Attach(const Observer: IUnknown);
    procedure Detach(const Observer: IUnknown);
    procedure Notify(const Operation: TBaseObservedOperation; const Data: IUnknown);
  end;

  { IBaseObjectReference }

  IBaseObjectReference = interface
    ['{63106F79-4592-46A6-B712-A8F74989865D}']
    function GetObject: TObject;
  end;

  { IBaseConcurency }

  IBaseConcurency = interface
    ['{1300E469-CDCB-411D-979B-C94C07E34E3E}']
    procedure Lock;
    procedure Unlock;
  end;

  { IBaseValue }

  IBaseValue = interface
    ['{D17D7430-6176-4E21-B7DF-2B02ADF4B28C}']
    function Clone(const AValue: variant): IBaseValue;
    function CompareTo(const V: variant): TVariantRelationship;
    function GetValue: variant;
    procedure SetValue(const Value: variant);
    property Value: variant read GetValue write SetValue;
  end;

  { IBaseEntry (key-value pair) }

  IBaseEntry = interface
    ['{D5BEE66C-BE7B-481F-9371-71024D82E1AB}']
    function GetName: string;
    function GetValue: variant;
    procedure SetValue(const Value: variant);
    property Name: string read GetName;
    property Value: variant read GetValue write SetValue;
  end;

  { IBaseValueEnumerator }

  IBaseValueEnumerator = IEnumerator<IBaseValue>;

  { IBaseEntryEnumerator }

  IBaseEntryEnumerator = IEnumerator<IBaseEntry>;

  { IBaseMap }

  IBaseMap = interface
    ['{454DC4C0-B559-4FA0-A203-74414C430182}']
    function Exists(const Name: string): boolean;
    function Find(const Name: string): IBaseEntry;
    function Remove(const Name: string): IBaseEntry;
    function GetEnumerator: IBaseEntryEnumerator;
    function GetValue(const Name: string): variant;
    function Size: integer;
    procedure Append(const Map: IBaseMap);
    procedure Clear;
    procedure SetValue(const Name: string; const Value: variant);
    property Values[Name: string]: variant read GetValue write SetValue; default;
  end;

  { IBaseMapEvents }

  IBaseMapEvents = interface
    function GetOnAfterAddItem: TNotifyEvent;
    function GetOnAfterChangeValue: TNotifyEvent;
    function GetOnAfterRemoveItem: TNotifyEvent;
    function GetOnBeforeAddItem: TNotifyEvent;
    function GetOnBeforeChangeValue: TNotifyEvent;
    function GetOnBeforeRemoveItem: TNotifyEvent;
    procedure SetOnAfterAddItem(AValue: TNotifyEvent);
    procedure SetOnAfterChangeValue(AValue: TNotifyEvent);
    procedure SetOnAfterRemoveItem(AValue: TNotifyEvent);
    procedure SetOnBeforeAddItem(AValue: TNotifyEvent);
    procedure SetOnBeforeChangeValue(AValue: TNotifyEvent);
    procedure SetOnBeforeRemoveItem(AValue: TNotifyEvent);
    property OnAfterAddItem: TNotifyEvent read GetOnAfterAddItem write SetOnAfterAddItem;
    property OnAfterRemoveItem: TNotifyEvent
      read GetOnAfterRemoveItem write SetOnAfterRemoveItem;
    property OnAfterChangeValue: TNotifyEvent
      read GetOnAfterChangeValue write SetOnAfterChangeValue;
    property OnBeforeAddItem: TNotifyEvent read GetOnBeforeAddItem
      write SetOnBeforeAddItem;
    property OnBeforeRemoveItem: TNotifyEvent
      read GetOnBeforeRemoveItem write SetOnBeforeRemoveItem;
    property OnBeforeChangeValue: TNotifyEvent
      read GetOnBeforeChangeValue write SetOnBeforeChangeValue;
  end;

  { TWeaklyInterfacedObject }

  TWeaklyInterfacedObject = class(TInterfacedObject, IWeakly)
  private
    FWeakRef: IWeakRef;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    function WeakRef: IWeakRef;
  end;

function CastBaseClonable(const I: IUnknown): IBaseClonable; inline;
function CastBaseEntry(const I: IUnknown): IBaseEntry; inline;
function CastBaseMap(const I: IUnknown): IBaseMap; inline;
function CastBaseObjectReference(const I: IUnknown): IBaseObjectReference; inline;
function CastBaseObserver(const I: IUnknown): IBaseObserver; inline;
function CastBaseSubject(const I: IUnknown): IBaseSubject; inline;
function CastBaseValue(const I: IUnknown): IBaseValue; inline;
function CastBaseVersionCounter(const I: IUnknown): IBaseVersionCounter; inline;
function CastInterface(const I: IUnknown): IInterface; inline;
function SafeBaseObjectReference(const I: IUnknown): TObject;
function SafeGetWeakRef(const I: IUnknown): IWeakRef;

//todo: create tests
function VarIsIntf(const V: variant): boolean;
function CastVarToIntf(const V: variant): IUnknown;

function GetWeakRefCounter: integer;
function GetWeaklyInterfacedObjectCounter: integer;
{$IFDEF WITHLOG}
procedure Log(Sender, Data: TObject; Msg: string; const Args: array of const);
{$ENDIF}

implementation

var
  WeakRefCounter: integer = 0;
  WeaklyInterfacedObjectCounter: integer = 0;

type

  { TWeakRef }

  TWeakRef = class(TInterfacedObject, IWeakRef)
  private
    FOwner: pointer;
  public
    constructor Create(const AOwner: IUnknown);
    destructor Destroy; override;
    function Get: IUnknown;
    function IsAlive: boolean;
    procedure AfterConstruction; override;
    procedure _Clean;
  end;

function CastInterface(const I: IUnknown): IInterface;
begin
  if I.QueryInterface(IInterface, Result) <> S_OK then
    raise EBaseIntfNotSupported.CreateMsg('IInterface');
end;

function SafeBaseObjectReference(const I: IUnknown): TObject;
var
  O: IBaseObjectReference;
begin
  if Assigned(I) and (I.QueryInterface(IBaseObjectReference, O) = S_OK) then
  begin
    Result := O.GetObject;
    O := nil; // forced clear reference
  end
  else
    Result := nil;
end;

function SafeGetWeakRef(const I: IUnknown): IWeakRef;
var
  E: IWeakly;
begin
  if Assigned(I) and (I.QueryInterface(IWeakly, E) = S_OK) then
  begin
    Result := E.WeakRef;
    E := nil;
  end
  else
    Result := nil;
end;

function CastBaseEntry(const I: IUnknown): IBaseEntry;
begin
  if not Assigned(I) then
    Result := nil
  else
  if I.QueryInterface(IBaseEntry, Result) <> S_OK then
    raise EBaseIntfNotSupported.CreateMsg('IBaseEntry');
end;

function CastBaseObserver(const I: IUnknown): IBaseObserver;
begin
  if not Assigned(I) then
    Result := nil
  else
  if I.QueryInterface(IBaseObserver, Result) <> S_OK then
    raise EBaseIntfNotSupported.CreateMsg('IBaseObserver');
end;

function CastBaseMap(const I: IUnknown): IBaseMap;
begin
  if not Assigned(I) then
    Result := nil
  else
  if I.QueryInterface(IBaseMap, Result) <> S_OK then
    raise EBaseIntfNotSupported.CreateMsg('IBaseMap');
end;

function CastBaseSubject(const I: IUnknown): IBaseSubject;
begin
  if not Assigned(I) then
    Result := nil
  else
  if I.QueryInterface(IBaseSubject, Result) <> S_OK then
    raise EBaseIntfNotSupported.CreateMsg('IBaseSubject');
end;

function CastBaseValue(const I: IUnknown): IBaseValue;
begin
  if not Assigned(I) then
    Result := nil
  else
  if I.QueryInterface(IBaseValue, Result) <> S_OK then
    raise EBaseIntfNotSupported.CreateMsg('IBaseValue');
end;

function CastBaseClonable(const I: IUnknown): IBaseClonable;
begin
  if not Assigned(I) then
    Result := nil
  else
  if I.QueryInterface(IBaseClonable, Result) <> S_OK then
    raise EBaseIntfNotSupported.CreateMsg('IBaseClonable');
end;

function CastBaseVersionCounter(const I: IUnknown): IBaseVersionCounter;
begin
  if not Assigned(I) then
    Result := nil
  else
  if I.QueryInterface(IBaseVersionCounter, Result) <> S_OK then
    raise EBaseIntfNotSupported.CreateMsg('IVersionCounter');
end;

function CastBaseObjectReference(const I: IUnknown): IBaseObjectReference;
begin
  if not Assigned(I) then
    Result := nil
  else
  if I.QueryInterface(IBaseObjectReference, Result) <> S_OK then
    raise EBaseIntfNotSupported.CreateMsg('IInterfaceObjectReference');
end;

function VarIsIntf(const V: variant): boolean;
var
  U: IUnknown;
  O: IBaseValue;
  P: PVarData;
begin
  if VarType(V) = varunknown then
  begin
    P := FindVarData(V);
    U := P.vunknown;
    Result := U.QueryInterface(IUnknown, O) = S_OK;
    U := nil;
  end;
end;

function CastVarToIntf(const V: variant): IUnknown;
begin
  if VarIsNull(V) or VarIsEmpty(v) then
    Result := nil
  else
    Result := V;
end;

function GetWeakRefCounter: integer;
begin
  Result := WeakRefCounter;
end;

function GetWeaklyInterfacedObjectCounter: integer;
begin
  Result := WeaklyInterfacedObjectCounter;
end;

{$IFDEF WITHLOG}
procedure Log(Sender, Data: TObject; Msg: string; const Args: array of const);
var
  s, d: string;
begin
  if Assigned(Sender) then
    S := format('%s@%p', [Sender.ClassName, pointer(Sender)])
  else
    s := '<nil>';
  if Assigned(Data) then
    d := format('%s@%p', [Data.ClassName, pointer(Data)])
  else
    d := '<nil>';
  Writeln(format('[%x] ', [GetCurrentThreadId]), s, '(', d, ') ', format(msg, Args));
end;

{$ENDIF}

{ EBaseIntfNotSupported }

constructor EBaseIntfNotSupported.CreateMsg(const msg: string);
begin
  inherited CreateFmt('Interface <%s> not supported', [msg]);
end;

procedure TWeakRef._Clean;
begin
  FOwner := nil;
end;

function TWeakRef.Get: IUnknown;
begin
  if Assigned(FOwner) then
    Result := IUnknown(FOwner)
  else
    Result := nil;
end;

constructor TWeakRef.Create(const AOwner: IUnknown);
begin
  FOwner := pointer(AOwner);
end;

procedure TWeakRef.AfterConstruction;
begin
  inherited AfterConstruction;
  InterLockedIncrement(WeakRefCounter);
end;

destructor TWeakRef.Destroy;
begin
  inherited Destroy;
  InterLockedDecrement(WeakRefCounter);
end;

function TWeakRef.IsAlive: boolean;
begin
  Result := Assigned(FOwner);
end;

{ TWeaklyInterfacedObject }

destructor TWeaklyInterfacedObject.Destroy;
begin
  inherited;
  if Assigned(FWeakRef) then
    FWeakRef._Clean;
  InterLockedDecrement(WeaklyInterfacedObjectCounter);
end;

procedure TWeaklyInterfacedObject.AfterConstruction;
begin
  inherited AfterConstruction;
  InterLockedIncrement(WeaklyInterfacedObjectCounter);
end;

function TWeaklyInterfacedObject.WeakRef: IWeakRef;
var
  obj: IWeakRef;
begin
  if not Assigned(FWeakRef) then
  begin
    obj := TWeakRef.Create(Self);
    obj._AddRef;
    if InterlockedCompareExchange(pointer(FWeakRef), pointer(obj), nil) <> nil then
      obj._Release;
    Assert(Assigned(FWeakRef));
  end;
  Result := FWeakRef;
end;

end.
