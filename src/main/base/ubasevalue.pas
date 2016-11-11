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

unit uBaseValue;

{$mode delphi}
{$i BaseOptions.inc}
{$undef WITHLOG}


interface

uses
  Classes, SysUtils, variants, uBaseInterface;

function NewValue(const AValue: variant): IBaseValue; overload;
function NewValue: IBaseValue; overload;
function NewImmutableValue(const V: variant): IBaseValue;

function VarCompare(const A, B: variant): TVariantRelationship;

function GetBaseValueCounter: integer;
function GetBaseValueMaxSetQueue: integer;
function GetBaseValueMaxGetQueue: integer;
function GetBaseValueReadSkipCount: integer;
function GetBaseValueNewCounter: integer;
function GetBaseValueDisposeCounter: integer;
function GetBaseValueReadLongestSkip: integer;
function GetBaseValueCollisions: integer;
function GetBaseValueLongestCollision: integer;

implementation

uses {$IFDEF PROF} utimers, {$ENDIF}dateutils, syncobjs, uBaseConsts,
  uBaseObserver, Math;

var
  BaseValueLongestCollision: integer = 0;
  BaseValueCollisions: integer = 0;
  BaseValueCounter: integer = 0;
  BaseValueMaxSetQueue: integer = 0;
  BaseValueMaxGetQueue: integer = 0;
  BaseValueReadSkipCount: integer = 0;
  BaseValueReadLongestSkip: integer = 0;
  BaseValueNewCounter: integer = 0;
  BaseValueDisposeCounter: integer = 0;

{$IFDEF WithCS}
{$IFDEF SingleCS}
var
  FCS: TCriticalSection;
{$ENDIF}
{$ENDIF}

{$IFDEF PROF}
var
  CS: TCriticalSection;
{$ENDIF}

{$IFNDEF WithCS}
const
  SHashError = 'HashConvolution: Invalid HashRefsBits';
  SDeadLockErrorFmt = '[%d]dead lock';

const
{$ifdef Use8bitHash}
  HashRefsBits = 8;
{$endif}
{$ifdef Use16bitHash}
  HashRefsBits = 16;
{$endif}
{$ifdef Use20bitHash}
  HashRefsBits = 20;
{$endif}
  HashRefsSize = 1 shl HashRefsBits;
  HashMask = HashRefsSize - 1;
{$ENDIF}

{$ifndef WithCS}
{$ifdef ExtCahe}
var
  FHashRefs: array[0..HashRefsSize - 1] of int64;
{$endif}
{$endif}

{$ifdef cahe}
{$ifdef ExtCahe}
var
  FHashCahe: array[0..HashRefsSize - 1] of PVariant;
  FCaheSize: integer = 0;
  FCaheMiss: integer = 0;
  FCaheHit: integer = 0;
  FCaheScan: integer = 0;
  FCahePtr: cardinal = 0;
{$endif}
{$endif}


type

  { TBaseImmutableValue }

  TBaseImmutableValue = class(TWeaklyInterfacedObject, IBaseObjectReference, IBaseValue)
  private
    FValue: variant;
    function CompareByIntf(const V: IBaseValue): TVariantRelationship;
    function CompareInternal(const V: variant): TVariantRelationship;
  public
    function GetObject: TObject;
    constructor Create(const AValue: variant);
    destructor Destroy; override;
    function CompareTo(const V: variant): TVariantRelationship;
    function GetValue: variant;
    procedure SetValue(const Value: variant);
    function Clone(const AValue: variant): IBaseValue;
  end;

  { TBaseValue }

  TBaseValue = class(TWeaklyInterfacedObject, IBaseSubject, IBaseValue,
    IBaseVersionCounter, IBaseObjectReference)
  private
    FValue: PVariant;
{$IFNDEF SingleCS}
{$IFDEF WithCS}
    FCS: TCriticalSection;
{$ENDIF}
{$ENDIF}
{$IFDEF PROF}
    Prof_Counter: int64;
    Prof_GetValue: int64;
    Prof_Release: int64;
    Prof_SetValue: int64;
    Prof_TryNew: int64;
{$ENDIF}
    FGetLockRefs: integer;
    FSetLockRefs: integer;
    FSubject: IBaseSubject;
    FVersion: integer;
{$ifndef WithCS}
{$ifndef ExtCahe}
    FHashRefs: array[0..HashRefsSize - 1] of int64;
{$endif}
{$ifdef cahe}
{$ifndef ExtCahe}
    FHashCahe: array[0..HashRefsSize - 1] of PVariant;
    FCaheSize: integer;
    FCaheMiss: integer;
    FCaheHit: integer;
    FCaheScan: integer;
    FCahePtr: cardinal;
{$endif}
{$endif}
    function DecHashRef(const H: cardinal): integer; inline;
    function HashConvolution(const x: pointer): cardinal; inline;
    function IncHashRef(const H: cardinal): integer; inline;
    procedure Release(var FTarget: PVariant);
    procedure TryNew(out ATarget: PVariant);
{$ifdef cahe}
    procedure CaheOrDispose(var P: pvariant; const h: cardinal);
    function TryReuse(out H: cardinal; out P: PVariant): boolean;
    procedure ClearCahe;
{$endif}
{$endif}
    class procedure CheckMax(const Current: integer; var Max: integer); inline;
    function CompareByIntf(const V: IBaseValue): TVariantRelationship;
    function CompareInternal(const V: variant): TVariantRelationship;
    function GetSubject: IBaseSubject;
    procedure OnChange(const NewValue: PVariant);
  public
    constructor Create(const AValue: variant); overload;
    constructor Create; overload;
    destructor Destroy; override;
    function Clone(const AValue: variant): IBaseValue;
    function CompareTo(const V: variant): TVariantRelationship;
    function GetObject: TObject;
    function GetValue: variant;
    function Version: integer;
    procedure Attach(const Observer: IUnknown);
    procedure Detach(const Observer: IUnknown);
    procedure Notify(const Operation: TBaseObservedOperation; const Data: IUnknown);
    procedure SetValue(const AValue: variant);
  published
    property Value: variant read GetValue write SetValue;
  end;

type
  TVarTypes = (
    IsUnknown,
    IsNull,
    IsNumeric,
    IsDate,
    IsStr,
    IsBool,
    IsArray);

  VarCompareFunc = function(const A, B: PVariant): TVariantRelationship;

function cmp(const a, b: PVariant): TVariantRelationship;
begin
  if a = b then
    Result := vrEqual
  else
  if a^ = b^ then
    Result := vrEqual
  else if a^ > b^ then
    Result := vrGreaterThan
  else
    Result := vrLessThan;
end;

function dat(const a, b: PVariant): TVariantRelationship;
var
  I: integer;
begin
  Assert((VarType(a^) = vardate) and (VarType(b^) = vardate));
  I := CompareValue(double(a^), double(b^), OneMillisecond);
  if I = 0 then
    Result := vrEqual
  else
  if I > 0 then
    Result := vrGreaterThan
  else
    Result := vrLessThan;
end;

function num(const a, b: PVariant): TVariantRelationship;

  function comporable(const v1, v2: TVarData; vartype: tvartype): boolean;
  begin
    Result := vartype in [v1.vtype, v2.vtype];
  end;

var
  v1, v2: TVarData;
  I: integer;
begin
  Assert(VarIsNumeric(a^) and VarIsNumeric(b^));
  if a = b then
  begin
    Result := vrEqual;
    exit;
  end;
  v1 := FindVarData(a^)^;
  v2 := FindVarData(b^)^;
  if comporable(v1, v2, varsingle) then
    I := CompareValue(single(a^), single(b^), 1e-6)
  else
  if comporable(v1, v2, vardouble) then
    I := CompareValue(double(a^), double(b^), 1e-13)
  else
  begin
    Result := cmp(a, b);
    exit;
  end;
  if I = 0 then
    Result := vrEqual
  else
  if I > 0 then
    Result := vrGreaterThan
  else
    Result := vrLessThan;
end;

function ndt(const a, b: PVariant): TVariantRelationship;
var
  d: TDateTime;
  I: integer;
begin
  Assert(VarIsNumeric(a^) and (VarType(b^) = vardate));
  d := b^;
  I := CompareValue(double(a^), d, OneMillisecond);
  if I = 0 then
    Result := vrEqual
  else
  if I > 0 then
    Result := vrGreaterThan
  else
    Result := vrLessThan;
end;

function dtn(const a, b: PVariant): TVariantRelationship;
begin
  Result := ndt(b, a);
  if Result = vrGreaterThan then
    Result := vrLessThan
  else
  if Result = vrLessThan then
    Result := vrGreaterThan;
end;

function dst(const a, b: PVariant): TVariantRelationship;
var
  D: TDateTime;
  C: integer;
begin
  Assert((VarType(a^) = vardate) and VarIsStr(b^));
  if TryStrToDateTime(b^, D, BaseFormatSettings) then
  begin
    C := CompareValue(double(a^), double(d), OneSecond);
    if C = 0 then
      Result := vrEqual
    else
    if c > 0 then
      Result := vrGreaterThan
    else
      Result := vrLessThan;
  end
  else
    Result := vrNotEqual;
end;

function std(const a, b: PVariant): TVariantRelationship;
begin
  Result := dst(b, a);
  if Result = vrGreaterThan then
    Result := vrLessThan
  else
  if Result = vrLessThan then
    Result := vrGreaterThan;
end;

function itb(const a: PVariant): integer;
begin
  Assert(VarIsNumeric(a^));
  if a^ = 0 then
    Result := 0
  else
  if a^ = 1 then
    Result := 1
  else
    Result := -1;
end;

function bst(const a, b: PVariant): TVariantRelationship;
var
  V: boolean;
  I: integer;
begin
  Assert(VarIsBool(a^) and VarIsStr(b^));
  if TryStrToBool(a^, v) then
  begin
    I := itb(b);
    if (v = True) and (I = 1) then
      Result := vrEqual
    else
    if (v = False) and (I = 0) then
      Result := vrNotEqual;
  end
  else
    Result := vrNotEqual;
end;

function stb(const a, b: PVariant): TVariantRelationship;
begin
  Result := bst(b, a);
  if Result = vrGreaterThan then
    Result := vrLessThan
  else
  if Result = vrLessThan then
    Result := vrGreaterThan;
end;

function nst(const a, b: PVariant): TVariantRelationship;
var
  I: int64;
  N: extended;
  v: variant;
begin
  Assert(VarIsNumeric(a^) and VarIsStr(b^));
  if TryStrToInt64(b^, I) then
  begin
    v := I;
    Result := num(a, @v);
  end
  else
  if TryStrToFloat(b^, N, BaseFormatSettings) then
  begin
    v := N;
    Result := num(a, @v);
  end
  else
    Result := vrNotEqual;
end;

function stn(const a, b: PVariant): TVariantRelationship;
begin
  Result := nst(b, a);
  if Result = vrGreaterThan then
    Result := vrLessThan
  else
  if Result = vrLessThan then
    Result := vrGreaterThan;
end;

function _ni(const a, b: PVariant): TVariantRelationship;
var
  v1, v2: TVarData;
begin
  if a = b then
  begin
    Result := vrEqual;
    exit;
  end;
  Result := vrNotEqual;
  v1 := FindVarData(a^)^;
  v2 := FindVarData(b^)^;
  if (v1.vType in [varEmpty, varNull]) and (v1.vType = v2.vType) then
    Result := vrEqual
  else if not (v2.vType in [varEmpty, varNull]) and not
    (v1.vType in [varEmpty, varNull]) then
    Assert(False);
end;

function _ne(const a, b: PVariant): TVariantRelationship;
begin
  Assert(assigned(A) and Assigned(b));
  Result := vrNotEqual;
end;

function VarArraySize(const a: PVariant): integer;
var
  d: integer;
  s: integer;
  I: integer;
begin
  Result := 0;
  if Assigned(A) and VarIsArray(a^) then
  begin
    d := VarArrayDimCount(A^);
    for I := 1 to d do
    begin
      s := 1 + VarArrayHighBound(a^, I) - VarArrayLowBound(a^, I);
      Assert(s > 0);
      if I = 1 then
        Result := s
      else
        Result := Result * s;
    end;
  end;
end;

function arr(const a, b: PVariant): TVariantRelationship;
var
  sa, sb: integer;
begin
  Assert(VarIsArray(a^) and VarIsArray(b^));
  if a = b then
    Result := vrEqual
  else
  begin
    //TODO in fture do compare by items
    sa := VarArraySize(a);
    sb := VarArraySize(b);
    if sa > sb then
      Result := vrGreaterThan
    else
    if sa < sb then
      Result := vrLessThan
    else
      Result := vrNotEqual;
  end;
end;

function err(const a, b: PVariant): TVariantRelationship;
begin
  Assert(assigned(A) and Assigned(b));
  Result := vrNotEqual;
end;

function VarTypeN(const A: variant): TVarTypes;
begin
  Assert(Assigned(@A));
  if VarIsNull(A) or VarIsEmpty(A) then
    Result := IsNull
  else
  if VarIsOrdinal(A) or VarIsFloat(A) or VarIsNumeric(A) then
    Result := IsNumeric
  else
  if VarType(A) = vardate then
    Result := IsDate
  else
  if VarIsStr(A) then
    Result := IsStr
  else
  if VarIsBool(A) then
    Result := IsBool
  else
  if VarIsArray(A) then
    Result := IsArray
  else
    Result := IsUnknown;
end;

function NewValue: IBaseValue;
begin
  Result := TBaseValue.Create;
end;

function NewImmutableValue(const V: variant): IBaseValue;
begin
  Result := TBaseImmutableValue.Create(V);
end;

function NewValue(const AValue: variant): IBaseValue;
begin
  Result := TBaseValue.Create(AValue);
end;

function GetBaseValueCounter: integer;
begin
  Result := BaseValueCounter;
end;

function GetBaseValueMaxSetQueue: integer;
begin
  Result := 0;
  InterLockedExchange(Result, BaseValueMaxSetQueue);
end;

function GetBaseValueMaxGetQueue: integer;
begin
  Result := 0;
  InterLockedExchange(Result, BaseValueMaxGetQueue);
end;

function GetBaseValueReadSkipCount: integer;
begin
  Result := 0;
  InterLockedExchange(Result, BaseValueReadSkipCount);
end;

function GetBaseValueNewCounter: integer;
begin
  Result := BaseValueNewCounter;
end;

function GetBaseValueDisposeCounter: integer;
begin
  Result := BaseValueDisposeCounter;
end;

function GetBaseValueReadLongestSkip: integer;
begin
  Result := BaseValueReadLongestSkip;
end;

function GetBaseValueCollisions: integer;
begin
  Result := BaseValueCollisions;
end;

function GetBaseValueLongestCollision: integer;
begin
  Result := BaseValueLongestCollision;
end;

const
  VarComparators: array[TVarTypes] of array[TVarTypes] of VarCompareFunc = (
    {  IsUnknown,  IsNull,  IsNumeric,  IsDate,  IsStr,  IsBool,  IsArray }
    (nil, nil, nil, nil, nil, nil, nil),           //IsUnknown
    (nil, _ni, _ne, _ne, _ne, _ne, _ne),           //IsNull
    (nil, _ne, num, ndt, nst, cmp, nil),           //IsNumeric
    (nil, _ne, dtn, dat, dst, nil, nil),           //IsDate
    (nil, _ne, stn, std, cmp, stb, nil),           //IsStr
    (nil, _ne, cmp, nil, bst, cmp, nil),           //IsBool
    (nil, _ne, nil, nil, nil, nil, arr));          //IsArray

function VarCompare(const A, B: variant): TVariantRelationship;
var
  A_, B_: TVarTypes;
  CompFn: VarCompareFunc;
begin
  Assert(Assigned(@A));
  Assert(Assigned(@B));
  if @A = @B then
  begin
    Result := vrEqual;
    exit;
  end;
  A_ := VarTypeN(A);
  B_ := VarTypeN(B);
  Assert((A_ <> IsUnknown) and (B_ <> IsUnknown));
  CompFn := VarComparators[A_][B_];
  if assigned(CompFn) then
    Result := CompFn(@A, @B)
  else
    Result := err(@A, @B);
end;

{ TBaseImmutableValue }

function TBaseImmutableValue.CompareByIntf(const V: IBaseValue): TVariantRelationship;
begin
  Result := CompareInternal(V.GetValue);
end;

function TBaseImmutableValue.CompareInternal(const V: variant): TVariantRelationship;
begin
  Result := VarCompare(FValue, V);
end;

function TBaseImmutableValue.GetObject: TObject;
begin
  Result := Self;
end;

constructor TBaseImmutableValue.Create(const AValue: variant);
begin
  inherited Create;
  InterLockedIncrement(BaseValueCounter);
  FValue := AValue;
end;

destructor TBaseImmutableValue.Destroy;
begin
  inherited Destroy;
  InterLockedDecrement(BaseValueCounter);
end;

function TBaseImmutableValue.CompareTo(const V: variant): TVariantRelationship;
var
  U: IUnknown;
  O: IBaseValue;
  P: PVarData;
begin
  Assert(Assigned(@V));
  Result := vrNotEqual;
  if VarType(V) = 13 then
  begin
    P := FindVarData(V);
    U := P.vunknown;
    if U.QueryInterface(IBaseValue, O) = S_OK then
      Result := CompareByIntf(O)
    else
      raise EBaseIntfNotSupported.CreateMsg('IBaseValue');
    O := nil;
  end
  else
    Result := CompareInternal(v);
end;

function TBaseImmutableValue.GetValue: variant;
begin
  Result := FValue;
end;

procedure TBaseImmutableValue.SetValue(const Value: variant);
begin
  assert(Assigned(@Value));
  raise EBaseException.Create(SPropReadOnly);
end;

function TBaseImmutableValue.Clone(const AValue: variant): IBaseValue;
begin
  Result := TBaseImmutableValue.Create(AValue);
end;

{ TBaseValue }

function TBaseValue.Version: integer;
begin
  Result := 0;
  InterLockedExchange(Result, FVersion);
end;

procedure TBaseValue.OnChange(const NewValue: PVariant);
begin
  if Assigned(FSubject) and Assigned(NewValue) then
    Notify(booChange, NewImmutableValue(NewValue^));
end;

{$ifndef WithCS}

{$IFNDEF CPU64}
function TBaseValue.HashConvolution(const x: pointer): cardinal;
var
  U32TO16: packed array[0..1] of word absolute x;
  U32TO8: packed array[0..3] of byte absolute x;
begin
  if HashRefsBits = 8 then
    {%H-}Result := U32TO8[0] xor U32TO8[1] xor U32TO8[2] xor U32TO8[3]
  else
  {%H-}if HashRefsBits = 16 then
    {%H-}Result := (U32TO16[0] xor U32TO16[1])
  else
  {%H-}if HashRefsSize > 16 then
    {%H-}Result := ((U32TO16[0] xor U32TO16[1]) and HashMask)
  else
    {%H-}raise EBaseException.Create(SHashError);
  Assert((Result {%H-} >= Low(FHashRefs)) and (Result <= High(FHashRefs)));
end;

{$ENDIF}
{$IFDEF CPU64}
function TBaseValue.HashConvolution(const x: pointer): cardinal;
var
  //U64TO32: packed array[0..1] of DWord absolute x;
  U64TO16: packed array[0..3] of word absolute x;
  U64TO8: packed array[0..7] of byte absolute x;
begin
  if HashRefsBits = 8 then
    Result := U64TO8[0] xor U64TO8[1] xor U64TO8[2] xor U64TO8[3] xor
      U64TO8[4] xor U64TO8[5] xor U64TO8[6] xor U64TO8[7]
  else
  if HashRefsBits = 16 then
    Result := U64TO16[0] xor U64TO16[1] xor U64TO16[2] xor U64TO16[3]
  else
    raise EBaseException.Create(SHashError);
  Assert((Result {%H-} >= Low(FHashRefs)) and (Result <= High(FHashRefs)));
end;

{$ENDIF}

{$ifdef cahe}
procedure TBaseValue.CaheOrDispose(var P: Pvariant; const h: cardinal);
var
  T: PVariant;
begin
  Assert(h = HashConvolution(P));
  T := InterlockedCompareExchange(FHashCahe[H], P, nil);
  if Assigned(T) and (Pointer(P) <> Pointer(T)) then
  begin
    // bad we need dispose this
    Dispose(p);
    p := nil;
    InterLockedIncrement(BaseValueDisposeCounter);
    InterLockedIncrement(FCaheMiss);
  end
  else
  if T = nil then
    InterLockedIncrement(FCaheSize);
end;

{$endif}

procedure TBaseValue.Release(var FTarget: PVariant);
var
  P: PVariant;
  H: cardinal;
  I: integer;
{$IFDEF PROF}
  Prof: int64;
{$ENDIF}
begin
{$IFDEF PROF}
  Prof := timer_GetTick;
{$ENDIF}
  P := InterLockedExchange(FTarget, nil);
  Assert(Assigned(P));
  H := HashConvolution(P);
  I := DecHashRef(H);
{$IFDEF WITHLOG}
  Log(Self, nil, 'TryRelease -> Hash=%x (%d)', [H, I]);
{$ENDIF}
  if I = 0 then
  begin
{$ifdef cahe}
    CaheOrDispose(P, H);
{$else}
    Dispose(p);
    InterLockedIncrement(BaseValueDisposeCounter);
{$endif}
  end
  else
  if I < 0 then
  begin
    I := IncHashRef(H);
    Assert(I <= 0);
  end
  else
  if InterlockedCompareExchange(FTarget, P, nil) <> nil then
    Assert(False);
{$IFDEF PROF}
  Prof := timer_GetTick - Prof;
  CS.Enter;
  Prof_Release := Prof_Release + Prof;
  CS.Leave;
{$ENDIF}
end;

{$ifdef cahe}
function TBaseValue.TryReuse(out H: cardinal; out P: PVariant): boolean;
var
  N: integer;
  hh: cardinal;
  I: integer;
begin
  P := nil;
  Result := False;
  N := 0;
  InterLockedExchange({%H-}N, FCaheSize);
  Assert(N <= HashRefsSize);
  //if N > 0 then
  //  if HashRefsBits < 9 then
  //    N := ((HashRefsSize - N) shr 1) + 1
  //  else
  //    N := 3;
  if N > 0 then
    N := 3;
  while (N > 0) and (not Assigned(P)) do
  begin
    I := 0;
    hh := InterLockedIncrement(FCahePtr) and HashMask;
    Assert((hh {%H-} >= Low(FHashRefs)) and (hh <= High(FHashRefs)));
    InterLockedExchange(I, PInteger(@FHashRefs[hh])^);
    if I = 0 then
    begin
      InterLockedExchange(p, FHashCahe[hh]);
      if Assigned(p) then
      begin
        Assert(hh = HashConvolution(p));
        h := hh;
        Result := True;
        break;
      end;
    end
    else
      Dec(N);
    Dec(N);
    InterLockedIncrement(FCaheScan);
  end;
  assert((N <= 0) or Assigned(P));
end;

{$endif}

procedure TBaseValue.TryNew(out ATarget: PVariant);
var
  P, O: PVariant;
  C, V: integer;
  h: cardinal;
{$IFDEF  DeadLockCheck}
  D: cardinal;
{$ENDIF}
{$IFDEF PROF}
  Prof: int64;
{$ENDIF}
{$ifdef cahe}
  HT: cardinal;
  CH: boolean;
{$endif}
begin
{$IFDEF PROF}
  Prof := timer_GetTick;
{$ENDIF}
{$IFDEF DeadLockCheck}
  D := GetTickCount;
{$ENDIF}
{$ifdef cahe}
  HT := maxLongint;
{$endif}
  C := 0;
  O := nil;
  ATarget := nil;
  H := maxLongint;
  repeat
{$ifdef cahe}
    // TODO: C>0 лучше работает на маленьких до 255
    // TODO: C=0 хорошо работает на больших от 65535
    if C > 0 then
      CH := TryReuse(h, P)
    else
    begin
      P := nil;
      CH := False;
    end;

    if not Assigned(P) then
    begin
      Assert(not CH);
      system.New(P);
      H := HashConvolution(P);
      InterLockedIncrement(BaseValueNewCounter);
    end;
{$else}
    system.New(P);
    InterLockedIncrement(BaseValueNewCounter);
    H := HashConvolution(P);
{$endif}
    if Assigned(O) then
    begin
{$ifdef cahe}
      assert(HT = HashConvolution(O));
      CaheOrDispose(O, HT);
{$else}
      Dispose(O);
      InterLockedIncrement(BaseValueDisposeCounter);
{$endif}
      O := nil;
    end;
{$IFDEF WITHLOG}
    Log(Self, nil, 'TryNew %p -> Hash=%x', [Pointer(P), H]);
{$ENDIF}
    Assert(H = HashConvolution(P));
    V := InterlockedCompareExchange(PInteger(@FHashRefs[H])^, 1, 0);
    if (v = 0) then
    begin
      InterlockedExchange(ATarget, P);
{$ifdef cahe}
      if CH then
        InterLockedIncrement(FCaheHit);
{$endif}
      break;
    end
    else
    if V <> 0 then
    begin
      InterLockedIncrement(BaseValueCollisions);
{$IFDEF WITHLOG}
      Log(Self, nil, ' New -> !!!!!!! Collission %x = %d', [H, V]);
{$ENDIF}
      Inc(C);
      CheckMax(C, BaseValueLongestCollision);
{$IFDEF DeadLockCheck}
      if (GetTickCount - D) > 1000 then
      begin
{$ifdef cahe}
        CaheOrDispose(P, HashConvolution(P));
{$else}
        Dispose(P);
        InterLockedIncrement(BaseValueDisposeCounter);
        P := nil;
{$endif}
        raise EBaseException.CreateFmt(SDeadLockErrorFmt, [1]);
      end;
{$ENDIF}
      if c < 0 then
        raise EBaseException.CreateFmt(SDeadLockErrorFmt, [2]);
      O := P;
{$ifdef cahe}
      HT := h;
{$endif}
      ThreadSwitch;
    end;
  until V = 0;
{$IFDEF PROF}
  Prof := timer_GetTick - Prof;
  CS.Enter;
  Prof_TryNew := Prof_TryNew + Prof;
  CS.Leave;
{$ENDIF}
end;

function TBaseValue.IncHashRef(const H: cardinal): integer;
begin
  Result := InterLockedIncrement(PInteger(@FHashRefs[H])^);
end;

function TBaseValue.DecHashRef(const H: cardinal): integer;
begin
  Result := InterLockedDecrement(PInteger(@FHashRefs[H])^);
end;

{$endif WithCS}

class procedure TBaseValue.CheckMax(const Current: integer; var Max: integer);
var
  K: integer;
begin
  K := 0;
  InterLockedExchange(K, Max);
  if Current > K then
    repeat
      K := InterlockedCompareExchange(Max, Current, K);
    until K <= Current;
end;

procedure TBaseValue.SetValue(const AValue: variant);
var
  N: PVariant;
  O: PVariant;
  I: integer;
{$IFDEF PROF}
  Prof: int64;
{$ENDIF}
begin
{$IFDEF PROF}
  Prof := timer_GetTick;
{$ENDIF}
  Assert(Assigned(@AValue));
{$IFDEF WITHLOG}
  Log(Self, nil, 'SetValue <- "%s"', [Copy(VarToStrDef(AValue, '<undef>'), 1, 20)]);
{$ENDIF}
  if VarTypeN(AValue) = IsUnknown then
    raise EVariantTypeCastError.CreateFmt(SVariantNotSupprtedFmt,
      [Ord(VarType(AValue))]);
  I := InterLockedIncrement(FSetLockRefs);
  CheckMax(I, BaseValueMaxSetQueue);
  OnChange(@AValue);
{$IFNDEF WithCS}
  TryNew(N);
  Assert(Assigned(N));
  N^ := AValue;
  O := InterLockedExchange(FValue, N);
  if Assigned(O) then
    Release(O);
  InterLockedIncrement(FVersion);
{$ELSE}
  N := nil;
  new(N);
  InterLockedIncrement(BaseValueNewCounter);
  Assert(Assigned(N));
  N^ := AValue;
  FCS.Enter;
  begin
    O := InterLockedExchange(FValue, N);
    InterLockedIncrement(FVersion);
    Assert(Assigned(O));
    Dispose(O);
    InterLockedIncrement(BaseValueDisposeCounter);
  end;
  FCS.Leave;
{$ENDIF}
  InterLockedDecrement(FSetLockRefs);
{$IFDEF WITHLOG}
  Log(Self, nil, 'SetValue -> DONE', []);
{$ENDIF}
{$IFDEF PROF}
  Prof := timer_GetTick - Prof;
  CS.Enter;
  Prof_SetValue := Prof_SetValue + Prof;
  CS.Leave;
{$ENDIF}
end;

function TBaseValue.GetValue: variant;
var
  P: PVariant;
  C, I: integer;
{$IFNDEF WithCS}
  J: integer;
  H: cardinal;
  Done: boolean;
{$ENDIF}
{$IFDEF DeadLockCheck}
  D: cardinal;
{$ENDIF}
{$IFDEF PROF}
  Prof: int64;
{$ENDIF}
begin
{$IFDEF DeadLockCheck}
  D := GetTickCount;
{$ENDIF}
{$IFDEF PROF}
  Prof := timer_GetTick;
{$ENDIF}
{$IFDEF WITHLOG}
  Log(Self, nil, 'GetValue -> ?', []);
{$ENDIF}
  C := 0;
  I := InterLockedIncrement(FGetLockRefs);
  CheckMax(I, BaseValueMaxGetQueue);
{$ifndef WithCS}
  Done := False;
  repeat
    P := InterlockedExchange(FValue, nil);
    if Assigned(P) then
    begin
      H := HashConvolution(P);
      J := IncHashRef(H);
      Assert(J > 1);
      if InterlockedCompareExchange(FValue, P, nil) <> nil then
        DecHashRef(H);
      Result := P^;
      Release(P);
      Done := True;
    end
    else
    begin
      Inc(C);
      InterLockedIncrement(BaseValueReadSkipCount);
      {$IFDEF DeadLockCheck}
      if (GetTickCount - D) > 1000 then
        raise EBaseException.CreateFmt(SDeadLockErrorFmt, [3]);
      {$ENDIF}
      if C < 0 then
        raise EBaseException.CreateFmt(SDeadLockErrorFmt, [4]);
      ThreadSwitch;
    end;
  until Done;
{$else}
  FCS.Enter;
  begin
    InterLockedExchange({%H-}P, FValue);
    Assert(Assigned(P));
    Result := P^;
  end;
  FCS.Leave;
{$endif}
  InterLockedDecrement(FGetLockRefs);
  if C > 0 then
    CheckMax(C, BaseValueReadLongestSkip);
{$IFDEF WITHLOG}
  Log(Self, nil, 'GetValue -> "%s"', [Copy(VarToStrDef(Result, '<undef>'), 1, 20)]);
{$ENDIF}
{$IFDEF PROF}
  Prof := timer_GetTick - Prof;
  CS.Enter;
  Prof_GetValue := Prof_GetValue + Prof;
  CS.Leave;
{$ENDIF}
end;

function TBaseValue.GetObject: TObject;
begin
  Result := Self;
end;

function TBaseValue.Clone(const AValue: variant): IBaseValue;
begin
  Result := TBaseValue.Create(AValue);
end;

function TBaseValue.CompareByIntf(const V: IBaseValue): TVariantRelationship;
begin
  Result := CompareInternal(V.GetValue);
end;

function TBaseValue.CompareInternal(const V: variant): TVariantRelationship;
begin
{$IFDEF WithCS}
  FCS.Enter;
  try
    Result := VarCompare(FValue^, V);
  finally
    FCS.Leave;
  end;
{$ELSE}
  //TODO need fast and safe compare
  Result := VarCompare(GetValue(), V);
{$ENDIF}
end;

function TBaseValue.CompareTo(const V: variant): TVariantRelationship;
var
  U: IUnknown;
  O: IBaseValue;
  P: PVarData;
begin
  Assert(Assigned(@V));
  Result := vrNotEqual;
  if VarType(V) = 13 then
  begin
    P := FindVarData(V);
    U := P.vunknown;
    if U.QueryInterface(IBaseValue, O) = S_OK then
      Result := CompareByIntf(O)
    else
      raise EBaseIntfNotSupported.CreateMsg('IBaseValue');
    O := nil;
  end
  else
    Result := CompareInternal(v);
end;

function TBaseValue.GetSubject: IBaseSubject;
begin
  if not Assigned(FSubject) then
    FSubject := TBaseSubject.Create(Self);
  Result := FSubject;
end;

procedure TBaseValue.Attach(const Observer: IUnknown);
begin
  GetSubject.Attach(Observer);
end;

procedure TBaseValue.Detach(const Observer: IUnknown);
begin
  if Assigned(FSubject) then
    FSubject.Detach(Observer);
end;

procedure TBaseValue.Notify(const Operation: TBaseObservedOperation;
  const Data: IUnknown);
begin
  if Assigned(FSubject) then
    GetSubject.Notify(Operation, Data);
end;

{$ifdef ExtCahe}
{$ifdef cahe}
procedure ClearCahe_;
var
  I: integer;
begin
  for I := 0 to high(FHashCahe) do
    if Assigned(FHashCahe[I]) and (FHashRefs[I] = 0) then
    begin
      Dispose(FHashCahe[I]);
      InterLockedIncrement(BaseValueDisposeCounter);
      FHashCahe[I] := nil;
      Dec(FCaheSize);
    end;
end;

{$endif}
{$endif}

{$ifdef cahe}
procedure TBaseValue.ClearCahe;
{$ifndef ExtCahe}
var
  I: integer;
{$endif}
begin
{$ifdef ExtCahe}
  ClearCahe_;
{$else}
  for I := 0 to high(FHashCahe) do
  begin
    if Assigned(FHashCahe[I]) and (FHashCahe[I] <> FValue) then
    begin
      Dispose(FHashCahe[I]);
      InterLockedIncrement(BaseValueDisposeCounter);
    end;
    FHashCahe[I] := nil;
    Dec(FCaheSize);
  end;
{$endif}
end;

{$endif}

destructor TBaseValue.Destroy;
begin
{$IFNDEF WithCS}
{$ifdef cahe}
{$ifdef WITHLOG}
  Writeln('* EffectiveCaheSize ', FCaheSize,
    format(' %d%%', [Round(FCaheSize / HashRefsSize * 100)]));
{$endif}
  if Assigned(FValue) then
    Release(FValue);
  Assert(not Assigned(FValue));
  ClearCahe;
{$endif}
  if Assigned(FValue) then
  begin
    Dispose(FValue);
    InterLockedIncrement(BaseValueDisposeCounter);
  end;
{$ELSE}
  Dispose(FValue);
  InterLockedIncrement(BaseValueDisposeCounter);
{$ENDIF}
{$ifdef cahe}
{$ifndef withCS}
{$ifdef WITHLOG}
  Writeln('* CaheMiss ', FCaheMiss);
  Writeln('* CaheHit ', FCaheHit);
  Writeln('* CaheScan ', FCaheScan);
{$endif}
{$endif}
{$endif}
{$ifdef WithCS}
{$ifndef SingleCS}
  FreeAndNil(FCS);
{$endif}
{$endif}
  inherited Destroy;
  InterLockedDecrement(BaseValueCounter);
{$IFDEF PROF}
  Writeln(format(' Prof_SetValue %.2f', [tickToTime(Prof_SetValue)]));
  Writeln(format(' Prof_GetValue %.2f', [tickToTime(Prof_GetValue)]));
  Writeln(format(' Prof_Release %.2f', [tickToTime(Prof_Release)]));
  Writeln(format(' Prof_TryNew %.2f', [tickToTime(Prof_TryNew)]));
{$ENDIF}
end;

constructor TBaseValue.Create;
begin
  inherited Create;
  InterLockedIncrement(BaseValueCounter);
{$ifdef WithCS}
{$ifndef SingleCS}
  FCS := TCriticalSection.Create;
{$endif}
{$endif}
{$ifndef WithCS}
  TryNew(FValue);
{$else}
  new(FValue);
  InterLockedIncrement(BaseValueNewCounter);
{$endif}
  FValue^ := Unassigned;
end;

constructor TBaseValue.Create(const AValue: variant);
begin
  inherited Create;
  InterLockedIncrement(BaseValueCounter);
{$ifdef WithCS}
{$ifndef SingleCS}
  FCS := TCriticalSection.Create;
{$endif}
{$endif}
{$ifndef WithCS}
  TryNew(FValue);
{$else}
  new(FValue);
  InterLockedIncrement(BaseValueNewCounter);
{$endif}
  FValue^ := AValue;
end;

{$ifdef cahe}
{$ifdef ExtCahe}

var
  I: integer;
{$endif}
{$endif}

initialization
{$IFDEF WithCS}
{$IFDEF SingleCS}
  FCS := TCriticalSection.Create;
{$ENDIF}
{$ENDIF}
{$IFDEF PROF}
  CS := TCriticalSection.Create;
{$ENDIF}
{$ifdef WithCS}
{$ifdef SingleCS}
  FCS := TCriticalSection.Create;
{$endif}
{$endif}


{$ifdef cahe}
{$ifdef ExtCahe}
  for I := 0 to HashRefsSize - 1 do
    FHashCahe[I] := nil;
{$endif}
{$endif}

finalization
{$IFDEF WithCS}
{$IFDEF SingleCS}
  FreeAndNil(FCS);
{$ENDIF}
{$ENDIF}
{$IFDEF PROF}
  FreeAndNil(CS);
{$ENDIF}
end.
