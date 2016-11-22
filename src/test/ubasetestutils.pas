unit uBaseTestUtils;

{$mode delphi}

interface

uses
  Classes, SysUtils;

function Murmur2(const S: ansistring; const Seed: longword = $9747b28c): longword;
function AddMurmur2(const S: string): string;
function TestMurmur2(const S: string): boolean;
function TestData: variant;
function Validate(const V: variant): boolean;

function TestString(const MaxLen: integer = 1000; const MinLen: integer = 0): variant;
function TestTime: variant;
function TestInteger: variant;
function TestDouble: variant;

function GetSafeObjName(const T: TObject): string;
function GetSafeIntfName(const I: IUnknown): string;


implementation

{$OVERFLOWCHECKS OFF}

uses Variants, uBaseInterface, Math, DateUtils;

function Murmur2(const S: ansistring; const Seed: longword = $9747b28c): longword;
var
  h: longword;
  len: longword;
  K: longword;
  Data: integer;
const
  // 'm' and 'r' are mixing constants generated offline.
  // They're not really 'magic', they just happen to work well.
  M = $5bd1e995;
  r = 24;
begin
  len := Length(S);

  //The default seed, $9747b28c, is from the original C library

  // Initialize the hash to a 'random' value
  h := seed xor len;

  // Mix 4 bytes at a time into the hash
  Data := 1;

  while (len >= 4) do
  begin
    K := PLongWord(@S[Data])^;

    K := K * M;
    K := K xor (K shr r);
    K := K * M;

    h := h * M;
    h := h xor K;

    Data := Data + 4;
    len := len - 4;
  end;

    {   Handle the last few bytes of the input array
            S: ... $69 $18 $2f
    }
  Assert(len <= 3);
  if len = 3 then
    h := h xor (longword(s[Data + 2]) shl 16);
  if len >= 2 then
    h := h xor (longword(s[Data + 1]) shl 8);
  if len >= 1 then
  begin
    h := h xor (longword(s[Data]));
    h := h * M;
  end;

  // Do a few final mixes of the hash to ensure the last few
  // bytes are well-incorporated.
  h := h xor (h shr 13);
  h := h * M;
  h := h xor (h shr 15);

  Result := h;
end;

function AddMurmur2(const S: string): string;
var
  L: longword;
  X: string;
begin
  L := Murmur2(S);
  X := format('@$%x', [L]);
  Result := S + X;
end;

function TestString(const MaxLen: integer; const MinLen: integer): variant;
const
  alphabet: string = '1234567890abcdefghijklmoprstuvxyz ';
var
  I: integer;
  S: string;
begin
  Assert(MinLen <= MaxLen);
  SetLength(S, MinLen + Random(MaxLen - MinLen));
  for I := 1 to Length(S) do
    S[I] := alphabet[1 + Random(Length(alphabet) - 1)];
  Result := AddMurmur2(S);
end;

function TestTime: variant;
begin
  Result := now();
end;

function TestInteger: variant;
begin
  Result := -Random(MaxInt - 1);
end;

function TestDouble: variant;
begin
  Result := Random(1000) * pi;
end;

function TestMurmur2(const S: string): boolean;
var
  I: integer;
  X: string;
  C: int64;
  L: longword;
begin
  I := Pos('@', S);
  if I > 0 then
  begin
    X := Copy(S, I + 1, Length(S));
    C := 0;
    if TryStrToInt64(X, C) then
    begin
      X := Copy(S, 1, I - 1);
      L := Murmur2(X);
      Result := C = L;
    end;
  end;
end;

function TestData: variant;
var
  K: integer;
begin
  K := Random(5);
  case K of
    0: Result := TestDouble;
    1: Result := TestString;
    2: Result := TestInteger;
    3: Result := TestData;
    4: Result := True;
  end;
end;

function Validate(const V: variant): boolean;
var
  //  C: int64;
  //  L: longword;
  I: integer;
  //  S: string;
  //  X: string;
  D: TDateTime;
  F, FF: extended;
begin
  Result := False;
  if VarIsBool(v) then
    Result := V
  else
  if VarType(V) = vardate then
  begin
    D := V;
    Result := (SecondsBetween(now, D) < 60);
  end
  else
  if VarIsFloat(V) then
  begin
    F := V;
    F := F / pi;
    FF := RoundTo(F, 0);
    F := F - FF; //frac(F);
    Result := CompareValue(F, 0.0, 1e-6) = EqualsValue;
  end
  else
  if VarIsOrdinal(V) then
  begin
    I := V;
    Result := I <= 0;
  end
  else
  if VarIsStr(V) then
    Result := TestMurmur2(V);
end;


function GetSafeObjName(const T: TObject): string;
begin
  if Assigned(T) then
    Result := format('<%s@%p>', [T.ClassName, pointer(T)])
  else
    Result := '<nil>';
end;

function GetSafeIntfName(const I: IUnknown): string;
var
  T: TObject;
begin
  T := SafeBaseObjectReference(I);
  if Assigned(T) then
    Result := GetSafeObjName(T)
  else
    Result := format('<%p>', [pointer(I)]);
end;


end.
