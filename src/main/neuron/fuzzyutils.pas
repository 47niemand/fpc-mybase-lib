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

unit FuzzyUtils;

{$mode delphi}

interface

uses
  Classes, SysUtils, NNWorkTypes;

type
  TFuzzyGetter = function(const PixelPtr: pointer): DType;
  TFuzzySetter = procedure(const PixelPtr: pointer; const Value: DType);

function FuzzyByte(const a: DType): byte;
function FuzzyTrue(const dither: DType = 0.0): DType;
function FuzzyFalse(const dither: DType = 0.0): DType;
function FuzzyNot(const Value: DType): DType;

function FuzzyByteGetter(const PixelPtr: pointer): DType;
procedure FuzzyByteSetter(const PixelPtr: pointer; const Value: DType);

function FuzzyRandom(const AValue: boolean; APropobility: double;
  const dither: DType = 0.0): DType;

function FuzzyNorm(const Value: DType): DType;

implementation

uses Math;

const
  ephsilon = 1e-8;
  fuzzyceli = 0.9;
  fuzzyflor = 0.1;

function FuzzyRandom(const AValue: boolean; APropobility: double;
  const dither: DType): DType;

  function _Inv(const A: boolean): DType;
  begin
    if A xor (not AValue) then
      Result := FuzzyTrue(dither)
    else
      Result := FuzzyFalse(dither);
  end;

begin
  if APropobility <= ephsilon then
    Result := _Inv(False)
  else
  if APropobility >= 1 - ephsilon then
    Result := _Inv(True)
  else
  if SameValue(APropobility, 0.5, ephsilon) then
  begin
    if Random(2) = 0 then
      Result := _Inv(False)
    else
      Result := _Inv(True);
  end
  else
  if APropobility < 0.5 then
  begin
    if Random < APropobility then
      Result := _Inv(True)
    else
      Result := _Inv(False);
  end
  else
  if Random > APropobility then
    Result := _Inv(False)
  else
    Result := _Inv(True);
end;

function FuzzyNorm(const Value: DType): DType; inline;
begin
  if Value < 0 then
    Result := 0
  else
  if Value > 1 then
    Result := 1
  else
    Result := Value;
end;

function FuzzyTrue(const dither: DType): DType;
begin
  Result := fuzzyceli;
  if dither <> 0 then
    if Random(2) = 0 then
      Result := Result + Random * dither
    else
      Result := Result - Random * dither;
  Result := FuzzyNorm(Result);
end;

function FuzzyFalse(const dither: DType): DType;
begin
  Result := fuzzyflor;
  if not SameValue(0.0, dither, ephsilon) then
    if Random(2) = 0 then
      Result := Result + Random * dither
    else
      Result := Result - Random * dither;
  Result := FuzzyNorm(Result);
end;

function FuzzyNot(const Value: DType): DType;
begin
  Result := 1.0 - FuzzyNorm(Value);
end;

function FuzzyByteGetter(const PixelPtr: pointer): DType;
begin
  Result := PByte(PixelPtr)^ / 255.0;
end;

procedure FuzzyByteSetter(const PixelPtr: pointer; const Value: DType);
begin
  PByte(PixelPtr)^ := FuzzyByte(Value);
end;

function FuzzyByte(const a: DType): byte;
begin
  Result := round(max(0, Min(1, a)) * 255);
end;

end.
