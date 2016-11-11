unit uVariantUtils;

{$mode delphi}

interface

uses
  Classes, SysUtils, uBaseConsts;

function BaseVarToStr(const AVar: variant; const AQuoteChar: char = '"'): string;
function BaseStrToVar(const AStr: string; const AQuoteChar: char = '"'): variant;

implementation

uses variants;

const
  WhiteSpace = [#0..' '];
  SNull = 'Null';
  SUnassigned = 'Unassigned';


function BaseVarToStr(const AVar: variant; const AQuoteChar: char): string;
var
  L: int64;
{$ifdef FPC_HAS_TYPE_EXTENDED}
  F: extended;
{$else}
  F: double;
{$endif}
  D: TDateTime;

begin
  if VarIsEmpty(AVar) then
    Result := SNull
  else
  if VarIsNull(AVar) then
    Result := SUnassigned
  else
  if VarIsBool(AVar) then
    Result := BoolToStr(AVar, True)
  else
  if VarIsOrdinal(AVar) then
  begin
    L := AVar;
    Result := IntToStr(L);
  end
  else
  if VarIsNumeric(AVar) then
  begin
    F := AVar;
    Result := FloatToStrF(F, ffGeneral, -1, 0, BaseFormatSettings);
  end
  else
  if VarType(AVar) = vardate then
  begin
    D := AVar;
    Result := DateTimeToStr(D, BaseFormatSettings);
  end
  else
  if VarIsStr(AVar) then
    Result := AnsiQuotedStr(AVar, AQuoteChar)
  else
    raise EVariantTypeCastError.CreateFmt('unsupported format %d', [VarType(AVar)]);
end;

function BaseStrToVar(const AStr: string; const AQuoteChar: char): variant;

  function IsQuoted(const S: string): boolean;
  var
    I, J: integer;
    B: boolean;
  begin
    J := Length(S);
    I := 1;
    Result := False;
    B := Length(S) > 1;
    while B and (I < Length(S)) do
    begin
      B := S[I] in WhiteSpace;
      if B then
        Inc(I);
    end;
    B := I < Length(S);
    if B then
      while B and (J > I) do
      begin
        B := S[J] in WhiteSpace;
        if B then
          Dec(J);
      end;
    if J > I then
      Result := (S[J] = AQuoteChar) and (S[I] = AQuoteChar);
  end;

  function TrimBSameText(const A, B: string): boolean;
  begin
    Result := AnsiCompareText(A, trim(B)) = 0;
  end;

var
  B: boolean;
  D: TDateTime;
  E: extended;
  I: integer;
  L: int64;

begin
  if IsQuoted(AStr) then
    Result := AnsiDequotedStr(AStr, AQuoteChar)
  else
  if TrimBSameText(SNull, AStr) then
    Result := Null
  else
  if TrimBSameText(SUnassigned, AStr) then
    Result := Unassigned
  else
  if TryStrToInt(AStr, I) then
    Result := I
  else
  if TryStrToInt64(AStr, L) then
    Result := L
  else
  if TryStrToFloat(AStr, E, BaseFormatSettings) then
    Result := E
  else
  if TryStrToBool(AStr, B) then
    Result := B
  else if TryStrToDateTime(AStr, D, BaseFormatSettings) then
    Result := D;
end;


end.

