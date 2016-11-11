unit uBaseAppUtils;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TVarArray = array of variant;


function SplitParams(const S: string): TStrings;
function ParseParams(const S: string): TStrings;
procedure ParamStrToArray(const S: string; var o: TVarArray);

implementation

uses variants;

function SplitParams(const S: string): TStrings;
const
  Del = [' ', ';', ',', #0..#31];
  Par = ['(', ')'];
var
  I, J: integer;
begin
  Result := TStringList.Create;
  I := 1;
  while I <= Length(S) do
  begin
    while (I <= Length(S)) and (S[I] in Del) do
      Inc(I);
    while (I <= Length(S)) and (S[I] in Par) do
    begin
      Result.Add(S[I]);
      Inc(I);
    end;
    J := I;
    while (I <= Length(S)) and (not (S[I] in Par + Del)) do
      Inc(I);
    if I > J then
      Result.Add(Copy(S, J, I - J));
  end;
end;

function ParseParams(const S: string): TStrings;
var
  T, P: TStrings;
  Name: string;
  Params: TStringList;
  I: integer;
begin
  Params := nil;
  Name := '';
  Result := TStringList.Create;
  try
    P := SplitParams(S);
    try
      for I := 0 to P.Count - 1 do
      begin
        Assert(True);
        if P.Strings[I] = '(' then
        begin
          if Length(Name) = 0 then
            raise Exception.Create('function name expected');
          if Assigned(Params) then
            raise Exception.Create('sub options not allowed');
          Params := TStringList.Create;
        end
        else
        if P.Strings[I] = ')' then
        begin
          if not Assigned(Params) then
            raise Exception.Create('unexpected close brace');

          if Params.Count > 0 then
          begin
            T := TStringList.Create;
            T.Values[Name] := Params.CommaText;
            Result.AddStrings(T);
            T.Free;
          end
          else
          begin
            Result.Add(Name);
          end;
          Name := '';
          FreeAndNil(Params);
        end
        else
        if Assigned(Params) then
          Params.Add(P.Strings[I])
        else
        begin
          if Length(Name) > 0 then
            Result.Add(Name);

          Name := P.Strings[I];
        end;
      end;
      if Length(Name) > 0 then
        Result.Add(Name);
      if Assigned(Params) then
        raise Exception.Create('syntax error');
    finally
      FreeAndNil(Params);
      P.Free;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure ParamStrToArray(const S: string; var o: TVarArray);
var
  t: TStrings;
  J, I: integer;
  q: string;
begin
  t := TStringList.Create;
  try
    t.CommaText := s;
    setlength(o, t.Count);
    J := 0;
    for I := 0 to t.Count - 1 do
    begin
      q := trim(t.Strings[I]);
      if Length(Q) > 0 then
      begin
        o[J] := q;
        Inc(J);
      end;
    end;
    SetLength(o, J);
  finally
    t.Free;
  end;
end;

end.


