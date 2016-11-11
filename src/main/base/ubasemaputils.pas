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

unit uBaseMapUtils;

{$mode delphi}
{$i BaseOptions.inc}

interface

uses
  Classes, SysUtils, uBaseInterface, uBaseMap, uVariantUtils;

type

  { IBaseMapHelper }

  IBaseMapHelper = interface
    function Clone: IBaseMap;
    function CompareTo(P: IBaseMap): integer;
    function GetAsString: string;
    function Map: IBaseMap;
    procedure LoadFromStrings(const T: TStrings);
    procedure SaveToStrings(var T: TStrings);
    procedure SetAsString(const S: string);
    property AsString: string read GetAsString write SetAsString;
  end;

  IBaseMapBuilder = interface
    function Add(const Entry: IBaseEntry): IBaseMapBuilder;
    function Append(const AMap: IBaseMap): IBaseMapBuilder;
    function Attach(const Observer: IUnknown): IBaseMapBuilder;
    function Build: IBaseMap;
    function Change(const AName: string; const AValue: variant): IBaseMapBuilder;
  end;

function NewMapBuilder: IBaseMapBuilder; overload;
function NewMapBuilder(const AMap: IBaseMap): IBaseMapBuilder; overload;
function NewMapHelper(const AMap: IBaseMap): IBaseMapHelper;


function GetBaseMapBuilderCounter: integer;
function GetBaseMapHelperCounter: integer;

implementation

uses
  uBaseConsts, uBaseValue, dateutils, Variants;

var
  BaseMapBuilderCounter: integer = 0;
  BaseMapHelperCounter: integer = 0;


type

  { TBaseMapBuilder }

  TBaseMapBuilder = class(TInterfacedObject, IBaseMapBuilder)
  private
    FMap: IBaseMap;
  public
    class function NewMapBuilder: IBaseMapBuilder; overload;
    class function NewMapBuilder(const AMap: IBaseMap): IBaseMapBuilder; overload;
    constructor Create; overload;
    constructor Create(const AMap: IBaseMap); overload;
    destructor Destroy; override;
    function Add(const Entry: IBaseEntry): IBaseMapBuilder;
    function Append(const AMap: IBaseMap): IBaseMapBuilder;
    function Attach(const Observer: IUnknown): IBaseMapBuilder;
    function Build: IBaseMap;
    function Change(const AName: string; const AValue: variant): IBaseMapBuilder;
  end;

  { TBaseMapHelper }

  TBaseMapHelper = class(TInterfacedObject, IBaseMapHelper)
  private
    FDelimiter: char;
    FMap: IBaseMap;
    FQuoteChar: char;
    procedure SetDelimiter(AValue: char);
    procedure SetQuoteChar(AValue: char);
  protected
    function GetAsString: string;
    procedure SetAsString(const S: string);
    property Delimiter: char read FDelimiter write SetDelimiter default ',';
    property QuoteChar: char read FQuoteChar write SetQuoteChar default '"';
  public
    constructor Create(AParam: IBaseMap);
    destructor Destroy; override;
    function Clone: IBaseMap;
    function CompareTo(P: IBaseMap): integer;
    function Map: IBaseMap;
    procedure AfterConstruction; override;
    procedure LoadFromStrings(const T: TStrings);
    procedure SaveToStrings(var T: TStrings);
    property AsString: string read GetAsString write SetAsString;
  end;

function NewMapBuilder(const AMap: IBaseMap): IBaseMapBuilder;
begin
  Result := TBaseMapBuilder.NewMapBuilder(AMap);
end;

function NewMapHelper(const AMap: IBaseMap): IBaseMapHelper;
begin
  Result := TBaseMapHelper.Create(AMap);
end;

function NewMapBuilder: IBaseMapBuilder;
begin
  Result := TBaseMapBuilder.NewMapBuilder;
end;

function GetBaseMapBuilderCounter: integer;
begin
  Result := BaseMapBuilderCounter;
end;

function GetBaseMapHelperCounter: integer;
begin
  Result := BaseMapHelperCounter;
end;

{ TBaseMapBuilder }

constructor TBaseMapBuilder.Create;
begin
  inherited Create;
  InterLockedIncrement(BaseMapBuilderCounter);
  FMap := NewMap;
end;

constructor TBaseMapBuilder.Create(const AMap: IBaseMap);
begin
  inherited Create;
  InterLockedIncrement(BaseMapBuilderCounter);
  FMap := AMap;
end;

class function TBaseMapBuilder.NewMapBuilder: IBaseMapBuilder;
begin
  Result := TBaseMapBuilder.Create;
end;

class function TBaseMapBuilder.NewMapBuilder(const AMap: IBaseMap): IBaseMapBuilder;
begin
  Result := TBaseMapBuilder.Create(AMap);
end;

function TBaseMapBuilder.Build: IBaseMap;
begin
  Result := FMap;
  FMap := nil;
end;

function TBaseMapBuilder.Add(const Entry: IBaseEntry): IBaseMapBuilder;
begin
  FMap.Values[Entry.Name] := Entry.Value;
  Result := Self;
end;

function TBaseMapBuilder.Append(const AMap: IBaseMap): IBaseMapBuilder;
begin
  FMap.Append(AMap);
  Result := Self;
end;

function TBaseMapBuilder.Change(const AName: string;
  const AValue: variant): IBaseMapBuilder;
begin
  FMap.Values[AName] := AValue;
  Result := Self;
end;

function TBaseMapBuilder.Attach(const Observer: IUnknown): IBaseMapBuilder;
var
  S: IBaseSubject;
begin
  if FMap.QueryInterface(IBaseSubject, S) = S_OK then
    S.Attach(Observer);
  Result := Self;
end;

destructor TBaseMapBuilder.Destroy;
begin
  inherited Destroy;
  InterLockedDecrement(BaseMapBuilderCounter);
end;

{ TBaseMapHelper }

procedure TBaseMapHelper.SetDelimiter(AValue: char);
begin
  if FDelimiter = AValue then
    Exit;
  FDelimiter := AValue;
end;

procedure TBaseMapHelper.SetQuoteChar(AValue: char);
begin
  if FQuoteChar = AValue then
    Exit;
  FQuoteChar := AValue;
end;

function TBaseMapHelper.GetAsString: string;
var
  T: TStrings;
begin
  T := TStringList.Create;
  try
    SaveToStrings(T);
    T.Delimiter := FDelimiter;
    T.QuoteChar := QuoteChar;
    Result := T.DelimitedText;
  finally
    T.Free;
  end;
end;

procedure TBaseMapHelper.SetAsString(const S: string);
var
  T: TStrings;
begin
  T := TStringList.Create;
  try
    T.Delimiter := FDelimiter;
    T.QuoteChar := FQuoteChar;
    T.DelimitedText := S;
    LoadFromStrings(T);
  finally
    T.Free;
  end;
end;


constructor TBaseMapHelper.Create(AParam: IBaseMap);
begin
  Delimiter := ',';
  QuoteChar := '"';
  FMap := AParam;
end;

procedure TBaseMapHelper.AfterConstruction;
begin
  inherited AfterConstruction;
  InterLockedIncrement(BaseMapHelperCounter);
end;

destructor TBaseMapHelper.Destroy;
begin
  inherited Destroy;
  InterLockedDecrement(BaseMapHelperCounter);
end;

function TBaseMapHelper.Map: IBaseMap;
begin
  Result := FMap;
end;

procedure TBaseMapHelper.SaveToStrings(var T: TStrings);
var
  E: IBaseEntry;
begin
  Assert(Assigned(T));
  for E in FMap do
    T.Values[E.Name] := BaseVarToStr(E.Value, QuoteChar);
end;

procedure TBaseMapHelper.LoadFromStrings(const T: TStrings);
var
  I: integer;
  N, V: string;
begin
  Assert(Assigned(T));
  FMap.Clear;
  for I := 0 to T.Count - 1 do
  begin
    N := T.Names[I];
    if Length(N) > 0 then
    begin
      V := T.ValueFromIndex[I];
      FMap.Values[N] := BaseVarToStr(V, QuoteChar);
    end;
  end;
end;

function TBaseMapHelper.Clone: IBaseMap;
begin
  Result := NewMap;
  Result.Append(Map);
end;

function TBaseMapHelper.CompareTo(P: IBaseMap): integer;
var
  R: TVariantRelationship;
  N: IBaseEntry;
begin
  Result := FMap.Size - P.Size;
  if Result = 0 then
  begin
    for N in FMap do
      try
        R := VarCompare(N.Value, P.Values[N.Name]);
        if R <> vrEqual then
        begin
          if R = vrGreaterThan then
            Result := 1
          else
          if R = vrLessThan then
            Result := -1
          else
            Result := MaxInt;
          exit;
        end;
      except
        Result := MaxInt;
        exit;
      end;
    Result := 0;
  end;
end;

end.
