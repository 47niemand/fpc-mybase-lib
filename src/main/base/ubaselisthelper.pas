unit uBaseListHelper;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type

  TBaseListOperation = (bloAddItem, bloChange, bloDeleteItem, bloCustom, bloFree);

  { EBaseListIdxOutOfBounds }

  EBaseListIdxOutOfBounds = class(EListError)
    constructor Create(const Idx: integer);
  end;

  TLeftComparison<T> = function(const Left: T; var Value): integer;

  { TListHelper this generic class provide some static method for searching
    in lists  }

  TListHelper<T> = class
  public
    class function BinarySearch(const Instance: TList; var Value;
      out FoundIndex: integer; Comparison: TLeftComparison<T>;
      Index, Count: integer): boolean; overload;
    class function BinarySearch(const Instance: TList; var Value;
      out FoundIndex: integer; Comparison: TLeftComparison<T>): boolean; overload;
    class function Contains(const Instance: TList; var Value;
      Comparison: TLeftComparison<T>): boolean;
    class function IndexOf(const Instance: TList; var Value;
      Comparison: TLeftComparison<T>): integer; overload;
    class function LastIndexOf(const Instance: TList; var Value;
      Comparison: TLeftComparison<T>): integer;
    class function IndexOf(const Instance: TList; var Value;
      Comparison: TLeftComparison<T>; Index, Count: integer): integer; overload;
  end;


implementation

{ TListHelper<T> }

class function TListHelper<T>.BinarySearch(const Instance: TList;
  var Value; out FoundIndex: integer; Comparison: TLeftComparison<T>;
  Index, Count: integer): boolean;
var
  L, H: integer;
  mid, cmp: integer;
begin
  Assert(Assigned(@Value));
  Assert(Assigned(Comparison));
  Assert(Assigned(Instance));
  Result := False;
  L := Index;
  H := Index + Count - 1;
  while L <= H do
  begin
    mid := L + (H - L) shr 1;
    cmp := Comparison(T(Instance.Items[mid]), Value);
    if cmp < 0 then
      L := mid + 1
    else
    begin
      H := mid - 1;
      if cmp = 0 then
        Result := True;
    end;
  end;
  FoundIndex := L;
end;

class function TListHelper<T>.BinarySearch(const Instance: TList;
  var Value; out FoundIndex: integer; Comparison: TLeftComparison<T>): boolean;
begin
  Result := BinarySearch(Instance, Value, FoundIndex, Comparison, 0, Instance.Count);
end;

class function TListHelper<T>.Contains(const Instance: TList; var Value;
  Comparison: TLeftComparison<T>): boolean;
begin
  Result := IndexOf(Instance, Value, Comparison) >= 0;
end;

class function TListHelper<T>.IndexOf(const Instance: TList; var Value;
  Comparison: TLeftComparison<T>): integer;
begin
  Result := IndexOf(Instance, Value, Comparison, 0, Instance.Count);
end;

class function TListHelper<T>.LastIndexOf(const Instance: TList;
  var Value; Comparison: TLeftComparison<T>): integer;
var
  I: integer;
begin
  for I := Instance.Count - 1 downto 0 do
    if Comparison(T(Instance.Items[I]), Value) = 0 then
      Exit(I);
  Result := -1;
end;

class function TListHelper<T>.IndexOf(const Instance: TList; var Value;
  Comparison: TLeftComparison<T>; Index, Count: integer): integer;
var
  I: integer;
begin
  for I := Index to Index + Count - 1 do
    if Comparison(T(Instance.Items[I]), Value) = 0 then
      Exit(I);
  Result := -1;
end;

{ EBaseListIdxOutOfBounds }

constructor EBaseListIdxOutOfBounds.Create(const Idx: integer);
begin
  inherited CreateFmt('List Index (%d) out of bounds', [Idx]);
end;


end.
