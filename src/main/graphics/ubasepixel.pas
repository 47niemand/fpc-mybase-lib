unit uBasePixel;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type

  // some getter/setter types

  TPixelGetter<T> = function(const P: pointer): T;
  TPixelSetter<T> = procedure(const P: pointer; const AValue: T);
  TPixelGetterMethod<T> = function(const P: pointer): T of object;
  TPixelSetterMethod<T> = procedure(const P: pointer; const AValue: T) of object;

  TPixelGetterByte = TPixelGetter<byte>;
  TPixelSetterByte = TPixelSetter<byte>;
  TPixelGetterByteMethod = TPixelGetterMethod<byte>;
  TPixelSetterByteMethod = TPixelGetterMethod<byte>;

  TPixelGetterInt = TPixelGetter<integer>;
  TPixelSetterInt = TPixelSetter<integer>;
  TPixelGetterIntMethod = TPixelGetterMethod<integer>;
  TPixelSetterIntMethod = TPixelGetterMethod<integer>;

  // generic pixel access interface
  { IPixel }

  IPixel<T> = interface
    function Getter(const P: pointer): T;
    function PixelSize: integer;
    procedure Setter(const P: pointer; const AValue: T);
    property Pixel[P: pointer]: T read Getter write Setter;
  end;

  // generic pixel access interface implementation
  { TPixel<T> }

  TPixel<T> = class(TInterfacedObject, IPixel<T>)
  public
    function Getter(const P: pointer): T;
    function PixelSize: integer;
    procedure Setter(const P: pointer; const AValue: T);
  end;

  // some basics implementation

  IPixelByte = IPixel<byte>;
  TPixelByte = TPixel<byte>;

  IPixelInt = IPixel<integer>;
  TPixelInt = TPixel<integer>;


function GetterPixelByte(const P: pointer): byte;
procedure SetterPixelByte(const P: pointer; const AValue: byte);

function GetterPixelInt(const P: pointer): integer;
procedure SetterPixelInt(const P: pointer; const AValue: integer);


implementation

{ TPixel<T> }

function TPixel<T>.Getter(const P: pointer): T;
type
  PT = ^T;
begin
  Result := PT(P)^;
end;

function TPixel<T>.PixelSize: integer;
begin
  Result := SizeOf(T);
end;

procedure TPixel<T>.Setter(const P: pointer; const AValue: T);
type
  PT = ^T;
begin
  PT(P)^ := AValue;
end;

function GetterPixelByte(const P: pointer): byte;
begin
  Result := PByte(P)^;
end;

procedure SetterPixelByte(const P: pointer; const AValue: byte);
begin
  PByte(P)^ := AValue;
end;

function GetterPixelInt(const P: pointer): integer;
begin
  Result := PInteger(P)^;

end;

procedure SetterPixelInt(const P: pointer; const AValue: integer);
begin
  PInteger(P)^ := AValue;
end;

end.
