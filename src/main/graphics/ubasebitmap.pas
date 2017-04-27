unit uBaseBitmap;

interface

uses SysUtils, Classes, contnrs;

type

  { IBaseBitmap }

  IBaseBitmap = interface
    ['{A54057AF-75DD-4E89-8567-0E644611C0D9}']
    function Clone(const ACopy: boolean = False): IBaseBitmap;
    function GetBuffer: pointer;
    function GetBufferSize: integer;
    function GetHeight: integer;
    function GetLineGap: integer;
    function GetObject: TObject;
    function GetPixel(X, Y: integer): pointer;
    function GetPixelSize: integer;
    function GetScanLine(Row: integer): pointer;
    function GetTag: integer;
    function GetWidth: integer;
    function IsEmpty: boolean;
    function LineOffset(const Col: integer): integer;
    function LineSize: integer;
    procedure Assign(const Source: IBaseBitmap);
    procedure Clear;
    procedure CopyTo(const Target: IBaseBitmap);
    procedure Draw(const R: TRect; const Source: IBaseBitmap;
      const SX: integer = 0; const SY: integer = 0);
    procedure Reset;
    procedure SetHeight(AValue: integer);
    procedure SetLineGap(AValue: integer);
    procedure SetPixelSize(AValue: integer);
    procedure SetSize(const AWidth, AHeight: integer);
    procedure SetTag(AValue: integer);
    procedure SetWidth(AValue: integer);
    property Buffer: pointer read GetBuffer;
    property BufferSize: integer read GetBufferSize;
    property Height: integer read GetHeight write SetHeight;
    property LineGap: integer read GetLineGap write SetLineGap;
    property Pixel[X, Y: integer]: pointer read GetPixel;
    property PixelSize: integer read GetPixelSize write SetPixelSize;
    property ScanLine[Row: integer]: pointer read GetScanLine;
    property Tag: integer read GetTag write SetTag;
    property Width: integer read GetWidth write SetWidth;
    function PixelInterface: IUnknown;
  end;

  { TCustomBaseBitmap }

  TCustomBaseBitmap = class(TInterfacedObject, IBaseBitmap)
  strict private
    FHeight: integer;
    FLineGap: integer;
    FPixelSize: integer;
    FWidth: integer;
    FTag: integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Clone(const ACopy: boolean = False): IBaseBitmap;
    function GetBuffer: pointer; virtual; abstract;
    function GetBufferSize: integer;
    function GetHeight: integer;
    function GetLineGap: integer;
    function GetObject: TObject;
    function GetPixel(X, Y: integer): pointer;
    function GetPixelSize: integer;
    function GetScanLine(Row: integer): pointer;
    function GetTag: integer;
    function GetWidth: integer;
    function IsEmpty: boolean; virtual; abstract;
    function LineOffset(const Col: integer): integer;
    function LineSize: integer;
    procedure Assign(const Source: IBaseBitmap);
    procedure Clear;
    procedure CopyTo(const Target: IBaseBitmap);
    procedure Draw(const R: TRect; const Source: IBaseBitmap;
      const SX: integer = 0; const SY: integer = 0);
    function PixelInterface: IUnknown;
    procedure Reset; virtual; abstract;
    procedure SetHeight(AValue: integer);
    procedure SetLineGap(AValue: integer);
    procedure SetPixelSize(AValue: integer);
    procedure SetSize(const AWidth, AHeight: integer);
    procedure SetTag(AValue: integer);
    procedure SetWidth(AValue: integer);
    property Buffer: pointer read GetBuffer;
    property BufferSize: integer read GetBufferSize;
    property Height: integer read GetHeight write SetHeight;
    property LineGap: integer read GetLineGap write SetLineGap;
    property Pixel[X, Y: integer]: pointer read GetPixel;
    property PixelSize: integer read GetPixelSize write SetPixelSize;
    property ScanLine[Row: integer]: pointer read GetScanLine;
    property Tag: integer read GetTag write SetTag;
    property Width: integer read GetWidth write SetWidth;
  end;

  { TBufferedBaseBitmap }

  TBufferedBaseBitmap = class(TCustomBaseBitmap)
  private
    FBuffer: pointer;
  protected
    procedure SetBuffer(AValue: pointer);
  public
    function IsEmpty: boolean; override;
    function GetBuffer: pointer; override;
    procedure Reset; override;
    property Buffer: pointer read GetBuffer write SetBuffer;
    destructor Destroy; override;
  end;

  { TBaseBitmap }

  TBaseBitmap = class(TCustomBaseBitmap)
  private
    FBuffer: pointer;
  public
    function IsEmpty: boolean; override;
    function GetBuffer: pointer; override;
    procedure Reset; override;
    destructor Destroy; override;
  end;

  { IBaseBitmapLayer }

  IBaseBitmapLayer = interface
    ['{2E614CA5-A729-4A77-A0B1-7CED9460B071}']
    function GetActiveLayer: integer;
    function GetLayer(Index: integer): IBaseBitmap;
    function GetLayersCount: integer;
    procedure SetActiveLayer(AValue: integer);
    procedure SetLayersCount(AValue: integer);
    property ActiveLayer: integer read GetActiveLayer write SetActiveLayer;
    property Layer[Index: integer]: IBaseBitmap read GetLayer;
    property LayersCount: integer read GetLayersCount write SetLayersCount;
  end;

  { TLayeredBaseBitmap }

  TLayeredBaseBitmap = class(TCustomBaseBitmap, IBaseBitmapLayer)
  private
    FActveBuffer: pointer;
    FActiveLayer: integer;
    FLayers: TList;
    FObjects: TList;
    FIsEmpty: boolean;
  protected
    procedure DeleteLayer(const ALayer: integer);
    procedure ResetLayer(const ALayer: integer);
    function GetLayerBuffer(const ALayer: integer): pointer;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActiveLayer: integer;
    function GetBuffer: pointer; override;
    function GetLayer(Index: integer): IBaseBitmap;
    function GetLayersCount: integer;
    function IsEmpty: boolean; override;
    procedure Reset; override;
    procedure SetActiveLayer(AValue: integer);
    procedure SetLayersCount(AValue: integer);
    property ActiveLayer: integer read GetActiveLayer write SetActiveLayer;
    property Layer[Index: integer]: IBaseBitmap read GetLayer;
    property LayersCount: integer read GetLayersCount write SetLayersCount;
  end;

implementation

uses Math;

type

  { TPixelBuf }

  TPixelBuf = packed record
    Size: byte;
    Ptr: pointer;
    procedure Init(const APtr: pointer; const ASize: byte); inline;
  end;

procedure TPixelBuf.Init(const APtr: pointer; const ASize: byte);
begin
  Size := ASize;
  Ptr := APtr;
end;

function PixBuf(const A: IBaseBitmap; const X, Y: integer): TPixelBuf;
begin
  Assert(A.PixelSize >= 0);
  Assert(A.PixelSize < 256);
  Result.Init(A.Pixel[X, Y], A.PixelSize);
end;

procedure _MovePixel(const Source: TPixelBuf; const Target: TPixelBuf);
begin
  //TODO: optimize
  move(Source.Ptr^, Target.Ptr^, min(Source.Size, Target.Size));
  if Target.Size > Source.Size then
    FillChar((Target.Ptr + Source.Size)^, Target.Size - Source.Size, 0);
end;

{ TLayeredBaseBitmap }

procedure TLayeredBaseBitmap.DeleteLayer(const ALayer: integer);
begin
  Assert(FLayers.Count = FObjects.Count);
  if (ALayer < 0) or (ALayer > FLayers.Count) then
    raise Exception.Create('Layer index out of range');
  ResetLayer(ALayer);
  FLayers.Delete(ALayer);
  FObjects.Delete(ALayer);
  Assert(FLayers.Count = FObjects.Count);
end;

procedure TLayeredBaseBitmap.ResetLayer(const ALayer: integer);
begin
  Assert(FLayers.Count = FObjects.Count);
  if (ALayer < 0) or (ALayer > FLayers.Count) then
    raise Exception.Create('Layer index out of range');
  Freemem(FLayers.Items[ALayer]);
  FLayers.List^[ALayer] := nil;
  IBaseBitmap(FObjects.List^[ALayer]) := nil;
end;

function TLayeredBaseBitmap.GetLayerBuffer(const ALayer: integer): pointer;
var
  P: pointer;
begin
  P := FLayers.Items[ALayer];
  if not assigned(P) then
  begin
    P := GetMem(GetBufferSize);
    if InterlockedCompareExchange(FLayers.List^[ALayer], P, nil) <> nil then
    begin
      Freemem(P);
      P := FLayers.Items[ALayer];
    end;
  end;
  Result := P;
  Assert(Assigned(Result));
  if FIsEmpty then
    FIsEmpty := False;
end;

function TLayeredBaseBitmap.GetActiveLayer: integer;
begin
  Result := FActiveLayer;
end;

function TLayeredBaseBitmap.GetBuffer: pointer;
begin
  Result := FLayers.Items[FActiveLayer];
  if not Assigned(Result) then
    Result := GetLayerBuffer(FActiveLayer);
end;

function TLayeredBaseBitmap.GetLayer(Index: integer): IBaseBitmap;
var
  L: TBufferedBaseBitmap;
  T: IBaseBitmap;
begin
  if not Assigned(FObjects.Items[Index]) then
  begin
    L := TBufferedBaseBitmap.Create;
    L.Height := Height;
    L.Width := Width;
    L.LineGap := LineGap;
    L.PixelSize := PixelSize;
    L.Buffer := GetLayerBuffer(Index);
    L._AddRef;
    T := L;
    if InterlockedCompareExchange(FObjects.List^[Index], pointer(T), nil) <> nil then
      L._Release;
  end;
  Result := IBaseBitmap(FObjects.Items[Index]);
  Assert(Assigned(Result));
end;

function TLayeredBaseBitmap.GetLayersCount: integer;
begin
  Result := FLayers.Count;
  Assert(Result = FObjects.Count);
end;

procedure TLayeredBaseBitmap.Reset;
var
  I: integer;
begin
  Assert(FLayers.Count = FObjects.Count);
  for I := 0 to FLayers.Count - 1 do
    ResetLayer(I);
  FIsEmpty := True;
end;

procedure TLayeredBaseBitmap.SetActiveLayer(AValue: integer);
begin
  if FActiveLayer = AValue then
    Exit;
  FActiveLayer := AValue;
  FActveBuffer := nil;
end;

procedure TLayeredBaseBitmap.SetLayersCount(AValue: integer);
begin
  Assert(AValue >= 0);
  Assert(FLayers.Count = FObjects.Count);
  while (FLayers.Count > 0) and (AValue < FLayers.Count) do
    DeleteLayer(FLayers.Count - 1);
  if FLayers.Count < AValue then
  begin
    FLayers.Count := AValue;
    FObjects.Count := AValue;
  end;
  Assert(FLayers.Count = FObjects.Count);
  if (FActiveLayer < 0) and (FLayers.Count > 0) then
    FActiveLayer := 0
  else if FActiveLayer >= FLayers.Count then
    FActiveLayer := FLayers.Count - 1;
  if FLayers.Count = 0 then
    FIsEmpty := True;
end;

constructor TLayeredBaseBitmap.Create;
begin
  inherited Create;
  FLayers := TList.Create;
  FObjects := TObjectList.Create(True);
  FActiveLayer := -1;
  FIsEmpty := True;
end;

function TLayeredBaseBitmap.IsEmpty: boolean;
begin
  Result := FIsEmpty;
end;

destructor TLayeredBaseBitmap.Destroy;
begin
  while LayersCount > 0 do
    DeleteLayer(LayersCount - 1);
  FreeAndNil(FObjects);
  FreeAndNil(FLayers);
  inherited Destroy;
end;

{ TBaseBitmap }

function TBaseBitmap.IsEmpty: boolean;
begin
  Result := not Assigned(FBuffer);
end;

function TBaseBitmap.GetBuffer: pointer;
begin
  if not Assigned(FBuffer) then
    FBuffer := GetMem(GetBufferSize());
  Result := FBuffer;
end;

procedure TBaseBitmap.Reset;
begin
  if Assigned(FBuffer) then
  begin
    Freemem(FBuffer);
    FBuffer := nil;
  end;
end;

destructor TBaseBitmap.Destroy;
begin
  Reset;
  Assert(not Assigned(FBuffer));
  inherited Destroy;
end;

{ TBufferedBaseBitmap }

procedure TBufferedBaseBitmap.SetBuffer(AValue: pointer);
begin
  FBuffer := AValue;
end;

function TBufferedBaseBitmap.IsEmpty: boolean;
begin
  Result := not Assigned(FBuffer);
end;

function TBufferedBaseBitmap.GetBuffer: pointer;
begin
  Result := FBuffer;
end;

procedure TBufferedBaseBitmap.Reset;
begin
  if not Assigned(FBuffer) then
    Exit;
  raise Exception.Create('can`t reset. buffer already allocated');
end;

destructor TBufferedBaseBitmap.Destroy;
begin
  inherited Destroy;
end;

{ TCustomBaseBitmap }

procedure TCustomBaseBitmap.Clear;
begin
  if Assigned(Buffer) then
  begin
    Assert(MemSize(Buffer) = GetBufferSize);
    FillChar(Buffer^, GetBufferSize, 0);
  end;
end;

function TCustomBaseBitmap.Clone(const ACopy: boolean): IBaseBitmap;
var
  New: TCustomBaseBitmap;
begin
  TObject(New) := ClassType.newinstance;
  New.Height := FHeight;
  New.LineGap := FLineGap;
  New.PixelSize := FPixelSize;
  New.Width := FWidth;
  if ACopy then
    CopyTo(New);
  Result := New;
end;

procedure TCustomBaseBitmap.CopyTo(const Target: IBaseBitmap);
var
  AW, AH: integer;
  Y, X: integer;
begin
  AW := min(Width, Target.Width);
  AH := min(Height, Target.Height);
  if (Target.LineSize = LineSize) and (Target.Width = Width) then
    move(Buffer^, Target.Buffer^, LineSize * AH)
  else
  if Target.PixelSize = PixelSize then
    for Y := 0 to AH - 1 do
      Move(ScanLine[Y]^, Target.ScanLine[Y]^, AW * PixelSize)
  else
    for Y := 0 to AH - 1 do
      for X := 0 to AW - 1 do
        _MovePixel(PixBuf(Self, x, y), PixBuf(Target, X, Y));
end;

procedure TCustomBaseBitmap.Draw(const R: TRect; const Source: IBaseBitmap;
  const SX: integer; const SY: integer);
var
  CX, CY, X, Y: integer;
begin
  Assert(Assigned(Source));
  //TODO: opimize for same pixel size by scanline
  for Y := R.Top to R.Bottom - 1 do
  begin
    if (Y < 0) or (Y >= Height) then
      Continue;
    CY := Y - R.Top + SY;
    if CY >= Source.Height then
      Continue;
    for X := R.Left to R.Right - 1 do
    begin
      CX := X - R.Left + SX;
      if CX >= Source.Width then
        Continue;
      _MovePixel(PixBuf(Source, CX, CY), PixBuf(Self, X, Y));
    end;
  end;
end;

function TCustomBaseBitmap.PixelInterface: IUnknown;
begin
  Result := nil;
end;

function TCustomBaseBitmap.GetPixelSize: integer;
begin
  Result := FPixelSize;
end;

function TCustomBaseBitmap.GetBufferSize: integer;
begin
  Result := LineSize() * Height;
end;

function TCustomBaseBitmap.GetHeight: integer;
begin
  Result := FHeight;
end;

function TCustomBaseBitmap.GetLineGap: integer;
begin
  Result := FLineGap;
end;

function TCustomBaseBitmap.GetPixel(X, Y: integer): pointer;
begin
  Assert(X >= 0);
  Assert(Y >= 0);
  Result := ScanLine[Y] + LineOffset(X);
end;

function TCustomBaseBitmap.GetScanLine(Row: integer): pointer;
begin
  if (Row < 0) or (Row >= Height) then
    raise ERangeError.Create('row out of range');
  Assert(Row >= 0);
  Result := GetBuffer() + Row * LineSize();
end;

function TCustomBaseBitmap.GetTag: integer;
begin
  Result := FTag;
end;

function TCustomBaseBitmap.GetWidth: integer;
begin
  Result := FWidth;
end;

function TCustomBaseBitmap.LineOffset(const Col: integer): integer;
begin
  if (Col < 0) or (Col >= Width) then
    raise ERangeError.Create('Col out of range');
  Assert(FPixelSize >= 0);
  Assert(Col >= 0);
  Result := Col * FPixelSize;
end;

function TCustomBaseBitmap.LineSize: integer;
begin
  Assert(FPixelSize >= 0);
  Assert(FLineGap >= 0);
  Assert(FWidth >= 0);
  Result := FWidth * FPixelSize + FLineGap;
end;

procedure TCustomBaseBitmap.SetHeight(AValue: integer);
begin
  if FHeight = AValue then
    exit;
  Reset;
  FHeight := AValue;
end;

procedure TCustomBaseBitmap.SetLineGap(AValue: integer);
begin
  if FLineGap = AValue then
    exit;
  Reset;
  FLineGap := AValue;

end;

procedure TCustomBaseBitmap.SetPixelSize(AValue: integer);
begin
  if FPixelSize = AValue then
    exit;
  Reset;
  FPixelSize := AValue;
end;

procedure TCustomBaseBitmap.SetSize(const AWidth, AHeight: integer);
begin
  Height := AHeight;
  Width := AWidth;
end;

procedure TCustomBaseBitmap.SetTag(AValue: integer);
begin
  if FTag = AValue then
    exit;
  FTag := AValue;
end;

procedure TCustomBaseBitmap.SetWidth(AValue: integer);
begin
  if AValue = FWidth then
    exit;
  Reset;
  FWidth := AValue;
end;

function TCustomBaseBitmap.GetObject: TObject;
begin
  Result := Self;
end;

procedure TCustomBaseBitmap.Assign(const Source: IBaseBitmap);
var
  X, Y: integer;
begin
  SetSize(Source.Width, Source.Height);
  PixelSize := Source.PixelSize;
  for Y := 0 to Height - 1 do
  begin
    if Y >= Source.Height then
      Continue;
    for X := 0 to Width - 1 do
    begin
      if X >= Source.Width then
        break;
      _MovePixel(PixBuf(Source, X, Y), PixBuf(Self, X, Y));
    end;
  end;
end;

constructor TCustomBaseBitmap.Create;
begin
  inherited Create;
  FPixelSize := 1;
end;

destructor TCustomBaseBitmap.Destroy;
begin
  inherited Destroy;
end;

end.
