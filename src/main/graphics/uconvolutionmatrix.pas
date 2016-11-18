unit uConvolutionMatrix;

{$mode delphi}

interface

uses
  Classes, SysUtils, uBaseBitmap, uBasePixel;

type

  TEdgeEffect = (EF_NONE, EF_WARP, EF_EXTEND);

  { TConvolutionMatrix }

  // TODO Set/Get Pixel Value

  TConvolutionMatrix = class
  private
    FNormal: integer;
    FSize: integer;
    FMatrix: array of array of integer;
    function GetCell(Row, Col: integer): integer;
    function GetRows: integer;
    function GetCols: integer;
    procedure SetCell(Row, Col: integer; AValue: integer);
    procedure SetNormal(AValue: integer);
    procedure SetSize(AValue: integer);
  protected
    procedure OnPropertyChangeing; virtual;
    procedure SetPixelValue(const P: pointer; const AValue: integer);
    function GetPixelValue(const P: pointer): integer;
    procedure ApplyConvolutionMatrixCore(const input, output: IBaseBitmap;
      const edgeeffect: TEdgeEffect; const midr, midc: integer; const x, y: integer);
  public
    procedure ApplyConvolutionMatrix(const input, output: IBaseBitmap);
    procedure Normalize;
    property Cell[Row, Col: integer]: integer read GetCell write SetCell; default;
    property Cols: integer read GetCols;
    property Normal: integer read FNormal write SetNormal;
    property Rows: integer read GetRows;
    property Size: integer read FSize write SetSize;
  end;


type
  TConvolutionKind = (BoxBlur, GaussianBlur, Identity, Sharpen,
    EdgeDetection, UnSharpen);


// some standart matrix as singlenton

function GetBoxBlurMatrix: TConvolutionMatrix;
function GetGaussianBlurMatrix: TConvolutionMatrix;
function GetIdentityMatrix: TConvolutionMatrix;
function GetSharpenMatrix: TConvolutionMatrix;
function GetEdgeDetectionMatrix: TConvolutionMatrix;
function GetUnSharpenMatrix: TConvolutionMatrix;

implementation

type

  { TMatrixSetup }

  TMatrixSetup = class
    class procedure MatrixDoesBoxBlur(const M: TConvolutionMatrix);
    class procedure MatrixDoesGaussianBlur(const M: TConvolutionMatrix);
    class procedure MatrixDoesIdentity(const M: TConvolutionMatrix);
    class procedure MatrixDoesSharpen(const M: TConvolutionMatrix);
    class procedure MatrixDoesEdgeDetection(const M: TConvolutionMatrix;
      intencity: integer = 1);
    class procedure MatrixDoesUnSharpen(const M: TConvolutionMatrix);
  end;


var
  FBoxBlurMatrix: TConvolutionMatrix;
  FGaussianBlurMatrix: TConvolutionMatrix;
  FIdentityMatrix: TConvolutionMatrix;
  FSharpenMatrix: TConvolutionMatrix;
  FEdgeDetectionMatrix: TConvolutionMatrix;
  FUnSharpenMatrix: TConvolutionMatrix;

type

  PObject = ^TObject;

  { TConvolutionMatrixImut }

  TConvolutionMatrixImut = class(TConvolutionMatrix)
  private
    FFinalized: boolean;
    FOwnptr: PObject;
  protected
    procedure OnPropertyChangeing; override;
    procedure Finalize;
  public
    constructor Create(var AOwnvar: TObject);
    destructor Destroy; override;
  end;

function GetBoxBlurMatrix: TConvolutionMatrix;
begin
  if not Assigned(FBoxBlurMatrix) then
  begin
    Result := TConvolutionMatrixImut.Create(TObject(FBoxBlurMatrix));
    TMatrixSetup.MatrixDoesBoxBlur(Result);
    if Result <> FBoxBlurMatrix then
      FreeAndNil(Result);
    if Assigned(Result) then
      TConvolutionMatrixImut(Result).Finalize;
  end;
  Result := FBoxBlurMatrix;
end;

function GetGaussianBlurMatrix: TConvolutionMatrix;
begin
  if not Assigned(FGaussianBlurMatrix) then
  begin
    Result := TConvolutionMatrixImut.Create(TObject(FGaussianBlurMatrix));
    TMatrixSetup.MatrixDoesGaussianBlur(Result);
    if Result <> FBoxBlurMatrix then
      FreeAndNil(Result);
    if Assigned(Result) then
      TConvolutionMatrixImut(Result).Finalize;
  end;
  Result := FGaussianBlurMatrix;
end;

function GetIdentityMatrix: TConvolutionMatrix;
begin
  if not Assigned(FIdentityMatrix) then
  begin
    Result := TConvolutionMatrixImut.Create(TObject(FIdentityMatrix));
    TMatrixSetup.MatrixDoesIdentity(Result);
    if InterlockedCompareExchange(pointer(FIdentityMatrix),
      pointer(Result), nil) <> nil then
      FreeAndNil(Result);
    if Assigned(Result) then
      TConvolutionMatrixImut(Result).Finalize;
  end;
  Result := FIdentityMatrix;
end;

function GetSharpenMatrix: TConvolutionMatrix;
begin
  if not Assigned(FSharpenMatrix) then
  begin
    Result := TConvolutionMatrixImut.Create(TObject(FSharpenMatrix));
    TMatrixSetup.MatrixDoesSharpen(Result);
    if Result <> FSharpenMatrix then
      FreeAndNil(Result);
    if Assigned(Result) then
      TConvolutionMatrixImut(Result).Finalize;
  end;
  Result := FSharpenMatrix;
end;

function GetEdgeDetectionMatrix: TConvolutionMatrix;
begin
  if not Assigned(FEdgeDetectionMatrix) then
  begin
    Result := TConvolutionMatrixImut.Create(TObject(FEdgeDetectionMatrix));
    TMatrixSetup.MatrixDoesEdgeDetection(Result);
    if Result <> FEdgeDetectionMatrix then
      FreeAndNil(Result);
    if Assigned(Result) then
      TConvolutionMatrixImut(Result).Finalize;
  end;
  Result := FEdgeDetectionMatrix;
end;

function GetUnSharpenMatrix: TConvolutionMatrix;
begin
  if not Assigned(FUnSharpenMatrix) then
  begin
    Result := TConvolutionMatrixImut.Create(TObject(FUnSharpenMatrix));
    TMatrixSetup.MatrixDoesUnSharpen(Result);
    if Result <> FUnSharpenMatrix then
      FreeAndNil(Result);
    if Assigned(Result) then
      TConvolutionMatrixImut(Result).Finalize;
  end;
  Result := FUnSharpenMatrix;
end;

{ TConvolutionMatrixImut }

procedure TConvolutionMatrixImut.OnPropertyChangeing;
begin
  if FFinalized then
    raise EPropReadOnly.Create('can`t change');
end;

procedure TConvolutionMatrixImut.Finalize;
begin
  FFinalized := True;
end;

constructor TConvolutionMatrixImut.Create(var AOwnvar: TObject);
begin
  inherited Create;
  if InterlockedCompareExchange(pointer(AOwnvar), pointer(Self), nil) = nil then
    FOwnptr := @AOwnvar;
end;

destructor TConvolutionMatrixImut.Destroy;
begin
  if Assigned(FOwnptr) then
    InterlockedCompareExchange(pointer(FOwnptr^), nil, pointer(self));
  inherited Destroy;
end;

{ TConvolutionMatrix }

function TConvolutionMatrix.GetCell(Row, Col: integer): integer;
begin
  if (Row < 0) or (Row >= Rows) or (Row < 0) or (Row >= Cols) then
    raise Exception.Create('index out of range');
  Result := FMatrix[Row][Col];
end;

function TConvolutionMatrix.GetRows: integer;
begin
  Result := FSize;
end;

function TConvolutionMatrix.GetCols: integer;
begin
  Result := FSize;
end;

procedure TConvolutionMatrix.SetCell(Row, Col: integer; AValue: integer);
begin
  if (Row < 0) or (Row >= Rows) or (Row < 0) or (Row >= Cols) then
    raise Exception.Create('index out of range');
  if FMatrix[Row][Col] = AValue then
    exit;
  OnPropertyChangeing;
  FMatrix[Row][Col] := AValue;
end;

procedure TConvolutionMatrix.SetNormal(AValue: integer);
begin
  if FNormal = AValue then
    Exit;
  OnPropertyChangeing;
  FNormal := AValue;
end;

procedure TConvolutionMatrix.SetSize(AValue: integer);
var
  I: integer;
begin
  if FSize = AValue then
    Exit;
  OnPropertyChangeing;
  if not odd(AValue) then
    raise Exception.Create('odd size requerd');
  FSize := AValue;
  for I := FSize to Length(FMatrix) - 1 do
    SetLength(FMatrix[I], 0);
  SetLength(FMatrix, FSize);
  for I := 0 to FSize - 1 do
    SetLength(FMatrix[I], FSize);
end;

procedure TConvolutionMatrix.OnPropertyChangeing;
begin
  // none in this scoup
end;

function TConvolutionMatrix.GetPixelValue(const P: pointer): integer;
begin
  Result := PByte(P)^;
end;

procedure TConvolutionMatrix.SetPixelValue(const P: pointer; const AValue: integer);
begin
  PByte(P)^ := byte(AValue);
end;

procedure TConvolutionMatrix.Normalize;
var
  r, c, s: integer;
begin
  s := 0;
  for r := 0 to Rows - 1 do
    for c := 0 to Cols - 1 do
      s := s + Cell[r, c];
  Normal := s;
end;

procedure TConvolutionMatrix.ApplyConvolutionMatrixCore(
  const input, output: IBaseBitmap;
  const edgeeffect: TEdgeEffect; const midr, midc: integer; const x, y: integer);
var
  r, cx, cy, c, cx_, cy_: integer;
  a: integer;
begin
  //TODO optimize
  a := 0;
  for r := 0 to Rows - 1 do
    for c := 0 to Cols - 1 do
    begin
      cx := x - midc + c;
      cy := y - midr + r;
      case edgeeffect of
        EF_EXTEND, EF_WARP: // TODO imlement different effects
        begin
          cx_ := cx;
          cy_ := cy;
          if cx_ < 0 then
            cx_ := 0;
          if cx_ >= input.Width then
            cx_ := input.Width - 1;
          if cy_ < 0 then
            cy_ := 0;
          if cy_ >= input.Height then
            cy_ := input.Height - 1;
          a := a + GetPixelValue(input.Pixel[cx_, cy_]) * Cell[r, c];
        end;
        else
          if (cx >= 0) and (cx < input.Width) and (cy >= 0) and
            (cy < input.Height) then
            a := a + GetPixelValue(input.Pixel[cx, cy]) * Cell[r, c];
      end;
    end;
  if Normal <> 0 then
    a := a div Normal;
  SetPixelValue(output.Pixel[x, y], a);
end;

procedure TConvolutionMatrix.ApplyConvolutionMatrix(const input, output: IBaseBitmap);
var
  midr, midc, y, x: integer;
  edgeeffect: TEdgeEffect;
begin
  //TODO optimize...
  edgeeffect := EF_EXTEND;    // TODO implement different effects
  midr := rows div 2;
  midc := Cols div 2;
  for y := 0 to input.Height - 1 do
    for x := 0 to input.Width - 1 do
      ApplyConvolutionMatrixCore(input, output, edgeeffect, midr, midc, x, y);
end;

{ TMatrixSetup }

class procedure TMatrixSetup.MatrixDoesIdentity(const M: TConvolutionMatrix);
begin
  with M do
  begin
    Size := 3;
    Cell[0, 0] := 0;
    Cell[0, 1] := 0;
    Cell[0, 2] := 0;

    Cell[1, 0] := 0;
    Cell[1, 1] := 1;
    Cell[1, 2] := 0;

    Cell[2, 0] := 0;
    Cell[2, 1] := 0;
    Cell[2, 2] := 0;
    Normalize;
  end;
end;

class procedure TMatrixSetup.MatrixDoesEdgeDetection(const M: TConvolutionMatrix;
  intencity: integer);
begin
  with M do
  begin
    Size := 3;
    case intencity of
      0:
      begin
        Cell[0, 0] := 1;
        Cell[0, 1] := 0;
        Cell[0, 2] := -1;

        Cell[1, 0] := 0;
        Cell[1, 1] := 0;
        Cell[1, 2] := 0;

        Cell[2, 0] := -1;
        Cell[2, 1] := 0;
        Cell[2, 2] := 1;
      end;
      1:
      begin
        Size := 3;
        Cell[0, 0] := 0;
        Cell[0, 1] := 1;
        Cell[0, 2] := 0;

        Cell[1, 0] := 1;
        Cell[1, 1] := -4;
        Cell[1, 2] := 1;

        Cell[2, 0] := 0;
        Cell[2, 1] := 1;
        Cell[2, 2] := 0;
      end
      else
      begin
        Size := 3;
        Cell[0, 0] := -1;
        Cell[0, 1] := -1;
        Cell[0, 2] := -1;

        Cell[1, 0] := -1;
        Cell[1, 1] := 8;
        Cell[1, 2] := -1;

        Cell[2, 0] := -1;
        Cell[2, 1] := -1;
        Cell[2, 2] := -1;
      end;
    end;
    Normalize;
  end;
end;

class procedure TMatrixSetup.MatrixDoesSharpen(const M: TConvolutionMatrix);
begin
  with M do
  begin
    Size := 3;
    Cell[0, 0] := 0;
    Cell[0, 1] := -1;
    Cell[0, 2] := 0;

    Cell[1, 0] := -1;
    Cell[1, 1] := 5;
    Cell[1, 2] := -1;

    Cell[2, 0] := 0;
    Cell[2, 1] := -1;
    Cell[2, 2] := 0;

    Normalize;
  end;
end;

class procedure TMatrixSetup.MatrixDoesBoxBlur(const M: TConvolutionMatrix);
begin
  with M do
  begin
    Size := 3;
    Cell[0, 0] := 1;
    Cell[0, 1] := 1;
    Cell[0, 2] := 1;

    Cell[1, 0] := 1;
    Cell[1, 1] := 1;
    Cell[1, 2] := 1;

    Cell[2, 0] := 1;
    Cell[2, 1] := 1;
    Cell[2, 2] := 1;
    Normalize;
  end;
end;

class procedure TMatrixSetup.MatrixDoesGaussianBlur(const M: TConvolutionMatrix);
begin
  with M do
  begin
    Size := 3;
    Cell[0, 0] := 1;
    Cell[0, 1] := 2;
    Cell[0, 2] := 1;

    Cell[1, 0] := 2;
    Cell[1, 1] := 4;
    Cell[1, 2] := 2;

    Cell[2, 0] := 1;
    Cell[2, 1] := 2;
    Cell[2, 2] := 1;
    Normalize;
  end;
end;

class procedure TMatrixSetup.MatrixDoesUnSharpen(const M: TConvolutionMatrix);
begin
  with M do
  begin
    Size := 5;
    Cell[0, 0] := 1;
    Cell[0, 1] := 4;
    Cell[0, 2] := 6;
    Cell[0, 3] := 4;
    Cell[0, 4] := 1;

    Cell[1, 0] := 4;
    Cell[1, 1] := 16;
    Cell[1, 2] := 24;
    Cell[1, 3] := 16;
    Cell[1, 4] := 4;

    Cell[2, 0] := 6;
    Cell[2, 1] := 24;
    Cell[2, 2] := -476;
    Cell[2, 3] := 24;
    Cell[2, 4] := 6;

    Cell[3, 0] := 4;
    Cell[3, 1] := 16;
    Cell[3, 2] := 24;
    Cell[3, 3] := 16;
    Cell[3, 4] := 4;

    Cell[4, 0] := 1;
    Cell[4, 1] := 4;
    Cell[4, 2] := 6;
    Cell[4, 3] := 4;
    Cell[4, 4] := 1;
    Normalize;
  end;
end;


initialization

finalization
  FreeAndNil(FBoxBlurMatrix);
  FreeAndNil(FGaussianBlurMatrix);
  FreeAndNil(FIdentityMatrix);
  FreeAndNil(FSharpenMatrix);
  FreeAndNil(FEdgeDetectionMatrix);
  FreeAndNil(FUnSharpenMatrix);
end.
