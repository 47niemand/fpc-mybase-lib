unit uConvolutionMatrix;

{$mode delphi}

interface

uses
  Classes, SysUtils, uBaseBitmap, uBasePixel;

type

  TEdgeEffect = (EF_NONE, EF_WARP, EF_EXTEND);

  { IConvolutionMatrix }

  IConvolutionMatrix = interface
    ['{41CE0832-5FEC-4E72-9860-91A10A8E3CD7}']
    function GetCell(Row, Col: integer): integer;
    function GetCols: integer;
    function GetNormal: integer;
    function GetRows: integer;
    procedure ApplyConvolutionMatrix(const input, output: IBaseBitmap);
    function GetSize: integer;
    procedure Normalize;
    procedure SetCell(Row, Col: integer; AValue: integer);
    procedure SetNormal(AValue: integer);
    procedure SetSize(AValue: integer);
    property Cell[Row, Col: integer]: integer read GetCell write SetCell; default;
    property Cols: integer read GetCols;
    property Normal: integer read GetNormal write SetNormal;
    property Rows: integer read GetRows;
    property Size: integer read GetSize write SetSize;
  end;

// some standart matrix interfaces

function GetBoxBlurMatrix: IConvolutionMatrix;
function GetEdgeDetectionMatrix: IConvolutionMatrix;
function GetEdgeDetectionMatrixLess: IConvolutionMatrix;
function GetEdgeDetectionMatrixMore: IConvolutionMatrix;
function GetGaussianBlurMatrix: IConvolutionMatrix;
function GetIdentityMatrix: IConvolutionMatrix;
function GetSharpenMatrix: IConvolutionMatrix;
function GetUnSharpenMatrix: IConvolutionMatrix;

implementation


uses variants, uBaseTypes, uBaseInterface, uBaseMap;

type

  { TConvolutionMatrix }

  // TODO Set/Get Pixel Value

  TConvolutionMatrix = class(TInterfacedObject, IConvolutionMatrix)
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
    function GetNormal: integer;
    function GetSize: integer;
    procedure Normalize;
    property Cell[Row, Col: integer]: integer read GetCell write SetCell; default;
    property Cols: integer read GetCols;
    property Normal: integer read GetNormal write SetNormal;
    property Rows: integer read GetRows;
    property Size: integer read GetSize write SetSize;
  end;

  { TConvolutionMatrixImut }

  TConvolutionMatrixImut = class(TConvolutionMatrix)
  private
    FFinalized: boolean;
  public
    function Finalize: IConvolutionMatrix;
    procedure OnPropertyChangeing; override;
  end;

  { TMatrixSetup }

  TMatrixSetup = class
    class function MatrixDoesBoxBlur(const M: TConvolutionMatrix): TConvolutionMatrix;
    class function MatrixDoesGaussianBlur(const M: TConvolutionMatrix): TConvolutionMatrix;
    class function MatrixDoesIdentity(const M: TConvolutionMatrix): TConvolutionMatrix;
    class function MatrixDoesSharpen(const M: TConvolutionMatrix): TConvolutionMatrix;
    class function MatrixDoesEdgeDetection(const M: TConvolutionMatrix;
      intencity: integer = 1): TConvolutionMatrix;
    class function MatrixDoesUnSharpen(const M: TConvolutionMatrix): TConvolutionMatrix;
  end;

function CastVarToConvolutionMatrix(const V: variant): IConvolutionMatrix;
begin
  if VarisNull(v) or VarIsEmpty(v) then
    Result := nil
  else
    Result := v;
end;

var
  BasicMatrix: IBaseMap;

procedure RegisterConvMatrix(const Name: string; const Intf: IConvolutionMatrix);
begin
  BasicMatrix.Values[Name] := CastInterface(Intf);
end;

function GetBoxBlurMatrix: IConvolutionMatrix;
const
  _GetName = 'GetBoxBlurMatrix';
begin
  Result := CastVarToConvolutionMatrix(BasicMatrix.Values[_GetName]);
  if not Assigned(Result) then
  begin
    Result := TConvolutionMatrixImut(TMatrixSetup.MatrixDoesBoxBlur(
      TConvolutionMatrixImut.Create)).Finalize;
    RegisterConvMatrix(_GetName, Result);
  end;
end;

function GetEdgeDetectionMatrix: IConvolutionMatrix;
const
  _GetName = 'GetEdgeDetectionMatrix';
begin
  Result := CastVarToConvolutionMatrix(BasicMatrix.Values[_GetName]);
  if not Assigned(Result) then
  begin
    Result := TConvolutionMatrixImut(TMatrixSetup.MatrixDoesEdgeDetection(
      TConvolutionMatrixImut.Create, 1)).Finalize;
    RegisterConvMatrix(_GetName, Result);
  end;
end;

function GetEdgeDetectionMatrixLess: IConvolutionMatrix;
const
  _GetName = 'GetEdgeDetectionMatrixLess';
begin
  Result := CastVarToConvolutionMatrix(BasicMatrix.Values[_GetName]);
  if not Assigned(Result) then
  begin
    Result := TConvolutionMatrixImut(TMatrixSetup.MatrixDoesEdgeDetection(
      TConvolutionMatrixImut.Create, 0)).Finalize;
    RegisterConvMatrix(_GetName, Result);
  end;
end;

function GetEdgeDetectionMatrixMore: IConvolutionMatrix;
const
  _GetName = 'GetEdgeDetectionMatrixMore';
begin
  Result := CastVarToConvolutionMatrix(BasicMatrix.Values[_GetName]);
  if not Assigned(Result) then
  begin
    Result := TConvolutionMatrixImut(TMatrixSetup.MatrixDoesEdgeDetection(
      TConvolutionMatrixImut.Create, 2)).Finalize;
    RegisterConvMatrix(_GetName, Result);
  end;
end;

function GetGaussianBlurMatrix: IConvolutionMatrix;
const
  _GetName = 'GetGaussianBlurMatrix';
begin
  Result := CastVarToConvolutionMatrix(BasicMatrix.Values[_GetName]);
  if not Assigned(Result) then
  begin
    Result := TConvolutionMatrixImut(TMatrixSetup.MatrixDoesGaussianBlur(
      TConvolutionMatrixImut.Create)).Finalize;
    RegisterConvMatrix(_GetName, Result);
  end;
end;

function GetIdentityMatrix: IConvolutionMatrix;
const
  _GetName = 'GetIdentityMatrix';
begin
  Result := CastVarToConvolutionMatrix(BasicMatrix.Values[_GetName]);
  if not Assigned(Result) then
  begin
    Result := TConvolutionMatrixImut(TMatrixSetup.MatrixDoesIdentity(
      TConvolutionMatrixImut.Create)).Finalize;
    RegisterConvMatrix(_GetName, Result);
  end;
end;

function GetSharpenMatrix: IConvolutionMatrix;
const
  _GetName = 'GetSharpenMatrix';
begin
  Result := CastVarToConvolutionMatrix(BasicMatrix.Values[_GetName]);
  if not Assigned(Result) then
  begin
    Result := TConvolutionMatrixImut(TMatrixSetup.MatrixDoesSharpen(
      TConvolutionMatrixImut.Create)).Finalize;
    RegisterConvMatrix(_GetName, Result);
  end;
end;

function GetUnSharpenMatrix: IConvolutionMatrix;
const
  _GetName = 'GetUnSharpenMatrix';
begin
  Result := CastVarToConvolutionMatrix(BasicMatrix.Values[_GetName]);
  if not Assigned(Result) then
  begin
    Result := TConvolutionMatrixImut(TMatrixSetup.MatrixDoesUnSharpen(
      TConvolutionMatrixImut.Create)).Finalize;
    RegisterConvMatrix(_GetName, Result);
  end;
end;

{ TConvolutionMatrixImut }

function TConvolutionMatrixImut.Finalize: IConvolutionMatrix;
begin
  Result := Self;
  if FFinalized then
    exit;
  FFinalized := True;
end;

procedure TConvolutionMatrixImut.OnPropertyChangeing;
begin
  raise EBasePropReadOnly.Create('Can`t change');
end;

{ TConvolutionMatrix }

function TConvolutionMatrix.GetCell(Row, Col: integer): integer;
begin
  if (Row < 0) or (Row >= Rows) or (Row < 0) or (Row >= Cols) then
    raise EBoundsCheckError.Create('index out of bounds');
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
    raise EBoundsCheckError.Create('index out of bounds');
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
    raise ERangeError.Create('odd size requerd');
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

function TConvolutionMatrix.GetNormal: integer;
begin
  Result := FNormal;
end;

function TConvolutionMatrix.GetSize: integer;
begin
  Result := FSize;
end;

{ TMatrixSetup }

class function TMatrixSetup.MatrixDoesIdentity(const M: TConvolutionMatrix):
TConvolutionMatrix;
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
  Result := M;
end;

class function TMatrixSetup.MatrixDoesEdgeDetection(const M: TConvolutionMatrix;
  intencity: integer): TConvolutionMatrix;
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
  Result := M;
end;

class function TMatrixSetup.MatrixDoesSharpen(const M: TConvolutionMatrix):
TConvolutionMatrix;
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
  Result := M;
end;

class function TMatrixSetup.MatrixDoesBoxBlur(const M: TConvolutionMatrix):
TConvolutionMatrix;
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
  Result := M;
end;

class function TMatrixSetup.MatrixDoesGaussianBlur(
  const M: TConvolutionMatrix): TConvolutionMatrix;
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
  Result := M;
end;

class function TMatrixSetup.MatrixDoesUnSharpen(const M: TConvolutionMatrix):
TConvolutionMatrix;
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
  Result := M;
end;

initialization
  BasicMatrix := NewBaseIntfMap;

finalization
  BasicMatrix.Clear;
  BasicMatrix := nil;
end.
