unit TestCaseBaseBitmap;

{$mode delphi}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, uBaseBitmap,
  uBaseBitmapUtils, Graphics;

type

  { TTestCaseBaseBitmap }

  TTestCaseBaseBitmap = class(TTestCase)
  published
    procedure TestBitmapInterface;
    procedure TestBitmapUtils;
  end;

implementation

procedure TTestCaseBaseBitmap.TestBitmapInterface;
var
  I, C, B: IBaseBitmap;
  L: IBaseBitmapLayer;
  S: integer;
begin
  I := TBaseBitmap.Create;
  I.SetSize(10, 10);
  C := I.Clone();
  AssertEquals(True, C.GetObject is TBaseBitmap);
  AssertEquals(C.Width, 10);
  AssertEquals(C.Height, 10);
  B := TLayeredBaseBitmap.Create;
  S := B.QueryInterface(IBaseBitmapLayer, L);
  AssertEquals(S, S_OK);
  AssertEquals(0, L.LayersCount);
  AssertEquals(-1, L.ActiveLayer);
  L.LayersCount := 2;
  AssertEquals(0, L.ActiveLayer);
  L.LayersCount := 1;
  AssertEquals(0, L.ActiveLayer);
  AssertEquals(1, L.LayersCount);
  AssertEquals(True, B.IsEmpty);
  B.Assign(C);
  AssertEquals(1, L.LayersCount);
  AssertEquals(C.Width, B.Width);
  AssertEquals(C.Height, B.Height);
  AssertEquals(False, B.IsEmpty);
  B.Width := 11;
  AssertEquals(True, B.IsEmpty);

end;

procedure TTestCaseBaseBitmap.TestBitmapUtils;
var
  B: IBaseBitmap;
  TestSource: TBitmap;
  O: TBitmap;
  L: IBaseBitmapLayer;
  S: integer;
begin
  TestSource := TBitmap.Create;
  TestSource.Width := 10;
  TestSource.Height := 10;
  TestSource.PixelFormat := pf24bit;
  //Test bitmap
  AssertEquals($00FFFF, clYellow);
  AssertEquals($00FF00, clLime);
  AssertEquals($FF0000, clBlue);
  AssertEquals($0000FF, clRed);
  AssertEquals($FFFFFF, clWhite);
  AssertEquals($000000, clBlack);
  AssertEquals($C0C0C0, clLtGray);
  AssertEquals($FFFF00, clAqua);

  //draw test pattern
  TestSource.Canvas.Pixels[1, 7] := clYellow;
  TestSource.Canvas.Pixels[2, 6] := clLime;
  TestSource.Canvas.Pixels[3, 5] := clBlue;
  TestSource.Canvas.Pixels[4, 4] := clRed;
  TestSource.Canvas.Pixels[5, 3] := clWhite;
  TestSource.Canvas.Pixels[6, 2] := clBlack;
  TestSource.Canvas.Pixels[7, 1] := clLtGray;
  TestSource.Canvas.Pixels[8, 0] := clAqua;
  //do tests
  B := CreateFromBitmap(TestSource, cc_luminosity);
  AssertEquals(B.Width, TestSource.Width);
  AssertEquals(B.Height, TestSource.Height);
  AssertEquals(1, B.PixelSize);
  AssertEquals(255, PByte(B.Pixel[5, 3])^);
  AssertEquals(0, PByte(B.Pixel[6, 2])^);
  S := B.QueryInterface(IBaseBitmapLayer, L);
  AssertEquals(True, S <> S_OK);
  b := nil;

  B := CreateFromBitmap(TestSource, cc_red);
  AssertEquals(255, PByte(B.Pixel[4, 4])^);
  AssertEquals(255, PByte(B.Pixel[5, 3])^);
  AssertEquals(0, PByte(B.Pixel[8, 0])^);
  AssertEquals($C0, PByte(B.Pixel[7, 1])^);
  S := B.QueryInterface(IBaseBitmapLayer, L);
  AssertEquals(True, S <> S_OK);
  b := nil;

  B := CreateFromBitmap(TestSource, cc_green);
  AssertEquals(255, PByte(B.Pixel[2, 6])^);
  AssertEquals(255, PByte(B.Pixel[1, 7])^);
  AssertEquals(0, PByte(B.Pixel[6, 2])^);
  AssertEquals($C0, PByte(B.Pixel[7, 1])^);
  S := B.QueryInterface(IBaseBitmapLayer, L);
  AssertEquals(True, S <> S_OK);
  b := nil;

  B := CreateFromBitmap(TestSource, cc_blue);
  AssertEquals(255, PByte(B.Pixel[3, 5])^);
  AssertEquals(255, PByte(B.Pixel[8, 0])^);
  AssertEquals(0, PByte(B.Pixel[6, 2])^);
  AssertEquals($C0, PByte(B.Pixel[7, 1])^);
  S := B.QueryInterface(IBaseBitmapLayer, L);
  AssertEquals(True, S <> S_OK);
  b := nil;

  //TODO cc_hue, cc_saturation, cc_lightness,
  //TODO cc_BGR, cc_HSL

  B := CreateFromBitmap(TestSource, cc_lightness);
  AssertEquals(255, PByte(B.Pixel[5, 3])^);
  AssertEquals(0, PByte(B.Pixel[6, 2])^);
  S := B.QueryInterface(IBaseBitmapLayer, L);
  AssertEquals(True, S <> S_OK);
  b := nil;

end;



initialization

  RegisterTest(TTestCaseBaseBitmap);
end.










