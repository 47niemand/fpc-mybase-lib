unit uBaseBitmapUtils;

interface

uses Classes, SysUtils, Graphics, uBaseBitmap, uPixelUtils;

//TODO
type
  CC_LAYER = (cc_unknown, cc_luminosity, cc_intencity, cc_red, cc_green, cc_blue,
    cc_BGR, cc_hue, cc_saturation, cc_lightness, cc_HSL);

const
  CC_BITMAP_LAYERS = [cc_red, cc_green, cc_blue];

function CreateFromBitmap(const Source: TBitmap;
  ALayer: CC_LAYER = cc_luminosity): IBaseBitmap;
function MakeBitmap(Source: IBaseBitmap): TBitmap;


implementation

function CreateFromBitmap(const Source: TBitmap; ALayer: CC_LAYER): IBaseBitmap;
var
  L: TLayeredBaseBitmap;
  x, y: integer;
  rgb: TBGRAPixel;
  hsl: THSLAPixel;
begin
  case ALayer of
    cc_HSL, cc_BGR:
    begin
      L := TLayeredBaseBitmap.Create;
      with L do
      begin
        Width := Source.Width;
        Height := Source.Height;
        PixelSize := 1;
        LayersCount := 3;
        Tag := Ord(ALayer);
      end;
      for y := 0 to Source.Height - 1 do
        for x := 0 to Source.Width - 1 do
        begin
          // TODO Optimize with scanline
          rgb := ColorToBGRA(Source.Canvas.Pixels[x, y]);
          case ALayer of
            cc_HSL:
            begin
              hsl := BGRAToHSLA(rgb);
              PByte(L.GetLayer(0).Pixel[x, y])^ := byte(hsl.hue shr 8);
              PByte(L.GetLayer(1).Pixel[x, y])^ := byte(hsl.saturation shr 8);
              PByte(L.GetLayer(2).Pixel[x, y])^ := byte(hsl.lightness shr 8);
            end;
            cc_BGR:
            begin
              PByte(L.GetLayer(0).Pixel[x, y])^ := rgb.blue;
              PByte(L.GetLayer(1).Pixel[x, y])^ := rgb.green;
              PByte(L.GetLayer(2).Pixel[x, y])^ := rgb.red;
            end;
          end;
        end;
      Result := L;
    end
    else
    begin
      Result := TBaseBitmap.Create;
      with Result do
      begin
        Width := Source.Width;
        Height := Source.Height;
        PixelSize := 1;
        Tag := Ord(ALayer);
      end;
      for y := 0 to Source.Height - 1 do
        for x := 0 to Source.Width - 1 do
        begin
          // TODO Optimize with scanline
          rgb := ColorToBGRA(Source.Canvas.Pixels[x, y]);
          case ALayer of
            cc_hue, cc_saturation, cc_lightness:
            begin
              hsl := BGRAToHSLA(rgb);
              case ALayer of
                cc_hue: PByte(Result.Pixel[X, Y])^ := byte(hsl.hue shr 8);
                cc_saturation: PByte(Result.Pixel[X, Y])^ := byte(hsl.saturation shr 8);
                cc_lightness: PByte(Result.Pixel[X, Y])^ := byte(hsl.lightness shr 8);
                else
                  Assert(False);
              end;
            end
            else
              case ALayer of
                cc_red: PByte(Result.Pixel[X, Y])^ := rgb.red;
                cc_blue: PByte(Result.Pixel[X, Y])^ := rgb.blue;
                cc_green: PByte(Result.Pixel[X, Y])^ := rgb.green;
                cc_intencity: PByte(Result.Pixel[X, Y])^ :=
                    byte(GetIntensity(rgb) shr 8);
                else
                  PByte(Result.Pixel[X, Y])^ := byte(GetLightness(rgb) shr 8);
              end;
          end;
        end;
    end;
  end;
end;

function MakeBitmap(Source: IBaseBitmap): TBitmap;
var
  rgb: TBGRAPixel;
  L: IBaseBitmapLayer;
  x, y: integer;
begin
  Result := TBitmap.Create;
  try
    Result.Width := Source.Width;
    Result.Height := Source.Height;
    Result.PixelFormat := pf24bit;
    case Source.Tag of
      Ord(cc_HSL), Ord(cc_BGR):
      begin
        if Source.QueryInterface(IBaseBitmapLayer, L) <> S_OK then
          raise Exception.Create('Interface IBaseBitmapLayer not supprted');
        Assert(L.GetLayer(0).tag in [Ord(cc_blue), Ord(cc_hue)]);
        Assert(L.GetLayer(1).tag in [Ord(cc_green), Ord(cc_saturation)]);
        Assert(L.GetLayer(2).tag in [Ord(cc_red), Ord(cc_lightness)]);
        for y := 0 to Source.Height - 1 do
          for x := 0 to Source.Width - 1 do
          begin
            rgb.blue := PByte(L.GetLayer(0).Pixel[x, y])^;
            rgb.green := PByte(L.GetLayer(1).Pixel[x, y])^;
            rgb.red := PByte(L.GetLayer(2).Pixel[x, y])^;
            Result.Canvas.Pixels[x, y] := BGRAToColor(rgb);
          end;
        L := nil;
      end;
      else
        for y := 0 to Source.Height - 1 do
          for x := 0 to Source.Width - 1 do
          begin
            rgb.blue := PByte(Source.Pixel[x, y])^;
            rgb.green := rgb.blue;
            rgb.red := rgb.green;
            Result.Canvas.Pixels[x, y] := BGRAToColor(rgb);
          end;
    end;
  except
    on e: Exception do
    begin
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

end.
