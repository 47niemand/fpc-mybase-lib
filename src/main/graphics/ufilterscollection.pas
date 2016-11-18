unit uFiltersCollection;

{$mode delphi}

interface

uses
  Classes, SysUtils, uConvolutionMatrix, uBaseBitmapFilter, uBaseBitmap, uBasePixel;

//type


//function FilterBoxBlur: TCustomBaseBitmapFilter;
//function FilterGaussianBlur: TCustomBaseBitmapFilter;
//function FilterSharpen: TCustomBaseBitmapFilter;
//function FilterSharpen: TCustomBaseBitmapFilter;
//function FilterEdgeLow: TCustomBaseBitmapFilter;
//function FilterEdgeLow: TCustomBaseBitmapFilter;
//function FilterEdgeMed: TCustomBaseBitmapFilter;
//function FilterEdgeHigh: TCustomBaseBitmapFilter;
//function FilterUnSharp: TCustomBaseBitmapFilter;
//function FilterBinarize: TCustomBaseBitmapFilter;
//function FilterPosterize: TCustomBaseBitmapFilter;




implementation

type

  { TAbstractConvolutionFilter }

  TAbstractConvolutionFilter = class(TCustomBaseBitmapFilter)
  private
    FKind: TConvolutionKind;
    M: TConvolutionMatrix;
    procedure SetKind(AValue: TConvolutionKind);
  protected
    procedure InitMatrix; virtual; abstract;
    procedure DoFilter(const input, output: IBaseBitmap); override;
    property Kind: TConvolutionKind read FKind write SetKind;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;


{ TAbstractConvolutionFilter }

procedure TAbstractConvolutionFilter.SetKind(AValue: TConvolutionKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  if Assigned(M) then
    M := nil;
end;

procedure TAbstractConvolutionFilter.DoFilter(const input, output: IBaseBitmap);
begin
  if not Assigned(M) then
    InitMatrix;
  M.ApplyConvolutionMatrix(input, output);
end;

constructor TAbstractConvolutionFilter.Create;
begin
  inherited Create;
  M := nil;
end;

destructor TAbstractConvolutionFilter.Destroy;
begin
  FreeAndNil(M);
  inherited Destroy;
end;

end.

