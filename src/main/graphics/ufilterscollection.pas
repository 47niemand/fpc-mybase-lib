unit uFiltersCollection;

{$mode delphi}

interface

uses
  Classes, SysUtils, uBaseBitmapFilter, uBaseBitmap;

function FilterConvBoxBlur: IBaseBitmapFilter;
function FilterConvGaussianBlur: IBaseBitmapFilter;
function FilterConvSharpen: IBaseBitmapFilter;
function FilterConvEdgeLow: IBaseBitmapFilter;
function FilterConvEdgeMed: IBaseBitmapFilter;
function FilterConvEdgeHigh: IBaseBitmapFilter;
function FilterConvUnSharp: IBaseBitmapFilter;
function FilterConvIdentity: IBaseBitmapFilter;

//TODO next
function FilterBinarize: IBaseBitmapFilter;
function FilterPosterize: IBaseBitmapFilter;
function FilterNormalize: IBaseBitmapFilter;

implementation

uses variants, uBaseMap, uBaseInterface, uConvolutionMatrix;

type

  { TConvolutionFilter }

  TConvolutionFilter = class(TCustomBaseBitmapFilter)
  protected
    M: IConvolutionMatrix;
    function InitMatrix(const I: IConvolutionMatrix): IBaseBitmapFilter;
    procedure DoFilter(const input, output: IBaseBitmap); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

var
  FilterCollection: IBaseMap;

function CastVarToBaseBitmapFilter(const V: variant): IBaseBitmapFilter;
begin
  if varisnull(v) or varisEmpty(v) then
    Result := nil
  else
    Result := v;
end;

procedure RegisterFilter(const Name: string; const Intf: IBaseBitmapFilter);
begin
  FilterCollection.Values[Name] := CastInterface(Intf);
end;

function GetFilter(const Name: string): IBaseBitmapFilter;
begin
  CastVarToBaseBitmapFilter(FilterCollection.Values[Name]);
end;



function FilterConvBoxBlur: IBaseBitmapFilter;
const
  _GetName = 'FilterConvBoxBlur';
begin
  Result := GetFilter(_GetName);
  if not Assigned(Result) then
  begin
    Result := TConvolutionFilter.Create.InitMatrix(GetBoxBlurMatrix);
    RegisterFilter(_GetName, Result);
  end;
end;

function FilterConvGaussianBlur: IBaseBitmapFilter;
const
  _GetName = 'FilterConvGaussianBlur';
begin
  Result := GetFilter(_GetName);
  if not Assigned(Result) then
  begin
    Result := TConvolutionFilter.Create.InitMatrix(GetGaussianBlurMatrix);
    RegisterFilter(_GetName, Result);
  end;
end;

function FilterConvSharpen: IBaseBitmapFilter;
const
  _GetName = 'FilterConvSharpen';
begin
  Result := GetFilter(_GetName);
  if not Assigned(Result) then
  begin
    Result := TConvolutionFilter.Create.InitMatrix(GetSharpenMatrix);
    RegisterFilter(_GetName, Result);
  end;
end;

function FilterConvEdgeLow: IBaseBitmapFilter;
const
  _GetName = 'FilterConvEdgeLow';
begin
  Result := GetFilter(_GetName);
  if not Assigned(Result) then
  begin
    Result := TConvolutionFilter.Create.InitMatrix(GetEdgeDetectionMatrixLess);
    RegisterFilter(_GetName, Result);
  end;
end;

function FilterConvEdgeMed: IBaseBitmapFilter;
const
  _GetName = 'FilterConvEdgeMed';
begin
  Result := GetFilter(_GetName);
  if not Assigned(Result) then
  begin
    Result := TConvolutionFilter.Create.InitMatrix(GetEdgeDetectionMatrix);
    RegisterFilter(_GetName, Result);
  end;
end;

function FilterConvEdgeHigh: IBaseBitmapFilter;
const
  _GetName = 'FilterConvEdgeHigh';
begin
  Result := GetFilter(_GetName);
  if not Assigned(Result) then
  begin
    Result := TConvolutionFilter.Create.InitMatrix(GetEdgeDetectionMatrixMore);
    RegisterFilter(_GetName, Result);
  end;
end;

function FilterConvUnSharp: IBaseBitmapFilter;
const
  _GetName = 'FilterConvUnSharp';
begin
  Result := GetFilter(_GetName);
  if not Assigned(Result) then
  begin
    Result := TConvolutionFilter.Create.InitMatrix(GetUnSharpenMatrix);
    RegisterFilter(_GetName, Result);
  end;
end;


function FilterConvIdentity: IBaseBitmapFilter;
const
  _GetName = 'FilterConvIdentity';
begin
  Result := GetFilter(_GetName);
  if not Assigned(Result) then
  begin
    Result := TConvolutionFilter.Create.InitMatrix(GetIdentityMatrix);
    RegisterFilter(_GetName, Result);
  end;
end;

function FilterBinarize: IBaseBitmapFilter;
begin
  //TODO
end;

function FilterPosterize: IBaseBitmapFilter;
begin
  //TODO
end;

function FilterNormalize: IBaseBitmapFilter;
begin
  //TODO
end;

{ TAbstractConvolutionFilter }

function TConvolutionFilter.InitMatrix(const I: IConvolutionMatrix): IBaseBitmapFilter;
begin
  M := I;
  Result := Self;
end;

procedure TConvolutionFilter.DoFilter(const input, output: IBaseBitmap);
begin
  M.ApplyConvolutionMatrix(input, output);
end;

constructor TConvolutionFilter.Create;
begin
  inherited Create;
end;

destructor TConvolutionFilter.Destroy;
begin
  M := nil;
  inherited Destroy;
end;

initialization
  FilterCollection := NewBaseIntfMap;

finalization
  FilterCollection.Clear;
  FilterCollection := nil;
end.
