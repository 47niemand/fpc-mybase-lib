unit uBaseBitmapFilter;

{$mode delphi}

interface

uses
  Classes, variants, SysUtils, contnrs, uBaseBitmap;

type

  { IBaseBitmapFilter }

  IBaseBitmapFilter = interface
    function GetName: string;
    function GetParams: string;
    procedure SetParams(const a: array of variant);
    procedure InitFilter(const args: array of variant);
    procedure ApplyFilter(const input, output: IBaseBitmap);
    property Name: string read GetName;
    property Params: string read GetParams;
  end;

  { TCustomBaseBitmapFilter }

  TCustomBaseBitmapFilter = class(TInterfacedObject, IBaseBitmapFilter)
  protected
    FName: string;
    FParams: array of variant;
    procedure DoFilter(const input, output: IBaseBitmap); virtual; abstract;
  public
    procedure InitFilter(const args: array of variant);
    procedure ApplyFilter(const input, output: IBaseBitmap);
    function GetName: string;
    function GetParams: string;
    procedure SetParams(const a: array of variant);
    property Name: string read GetName;
    property Params: string read GetParams;
    constructor Create; virtual;
  end;

  { TBaseBitmapCompositeFilter }

  TBaseBitmapCompositeFilter = class(TCustomBaseBitmapFilter)
  private
    FList: TList;
    FParams: TObjectList;
    procedure ClearList;
  protected
    procedure DoFilter(const input, output: IBaseBitmap); override;
  public
    procedure Register(const F: IBaseBitmapFilter; const AParams: array of variant);
    constructor Create; override;
    destructor Destroy; override;
  end;


implementation

uses uVariantUtils;

type

  { TCompositeParamContainer }

  TCompositeParamContainer = class
    FParams: array of variant;
    function AsString: string;
    constructor Create(const AParams: array of variant);
  end;

{ TCompositeParamConatiner }

function TCompositeParamContainer.AsString: string;
var
  I: integer;
  T: TStringList;
begin
  T := TStringList.Create;
  try
    for I := 0 to Length(FParams) - 1 do
      T.Add(FParams[I]);
    if T.Count > 0 then
      Result := '(' + T.CommaText + ')'
    else
      Result := '';
  finally
    T.Free;
  end;
end;

constructor TCompositeParamContainer.Create(const AParams: array of variant);
var
  I: integer;
begin
  SetLength(FParams, Length(AParams));
  Assert(Low(FParams) = Low(AParams));
  Assert(High(FParams) = High(AParams));
  for I := low(AParams) to High(AParams) do
    FParams[I] := AParams[I];
end;


{ TBaseBitmapCompositeFilter }

procedure TBaseBitmapCompositeFilter.ClearList;
var
  I: integer;
begin
  for I := 0 to FList.Count - 1 do
    IBaseBitmapFilter(FList.List^[I]) := nil;
  FList.Clear;
end;

procedure TBaseBitmapCompositeFilter.Register(const F: IBaseBitmapFilter;
  const AParams: array of variant);
var
  I: integer;
begin
  Assert(FList.Count = FParams.Count);
  I := FList.Add(nil);
  IBaseBitmapFilter(FList.List^[I]) := F;
  FParams.Add(TCompositeParamContainer.Create(AParams));
  Assert(FList.Count = FParams.Count);
end;

procedure TBaseBitmapCompositeFilter.DoFilter(const input, output: IBaseBitmap);
var
  I: integer;
  A, B, C: IBaseBitmap;
begin
  if FList.Count = 1 then
  begin
    I := 0;
    with IBaseBitmapFilter(FList.Items[I]) do
      ApplyFilter(input, output);
  end
  else
  begin
    A := output.Clone;
    B := nil;
    C := nil;
    try
      for I := 0 to FList.Count - 1 do
        with IBaseBitmapFilter(FList.Items[I]) do
          if I = 0 then
            ApplyFilter(input, A)
          else
          if I = FList.Count - 1 then
            ApplyFilter(A, output)
          else
          begin
            if not Assigned(B) then
              B := A.Clone;
            ApplyFilter(A, B);
            C := B;
            B := A;
            A := C;
            C := nil;
          end;
    finally
      A := nil;
      B := nil;
      C := nil;
    end;
  end;
end;

constructor TBaseBitmapCompositeFilter.Create;
begin
  inherited Create;
  FList := TList.Create;
  FParams := TObjectList.Create(True);
end;

destructor TBaseBitmapCompositeFilter.Destroy;
begin
  ClearList;
  FreeAndNil(FParams);
  FreeAndNil(FList);
  inherited Destroy;
end;

{ TCustomBaseBitmapFilter }

procedure TCustomBaseBitmapFilter.InitFilter(const args: array of variant);
var
  I: integer;
begin
  SetLength(FParams, Length(args));
  Assert(Low(args) = Low(FParams));
  Assert(High(args) = High(FParams));
  for I := Low(args) to High(args) do
    FParams[I] := args[I];
end;

procedure TCustomBaseBitmapFilter.ApplyFilter(const input, output: IBaseBitmap);
begin
  DoFilter(input, output);
end;

function TCustomBaseBitmapFilter.GetName: string;
begin
  Result := FName;
end;

function TCustomBaseBitmapFilter.GetParams: string;
var
  T: TStrings;
  I: integer;
begin
  T := TStringList.Create;
  try
    for I := low(FParams) to High(FParams) do
      T.Add(BaseVarToStr(FParams[I]));
    Result := T.DelimitedText;
  finally
    FreeAndNil(T);
  end;
end;

procedure TCustomBaseBitmapFilter.SetParams(const a: array of variant);
var
  I: integer;
begin
  SetLength(FParams, length(a));
  Assert(low(a) = Low(FParams));
  Assert(High(a) = High(FParams));
  for I := Low(a) to High(a) do
    FParams[I] := a[I];
end;

constructor TCustomBaseBitmapFilter.Create;
begin
  inherited Create;
  FName := Self.ClassName;
end;

end.
