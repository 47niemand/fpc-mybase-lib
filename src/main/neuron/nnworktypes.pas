unit NNWorkTypes;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TLayerType = (NNW_ALL, NNW_INPUT, NNW_HIDDEN, NNW_OUTPUT);

  DType = double;
  PDType = ^DType;
  TVarArrayOfDType = array of DType;
  PVarArrayOfDType = ^TVarArrayOfDType;

  TVarPointerArray = array of pointer;
  PVarPointerArray = ^TVarPointerArray;


implementation

end.

