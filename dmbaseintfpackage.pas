{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DmBaseIntfPackage;

interface

uses
  uBaseEntry, uBaseInterface, uBaseList, uBaseListHelper, uBaseMap, 
  uBaseMapUtils, uBaseObserver, uBaseSet, uBaseValue, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('DmBaseIntfPackage', @Register);
end.
