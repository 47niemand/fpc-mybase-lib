{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DmBasePackage;

interface

uses
  uBaseAppUtils, uBaseConsts, uBaseThreadPool, utimers, uVariantUtils, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('DmBasePackage', @Register);
end.
