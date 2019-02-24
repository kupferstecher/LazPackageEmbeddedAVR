{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazPackageEmbeddedAVR;

{$warn 5023 off : no warning about unused units}
interface

uses
  LazPackageEmbeddedAVR_Intf, dlgNewAVRProject, uAVRProject, uDeviceLoader, 
  UStrings, uCOMUtils, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LazPackageEmbeddedAVR_Intf', 
    @LazPackageEmbeddedAVR_Intf.Register);
end;

initialization
  RegisterPackage('LazPackageEmbeddedAVR', @Register);
end.
