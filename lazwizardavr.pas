{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazWizardAVR;

{$warn 5023 off : no warning about unused units}
interface

uses
  lazwizardavr_intf, dlgNewAVRProject, uAVRProject, uDeviceLoader, UStrings, 
  uCOMUtils, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('lazwizardavr_intf', @lazwizardavr_intf.Register);
end;

initialization
  RegisterPackage('LazWizardAVR', @Register);
end.
