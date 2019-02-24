/////////////////////////////////////////////////////////////////////////////
//
//   LazPackageEmbeddedAVR - LazPackageEmbeddedAVR_Intf
//
//   Author: kupferstecher
//
/////////////////////////////////////////////////////////////////////////////
//                                                                         //
//   This source is free software; you can redistribute it and/or modify   //
//   it under the terms of the GNU General Public License as published by  //
//   the Free Software Foundation; either version 2 of the License, or     //
//   (at your option) any later version.                                   //
//                                                                         //
//   This code is distributed in the hope that it will be useful, but      //
//   WITHOUT ANY WARRANTY; without even the implied warranty of            //
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU      //
//   General Public License for more details.                              //
//                                                                         //
//   A copy of the GNU General Public License is available on the World    //
//   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      //
//   obtain it by writing to the Free Software Foundation, Inc.            //
//                                                                         //
/////////////////////////////////////////////////////////////////////////////

unit LazPackageEmbeddedAVR_Intf;

{$mode objfpc}{$H+}

INTERFACE

uses
  Classes, SysUtils, Forms, Controls,
  LazIDEIntf, CompOptsIntf, ProjectIntf;

const
  cPackageName = 'LazPackageEmbeddedAVR';
  cPkgFileName = 'lazpackageembeddedavr.lpk';

procedure Register;

Type

{ TAvrEmbeddedApplicationDescriptor }

 TAvrEmbeddedApplicationDescriptor = class(TProjectDescriptor)
  private

  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function DoInitDescriptor: TModalResult; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
end;

var
  ProjectDescrAVREmbedded: TAvrEmbeddedApplicationDescriptor;



//#############################################################################
IMPLEMENTATION
uses
  FileUtil, Dialogs, PackageLinkIntf, PackageIntf, SrcEditorIntf,
  uAVRProject, dlgNewAVRProject, uStrings;

procedure Register;
begin
  ProjectDescrAVREmbedded := TAvrEmbeddedApplicationDescriptor.Create;
  RegisterProjectDescriptor(ProjectDescrAVREmbedded);
end;

{ TAvrEmbeddedApplicationDescriptor }

constructor TAvrEmbeddedApplicationDescriptor.Create;
begin
  inherited Create;
  Name:= 'AVR Embedded Project';
end;

function TAvrEmbeddedApplicationDescriptor.GetLocalizedName: string;
begin
  Result:= 'AVR Embedded Project';
end;

function TAvrEmbeddedApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result:= 'AVR Embedded Project' + LineEnding +
  'Lazarus project for AVR microcontrollers (ATMega and ATTiny types).'+ LineEnding +
  'Compilation to hex-files.';
end;

function TAvrEmbeddedApplicationDescriptor.DoInitDescriptor: TModalResult;
var
  ThisPackage: TPackageLink;
  PackPath: string;
begin
  TAVRProject.Name:= 'avrproject';
  TAVRProject.Path:= '';
  TAVRProject.Device:= 'atmega32';
  TAVRProject.InstructionSet:= 'avr5';
  TAVRProject.ProgrammerCommand:= '';
  TAVRProject.ProgrammerNeedsPort:= false;

  ////Package Path
  ThisPackage:= PkgLinks.FindLinkWithPkgName(cPackageName);
  PackPath:= ThisPackage.LPKFilename;
  TAVRProject.PackagePath:= '';
  if length(PackPath) > length(cPkgFileName)+3 then
   if equal(PackPath,cPkgFileName,length(PackPath)-length(cPkgFileName)+1)
   then TAVRProject.
          PackagePath:= copy(PackPath,1,length(PackPath)-length(cPkgFileName));

  ////New project dialog
  if not assigned(dlgAVRProjForm) then dlgAVRProjForm:= TdlgAVRProjForm.Create(nil);
  dlgAVRProjForm.Init;

  Result:= dlgAVRProjForm.ShowModal;

end;

function TAvrEmbeddedApplicationDescriptor.InitProject(AProject: TLazProject): TModalResult;
begin
  inherited InitProject(AProject);

  AProject.Title := TAVRProject.Name;

  AProject.ProjectInfoFile := TAVRProject.Path + DirectorySeparator +
                                          Lowercase(TAVRProject.Name) + '.lpi';

  AProject.LazCompilerOptions.TargetOS:= 'embedded';
  AProject.LazCompilerOptions.TargetCPU:= 'avr';
  AProject.LazCompilerOptions.TargetProcessor:= TAVRProject.InstructionSet;

  AProject.LazCompilerOptions.CustomOptions:= '-Wp'+TAVRProject.Device+' -XPavr-embedded- -al';

  AProject.Flags:= AProject.Flags - [pfRunnable];
  AProject.LazCompilerOptions.UseAnsiStrings:= false;
  AProject.LazCompilerOptions.GenerateDebugInfo:= false;

  AProject.LazCompilerOptions.ExecuteAfter.Command:= TAVRProject.ProgrammerCommand;
  AProject.LazCompilerOptions.ExecuteAfter.CompileReasons:= [crBuild]+[crRun];
  AProject.LazCompilerOptions.ExecuteBefore.CompileReasons:= [crCompile]+[crBuild]+[crRun];

   Result:= mrOK;
end;

function TAvrEmbeddedApplicationDescriptor.CreateStartFiles(
  AProject: TLazProject): TModalResult;
var
  NewSource,InterruptsSource: TStringList;
  MainFile: TLazProjectFile;
  ProjFileName,IrqFileName: string;
  SourceWindow: TSourceEditorInterface;
begin
  Result:=inherited CreateStartFiles(AProject);

  ////Programm file
  NewSource:= TStringList.Create;
  NewSource.Add('eins');
  TAVRProject.WriteProgrammFile(NewSource); //Init source Code

  ProjFileName:= TAVRProject.Path+DirectorySeparator+Lowercase(TAVRProject.Name)+'.lpr';
  MainFile := AProject.CreateProjectFile(ProjFileName);
  MainFile.SetSourceText(NewSource.Text);
  MainFile.IsPartOfProject := True;

  AProject.AddFile(MainFile, False {NOT Added To Project Uses Clause});
  AProject.MainFileID := 0;


  ////Interrupt file
  InterruptsSource:= TStringList.Create;
  TAVRProject.WriteInterruptFile(InterruptsSource);
  IrqFileName:= TAVRProject.Path+DirectorySeparator+Lowercase('uInterrupts')+'.pas';
  InterruptsSource.SaveToFile(IrqFileName);
  InterruptsSource.Free;

  LazarusIDE.DoOpenEditorFile(ProjFileName,-1,-1,[ofOnlyIfExists,ofQuiet]);
  LazarusIDE.DoOpenEditorFile(IrqFileName,-1,-1,[ofOnlyIfExists,ofQuiet]);

  SourceEditorManagerIntf.ActiveEditor:= SourceEditorManagerIntf.SourceEditors[0];

  LazarusIDE.DoSaveAll([]);
end;

end.



