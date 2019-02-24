/////////////////////////////////////////////////////////////////////////////
//
//   LazPackageEmbeddedAVR - uAVRProject
//
//   Author: kupferstecher
//
//   Description:
//   + Routines for loading template files, storing project options and
//     writing project files
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

unit uAVRProject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type TAVRProject = class
   public
    class var Name: String;
    class var Path: String;
    class var Device: String;
    class var InstructionSet: String;
    class var ProgrammerCommand: string;
    class var ProgrammerNeedsPort: Boolean;
    class var ProgrammerPort: string;
   public
    class var PackagePath: string;          //Path to be initialized from outside before Write___File procedures are called
   private
    class var ProjectFile: TStringList;   //CLEANUP
   public
    Class Procedure CreateProjectDirectory(var Success:Boolean);
    Class procedure WriteProjectFile;
    Class procedure WriteProgrammFile(AFileData: TStringList);
    Class procedure WriteInterruptFile(AFileData: TStringList);
    Class Procedure AddServiceRoutines(AIrqFile:TStringList);
    Class Function CommandAdaption(InString: String):String;

end;

Type TTemplateWriter = class
   public
    class var CompileOptions: string;
   public

    Class Procedure CreateProjectFile(AProjFile:TStringList);

   private
    Class procedure ListReplaceToken(AList:TStringList;AToken,AReplacement:String);

end;

//##############################################################################

IMPLEMENTATION
uses
  Dialogs, uStrings, PackageLinkIntf;


//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Class Procedure TAVRProject.CreateProjectDirectory(var Success:Boolean);
begin

  //Create Folder
  Path:= Path+DirectorySeparator+'L'+Name;
  Success:= CreateDir(Path);

end;

//---------------------------------------------------------
Class procedure TAVRProject.WriteProjectFile;
var
  prFile: TextFile;
  ii: Integer;
begin
  AssignFile(prFile, Path+DirectorySeparator+Name+'.lpi');

  try
    Rewrite(prFile);
    for ii:= 0 to ProjectFile.Count-1
    do writeln(prFile,ProjectFile.Strings[ii]);

    CloseFile(prFile);
  except
    ShowMessage('Error! '+'Error while writing project file.');
  end;

end;

//---------------------------------------------------------
Class procedure TAVRProject.WriteProgrammFile(AFileData: TStringList);
begin
  //Load template file
  AFileData.LoadFromFile(PackagePath+'Templates'+DirectorySeparator+'ProgramTemplate_lpr');
  TTemplateWriter.ListReplaceToken(AFileData,'$(LAZWIZProjectName)',TAVRProject.Name);

end;

//---------------------------------------------------------
Class procedure TAVRProject.WriteInterruptFile(AFileData: TStringList);
begin
  //Load template file
  try AFileData.LoadFromFile(PackagePath+'Templates'+DirectorySeparator+'uInterruptsTemplate_pas');
  except on e:Exception do EXIT; end;
  TTemplateWriter.ListReplaceToken(AFileData,'$(LAZWIZDevice)',TAVRProject.Device);

  AddServiceRoutines(AFileData);

end;

//---------------------------------------------------------
Class Procedure TAVRProject.AddServiceRoutines(AIrqFile:TStringList);
var
  FileLine: string;
  ISRName, ISRComment: String;
  srcFile: TextFile;
  pp1,pp2: Integer;
begin
  AssignFile(srcFile, PackagePath+'Devices'+DirectorySeparator+Device+'.pp');

  try
    Reset(srcFile); //Try to open for reading
  except
    ShowMessage('Error! '+'Couldn''t open device specific file.');
  end;

  While Not EOF(srcFile) Do Begin
    ReadLn(srcFile, FileLine);
    if length(FileLine) < 20 then CONTINUE;
    if not EqualLower(FileLine,'procedure',1) then CONTINUE;
    if Index(FileLine,'ISR',10) = 0 then CONTINUE;

    pp1:= Index(FileLine,'external name',10);
    if pp1 = 0 then CONTINUE;

    pp1:= Scan(FileLine,'''',pp1+13);
    pp2:= Scan(FileLine,'''',pp1+1);

    if (pp1 = 0) or (pp2 = 0) then CONTINUE;

    ISRName:= CopyUntil(FileLine,pp1+1,pp2-1);

    pp1:= Index(FileLine,'//',pp2+1);

    if pp1 > 0
    then ISRComment:= CopyFrom(FileLine,pp1+2)
    else ISRComment:= ISRName;
    ISRComment:= trim(ISRComment);


    AIrqFile.Add('//--- '+ISRComment+' ---');
    AIrqFile.Add('{Procedure '+ISRName+'; Alias: '''+ISRName+'''; Interrupt; Public;');
    AIrqFile.Add('begin');
    AIrqFile.Add('');
    AIrqFile.Add('end; }');
    AIrqFile.Add('');

  End;//While

  CloseFile(srcFile);

  AIrqFile.Add('');
  AIrqFile.Add('end.');
  AIrqFile.Add('');

end;

//---------------------------------------------------------
Class Function TAVRProject.CommandAdaption(InString: String):String;
var pp1: Integer;
begin
  Result:= InString;
  pp1:= Index(InString,'$(LAZWIZPort)');
  if pp1 = 0 then ProgrammerNeedsPort:= false else ProgrammerNeedsPort:= true;

  Result:= ReplaceToken(Result,'$(LAZWIZProject)',Name);
  Result:= ReplaceToken(Result,'$(LAZWIZDevice)',Device);
  Result:= ReplaceToken(Result,'$(LAZWIZPort)',ProgrammerPort);

end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Class Procedure TTemplateWriter.CreateProjectFile(AProjFile:TStringList);
begin
  AProjFile.LoadFromFile('Templates'+DirectorySeparator+'ProjectTemplate_lpi');

  ListReplaceToken(AProjFile,'$(LAZWIZProjectName)',TAVRProject.Name);
  ListReplaceToken(AProjFile,'$(LAZWIZInstructionSet)',TAVRProject.InstructionSet);
  ListReplaceToken(AProjFile,'$(LAZWIZCompileOptions)','-Wp'+TAVRProject.Device+' -XPavr-embedded- -al');
  ListReplaceToken(AProjFile,'$(LAZWIZProgCommand)',TAVRProject.ProgrammerCommand);
  ListReplaceToken(AProjFile,'$(LAZWIZPathDelim)',DirectorySeparator);

end;

//---------------------------------------------------------
Class procedure TTemplateWriter.ListReplaceToken(AList:TStringList;AToken,AReplacement:String);
var
  ii,pp1: Integer;
  tmpStr: String;
begin
  for ii:= 0 to AList.Count-1 do begin
    pp1:= Index(AList.Strings[ii],AToken);
    if pp1 = 0 then CONTINUE;

    tmpStr:= CopyUntil(AList.Strings[ii],1,pp1-1) + AReplacement +
      CopyUntil(AList.Strings[ii],pp1+length(AToken),length(AList.Strings[ii]));

    AList.Strings[ii]:= tmpStr;

  end;//do

end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
INITIALIZATION
  TAVRProject.Name:= 'avrproject';
  TAVRProject.ProgrammerPort:= 'COM1';

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
end.
