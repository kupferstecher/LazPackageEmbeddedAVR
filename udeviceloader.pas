/////////////////////////////////////////////////////////////////////////////
//
//   LazPackageEmbeddedAVR - uDeviceLoader
//
//   Author: kupferstecher
//
//   Description:
//   + Routines for loading: - device files
//                           - corresponding instruction sets
//                           - programmer commands
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

unit uDeviceLoader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


Type TDeviceLoader = class
   public
    Class var DeviceList: TStringList;
    Class var InstrSetDevicesList,InstrSetList: TStringList;
    Class var CollectedInstructionSets: TStringList;
    Class var ProgrammerListDude: TStringList;
    Class var ProgrammerListOthers: TStringList;
   private
    Class var SearchResult: TSearchRec;

   public
    Class Procedure LoadDevices;
    Class Procedure LoadInstructionSetDefinitions;
    Class Function GetInstructionSet(Device: String): String;
   public
    Class Procedure LoadProgAvrdude;
    Class Procedure LoadProgOthers;
    Class Function LoadCommand:string;
   private
    Class Procedure LoadFile(AFileName:string; var AFileData: string);
end;

//##############################################################################
IMPLEMENTATION

uses
  Dialogs, uStrings, dlgNewAVRProject, uAVRProject;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Class Procedure TDeviceLoader.LoadDevices;
var
  Status: Integer;
  FileName: string;
  pp1: Integer;
begin
  if not assigned(DeviceList)
  then DeviceList:= TStringList.Create
  else DeviceList.Clear;


  Status:= FindFirst(TAVRProject.PackagePath+'Devices/*.pp',0,SearchResult);

  if Status <> 0 then begin
    ShowMessage('Error: Cannot open device specific files. Installation error?');
    EXIT;
  end;//if

  While (Status = 0) Do Begin
    //Add Device to List
    FileName:= SearchResult.Name;
    pp1:= INDEX(FileName,'.pp');
    if pp1 = 0 then begin ShowMessage('Internal Error: Wrong file extension.'); EXIT; end;
    DeviceList.Add(Copy(FileName,1,pp1-1));

    //Find next Device
    Status:= FindNext(SearchResult);

  End;//Do

  FindClose(SearchResult);

end;

//-------------------------------------------------------------------
Class Procedure TDeviceLoader.LoadInstructionSetDefinitions;
var
  ii,pos: Integer;
  FileData: TStringList;
begin
  FileData:= TStringList.Create;
  InstrSetDevicesList:= TStringList.Create;
  InstrSetList:= TStringList.Create;
  CollectedInstructionSets:= TStringList.Create;


  //Load Data
  try
    FileData.LoadFromFile(TAVRProject.PackagePath+'Devices'+
                           DirectorySeparator+'InstructionSetDefinitions.dat');
  except on e:Exception do begin
    ShowMessage('Error: Couldn''t load file InstructionSetDefinitions.dat. Installation error?');
    FileData.Free; EXIT;
  end;end;//try

  for ii:= 0 to FileData.count-1 do begin
    pos:= Scan(FileData.Strings[ii],' '+#09);
    InstrSetDevicesList.Add(CopyUntil(FileData.Strings[ii],1,pos-1));
    InstrSetList.Add(CopyFrom(FileData.Strings[ii],pos+1));
  end;//end;

  FileData.Free;

  //Extract Instruction Sets
  For ii:= 0 To InstrSetList.count-1 Do Begin
    if CollectedInstructionSets.IndexOf(InstrSetList.Strings[ii]) < 0
    then CollectedInstructionSets.Add(InstrSetList.Strings[ii]);
  End;//Do

end;

//-------------------------------------------------------------------
Class Function TDeviceLoader.GetInstructionSet(Device: String): String;
var
  pp: Integer;
begin
  pp:= InstrSetDevicesList.IndexOf(Device);
  if pp >= 0
  then Result:= InstrSetList.Strings[pp]
  else Result:= '';

end;

//-------------------------------------------------------------------
Class Procedure TDeviceLoader.LoadProgAvrdude;
var
  Status: Integer;
  FileName: string;
begin

  ProgrammerListDude:= TStringList.Create;

  Status:= FindFirst(TAVRProject.PackagePath+'Programmer'+DirectorySeparator
                             +'AVRDude'+DirectorySeparator+'*',0,SearchResult);

  if Status <> 0 then begin
    FindClose(SearchResult);
    ShowMessage('Error: Couldn''t load programmer definition files for AVRDude. Installation error?');
    EXIT;
  end;//if

  While (Status = 0) Do Begin
    //Add Programmer to List
    FileName:= SearchResult.Name;
    ProgrammerListDude.Add(FileName);

    //Find next Device
    Status:= FindNext(SearchResult);

  End;//Do

  FindClose(SearchResult);

end;

//-------------------------------------------------------------------
Class Procedure TDeviceLoader.LoadProgOthers;
var
  Status: Integer;
  FileName: string;
begin

  ProgrammerListOthers:= TStringList.Create;

  Status:= FindFirst(TAVRProject.PackagePath+'Programmer'+DirectorySeparator
                              +'Others'+DirectorySeparator+'*',0,SearchResult);

  if Status <> 0 then begin
    ShowMessage('Error: Couldn''t load programmer definition files. Installation error?');
    FindClose(SearchResult);
    EXIT;
  end;//if

  While (Status = 0) Do Begin
    //Add Programmer to List
    FileName:= SearchResult.Name;
    ProgrammerListOthers.Add(FileName);

    //Find next Device
    Status:= FindNext(SearchResult);

  End;//Do

  FindClose(SearchResult);

end;

//-------------------------------------------------------------------
Class Function TDeviceLoader.LoadCommand:string;
var
  FileName: String;
begin
  Result:= '';
  Case dlgAVRProjForm.ComboBoxSoftware.ItemIndex of
  0:begin //Custom Commandline
    end;
  1:begin //AVR Dude
      if dlgAVRProjForm.ComboBoxProgrammerDude.ItemIndex < 0 then EXIT;
      FileName:=
      ProgrammerListDude.Strings[dlgAVRProjForm.ComboBoxProgrammerDude.ItemIndex];
      FileName:= TAVRProject.PackagePath+'Programmer'+DirectorySeparator+'AVRDude'+DirectorySeparator+FileName;
      LoadFile(FileName,Result);
    end;
  2:begin //Others
      if dlgAVRProjForm.ComboBoxProgrammerOthers.ItemIndex < 0 then EXIT;
      FileName:=
      ProgrammerListOthers.Strings[dlgAVRProjForm.ComboBoxProgrammerOthers.ItemIndex];
      FileName:= TAVRProject.PackagePath+'Programmer'+DirectorySeparator+'Others'+DirectorySeparator+FileName;
      LoadFile(FileName,Result);
    end;
  end;//case

end;

//-------------------------------------------------------------------
Class Procedure TDeviceLoader.LoadFile(AFileName:string; var AFileData: string);
var
  cmdFile: TextFile;
begin
  AFileData:= '';
  try
    AssignFile(cmdFile, AFileName);
    Reset(cmdFile); //Try to open for reading
  except on e:Exception do begin
      dlgAVRProjForm.PromptMessage('Internal Error!','Couldn''t open file:'
      +#13+#10 + ' '+AFilename);
      EXIT;
  end;end;//try

  try
    if not EOF(cmdFile) then ReadLn(cmdFile,AFileData);
  except on e:Exception do begin
    dlgAVRProjForm.PromptMessage('Internal Error!','Couldn''t open file:'
    +#13+#10 + ' '+AFilename);
  end;end;//try

  CloseFile(cmdFile);

end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

end.

