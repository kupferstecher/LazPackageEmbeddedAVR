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
    Class var ProgrammerListDude_Names: TStringList;
    Class var ProgrammerListDude_Commands: TStringList;
    Class var ProgrammerListOthers_Names: TStringList;
    Class var ProgrammerListOthers_Commands: TStringList;
   private
    Class var SearchResult: TSearchRec;

   public
    Class Procedure LoadDevices;
    Class Procedure LoadInstructionSetDefinitions;
    Class Function GetInstructionSet(Device: String): String;
   public
    Class Procedure LoadProgAvrdude;
    Class Procedure LoadProgOthers;
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
  FileLines: TStringList;
  CurrentLine,NameString,CommandString: string;
  pp1,pp2: Integer;
begin

  ProgrammerListDude_Names:= TStringList.Create;
  ProgrammerListDude_Commands:= TStringList.Create;

  FileLines:= TStringList.Create;

  try
    FileLines.LoadFromFile(TAVRProject.PackagePath+'Programmer'
                                         + DirectorySeparator + 'AVRDude.dat');
  except on e:Exception do begin
    ShowMessage('Error: Couldn''t load programmer definition files for AVRDude. Installation error?');
    FileLines.Free; EXIT;
  end;end;//try

  For CurrentLine in FileLines Do Begin

    if length(trim(CurrentLine)) = 0 then CONTINUE;
    if SCAN(trimleft(CurrentLine),'#') = 1 then CONTINUE;
    pp1:= SCAN(CurrentLine,'"');       if pp1 = 0 then CONTINUE;
    pp2:= SCAN(CurrentLine,'"',pp1+1); if pp2 = 0 then CONTINUE;
    NameString:= trim(CopyUntil(CurrentLine,pp1+1,pp2-1));
    if length(NameString) = 0 then CONTINUE;

    pp1:= SCAN(CurrentLine,'"',pp2+1); if pp1 = 0 then CONTINUE;
    pp2:= SCAN(CurrentLine,'"',pp1+1); if pp2 = 0 then CONTINUE;
    CommandString:= trim(CopyUntil(CurrentLine,pp1+1,pp2-1));
    if length(CommandString) = 0 then CONTINUE;

    ProgrammerListDude_Names.Add(NameString);
    ProgrammerListDude_Commands.Add(CommandString);

  End;//Do

  FileLines.Free;

end;

//-------------------------------------------------------------------
Class Procedure TDeviceLoader.LoadProgOthers;
var
  FileLines: TStringList;
  CurrentLine,NameString,CommandString: string;
  pp1,pp2: Integer;
begin

  ProgrammerListOthers_Names:= TStringList.Create;
  ProgrammerListOthers_Commands:= TStringList.Create;

  FileLines:= TStringList.Create;

  try
    FileLines.LoadFromFile(TAVRProject.PackagePath+'Programmer'
                                         + DirectorySeparator + 'Others.dat');
  except on e:Exception do begin
    ShowMessage('Error: Couldn''t load programmer definition files. Installation error?');
    FileLines.Free; EXIT;
  end;end;//try

  For CurrentLine in FileLines Do Begin

    if length(trim(CurrentLine)) = 0 then CONTINUE;
    if SCAN(trimleft(CurrentLine),'#') = 1 then CONTINUE;
    pp1:= SCAN(CurrentLine,'"');       if pp1 = 0 then CONTINUE;
    pp2:= SCAN(CurrentLine,'"',pp1+1); if pp2 = 0 then CONTINUE;
    NameString:= trim(CopyUntil(CurrentLine,pp1+1,pp2-1));
    if length(NameString) = 0 then CONTINUE;

    pp1:= SCAN(CurrentLine,'"',pp2+1); if pp1 = 0 then CONTINUE;
    pp2:= SCAN(CurrentLine,'"',pp1+1); if pp2 = 0 then CONTINUE;
    CommandString:= trim(CopyUntil(CurrentLine,pp1+1,pp2-1));
    if length(CommandString) = 0 then CONTINUE;

    ProgrammerListOthers_Names.Add(NameString);
    ProgrammerListOthers_Commands.Add(CommandString);

  End;//Do

  FileLines.Free;

end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

end.

