/////////////////////////////////////////////////////////////////////////////
//
//   LazPackageEmbeddedAVR - dlgAVRProjForm
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

unit dlgNewAVRProject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, EditBtn, Grids, Buttons, uAVRProject, uDeviceLoader, uCOMUtils;

type

  { TdlgAVRProjForm }

  TdlgAVRProjForm = class(TForm)
    ButtonPortsUpdate: TButton;
    ButtonClose: TButton;
    ButtonConfirmMessage: TButton;
    ButtonNext: TButton;
    ButtonCreate: TButton;
    ButtonBack: TButton;
    ComboBoxPort: TComboBox;
    ComboBoxProgrammerDude: TComboBox;
    ComboBoxDevice: TComboBox;
    ComboBoxInstrSet: TComboBox;
    ComboBoxProgrammerOthers: TComboBox;
    ComboBoxSoftware: TComboBox;
    DirectoryEdit1: TDirectoryEdit;
    EditProjName: TEdit;
    EditCommandLine: TEdit;
    Label1: TLabel;
    LabelHelpProgrammer: TLabel;
    Label14: TLabel;
    Label16: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelHelpController: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    MemoMessageTitle: TMemo;
    MemoMessage: TMemo;
    Notebook1: TNotebook;
    Page1: TPage;
    Page2: TPage;
    Page3: TPage;
    Page4: TPage;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    PanelCOMPortList: TPanel;
    PanelWarning: TPanel;
    PanelCommandLine: TPanel;
    PanelPort: TPanel;
    PanelProgrammerDude: TPanel;
    PanelProgrammerOthers: TPanel;
    PanelUtility: TPanel;
    Shape10: TShape;
    Shape11: TShape;
    Shape12: TShape;
    Shape13: TShape;
    Shape14: TShape;
    Shape15: TShape;
    Shape16: TShape;
    Shape17: TShape;
    Shape18: TShape;
    Shape20: TShape;
    Shape21: TShape;
    Shape22: TShape;
    Shape25: TShape;
    Shape26: TShape;
    Shape34: TShape;
    Shape35: TShape;
    StringGridPorts: TStringGrid;
    TrennlinieProg: TShape;
    Shape19: TShape;
    Shape23: TShape;
    Shape24: TShape;
    Shape28: TShape;
    Shape29: TShape;
    Shape30: TShape;
    Shape31: TShape;
    Shape32: TShape;
    Shape33: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape8: TShape;
    Shape7: TShape;
    Shape9: TShape;
    procedure ButtonPortsUpdateClick(Sender: TObject);
    procedure ButtonBackClick(Sender: TObject);
    procedure ButtonConfirmMessageClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonCreateClick(Sender: TObject);
    procedure ButtonNextClick(Sender: TObject);
    procedure ComboBoxDeviceChange(Sender: TObject);
    procedure ComboBoxPortChange(Sender: TObject);
    procedure ComboBoxProgrammerDudeChange(Sender: TObject);
    procedure ComboBoxProgrammerOthersChange(Sender: TObject);
    procedure ComboBoxSoftwareChange(Sender: TObject);
    procedure DirectoryEdit1AcceptDirectory(Sender: TObject; var Value: String);
    procedure DirectoryEdit1EditingDone(Sender: TObject);
    procedure EditProjNameEditingDone(Sender: TObject);
    procedure LabelHelpControllerClick(Sender: TObject);
    procedure LabelHelpControllerMouseEnter(Sender: TObject);
    procedure LabelHelpControllerMouseLeave(Sender: TObject);
    procedure LabelHelpProgrammerClick(Sender: TObject);
    procedure LabelHelpProgrammerMouseEnter(Sender: TObject);
    procedure LabelHelpProgrammerMouseLeave(Sender: TObject);
    procedure StringGridPortsClick(Sender: TObject);

  public
    Procedure ReadInputFields(Sender:TObject);
    Procedure Init;                                 //Called on Startup
    Procedure ProgrammerSelect;
    Procedure PromptMessage(Title,Message:String);  //Shows error message in main form
  private
    ErrorDone: Boolean;                             //Flag for error screen
    procedure PortsListUpdate;
  end;

var
  dlgAVRProjForm: TdlgAVRProjForm;

implementation

{$R *.lfm}

{ TForm1 }


procedure TdlgAVRProjForm.DirectoryEdit1AcceptDirectory(Sender: TObject;
  var Value: String);
begin
  TAVRProject.Path:= DirectoryEdit1.Directory;
end;

procedure TdlgAVRProjForm.DirectoryEdit1EditingDone(Sender: TObject);
begin
  TAVRProject.Path:= DirectoryEdit1.Directory;
end;

procedure TdlgAVRProjForm.EditProjNameEditingDone(Sender: TObject);
begin
  TAVRProject.Name:= EditProjName.Text;
end;

procedure TdlgAVRProjForm.LabelHelpControllerClick(Sender: TObject);
begin
  PromptMessage('How to add controllers',
    'If the FreePascal compiler supports controllers that are not yet listed in the '
    +'dropdown menu then you can add the device yourself.'+#13+#10
    +'Therefore copy the device specific file from your FreePascal installation '
    +'to the subdirectory ''Devices'' of the package installation place. '
    +'This is where you unpacked the package files before you started the package installation. '
    +'The device specific file has the name of the device and the file extension .pp, e.g. ''atmega32.pp''. '
    +'The files can be found in the fpc source directory under fpc/rtl/embedded/avr. '
    +'Next time you start the new AVR project dialog, the controller will '
    +'be listed automatically, no recompilation of the package needed.' +#13+#10
    +'The device specific source file ''uInterrupts.pas'' will be generated based '
    +'on the device specific file.'+#13+#10
    +'To connect the correct instruction set to the device (optional), the file '
    +'''InstructionSetDefinitions.dat'' in the Subdirectory ''Devices'' has to be modified, '
    +'just add a line with the end of the file with the device name and the '
    +'instruction set seperated by a space character.');

end;

procedure TdlgAVRProjForm.LabelHelpControllerMouseEnter(Sender: TObject);
begin
  LabelHelpController.Font.Style:= LabelHelpController.Font.Style + [fsUnderline];
end;

procedure TdlgAVRProjForm.LabelHelpControllerMouseLeave(Sender: TObject);
begin
  LabelHelpController.Font.Style:= LabelHelpController.Font.Style - [fsUnderline];
end;

procedure TdlgAVRProjForm.LabelHelpProgrammerClick(Sender: TObject);
begin
  PromptMessage('How to add programmers',
    'Programmers can be added easily.'+#13+#10
    +'For examples check the subfolder ''Programmer'' in the package installation directory. '
    +'This is where you unpacked the files before you started the package installation.'+#13+#10
    +'For a new programmer add a line in the definition files ''AVRDude.dat'' or ''Others.dat''. '
    +'The first string in quotation marks is the name of the programmer displayed '
    +'in the pulldown menu. The second string in quotation marks is the '
    +'command line for the programmer.'+#13+#10
    +'Next time you start the new AVR project dialog, your programmer will '
    +'be listed automatically, no recompilation needed.' +#13+#10
    +#13+#10
    +'Also see the README.TXT in the ''Programmer'' subdirectory.');

end;

procedure TdlgAVRProjForm.LabelHelpProgrammerMouseEnter(Sender: TObject);
begin
  LabelHelpProgrammer.Font.Style:= LabelHelpProgrammer.Font.Style + [fsUnderline];
end;

procedure TdlgAVRProjForm.LabelHelpProgrammerMouseLeave(Sender: TObject);
begin
  LabelHelpProgrammer.Font.Style:= LabelHelpProgrammer.Font.Style - [fsUnderline];
end;

procedure TdlgAVRProjForm.StringGridPortsClick(Sender: TObject);
begin
  if StringGridPorts.Col <> 0 then EXIT;
  if StringGridPorts.Row > StringGridPorts.RowCount then EXIT;
  if StringGridPorts.Cells[0,StringGridPorts.Row] = '-' then EXIT;

  ComboBoxPort.Text:= StringGridPorts.Cells[0,StringGridPorts.Row];
  ComboBoxPortChange(ComboBoxPort);
end;

procedure TdlgAVRProjForm.ButtonNextClick(Sender: TObject);
begin
  ReadInputFields(Self);
  //ProgrammerSelect;
  Notebook1.PageIndex:= 1;
  ProgrammerSelect;
end;

procedure TdlgAVRProjForm.ComboBoxDeviceChange(Sender: TObject);
var tmpString: String;
begin

  tmpString:= TDeviceLoader.GetInstructionSet(ComboboxDevice.Text);

  if tmpString <> '' then begin
    TAVRProject.InstructionSet:= tmpString;
    ComboboxInstrSet.Text:= tmpString;
    ComboBoxInstrSet.Style:= csDropDownList;
    ComboboxInstrSet.Enabled:= false;
    PanelWarning.Visible:= false;
  end else begin

    ComboboxInstrSet.Text:= '';
    ComboBoxInstrSet.Style:= csDropDown;
    ComboboxInstrSet.Enabled:= true;
    PanelWarning.Visible:= true;
  end;//if


end;

procedure TdlgAVRProjForm.ComboBoxPortChange(Sender: TObject);
begin
  TAVRProject.ProgrammerPort:= ComboBoxPort.Text;
  ProgrammerSelect;
end;

procedure TdlgAVRProjForm.ComboBoxProgrammerDudeChange(Sender: TObject);
begin
  ProgrammerSelect;
end;

procedure TdlgAVRProjForm.ComboBoxProgrammerOthersChange(Sender: TObject);
begin
  ProgrammerSelect;
end;

procedure TdlgAVRProjForm.ComboBoxSoftwareChange(Sender: TObject);
begin
  ProgrammerSelect;
end;

procedure TdlgAVRProjForm.ButtonBackClick(Sender: TObject);
begin
  Notebook1.PageIndex:= 0;
end;

procedure TdlgAVRProjForm.ButtonPortsUpdateClick(Sender: TObject);
begin PortsListUpdate; end;

procedure TdlgAVRProjForm.ButtonCloseClick(Sender: TObject);
begin
  dlgAVRProjForm.Close;
end;

procedure TdlgAVRProjForm.ButtonCreateClick(Sender: TObject);
var success: Boolean;
begin

  ReadInputFields(self);

  if TAVRProject.Name = '' then begin
    PromptMessage('Error!','No project name defined.');
    dlgAVRProjForm.Notebook1.PageIndex:= 0;
    EXIT;
  end;//if

  if TAVRProject.Path = '' then begin
    PromptMessage('Error!','No project path defined.');
    dlgAVRProjForm.Notebook1.PageIndex:= 0;
    EXIT;
  end;//if

  TAVRProject.CreateProjectDirectory(success);

  if not success then begin
    PromptMessage('Error!',
      'Couldn''t create project directory at desired location.'+
      #13+#10+#13+#10+'Directory with same name may already exist,'+
      #13+#10+'change project name and delete existing directory.');
    dlgAVRProjForm.Notebook1.PageIndex:= 0;
    EXIT;
  end;//if

  ModalResult:= mrOK;


end;


Procedure TdlgAVRProjForm.ReadInputFields(Sender:TObject);
begin
  TAVRProject.Name:= EditProjName.Text;
  TAVRProject.Path:= DirectoryEdit1.Directory;
  TAVRProject.Device:= ComboBoxDevice.Text;
  TAVRProject.InstructionSet:= ComboBoxInstrSet.Text;
  TAVRProject.ProgrammerCommand:= EditCommandLine.Text;

  if (length(TAVRProject.Path) > 0)
  and (TAVRProject.Path[length(TAVRProject.Path)] = string(DirectorySeparator)[1])
  then TAVRProject.Path:= copy(TAVRProject.Path,1,length(TAVRProject.Path)-1);

end;

Procedure TdlgAVRProjForm.Init;
begin

  TDeviceLoader.LoadDevices;
  TDeviceLoader.DeviceList.Sort;
  ComboBoxDevice.Items.Clear;
  ComboBoxDevice.Items.Assign(TDeviceLoader.DeviceList);
  ComboBoxDevice.ItemIndex:= ComboBoxDevice.Items.IndexOf('atmega32');

  TDeviceLoader.LoadInstructionSetDefinitions;
  ComboboxInstrSet.Items.Clear;
  ComboboxInstrSet.Items.Assign(TDeviceLoader.CollectedInstructionSets);
  ComboboxInstrSet.ItemIndex:= ComboboxInstrSet.Items.IndexOf('avr5');

  TDeviceLoader.LoadProgAvrdude;
  ComboBoxProgrammerDude.Items.Clear;
  ComboBoxProgrammerDude.Items.Assign(TDeviceLoader.ProgrammerListDude_Names);
  ComboBoxProgrammerDude.ItemIndex:= ComboBoxProgrammerDude.Items.IndexOf('stk500v2');

  TDeviceLoader.LoadProgOthers;
  ComboBoxProgrammerOthers.Items.Clear;
  ComboBoxProgrammerOthers.Items.Assign(TDeviceLoader.ProgrammerListOthers_Names);
  ComboBoxProgrammerOthers.ItemIndex:= 0;
  PanelProgrammerOthers.Top:= PanelUtility.Top + 48;
  PanelProgrammerOthers.Visible:= false;

  Notebook1.PageIndex:= 0;

end;

Procedure TdlgAVRProjForm.ProgrammerSelect;
var Command: String;
begin
  //ShowMessage('TdlgAVRProjForm.ProgrammerSelect');

  Case ComboBoxSoftware.ItemIndex of
  0:begin //Custom Command
      PanelProgrammerDude.Visible:= false;
      PanelProgrammerOthers.Visible:= false;
      PanelPort.Visible:= false;
      PanelCOMPortList.Visible:= false;
      PanelCommandLine.Top:= PanelUtility.Top + 48;
      EditCommandLine.Enabled:= true;
    end;
  1:begin //AVR Dude
      PanelProgrammerOthers.Visible:= false;
      PanelProgrammerDude.Visible:= true;
      if ComboBoxProgrammerDude.ItemIndex < 0 then ComboBoxProgrammerDude.ItemIndex:= 0;
      Command:= TDeviceLoader.ProgrammerListDude_Commands.Strings[ComboBoxProgrammerDude.ItemIndex];
      EditCommandLine.Text:= TAVRProject.CommandAdaption(Command);
      if TAVRProject.ProgrammerNeedsPort then begin
        PanelPort.Visible:= true;
        PanelCOMPortList.Visible:= true; PortsListUpdate;
        PanelCommandLine.Top:= PanelPort.Top + 48;
      end else begin
        PanelPort.Visible:= false; PanelCOMPortList.Visible:= false;
        PanelCommandLine.Top:= PanelPort.Top;
      end;//if
      EditCommandLine.Enabled:= false;
    end;
  2:begin //Others
      PanelProgrammerDude.Visible:= false;
      PanelProgrammerOthers.Visible:= true;
      if ComboBoxProgrammerOthers.ItemIndex < 0 then ComboBoxProgrammerOthers.ItemIndex:= 0;
      Command:= TDeviceLoader.ProgrammerListOthers_Commands.Strings[ComboBoxProgrammerOthers.ItemIndex];
      EditCommandLine.Text:= TAVRProject.CommandAdaption(Command);
      if TAVRProject.ProgrammerNeedsPort then begin
        PanelPort.Visible:= true;
        PanelCOMPortList.Visible:= true; PortsListUpdate;
        PanelCommandLine.Top:= PanelPort.Top + 48;
      end else begin
        PanelPort.Visible:= false; PanelCOMPortList.Visible:= false;
        PanelCommandLine.Top:= PanelPort.Top;
      end;//if
      EditCommandLine.Enabled:= false;
    end;
  end;//case

  TrennlinieProg.Top:= PanelCommandLine.Top + 48;

end;

Procedure TdlgAVRProjForm.PromptMessage(Title,Message:String);
var
  PageIndexSave: Integer;
begin
  ErrorDone:= false;
  PageIndexSave:= Notebook1.PageIndex;
  MemoMessageTitle.Clear;
  MemoMessageTitle.Lines.Add(Title);
  MemoMessage.Clear;
  MemoMessage.Lines.Add(Message);
  Notebook1.PageIndex:= 3;
  if dlgAVRProjForm.Visible
  then ButtonConfirmMessage.SetFocus
  else EXIT;


  //Waiting until Butten 'OK' was pressed (Flag ErrorDone is set by button)
  While not ErrorDone do begin
    Application.ProcessMessages;
    sleep(20);
  end;//while

  Notebook1.PageIndex:= PageIndexSave;

end;

procedure TdlgAVRProjForm.ButtonConfirmMessageClick(Sender: TObject);
begin
  ErrorDone:= true;
end;

procedure TdlgAVRProjForm.PortsListUpdate;
var PortStrings: TStringList;
begin
  PortStrings:= TStringList.Create;

  COMUtils.GetCOMPorts(PortStrings);
  StringGridPorts.RowCount:= PortStrings.Count;
  StringGridPorts.Cols[0].Assign(PortStrings);

  PortStrings.Free;

end;

end.

