unit uCOMUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type TComUtils = object
  public
    Procedure GetCOMPorts(Ports:TStringList);
  private
    function GetLinuxSerialPortNames: string;
end;

var
  ComUtils: TComUtils;

IMPLEMENTATION

uses
  {$IFDEF Windows} Synaser, {$ENDIF}
  {$IFDEF Linux} BaseUnix, {$ENDIF}
  uStrings;


//-----------------------------------------------
Procedure TComUtils.GetCOMPorts(Ports:TStringList);
var
  portNames:String;
  ii,pp1,pp2:Integer;
begin
  {$IFDEF Windows} PortNames:=GetSerialPortNames; {$ENDIF}
  {$IFDEF Linux}   PortNames:=GetLinuxSerialPortNames; {$ENDIF}
  //cmdln('TComPort.GetPorts: Available Ports:');
  //cmdln(PortNames);

  Ports.Clear;

  If length(PortNames)=0 then begin
    Ports.Append('-');
    EXIT;
  End;//If

  pp1:= 1;
  For ii:=1 to 100 do begin
      pp2:= Scan(Portnames,',',pp1);
      if pp2<>0 then begin //noch weitere Ports
        Ports.Append(CopyUntil(PortNames,pp1,pp2-1));
        //cmdln('/'+CopyUntil(PortNames,pp1,pp2-1)+'/');
        pp1:= pp2+1;
        CONTINUE;
      end else begin      //keine weiteren Ports
        Ports.Append(CopyFrom(PortNames,pp1));
        BREAK;
      end;//if
  End;//do

end;

//-------------------------------------------------------------------
function TComUtils.GetLinuxSerialPortNames: string;
{$IFNDEF Linux} begin Result:= ''; end;
{$ELSE}
type
TSerialStruct = packed record
  typ: Integer;
  line: Integer;
  port: Cardinal;
  irq: Integer;
  flags: Integer;
  xmit_fifo_size: Integer;
  custom_divisor: Integer;
  baud_base: Integer;
  close_delay: Word;
  io_type: Char;
  reserved_char: Char;
  hub6: Integer;
  closing_wait: Word; // time to wait before closing
  closing_wait2: Word; // no longer used...
  iomem_base: ^Char;
  iomem_reg_shift: Word;
  port_high: Cardinal;
  iomap_base: LongWord; // cookie passed into ioremap
end;
var
  i: Integer;
  sr : TSearchRec;
  sl: TStringList;
  st: stat;
  s: String;
  fd: PtrInt;
  Ser : TSerialStruct;
const TIOCGSERIAL = $541E;
  PORT_UNKNOWN = 0;
begin
  Result := '';
  sl := TStringList.Create;
  try
    // 1. Alle möglichen Ports finden
    if FindFirst('/sys/class/tty/*', LongInt($FFFFFFFF), sr) = 0 then begin
      repeat
        if (sr.Name <> '.') and (sr.Name <> '..') Then
          if (sr.Attr and LongInt($FFFFFFFF)) = Sr.Attr then
            sl.Add(sr.Name);
      until FindNext(sr) <> 0;
    end;//if
    FindClose(sr);
    // 2. heraussuchen ob ./device/driver vorhanden ist
    for i := sl.Count - 1 Downto 0 Do
    Begin
      If Not DirectoryExists('/sys/class/tty/' + sl[i] + '/device/driver') Then
        sl.Delete(i); // Nicht vorhanden >> Port existiert nicht
    end;
    // 3. Herausfinden welcher Treiber
    st.st_mode := 0;
    for i := sl.Count - 1 Downto 0 Do
    Begin
      IF fpLstat('/sys/class/tty/' + sl[i] + '/device', st) = 0 Then Begin
        if fpS_ISLNK(st.st_mode) then begin
          s := fpReadLink('/sys/class/tty/' + sl[i] + '/device/driver');
          s := ExtractFileName(s);
          // 4. Bei serial8250 Treiber muss der Port geprüft werden
          If s = 'serial8250' Then Begin
            sl.Objects[i] := TObject(PtrInt(1));
            fd := FpOpen('/dev/' + sl[i], O_RDWR Or O_NONBLOCK Or O_NOCTTY);
            If fd > 0 Then Begin
              if FpIOCtl(fd, TIOCGSERIAL, @Ser) = 0 then begin
                if Ser.typ = PORT_UNKNOWN then // PORT_UNKNOWN
                  sl.Delete(i);
              end;//if
              FpClose(fd);
            End Else sl.Delete(i); // Port kann nicht geöffnet werden
          End;//If
        end;//if
      End;//If
    end;
    // 5. Dev anhängen
    for i := 0 To sl.Count - 1 Do sl[i] := '/dev/' + sl[i];
    Result := sl.CommaText;
  finally
    sl.Free;
  end;
end;
{$ENDIF}


end.

