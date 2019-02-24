/////////////////////////////////////////////////////////////////////////////
//
//   Unit: uStrings
//
//   Author: kupferstecher
//
//   Description:
//   + Standard handling routines for character strings
//   + Result value always absolut position within the string
//     and not relative to LPos
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

unit UStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Function INDEX(const InString : string; InToken : string; Lpos : Integer; RPos : Integer): Integer; overload;
Function INDEX(const InString : string; InToken : string; Lpos : Integer): Integer; overload;
Function INDEX(const InString : string; InToken : string): Integer; overload;

Function SCAN(const InString : string; InToken : string; Lpos : Integer; RPos : Integer): Integer; overload;
Function SCAN(const InString : string; InToken : string; Lpos : Integer): Integer; overload;
Function SCAN(const InString : string; InToken : string): Integer; overload;

Function VERIFY(const InString : string; InToken : string; Lpos : Integer; RPos : Integer): Integer; overload;
Function VERIFY(const InString : string; InToken : string; Lpos : Integer): Integer; overload;
Function VERIFY(const InString : string; InToken : string): Integer; overload;

Function Equal(const InString,InToken : string; Lpos,RPos : Integer): Boolean; overload;
Function Equal(const InString,InToken : string; Lpos : Integer): Boolean; overload;

//Doesn't consider capital letters
Function EqualLower(InString : string; InToken : string; Lpos : Integer): Boolean;

Function CopyUntil(const Instring: string; Lpos,Rpos : Integer): String;
Function CopyFrom(const Instring: string; Lpos : Integer): String;

Function ReplaceToken(InString,AToken,AReplacement:String):String;
 
implementation


//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Function INDEX(const InString : string; InToken : string; Lpos : Integer; RPos : Integer): Integer; overload;
 var 
 ii,kk : Integer;
 begin

 Result:=0;
 
 FOR ii:=Lpos to Rpos do 
 BEGIN
 
   IF InString[ii] = InToken[1] then begin
       if Length(InToken)=1 then begin Result := ii; EXIT; end;
       for kk:=1 to (Length(InToken)-1) do 
       begin
          if (ii+kk) > RPos then EXIT;  //Außerhalb Bereich
          if InString[ii+kk] <> InToken[1+kk] then BREAK;  //test and //cleanup
          if (kk = Length(InToken)-1) and (ii+kk<=RPos) then Result := ii;
       end;//do
   End;//if 
    
 END;//do

end;//function INDEX
//-------------------------------------
Function INDEX(const InString : string; InToken : string; Lpos : Integer): Integer; overload;
begin
   Result := index(InString, InToken, Lpos, Length(InString));
end;//function INDEX
//-------------------------------------
Function INDEX(const InString : string; InToken : string): Integer; overload;
begin
   Result := index(InString, InToken, 1, Length(InString));
end;//function INDEX

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Function SCAN(const InString : string; InToken : string; Lpos : Integer; RPos : Integer): Integer; overload;
 var 
 ii,kk : Integer;
 begin

 Result:=0;
 
 FOR ii:=Lpos to Rpos do BEGIN
 for kk:=1 to Length(InToken) do begin
 
   IF InString[ii] = InToken[kk] then begin Result := ii; exit; end;
  
 end;//do   
 END;//do

end;//function SCAN 
//-------------------------------------
Function SCAN(const InString : string; InToken : string; Lpos : Integer): Integer; overload;
begin
   Result := SCAN(InString, InToken, Lpos, Length(InString));
end;//function SCAN
//-------------------------------------
Function SCAN(const InString : string; InToken : string): Integer; overload;
begin
   Result := SCAN(InString, InToken, 1, Length(InString));
end;//function SCAN

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Function VERIFY(const InString : string; InToken : string; Lpos : Integer; RPos : Integer): Integer; overload;
 var 
 ii,kk : Integer;
 found : boolean;
 begin

 Result:=0;

 FOR ii:=Lpos to Rpos do BEGIN
 
   found := false;

   for kk:=1 to Length(InToken) do begin
      IF InString[ii] = InToken[kk] then begin found:=true ; break; end;
   end;//do   

   if not found then begin result:=ii; break; end; 

 END;//do

end;//function VERIFY 
//-------------------------------------
Function VERIFY(const InString : string; InToken : string; Lpos : Integer): Integer; overload;
begin
   Result := VERIFY(InString, InToken, Lpos, Length(InString));
end;//function VERIFY
//-------------------------------------
Function VERIFY(const InString : string; InToken : string): Integer; overload;
begin
   Result := VERIFY(InString, InToken, 1, Length(InString));
end;//function VERIFY

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Function CopyUntil(const Instring: string; Lpos,Rpos: Integer): String;
begin
 Result := Copy(InString,Lpos,(Rpos-Lpos+1)) 
end;//function COPYUNTIL

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Function CopyFrom(const Instring: string; Lpos: Integer): String;
begin
 Result := Copy(InString,Lpos,Length(InString)) 
end;//function COPYFROM

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Function Equal(const InString,InToken: string; Lpos,RPos: Integer): Boolean; overload;
var
 ii : Integer;
begin
   Result := false;
   
   IF InString[Lpos]=InToken[1] then begin
       if Length(InToken)=1 then begin Result :=true; exit; end;
       for ii:=1 to (Length(InToken)-1) do begin
          if LPos+ii > RPos then BREAK;
          if InString[Lpos+ii] <> InToken[1+ii] then break;
          if (ii = (Length(InToken)-1)) and (Lpos+ii<=RPos) then Result := true;
       end;//do
   End;//if 
   
end;//function EQUAL

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Function EQUAL(const InString,InToken: string; Lpos : Integer): Boolean; overload;
begin
   Result := Equal(InString, InToken, Lpos, Length(InString));
end;//function EQUAL

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Function EqualLower(InString:string;InToken:string;Lpos:Integer):Boolean;
var
 ii,RPos : Integer;
 tmpChar:Char;
begin
   Result := false;
   RPos:= length(InString);
   tmpChar:= InString[Lpos];
   if (tmpChar >= 'A') and (tmpChar <= 'Z') then tmpChar:=Chr(Ord(tmpChar)+32);
   IF tmpChar=InToken[1] then begin
       if Length(InToken)=1 then begin Result :=true; exit; end;
       for ii:=1 to (Length(InToken)-1) do begin
         if LPos+ii > RPos then BREAK;
         tmpChar:= InString[Lpos+ii];
         if(tmpChar>='A')and(tmpChar<='Z')then tmpChar:= Chr(Ord(tmpChar)+32);
         if tmpChar <> InToken[1+ii] then break;
         if (ii = (Length(InToken)-1)) and (Lpos+ii<=RPos) then Result := true;
       end;//do
   End;//if

end;//function EqualLower

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Function ReplaceToken(InString,AToken,AReplacement:String):String;
var pp1: Integer;
begin

  pp1:= Index(InString,AToken);
  if pp1 = 0 then begin
    Result:= InString;
    EXIT;
  end;//if

  Result:= CopyUntil(InString,1,pp1-1) + AReplacement +
            CopyFrom(InString,pp1+length(AToken));

end;//function ReplaceToken


//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
end.


