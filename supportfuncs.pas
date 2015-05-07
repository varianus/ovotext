{ Ovotext - simple text editor

  Copyright (C) 2015 Marco Caselli <marcocas@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit SupportFuncs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF UNIX}
  ,BaseUnix
  {$ENDIF}
  ;
Const
// TODO: read it from config file
 NUMBEROFSPACEFORTAB = 4;

//Remove invalid char from highlighters name and attributes
Function CleanupName(aName:string):string;

//Split a delimited string in a Stringlist
procedure StrToStrings(S, Sep: string; const List: TStrings; const AllowEmptyString: boolean = True);


{$IFDEF UNIX}
function isRoot:boolean;
{$ENDIF}

function RemoveSpacesInExcess(const s: string): string;
Function TabsToSpace(Const S: string):string;

implementation

Function CleanupName(aName:string):string;
var
  c: integer;
begin
  Result := aName;
  for c:= 1 to Length(aName) do
  if not (upcase(aName[c])  in['A'..'Z','0'..'9','_']) then
    Result[c] := '_';
end;

function RemoveSpacesInExcess(const s: string): string;
var
  p: integer;
begin
  Result := trim(s);
  p := pos(#32#32, Result);
  while p > 0 do
  begin
    Result := StringReplace(Result, #32#32, #32, [rfReplaceAll]);
    p := pos(#32#32, Result);
  end;

end;

function TabsToSpace(const S: string): string;
begin
  Result := StringReplace(s, #9, StringOfChar(#32,NUMBEROFSPACEFORTAB), [rfReplaceAll]);
end;

procedure StrToStrings(S, Sep: string; const List: TStrings; const AllowEmptyString: boolean = True);
var
  I, L: integer;
  Left: string;
begin
  Assert(List <> nil);
  List.BeginUpdate;
  try
    List.Clear;
    L := Length(Sep);
    I := Pos(Sep, S);
    while I > 0 do
    begin
      Left := LeftStr(S, I - 1);
      if (Left <> '') or AllowEmptyString then
        List.Add(Left);
      Delete(S, 1, I + L - 1);
      I := Pos(Sep, S);
    end;
    if S <> '' then
      List.Add(S);
  finally
    List.EndUpdate;
  end;
end;
{$IFDEF UNIX}
function isRoot: boolean;
begin
  Result:= FpGetuid = 0;
end;
{$ENDIF}


end.

