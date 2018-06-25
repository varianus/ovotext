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
  , BaseUnix
  {$ENDIF}  ;

const
  // TODO: read it from config file
  NUMBEROFSPACEFORTAB = 4;

//Remove invalid char from highlighters name and attributes
function CleanupName(aName: string): string;

//Split a delimited string in a Stringlist
procedure StrToStrings(S, Sep: string; const List: TStrings; const AllowEmptyString: boolean = True);


{$IFDEF UNIX}
function isRoot: boolean;
{$ENDIF}

function RemoveSpacesInExcess(const s: string): string;
function TabsToSpace(const S: string): string;
function FormatXML(const S: string): string;
function FormatJson(const S: string): string;


implementation
uses math;

function CleanupName(aName: string): string;
var
  c: integer;
begin
  Result := aName;
  for c := 1 to Length(aName) do
    if not (upcase(aName[c]) in ['A'..'Z', '0'..'9', '_']) then
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
  Result := StringReplace(s, #9, StringOfChar(#32, NUMBEROFSPACEFORTAB), [rfReplaceAll]);
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

function FormatXML(const S: string): string;
const
  nIndentDepth = 3;
  bHighlightCloseTag = False;
var
  st: TMemoryStream;
  bDoIndent: boolean;
  szCheckTag: string;
  bTagBuilding: boolean;
  szCurrentTag: string;
  c, cNextChar: char;
  bQuoteActive: boolean;
  cChar: char;
  bCheckIndent: boolean;
  bTagActive: boolean;
  nStringLoop: integer;
  nIndent: integer;

  procedure OutputIndent;
  var
    nIndentLoop: integer;
  begin
    if st.Size > 0 then
      st.Write(sLineBreak, Length(sLineBreak));

    if nIndent < 0 then
    begin
      nIndent := 0; // fix negative indents due to bad   XML
      // result := result + '[NEGINDENT]';
    end;
    for nIndentLoop := 0 to nIndent do
    begin
      c := ' ';
      st.Write(c, SizeOf(char));
    end;
  end;

begin
  bTagBuilding := False;
  bQuoteActive := False;
  bTagActive := False;
  Result := '';
  nIndent := 0;
  bCheckIndent := False;
  szCurrentTag := '';
  St := TMemoryStream.Create;
  st.SetSize(Length(S));
  for nStringLoop := 1 to Length(S) do
  begin
    cChar := S[nStringLoop];
    if nStringLoop < Length(S) then
    begin
      cNextChar := S[nStringLoop + 1];
    end
    else
    begin
      cNextChar := ' '; // safe char
    end;
    case cChar of
      '<':
      begin
        bDoIndent := False;
        bTagActive := True;
        if cNextChar = '/' then
        begin
          Dec(nIndent, nIndentDepth);
          bTagBuilding := False;
          bCheckIndent := False;
          szCheckTag := Copy(S, nStringLoop + 2, Length(szCurrentTag));
          if szCheckTag <> szCurrentTag then
            bDoIndent := True;
        end
        else
        begin
          bTagBuilding := True;
          szCurrentTag := '';
          bCheckIndent := True;
          if not bHighlightCloseTag then
            bDoIndent := True;
        end;
        if bHighlightCloseTag then
          bDoIndent := True;
        if bDoIndent then
          OutputIndent;
        c := '<';
        st.Write(c, SizeOf(char));
      end;
      '>':
      begin
        bTagActive := False;
        bTagBuilding := False;
        c := '>';
        st.Write(c, SizeOf(char));
        if bCheckIndent then
          Inc(nIndent, nIndentDepth);
      end;
      '"':
      begin
        st.Write(cChar, 1);
        if bTagActive then
          bQuoteActive := not bQuoteActive;
      end;
      '/':
      begin
        if (bTagActive) and (not bQuoteActive) then
        begin
          if bCheckIndent then
          begin
            if cNextChar = '>' then
              Dec(nIndent, nIndentDepth);
          end;
        end;
        c := '/';
        st.Write(c, SizeOf(char));
      end;
      #13, #10, #9:
      begin
      end;

      else
      begin
        if bTagBuilding then
        begin
          if cChar <> ' ' then
          begin
            szCurrentTag := szCurrentTag + cChar;
          end
          else
          begin
            bTagBuilding := False;
          end;
        end;
        st.Write(cChar, SizeOf(char));
      end;
    end; // case
  end;

  SetString(Result, st.Memory, st.Size);
  st.Free;
end;


{
Json formatting code taken and adapted from delphi-xe-json by Nils Achterholt
   https://bitbucket.org/Gloegg/delphi-xe-json
}
class function getSpaces(aQuantity: integer): string;
begin
  result := '';
  while aQuantity > 0 do
  begin
    result := result + ' ';
    dec(aQuantity);
  end;
end;

function RemoveWhiteSpace(const aInput: string): string;
const
  whitespace = [#0, #8, #9, #10, #12, #13, ' '];
var
  i: integer;
  insideString: boolean;
begin
  i := 1;
  result:='';
  insideString := false;
  while i <= length(aInput) do
  begin
    if (aInput[i] = '\') then
    begin
      result := result + aInput[i] + aInput[i + 1];
      inc(i, 2);
    end
    else if aInput[i] = '"' then
    begin
      result := result + aInput[i];
      insideString := not insideString;
      inc(i);
    end
    else if not insideString and (aInput[i] in whitespace) then
      inc(i)
    else
    begin
      result := result + aInput[i];
      inc(i);
    end;
  end;
end;

function Indent(const aInput: string): string;
var
  sl: TStringList;
  i: integer;
  lvl: integer;
begin
  lvl := 0;
  sl := TStringList.Create;
  try
    sl.Text := aInput;
    for i := 0 to sl.Count - 1 do
    begin
      case sl[i][1] of
        '{':
          begin
            sl[i] := getSpaces(lvl * 2) + sl[i];
            inc(lvl);
            if (Length(sl[i]) > 1) and (sl[i][2] = '}') then
              dec(lvl);
          end;
        '[':
          begin
            sl[i] := getSpaces(lvl * 2) + sl[i];
            inc(lvl);
            if (Length(sl[i]) > 1) and (sl[i][2] = ']') then
              dec(lvl);
          end;
        '}', ']':
          begin
            dec(lvl);
            lvl := max(lvl, 0);
            sl[i] := getSpaces(lvl * 2) + sl[i];
          end
      else
        sl[i] := getSpaces(lvl * 2) + sl[i];
      end;
    end;
    result := sl.Text;
  finally
    sl.Free;
  end;
end;

function InsertLineBreaks(const aInput: string): string;
var
  i: integer;
  insideString: boolean;
  s: string;
begin
  s := '';
  i := 1;
  insideString := false;
  while i <= length(aInput) do
  begin
    if insideString then
    begin
      s := s + aInput[i];
      if aInput[i] = '\' then
        begin
          s := s + aInput[i + 1];
          inc(i, 2);
        end
      else
        begin
          if aInput[i] = '"' then
            insideString := false;
          inc(i);
          end;
      end
    else
    begin
      case aInput[i] of
        '\':
          begin
            s := s + aInput[i] + aInput[i + 1];
            inc(i, 2);
          end;
        '"':
          begin
            s := s + aInput[i];
            insideString := not insideString;
            inc(i);
          end;
        '{':
          begin
            if aInput[i + 1] = '}' then
            begin
              s := s + '{}';
              inc(i, 2);
            end
            else
            begin
              s := s + sLineBreak + aInput[i] + sLineBreak;
              inc(i);
            end;
          end;
        '[':
          begin
            if aInput[i + 1] = ']' then
            begin
              s := s + '[]';
              inc(i, 2);
            end
            else
            begin
              s := s + sLineBreak + aInput[i] + sLineBreak;
              inc(i);
            end;
          end;
        '}', ']':
          begin
            if (length(aInput) > i) and (aInput[i + 1] = ',') then
            begin
              s := s + sLineBreak + aInput[i] + ',' + sLineBreak;
              inc(i, 2);
            end
            else
            begin
              s := s + sLineBreak + aInput[i] + sLineBreak;
              inc(i);
            end;
          end;
        ',':
          begin
            s := s + aInput[i] + sLineBreak;
            inc(i);
          end;
      else
        begin
          s := s + aInput[i];
          inc(i);
        end;
      end;
    end;
  end;
  result := s;
end;

function RemoveEmptyLines(const aInput: string): string;
var
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  try
    sl.Text := aInput;
    for i := sl.Count - 1 downto 0 do
    begin
      if sl[i] = '' then
        sl.Delete(i);
    end;
    result := sl.Text;
  finally
    sl.Free;
  end;
end;

function FormatJSON(const S: string): string;
begin
  // Clean the input from previous formatting
  result := RemoveWhiteSpace(S);
  // Split up logical units of JSON
  result := InsertLineBreaks(result);
  // It's easier to clean up empty lines then preventing them
  result := RemoveEmptyLines(result);
  // Indent each line with the correct space
  result := Indent(result);
end;




{$IFDEF UNIX}
function isRoot: boolean;
begin
  Result := FpGetuid = 0;
end;

{$ENDIF}


end.
