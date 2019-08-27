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
{$I codegen.inc}
unit SynEditPrintExtProcs;

interface

uses
  LCLType, Classes, Graphics;

// Converting tabs to spaces: To use the function several times it's better
// to use a function pointer that is set to the fastest conversion function.
type
  TConvertTabsProc = function(const Line: String;
    TabWidth: Integer): String;

function GetBestConvertTabsProc(TabWidth: Integer): TConvertTabsProc;
// This is the slowest conversion function which can handle TabWidth <> 2^n.
function ConvertTabs(const Line: String; TabWidth: Integer): String;

type
  TConvertTabsProcEx = function(const Line: String; TabWidth: Integer;
    var HasTabs: Boolean): String;

function GetBestConvertTabsProcEx(TabWidth: Integer): TConvertTabsProcEx;
// This is the slowest conversion function which can handle TabWidth <> 2^n.
function ConvertTabsEx(const Line: String; TabWidth: Integer;
  var HasTabs: Boolean): String;

function GetExpandedLength(const aStr: String; aTabWidth: Integer): Integer;

function CharIndex2CaretPos(Index, TabWidth: Integer;
  const Line: String): Integer;
function CaretPos2CharIndex(Position, TabWidth: Integer; const Line: String;
  var InsideTabChar: Boolean): Integer;

// Remove all '/' characters from string by changing them into '\.'.
// Change all '\' characters into '\\' to allow for unique decoding.
function EncodeString(s: string): string;

// Decodes string, encoded with EncodeString.
function DecodeString(s: string): string;

implementation

// Please don't change this function; no stack frame and efficient register use.
function GetHasTabs(pLine: PChar; var CharsBefore: Integer): Boolean;
begin
  CharsBefore := 0;
  if Assigned(pLine) then
  begin
    while pLine^ <> #0 do
    begin
      if pLine^ = #9 then break;
      Inc(CharsBefore);
      Inc(pLine);
    end;
    Result := pLine^ = #9;
  end
  else
    Result := False;
end;


function ConvertTabs1Ex(const Line: string; TabWidth: Integer;
  var HasTabs: Boolean): string;
var
  pDest: PChar;
  nBeforeTab: Integer;
begin
  Result := Line;  // increment reference count only
  if GetHasTabs(pointer(Line), nBeforeTab) then
  begin
    HasTabs := True;
    pDest := @Result[nBeforeTab + 1]; // this will make a copy of Line
    // We have at least one tab in the string, and the tab width is 1.
    // pDest points to the first tab char. We overwrite all tabs with spaces.
    repeat
      if (pDest^ = #9) then pDest^ := ' ';
      Inc(pDest);
    until (pDest^ = #0);
  end
  else
    HasTabs := False;
end;

function ConvertTabs1(const Line: string; TabWidth: Integer): string;
var
  HasTabs: Boolean;
begin
  Result := ConvertTabs1Ex(Line, TabWidth, HasTabs);
end;

function ConvertTabs2nEx(const Line: string; TabWidth: Integer;
  var HasTabs: Boolean): string;
var
  i, DestLen, TabCount, TabMask: Integer;
  pSrc, pDest: PChar;
begin
  Result := Line;  // increment reference count only
  if GetHasTabs(pointer(Line), DestLen) then
  begin
    HasTabs := True;
    pSrc := @Line[1 + DestLen];
    // We have at least one tab in the string, and the tab width equals 2^n.
    // pSrc points to the first tab char in Line. We get the number of tabs
    // and the length of the expanded string now.
    TabCount := 0;
    TabMask := (TabWidth - 1) xor $7FFFFFFF;
    repeat
      if pSrc^ = #9 then
      begin
        DestLen := (DestLen + TabWidth) and TabMask;
        Inc(TabCount);
      end
      else
        Inc(DestLen);
      Inc(pSrc);
    until (pSrc^ = #0);
    // Set the length of the expanded string.
    SetLength(Result, DestLen);
    DestLen := 0;
    pSrc := PChar(Line);
    pDest := PChar(Result);
    // We use another TabMask here to get the difference to 2^n.
    TabMask := TabWidth - 1;
    repeat
      if pSrc^ = #9 then
      begin
        i := TabWidth - (DestLen and TabMask);
        Inc(DestLen, i);
        //This is used for both drawing and other stuff and is meant to be #9 and not #32
        repeat
          pDest^ := #9;
          Inc(pDest);
          Dec(i);
        until (i = 0);
        Dec(TabCount);
        if TabCount = 0 then
        begin
          repeat
            Inc(pSrc);
            pDest^ := pSrc^;
            Inc(pDest);
          until (pSrc^ = #0);
          exit;
        end;
      end
      else
      begin
        pDest^ := pSrc^;
        Inc(pDest);
        Inc(DestLen);
      end;
      Inc(pSrc);
    until (pSrc^ = #0);
  end
  else
    HasTabs := False;
end;

function ConvertTabs2n(const Line: string; TabWidth: Integer): string;
var
  HasTabs: Boolean;
begin
  Result := ConvertTabs2nEx(Line, TabWidth, HasTabs);
end;

function ConvertTabsEx(const Line: string; TabWidth: Integer;
  var HasTabs: Boolean): string;
var
  i, DestLen, TabCount: Integer;
  pSrc, pDest: PChar;
begin
  Result := Line;  // increment reference count only
  if GetHasTabs(pointer(Line), DestLen) then
  begin
    HasTabs := True;
    pSrc := @Line[1 + DestLen];
    // We have at least one tab in the string, and the tab width is greater
    // than 1. pSrc points to the first tab char in Line. We get the number
    // of tabs and the length of the expanded string now.
    TabCount := 0;
    repeat
      if pSrc^ = #9 then
      begin
        DestLen := DestLen + TabWidth - DestLen mod TabWidth;
        Inc(TabCount);
      end
      else
        Inc(DestLen);
      Inc(pSrc);
    until (pSrc^ = #0);
    // Set the length of the expanded string.
    SetLength(Result, DestLen);
    DestLen := 0;
    pSrc := PChar(Line);
    pDest := PChar(Result);
    repeat
      if pSrc^ = #9 then
      begin
        i := TabWidth - (DestLen mod TabWidth);
        Inc(DestLen, i);
        repeat
          pDest^ := #9;
          Inc(pDest);
          Dec(i);
        until (i = 0);
        Dec(TabCount);
        if TabCount = 0 then
        begin
          repeat
            Inc(pSrc);
            pDest^ := pSrc^;
            Inc(pDest);
          until (pSrc^ = #0);
          exit;
        end;
      end
      else
      begin
        pDest^ := pSrc^;
        Inc(pDest);
        Inc(DestLen);
      end;
      Inc(pSrc);
    until (pSrc^ = #0);
  end
  else
    HasTabs := False;
end;

function ConvertTabs(const Line: string; TabWidth: Integer): string;
var
  HasTabs: Boolean;
begin
  Result := ConvertTabsEx(Line, TabWidth, HasTabs);
end;

function IsPowerOfTwo(TabWidth: Integer): Boolean;
var
  nW: Integer;
begin
  nW := 2;
  repeat
    if (nW >= TabWidth) then break;
    Inc(nW, nW);
  until (nW >= $10000);  // we don't want 64 kByte spaces...
  Result := (nW = TabWidth);
end;

function GetBestConvertTabsProc(TabWidth: Integer): TConvertTabsProc;
begin
  if (TabWidth < 2) then Result := TConvertTabsProc(@ConvertTabs1)
    else if IsPowerOfTwo(TabWidth) then
      Result := TConvertTabsProc(@ConvertTabs2n)
    else
      Result := TConvertTabsProc(@ConvertTabs);
end;

function GetBestConvertTabsProcEx(TabWidth: Integer): TConvertTabsProcEx;
begin
  if (TabWidth < 2) then Result := @ConvertTabs1Ex
    else if IsPowerOfTwo(TabWidth) then
      Result := @ConvertTabs2nEx
    else
      Result := @ConvertTabsEx;
end;

function GetExpandedLength(const aStr: string; aTabWidth: Integer): Integer;
var
  iRun: PChar;
begin
  Result := 0;
  iRun := PChar(aStr);
  while iRun^ <> #0 do
  begin
    if iRun^ = #9 then
      Inc(Result, aTabWidth - (Result mod aTabWidth))
    else
      Inc(Result);
    Inc(iRun);
  end;
end;

function CharIndex2CaretPos(Index, TabWidth: Integer;
  const Line: string): Integer;
var
  iChar: Integer;
  pNext: PChar;
begin
// possible sanity check here: Index := Max(Index, Length(Line));
  if Index > 1 then
  begin
    if (TabWidth <= 1) or not GetHasTabs(pointer(Line), iChar) then
      Result := Index
    else
    begin
      if iChar + 1 >= Index then
        Result := Index
      else
      begin
        // iChar is number of chars before first #9
        Result := iChar;
        // Index is *not* zero-based
        Inc(iChar);
        Dec(Index, iChar);
        pNext := @Line[iChar];
        while Index > 0 do
        begin
          case pNext^ of
            #0:
              begin
                Inc(Result, Index);
                break;
              end;
            #9:
              begin
                // Result is still zero-based
                Inc(Result, TabWidth);
                Dec(Result, Result mod TabWidth);
              end;
            else
              Inc(Result);
          end;
          Dec(Index);
          Inc(pNext);
        end;
        // done with zero-based computation
        Inc(Result);
      end;
    end;
  end
  else
    Result := 1;
end;

function CaretPos2CharIndex(Position, TabWidth: Integer; const Line: string;
  var InsideTabChar: Boolean): Integer;
var
  iPos: Integer;
  pNext: PChar;
begin
  InsideTabChar := False;
  if Position > 1 then
  begin
    if (TabWidth <= 1) or not GetHasTabs(pointer(Line), iPos) then
      Result := Position
    else
    begin
      if iPos + 1 >= Position then
        Result := Position
      else
      begin
        // iPos is number of chars before first #9
        Result := iPos + 1;
        pNext := @Line[Result];
        // for easier computation go zero-based (mod-operation)
        Dec(Position);
        while iPos < Position do
        begin
          case pNext^ of
            #0: break;
            #9: begin
                  Inc(iPos, TabWidth);
                  Dec(iPos, iPos mod TabWidth);
                  if iPos > Position then
                  begin
                    InsideTabChar := True;
                    break;
                  end;
                end;
            else
              Inc(iPos);
          end;
          Inc(Result);
          Inc(pNext);
        end;
      end;
    end;
  end
  else
    Result := Position;
end;
function EncodeString(s: string): string;
var
  i, j: integer;
begin
  SetLength(Result, 2 * Length(s)); // worst case
  j := 0;
  for i := 1 to Length(s) do begin
    Inc(j);
    if s[i] = '\' then begin
      Result[j] := '\';
      Result[j + 1] := '\';
      Inc(j);
    end else if s[i] = '/' then begin
      Result[j] := '\';
      Result[j + 1] := '.';
      Inc(j);
    end else
      Result[j] := s[i];
  end; //for
  SetLength(Result, j);
end; { EncodeString }

function DecodeString(s: string): string;
var
  i, j: integer;
begin
  SetLength(Result, Length(s)); // worst case
  j := 0;
  i := 1;
  while i <= Length(s) do begin
    Inc(j);
    if s[i] = '\' then begin
      Inc(i);
      if s[i] = '\' then
        Result[j] := '\'
      else
        Result[j] := '/';
    end else
      Result[j] := s[i];
    Inc(i);
  end; //for
  SetLength(Result,j);
end; { DecodeString }

end.

