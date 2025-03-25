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
unit SupportFuncs;

interface

uses
  Classes, SysUtils, RegExpr, LazFileUtils
  {$IFDEF UNIX}
  , BaseUnix
  {$ENDIF}  ;

const
  // TODO: read it from config file
  NUMBEROFSPACEFORTAB = 2;

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
function CompactXML(const S: string): string;
function FormatJson(const S: string): string;
function CompactJson(const S: string): string;
function FormatSQL(const S: string): string;
function BuildFolderList(const Path: string; const List: TStrings): boolean;
function BuildFileList(const Path: string; const Attr: integer; const List: TStrings; Recurring: boolean): boolean;
function DecodeExtendedSearch(S: string): string;

implementation

uses Math, lazutf8;

const
  Aligner = #31;
  CRLF = #13#10;
  CR = #13;
  LF = #10;
  DOUBLEQUOTE = #34;
  QUOTE = #39;
  SQUARE_OPEN = #91;
  SQUARE_CLOSE = #93;
  ROUND_OPEN = #40;
  ROUND_CLOSE = #41;
  COMMA = #44;
  SEMICOLON = #59;
  SPACE = #32;
  SQLKEYWORDMAX = 21;

var
  sqlKeyWord: array[1..SQLKEYWORDMAX] of string = (' INNER JOIN', ' LEFT JOIN', ' RIGHT JOIN', ' WHERE', ' LEFT OUTER JOIN', ' GROUP BY',
                                                   ' ORDER BY', ' HAVING', ' FROM', ' SELECT', ' AND', ' FOR', ' INSERT INTO', ' OR',
                                                   ' UPDATE', ' SET', ' DELETE', ' ALTER ', ' JOIN', ' DROP', ' VALUES');

function DecodeExtendedSearch(S: string): string;
const
  Escape = ['b', 't', 'n', 'v', 'f', 'r'];
var
  C: PChar;
  R: string;
  I, J: Integer;
  H: string;
begin
  C := PChar(S);
  I := Length(S);
  if I < 1 then
    Exit('');
  R := '';
  SetLength(R, I);
  I := 1;
  while C^ <> #00 do
  begin
    if C^ = '\' then
    begin
      Inc(C);
      if C^ in Escape then
      case C^ of
        'b': R[I] := #8;
        't': R[I] := #9;
        'n': R[I] := #10;
        'v': R[I] := #11;
        'f': R[I] := #12;
        'r': R[I] := #13;
      end
      else if C^ = 'u' then
      begin
        H := UnicodeToUTF8(StrToInt('$' + C[1] + C[2] + C[3] + C[4]));
        for J := 1 to Length(H) - 1 do
        begin
          R[I] := H[J];
          Inc(I);
        end;
        R[I] := H[Length(H)];
        Inc(C, 4);
      end
      else
        R[I] := C^;
    end
    else
      R[I] := C^;
    Inc(C);
    Inc(I);
  end;
  SetLength(r,i-1);
  Result := R;
end;

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
  bHighlightCloseTag = False;
  CDATA = '![CDATA[';
  COMMENT = '!--';
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
  bSkipping: boolean;
  bTagActive: boolean;
  nStringLoop: integer;
  nIndent: integer;

  procedure OutputIndent;
  var
    nIndentLoop: integer;
  begin
    if st.Size > 0 then
      st.Write(string(sLineBreak)[1], Length(sLineBreak));

    if nIndent < 0 then
    begin
      nIndent := 0; // fix negative indents due to bad   XML
      // result := result + '[NEGINDENT]';
    end;
    c := ' ';
    for nIndentLoop := 0 to nIndent - 1 do
    begin
      st.Write(c, SizeOf(char));
    end;
  end;

  procedure SkipCDATAorComment;
  var
    MaybeDone: boolean;
  begin
    Inc(nStringLoop);
    bSkipping := False;
    if copy(s, nStringLoop, Length(CDATA)) = CDATA then
    begin
      MaybeDone := False;
      while nStringLoop <= Length(S) do
      begin
        if s[nStringLoop] = ']' then
        begin
          if MaybeDone then
          begin
            c := ']';
            st.Write(c, SizeOf(char));
            szCurrentTag := CDATA;
            bSkipping := True;
            break;
          end
          else
            MaybeDone := True;
        end
        else
          MaybeDone := False;
        c := s[nStringLoop];
        st.Write(c, SizeOf(char));
        Inc(nStringLoop);
      end;
    end
    else
    begin
      if copy(s, nStringLoop, Length(COMMENT)) = COMMENT then
      begin
        MaybeDone := False;
        while nStringLoop <= Length(S) do
        begin
          if s[nStringLoop] = '-' then
          begin
            if MaybeDone then
            begin
              c := '-';
              st.Write(c, SizeOf(char));
              szCurrentTag := COMMENT;
              bSkipping := True;
              break;
            end
            else
              MaybeDone := True;
          end
          else
            MaybeDone := False;
          c := s[nStringLoop];
          st.Write(c, SizeOf(char));
          Inc(nStringLoop);
        end;
      end;
    end;

  end;

begin
  bTagBuilding := False;
  bQuoteActive := False;
  bTagActive := False;
  Result := '';
  nIndent := 0;
  bSkipping := False;
  bCheckIndent := False;
  szCurrentTag := '';
  St := TMemoryStream.Create;
  nStringLoop := 1;
  while nStringLoop <= Length(S) do
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
          Dec(nIndent, NUMBEROFSPACEFORTAB);
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
        if bDoIndent then
          OutputIndent;
        c := '<';
        st.Write(c, SizeOf(char));
        if cNextChar = '!' then
          SkipCDATAorComment;
      end;
      '>':
      begin
        bTagActive := False;
        bTagBuilding := False;
        c := '>';
        st.Write(c, SizeOf(char));
        if bCheckIndent then
          Inc(nIndent, NUMBEROFSPACEFORTAB);
        if bSkipping then
        begin
          Dec(nIndent, NUMBEROFSPACEFORTAB);
          bSkipping := False;
        end;
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
              Dec(nIndent, NUMBEROFSPACEFORTAB);
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
    Inc(nStringLoop);
  end;

  SetString(Result, st.Memory, st.Size);
  st.Free;
end;



function Spaces(Num: integer): string;
begin
  Result := StringOfChar(' ', num);
end;


{
Json formatting code taken and adapted from delphi-xe-json by Nils Achterholt
   https://bitbucket.org/Gloegg/delphi-xe-json
}

function RemoveWhiteSpace(const aInput: string): string;
const
  whitespace = [#0, #8, #9, #10, #12, #13, ' '];
var
  i: integer;
  insideString: boolean;
begin
  i := 1;
  Result := '';
  insideString := False;
  while i <= length(aInput) do
  begin
    if (aInput[i] = '\') then
    begin
      Result := Result + aInput[i] + aInput[i + 1];
      Inc(i, 2);
    end
    else if aInput[i] = '"' then
    begin
      Result := Result + aInput[i];
      insideString := not insideString;
      Inc(i);
    end
    else if not insideString and (aInput[i] in whitespace) then
      Inc(i)
    else
    begin
      Result := Result + aInput[i];
      Inc(i);
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
      case sl[i][Length(sl[i])] of
        '{':
        begin
          sl[i] := Spaces(lvl * NUMBEROFSPACEFORTAB) + sl[i];
          Inc(lvl);
          if (Length(sl[i]) > 1) and (sl[i][2] = '}') then
            Dec(lvl);
        end;
        '[':
        begin
          sl[i] := Spaces(lvl * NUMBEROFSPACEFORTAB) + sl[i];
          Inc(lvl);
          if (Length(sl[i]) > 1) and (sl[i][2] = ']') then
            Dec(lvl);
        end;
        else
          case sl[i][1] of
            '}', ']':
            begin
              Dec(lvl);
              lvl := max(lvl, 0);
              sl[i] := Spaces(lvl * NUMBEROFSPACEFORTAB) + sl[i];
            end
            else
              sl[i] := Spaces(lvl * NUMBEROFSPACEFORTAB) + sl[i];
          end;
      end;
    end;
    Result := sl.Text;
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
  insideString := False;
  while i <= length(aInput) do
  begin
    if insideString then
    begin
      s := s + aInput[i];
      if aInput[i] = '\' then
      begin
        s := s + aInput[i + 1];
        Inc(i, 2);
      end
      else
      begin
        if aInput[i] = '"' then
          insideString := False;
        Inc(i);
      end;
    end
    else
    begin
      case aInput[i] of
        '\':
        begin
          s := s + aInput[i] + aInput[i + 1];
          Inc(i, 2);
        end;
        '"':
        begin
          s := s + aInput[i];
          insideString := not insideString;
          Inc(i);
        end;
        '{':
        begin
          if aInput[i + 1] = '}' then
          begin
            s := s + '{}';
            Inc(i, 2);
          end
          else
          begin
            s := s + aInput[i] + sLineBreak;
            Inc(i);
          end;
        end;
        '[':
        begin
          if aInput[i + 1] = ']' then
          begin
            s := s + '[]';
            Inc(i, 2);
          end
          else
          begin
            s := s + aInput[i] + sLineBreak;
            Inc(i);
          end;
        end;
        '}', ']':
        begin
          if (length(aInput) > i) and (aInput[i + 1] = ',') then
          begin
            s := s + sLineBreak + aInput[i] + ',' + sLineBreak;
            Inc(i, 2);
          end
          else
          begin
            s := s + sLineBreak + aInput[i] + sLineBreak;
            Inc(i);
          end;
        end;
        ',':
        begin
          s := s + aInput[i] + sLineBreak;
          Inc(i);
        end;
        else
        begin
          s := s + aInput[i];
          Inc(i);
        end;
      end;
    end;
  end;
  Result := s;
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
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

function CompactXML(const S: string): string;
begin
  Result := ReplaceRegExpr('>\s{0,}<', S, '><', True);
end;

function FormatJSON(const S: string): string;
begin
  // Clean the input from previous formatting
  Result := RemoveWhiteSpace(S);
  // Split up logical units of JSON
  Result := InsertLineBreaks(Result);
  // It's easier to clean up empty lines then preventing them
  Result := RemoveEmptyLines(Result);
  // Indent each line with the correct space
  Result := Indent(Result);
end;

{$IFDEF UNIX}
function isRoot: boolean;
begin
  Result := FpGetuid = 0;
end;

{$ENDIF}

// Derived from "Like" by Michael Winter
function StrMatches(const Substr, S: string; const Index: SizeInt = 1): boolean;
var
  StringPtr: PChar;
  PatternPtr: PChar;
  StringRes: PChar;
  PatternRes: PChar;
begin
  Result := SubStr = '*';

  if Result or (S = '') then
    Exit;

  StringPtr := PChar(@S[Index]);
  PatternPtr := PChar(SubStr);
  StringRes := nil;
  PatternRes := nil;

  repeat
    repeat
      case PatternPtr^ of
        #0:
        begin
          Result := StringPtr^ = #0;
          if Result or (StringRes = nil) or (PatternRes = nil) then
            Exit;

          StringPtr := StringRes;
          PatternPtr := PatternRes;
          Break;
        end;
        '*':
        begin
          Inc(PatternPtr);
          PatternRes := PatternPtr;
          Break;
        end;
        '?':
        begin
          if StringPtr^ = #0 then
            Exit;
          Inc(StringPtr);
          Inc(PatternPtr);
        end;
        else
        begin
          if StringPtr^ = #0 then
            Exit;
          if StringPtr^ <> PatternPtr^ then
          begin
            if (StringRes = nil) or (PatternRes = nil) then
              Exit;
            StringPtr := StringRes;
            PatternPtr := PatternRes;
            Break;
          end
          else
          begin
            Inc(StringPtr);
            Inc(PatternPtr);
          end;
        end;
      end;
    until False;

    repeat
      case PatternPtr^ of
        #0:
        begin
          Result := True;
          Exit;
        end;
        '*':
        begin
          Inc(PatternPtr);
          PatternRes := PatternPtr;
        end;
        '?':
        begin
          if StringPtr^ = #0 then
            Exit;
          Inc(StringPtr);
          Inc(PatternPtr);
        end;
        else
        begin
          repeat
            if StringPtr^ = #0 then
              Exit;
            if StringPtr^ = PatternPtr^ then
              Break;
            Inc(StringPtr);
          until False;
          Inc(StringPtr);
          StringRes := StringPtr;
          Inc(PatternPtr);
          Break;
        end;
      end;
    until False;
  until False;
end;


function IsFileNameMatch(FileName: string; const Mask: string; const CaseSensitive: boolean): boolean;
begin
  Result := True;
  {$IFDEF MSWINDOWS}
  if (Mask = '') or (Mask = '*') or (Mask = '*.*') then
    Exit;
  if Pos('.', FileName) = 0 then
    FileName := FileName + '.';  // file names w/o extension match '*.'
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  if (Mask = '') or (Mask = '*') then
    Exit;
  {$ENDIF UNIX}
  if CaseSensitive then
    Result := StrMatches(Mask, FileName)
  else
    Result := StrMatches(AnsiUpperCase(Mask), AnsiUpperCase(FileName));
end;

function BuildFolderList(const Path: string; const List: TStrings): boolean;
var
  SearchRec: TSearchRec;
  Directory: string;
begin
  Assert(List <> nil);
  {* extract the Directory *}
  Directory := ExtractFileDir(Path);

  {* files can be searched in the current directory *}
  if Directory <> '' then
    begin
       Directory := IncludeTrailingPathDelimiter(Directory);
    end;

  {* search all files in the directory *}
  Result := FindFirstUTF8(Directory + AllFilesMask, faDirectory, SearchRec) = 0;

  List.BeginUpdate;
  try
    while Result do
      begin
        if (SearchRec.Name <> '.') and
           (SearchRec.Name <> '..') and
           ((SearchRec.Attr and faDirectory) = faDirectory)  then
          begin
          List.Add(Directory + SearchRec.Name);
          end;

      case FindNextUTF8(SearchRec) of
        0: ;
        2: //ERROR_NO_MORE_FILES:
          Break;
        else
          Result := False;
        end;
      end;
  finally
    FindCloseUTF8(SearchRec);
    List.EndUpdate;
  end;
end;


function BuildFileList(const Path: string; const Attr: integer; const List: TStrings; Recurring: boolean): boolean;
var
  SearchRec: TSearchRec;
  IndexMask: integer;
  MaskList: TStringList;
  Masks, Directory: string;
begin
  Assert(List <> nil);
  MaskList := TStringList.Create;
  try
    {* extract the Directory *}
    Directory := ExtractFileDir(Path);

    {* files can be searched in the current directory *}
    if Directory <> '' then
    begin
      Directory := IncludeTrailingPathDelimiter(Directory);
      {* extract the FileMasks portion out of Path *}
      Masks := copy(Path, Length(Directory) + 1, Length(Path));
    end
    else
      Masks := Path;

    {* put the Masks into TStringlist *}
    StrToStrings(Masks, ';', MaskList, False);

    {* search all files in the directory *}
    Result := FindFirstUTF8(Directory + AllFilesMask, faAnyFile, SearchRec) = 0;

    List.BeginUpdate;
    try
      while Result do
      begin
        {* if the filename matches any mask then it is added to the list *}
        if Recurring and ((searchrec.Attr and faDirectory) <> 0) and (SearchRec.Name <> '.') and
          (SearchRec.Name <> '..') then
          BuildFileList(IncludeTrailingPathDelimiter(Directory + SearchRec.Name) + masks,
            Attr, list, Recurring);

        for IndexMask := 0 to MaskList.Count - 1 do
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
            ((SearchRec.Attr and Attr) = (SearchRec.Attr and faAnyFile)) and
            ((searchrec.Attr and faDirectory) <> faDirectory) and
            IsFileNameMatch(SearchRec.Name, MaskList.Strings[IndexMask], False) then
          begin
            List.Add(Directory + SearchRec.Name);
            Break;
          end;

        case FindNext(SearchRec) of
          0: ;
          2: //ERROR_NO_MORE_FILES:
            Break;
          else
            Result := False;
        end;
      end;
    finally
      FindClose(SearchRec);
      List.EndUpdate;
    end;
  finally
    MaskList.Free;
  end;
end;

function CharReplace(var S: string; const Search, Replace: char): integer;
var
  P: PChar;
begin
  Result := 0;
  if Search <> Replace then
  begin
    UniqueString(S);
    P := PChar(S);
    while P^ <> #0 do
    begin
      if P^ = Search then
      begin
        P^ := Replace;
        Inc(Result);
      end;
      Inc(P);
    end;
  end;
end;

function Match(OpenChar, CloseChar: char; Text: string): longint;
var
  Last, First: longint;
begin
  First := 1;
  Last := Pos(CloseChar, Text);
  if Last = 0 then
    Last := Length(Text);
  repeat
    repeat
      Inc(First);
    until (First = last) or (Text[First] = openchar);

    if First < last then
      repeat
        Inc(Last);
      until (last >= Length(Text)) or (Text[Last] = closeChar);
  until First = last;
  Result := Last;
end;

function CompactJson(const S: string): string;
begin
  Result := RemoveWhiteSpace(S);
end;

function FormatSQL(const S: string): string;
var
  strSQL: string;
  blnkeyWord: boolean;
  intKeyWord: integer;
  lngChar: longint;
  lngEnd: longint;
  strOut: string;
  maxKeyword: integer;
  cntSpaces: integer;
  After: string;
  Before: string;
  SL: TStringList;

  procedure EmitStatement;
  var
    idRow: integer;
    AlignerPos: integer;
  begin
    SL := TStringList.Create;
    try
      SL.Text := strOut;
      for idRow := 0 to SL.Count - 1 do
      begin
        AlignerPos := pos(Aligner, SL[idRow]);
        if AlignerPos = 0 then
          Continue;
        cntSpaces := (maxKeyword - AlignerPos);
        Before := Trim(copy(SL[idRow], 1, AlignerPos - 1));
        After := sl[idRow];
        Delete(After, 1, AlignerPos);
        SL[idRow] := spaces(cntSpaces) + Before + ' ' + After;
      end;
      Result := Result + SL.Text;
      maxKeyword := 0;
      strOut := '';
    finally
      SL.Free;
    end;
  end;

begin
  strout := '';
  maxKeyword := 0;
  Result := '';
  if S <> '' then
  begin
    strSQL := S;
    strSQL := ' ' + strSQL + ' ';
    CharReplace(strSQL, CR, SPACE);
    CharReplace(strSQL, LF, SPACE);
    strSQL := RemoveSpacesInExcess(strSQL);

    lngChar := 1;
    while lngChar <= Length(strSQL) do
    begin
      blnkeyWord := False;
      for intKeyWord := 1 to SQLKEYWORDMAX do
        if UpperCase(Copy(strSQL, lngChar, Length(sqlKeyWord[intKeyWord]))) = (sqlKeyWord[intKeyWord]) then
        begin
          blnkeyWord := True;
          Maxkeyword := max(Length(sqlKeyWord[intKeyWord]), maxKeyword);
          Break;
        end;

      if blnkeyWord then
      begin
        after := Aligner;
        before := CRLF;
        cntSpaces := 0;
        strOut := strOut + Before + trim(LowerCase(sqlKeyWord[intKeyWord])) + After + Spaces(cntSpaces);
        lngChar := lngChar + Length(sqlKeyWord[intKeyWord]);
      end
      else
      if (strSQL[lngChar] = CR) or (strSQL[lngChar] = LF) then
        lngChar := lngChar + 1
      else
        case strSQL[lngChar] of
          QUOTE, DOUBLEQUOTE:
          begin
            lngEnd :=
              pos(strSQL[lngChar], Copy(strSQL, lngChar + 1, Length(strsql)));
            strOut := strOut + Copy(strSQL, lngChar, lngEnd + 1);
            lngChar := lngChar + lngEnd + 1;
          end;
          SQUARE_OPEN:
          begin
            lngEnd :=
              Match(SQUARE_OPEN, SQUARE_CLOSE, Copy(strSQL, lngChar + 1, Length(strsql)));
            strOut := strOut + Copy(strSQL, lngChar, lngEnd + 1);
            lngChar := lngChar + lngEnd + 1;
          end;
          ROUND_OPEN:
          begin
            lngEnd :=
              Match(ROUND_OPEN, ROUND_CLOSE, Copy(strSQL, lngChar + 1, Length(strsql)));

            strOut := strOut + Copy(strSQL, lngChar, lngEnd + 1);
            // Strout := strOut + ROUND_OPEN+ FormatSQL(Copy(strSQL, lngChar+1, lngEnd -1 )) + ROUND_CLOSE;
            lngChar := lngChar + lngEnd + 1;
          end;
          SPACE:
          begin
            strout := strout + SPACE;
            Inc(lngChar);
          end;
          COMMA:
          begin
            Strout := Strout + CRLF + Aligner + strSQL[lngChar];
            Inc(lngChar);
            if strSQL[lngChar] = Space then
              Inc(lngChar);
          end;
          SEMICOLON:
          begin
            Strout := Strout + strSQL[lngChar] + CRLF;
            EmitStatement;
            strSQL[lngChar] := SPACE;

          end;
          else
          begin
            //if strSQL[lngChar] = '!' then
            //  strSQL[lngChar] := '|';
            strOut := strOut + UpperCase(strSQL[lngChar]);
            Inc(lngChar);
          end;
        end;
    end;
    if strOut <> '' then
      EmitStatement;
  end;

end;


end.
