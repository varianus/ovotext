unit SupportFuncs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
//Remove invalid char from highlighters name and attributes
Function CleanupName(aName:string):string;

//Split a delimited string in a Stringlist
procedure StrToStrings(S, Sep: string; const List: TStrings; const AllowEmptyString: boolean = True);


function RemoveSpacesInExcess(s: string): string;

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

function RemoveSpacesInExcess(s: string): string;
var
  p: integer;
begin
  s := trim(s);
  p := pos(#32#32, s);
  while p > 0 do
  begin
    s := StringReplace(s, #32#32, #32, [rfReplaceAll]);
    p := pos(#32#32, s);
  end;
  Result := s;
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

end.
