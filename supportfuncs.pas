unit SupportFuncs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure StrToStrings(S, Sep: string; const List: TStrings; const AllowEmptyString: boolean = True);

implementation

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

