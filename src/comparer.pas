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
unit Comparer;

interface

type
  TCompareString = function(const Item1, Item2: string): integer of object;

  TBaseComparer = class
  public
    Ascending: boolean;
    AsNumeric: boolean;
    function Compare(const Item1, Item2: string): integer; virtual abstract;
  end;

  TSimpleComparer = class(TBaseComparer)
    function Compare(const Item1, Item2: string): integer; override;
  end;


  { RColumnComparer }

  TColumnComparer = class(TBaseComparer)
    ColStart, ColEnd: integer;
    function Compare(const Item1, Item2: string): integer; override;
  end;

  { RDelimitedComparer }

  TDelimitedComparer = class(TBaseComparer)
    Column: integer;
    Delimiters: string;
    QuoteChar: char;
    function Compare(const Item1, Item2: string): integer; override;
  end;


implementation

uses
  Classes, SysUtils, strutils;

function TSimpleComparer.Compare(const Item1, Item2: string): integer;
begin
  Result := CompareStr(Item1, Item2);
  if not Ascending then
    Result := Result * -1;
end;

{ RColumnComparer }

function TColumnComparer.Compare(const Item1, Item2: string): integer;
var
  Value1, Value2: string;
  Num1, Num2: double;
begin
  Value1 := Copy(Item1, ColStart, ColEnd - ColStart);
  Value2 := Copy(Item2, ColStart, ColEnd - ColStart);

  if AsNumeric then
    if TryStrToFloat(Value1, Num1) and TryStrToFloat(Value2, Num2) then
    begin
      // Ordinamento numerico
      if Num1 < Num2 then
        Result := -1
      else if Num1 > Num2 then
        Result := 1
      else
        Result := 0;
      if not Ascending then
        Result := Result * -1;
      exit;
    end;
  Result := CompareStr(Value1, Value2);
  if not Ascending then
    Result := Result * -1;

end;

{ RDelimitedComparer }

function TDelimitedComparer.Compare(const Item1, Item2: string): integer;

  function GetColumn(const Line: string; ColumnIndex: integer): string;
  var
    Columns: specialize TArray<string>;
  begin
    Result := '';
    if QuoteChar <> #00 then
      Columns := Line.Split(Delimiters)
    else
      Columns := Line.Split(Delimiters, QuoteChar);

    if (ColumnIndex >= 0) and (ColumnIndex < Length(Columns)) then
      Result := Columns[ColumnIndex].DeQuotedString(QuoteChar);

    if AsNumeric and (QuoteChar <> #00) then
      Result:=Result.DeQuotedString(QuoteChar);
  end;

var
  Value1, Value2: string;
  Num1, Num2: double;
begin
  Value1 := GetColumn(Item1, Column);
  Value2 := GetColumn(item2, Column);

  if AsNumeric then
    if TryStrToFloat(Value1, Num1) and TryStrToFloat(Value2, Num2) then
    begin
      // Ordinamento numerico
      if Num1 < Num2 then
        Result := -1
      else if Num1 > Num2 then
        Result := 1
      else
        Result := 0;
      if not Ascending then
        Result := Result * -1;
      exit;
    end;
  Result := CompareStr(Value1, Value2);
  if not Ascending then
    Result := Result * -1;

end;

end.

