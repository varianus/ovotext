{ <PiNote - free source code editor>

Copyright (C) <2020> <Enzo Antonio Calogiuri> <ecalogiuri(at)gmail.com>

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
to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1335, USA.
}
unit MySEHighlighterGO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynFacilHighlighter, SynEditHighlighter;

Type

    { TMySEHighlighterGO }

    TMySEHighlighterGO               = Class(TSynFacilSyn)
     Private
      fKeyWordList                       : TStringList;

     Protected
      function IsFilterStored: Boolean; override;
      function GetSampleSource: string; override;

     Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;

      class function GetLanguageName: string; override;
    end;
Const
     SYNS_FilterGO        = 'Go source files (*.go)|*.go';

implementation

Uses SynFacilBasic;

Const
     SYNS_LangGO          = 'Go';

     GOKeyWords           = 'break,case,chan,const,continue,default,defer,else,' +
                            'fallthrough,for,func,go,goto,if,import,interface,' +
                            'map,package,range,return,select,struct,switch,type,' +
                            'var,bool,byte,complex64,complex128,error,float32,' +
                            'float64,int,int8,int16,int32,int64,rune,string,uint,' +
                            'uint8,uint16,uint32,uint64,uintptr,true,false,iota,' +
                            'nil,append,cap,close,complex,copy,delete,imag,' +
                            'len,make,new,panic,print,println,real,recover';

{ TMySEHighlighterGO }

function TMySEHighlighterGO.IsFilterStored: Boolean;
begin
 Result := fDefaultFilter <> SYNS_FilterGO;
end;

function TMySEHighlighterGO.GetSampleSource: string;
begin
 Result := '//Simple GO source code' + #13#10 +
           '' + #13#10 +
           'package main' + #13#10 +
           '' + #13#10 +
           'import (' + #13#10 +
           '	"fmt"' + #13#10 +
           '	"math/cmplx"' + #13#10 +
           ')' + #13#10 +
           '' + #13#10 +
           'var (' + #13#10 +
           '	ToBe   bool       = false' + #13#10 +
           '	MaxInt uint64     = 1<<64 - 1' + #13#10 +
           '	z      complex128 = cmplx.Sqrt(-5 + 12i)' + #13#10 +
           ')' + #13#10 +
           '' + #13#10 +
           'func main() {' + #13#10 +
           '	fmt.Printf("Type: %T Value: %v\n", ToBe, ToBe)' + #13#10 +
           '	fmt.Printf("Type: %T Value: %v\n", MaxInt, MaxInt)' + #13#10 +
           '	fmt.Printf("Type: %T Value: %v\n", z, z)' + #13#10 +
           '}';
end;

constructor TMySEHighlighterGO.Create(AOwner: TComponent);
 Var I : Integer;
begin
 LangName := SYNS_LangGO;
 fKeyWordList := TStringList.Create;
 fKeyWordList.Delimiter := ',';
 fKeyWordList.StrictDelimiter := True;

 fKeyWordList.DelimitedText := GOKeyWords;

 Inherited Create(AOwner);

 ClearMethodTables;
 ClearSpecials;

 DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');

 For I := 0 To fKeyWordList.Count - 1 Do
  AddKeyword(fKeyWordList[I]);

 fKeyWordList.Free;

 DefTokDelim('"','"', tnString);
 DefTokDelim('''','''', tnString);
 DefTokDelim('//','', tnComment);

 //DefTokContent('$', '[A-Za-z]*', tnDirective);
 DefTokContent('[0123456789]','[0-9]', tnNumber);
 DefTokContent('0x','[0-9A-Fa-f]*', tnNumber);

 fDefaultFilter := SYNS_FilterGO;

 Rebuild;

 SetAttributesOnChange(@DefHighlightChange);
end;

destructor TMySEHighlighterGO.Destroy;
begin
  inherited Destroy;
end;

class function TMySEHighlighterGO.GetLanguageName: string;
begin
 Result := SYNS_LangGO;
end;
Initialization
  {$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TMySEHighlighterGO);
  {$ENDIF}
end.

