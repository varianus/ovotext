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
unit MySEHighlighterRust;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynFacilHighlighter, SynEditHighlighter;

Type

  { TMySEHighlighterRust }

  TMySEHighlighterRust              = Class(TSynFacilSyn)
    Private
     fKeyWordList                       : TStringList;

    Protected
     function IsFilterStored: Boolean; override;
     function GetSampleSource: string; override;
    Public
     Constructor Create(AOwner: TComponent); Override;
     Destructor Destroy; Override;

     class function GetLanguageName: string; override;
  End;

const
  SYNS_FilterRust        = 'Rust source files (*.rs;*.rslib)|*.rs;*.rslib';

implementation

Uses SynFacilBasic;

Const

     SYNS_LangTust          = 'Rust';

     RustKeyWords           = 'as,break,const,continue,crate,else,enum,extern,' +
                              'false,fn,for,if,impl,in,let,loop,match,mod,move,' +
                              'mut,pub,ref,return,self,static,struct,super,' +
                              'trait,true,type,unsafe,use,where,while,dyn,' +
                              'abstract,become,box,do,final,macro,override,priv,' +
                              'typeof,unsized,virtual,yield,async,await,try,' +
                              'union,bool,u8,u16,u32,u64,u128,i8,i16,' +
                              'i32,i64,i128,char,str';

{ TMySEHighlighterRust }

function TMySEHighlighterRust.IsFilterStored: Boolean;
begin
 Result := fDefaultFilter <> SYNS_FilterRust;
end;

function TMySEHighlighterRust.GetSampleSource: string;
begin
  Result:=inherited GetSampleSource;
end;

constructor TMySEHighlighterRust.Create(AOwner: TComponent);
 Var I : Integer;
begin
 LangName :=  SYNS_LangTust;
 fKeyWordList := TStringList.Create;
 fKeyWordList.Delimiter := ',';
 fKeyWordList.StrictDelimiter := True;

 fKeyWordList.DelimitedText := RustKeyWords;

 Inherited Create(AOwner);

 ClearMethodTables;
 ClearSpecials;

 DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');

 For I := 0 To fKeyWordList.Count - 1 Do
  AddKeyword(fKeyWordList[I]);

 fKeyWordList.Free;

 DefTokDelim('"','"', tnString);
 DefTokDelim('//','', tnComment);

 DefTokContent('#', '[A-Za-z]*', tnDirective);
 DefTokContent('[0123456789]','[0-9]', tnNumber);
 //DefTokContent('''','[0-9A-FHOa-fho]*', tnNumber);

 fDefaultFilter := SYNS_FilterRust;

 Rebuild;

 SetAttributesOnChange(@DefHighlightChange);
end;

destructor TMySEHighlighterRust.Destroy;
begin
  inherited Destroy;
end;

class function TMySEHighlighterRust.GetLanguageName: string;
begin
  Result:=SYNS_LangTust;
end;

Initialization
  {$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TMySEHighlighterRust);
  {$ENDIF}
end.

