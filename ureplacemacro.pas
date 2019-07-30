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
unit uReplaceMacro;

interface

uses
  SysUtils, Classes, TypInfo, Graphics, Controls, Forms, Dialogs,
  SynEdit, Menus, ActnList, StdCtrls, ExtCtrls, LCLType,
  ReplaceDialog, fpjson,
  SynEditKeyCmds, SynMacroRecorder, SynEditTypes;

const
  ecReplace: TsynEditorCommand = ecUserFirst +15001;

type

  { TReplaceMacroEvent }

  TReplaceMacroEvent = class(TSynMacroEvent)

  private
    fSearch, fReplace: string;
    fReplaceOptions: TMySynSearchOptions;
  protected
    function GetAsString : String; override;
    procedure InitEventParameters(sStr : String); override;
  public
    constructor Create(); override;
    destructor Destroy; override;
    procedure Initialize(aCmd: TSynEditorCommand; const aChar: TUTF8Char; aData: Pointer);      override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure Playback(aEditor: TCustomSynEdit); override;
  public
    property Replace: string read fReplace write fReplace;
    property Search: string read fSearch write fSearch;
    property ReplaceOptions: TMySynSearchOptions read fReplaceOptions write fReplaceOptions;

  end;

implementation

{ TReplaceMacroEvent }

constructor TReplaceMacroEvent.Create();
begin
  inherited Create;
end;

destructor TReplaceMacroEvent.Destroy;
begin
end;

function TReplaceMacroEvent.GetAsString: String;
begin

  Result := 'ecReplace ' + fSearch  + #09 +
                           fReplace + #09 +
                           SetToString(PTypeInfo(TypeInfo(TmySynSearchOptions)), LongInt(fReplaceOptions), true);
end;

procedure TReplaceMacroEvent.InitEventParameters(sStr: String);
var
  cHead: PChar;
  st: TStringList;
begin
  cHead:= PChar(sStr);
  while cHead^ in [#6, ' '] do Inc(cHead);
  st := TStringList.Create;
  try
    st.Delimiter := #09;
    st.StrictDelimiter := true;
    st.DelimitedText := Copy(cHead, 1, Length(sStr));
    fSearch := st[0];
    fReplace := st[1];
    fReplaceOptions:=TMySynSearchOptions(StringToSet(PTypeInfo(TypeInfo(TMySynSearchOptions)), st[2]));
  finally
    st.free
  end;


end;

procedure TReplaceMacroEvent.Initialize(aCmd: TSynEditorCommand; const aChar: TUTF8Char; aData: Pointer);
begin
  fSearch := String(aData);
end;

procedure TReplaceMacroEvent.LoadFromStream(aStream: TStream);
var
  l : Integer;
  cBuff : PChar;

begin
  aStream.Read(l, SizeOf(l));
  GetMem(cBuff, l);
  try
    FillByte(cBuff, l, 0);
    aStream.Read(cBuff^, l);
    InitEventParameters(cBuff);
  finally
    FreeMem(cBuff);
  end;
end;

procedure TReplaceMacroEvent.Playback(aEditor: TCustomSynEdit);
var
  ReplaceText, FindText: String;
begin
  if ssoExtended in TMySynSearchOptions(fReplaceOptions) then
    begin
      FindText := JSONStringToString(fSearch);
      ReplaceText := JSONStringToString(fReplace);
    end
  else
    begin
      FindText := fSearch;
      ReplaceText := fReplace;
    end;
  aEditor.SearchReplace(FindText, ReplaceText, TSynSearchOptions(fReplaceOptions));
end;

procedure TReplaceMacroEvent.SaveToStream(aStream: TStream);
var
  l : Integer;
  cBuff : PChar;

begin
  aStream.Write(ecReplace, SizeOf(ecReplace));
  l := Length(AsString) + 1;
  aStream.Write(l, sizeof(l));
  GetMem(cBuff, l);
  try
    FillByte(cBuff, l, 0);
    StrPCopy(cBuff, AsString);
    aStream.Write(cBuff^, l);
  finally
    FreeMem(cBuff);
  end;

end;


const
  SynReplaceCommandStrs: array[0..0] of TIdentMapEntry = (
    (Value: ecUserFirst +15001;       Name: 'ecReplace')
  );

function IdentToReplaceCommand(const Ident: string; var Cmd: longint): boolean;
begin
  Result := IdentToInt(Ident, Cmd, SynReplaceCommandStrs);
end;

function ReplaceCommandToIdent(Cmd: longint; var Ident: string): boolean;
begin
  Result := Cmd = ecReplace;
  if not Result then exit;
  Result := IntToIdent(Cmd, Ident, SynReplaceCommandStrs);
end;

initialization
 RegisterKeyCmdIdentProcs(@IdentToReplaceCommand, @ReplaceCommandToIdent);

end.
