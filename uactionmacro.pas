{
  The contents of this file are subject to the terms and conditions found under
  the GNU General Public License Version 2 or later (the "GPL").
  See http://www.opensource.org/licenses/gpl-license.html or
  http://www.fsf.org/copyleft/gpl.html for further information.


  The TActionMacroEvent class was taken from the uSynAPI.pas file in the syn
  project (see http://syn.sourceforge.net/). The only changes made is to
  create a separate unit for the class and the addition of constructor and
  destructor methods as well as the ActionLists property.

  How it works:

    The TSynMacroRecorder works by recording events and keeping them in an
    array. It records all text editting functionailty, but does not record
    external events like searching/replacing, and so on.

    However, since each event in the array is actually a separate object,
    you can add custom events to the recorder by inheriting from the base
    TSynMacroEvent class and implementing new event recording functionality.

    This class (TActionMacroEvent) is one such class and is designed for the
    recording of action list events and their subsequent playback. It works by
    hooking onto the action list's OnExecute function and creating a new
    TActionMacroEvent object and adding it to the recorder's event list.

    By storing the Action List's name and providing to the TActionMacroEvent
    class the list of all possible actions, the object is able to locate the
    recorded action and re-execute it on playback.

  How to use this class:

    1) Include a TSynMacroRecorder in your project and add a method to the
       OnStateChange event. Add the following StateChange function to your form.

       This code assumes your form class is TfrmMain, the name of the
       TSynMacroRecorder instance is SynMR, and you have 4 action lists
       (alEdit, alFile, alFormat and alSearch).

          procedure TfrmMain.SynMRStateChange(Sender: TObject);
          begin
            if SynMR.State = msRecording then
            begin
              alEdit.OnExecute:= pRecordActions;
              alFile.OnExecute:= pRecordActions;
              alFormat.OnExecute:= pRecordActions;
              alSearch.OnExecute:= pRecordActions;
            end
            else if Assigned(alEdit.OnExecute) then
            begin
              alEdit.OnExecute:= nil;
              alFile.OnExecute:= nil;
              alFormat.OnExecute:= nil;
              alSearch.OnExecute:= nil;
            end;
          end;

    2) Add the InExecute variable and pRecordActions procedure to your form:

        var
          InExecute: Boolean;

        procedure TfrmMain.pRecordActions(Action: TBasicAction; var Handled: Boolean);
        var
          AEvent: TActionMacroEvent;
        begin
          if not InExecute and (Action <> actRecord) and (Action <> actPlay) then
            with SynMR do
            begin
              AEvent:= TActionMacroEvent.Create;
              AEvent.ActionName:= Action.Name;
              AEvent.ActionLists.Add(alEdit);
              AEvent.ActionLists.Add(alFile);
              AEvent.ActionLists.Add(alFormat);
              AEvent.ActionLists.Add(alSearch);
              AddCustomEvent(TSynMacroEvent(AEvent));
              InExecute:= True;
              try
                Action.Execute;
                Handled:= True;
              finally
                InExecute:= False;
              end;
            end;
        end;

    3) Done!

    Written by lesx99 and donated to the Tinn project. Thanks!
}

(*
 Tinn is a ASCII file editor primarily intended as a better replacement
 of the default Notepad.exe under Windows.

 This software is distributed under the terms of the GNU General
 Public License, either Version 2, June 1991 or Version 3, June 2007.
 The terms of version 2 and of the license are in a folder called
 doc (licence_gpl2.txt and licence_gpl2.txt)
 which you should have received with this software.

 See http://www.opensource.org/licenses/gpl-license.html or
 http://www.fsf.org/copyleft/gpl.html for further information.

 Copyright
  Russell May - http://www.solarvoid.com

 Tinn-R is an extension of Tinn that provides additional tools to control R
 (http://cran.r-project.org). The project is coordened by Jos� Cl�udio Faria
 (joseclaudio.faria@gmail.com).

 As such, Tinn-R is a feature-rich replacement of the basic script editor
 provided with Rgui. It provides syntax-highlighting, submission of code in
 whole, or line-by-line, and many other useful tools to ease writting and
 debugging of R code.

 Copyright
  Tinn-R team October/2005
  Tinn-R team October/2013

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 and 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

unit uActionMacro;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SynEdit, Menus, StdActns, ActnList, StdCtrls, ExtCtrls, LCLType,
  SynEditKeyCmds, SynMacroRecorder;

const
  ecAction: TsynEditorCommand = ecUserFirst +15000;

type
  TActionMacroEvent = class(TSynMacroEvent)

  private
    fActionName: string;
    fActionLists: TList;
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
    property ActionName: string read fActionName write fActionName;
    property ActionLists: TList read fActionLists write fActionLists;
  end;

implementation

{ TActionMacroEvent }

constructor TActionMacroEvent.Create;
begin
  inherited Create;
  fActionLists := TList.Create;
end;

destructor TActionMacroEvent.Destroy;
begin
  FreeAndNil(fActionLists);
end;

function TActionMacroEvent.GetAsString: String;
begin
  Result := 'ecAction ' + FActionName;
  if RepeatCount > 1 then Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TActionMacroEvent.InitEventParameters(sStr: String);
var
  cHead, cTail: PChar;

begin
  cHead:= PChar(sStr);
  while cHead^ in [#6, ' '] do Inc(cHead);
  cTail:= cHead;
  while cTail^ in ['A'..'Z', 'a'..'z', '0'..'9', '_'] do Inc(cTail);
  fActionName:= Copy(cHead, 1, cTail-cHead);
  RepeatCount := StrToIntDef(Trim(cTail), 1);
end;

procedure TActionMacroEvent.Initialize(aCmd: TSynEditorCommand; const aChar: TUTF8Char; aData: Pointer);
begin
  fActionName := String(aData);
end;

procedure TActionMacroEvent.LoadFromStream(aStream: TStream);
var
  l : Integer;
  cBuff : PChar;

begin
  aStream.Read(l, SizeOf(l));
  GetMem(cBuff, l);
  try
    FillByte(cBuff, l, 0);
    aStream.Read(cBuff^, l);
    fActionName := cBuff;
  finally
    FreeMem(cBuff);
  end;
  aStream.Read( fRepeatCount, SizeOf(fRepeatCount) );
end;

procedure TActionMacroEvent.Playback(aEditor: TCustomSynEdit);

  function GetLoadedAction(const AActionName: string): TContainedAction;
  (* returns already created action with name AActionName *)
  var
    i, j: integer;
  begin
    Assert(Assigned(fActionLists));
    for i:= 0 to fActionLists.Count-1 do
      with TCustomActionList(fActionLists[i]) do
        for j:= 0 to ActionCount-1 do
          if SameText(Actions[j].Name, AActionName) then begin
            Result:= Actions[j];
            Exit;
          end;
    Result:= nil;
  end;

var
  action: TContainedAction;
  i: Integer;

begin
  action:= GetLoadedAction(FActionName);
  if Assigned(action) then for i:= 1 to RepeatCount do
    action.Execute;
end;

procedure TActionMacroEvent.SaveToStream(aStream: TStream);
var
  l : Integer;
  cBuff : PChar;

begin
  aStream.Write(ecAction, SizeOf(ecAction));
  l := Length(FActionName) + 1;
  aStream.Write(l, sizeof(l));
  GetMem(cBuff, l);
  try
    FillByte(cBuff, l, 0);
    StrPCopy(cBuff, fActionName);
    aStream.Write(cBuff^, l);
  finally
    FreeMem(cBuff);
  end;
  aStream.Write( RepeatCount, SizeOf(RepeatCount) );
end;


const
  SynActionCommandStrs: array[0..0] of TIdentMapEntry = (
    (Value: ecUserFirst +15000;       Name: 'ecAction')
  );

function IdentToActionCommand(const Ident: string; var Cmd: longint): boolean;
begin
  Result := IdentToInt(Ident, Cmd, SynActionCommandStrs);
end;

function ActionCommandToIdent(Cmd: longint; var Ident: string): boolean;
begin
  Result := Cmd = ecAction;
  if not Result then exit;
  Result := IntToIdent(Cmd, Ident, SynActionCommandStrs);
end;

initialization
 RegisterKeyCmdIdentProcs(@IdentToActionCommand, @ActionCommandToIdent);

end.
