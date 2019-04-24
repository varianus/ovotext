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
unit uMacroRecorder;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, ueditor, uActionMacro, uReplaceMacro, SynMacroRecorder, SynEditKeyCmds, ActnList, LCLProc,
  SynEditTypes, Config, Stringcostants, Generics.Collections;

type

  { TMacroRecorder }
  TMacro = Class
    Name: string;
    Commands: String;
    ShortCut: TShortCut;
    Saved: boolean;
  end;

  TMacroList = class (TObjectList<TMacro>);


  TMacroRecorder = class
  private
    fFactory: TEditorFactory;
    fOnStateChange: TNotifyEvent;
    fRecordedMacro: TMacro;
    InExecute: Boolean;
    SynMacroRec: TSynMacroRecorder;

    FMacros: TMacroList;

    function GetState: TSynMacroState;
    procedure pRecordActions(AAction: TBasicAction; var Handled: Boolean);
    procedure pRecordSearchReplace(Sender: TObject; const ASearch, AReplace: string; AOptions: TSynSearchOptions);

    procedure SynMacroRecStateChange(Sender: TObject);
    procedure SynMacroRecUserCommand(aSender: TCustomSynMacroRecorder; aCmd: TSynEditorCommand; var aEvent: TSynMacroEvent);
  protected
    Function LoadMacros: integer;
  public
    Constructor Create(Factory: TEditorFactory);
    Destructor Destroy; override;
    Procedure SaveMacros;

    Property State: TSynMacroState read GetState;
    property OnStateChange: TNotifyEvent read fOnStateChange write fOnStateChange;
    Property Macros: TMacroList read FMacros;

    procedure Start;
    procedure Stop;
    procedure Pause;
    procedure PlayBack(Macro: TMacro; Multiple: boolean=false; Count:integer=1);
  end;

implementation
uses
  umain;


constructor TMacroRecorder.Create(Factory: TEditorFactory);
begin
  inherited Create;
  fFactory := Factory;
  FMacros:= TMacroList.Create;
  SynMacroRec := TSynMacroRecorder.Create(nil);
  SynMacroRec.OnStateChange := SynMacroRecStateChange;
  SynMacroRec.OnUserCommand := SynMacroRecUserCommand;
  LoadMacros;
end;

destructor TMacroRecorder.Destroy;
begin
  if Assigned(SynMacroRec) then
    SynMacroRec.Free;
  inherited Destroy;
end;

{ TMacroRecorder }

procedure TMacroRecorder.Start;
begin
  case SynMacroRec.State of
    msStopped: begin
                 SynMacroRec.AddEditor(fFactory.CurrentEditor);
                 SynMacroRec.RecordMacro(fFactory.CurrentEditor);
                 fFactory.CurrentEditor.SetFocus;
               end;
    msRecording,
    msPlaying: exit;
    msPaused: SynMacroRec.Resume;
  end;

end;

procedure TMacroRecorder.PlayBack(Macro:TMacro; Multiple: boolean=false; Count:integer=1);
var
  CurrRow, i: integer;
  ed : TEditor;
begin
  if not (SynMacroRec.State = msStopped) then
    exit;

  ed := fFactory.CurrentEditor;
  if not Assigned(ed) then
    exit;

  ed.SetFocus;
  SynMacroRec.AsString := Macro.Commands;
  if multiple then
     begin
       if count > 0 then
        begin
          for i := 0 to pred(Count) do
            SynMacroRec.PlaybackMacro(Ed);
        end
       else
        begin
          // Avoid macro infinite loop:
          // if after playback i'm on the same line stop execution loop
          CurrRow := ed.CaretY;
          repeat
            SynMacroRec.PlaybackMacro(Ed);

          until (ed.CaretY = 1) or
                (ed.CaretY = ed.Lines.Count ) or
                (ed.CaretY = CurrRow);
        end;

     end

  else
    SynMacroRec.PlaybackMacro(fFactory.CurrentEditor);
end;

procedure TMacroRecorder.Stop;
begin
  if not (SynMacroRec.State in [msRecording, msPaused]) then
    exit;
  fRecordedMacro := TMacro.Create;
  fRecordedMacro.Commands := SynMacroRec.AsString;
  TEditor(SynMacroRec.CurrentEditor).OnSearchReplace := nil;
  fRecordedMacro.Name := RSMacroDefaultName;
  fRecordedMacro.Saved := false;
  FMacros.Add(fRecordedMacro);
  SynMacroRec.Stop;
  SaveMacros;
end;

procedure TMacroRecorder.Pause;
begin
  if SynMacroRec.State = msRecording then
    SynMacroRec.Pause;
end;

procedure TMacroRecorder.pRecordSearchReplace(Sender:TObject; const ASearch, AReplace: string; AOptions: TSynSearchOptions);
var
  AEvent: TReplaceMacroEvent;
begin
  with SynMacroRec do
  begin
    AEvent                := TReplaceMacroEvent.Create;
    AEvent.Search         := ASearch;
    AEvent.Replace        := AReplace;
    AEvent.ReplaceOptions := AOptions;
    AddCustomEvent(TSynMacroEvent(AEvent));

  end;
end;

procedure TMacroRecorder.SynMacroRecStateChange(Sender: TObject);
begin
  if Assigned(fOnStateChange) then
    fOnStateChange(Sender);

  if SynMacroRec.State = msRecording then
   begin
     fMain.ActionList.OnExecute:= pRecordActions;
     TEditor(SynMacroRec.CurrentEditor).OnSearchReplace := pRecordSearchReplace;
   end
   else
   begin
     if Assigned(fMain.ActionList.OnExecute) then
       fMain.ActionList.OnExecute:= nil;
     if assigned(SynMacroRec.CurrentEditor) then
     TEditor(SynMacroRec.CurrentEditor).OnSearchReplace := nil;
   end;
end;

procedure TMacroRecorder.SynMacroRecUserCommand(aSender: TCustomSynMacroRecorder; aCmd: TSynEditorCommand;
  var aEvent: TSynMacroEvent);
begin
  if aCmd = ecAction then
   begin
     aEvent := TActionMacroEvent.Create;
     TActionMacroEvent(AEvent).ActionLists.Add(fMain.ActionList);
   end;
  if aCmd = ecReplace then
   begin
     aEvent := TReplaceMacroEvent.Create;
   end;

end;

function TMacroRecorder.LoadMacros: integer;
var
  NewCount: LongInt;
  NewMacro: TMacro;
  i: Integer;
begin
  NewCount := ConfigObj.ConfigHolder.GetValue('Macros/Count',0);

  for i:=0 to NewCount-1 do
    begin
    NewMacro := TMacro.Create;
    NewMacro.Name     := ConfigObj.ConfigHolder.GetValue('Macros/Macro'+IntToStr(i+1)+'/Name','');
    NewMacro.Commands := ConfigObj.ConfigHolder.GetValue('Macros/Macro'+IntToStr(i+1)+'/Commands','');
    NewMacro.ShortCut := ConfigObj.ConfigHolder.GetValue('Macros/Macro'+IntToStr(i+1)+'/Shortcut',0);
    NewMacro.Saved    := true;

    if FMacros.Count>i then
      FMacros[i]:=NewMacro
    else
      FMacros.Add(NewMacro);
  end;
  while FMacros.Count>NewCount do FMacros.Delete(FMacros.Count-1);

end;

procedure TMacroRecorder.SaveMacros;
var
  i: Integer;
begin
  ConfigObj.ConfigHolder.SetDeleteValue('Macros/Count',FMacros.Count, 0);
  for i := 0 to FMacros.Count -1 do
    begin
     ConfigObj.ConfigHolder.SetDeleteValue('Macros/Macro'+IntToStr(i+1)+'/Name',FMacros[i].Name,'') ;
     ConfigObj.ConfigHolder.SetDeleteValue('Macros/Macro'+IntToStr(i+1)+'/Commands',FMacros[i].Commands,'') ;
     ConfigObj.ConfigHolder.SetDeleteValue('Macros/Macro'+IntToStr(i+1)+'/Shortcut',FMacros[i].ShortCut,0) ;
     FMacros[i].Saved := true;
    end;

end;

procedure TMacroRecorder.pRecordActions(AAction: TBasicAction; var Handled: Boolean);
var
  AEvent: TActionMacroEvent;
begin

  // record only actions that do simple text manipulation
  if not InExecute and (aaction.name <> '') and ((AAction.Tag and 1) = 1) then
    with SynMacroRec do
    begin
      AEvent:= TActionMacroEvent.Create;
      AEvent.ActionName:= AAction.Name;
      AEvent.ActionLists.Add(fMain.ActionList);
      AddCustomEvent(TSynMacroEvent(AEvent));
      InExecute:= True;
      try
        AAction.Execute;
        Handled:= True;
      finally
        InExecute:= False;
      end;
    end;
end;

function TMacroRecorder.GetState: TSynMacroState;
begin
  Result := SynMacroRec.State;
end;


initialization

finalization

end.

