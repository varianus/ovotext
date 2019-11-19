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
unit uMacroRecorder;

interface

uses
  Classes, SysUtils, ueditor, uActionMacro, uReplaceMacro, SynMacroRecorder, SynEditKeyCmds, ActnList, LCLProc,
  Config, Stringcostants,  lclversion,
  generics.Collections,
  ReplaceDialog, JsonTools;

type

  { TMacroRecorder }
  TMacro = Class
    Name: string;
    Commands: String;
    ShortCut: TShortCut;
    Saved: boolean;
  end;

  { TMacroList }

  TMacroList = class ( specialize TObjectList<TMacro>)
  public
    function MacroNames(List:TStrings):Integer;

  end;


  TMacroRecorder = class
  private
    fFactory: TEditorFactory;
    fOnListChange: TNotifyEvent;
    fOnStateChange: TNotifyEvent;
    fRecordedMacro: TMacro;
    InExecute: Boolean;
    SynMacroRec: TSynMacroRecorder;

    FMacros: TMacroList;

    function GetState: TSynMacroState;
    procedure pRecordActions(AAction: TBasicAction; var Handled: Boolean);
    procedure MacroListChange(ASender: TObject; constref AItem: TMacro; AAction: TCollectionNotification);
    procedure pRecordSearchReplace(Sender: TObject; const ASearch, AReplace: string; AOptions: TMySynSearchOptions);
    procedure SynMacroRecStateChange(Sender: TObject);
    procedure SynMacroRecUserCommand(aSender: TCustomSynMacroRecorder; aCmd: TSynEditorCommand; var aEvent: TSynMacroEvent);
  protected
    Function LoadMacros: integer;
  public
    Constructor Create(Factory: TEditorFactory);
    Destructor Destroy; override;
    Procedure SaveMacros;
    procedure SignalChange;
    Property LastMacro: TMacro read fRecordedMacro;

    Property State: TSynMacroState read GetState;
    property OnStateChange: TNotifyEvent read fOnStateChange write fOnStateChange;
    property OnListChange: TNotifyEvent read fOnListChange write fOnListChange;
    Property Macros: TMacroList read FMacros;
    property Factory: TEditorFactory read ffactory;

    procedure Start;
    procedure Stop;
    procedure Pause;
    procedure PlayBack(Macro: TMacro; Multiple: boolean=false; Count:integer=1);
  end;

implementation
uses
  umain;

{ TMacroList }

function TMacroList.MacroNames(List: TStrings): Integer;
var
  i: Integer;
begin
  List.Clear;
  for i:= 0 to Count -1 do
    List.Add(Items[I].name);
end;


constructor TMacroRecorder.Create(Factory: TEditorFactory);
begin
  inherited Create;
  fFactory := Factory;
  FMacros:= TMacroList.Create(true);
  SynMacroRec := TSynMacroRecorder.Create(nil);
  SynMacroRec.OnStateChange := @SynMacroRecStateChange;
  SynMacroRec.OnUserCommand := @SynMacroRecUserCommand;
  LoadMacros;
  FMacros.OnNotify := @MacroListChange;
  fRecordedMacro := nil;
end;

destructor TMacroRecorder.Destroy;
begin
  if Assigned(SynMacroRec) then
   SynMacroRec.Free;

  FMacros.Free;

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

procedure TMacroRecorder.pRecordSearchReplace(Sender:TObject; const ASearch, AReplace: string; AOptions: TMySynSearchOptions);
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

procedure TMacroRecorder.MacroListChange(ASender: TObject; constref AItem: TMacro; AAction: TCollectionNotification);
begin
  if Assigned(fOnListChange) then
     fOnListChange(self);

end;

procedure TMacroRecorder.SynMacroRecStateChange(Sender: TObject);
begin
  if Assigned(fOnStateChange) then
    fOnStateChange(Sender);

  if SynMacroRec.State = msRecording then
   begin
     fMain.ActionList.OnExecute:= @pRecordActions;
     TEditor(SynMacroRec.CurrentEditor).OnSearchReplace := @pRecordSearchReplace;
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
  ArrayNode: TJsonNode;
  MacroNode: TJsonNode;
begin

  FMacros.Clear;
  ArrayNode:= ConfigObj.ConfigHolder.Find('Macros');
  if not Assigned(ArrayNode) then
    exit;

  for MacroNode in ArrayNode do
    begin
      NewMacro := TMacro.Create;
      NewMacro.Name     := MacroNode.GetValueDef('Name','');
      NewMacro.Commands := MacroNode.GetValueDef('Commands','');
      NewMacro.ShortCut := MacroNode.GetValueDef('Shortcut',0);
      NewMacro.Saved    := true;
      FMacros.Add(NewMacro);
  end;

end;

procedure TMacroRecorder.SaveMacros;
var
 Macro: TMacro;
 ArrayNode: TJsonNode;
 MacroNode: TJsonNode;

begin

  ArrayNode:= ConfigObj.ConfigHolder.Find('Macros', true);
  ArrayNode.Kind := nkArray;
  ArrayNode.Clear;


  for Macro in Macros do
    begin
       MacroNode := ArrayNode.Add('Macro', nkObject);
       MacroNode.Find('Name',True).AsString := Macro.Name;
       MacroNode.Find('Commands',True).AsString := Macro.Commands;
       MacroNode.Find('ShortCut',True).AsInteger := Macro.ShortCut;
    end;

end;

procedure TMacroRecorder.SignalChange;
begin
  if Assigned(fOnListChange) then
    fOnListChange(self);
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

