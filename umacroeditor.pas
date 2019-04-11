unit uMacroEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Spin, ComCtrls, ueditor, uActionMacro,
  SynMacroRecorder, SynEditKeyCmds, ActnList, LCLProc;

type

  { TFMacroEditor }

  TFMacroEditor = class(TForm)
    btnDelete: TButton;
    btnEdit: TButton;
    btnPlay: TButton;
    btnRecord: TButton;
    btnRecordStop: TButton;
    btnRename: TButton;
    btnSelect: TButton;
    btnSetKeys: TButton;
    chkRepeat: TCheckBox;
    edRepeat: TSpinEdit;
    Label1: TLabel;
    lbMacroView: TListView;
    PanelRepeat: TPanel;
    pnlButtons: TPanel;
    rbRepeatNTimes: TRadioButton;
    rbRepeatUntilEof: TRadioButton;
    SynMacroRec: TSynMacroRecorder;
    procedure btnPlayClick(Sender: TObject);
    procedure btnRecordClick(Sender: TObject);
    procedure btnRecordStopClick(Sender: TObject);
    procedure chkRepeatChange(Sender: TObject);
    procedure SynMacroRecStateChange(Sender: TObject);
    procedure SynMacroRecUserCommand(aSender: TCustomSynMacroRecorder; aCmd: TSynEditorCommand;
      var aEvent: TSynMacroEvent);
  private
    fFactory: TEditorFactory;
    mc: string;
    InExecute: Boolean;
    procedure pRecordActions(AAction: TBasicAction; var Handled: Boolean);
  public

  end;

Procedure ShowMacroEditor(Factory: TEditorFactory);

implementation
uses
  umain;
var
  FMacroEditor: TFMacroEditor;

procedure ShowMacroEditor(Factory: TEditorFactory);
begin
  if not Assigned(FMacroEditor) then
   FMacroEditor := TFMacroEditor.Create(Nil);
  FMacroEditor.fFactory := Factory;
  FMacroEditor.Show;
end;


{$R *.lfm}

{ TFMacroEditor }

procedure TFMacroEditor.btnRecordClick(Sender: TObject);
begin
  SynMacroRec.AddEditor(fFactory.CurrentEditor);
  SynMacroRec.RecordMacro(fFactory.CurrentEditor);
  fFactory.CurrentEditor.SetFocus;
end;

procedure TFMacroEditor.btnPlayClick(Sender: TObject);
begin
  fFactory.CurrentEditor.SetFocus;
  SynMacroRec.AsString := mc;
  SynMacroRec.PlaybackMacro(fFactory.CurrentEditor);
end;

procedure TFMacroEditor.btnRecordStopClick(Sender: TObject);
begin
  mc := SynMacroRec.AsString;
  SynMacroRec.Stop;
end;

procedure TFMacroEditor.chkRepeatChange(Sender: TObject);
begin
  rbRepeatNTimes.Enabled :=  chkRepeat.Checked;
  rbRepeatUntilEof.Enabled :=  chkRepeat.Checked;
  edRepeat.Enabled := chkRepeat.Checked;

end;

procedure TFMacroEditor.SynMacroRecStateChange(Sender: TObject);
begin
  if SynMacroRec.State = msRecording then
   begin
     fMain.ActionList.OnExecute:= @pRecordActions;
   end
   else if Assigned(fMain.ActionList.OnExecute) then
   begin
     fMain.ActionList.OnExecute:= nil;
   end;
end;

procedure TFMacroEditor.SynMacroRecUserCommand(aSender: TCustomSynMacroRecorder; aCmd: TSynEditorCommand;
  var aEvent: TSynMacroEvent);
begin
  if aCmd = ecAction then
   begin
     aEvent := TActionMacroEvent.Create;
     TActionMacroEvent(AEvent).ActionLists.Add(fMain.ActionList);
   end;
end;

procedure TFMacroEditor.pRecordActions(AAction: TBasicAction; var Handled: Boolean);
var
  AEvent: TActionMacroEvent;
begin

  if not InExecute and (aaction.name <> '') then
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


initialization
  FMacroEditor := nil;

finalization
  if Assigned(FMacroEditor) then
    FMacroEditor.Free;

end.

