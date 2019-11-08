unit umacroplayback;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, Spin, StdCtrls, ButtonPanel,
  uMacroRecorder, ueditor;

type

  { TfMacroPlayBack }

  TfMacroPlayBack = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbMacro: TComboBox;
    cbRepeatUntilEof: TRadioButton;
    chkRepeat: TRadioButton;
    edRepeat: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    PanelRepeat: TPanel;
    procedure OKButtonClick(Sender: TObject);
  private
    SynMacroRec: TMacroRecorder;
    procedure ReloadMacros;
  public

  end;

procedure ShowMacroPlayBack(Recorder: TMacroRecorder);

implementation
uses
  Config;
var
  fMacroPlayBack: TfMacroPlayBack;

{$R *.lfm}

procedure ShowMacroPlayBack(Recorder: TMacroRecorder);
begin
  if not Assigned(fMacroPlayBack) then
    fMacroPlayBack := TfMacroPlayBack.Create(Nil);
  fMacroPlayBack.SynMacroRec:= Recorder;
  fMacroPlayBack.ReloadMacros;
  fMacroPlayBack.Show;

end;

{ TfMacroPlayBack }

procedure TfMacroPlayBack.OKButtonClick(Sender: TObject);
var
  ed : TEditor;
begin
  ed := SynMacroRec.Factory.CurrentEditor;
  if not Assigned(ed) then
    exit;

  if not Assigned(cbMacro.Items.Objects[cbMacro.ItemIndex]) then
    exit;

  ed.SetFocus;
  if chkRepeat.Checked then
    SynMacroRec.Playback(TMacro(cbMacro.Items.Objects[cbMacro.ItemIndex]), True, edRepeat.Value)
  else
  if cbRepeatUntilEof.Checked then
          SynMacroRec.Playback(TMacro(cbMacro.Items.Objects[cbMacro.ItemIndex]), True, -1)
  else
    SynMacroRec.Playback(TMacro(cbMacro.Items.Objects[cbMacro.ItemIndex]));
end;

procedure TfMacroPlayBack.ReloadMacros;
var
  Macro: TMacro;
  i: Integer;
begin
  cbMacro.Clear;
  for i := 0 to SynMacroRec.Macros.Count - 1 do
    begin
      Macro := SynMacroRec.Macros[i];
      cbMacro.Items.AddObject(Macro.Name, Macro);
    end;
end;

end.

