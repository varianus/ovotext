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
unit uMacroEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Spin, ComCtrls, ueditor, uActionMacro,
  uReplaceMacro, SynMacroRecorder, SynEditKeyCmds, ActnList, LCLProc, SynEditTypes, uMacroRecorder;

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
    Panel1: TPanel;
    PanelRepeat: TPanel;
    pnlButtons: TPanel;
    rbRepeatNTimes: TRadioButton;
    rbRepeatUntilEof: TRadioButton;
    procedure btnPlayClick(Sender: TObject);
    procedure btnRecordClick(Sender: TObject);
    procedure btnRecordStopClick(Sender: TObject);
    procedure chkRepeatChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fFactory: TEditorFactory;
    SynMacroRec: TMacroRecorder;
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
  FMacroEditor.SynMacroRec:= TMacroRecorder.Create(Factory);
  FMacroEditor.Show;
end;


{$R *.lfm}

{ TFMacroEditor }

procedure TFMacroEditor.btnRecordClick(Sender: TObject);
begin
  SynMacroRec.Start;
end;

procedure TFMacroEditor.btnPlayClick(Sender: TObject);
var
  CurrRow, i: integer;
  ed : TEditor;
begin
  ed := fFactory.CurrentEditor;
  if not Assigned(ed) then
    exit;

  ed.SetFocus;
  if chkRepeat.Checked then
     begin
       if rbRepeatNTimes.Checked then
          SynMacroRec.Playback(True, edRepeat.Value) ;

       if rbRepeatUntilEof.Checked then
        SynMacroRec.Playback(True, -1) ;
     end

  else
    SynMacroRec.Playback();
end;

procedure TFMacroEditor.btnRecordStopClick(Sender: TObject);
begin
  SynMacroRec.Stop;
end;

procedure TFMacroEditor.chkRepeatChange(Sender: TObject);
begin
  rbRepeatNTimes.Enabled :=  chkRepeat.Checked;
  rbRepeatUntilEof.Enabled :=  chkRepeat.Checked;
  edRepeat.Enabled := chkRepeat.Checked;

end;

procedure TFMacroEditor.FormShow(Sender: TObject);
var
  Macro: RMacro;
  Item: TListItem;
begin
  lbMacroView.Clear;
  for macro in SynMacroRec.Macros do
    begin
      item := lbMacroView.Items.Add;
      item.Caption := Macro.Name;
      item.SubItems.Add(ShortCutToText(Macro.ShortCut));
    end;

end;



initialization
  FMacroEditor := nil;

finalization
  if Assigned(FMacroEditor) then
    FMacroEditor.Free;

end.

