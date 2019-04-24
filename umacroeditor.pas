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
  uReplaceMacro, SynEditKeyCmds, ActnList, LCLProc, uMacroRecorder, Stringcostants;

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
    procedure btnDeleteClick(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnRecordClick(Sender: TObject);
    procedure btnRecordStopClick(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
    procedure chkRepeatChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    fFactory: TEditorFactory;
    SynMacroRec: TMacroRecorder;
    procedure ReloadMacros;
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
  ed : TEditor;
begin
  ed := fFactory.CurrentEditor;
  if not Assigned(ed) then
    exit;

  ed.SetFocus;
  if chkRepeat.Checked then
     begin
       if rbRepeatNTimes.Checked then
          SynMacroRec.Playback(TMacro(lbMacroView.Selected.Data),  True, edRepeat.Value) ;

       if rbRepeatUntilEof.Checked then
        SynMacroRec.Playback(TMacro(lbMacroView.Selected.Data), True, -1) ;
     end

  else
    SynMacroRec.Playback(TMacro(lbMacroView.Selected.Data));
end;

procedure TFMacroEditor.btnDeleteClick(Sender: TObject);
begin
  if not Assigned(lbMacroView.Selected) then
    exit;

  if MessageDlg(RSMacro, RSMacroDelete, mtConfirmation, [mbYes,mbNo], 0) = mrYes then
    begin
      SynMacroRec.Macros.Remove(TMacro(lbMacroView.Selected.Data));
    end;
  ReloadMacros;
end;

procedure TFMacroEditor.btnRecordStopClick(Sender: TObject);
begin
  SynMacroRec.Stop;
  ReloadMacros;
end;

procedure TFMacroEditor.btnRenameClick(Sender: TObject);
var
  NewName: string;
begin
  if not Assigned(lbMacroView.Selected) then
    exit;

  NewName := trim(InputBox(RSMacro, RSMacroNewName, lbMacroView.Selected.Caption));
  if (NewName <> '') and (NewName <>  lbMacroView.Selected.Caption) then
     begin
       TMacro(lbMacroView.Selected.Data).Name := NewName;
     end;
  ReloadMacros;

end;

procedure TFMacroEditor.chkRepeatChange(Sender: TObject);
begin
  rbRepeatNTimes.Enabled :=  chkRepeat.Checked;
  rbRepeatUntilEof.Enabled :=  chkRepeat.Checked;
  edRepeat.Enabled := chkRepeat.Checked;

end;

procedure TFMacroEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SynMacroRec.SaveMacros;
end;

procedure TFMacroEditor.FormShow(Sender: TObject);
begin
  ReloadMacros;
end;

procedure TFMacroEditor.ReloadMacros;
var
  Macro: TMacro;
  Item: TListItem;
  i: Integer;
begin
  lbMacroView.Clear;
  for i := 0 to SynMacroRec.Macros.Count - 1 do
    begin
      Macro := SynMacroRec.Macros[i];
      item := lbMacroView.Items.Add;
      item.Caption := Macro.Name;
      item.Data := Macro;
      item.SubItems.Add(ShortCutToText(Macro.ShortCut));
    end;

end;

initialization
  FMacroEditor := nil;

finalization
  if Assigned(FMacroEditor) then
    FMacroEditor.Free;

end.

