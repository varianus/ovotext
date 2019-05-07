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
  uReplaceMacro, SynEditKeyCmds, SynEdit, ActnList, LCLProc, uMacroRecorder, Stringcostants;

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
    lbMacroView: TListView;
    pnlTop: TPanel;
    PanelRepeat: TPanel;
    pnlButtons: TPanel;
    cbRepeatUntilEof: TCheckBox;
    SynEdit1: TSynEdit;
    procedure btnDeleteClick(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnRecordClick(Sender: TObject);
    procedure btnRecordStopClick(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
    procedure cbRepeatUntilEofChange(Sender: TObject);
    procedure chkRepeatChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure lbMacroViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    SynMacroRec: TMacroRecorder;
    procedure ReloadMacros;
  public

  end;

Procedure ShowMacroEditor(Recorder: TMacroRecorder);

implementation
uses
  Config;
var
  FMacroEditor: TFMacroEditor;

procedure ShowMacroEditor(Recorder: TMacroRecorder);
begin
  if not Assigned(FMacroEditor) then
   FMacroEditor := TFMacroEditor.Create(Nil);
  FMacroEditor.SynMacroRec:= Recorder;
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
  ed := SynMacroRec.Factory.CurrentEditor;
  if not Assigned(ed) then
    exit;

  ed.SetFocus;
  if chkRepeat.Checked then
     begin
       if cbRepeatUntilEof.Checked then
        SynMacroRec.Playback(TMacro(lbMacroView.Selected.Data), True, -1)
       else
        SynMacroRec.Playback(TMacro(lbMacroView.Selected.Data),  True, edRepeat.Value) ;
     end

  else
    SynMacroRec.Playback(TMacro(lbMacroView.Selected.Data));
end;

procedure TFMacroEditor.btnDeleteClick(Sender: TObject);
begin
  if not Assigned(lbMacroView.Selected) then
    exit;

  if MessageDlg(RSMacro, format(RSMacroDelete, [lbMacroView.Selected.Caption]), mtConfirmation, [mbYes,mbNo], 0) = mrYes then
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

procedure TFMacroEditor.cbRepeatUntilEofChange(Sender: TObject);
begin
    edRepeat.Enabled := not cbRepeatUntilEof.Checked;
end;

procedure TFMacroEditor.chkRepeatChange(Sender: TObject);
begin
  cbRepeatUntilEof.Enabled :=  chkRepeat.Checked;
  edRepeat.Enabled := chkRepeat.Checked and not cbRepeatUntilEof.Checked;

end;

procedure TFMacroEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SynMacroRec.SaveMacros;
end;

procedure TFMacroEditor.FormShow(Sender: TObject);
var
  DefaultAttr: TFontAttributes;
begin
  ReloadMacros;
  SynEdit1.Highlighter:= ConfigObj.getHighLighter('.pas');
  SynEdit1.Font.Assign(ConfigObj.Font);
  DefaultAttr := ConfigObj.ReadFontAttributes('Schema/Default/Text/', FontAttributes());

  SynEdit1.Font.Color := DefaultAttr.Foreground;
  SynEdit1.Font.Style := DefaultAttr.Styles;

  SynEdit1.Color := DefaultAttr.Background;

end;

procedure TFMacroEditor.lbMacroViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
 if Assigned(Item) then
    SynEdit1.Lines.Text :=  TMacro(Item.Data).Commands;
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

