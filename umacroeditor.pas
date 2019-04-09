unit uMacroEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Spin, ComCtrls, ueditor, SynMacroRecorder, SynEditKeyCmds;

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
    PanelRepeat: TPanel;
    pnlButtons: TPanel;
    SynMacroRec: TSynMacroRecorder;
    procedure btnPlayClick(Sender: TObject);
    procedure btnRecordClick(Sender: TObject);
    procedure btnRecordStopClick(Sender: TObject);
  private
    fFactory: TEditorFactory;
    mc: string;
  public

  end;

Procedure ShowMacroEditor(Factory: TEditorFactory);

implementation
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

initialization
  FMacroEditor := nil;

finalization
  if Assigned(FMacroEditor) then
    FMacroEditor.Free;

end.

