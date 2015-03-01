unit uDglGoTo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Spin,
  StdCtrls, ButtonPanel, ueditor;

type

  { TdlgGoTo }

  TdlgGoTo = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    Label2: TLabel;
    seLine: TSpinEdit;
    seColumn: TSpinEdit;
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure seLineChange(Sender: TObject);
  private
    { private declarations }
  public
    Editor : TEditor;
  end;

var
  dlgGoTo: TdlgGoTo;

implementation

{$R *.lfm}

{ TdlgGoTo }

procedure TdlgGoTo.FormShow(Sender: TObject);
begin
  seLine.MaxValue:=Editor.Lines.Count;
end;

procedure TdlgGoTo.OKButtonClick(Sender: TObject);
begin
  Editor.CaretXY := Point(seColumn.Value, seLine.Value-1);
end;

procedure TdlgGoTo.seLineChange(Sender: TObject);
begin
  seColumn.MaxValue:= Length(Editor.Lines[seLine.Value -1]);
end;

end.

