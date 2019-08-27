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
unit uDglGoTo;

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
  seLine.MaxValue:= Editor.Lines.Count;
end;

procedure TdlgGoTo.OKButtonClick(Sender: TObject);
begin
  Editor.CaretXY := Point(seColumn.Value, seLine.Value);
end;

procedure TdlgGoTo.seLineChange(Sender: TObject);
begin
  seColumn.MaxValue:= Length(Editor.Lines[seLine.Value]);
end;

end.

