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
unit udlgsort;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls, Spin, ExtCtrls, MaskEdit, StrUtils,
  ueditor, Comparer;

type

  { TdlgSort }

  TdlgSort = class(TForm)
    ButtonPanel1: TButtonPanel;
    cgDelimiters: TCheckGroup;
    cbQuoteChar: TComboBox;
    cbNumeric: TCheckBox;
    edChar: TMaskEdit;
    GroupBox1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    pnlDelimited: TPanel;
    pnlColumns: TPanel;
    rgOrder: TRadioGroup;
    rbColumnMode: TRadioButton;
    rbDelimitedMode: TRadioButton;
    dlgSort: TSpinEdit;
    speEnd: TSpinEdit;
    speStart: TSpinEdit;
    speColumn: TSpinEdit;
    procedure OKButtonClick(Sender: TObject);
    procedure ChangeSortMode(Sender: TObject);
  private
    procedure SortByColumns;
    procedure SortByDelimiters;

  public
    Editor: TEditor;
  end;

var
  dlgSort: TdlgSort;

implementation

{$R *.lfm}

{ TdlgSort }

procedure TdlgSort.SortByColumns;
var
  CompareRec: TColumnComparer;
begin
  CompareRec := TColumnComparer.Create;
  try
    CompareRec.Ascending := rgOrder.ItemIndex = 0;
    CompareRec.AsNumeric := cbNumeric.Checked;
    CompareRec.ColStart := speStart.Value;
    CompareRec.ColEnd := speEnd.Value;
    Editor.BeginUpdate(True);
    try
      Editor.CustomSort(@(CompareRec.Compare));
    finally
      Editor.EndUpdate;
    end;

  finally
    CompareRec.Free;
  end;
end;

procedure TdlgSort.SortByDelimiters;
var
  CompareRec: TDelimitedComparer;
begin
  CompareRec := TDelimitedComparer.Create;
  try
    CompareRec.Ascending := rgOrder.ItemIndex = 0;
    CompareRec.AsNumeric := cbNumeric.Checked;
    CompareRec.Column := speColumn.Value -1;
    case cbQuoteChar.ItemIndex of
      0: CompareRec.QuoteChar := '"';
      1: CompareRec.QuoteChar := '''';
      else
        CompareRec.QuoteChar := #00;
    end;
    CompareRec.Delimiters := '';
    if cgDelimiters.Checked[0] then
        CompareRec.Delimiters := CompareRec.Delimiters +',';
    if cgDelimiters.Checked[1] then
        CompareRec.Delimiters := CompareRec.Delimiters +';';
    if cgDelimiters.Checked[2] then
        CompareRec.Delimiters := CompareRec.Delimiters +#09;
    if cgDelimiters.Checked[3] then
        CompareRec.Delimiters := CompareRec.Delimiters + ' ';
    if cgDelimiters.Checked[4] then
        CompareRec.Delimiters := CompareRec.Delimiters + edChar.Text;

    Editor.BeginUpdate(True);
    try
      Editor.CustomSort(@(CompareRec.Compare));
    finally
      Editor.EndUpdate;
    end;

  finally
    CompareRec.Free;
  end;

end;

procedure TdlgSort.OKButtonClick(Sender: TObject);
begin
  if rbColumnMode.Checked then
    SortByColumns;
  if rbDelimitedMode.Checked then
    SortByDelimiters;

end;

procedure TdlgSort.ChangeSortMode(Sender: TObject);
begin
  pnlColumns.Enabled := rbColumnMode.Checked;
  pnlDelimited.Enabled := rbDelimitedMode.Checked;
end;

end.
