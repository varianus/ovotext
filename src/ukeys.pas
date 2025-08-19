unit uKeys;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, Grids, SynEditKeyCmds;

type

  { TfKeys }

  TfKeys = class(TForm)
    ButtonPanel1: TButtonPanel;
    grdCommands: TStringGrid;
  private

  public
    procedure Initialize(Keys: TSynEditKeyStrokes);
  end;

var
  fKeys: TfKeys;

implementation

{$R *.lfm}

{ TfKeys }
uses LCLProc;

procedure TfKeys.Initialize(Keys: TSynEditKeyStrokes);
var
  Key: TSynEditKeyStroke;
  i: integer;
begin
  grdCommands.Clear;
  IF Not assigned(Keys) then
    exit;

  grdCommands.RowCount := Keys.Count + 1;

  for i := 0 to Keys.Count - 1 do
  begin
    key := Keys[i];
    grdCommands.Cells[0, i + 1] := EditorCommandToCodeString(Key.Command);
    grdCommands.Cells[1, i + 1] := ShortCutToText(Key.ShortCut);
  end;
end;

end.
