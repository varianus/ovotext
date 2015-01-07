unit ueditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  SynEdit;

type

  { TfEditor }

  TfEditor = class(TForm)
    SynEdit1: TSynEdit;
  private
    { private declarations }
  public
    procedure SetTextBuf(Buffer: PChar); override;

  public
    MasterForm:  TForm;
  end; 

var
  fEditor: TfEditor;

implementation

{$R *.lfm}

{ TfEditor }

procedure TfEditor.SetTextBuf(Buffer: PChar);
begin
  if Assigned(Parent) then
    Parent.SetTextBuf(Buffer)
 else
    inherited SetTextBuf(Buffer);
end;

end.

