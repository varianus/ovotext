unit ueditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  udmmain, SynEdit;

type

  { TfEditor }

  TfEditor = class(TForm)
    SynEdit1: TSynEdit;
  private
    function GetModified: boolean;
    { private declarations }
  public
    procedure SetTextBuf(Buffer: PChar); override;
  public
    MasterForm:  TForm;
    procedure LoadFromfile(FileName:TFileName);
    Property Modified : boolean read GetModified;
  end; 

var
  fEditor: TfEditor;

implementation

{$R *.lfm}

{ TfEditor }

function TfEditor.GetModified: boolean;
begin
  Result := SynEdit1.Modified;
end;

procedure TfEditor.SetTextBuf(Buffer: PChar);
begin
  if Assigned(Parent) then
    Parent.SetTextBuf(Buffer)
 else
    inherited SetTextBuf(Buffer);
end;

procedure TfEditor.LoadFromfile(FileName: TFileName);
begin
  SynEdit1.Lines.LoadFromFile(FileName);
  SynEdit1.Highlighter := dmMain.getHighLighter(ExtractFileExt(FileName));
end;

end.

