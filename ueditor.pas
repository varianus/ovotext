unit ueditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, comctrls, SynEditTypes, SynEdit,
  Stringcostants;

type

  TEditor = class;
  TEditorTabSheet = class;
  TEditorFactory = class;

  { TEditorFactory }

  TEditor = class(TSynEdit)
  private
    FFileName: TFilename;
    FSheet: TEditorTabSheet;
    FUntitled:boolean;

  public
    procedure LoadFromfile(FileName: TFileName);
  end;

  { TEditorTabSheet }

  TEditorTabSheet = class (TTabSheet)
  private
    FEditor: TEditor;
  public
    Property Editor: TEditor read FEditor;

  end;


  TEditorFactory = class (TPageControl)
   private
     FonStatusChange: TStatusChangeEvent;
     fUntitledCounter :Integer;
     function GetCurrentEditor: TEditor;
   protected
     procedure DoChange; override;
   public
     property CurrentEditor: TEditor read GetCurrentEditor;
     property OnStatusChange: TStatusChangeEvent read FonStatusChange write FOnStatusChange;
     //--//
     function AddEditor(FileName:TFilename=''): TEditor;
     constructor Create(AOwner:TComponent); override;
     destructor Destroy;  override;
   end;

implementation
uses
  udmmain;

procedure TEditor.LoadFromfile(FileName: TFileName);
begin
  FFileName:= FileName;
  Lines.LoadFromFile(FileName);
  Highlighter := dmMain.getHighLighter(ExtractFileExt(FileName));
  FSheet.Caption:= ExtractFileName(fFileName);
  FSheet.Hint:=FileName;
end;

{ TEditorFactory }

function TEditorFactory.GetCurrentEditor: TEditor;
begin
  Result := nil;
  if (PageCount > 0) and (ActivePageIndex >= 0) then
     result := TEditorTabSheet(ActivePage).Editor;

end;

procedure TEditorFactory.DoChange;
begin
  inherited DoChange;
  Hint:= ActivePage.Hint;

end;

function TEditorFactory.AddEditor(FileName:TFilename=''): TEditor;
var
  Sheet: TEditorTabSheet;
begin
  Sheet:= TEditorTabSheet.Create(Self);
  Sheet.PageControl := Self;

  Result := TEditor.Create(Sheet);
  Result.FSheet := Sheet;
  Result.Align:= alClient;
  Sheet.FEditor := Result;
  Result.OnStatusChange:=OnStatusChange;
  OnStatusChange(Result, [scCaretX,scCaretY,scModified,scInsertMode]);
  Result.Parent := Sheet;
  if FileName = '' then
     begin
       Sheet.Caption:= Format(RSNewFile, [fUntitledCounter]);
       Result.FUntitled:= true;
       inc(fUntitledCounter);
     end
  else
     Result.LoadFromfile(FileName);

   ActivePage := Sheet;
end;

constructor TEditorFactory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fUntitledCounter:=0;
end;

destructor TEditorFactory.Destroy;
begin
  inherited Destroy;
end;

end.

