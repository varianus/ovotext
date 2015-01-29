unit ueditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, dialogs, comctrls, SynEditTypes, SynEdit,
  Stringcostants, forms;

type

  TEditor = class;
  TEditorTabSheet = class;
  TEditorFactory = class;

  { TEditorFactory }

  TOnBeforeClose = procedure(Editor: TEditor; var Cancel: boolean) of object;

  TEditor = class(TSynEdit)
  private
    FFileName: TFilename;
    FSheet: TEditorTabSheet;
    FUntitled:boolean;
    procedure SetFileName(AValue: TFileName);
    procedure SetUntitled(AValue: boolean);

  public
    Property Sheet: TEditorTabSheet read FSheet;
    Property FileName: TFileName read FFileName write SetFileName;
    property Untitled: boolean read FUntitled write SetUntitled;
    // -- //
    procedure LoadFromfile(AFileName: TFileName);
    function Save: Boolean;
    function SaveAs(AFileName: TFileName): boolean;
  end;

  { TEditorTabSheet }

  TEditorTabSheet = class (TTabSheet)
  private
    FEditor: TEditor;
  protected
    procedure DoShow; override;

  public
    Property Editor: TEditor read FEditor;
    //--//
  end;


  TEditorFactory = class (TPageControl)
   private
     FOnBeforeClose: TOnBeforeClose;
     FonStatusChange: TStatusChangeEvent;
     fUntitledCounter :Integer;
     function GetCurrentEditor: TEditor;
     procedure SetOnBeforeClose(AValue: TOnBeforeClose);
   protected
     procedure DoChange; override;
   public
     property CurrentEditor: TEditor read GetCurrentEditor;
     property OnStatusChange: TStatusChangeEvent read FonStatusChange write FOnStatusChange;
     property OnBeforeClose: TOnBeforeClose read FOnBeforeClose write SetOnBeforeClose;
     //--//
     procedure DoCloseTabClicked(APage: TCustomPage); override;
     function AddEditor(FileName:TFilename=''): TEditor;
     Function CloseEditor(Editor: TEditor):boolean;
     Function CloseAll:boolean;
     constructor Create(AOwner:TComponent); override;
     destructor Destroy;  override;
   end;

implementation
uses
  udmmain;

{ TEditorTabSheet }

procedure TEditorTabSheet.DoShow;
begin
  inherited DoShow;

end;

{ TEditor }

procedure TEditor.SetFileName(AValue: TFileName);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
  if FFileName <> EmptyStr then
     FUntitled:= false;
end;

procedure TEditor.SetUntitled(AValue: boolean);
begin
  if FUntitled=AValue then Exit;
  FUntitled:=AValue;
  if FUntitled then
     FFileName:=EmptyStr;
end;

procedure TEditor.LoadFromfile(AFileName: TFileName);
begin
  FFileName:= AFileName;
  Lines.LoadFromFile(FFileName);
  Highlighter := dmMain.getHighLighter(ExtractFileExt(fFileName));
  FSheet.Caption:= ExtractFileName(fFileName);
  FSheet.Hint:=FileName;
  FUntitled:=false;

end;

function TEditor.Save: Boolean;
begin
  Result := SaveAs(FFileName);
end;

function TEditor.SaveAs(AFileName: TFileName): boolean;
begin
  try
    FFileName:=AFileName;
    Lines.SaveToFile(AFileName);
    Result := true;
    FUntitled:= false;
  Except
    Result := false;
  end;
end;

{ TEditorFactory }

function TEditorFactory.GetCurrentEditor: TEditor;
begin
  Result := nil;
  if (PageCount > 0) and (ActivePageIndex >= 0) then
     result := TEditorTabSheet(ActivePage).Editor;

end;

procedure TEditorFactory.SetOnBeforeClose(AValue: TOnBeforeClose);
begin
  if FOnBeforeClose=AValue then Exit;
  FOnBeforeClose:=AValue;
end;

procedure TEditorFactory.DoChange;
begin
  inherited DoChange;
  Hint:= ActivePage.Hint;
  if Assigned(OnStatusChange) then
     OnStatusChange(GetCurrentEditor, [scCaretX,scCaretY,scModified,scInsertMode]);

  TEditorTabSheet(ActivePage).Editor.SetFocus;
end;

procedure TEditorFactory.DoCloseTabClicked(APage: TCustomPage);
begin
  inherited DoCloseTabClicked(APage);
  if Assigned(APage) and
     (APage is TEditorTabSheet) then
    CloseEditor(TEditorTabSheet(APage).FEditor);
end;

function TEditorFactory.AddEditor(FileName:TFilename=''): TEditor;
var
  Sheet: TEditorTabSheet;
  i: integer;
begin
  if FileName <> EmptyStr then
    begin
      // do not reopen same file
      for i:= 0 to PageCount -1 do
       begin
        Sheet := TEditorTabSheet(Pages[i]);
        if Sheet.Editor.FileName = FileName then
          begin
            ActivePageIndex:= i;
            exit;
          end;
      end;

    // try to reuse an empty shhet
     for i:= 0 to PageCount -1 do
       begin
         Sheet := TEditorTabSheet(Pages[i]);
         if (Sheet.Editor.Untitled) and not Sheet.Editor.Modified then
           begin
             Sheet.Editor.LoadFromfile(FileName);
             ActivePageIndex:= i;
             exit;
           end;
        end;

    end;

  Sheet:= TEditorTabSheet.Create(Self);
  Sheet.PageControl := Self;

  Result := TEditor.Create(Sheet);
  Result.FSheet := Sheet;
  Result.Align:= alClient;
  Sheet.FEditor := Result;
  Result.OnStatusChange:=OnStatusChange;
  if Assigned(OnStatusChange) then
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

function TEditorFactory.CloseEditor(Editor: TEditor): boolean;
var
  Sheet: TEditorTabSheet;
  Cancel: boolean;
begin
  Cancel:= True;
  if Assigned(FOnBeforeClose) then
     FOnBeforeClose(Editor, Cancel);

  if not Cancel then
     begin
       Sheet := Editor.FSheet;
       Application.ReleaseComponent(Editor);
       Application.ReleaseComponent(Sheet);
     end;
end;

function TEditorFactory.CloseAll: boolean;
var
  i: integer;
begin
  for i := 0 to PageCount -1 do
    if not CloseEditor(TEditorTabSheet(Pages[i]).Editor)   then
       break;

end;

constructor TEditorFactory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fUntitledCounter:=0;
  Options := Options + [nboShowCloseButtons];

end;

destructor TEditorFactory.Destroy;
begin
  inherited Destroy;
end;

end.

