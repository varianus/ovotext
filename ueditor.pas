unit ueditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, dialogs, comctrls, SynEditTypes, SynEdit,
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
    Property Sheet: TEditorTabSheet read FSheet;
    // -- //
    procedure LoadFromfile(FileName: TFileName);
    function Save: Boolean;
    function SaveAs(FileName: TFileName): boolean;
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
     FonStatusChange: TStatusChangeEvent;
     fUntitledCounter :Integer;
     function GetCurrentEditor: TEditor;
   protected
     procedure DoChange; override;
   public
     property CurrentEditor: TEditor read GetCurrentEditor;
     property OnStatusChange: TStatusChangeEvent read FonStatusChange write FOnStatusChange;
     //--//
     procedure DoCloseTabClicked(APage: TCustomPage); override;
     function AddEditor(FileName:TFilename=''): TEditor;
     Function CloseEditor(Editor: TEditor):boolean;
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

procedure TEditor.LoadFromfile(FileName: TFileName);
begin
  FFileName:= FileName;
  Lines.LoadFromFile(FileName);
  Highlighter := dmMain.getHighLighter(ExtractFileExt(FileName));
  FSheet.Caption:= ExtractFileName(fFileName);
  FSheet.Hint:=FileName;
end;

function TEditor.Save: Boolean;
begin
  Result := SaveAs(FFileName);
end;

function TEditor.SaveAs(FileName: TFileName): boolean;
begin
  try
    Lines.SaveToFile(FileName);
    FFileName:=FileName;
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
begin
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
begin
  if not Editor.Modified then
     Result:= true
  else
  case MessageDlg(Format(RSSaveChanges, [Caption]), mtWarning, [mbYes, mbNo, mbCancel], 0) of
    mrYes: begin
           //if Editor.FUntitled then
           //   begin
           //      SaveDialog1.FileName:=Caption;
           //      if SaveDialog1.Execute then
           //         FFileName:= SaveDialog1.FileName
           //     else
           //         begin
           //           result:=false;
           //           exit;
           //         end;
              end;
    //      Lines.SaveToFile(FFileName);
    //      CanClose:=true;
    //end;
    //mrNo: Result := true;
    //mrCancel: Result := false;
  end;

  if Result then
     begin
       Sheet := Editor.FSheet;
       FreeAndNil(Editor);
       FreeAndNil(Sheet);
     end;
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
