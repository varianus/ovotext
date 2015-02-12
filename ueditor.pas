unit ueditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Dialogs, ComCtrls,
  SynEditTypes, SynEdit, SynGutter, SynGutterMarks, SynGutterChanges, SynGutterLineNumber,
  Stringcostants, Forms, Graphics, Config, udmmain;

type

  TEditor = class;
  TEditorTabSheet = class;
  TEditorFactory = class;

  { TEditorFactory }

  TOnBeforeClose = procedure(Editor: TEditor; var Cancel: boolean) of object;
  TOnEditorEvent = procedure(Editor: TEditor) of object;

  TEditor = class(TSynEdit)
  private
    FFileName: TFilename;
    FSheet: TEditorTabSheet;
    FUntitled: boolean;
    procedure CreateDefaultGutterParts;
    procedure SetFileName(AValue: TFileName);
    procedure SetUntitled(AValue: boolean);

  public
    constructor Create(AOwner: TComponent); override;
    property Sheet: TEditorTabSheet read FSheet;
    property FileName: TFileName read FFileName write SetFileName;
    property Untitled: boolean read FUntitled write SetUntitled;
    // -- //
    procedure LoadFromfile(AFileName: TFileName);
    function Save: boolean;
    function SaveAs(AFileName: TFileName): boolean;
  end;

  { TEditorTabSheet }

  TEditorTabSheet = class(TTabSheet)
  private
    FEditor: TEditor;
  protected
    procedure DoShow; override;

  public
    property Editor: TEditor read FEditor;
    //--//
  end;


  TEditorFactory = class(TPageControl)
  private
    FOnBeforeClose: TOnBeforeClose;
    FOnNewEditor: TOnEditorEvent;
    FonStatusChange: TStatusChangeEvent;
    fUntitledCounter: integer;
    function GetCurrentEditor: TEditor;
    procedure SetOnBeforeClose(AValue: TOnBeforeClose);
    procedure SetOnNewEditor(AValue: TOnEditorEvent);
  protected
    procedure DoChange; override;
  public
    property CurrentEditor: TEditor read GetCurrentEditor;
    property OnStatusChange: TStatusChangeEvent read FonStatusChange write FOnStatusChange;
    property OnBeforeClose: TOnBeforeClose read FOnBeforeClose write SetOnBeforeClose;
    property OnNewEditor: TOnEditorEvent read FOnNewEditor write SetOnNewEditor;
    //--//
    procedure DoCloseTabClicked(APage: TCustomPage); override;
    function AddEditor(FileName: TFilename = ''): TEditor;
    function CloseEditor(Editor: TEditor): boolean;
    function CloseAll: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TEditorTabSheet }

procedure TEditorTabSheet.DoShow;
begin
  inherited DoShow;

end;

{ TEditor }

procedure TEditor.SetFileName(AValue: TFileName);
begin
  if FFileName = AValue then
    Exit;
  FFileName := AValue;
  if FFileName <> EmptyStr then
    FUntitled := False;
end;

procedure TEditor.SetUntitled(AValue: boolean);
begin
  if FUntitled = AValue then
    Exit;
  FUntitled := AValue;
  if FUntitled then
    FFileName := EmptyStr;
end;

constructor TEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateDefaultGutterParts;
end;

procedure TEditor.LoadFromfile(AFileName: TFileName);
begin
  FFileName := AFileName;
  Lines.LoadFromFile(FFileName);
  Highlighter := dmMain.getHighLighter(ExtractFileExt(fFileName));
  FSheet.Caption := ExtractFileName(fFileName);
  FSheet.Hint := FileName;
  FUntitled := False;

end;

function TEditor.Save: boolean;
begin
  Result := SaveAs(FFileName);
end;

function TEditor.SaveAs(AFileName: TFileName): boolean;
begin
    try
    FFileName := AFileName;
    Lines.SaveToFile(AFileName);
    Result := True;
    FUntitled := False;
    except
    Result := False;
    end;
end;

procedure TEditor.CreateDefaultGutterParts;
begin
  Gutter.Parts.Clear;
  with TSynGutterMarks.Create(Gutter.Parts) do
    Name := 'SynGutterMarks1';
  with TSynGutterLineNumber.Create(Gutter.Parts) do
    Name := 'SynGutterLineNumber1';
  with TSynGutterChanges.Create(Gutter.Parts) do
    Name := 'SynGutterChanges1';
end;

{ TEditorFactory }

function TEditorFactory.GetCurrentEditor: TEditor;
begin
  Result := nil;
  if (PageCount > 0) and (ActivePageIndex >= 0) then
    Result := TEditorTabSheet(ActivePage).Editor;

end;

procedure TEditorFactory.SetOnBeforeClose(AValue: TOnBeforeClose);
begin
  if FOnBeforeClose = AValue then
    Exit;
  FOnBeforeClose := AValue;
end;

procedure TEditorFactory.SetOnNewEditor(AValue: TOnEditorEvent);
begin
  if FOnNewEditor = AValue then
    Exit;
  FOnNewEditor := AValue;
end;

procedure TEditorFactory.DoChange;
begin
  inherited DoChange;
  Hint := ActivePage.Hint;
  if Assigned(OnStatusChange) then
    OnStatusChange(GetCurrentEditor, [scCaretX, scCaretY, scModified, scInsertMode]);

  TEditorTabSheet(ActivePage).Editor.SetFocus;
end;

procedure TEditorFactory.DoCloseTabClicked(APage: TCustomPage);
begin
  inherited DoCloseTabClicked(APage);
  if Assigned(APage) and (APage is TEditorTabSheet) then
    CloseEditor(TEditorTabSheet(APage).FEditor);
end;

function TEditorFactory.AddEditor(FileName: TFilename = ''): TEditor;
var
  Sheet: TEditorTabSheet;
  i: integer;
  DefaultAttr : TFontAttributes;
begin
  if FileName <> EmptyStr then
    begin
    // do not reopen same file
    for i := 0 to PageCount - 1 do
      begin
      Sheet := TEditorTabSheet(Pages[i]);
      if Sheet.Editor.FileName = FileName then
        begin
        ActivePageIndex := i;
        exit;
        end;
      end;

    // try to reuse an empty shhet
    for i := 0 to PageCount - 1 do
      begin
      Sheet := TEditorTabSheet(Pages[i]);
      if (Sheet.Editor.Untitled) and not Sheet.Editor.Modified then
        begin
        Sheet.Editor.LoadFromfile(FileName);
        ActivePageIndex := i;
        exit;
        end;
      end;

    end;

  Sheet := TEditorTabSheet.Create(Self);
  Sheet.PageControl := Self;

  Result := TEditor.Create(Sheet);
  Result.Font.Assign(ConfigObj.Font);
  DefaultAttr := ConfigObj.ReadFontAttributes('text', FontAttributes());

  Result.FSheet := Sheet;
  Result.Align := alClient;
  Sheet.FEditor := Result;

  Result.Font.Color := DefaultAttr.Foreground;
  Result.Font.Style := DefaultAttr.Styles;

  Result.Color := ConfigObj.BackGroundColor;
  Result.Options := Result.Options + [eoHideRightMargin];
  Result.BookMarkOptions.BookmarkImages := dmMain.imgBookMark;

  Result.OnStatusChange := OnStatusChange;
  if Assigned(OnStatusChange) then
    OnStatusChange(Result, [scCaretX, scCaretY, scModified, scInsertMode]);

  Result.Parent := Sheet;
  if FileName = '' then
    begin
    Sheet.Caption := Format(RSNewFile, [fUntitledCounter]);
    Result.FUntitled := True;
    Inc(fUntitledCounter);
    end
  else
    Result.LoadFromfile(FileName);

  ActivePage := Sheet;

  if Assigned(FOnNewEditor) then
    FOnNewEditor(Result);

end;

function TEditorFactory.CloseEditor(Editor: TEditor): boolean;
var
  Sheet: TEditorTabSheet;
  Cancel: boolean;
begin
  Cancel := True;
  // if last tab in unused
  if (PageCount = 1) and Editor.Untitled and not Editor.Modified and not ConfigObj.AppSettings.CloseWithLastTab then
    exit;

  if Assigned(FOnBeforeClose) then
    FOnBeforeClose(Editor, Cancel);

  if not Cancel then
    begin
    Sheet := Editor.FSheet;
    Editor.PopupMenu := nil;
    Application.ReleaseComponent(Editor);
    Application.ReleaseComponent(Sheet);
    Application.ProcessMessages;
    if (PageCount = 0) and not ConfigObj.AppSettings.CloseWithLastTab then
      AddEditor();
    end;


end;

function TEditorFactory.CloseAll: boolean;
var
  i: integer;
begin
  for i := PageCount - 1 downto 0 do
    if not CloseEditor(TEditorTabSheet(Pages[i]).Editor) then
      break;

end;

constructor TEditorFactory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fUntitledCounter := 0;
  Options := Options + [nboShowCloseButtons];

end;

destructor TEditorFactory.Destroy;
begin
  inherited Destroy;
end;

end.
