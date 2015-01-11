unit ueditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources,
  LMessages, lclintf,
  comctrls, Forms, Controls, Graphics, Dialogs, contnrs,
  Menus, udmmain, SynEdit;


const
    LM_DELETETHIS  =  LM_USER + 100;

type

   TfEditor =class;

   { TEditorTabSheet }

   TEditorTabSheet = class (TTabSheet)
   public
     Editor : TFEditor;
     procedure WMDeleteThis(var Msg: TLMNoParams);  message lM_DELETETHIS;

   end;

   { TEditorFactory }

   TEditorFactory = class (TObject)
   private
    FCurrentEditor: TfEditor;
    fEditors:TObjectList;
    fUntitledCounter :Integer;
    procedure EditorSheetHide(Sender: TObject);
    procedure EditorSheetShow(Sender: TObject);
    procedure SetCurrentEditor(AValue: TfEditor);
   public
     constructor Create;
     destructor Destroy;  override;
     function CreateTabSheet(AOwner: TPageControl; FileName:TFileName=''): TfEditor;
     Property CurrentEditor: TfEditor read FCurrentEditor write SetCurrentEditor;

   end;

   { TfEditor }
  TfEditor = class(TForm)
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    pumEdit: TPopupMenu;
    SynEdit1: TSynEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    fUntitled :Boolean;
  public
    procedure SetTextBuf(Buffer: PChar); override;
  public
    Factory:  TEditorFactory;
    procedure LoadFromfile(FileName:TFileName);
    procedure SetUntitled;
  end;

var
  fEditor: TfEditor;

implementation
uses Stringcostants;

{$R *.lfm}

{ TEditorTabSheet }

procedure TEditorTabSheet.WMDeleteThis(var Msg: TLMNoParams);
begin
  free;
end;

{ TfEditor }

procedure TfEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caNone;

  PostMessage(Parent.Handle, lM_DELETETHIS, 0, 0);
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

procedure TfEditor.SetUntitled;
begin
  fUntitled:= true;
end;


procedure TEditorFactory.SetCurrentEditor(AValue: TfEditor);
begin
  if FCurrentEditor=AValue then Exit;
  FCurrentEditor:=AValue;
end;

constructor TEditorFactory.Create;
begin
   fEditors := TObjectList.Create;
   fEditors.OwnsObjects:=true;
   fUntitledCounter:=1;
end;

destructor TEditorFactory.Destroy;
begin
   fEditors.Free;
   inherited Destroy;
end;

function TEditorFactory.CreateTabSheet(AOwner: TPageControl; FileName: TFileName): TfEditor;

var
   Sheet: TEditorTabSheet;
begin
   Sheet := TEditorTabSheet.Create(AOwner);
   Result:= TfEditor.Create(sheet);
   try
      Sheet.PageControl := AOwner;
      Sheet.ImageIndex := Sheet.TabIndex;
      Result := TfEditor.Create(Sheet);
      with Result do
        begin
         Factory := Self;
         BorderStyle := bsNone;
         Parent := Sheet;
         Align := alClient;
         Visible := TRUE;
         SetFocus;
        end;
      Sheet.Editor := Result;
      Sheet.OnHide:=@EditorSheetHide;
      Sheet.OnShow:=@EditorSheetShow;
      AOwner.ActivePage := Sheet;
      Result.Realign;
      if Result <> NIL then
         fEditors.Add(Result);
   except
      Sheet.Free;
   end;
  if FileName = '' then
     begin
        result.Caption:= Format(RSNewFile, [fUntitledCounter]);
        Result.SetUntitled;
        inc(fUntitledCounter);
     end
  else
     Result.LoadFromfile(FileName);
end;

procedure TEditorFactory.EditorSheetShow(Sender: TObject);
var
  Sheet: TEditorTabSheet absolute Sender;
begin
  FCurrentEditor := Sheet.Editor;
end;


procedure TEditorFactory.EditorSheetHide(Sender: TObject);
var
  Sheet: TEditorTabSheet absolute Sender;
begin
  if FCurrentEditor = Sheet.Editor then
     FCurrentEditor := nil;
end;

end.
