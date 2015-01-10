unit ueditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources,
  comctrls, Forms, Controls, Graphics, Dialogs, contnrs,
  Menus, udmmain, SynEdit, uEditorFactory;

type


   TfEditor =class;

   TEditorTabSheet = class (TTabSheet)
   end;

   { TEditorFactory }

   TEditorFactory = class (TObject)
   private
    FCurrentEditor: TfEditor;
    fEditors:TObjectList;
    fUntitledCounter :Integer;
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

{ TfEditor }

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
   Sheet: TTabSheet;
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
         AOwner.ActivePage := Sheet;
         SetFocus;
        end;
      Sheet.Tag := PtrUInt(Result);
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



end.
