unit ueditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources,
  LMessages, lclintf,
  comctrls, Forms, Controls, Graphics, Dialogs, contnrs,
  Menus, udmmain, SynEdit, SynEditTypes;


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
    FCurrentEditor: TSynEdit;
    FCurrentSubForm: TfEditor;
    fEditors:TObjectList;
    fUntitledCounter :Integer;
    procedure EditorSheetHide(Sender: TObject);
    procedure EditorSheetShow(Sender: TObject);
    procedure SetCurrentEditor(AValue: TSynEdit);
    procedure SetCurrentSubForm(AValue: TfEditor);
   public
     constructor Create;
     destructor Destroy;  override;
     function CreateTabSheet(AOwner: TPageControl;Event:TStatusChangeEvent; FileName:TFileName=''): TfEditor;
     Property CurrentSubForm: TfEditor read FCurrentSubForm write SetCurrentSubForm;
     Property CurrentEditor: TSynEdit read FCurrentEditor write SetCurrentEditor;

   end;

   { TfEditor }
  TfEditor = class(TForm)
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    pumEdit: TPopupMenu;
    SaveDialog1: TSaveDialog;
    SynEdit1: TSynEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure SynEdit1Change(Sender: TObject);
  private
    FUntitled :Boolean;
    FFileName :string;
    Sheet: TEditorTabSheet;
    function GetModified: boolean;

  public
    procedure SetTextBuf(Buffer: PChar); override;
  public
    Factory:  TEditorFactory;
    procedure LoadFromfile(FileName:TFileName);
    Function Save:Boolean;
    Function SaveAs(FileName:TFileName):boolean;
    procedure SetUntitled;
    Property Modified : boolean read GetModified;
  end;

var
  fEditor: TfEditor;

implementation
uses Stringcostants;

{$R *.lfm}

{ TEditorTabSheet }

procedure TEditorTabSheet.WMDeleteThis(var Msg: TLMNoParams);
begin

  if Editor = Editor.Factory.CurrentSubForm then
    begin
     Editor.Factory.CurrentSubForm:= nil;
    end;


  free;
end;

{ TfEditor }

procedure TfEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caNone;
  Factory.fEditors.extract(Self);
  PostMessage(Parent.Handle, LM_DELETETHIS, 0, 0);
end;

procedure TfEditor.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if not Modified then
     begin
        CanClose:= true;
        exit;
     end;
  case MessageDlg(Format('Save changes to "%s"?',[Caption]), mtWarning, [mbYes, mbNo, mbCancel], 0) of
    mrYes: begin
           if FUntitled then
              begin
                 SaveDialog1.FileName:=Caption;
                 if SaveDialog1.Execute then
                    FFileName:= SaveDialog1.FileName
                 else
                    begin
                      CanClose:=false;
                      exit;
                    end;
              end;
          SynEdit1.Lines.SaveToFile(FFileName);
          CanClose:=true;
    end;
    mrNo: CanClose := true;
    mrCancel: CanClose := false;
  end;

end;

procedure TfEditor.SynEdit1Change(Sender: TObject);
begin
  if SynEdit1.Modified then
     sheet.ImageIndex:=28
  else
     sheet.ImageIndex:=19;
end;

function TfEditor.GetModified: boolean;
begin
  Result := SynEdit1.Modified;
end;

procedure TfEditor.SetTextBuf(Buffer: PChar);
begin
  if Assigned(Parent) then
    Parent.SetTextBuf(Buffer);
  inherited SetTextBuf(Buffer);
end;

procedure TfEditor.LoadFromfile(FileName: TFileName);
begin
  FFileName:= FileName;
  SynEdit1.Lines.LoadFromFile(FileName);
  SynEdit1.Highlighter := dmMain.getHighLighter(ExtractFileExt(FileName));
  Caption:= ExtractFileName(fFileName);
  Sheet.Hint:=FileName;
end;

function TfEditor.Save: Boolean;
begin
  try
    SynEdit1.Lines.SaveToFile(FFileName);
    Result := true;
    FUntitled:= false;
  Except
    Result := false;
  end;
end;

function TfEditor.SaveAs(FileName: TFileName): boolean;
begin
  try
    SynEdit1.Lines.SaveToFile(FileName);
    FFileName:=FileName;
    Result := true;
    FUntitled:= false;
  Except
    Result := false;
  end;

end;

procedure TfEditor.SetUntitled;
begin
  fUntitled:= true;
end;


procedure TEditorFactory.SetCurrentSubForm(AValue: TfEditor);
begin
  if FCurrentSubForm=AValue then Exit;
  FCurrentSubForm:=AValue;
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

function TEditorFactory.CreateTabSheet(AOwner: TPageControl; Event:TStatusChangeEvent; FileName: TFileName): TfEditor;
var
   ASheet: TEditorTabSheet;
begin
   ASheet := TEditorTabSheet.Create(AOwner);
   Result:= TfEditor.Create(Asheet);
   try
      ASheet.PageControl := AOwner;
      ASheet.OnHide:=@EditorSheetHide;
      ASheet.OnShow:=@EditorSheetShow;
      ASheet.ImageIndex := 19;
      with Result do
        begin
         Sheet := ASheet;
         Factory := Self;
         BorderStyle := bsNone;
         Parent := Sheet;
         Align := alClient;
         Visible := TRUE;
         SetFocus;
         SynEdit1.OnStatusChange:=Event;
        end;
      ASheet.Editor := Result;
      AOwner.ActivePage := ASheet;
      ASheet.Show;
      Result.Realign;
      fEditors.Add(Result);
      if FileName = '' then
         begin
            result.Caption:= Format(RSNewFile, [fUntitledCounter]);
            Result.SetUntitled;
            inc(fUntitledCounter);
         end
      else
         Result.LoadFromfile(FileName);

   except
      ASheet.Free;
   end;
end;

procedure TEditorFactory.EditorSheetShow(Sender: TObject);
var
  Sheet: TEditorTabSheet absolute Sender;
begin
  FCurrentSubform := Sheet.Editor;
  FCurrentEditor := Sheet.Editor.SynEdit1;
end;

procedure TEditorFactory.SetCurrentEditor(AValue: TSynEdit);
begin
  if FCurrentEditor=AValue then Exit;
  FCurrentEditor:=AValue;
end;


procedure TEditorFactory.EditorSheetHide(Sender: TObject);
var
  Sheet: TEditorTabSheet absolute Sender;
begin

  if FCurrentSubForm = Sheet.Editor then
    begin
     FCurrentSubForm := nil;
     FCurrentEditor:= nil;
    end;

end;

end.

