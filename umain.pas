unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ActnList, Menus, ComCtrls, StdActns, uEditor, uEditorFactory, Stringcostants;

type

  { TfMain }

  TfMain = class(TForm)
    actExit: TAction;
    actAbout: TAction;
    FileNew1: TAction;
    EditRedo1: TAction;
    ActionList: TActionList;
    EditCopy1: TEditCopy;
    EditCut1: TEditCut;
    EditDelete1: TEditDelete;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    FileOpen1: TFileOpen;
    FileSaveAs1: TFileSaveAs;
    imgList: TImageList;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mnuMain: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    pcMain: TPageControl;
    SearchFind1: TSearchFind;
    SearchFindFirst1: TSearchFindFirst;
    SearchFindNext1: TSearchFindNext;
    SearchReplace1: TSearchReplace;
    StatusBar1: TStatusBar;
    procedure actAboutExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure FileNew1Execute(Sender: TObject);
    procedure FileOpen1Accept(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    EditorFactory:TEditorFactory;
  public
    { public declarations }
  end; 

var
  fMain: TfMain;

implementation

{$R *.lfm}

{ TfMain }

procedure TfMain.actExitExecute(Sender: TObject);
begin
  Application.terminate;
end;

procedure TfMain.FileNew1Execute(Sender: TObject);
var
  Editor: TfEditor;
begin
  Editor := EditorFactory.CreateTabSheet(pcMain);
  Editor.Caption:= Format(RSNewFile, [1]);

end;

procedure TfMain.FileOpen1Accept(Sender: TObject);
begin
  with EditorFactory.CreateTabSheet(pcMain) do
   begin
      loadfromFile(FileOpen1.Dialog.FileName);
      Caption:=ExtractFileName(FileOpen1.Dialog.FileName);
   end;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
//
 EditorFactory:=TEditorFactory.Create;
 EditorFactory.MasterForm:=self;
 FileNew1.Execute;

end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
 EditorFactory.Free;
end;

procedure TfMain.actAboutExecute(Sender: TObject);
begin
///
end;

end.

