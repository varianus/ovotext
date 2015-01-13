unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ActnList, Menus, ComCtrls, StdActns, uEditor, udmmain;

type

  { TfMain }

  TfMain = class(TForm)
    FileExit: TAction;
    HelpAbout: TAction;
    FileClose: TAction;
    FileNew: TAction;
    EditRedo: TAction;
    ActionList: TActionList;
    EditCopy: TEditCopy;
    EditCut: TEditCut;
    EditDelete: TEditDelete;
    EditPaste: TEditPaste;
    EditSelectAll: TEditSelectAll;
    EditUndo: TEditUndo;
    FileOpen: TFileOpen;
    FileSaveAs: TFileSaveAs;
    FindDialog1: TFindDialog;
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
    ReplaceDialog1: TReplaceDialog;
    SearchFind: TSearchFind;
    SearchFindFirst: TSearchFindFirst;
    SearchFindNext1: TSearchFindNext;
    SearchReplace: TSearchReplace;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure FileCloseExecute(Sender: TObject);
    procedure FileExitExecute(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FileOpenAccept(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pcMainCloseTabClicked(Sender: TObject);
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

procedure TfMain.FileExitExecute(Sender: TObject);
begin
  Application.terminate;
end;

procedure TfMain.FileCloseExecute(Sender: TObject);
begin
  if Assigned(EditorFactory.CurrentEditor) then
    EditorFactory.CurrentEditor.Close;
end;

procedure TfMain.FileNewExecute(Sender: TObject);
var
  Editor: TfEditor;
begin
  Editor := EditorFactory.CreateTabSheet(pcMain);

end;

procedure TfMain.FileOpenAccept(Sender: TObject);
begin
  with EditorFactory.CreateTabSheet(pcMain) do
    begin
      loadfromFile(FileOpen.Dialog.FileName);
    end;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
//
 EditorFactory:=TEditorFactory.Create;
 FileNew.Execute;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
 EditorFactory.Free;
end;

procedure TfMain.pcMainCloseTabClicked(Sender: TObject);
begin
 if Sender is TEditorTabSheet then
   TEditorTabSheet(Sender).Editor.Close;
end;

end.

