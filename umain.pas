unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ActnList, Menus, ComCtrls, StdActns, uEditor, udmmain;

type

  { TfMain }

  TfMain = class(TForm)
    FileSave: TAction;
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
    SearchFind: TSearchFind;
    SearchFindFirst: TSearchFindFirst;
    SearchFindNext1: TSearchFindNext;
    SearchReplace: TSearchReplace;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure EditRedoExecute(Sender: TObject);
    procedure EditRedoUpdate(Sender: TObject);
    procedure EditUndoUpdate(Sender: TObject);
    procedure FileCloseExecute(Sender: TObject);
    procedure FileExitExecute(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FileOpenAccept(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pcMainCloseTabClicked(Sender: TObject);
    procedure SearchFindAccept(Sender: TObject);
  private
    EditorFactory:TEditorFactory;
    function EditorAvalaible: boolean; inline;
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
  if EditorAvalaible then
    EditorFactory.CurrentSubForm.Close;
end;

procedure TfMain.EditRedoExecute(Sender: TObject);
begin
 if EditorAvalaible then
    EditorFactory.CurrentEditor.Redo;
end;

procedure TfMain.EditRedoUpdate(Sender: TObject);
begin
  EditRedo.Enabled:= EditorAvalaible and EditorFactory.CurrentEditor.CanRedo;
end;

procedure TfMain.EditUndoUpdate(Sender: TObject);
begin
  EditUndo.Enabled:= EditorAvalaible and EditorFactory.CurrentEditor.CanUndo;
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

procedure TfMain.SearchFindAccept(Sender: TObject);
begin
  if not EditorAvalaible then
    exit;

end;

function TfMain.EditorAvalaible: boolean;
begin
  Result := Assigned(EditorFactory.CurrentSubForm);
end;

end.

