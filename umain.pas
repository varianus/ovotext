unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ActnList, Menus, ComCtrls, StdActns, uEditor, udmmain, LCLType, SynEditTypes;

type

  { TfMain }

  TfMain = class(TForm)
    AppProperties: TApplicationProperties;
    FileCloseAll: TAction;
    FileSave: TAction;
    FileExit: TAction;
    FindDialog: TFindDialog;
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
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
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
    SearchFind: TAction;
    SearchFindFirst: TAction;
    SearchFindNext1: TAction;
    SearchReplace: TAction;
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
    procedure AppPropertiesShowHint(var HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
    procedure EditRedoExecute(Sender: TObject);
    procedure EditRedoUpdate(Sender: TObject);
    procedure EditUndoUpdate(Sender: TObject);
    procedure FileCloseExecute(Sender: TObject);
    procedure FileExitExecute(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FileOpenAccept(Sender: TObject);
    procedure FindDialogClose(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure pcMainCloseTabClicked(Sender: TObject);
    procedure SearchFindAccept(Sender: TObject);
    procedure SearchFindExecute(Sender: TObject);
  private
    EditorFactory:TEditorFactory;
    function EditorAvalaible: boolean; inline;
    Procedure PrepareSearch(Dialog:TFindDialog; Out SynOption: TSynSearchOptions) ;
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
  public
    { public declarations }
  end; 

var
  fMain: TfMain;

implementation
uses lclproc,   Stringcostants;
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

procedure TfMain.AppPropertiesShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: THintInfo);
begin
  StatusBar1.Panels[0].Text:=HintInfo.HintStr;
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
  Editor := EditorFactory.CreateTabSheet(pcMain, @EditorStatusChange);

end;

procedure TfMain.FileOpenAccept(Sender: TObject);
begin
  with EditorFactory.CreateTabSheet(pcMain, @EditorStatusChange) do
    begin
      loadfromFile(FileOpen.Dialog.FileName);
    end;
end;

procedure TfMain.FindDialogClose(Sender: TObject);
begin
  self.BringToFront;
end;

procedure TfMain.FindDialogFind(Sender: TObject);
var
  Options: TSynSearchOptions;
begin
  PrepareSearch(FindDialog, Options);
  EditorFactory.CurrentEditor.SearchReplace(FindDialog.FindText,'',Options);

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

procedure TfMain.pcMainChange(Sender: TObject);
begin
  EditorStatusChange(nil,[scCaretX,scSelection,scInsertMode]);
  pcMain.Hint:= pcMain.ActivePage.Hint;
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
procedure TfMain.EditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if not EditorAvalaible then exit;

  if  (scCaretX in Changes) or (scCaretY in Changes) then
     StatusBar1.Panels[1].Text:= Format(RSStatusBarPos,[EditorFactory.CurrentEditor.CaretY, EditorFactory.CurrentEditor.CaretX]);

  if  (scSelection in Changes) then
     StatusBar1.Panels[2].Text:= Format(RSStatusBarSel,[EditorFactory.CurrentEditor.SelEnd - EditorFactory.CurrentEditor.SelStart]);

  if  (scInsertMode in Changes) then
     if EditorFactory.CurrentEditor.InsertMode  then
        StatusBar1.Panels[3].Text:= RSStatusBarInsMode
     else
        StatusBar1.Panels[3].Text:= RSStatusBarOvrMode;

procedure TfMain.SearchFindExecute(Sender: TObject);
begin
  if EditorAvalaible then
     FindDialog.Execute;
end;

function TfMain.EditorAvalaible: boolean;
begin
  Result := Assigned(EditorFactory.CurrentSubForm) and Assigned(EditorFactory.CurrentEditor);;
end;

procedure TfMain.PrepareSearch(Dialog: TFindDialog; out
  SynOption: TSynSearchOptions);
begin
  SynOption := [];
  if not (frDown in Dialog.Options) then Include(SynOption, ssoBackwards);
  if frWholeWord in Dialog.Options then Include(SynOption, ssoWholeWord);
  if frMatchCase in Dialog.Options then Include(SynOption, ssoMatchCase);
  if frEntireScope in Dialog.Options then Include(SynOption, ssoEntireScope);

end;




end.
