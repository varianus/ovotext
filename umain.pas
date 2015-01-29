unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ActnList, Menus, ComCtrls, StdActns, uEditor, LCLType, SynEditTypes,
  SynHighlighterPas, mrumanager, Config;

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
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    mnuOpenRecent: TMenuItem;
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
    pumEdit: TPopupMenu;
    SaveDialog: TSaveDialog;
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
    ToolButton15: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    tbbClose: TToolButton;
    tbbCloseAll: TToolButton;
    tbbSepClose: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure AppPropertiesShowHint(var HintStr: string; var CanShow: boolean; var HintInfo: THintInfo);
    procedure EditRedoExecute(Sender: TObject);
    procedure EditRedoUpdate(Sender: TObject);
    procedure EditUndoUpdate(Sender: TObject);
    procedure FileCloseAllExecute(Sender: TObject);
    procedure FileCloseExecute(Sender: TObject);
    procedure FileExitExecute(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FileOpenAccept(Sender: TObject);
    procedure FindDialogClose(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure MenuItem28Click(Sender: TObject);
    procedure MenuItem29Click(Sender: TObject);
    procedure SearchFindAccept(Sender: TObject);
    procedure SearchFindExecute(Sender: TObject);
  private
    EditorFactory: TEditorFactory;
    MRU: TMRUMenuManager;
    FConfig: TConfig;
    function EditorAvalaible: boolean; inline;
    procedure BeforeCloseEditor(Editor: TEditor; var Cancel: boolean);
    procedure PrepareSearch(Dialog: TFindDialog; Out SynOption: TSynSearchOptions);
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure RecentFileEvent (Sender : TObject; Const AFileName : String);
  public
    { public declarations }
  end;

var
  fMain: TfMain;

implementation

uses lclproc, Stringcostants, uabout;

{$R *.lfm}

{ TfMain }

procedure TfMain.FileExitExecute(Sender: TObject);
begin
  Application.terminate;
end;

procedure TfMain.FileCloseExecute(Sender: TObject);
begin
  if EditorAvalaible then
    EditorFactory.CloseEditor(EditorFactory.CurrentEditor);
end;

procedure TfMain.EditRedoExecute(Sender: TObject);
begin
  if EditorAvalaible then
    EditorFactory.CurrentEditor.Redo;
end;

procedure TfMain.AppPropertiesShowHint(var HintStr: string; var CanShow: boolean; var HintInfo: THintInfo);
begin
  StatusBar1.Panels[3].Text := HintInfo.HintStr;
end;

procedure TfMain.EditRedoUpdate(Sender: TObject);
begin
  EditRedo.Enabled := EditorAvalaible and EditorFactory.CurrentEditor.CanRedo;
end;

procedure TfMain.EditUndoUpdate(Sender: TObject);
begin
  EditUndo.Enabled := EditorAvalaible and EditorFactory.CurrentEditor.CanUndo;
end;

procedure TfMain.FileCloseAllExecute(Sender: TObject);
begin
  EditorFactory.CloseAll;
end;

procedure TfMain.FileNewExecute(Sender: TObject);
var
  Editor: TEditor;
begin
  Editor := EditorFactory.AddEditor();

end;

procedure TfMain.FileOpenAccept(Sender: TObject);
var
  i:Integer;
begin

  for i :=0 to FileOpen.Dialog.Files.Count -1 do
    begin
      EditorFactory.AddEditor(FileOpen.Dialog.Files[i]);
      MRU.AddToRecent(FileOpen.Dialog.Files[i]);
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
  EditorFactory.CurrentEditor.SearchReplace(FindDialog.FindText, '', Options);

end;

procedure TfMain.FormActivate(Sender: TObject);
begin
  EditorFactory.CurrentEditor.SetFocus;
end;

procedure TfMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Assigned(EditorFactory) and (EditorFactory.PageCount > 0) then
    CanClose:= EditorFactory.CloseAll
  Else
    CanClose:= true;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  FConfig := TConfig.Create;
  FConfig.ReadConfig;
  MRU := TMRUMenuManager.Create(Self);
  MRU.MenuItem := mnuOpenRecent;
  MRU.OnRecentFile:=@RecentFileEvent;
  MRU.MaxRecent:=10;
  FConfig.ReadStrings('Recent','File', MRU.Recent);
  MRU.ShowRecentFiles;
  EditorFactory := TEditorFactory.Create(Self);
  EditorFactory.Align := alClient;
  EditorFactory.OnStatusChange := @EditorStatusChange;
  EditorFactory.OnBeforeClose  := @BeforeCloseEditor;
  EditorFactory.Images := imgList;
  EditorFactory.Parent := self;
  FileNew.Execute;
  // move close button to right
  tbbCloseAll.Align:= alRight;
  tbbSepClose.Align:= alRight;
  tbbClose.Align:= alRight;

end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  FConfig.WriteStrings('Recent', 'File', MRU.Recent);
  FConfig.free;
  FreeAndNil(EditorFactory);
end;

procedure TfMain.HelpAboutExecute(Sender: TObject);
begin
  with TfAbout.Create(self) do
    Show;
end;

procedure TfMain.MenuItem28Click(Sender: TObject);
begin
  MRU.Recent.Clear;
  MRU.ShowRecentFiles;
end;

procedure TfMain.MenuItem29Click(Sender: TObject);
var i : integer;
begin
  for i := 0 to MRU.Recent.Count -1 do
    EditorFactory.AddEditor(MRU.Recent[i]);

end;

procedure TfMain.SearchFindAccept(Sender: TObject);
begin
  if not EditorAvalaible then
    exit;

end;

procedure TfMain.EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
var
  Editor: TEditor;

begin
  if not EditorAvalaible then
    exit;

  Editor := EditorFactory.CurrentEditor;

  if (scCaretX in Changes) or (scCaretY in Changes) then
    StatusBar1.Panels[0].Text := Format(RSStatusBarPos, [Editor.CaretY, Editor.CaretX]);

  if (scSelection in Changes) then
    StatusBar1.Panels[1].Text := Format(RSStatusBarSel, [Editor.SelEnd - Editor.SelStart]);

  if (scInsertMode in Changes) then
    if Editor.InsertMode then
      StatusBar1.Panels[2].Text := RSStatusBarInsMode
    else
      StatusBar1.Panels[2].Text := RSStatusBarOvrMode;

  if (scModified in Changes) then
    if Editor.Modified then
      Editor.Sheet.ImageIndex := 28
    else
      Editor.Sheet.ImageIndex := 19;

end;

procedure TfMain.RecentFileEvent(Sender: TObject; const AFileName: String);
begin
  EditorFactory.AddEditor(AFileName);
end;

procedure TfMain.SearchFindExecute(Sender: TObject);
var
  Editor: TEditor;
begin

  if not EditorAvalaible then
    exit;
  Editor := EditorFactory.CurrentEditor;
  if Editor.SelAvail and (Editor.BlockBegin.Y = Editor.BlockEnd.Y) then
    FindDialog.FindText := Editor.SelText;
  FindDialog.Execute;
end;

function TfMain.EditorAvalaible: boolean;
begin
  Result := Assigned(EditorFactory) and Assigned(EditorFactory.CurrentEditor);
end;

procedure TfMain.PrepareSearch(Dialog: TFindDialog; out SynOption: TSynSearchOptions);
begin
  SynOption := [];
  if not (frDown in Dialog.Options) then
    Include(SynOption, ssoBackwards);
  if frWholeWord in Dialog.Options then
    Include(SynOption, ssoWholeWord);
  if frMatchCase in Dialog.Options then
    Include(SynOption, ssoMatchCase);
  if frEntireScope in Dialog.Options then
    Include(SynOption, ssoEntireScope);

end;

procedure TfMain.BeforeCloseEditor(Editor:TEditor; var Cancel:boolean);
begin
  if not Editor.Modified then
     Cancel:= False
  else
  case MessageDlg(Format(RSSaveChanges, [ExtractFileName(Editor.Sheet.Caption)]), mtWarning, [mbYes, mbNo, mbCancel], 0) of
    mrYes: begin
           if Editor.Untitled then
              begin
                 SaveDialog.FileName:=Editor.Sheet.Caption;
                 if SaveDialog.Execute then
                    begin
                      Editor.FileName:= SaveDialog.FileName;
                    end
                else
                    begin
                      Cancel:=true;
                      exit;
                    end;
              end;
          Editor.Save;
          Cancel:=false;
       end;
    mrNo: Cancel := False;
    mrCancel: Cancel := true;
  end;

end;

end.
