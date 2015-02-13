unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, ActnList, Menus, ComCtrls, StdActns, uEditor, LCLType,
  SynEditTypes, SynHighlighterPas, mrumanager, Config;

type

  { TfMain }

  TfMain = class(TForm)
    actFont: TAction;
    actTrimLeading: TAction;
    AppProperties: TApplicationProperties;
    FileCloseAll: TAction;
    FileSave: TAction;
    FileExit: TAction;
    FindDialog: TFindDialog;
    FontDialog: TFontDialog;
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
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
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
    ReplaceDialog: TReplaceDialog;
    SaveDialog: TSaveDialog;
    SearchFind: TAction;
    SearchFindPrevious: TAction;
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
    procedure actFontExecute(Sender: TObject);
    procedure ActionListUpdate(AAction: TBasicAction; var Handled: boolean);
    procedure actTrimLeadingExecute(Sender: TObject);
    procedure AppPropertiesShowHint(var HintStr: string; var CanShow: boolean; var HintInfo: THintInfo);
    procedure EditRedoExecute(Sender: TObject);
    procedure FileCloseAllExecute(Sender: TObject);
    procedure FileCloseExecute(Sender: TObject);
    procedure FileExitExecute(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FileOpenAccept(Sender: TObject);
    procedure FindDialogClose(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure FontDialogApplyClicked(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure MenuItem28Click(Sender: TObject);
    procedure MenuItem29Click(Sender: TObject);
    procedure ReplaceDialogFind(Sender: TObject);
    procedure ReplaceDialogReplace(Sender: TObject);
    procedure SearchFindAccept(Sender: TObject);
    procedure SearchFindExecute(Sender: TObject);
    procedure SearchFindNext1Execute(Sender: TObject);
    procedure SearchFindPreviousExecute(Sender: TObject);
    procedure SearchReplaceExecute(Sender: TObject);
  private
    EditorFactory: TEditorFactory;
    MRU: TMRUMenuManager;

    FindText, ReplaceText: string;
    SynOption: TSynSearchOptions;

    function EditorAvalaible: boolean; inline;
    procedure BeforeCloseEditor(Editor: TEditor; var Cancel: boolean);
    procedure ExecFind(Dialog: TFindDialog);
    procedure PrepareReplace(Dialog: TReplaceDialog);

    procedure PrepareSearch(Dialog: TFindDialog);
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure RecentFileEvent(Sender: TObject; const AFileName: string);
    procedure NewEditor(Editor: TEditor);
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

procedure TfMain.ActionListUpdate(AAction: TBasicAction; var Handled: boolean);
var
  Avail: boolean;
begin
  Avail := EditorAvalaible;
  EditRedo.Enabled := Avail and EditorFactory.CurrentEditor.CanRedo;
  EditUndo.Enabled := Avail and EditorFactory.CurrentEditor.CanUndo;
  Handled := True;
end;

procedure TfMain.actTrimLeadingExecute(Sender: TObject);
var
  Ed: TEditor;
  i: integer;
begin

  Ed := EditorFactory.CurrentEditor;
  ed.BeginUpdate(True);
  try
    for i := 0 to Ed.Lines.Count - 1 do
      begin
        Ed.TextBetweenPoints[Point(1, i+1), Ed.PhysicalToLogicalPos(Point(Length(Ed.Lines[i])+1,i+1))] :=TrimLeft(Ed.Lines[i]);
      end;

  finally
    ed.EndUpdate;
  end;

end;

procedure TfMain.actFontExecute(Sender: TObject);
var
  i: integer;
begin
  FontDialog.Font.Assign(ConfigObj.Font);
  if FontDialog.Execute then
    begin
    for i := 0 to EditorFactory.PageCount - 1 do
      TEditorTabSheet(EditorFactory.Pages[i]).Editor.Font.Assign(FontDialog.Font);
    ConfigObj.Font.Assign(FontDialog.Font);
    end;

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
  i: integer;
begin

  for i := 0 to FileOpen.Dialog.Files.Count - 1 do
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
begin
  ExecFind(FindDialog);
end;

procedure TfMain.ExecFind(Dialog: TFindDialog);
var
  Options: TSynSearchOptions;
begin
  PrepareSearch(Dialog);

  if EditorFactory.CurrentEditor.SearchReplace(FindText, '', Options) = 0 then
    ShowMessage(Format(RSTextNotfound, [Dialog.FindText]));

end;

procedure TfMain.FontDialogApplyClicked(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to EditorFactory.PageCount - 1 do
    TEditorTabSheet(EditorFactory.Pages[i]).Editor.Font.Assign(FontDialog.Font);

  ConfigObj.Font.Assign(FontDialog.Font);
end;

procedure TfMain.FormActivate(Sender: TObject);
begin
  EditorFactory.CurrentEditor.SetFocus;
end;

procedure TfMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Assigned(EditorFactory) and (EditorFactory.PageCount > 0) then
    CanClose := EditorFactory.CloseAll
  else
    CanClose := True;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  MRU := TMRUMenuManager.Create(Self);
  MRU.MenuItem := mnuOpenRecent;
  MRU.OnRecentFile := @RecentFileEvent;
  MRU.MaxRecent := 10;
  MRU.Recent.Clear;
  ConfigObj.ReadStrings('Recent', 'File', MRU.Recent);
  MRU.ShowRecentFiles;
  EditorFactory := TEditorFactory.Create(Self);
  EditorFactory.Align := alClient;
  EditorFactory.OnStatusChange := @EditorStatusChange;
  EditorFactory.OnBeforeClose := @BeforeCloseEditor;
  EditorFactory.OnNewEditor := @NewEditor;
  EditorFactory.Images := imgList;
  EditorFactory.Parent := self;
  FileNew.Execute;
  // move close button to right
  tbbCloseAll.Align := alRight;
  tbbSepClose.Align := alRight;
  tbbClose.Align := alRight;

end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  ConfigObj.WriteStrings('Recent', 'File', MRU.Recent);
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
  ConfigObj.WriteStrings('Recent', 'File', MRU.Recent);
end;

procedure TfMain.MenuItem29Click(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to MRU.Recent.Count - 1 do
    EditorFactory.AddEditor(MRU.Recent[i]);

end;

procedure TfMain.ReplaceDialogFind(Sender: TObject);
begin
  ExecFind(ReplaceDialog);
end;

procedure TfMain.ReplaceDialogReplace(Sender: TObject);
var
  Options: TSynSearchOptions;
begin
  PrepareReplace(ReplaceDialog);

  if EditorFactory.CurrentEditor.SearchReplace(FindText, ReplaceText, Options) = 0 then
    ShowMessage(Format(RSTextNotfound, [FindText]))
  else
    if (ssoReplace in Options) and not (ssoReplaceAll in Options) then
      begin
      Exclude(Options, ssoReplace);
      EditorFactory.CurrentEditor.SearchReplace(FindText, '', Options);
      end;

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
    StatusBar1.Panels[1].Text :=
      Format(RSStatusBarSel, [Editor.SelEnd - Editor.SelStart]);

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

procedure TfMain.RecentFileEvent(Sender: TObject; const AFileName: string);
begin
  EditorFactory.AddEditor(AFileName);
end;

procedure TfMain.NewEditor(Editor: TEditor);
begin
  Editor.PopupMenu := pumEdit;
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

procedure TfMain.SearchFindNext1Execute(Sender: TObject);
var
  sOpt: TSynSearchOptions;
  Ed: TEditor;
begin
  if (FindText = EmptyStr) then
    Exit;
  Ed := EditorFactory.CurrentEditor;
  if Assigned(Ed) then
    begin
    sOpt := SynOption;
    sOpt := sOpt - [ssoBackWards];
    if Ed.SearchReplace(FindText, '', sOpt) = 0 then
      ShowMessage(Format(RSTextNotfound, [FindText]));
    ;
    end;

end;

procedure TfMain.SearchFindPreviousExecute(Sender: TObject);
var
  sOpt: TSynSearchOptions;
  Ed: TEditor;
begin
  if (FindText = EmptyStr) then
    Exit;
  Ed := EditorFactory.CurrentEditor;
  if Assigned(Ed) then
    begin
    sOpt := SynOption;
    sOpt := sOpt + [ssoBackWards];
    if Ed.SearchReplace(FindText, '', sOpt) = 0 then
      ShowMessage(Format(RSTextNotfound, [FindText]));
    ;
    end;

end;

procedure TfMain.SearchReplaceExecute(Sender: TObject);
var
  Editor: TEditor;
begin

  if not EditorAvalaible then
    exit;
  Editor := EditorFactory.CurrentEditor;
  if Editor.SelAvail and (Editor.BlockBegin.Y = Editor.BlockEnd.Y) then
    ReplaceDialog.FindText := Editor.SelText;
  ReplaceDialog.Execute;
end;


function TfMain.EditorAvalaible: boolean;
begin
  Result := Assigned(EditorFactory) and Assigned(EditorFactory.CurrentEditor);
end;

procedure TfMain.PrepareSearch(Dialog: TFindDialog);
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
  FindText := Dialog.FindText;

end;

procedure TfMain.PrepareReplace(Dialog: TReplaceDialog);
begin
  PrepareSearch(Dialog);

  if frReplace in Dialog.Options then
    Include(SynOption, ssoReplace);

  if frReplaceAll in Dialog.Options then
    Include(SynOption, ssoReplaceAll);

  if frFindNext in Dialog.Options then
    begin
    Exclude(SynOption, ssoReplace);
    Exclude(SynOption, ssoReplaceAll);
    end;
  ReplaceText := Dialog.FindText;
end;


procedure TfMain.BeforeCloseEditor(Editor: TEditor; var Cancel: boolean);
begin
  if not Editor.Modified then
    Cancel := False
  else
    case MessageDlg(Format(RSSaveChanges, [ExtractFileName(Editor.Sheet.Caption)]), mtWarning,
        [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        begin
        if Editor.Untitled then
          begin
          SaveDialog.FileName := Editor.Sheet.Caption;
          if SaveDialog.Execute then
            begin
            Editor.FileName := SaveDialog.FileName;
            end
          else
            begin
            Cancel := True;
            exit;
            end;
          end;
        Editor.Save;
        Cancel := False;
        end;
      mrNo: Cancel := False;
      mrCancel: Cancel := True;
      end;

end;

end.
