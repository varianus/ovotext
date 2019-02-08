{ Ovotext - simple text editor

  Copyright (C) 2015 Marco Caselli <marcocas@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ActnList, Menus, ComCtrls, StdActns, uEditor, LCLType, Clipbrd, StdCtrls,
  SynEditTypes, SynHighlighterPas, PrintersDlgs, Config, SupportFuncs, LazUtils,
  udmmain, uDglGoTo, SynEditPrint, simplemrumanager, SynEditLines;

type

  { TfMain }

  TSaveMode = (smText, smRTF, smHTML);

  TfMain = class(TForm)
    actFont: TAction;
    actFullNameToClipBoard: TAction;
    actGoTo: TAction;
    actCloseAllExceptThis: TAction;
    actCloseBefore: TAction;
    actCloseAfter: TAction;
    FileReload: TAction;
    actPathToClipboard: TAction;
    actSQLPrettyPrint: TAction;
    actXMLCompact: TAction;
    actJSONPrettyPrint: TAction;
    actQuote: TAction;
    actLanguageNone: TAction;
    actXMLPrettyPrint: TAction;
    actTabToSpace: TAction;
    ExportHtmlToClipBoard: TAction;
    ExportHtmlToFile: TAction;
    actUpperCase: TAction;
    actLowerCase: TAction;
    ExportRTFToClipBoard: TAction;
    ExportRTFToFile: TAction;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem58: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem60: TMenuItem;
    MenuItem61: TMenuItem;
    MenuItem62: TMenuItem;
    MenuItem63: TMenuItem;
    MenuItem64: TMenuItem;
    MenuItem77: TMenuItem;
    MenuItem78: TMenuItem;
    MenuItem79: TMenuItem;
    MenuItem80: TMenuItem;
    MenuItem81: TMenuItem;
    MenuItem82: TMenuItem;
    MenuItem83: TMenuItem;
    N2: TMenuItem;
    N1: TMenuItem;
    mnuLineEndings: TMenuItem;
    mnuCRLF: TMenuItem;
    MenuItem65: TMenuItem;
    MenuItem66: TMenuItem;
    MenuItem67: TMenuItem;
    MenuItem68: TMenuItem;
    MenuItem69: TMenuItem;
    MenuItem70: TMenuItem;
    MenuItem71: TMenuItem;
    MenuItem72: TMenuItem;
    MenuItem73: TMenuItem;
    MenuItem74: TMenuItem;
    MenuItem75: TMenuItem;
    MenuItem76: TMenuItem;
    mnuLF: TMenuItem;
    mnuCR: TMenuItem;
    mnuThemes: TMenuItem;
    mnuNone: TMenuItem;
    mnuLanguage: TMenuItem;
    mnuTabs: TMenuItem;
    pumTabs: TPopupMenu;
    PrintDialog1: TPrintDialog;
    SortAscending: TAction;
    actPrint: TAction;
    SortDescending: TAction;
    FileSaveAll: TAction;
    actTrimTrailing: TAction;
    actTrim: TAction;
    ActCompressSpaces: TAction;
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
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
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
    lbMessage: TStaticText;
    StatusBar: TStatusBar;
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
    procedure actCloseAfterExecute(Sender: TObject);
    procedure actCloseAllExceptThisExecute(Sender: TObject);
    procedure actCloseBeforeExecute(Sender: TObject);
    procedure ActCompressSpacesExecute(Sender: TObject);
    procedure actFontExecute(Sender: TObject);
    procedure actFullNameToClipBoardExecute(Sender: TObject);
    procedure actGoToExecute(Sender: TObject);
    procedure ActionListUpdate(AAction: TBasicAction; var Handled: boolean);
    procedure actJSONPrettyPrintExecute(Sender: TObject);
    procedure actLanguageNoneExecute(Sender: TObject);
    procedure actPathToClipboardExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actQuoteExecute(Sender: TObject);
    procedure actSQLPrettyPrintExecute(Sender: TObject);
    procedure actTabToSpaceExecute(Sender: TObject);
    procedure actTrimExecute(Sender: TObject);
    procedure actTrimLeadingExecute(Sender: TObject);
    procedure actTrimTrailingExecute(Sender: TObject);
    procedure actXMLCompactExecute(Sender: TObject);
    procedure actXMLPrettyPrintExecute(Sender: TObject);
    procedure AppPropertiesActivate(Sender: TObject);
    procedure AppPropertiesDropFiles(Sender: TObject; const FileNames: array of String);
    procedure AppPropertiesShowHint(var HintStr: string; var CanShow: boolean; var HintInfo: THintInfo);
    procedure EditRedoExecute(Sender: TObject);
    procedure ExportHtmlToClipBoardExecute(Sender: TObject);
    procedure ExportHtmlToFileExecute(Sender: TObject);
    procedure ExportRTFToClipBoardExecute(Sender: TObject);
    procedure ExportRTFToFileExecute(Sender: TObject);
    procedure FileCloseAllExecute(Sender: TObject);
    procedure FileCloseExecute(Sender: TObject);
    procedure FileExitExecute(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FileOpenAccept(Sender: TObject);
    procedure FileOpenBeforeExecute(Sender: TObject);
    procedure FileReloadExecute(Sender: TObject);
    procedure FileSaveAsAccept(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure FindDialogClose(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure FontDialogApplyClicked(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure actLowerCaseExecute(Sender: TObject);
    procedure MenuItem28Click(Sender: TObject);
    procedure MenuItem29Click(Sender: TObject);
    procedure mnuLineEndingsClick(Sender: TObject);
    procedure mnuCRClick(Sender: TObject);
    procedure mnuCRLFClick(Sender: TObject);
    procedure mnuLFClick(Sender: TObject);
    procedure mnuTabsClick(Sender: TObject);
    procedure ReplaceDialogFind(Sender: TObject);
    procedure ReplaceDialogReplace(Sender: TObject);
    procedure SearchFindAccept(Sender: TObject);
    procedure SearchFindExecute(Sender: TObject);
    procedure SearchFindNext1Execute(Sender: TObject);
    procedure SearchFindPreviousExecute(Sender: TObject);
    procedure SearchReplaceExecute(Sender: TObject);
    procedure SortAscendingExecute(Sender: TObject);
    procedure SortDescendingExecute(Sender: TObject);
    procedure actUpperCaseExecute(Sender: TObject);
  private
    EditorFactory: TEditorFactory;
    MRU: TMRUMenuManager;

    FindText, ReplaceText: string;
    SynOption: TSynSearchOptions;
    prn: TSynEditPrint;

    function AskFileName(Editor: TEditor): boolean;
    procedure ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    function EditorAvalaible: boolean; inline;
    procedure BeforeCloseEditor(Editor: TEditor; var Cancel: boolean);
    procedure ExecFind(Dialog: TFindDialog);
    procedure mnuLangClick(Sender: TObject);
    procedure mnuThemeClick(Sender: TObject);
    procedure PrepareReplace(Dialog: TReplaceDialog);

    procedure PrepareSearch(Dialog: TFindDialog);
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure RecentFileEvent(Sender: TObject; const AFileName: string);
    procedure NewEditor(Editor: TEditor);
    procedure ShowTabs(Sender: TObject);
    Procedure SetupSaveDialog(SaveMode: TSaveMode);
    procedure SaveConfig;
    procedure ReadConfig;
  public
    { public declarations }
  end;



var
  fMain: TfMain;

implementation

uses lclproc, Stringcostants, uabout, SynExportHTML;

{$R *.lfm}

const
  IDX_IMG_MODIFIED = 28;
  IDX_IMG_STANDARD = 19;

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

procedure TfMain.ExportHtmlToClipBoardExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  Ed := EditorFactory.CurrentEditor;

  dmMain.HTMLExporter.Highlighter := Ed.Highlighter;
  dmMain.HTMLExporter.ExportAsText := True;
  dmMain.HTMLExporter.Options := [heoFragmentOnly];
  dmMain.HTMLExporter.ExportRange(ed.Lines, ed.BlockBegin, ed.BlockEnd);
  dmMain.HTMLExporter.CopyToClipboard;

end;

procedure TfMain.ExportHtmlToFileExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  Ed := EditorFactory.CurrentEditor;
  SetupSaveDialog(smHTML);
  if SaveDialog.Execute then
    begin
      dmMain.HTMLExporter.Highlighter := Ed.Highlighter;
      dmMain.HTMLExporter.ExportAsText := True;
      dmMain.HTMLExporter.Options := [heoDoctype, heoCharset];
      dmMain.HTMLExporter.ExportAll(ed.Lines);
      dmMain.HTMLExporter.SaveToFile(SaveDialog.FileName);
    end;
end;

procedure TfMain.ExportRTFToClipBoardExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  Ed := EditorFactory.CurrentEditor;
  dmMain.RFTExporter.UseBackground:=true;
  dmMain.RFTExporter.Highlighter := Ed.Highlighter;
  dmMain.RFTExporter.ExportAsText:=false;
  dmMain.RFTExporter.ExportRange(Ed.Lines, Ed.BlockBegin, Ed.BlockEnd);
  dmMain.RFTExporter.CopyToClipboard;

end;

procedure TfMain.ExportRTFToFileExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  Ed := EditorFactory.CurrentEditor;
  SetupSaveDialog(smRTF);
  if SaveDialog.Execute then
    begin
      dmMain.RFTExporter.Highlighter := Ed.Highlighter;
      dmMain.RFTExporter.ExportAll(ed.Lines);
      dmMain.RFTExporter.SaveToFile(SaveDialog.FileName);
    end;
end;

procedure TfMain.AppPropertiesShowHint(var HintStr: string; var CanShow: boolean; var HintInfo: THintInfo);
begin
  StatusBar.Panels[5].Text := HintInfo.HintStr;
end;

procedure TfMain.ContextPopup(Sender: TObject; MousePos: TPoint;var Handled: Boolean);
begin
  if not EditorAvalaible then
    exit;

  EditorFactory.ActivePageIndex := EditorFactory.IndexOfTabAt(MousePos);

  if EditorFactory.ActivePageIndex > -1 then
    begin
      Handled:=true;
      MousePos:=EditorFactory.ClientToScreen(MousePos);
      pumTabs.PopupComponent:=EditorFactory.CurrentEditor;
      pumTabs.PopUp(MousePos.X,MousePos.Y);
    end;

end;

procedure TfMain.ActionListUpdate(AAction: TBasicAction; var Handled: boolean);
var
  Avail: boolean;
  ed: TEditor;
begin
  Avail := EditorAvalaible;
  Ed := EditorFactory.CurrentEditor;
  EditRedo.Enabled := Avail and Ed.CanRedo;
  EditUndo.Enabled := Avail and Ed.CanUndo;
  FileSave.Enabled := Avail and Ed.Modified;
  FileReload.Enabled := Avail and not Ed.Untitled;
  EditCopy.Enabled := Avail and ed.SelAvail;
  EditCut.Enabled := Avail and ed.SelAvail;
  ExportHtmlToClipBoard.Enabled := Avail and ed.SelAvail;
  ExportRTFToClipBoard.Enabled := Avail and ed.SelAvail;
  actFullNameToClipBoard.Enabled := Avail and not ed.Untitled;
  ExportHtmlToFile.Enabled:= Avail;
  ExportRTFToFile.Enabled:= Avail;
  actGoTo.Enabled := Avail and (ed.Lines.Count > 0);
  actCloseAfter.Enabled := EditorFactory.PageCount > EditorFactory.PageIndex;
  actCloseBefore.Enabled := EditorFactory.PageIndex > 0;

  Handled := True;
end;

procedure TfMain.actJSONPrettyPrintExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  Ed.TextOperation(@FormatJSON, [tomFullText]);
end;

procedure TfMain.actLanguageNoneExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  Ed.Highlighter := nil;
end;

procedure TfMain.actPathToClipboardExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  Ed := EditorFactory.CurrentEditor;
  Clipboard.AsText := ExtractFilePath(Ed.FileName);
end;

procedure TfMain.actPrintExecute(Sender: TObject);
var
  Ed: TEditor;
begin

  Ed := EditorFactory.CurrentEditor;
  prn.SynEdit := Ed;
  if PrintDialog1.Execute then
    prn.Print;

end;

procedure TfMain.actQuoteExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  Ed.TextOperation(@QuotedStr, [tomLines]);
end;

procedure TfMain.actSQLPrettyPrintExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  Ed.TextOperation(@FormatSQL, [tomFullText]);
end;

procedure TfMain.actTabToSpaceExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  Ed.TextOperation(@TabsToSpace);
end;

procedure TfMain.actTrimExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  Ed.TextOperation(@Trim);

end;

procedure TfMain.actTrimLeadingExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  Ed.TextOperation(@TrimLeft);

end;

procedure TfMain.actTrimTrailingExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  Ed.TextOperation(@TrimRight);

end;

procedure TfMain.actXMLCompactExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  Ed.TextOperation(@CompactXML, [tomFullText]);
end;

procedure TfMain.actXMLPrettyPrintExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  Ed.TextOperation(@FormatXML, [tomFullText]);
end;

procedure TfMain.AppPropertiesActivate(Sender: TObject);
begin
  EditorFactory.DoCheckFileChanges;
end;

procedure TfMain.AppPropertiesDropFiles(Sender: TObject; const FileNames: array of String);
var
  i :Integer;
begin
  for i:= Low(FileNames) to High(FileNames) do
    EditorFactory.AddEditor(FileNames[i])
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
    ConfigObj.Dirty := true;
  end;

end;

procedure TfMain.actFullNameToClipBoardExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  Ed := EditorFactory.CurrentEditor;
  Clipboard.AsText := Ed.FileName;
end;

procedure TfMain.actGoToExecute(Sender: TObject);
begin
  with TdlgGoTo.Create(Self) do
  begin
    Editor := EditorFactory.CurrentEditor;
    ShowModal;
    Free;
  end;

end;

procedure TfMain.ActCompressSpacesExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  Ed.TextOperation(@RemoveSpacesInExcess);

end;

procedure TfMain.actCloseAllExceptThisExecute(Sender: TObject);
begin
  EditorFactory.CloseAll(true);
end;

procedure TfMain.actCloseAfterExecute(Sender: TObject);
begin
  EditorFactory.CloseAfter
end;

procedure TfMain.actCloseBeforeExecute(Sender: TObject);
begin
  EditorFactory.CloseBefore;
end;

procedure TfMain.FileCloseAllExecute(Sender: TObject);
begin
  EditorFactory.CloseAll;
end;

procedure TfMain.FileNewExecute(Sender: TObject);
begin
  EditorFactory.AddEditor();

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

procedure TfMain.FileOpenBeforeExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  Ed := EditorFactory.CurrentEditor;
  if EditorAvalaible and (not Ed.Untitled) then
    FileOpen.Dialog.InitialDir := ExtractFilePath(Ed.FileName);

end;

procedure TfMain.FileReloadExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  Ed := EditorFactory.CurrentEditor;
  if Ed.Untitled then
    exit;

  if Ed.Modified then
    if MessageDlg(RSReload, Format(RSReloadFile, [ed.FileName]), mtConfirmation, [mbYes, mbNo], 0) = mrno then
      exit;

  ed.PushPos;
  ed.LoadFromFile(ed.FileName);
  ed.Modified := False;
  ed.PopPos;

end;

procedure TfMain.FileSaveAsAccept(Sender: TObject);
var
  Editor: TEditor;
begin
  Editor := EditorFactory.CurrentEditor;

  // if AskFileName(Editor) then

  Editor.SaveAs(FileSaveAs.Dialog.FileName);
  MRU.AddToRecent(FileSaveAs.Dialog.FileName);

end;

procedure TfMain.FileSaveExecute(Sender: TObject);
var
  Editor: TEditor;
begin
  Editor := EditorFactory.CurrentEditor;
  if Editor.Untitled then
    if not AskFileName(Editor) then
    begin
      Exit;
    end;
  Editor.Save;

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
begin
  PrepareSearch(Dialog);

  if EditorFactory.CurrentEditor.SearchReplace(FindText, '', SynOption) = 0 then
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
  if Assigned(EditorFactory) and assigned(EditorFactory.CurrentEditor) then
    EditorFactory.CurrentEditor.SetFocus;
end;

procedure TfMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveConfig;
end;

procedure TfMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Assigned(EditorFactory) and (EditorFactory.PageCount > 0) then
    CanClose := EditorFactory.CloseAll
  else
    CanClose := True;
end;

procedure TfMain.FormCreate(Sender: TObject);
var
  i: integer;
  mnuLang: TMenuItem;
  mnuTheme: TmenuItem;
  CurrLetter: string;
  SaveLetter: string;
  CurrMenu: TMenuItem;

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
  EditorFactory.OnContextPopup := @ContextPopup;
  EditorFactory.Images := imgList;
  EditorFactory.Parent := self;
  FileNew.Execute;
  // move close button to right
  tbbCloseAll.Align := alRight;
  tbbSepClose.Align := alRight;
  tbbClose.Align := alRight;
  // Parameters
  FileOpen.Dialog.Filter := configobj.GetFiters;

  for i := 1 to Paramcount do
  begin
    // dirty hack to skip parameter as --debug=....
    if copy(ParamStr(i), 1, 2) <> '--' then
      EditorFactory.AddEditor(ParamStr(i));
  end;

  prn := TSynEditPrint.Create(Self);
  prn.Colors := True;

  {$IFDEF UNIX}
  if isRoot then
  begin
    lbMessage.Caption := RSAdministrativeRights;
    lbMessage.Visible := True;
  end;
  {$ENDIF}

  SaveLetter := '';
  for i := 0 to HIGHLIGHTERCOUNT - 1 do
  begin
    mnuLang := TMenuItem.Create(Self);
    mnuLang.Caption := ARHighlighter[i].HLClass.GetLanguageName;
    mnuLang.Tag := i;
    mnuLang.OnClick := @mnuLangClick;
    CurrLetter := UpperCase(Copy(mnuLang.Caption, 1, 1));
    if SaveLetter <> CurrLetter then
    begin
      SaveLetter := CurrLetter;
      CurrMenu := TMenuItem.Create(Self);
      CurrMenu.Caption := CurrLetter;
      mnuLanguage.Add(CurrMenu);
    end;

    CurrMenu.Add(mnuLang);
  end;

  for i := 0 to ConfigObj.ThemeList.Count - 1 do
  begin
    mnuTheme := TMenuItem.Create(Self);
    mnuTheme.Caption := ConfigObj.ThemeList.Keys[i];
    mnuTheme.RadioItem := true;
    if ConfigObj.ThemeList.Data[i] = ConfigObj.XMLConfigExtended.Filename then
      mnuTheme.Checked := true;
    mnuTheme.Tag := i;
    mnuTheme.OnClick:=@mnuThemeClick;
    mnuThemes.Add(mnuTheme);
  end;

end;

procedure TfMain.mnuLangClick(Sender: TObject);
var
  idx: integer;
  Ed: TEditor;

begin
  idx := TMenuItem(Sender).Tag;

  if not EditorAvalaible then
    exit;

  try
    // Reloading highlighter for some files, for example big and complex XML files,
    // could be slow, better to give feedback to user
    Screen.Cursor := crHourGlass;
    Ed := EditorFactory.CurrentEditor;
    Ed.Highlighter := ConfigObj.getHighLighter(Idx);

  finally
    Screen.Cursor :=  crDefault;
  end;

end;

procedure TfMain.mnuThemeClick(Sender: TObject);
var
  idx: integer;
begin
  idx := TMenuItem(Sender).Tag;
  TMenuItem(Sender).Checked := true;
  try
    Screen.Cursor := crHourGlass;
    ConfigObj.SetTheme(idx);
    EditorFactory.ReloadHighLighters;
  finally
    Screen.Cursor :=  crDefault;
  end;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  ConfigObj.WriteStrings('Recent', 'File', MRU.Recent);
  Mru.Free;
  FreeAndNil(EditorFactory);
end;

procedure TfMain.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  i: integer;
begin
  for i := Low(FileNames) to High(FileNames) do
  begin
    EditorFactory.AddEditor(FileNames[i]);
    MRU.AddToRecent(FileNames[i]);
  end;

end;

procedure TfMain.FormResize(Sender: TObject);
begin
  ConfigObj.Dirty := true;
end;

procedure TfMain.FormShow(Sender: TObject);
begin
  ReadConfig;
end;

procedure TfMain.FormWindowStateChange(Sender: TObject);
begin
  ConfigObj.Dirty := true;
end;

procedure TfMain.HelpAboutExecute(Sender: TObject);
begin
  with TfAbout.Create(self) do
    Show;
end;

procedure TfMain.actUpperCaseExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  Ed.TextOperation(@UpperCase);

end;

procedure TfMain.actLowerCaseExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  Ed.TextOperation(@LowerCase);

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

procedure TfMain.mnuLineEndingsClick(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  case Ed.LineEndingType of
    sfleCrLf: mnuCRLF.Checked := true;
    sfleLf: mnuLF.Checked := true;
    sfleCr: mnuCR.Checked := true;
  end;
  mnuCR.Enabled := not mnuCR.Checked;
  mnuLF.Enabled := not mnuLF.Checked;
  mnuCRLF.Enabled := not mnuCRLF.Checked;

end;

procedure TfMain.mnuCRClick(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  ed.LineEndingType := sfleCr;

end;

procedure TfMain.mnuCRLFClick(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  ed.LineEndingType := sfleCrLf;
end;

procedure TfMain.mnuLFClick(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  ed.LineEndingType := sfleLf;
end;

procedure TfMain.ShowTabs(Sender: TObject);
begin
  EditorFactory.ActivePageIndex := (Sender as TMenuItem).Tag;
end;

procedure TfMain.SetupSaveDialog(SaveMode: TSaveMode);
begin
  case SaveMode of
    smText:begin
             SaveDialog.DefaultExt:='.txt';
             SaveDialog.Filter:= ConfigObj.GetFiters;
           end;
    smRTF: begin
             SaveDialog.DefaultExt:='.rtf';
             SaveDialog.Filter:= 'RTF Files (*.rtf)|*.rtf';
             SaveDialog.Title:='Export as RTF File';
           end;
    smHTML:begin
             SaveDialog.DefaultExt:='.html';
             SaveDialog.Filter:= 'HTML Files (*.htm*)|*.htm*';
             SaveDialog.Title:='Export as HTML File';
           end;
  end;
end;

procedure TfMain.SaveConfig;
begin
  with ConfigObj.ConfigHolder do begin
    SetValue('MainForm/NormalLeft', Left);
    SetValue('MainForm/NormalTop', Top);
    SetValue('MainForm/NormalWidth', Width);
    SetValue('MainForm/NormalHeight', Height);

    SetValue('MainForm/RestoredLeft', RestoredLeft);
    SetValue('MainForm/RestoredTop', RestoredTop);
    SetValue('MainForm/RestoredWidth', RestoredWidth);
    SetValue('MainForm/RestoredHeight', RestoredHeight);

    SetValue('MainForm/WindowState', Integer(WindowState));
  end;

end;

procedure TfMain.ReadConfig;
var
  LastWindowState: TWindowState;
begin
  with ConfigObj.ConfigHolder do
  begin
    LastWindowState := TWindowState(GetValue('MainForm/WindowState', Integer(WindowState)));

    if LastWindowState = wsMaximized then
    begin
      WindowState := wsNormal;
      BoundsRect := Bounds(
        GetValue('MainForm/RestoredLeft', RestoredLeft),
        GetValue('MainForm/RestoredTop', RestoredTop),
        GetValue('MainForm/RestoredWidth', RestoredWidth),
        GetValue('MainForm/RestoredHeight', RestoredHeight));
      WindowState := wsMaximized;
    end
    else
    begin
      WindowState := wsNormal;
      BoundsRect := Bounds(
        GetValue('MainForm/NormalLeft', Left),
        GetValue('MainForm/NormalTop', Top),
        GetValue('MainForm/NormalWidth', Width),
        GetValue('MainForm/NormalHeight', Height));
    end;
  end;

end;

procedure TfMain.mnuTabsClick(Sender: TObject);
var
  i: integer;
  mnuitem: TMenuItem;
begin
  mnuTabs.Clear;

  for i := 0 to EditorFactory.PageCount - 1 do
  begin
    mnuitem := TMenuItem.Create(mnuTabs);
    mnuitem.Caption := EditorFactory.Pages[i].Caption;
    mnuitem.Tag := i;
    mnuitem.OnClick := @ShowTabs;
    mnuTabs.Add(mnuitem);
  end;

end;

procedure TfMain.ReplaceDialogFind(Sender: TObject);
begin
  ExecFind(ReplaceDialog);
end;

procedure TfMain.ReplaceDialogReplace(Sender: TObject);

begin
  PrepareReplace(ReplaceDialog);

  if EditorFactory.CurrentEditor.SearchReplace(FindText, ReplaceText, SynOption) = 0 then
    ShowMessage(Format(RSTextNotfound, [FindText]))
  else
  if (ssoReplace in SynOption) and not (ssoReplaceAll in SynOption) then
  begin
    Exclude(SynOption, ssoReplace);
    EditorFactory.CurrentEditor.SearchReplace(FindText, '', SynOption);
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
    StatusBar.Panels[0].Text := Format(RSStatusBarPos, [Editor.CaretY, Editor.CaretX]);

  if (scSelection in Changes) then
    StatusBar.Panels[1].Text :=
      Format(RSStatusBarSel, [Editor.SelEnd - Editor.SelStart]);

  if (scInsertMode in Changes) then
    if Editor.InsertMode then
      StatusBar.Panels[2].Text := RSStatusBarInsMode
    else
      StatusBar.Panels[2].Text := RSStatusBarOvrMode;

  if (scModified in Changes) then
    if Editor.Modified then
      Editor.Sheet.ImageIndex := IDX_IMG_MODIFIED
    else
      Editor.Sheet.ImageIndex := IDX_IMG_STANDARD;

  case Editor.LineEndingType of
    sfleCrLf: StatusBar.Panels[3].Text := mnuCRLF.Caption;
    sfleLf:   StatusBar.Panels[3].Text := mnuLF.Caption;
    sfleCr:   StatusBar.Panels[3].Text := mnuCR.Caption;
  else
    StatusBar.Panels[3].Text:= '';
  end;

  StatusBar.Panels[4].Text := Editor.DiskEncoding;

end;

procedure TfMain.RecentFileEvent(Sender: TObject; const AFileName: string);
begin
  EditorFactory.AddEditor(AFileName);
  MRU.AddToRecent(AFileName);
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


procedure TfMain.SortAscendingExecute(Sender: TObject);
var
  Ed: TEditor;
begin

  Ed := EditorFactory.CurrentEditor;
  ed.BeginUpdate(True);
  try
    Ed.Sort(True);

  finally
    Ed.EndUpdate;
  end;

end;

procedure TfMain.SortDescendingExecute(Sender: TObject);
var
  Ed: TEditor;
begin

  Ed := EditorFactory.CurrentEditor;
  ed.BeginUpdate(True);
  try
    Ed.Sort(False);
  finally
    Ed.EndUpdate;
  end;

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
  ReplaceText := Dialog.ReplaceText;
end;

function TfMain.AskFileName(Editor: TEditor): boolean;
begin
  SaveDialog.FileName := Editor.Sheet.Caption;
  if SaveDialog.Execute then
  begin
    Editor.FileName := SaveDialog.FileName;
    Result := True;
  end
  else
  begin
    Result := False;
  end;

end;

procedure TfMain.BeforeCloseEditor(Editor: TEditor; var Cancel: boolean);
begin
  if not Editor.Modified then
    Cancel := False
  else
    case MessageDlg(Format(RSSaveChanges, [ExtractFileName(trim(Editor.Sheet.Caption))]), mtWarning,
        [mbYes, mbNo, mbCancel], 0) of
      mrYes:
      begin
        if Editor.Untitled then
          if not AskFileName(Editor) then
          begin
            Cancel := True;
            Exit;
          end;
        Cancel := not Editor.Save;
      end;
      mrNo: Cancel := False;
      mrCancel: Cancel := True;
    end;

end;

end.
