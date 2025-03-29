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
{$I codegen.inc}
unit umain;

interface

uses
  Classes, SysUtils, Math, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ActnList, Menus, ComCtrls, StdActns, uEditor, LCLType, Clipbrd, StdCtrls, ExtCtrls,
  SynEditTypes, PrintersDlgs, Config, SupportFuncs, LazUtils, LazUTF8, SingleInstance,
  udmmain, uDglGoTo, SynEditPrint, simplemrumanager, SynMacroRecorder, uMacroRecorder, uMacroEditor,
  SynEditLines, SynEdit, SynEditKeyCmds, replacedialog, lclintf, jsontools, umacroplayback, iconloader, LMessages;

type

  { TfMain }

  TSaveMode = (smText, smRTF, smHTML);

  TFileTreeNode = class(TTreeNode)
  public
    FullPath: string;
    isDir: boolean;
  end;

  TfMain = class(TForm)
    actFont: TAction;
    actFullNameToClipBoard: TAction;
    actGoTo: TAction;
    actCloseAllExceptThis: TAction;
    actCloseBefore: TAction;
    actCloseAfter: TAction;
    actFindLongestLine: TAction;
    actFullScreen: TAction;
    actFileNameToClipboard: TAction;
    actUnQuote: TAction;
    FileBrowseFolder: TAction;
    FileCloseFolder: TAction;
    FileReloadFolder: TAction;
    FileOpenFolder: TAction;
    actMacroSave: TAction;
    actShowRowNumber: TAction;
    actShowToolbar: TAction;
    actJSONCompact: TAction;
    actZoomIn: TAction;
    actZoomOut: TAction;
    actZoomReset: TAction;
    actToggleSpecialChar: TAction;
    actMacroManager: TAction;
    actMacroPlayBack: TAction;
    actMacroStop: TAction;
    actMacroPlaybackMulti: TAction;
    actMacroRecord: TAction;
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
    FilesTree: TTreeView;
    MenuItem100: TMenuItem;
    MenuItem101: TMenuItem;
    MenuItem102: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem46: TMenuItem;
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
    MenuItem84: TMenuItem;
    MenuItem85: TMenuItem;
    MenuItem86: TMenuItem;
    MenuItem87: TMenuItem;
    MenuItem88: TMenuItem;
    MenuItem89: TMenuItem;
    MenuItem90: TMenuItem;
    MenuItem95: TMenuItem;
    MenuItem96: TMenuItem;
    MenuItem97: TMenuItem;
    MenuItem98: TMenuItem;
    MenuItem99: TMenuItem;
    N5: TMenuItem;
    MenuItem94: TMenuItem;
    N4: TMenuItem;
    MenuItem91: TMenuItem;
    MenuItem92: TMenuItem;
    MenuItem93: TMenuItem;
    N3: TMenuItem;
    mnuSavedMacros: TMenuItem;
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
    pnlLeft: TPanel;
    pumTabs: TPopupMenu;
    PrintDialog1: TPrintDialog;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
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
    mnuCleanRecent: TMenuItem;
    mnuReopenAllRecent: TMenuItem;
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
    SaveDialog: TSaveDialog;
    SearchFind: TAction;
    SearchFindPrevious: TAction;
    SearchFindNext: TAction;
    SearchReplace: TAction;
    lbMessage: TStaticText;
    splLeftBar: TSplitter;
    StatusBar: TStatusBar;
    MainToolbar: TToolBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton3: TToolButton;
    tbbClose: TToolButton;
    tbbCloseAll: TToolButton;
    tbbSepClose: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure actCloseAfterExecute(Sender: TObject);
    procedure actCloseAllExceptThisExecute(Sender: TObject);
    procedure actCloseBeforeExecute(Sender: TObject);
    procedure ActCompressSpacesExecute(Sender: TObject);
    procedure actFileNameToClipboardExecute(Sender: TObject);
    procedure actFindLongestLineExecute(Sender: TObject);
    procedure actFontExecute(Sender: TObject);
    procedure actFullNameToClipBoardExecute(Sender: TObject);
    procedure actFullScreenExecute(Sender: TObject);
    procedure actGoToExecute(Sender: TObject);
    procedure actJSONCompactExecute(Sender: TObject);
    procedure actMacroManagerExecute(Sender: TObject);
    procedure ActionListUpdate(AAction: TBasicAction; var Handled: boolean);
    procedure actJSONPrettyPrintExecute(Sender: TObject);
    procedure actLanguageNoneExecute(Sender: TObject);
    procedure actMacroPlayBackExecute(Sender: TObject);
    procedure actMacroPlaybackMultiExecute(Sender: TObject);
    procedure actMacroRecordExecute(Sender: TObject);
    procedure actMacroSaveExecute(Sender: TObject);
    procedure actMacroStopExecute(Sender: TObject);
    procedure actPathToClipboardExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actQuoteExecute(Sender: TObject);
    procedure actUnQuoteExecute(Sender: TObject);
    procedure FileBrowseFolderExecute(Sender: TObject);
    procedure FileCloseFolderExecute(Sender: TObject);
    procedure FileReloadFolderExecute(Sender: TObject);
    procedure actShowRowNumberExecute(Sender: TObject);
    procedure actShowToolbarExecute(Sender: TObject);
    procedure actSQLPrettyPrintExecute(Sender: TObject);
    procedure actTabToSpaceExecute(Sender: TObject);
    procedure actToggleSpecialCharExecute(Sender: TObject);
    procedure actTrimExecute(Sender: TObject);
    procedure actTrimLeadingExecute(Sender: TObject);
    procedure actTrimTrailingExecute(Sender: TObject);
    procedure actXMLCompactExecute(Sender: TObject);
    procedure actXMLPrettyPrintExecute(Sender: TObject);
    procedure actZoomInExecute(Sender: TObject);
    procedure actZoomOutExecute(Sender: TObject);
    procedure actZoomResetExecute(Sender: TObject);
    procedure AppPropertiesActivate(Sender: TObject);
    procedure AppPropertiesDropFiles(Sender: TObject; const FileNames: array of string);
    procedure AppPropertiesIdle(Sender: TObject; var Done: boolean);
    procedure AppPropertiesShowHint(var HintStr: string; var CanShow: boolean; var HintInfo: THintInfo);
    procedure EditCopyExecute(Sender: TObject);
    procedure EditCutExecute(Sender: TObject);
    procedure EditPasteExecute(Sender: TObject);
    procedure EditRedoExecute(Sender: TObject);
    procedure EditSelectAllExecute(Sender: TObject);
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
    procedure FileOpenFolderExecute(Sender: TObject);
    procedure FileReloadExecute(Sender: TObject);
    procedure FileSaveAsAccept(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure FilesTreeCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
    procedure FilesTreeDblClick(Sender: TObject);
    procedure FilesTreeExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: boolean);
    procedure FilesTreeGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure FilesTreeGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure FindDialogClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FontDialogApplyClicked(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure actLowerCaseExecute(Sender: TObject);
    procedure mnuCleanRecentClick(Sender: TObject);
    procedure mnuReopenAllRecentClick(Sender: TObject);
    procedure mnuLineEndingsClick(Sender: TObject);
    procedure mnuCRClick(Sender: TObject);
    procedure mnuCRLFClick(Sender: TObject);
    procedure mnuLFClick(Sender: TObject);
    procedure mnuTabsClick(Sender: TObject);
    procedure ReplaceDialogFind(Sender: TObject);
    procedure ReplaceDialogReplace(Sender: TObject);
    procedure SearchFindAccept(Sender: TObject);
    procedure SearchFindExecute(Sender: TObject);
    procedure SearchFindNextExecute(Sender: TObject);
    procedure SearchFindPreviousExecute(Sender: TObject);
    procedure SearchReplaceExecute(Sender: TObject);
    procedure SortAscendingExecute(Sender: TObject);
    procedure SortDescendingExecute(Sender: TObject);
    procedure actUpperCaseExecute(Sender: TObject);
    procedure StatusBarResize(Sender: TObject);
  private
    EditorFactory: TEditorFactory;
    SynMacroRec: TMacroRecorder;
    MRU: TMRUMenuManager;
    Macros: TMRUMenuManager;

    FindText, ReplaceText: string;
    SynOption: TMySynSearchOptions;
    prn: TSynEditPrint;
    ReplaceDialog: TCustomReplaceDialog;
    rect: TRect;
    ws: TWindowState;
    BrowsingPath: string;

    function AskFileName(Editor: TEditor): boolean;
    procedure ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: boolean);

    function EditorAvalaible: boolean; inline;
    procedure BeforeCloseEditor(Editor: TEditor; var Cancel: boolean);
    procedure ExpandNode(NodeDir: TFileTreeNode; const Path: string);
    procedure LoadDir(Path: string);
    procedure LoadImageList;
    procedure mnuLangClick(Sender: TObject);
    procedure mnuThemeClick(Sender: TObject);
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure RecentFileEvent(Sender: TObject; const AFileName: string; const AData: TObject);
    procedure NewEditor(Editor: TEditor);
    procedure RecentMacroEvent(Sender: TObject; const AFileName: string; const AData: TObject);
    procedure ServerReceivedParams(Sender: TBaseSingleInstance; aParams: TStringList);
    procedure ShowTabs(Sender: TObject);
    procedure SetupSaveDialog(SaveMode: TSaveMode);
    procedure SynMacroRecListChange(Sender: TObject);
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
  IDX_IMG_MODIFIED = 3;
  IDX_IMG_STANDARD = -1;

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

procedure TfMain.EditSelectAllExecute(Sender: TObject);
begin
  SendMessage(GetFocus, LM_CHAR, VK_CONTROL + VK_A, 0);
end;

procedure TfMain.ExportHtmlToClipBoardExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  Ed := EditorFactory.CurrentEditor;

  dmMain.HTMLExporter.Highlighter  := Ed.Highlighter;
  dmMain.HTMLExporter.ExportAsText := True;
  dmMain.HTMLExporter.Options      := [heoFragmentOnly];
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
    dmMain.HTMLExporter.Highlighter  := Ed.Highlighter;
    dmMain.HTMLExporter.ExportAsText := True;
    dmMain.HTMLExporter.Options      := [heoDoctype, heoCharset];
    dmMain.HTMLExporter.ExportAll(ed.Lines);
    dmMain.HTMLExporter.SaveToFile(SaveDialog.FileName);
  end;
end;

procedure TfMain.ExportRTFToClipBoardExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  Ed := EditorFactory.CurrentEditor;
  dmMain.RFTExporter.UseBackground := True;
  dmMain.RFTExporter.Highlighter := Ed.Highlighter;
  dmMain.RFTExporter.ExportAsText := False;
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
  StatusBar.Panels[3].Text := HintInfo.HintStr;
end;

procedure TfMain.EditCopyExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  SendMessage(ed.Handle, LM_COPY, 0, 0);
end;

procedure TfMain.EditCutExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  SendMessage(ed.Handle, LM_CUT, 0, 0);
end;

procedure TfMain.EditPasteExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  SendMessage(ed.Handle, LM_PASTE, 0, 0);
end;

procedure TfMain.ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: boolean);
begin
  if not EditorAvalaible then
    exit;

  EditorFactory.ActivePageIndex := EditorFactory.IndexOfTabAt(MousePos);

  if EditorFactory.ActivePageIndex > -1 then
  begin
    Handled  := True;
    MousePos := EditorFactory.ClientToScreen(MousePos);
    pumTabs.PopupComponent := EditorFactory.CurrentEditor;
    pumTabs.PopUp(MousePos.X, MousePos.Y);
  end;

end;

procedure TfMain.ActionListUpdate(AAction: TBasicAction; var Handled: boolean);
var
  Avail: boolean;
  ed: TEditor;
begin
  Avail := EditorAvalaible;
  Ed    := EditorFactory.CurrentEditor;
  EditRedo.Enabled := Avail and Ed.CanRedo;
  EditUndo.Enabled := Avail and Ed.CanUndo;
  FileSave.Enabled := Avail and Ed.Modified;
  FileReload.Enabled := Avail and not Ed.Untitled;
  EditCopy.Enabled := Avail and ed.SelAvail;
  EditCut.Enabled := Avail and ed.SelAvail;
  ExportHtmlToClipBoard.Enabled := Avail and ed.SelAvail;
  ExportRTFToClipBoard.Enabled := Avail and ed.SelAvail;
  actFullNameToClipBoard.Enabled := Avail and not ed.Untitled;
  actFileNameToClipboard.Enabled := Avail and not ed.Untitled;
  actPathToClipboard.Enabled := Avail and not ed.Untitled;
  FileBrowseFolder.Enabled := Avail and not ed.Untitled;
  ExportHtmlToFile.Enabled := Avail;
  ExportRTFToFile.Enabled := Avail;
  actGoTo.Enabled := Avail and (ed.Lines.Count > 0);
  actCloseAfter.Enabled := EditorFactory.PageCount > EditorFactory.PageIndex;
  actCloseBefore.Enabled := EditorFactory.PageIndex > 0;

  actMacroRecord.Enabled   := SynMacroRec.State <> msRecording;
  actMacroStop.Enabled     := SynMacroRec.State = msRecording;
  actMacroPlayBack.Enabled := (SynMacroRec.State <> msRecording) and Assigned(SynMacroRec.LastMacro);
  actMacroPlaybackMulti.Enabled := SynMacroRec.State <> msRecording;

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

procedure TfMain.actMacroPlayBackExecute(Sender: TObject);
begin
  SynMacroRec.PlayBack(SynMacroRec.LastMacro);
end;

procedure TfMain.actMacroPlaybackMultiExecute(Sender: TObject);
begin
  ShowMacroPlayBack(SynMacroRec);
end;

procedure TfMain.actMacroRecordExecute(Sender: TObject);
begin
  SynMacroRec.Start;
end;

procedure TfMain.actMacroSaveExecute(Sender: TObject);
var
  NewName: string;
begin

  NewName := InputBox(RSMacroSaving, RSMacroNewName, '');

  if NewName = '' then exit;
  SynMacroRec.SaveMacro(NewName);
  SynMacroRec.SignalChange;

end;

procedure TfMain.actMacroStopExecute(Sender: TObject);
begin
  SynMacroRec.Stop;
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

function ExtractQuotedStr(const S: string): string;
var
  tmp: pchar;
begin
  tmp    := PChar(trim(s));
  Result := AnsiExtractQuotedStr(tmp, '''');
end;

procedure TfMain.actUnQuoteExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  Ed.TextOperation(@ExtractQuotedStr, [tomLines]);
end;

procedure TfMain.FileBrowseFolderExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  Ed := EditorFactory.CurrentEditor;
  LoadDir(ExtractFileDir(Ed.FileName));
end;

procedure TfMain.FileCloseFolderExecute(Sender: TObject);
begin
  pnlLeft.Visible    := False;
  splLeftBar.Visible := False;
end;

procedure TfMain.FileReloadFolderExecute(Sender: TObject);
begin
  LoadDir(BrowsingPath);
end;

procedure TfMain.actShowRowNumberExecute(Sender: TObject);
var
  i: integer;
begin
  actShowRowNumber.Checked := not actShowRowNumber.Checked;

  for i := 0 to EditorFactory.PageCount - 1 do
    TEditorTabSheet(EditorFactory.Pages[i]).Editor.Gutter.Visible := actShowRowNumber.Checked;

  ConfigObj.ShowRowNumber := actShowRowNumber.Checked;

end;

procedure TfMain.actShowToolbarExecute(Sender: TObject);
begin
  actShowToolbar.Checked := not actShowToolbar.Checked;
  ConfigObj.ShowToolbar  := actShowToolbar.Checked;
  MainToolbar.Visible    := ConfigObj.ShowToolbar;

end;

procedure TfMain.actFileNameToClipboardExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  Ed := EditorFactory.CurrentEditor;
  Clipboard.AsText := ExtractFileName(Ed.FileName);
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

procedure TfMain.actToggleSpecialCharExecute(Sender: TObject);
begin
  actToggleSpecialChar.Checked := not actToggleSpecialChar.Checked;
  EditorFactory.ChangeOptions(eoShowSpecialChars, actToggleSpecialChar.Checked);

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

procedure TfMain.actZoomInExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  Ed.ExecuteCommand(ecZoomIn, #0, nil);

end;

procedure TfMain.actZoomOutExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  Ed.ExecuteCommand(ecZoomout, #0, nil);

end;

procedure TfMain.actZoomResetExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  Ed.ExecuteCommand(ecZoomNorm, #0, nil);

end;


procedure TfMain.AppPropertiesActivate(Sender: TObject);
begin
  EditorFactory.DoCheckFileChanges;
end;

procedure TfMain.AppPropertiesDropFiles(Sender: TObject; const FileNames: array of string);
var
  i: integer;
begin
  for i := Low(FileNames) to High(FileNames) do
    EditorFactory.AddEditor(FileNames[i]);
end;

procedure TfMain.AppPropertiesIdle(Sender: TObject; var Done: boolean);
begin
  Application.SingleInstance.ServerCheckMessages;
end;

procedure TfMain.actFontExecute(Sender: TObject);
var
  i: integer;
begin
  if Assigned(ConfigObj.Font) then
    FontDialog.Font.Assign(ConfigObj.Font);
  if FontDialog.Execute then
  begin
    for i := 0 to EditorFactory.PageCount - 1 do
      TEditorTabSheet(EditorFactory.Pages[i]).Editor.Font.Assign(FontDialog.Font);
    ConfigObj.Font.Assign(FontDialog.Font);
    ConfigObj.Dirty := True;
  end;

end;

procedure TfMain.actFullNameToClipBoardExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  Ed := EditorFactory.CurrentEditor;
  Clipboard.AsText := Ed.FileName;
end;

procedure TfMain.actFullScreenExecute(Sender: TObject);
var
  r: TRect;
begin

  if WindowState <> wsFullScreen then
  begin
    ws   := WindowState;
    rect := BoundsRect;
    MainToolbar.Visible := False;
    Menu := nil;
    {$IFDEF WINDOWS}
    BorderStyle := bsNone;
    {$ENDIF}
    WindowState := wsFullScreen;
    Application.ProcessMessages;
    WindowState := wsFullScreen;
  end
  else
  begin
    MainToolbar.Visible := True;
    Menu := mnuMain;
    {$IFDEF WINDOWS}
    BorderStyle := bsSizeable;
    {$ENDIF}
    WindowState := ws;
    BoundsRect := rect;

  end;
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

procedure TfMain.actJSONCompactExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  if not EditorAvalaible then
    exit;

  Ed := EditorFactory.CurrentEditor;
  Ed.TextOperation(@CompactJson, [tomFullText]);
end;

procedure TfMain.actMacroManagerExecute(Sender: TObject);
begin
  ShowMacroEditor(SynMacroRec);
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

procedure TfMain.actFindLongestLineExecute(Sender: TObject);
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
  EditorFactory.CloseAll(True);
end;

procedure TfMain.actCloseAfterExecute(Sender: TObject);
begin
  EditorFactory.CloseAfter;
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

procedure TfMain.FileOpenFolderExecute(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
  begin
    LoadDir(SelectDirectoryDialog1.FileName);
  end;

end;

procedure TfMain.FileReloadExecute(Sender: TObject);
var
  Ed: TEditor;
begin
  Ed := EditorFactory.CurrentEditor;
  if Ed.Untitled then
    exit;

  if Ed.Modified then
    if MessageDlg(RSReload, Format(RSReloadFile, [ed.FileName]), mtConfirmation, [mbYes, mbNo], 0) = mrNo then
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

procedure TfMain.FindDialogClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  self.BringToFront;
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
  //ActionList.State := asNormal;
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

procedure TfMain.ServerReceivedParams(Sender: TBaseSingleInstance; aParams: TStringList);
var
  str: string;
  Editor: TEditor;
begin

  for str in aParams do
  begin
    if copy(str, 1, 2) <> '--' then
    begin
      Editor := EditorFactory.AddEditor(str);
      if Assigned(Editor) and not Editor.Untitled then
        MRU.AddToRecent(str);
    end;
  end;
  Application.BringToFront;
  ShowOnTop;
end;

procedure TfMain.FormCreate(Sender: TObject);
var
  i: integer;
  mnuLang: TMenuItem;
  mnuTheme: TmenuItem;
  CurrLetter: string;
  SaveLetter, Key: string;
  CurrMenu: TMenuItem;
  ParamList: TStringList;
begin

  LoadImageList;

  Application.SingleInstance.OnServerReceivedParams := @ServerReceivedParams;
  MRU := TMRUMenuManager.Create(Self);
  MRU.MenuItem := mnuOpenRecent;
  MRU.OnRecentFile := @RecentFileEvent;
  MRU.MaxRecent := 15;
  MRU.Recent.Clear;
  actShowRowNumber.Checked := ConfigObj.ShowRowNumber;
  actShowToolbar.Checked   := ConfigObj.ShowToolbar;

  ConfigObj.ReadStrings('Recent', 'Files', MRU.Recent);
  MRU.ShowRecentFiles;
  ReplaceDialog := TCustomReplaceDialog.Create(self);
  with ReplaceDialog do
  begin
    OnClose   := @FindDialogClose;
    //      Options := [ssoReplace, ssoEntireScope];
    OnFind    := @ReplaceDialogFind;
    OnReplace := @ReplaceDialogReplace;
  end;

  EditorFactory := TEditorFactory.Create(Self);
  EditorFactory.Align := alClient;
  EditorFactory.OnStatusChange := @EditorStatusChange;
  EditorFactory.OnBeforeClose := @BeforeCloseEditor;
  EditorFactory.OnNewEditor := @NewEditor;
  EditorFactory.OnContextPopup := @ContextPopup;
  EditorFactory.Images := imgList;
  EditorFactory.Parent := self;

  SynMacroRec := TMacroRecorder.Create(EditorFactory);
  Macros      := TMRUMenuManager.Create(Self);
  Macros.MenuItem := mnuSavedMacros;
  Macros.OnRecentFile := @RecentMacroEvent;
  Macros.MaxRecent := 15;
  Macros.Recent.Clear;
  SynMacroRec.Macros.MacroNames(Macros.Recent);
  Macros.ShowRecentFiles;
  SynMacroRec.OnListChange := @SynMacroRecListChange;

  //// move close button to right
  //tbbCloseAll.Align := alRight;
  //tbbSepClose.Align := alRight;
  //tbbClose.Align := alRight;

  // Parameters
  FileOpen.Dialog.Filter := configobj.GetFiters;

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
    mnuLang     := TMenuItem.Create(Self);
    mnuLang.Caption := ARHighlighter[i].HLClass.GetLanguageName;
    mnuLang.Tag := i;
    mnuLang.OnClick := @mnuLangClick;
    CurrLetter  := UpperCase(Copy(mnuLang.Caption, 1, 1));
    if SaveLetter <> CurrLetter then
    begin
      SaveLetter := CurrLetter;
      CurrMenu   := TMenuItem.Create(Self);
      CurrMenu.Caption := CurrLetter;
      mnuLanguage.Add(CurrMenu);
    end;

    CurrMenu.Add(mnuLang);
  end;

  for Key in ConfigObj.ThemeList.Keys do
  begin
    mnuTheme := TMenuItem.Create(Self);
    mnuTheme.Caption := Key;
    mnuTheme.RadioItem := True;
    if ConfigObj.ThemeList.Items[key] = ConfigObj.AppSettings.ColorSchema then
      mnuTheme.Checked := True;
    mnuTheme.OnClick   := @mnuThemeClick;
    mnuThemes.Add(mnuTheme);
  end;

  if ParamCount > 0 then
  try
    ParamList := TStringList.Create;
    for i := 1 to ParamCount do
      ParamList.Add(ParamStr(i));
    ServerReceivedParams(Application.SingleInstance, ParamList);
  finally
    FreeAndNil(ParamList);
  end;

  if EditorFactory.PageCount = 0 then
    FileNew.Execute;

  pnlLeft.Visible    := False;
  splLeftBar.Visible := False;

end;

procedure TfMain.FormDeactivate(Sender: TObject);
begin
  //ActionList.State := asSuspended;
end;

procedure TfMain.LoadImageList;
var
  s: TResourceStream;
  iconRender: TIconRenderer;
  {$R ovotextfont.res}
begin
  S := TResourceStream.Create(HInstance, 'OVOFONT', RT_RCDATA);
  ImgList.BeginUpdate;
  ImgList.Clear;
  ImgList.Scaled := False;
  ;
  ImgList.Height := MulDiv(24, Screen.PixelsPerInch, 96);
  ImgList.Width  := ImgList.Height;

  iconRender := TIconRenderer.Create(S);
  iconRender.Color := GetSysColor(COLOR_BTNTEXT);
  iconRender.SetSize(24, 22);
  iconRender.AddToImageList(imglist, [$41, $42, $43, $44, $45,  // 0.. 4  A B C D E
    $46, $47, $48, $49, $4a,  // 5.. 9  F G H I J
    $4b, $4c, $4d, $4e, $4f,  //10..14  K L M N O
    $50, $51, $52, $53, $54,  //15..19  P Q R S T
    $55, $56, $57, $58, $59,  //20..24  U V W X Y
    $5a, $61, $62, $63, $64,  //25..29  Z a b c d
    $65, $66, $67, $68, $69,  //30..34  e f g h i
    $6a, $6b, $6c, $6d, $3b,  //31..39  j k l m ;
    $3c, $5b]);            //40..41  < [

  ImgList.EndUpdate;
  dmMain.imgBookMark.BeginUpdate;
  dmMain.imgBookMark.Clear;
  dmMain.imgBookMark.Height := MulDiv(16, Screen.PixelsPerInch, 96);
  dmMain.imgBookMark.Width  := dmMain.imgBookMark.Height;

  iconRender.Color := GetSysColor(COLOR_HIGHLIGHT);
  iconRender.SetSize(16, 16);
  iconRender.AddToImageList(dmMain.imgBookMark, [$30, $31, $32, $33, $34,
    $35, $36, $37, $38, $39, $3a]);
  iconRender.Color := GetSysColor(COLOR_BTNTEXT);
  iconRender.AddToImageList(dmMain.imgBookMark, [$3b, $3c]);

  dmMain.imgBookMark.EndUpdate;
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
    Screen.Cursor := crDefault;
  end;

end;

procedure TfMain.mnuThemeClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
  try
    Screen.Cursor := crHourGlass;
    ConfigObj.SetTheme(TMenuItem(Sender).Caption);
    EditorFactory.ReloadHighLighters;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  Application.SingleInstance.OnServerReceivedParams := nil;
  ConfigObj.WriteStrings('Recent', 'Files', MRU.Recent);
  Mru.Free;
  SynMacroRec.Free;
  FreeAndNil(EditorFactory);
  ReplaceDialog.Free;
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
  ConfigObj.Dirty := True;
end;

procedure TfMain.FormShow(Sender: TObject);
begin
  ReadConfig;
end;

procedure TfMain.FormWindowStateChange(Sender: TObject);
begin
  ConfigObj.Dirty := True;
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

procedure TfMain.StatusBarResize(Sender: TObject);
var
  i: integer;
  pnlSize: integer;
begin
  pnlSize := 0;
  for i := 1 to StatusBar.Panels.Count - 1 do
  begin
    Inc(pnlSize, StatusBar.Panels[i].Width);
  end;

  StatusBar.Panels[0].Width := max(0, StatusBar.Width - pnlSize - (StatusBar.BorderWidth * 2));

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

procedure TfMain.mnuCleanRecentClick(Sender: TObject);
begin
  MRU.Recent.Clear;
  MRU.ShowRecentFiles;
  ConfigObj.WriteStrings('Recent', 'File', MRU.Recent);
end;

procedure TfMain.mnuReopenAllRecentClick(Sender: TObject);
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
    sfleCrLf: mnuCRLF.Checked := True;
    sfleLf: mnuLF.Checked     := True;
    sfleCr: mnuCR.Checked     := True;
  end;
  mnuCR.Enabled   := not mnuCR.Checked;
  mnuLF.Enabled   := not mnuLF.Checked;
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
    smText: begin
      SaveDialog.DefaultExt := '.txt';
      SaveDialog.Filter     := ConfigObj.GetFiters;
    end;
    smRTF: begin
      SaveDialog.DefaultExt := '.rtf';
      SaveDialog.Filter     := 'RTF Files (*.rtf)|*.rtf';
      SaveDialog.Title      := 'Export as RTF File';
    end;
    smHTML: begin
      SaveDialog.DefaultExt := '.html';
      SaveDialog.Filter     := 'HTML Files (*.htm*)|*.htm*';
      SaveDialog.Title      := 'Export as HTML File';
    end;
  end;
end;

procedure TfMain.SynMacroRecListChange(Sender: TObject);
begin
  SynMacroRec.Macros.MacroNames(Macros.Recent);
  Macros.ShowRecentFiles;
end;

procedure TfMain.SaveConfig;
begin
  with ConfigObj.ConfigHolder do
  begin
    Find('MainForm/NormalLeft', True).AsInteger   := ScaleFormTo96(Left);
    Find('MainForm/NormalTop', True).AsInteger    := ScaleFormTo96(Top);
    Find('MainForm/NormalWidth', True).AsInteger  := ScaleFormTo96(Width);
    Find('MainForm/NormalHeight', True).AsInteger := ScaleFormTo96(Height);

    Find('MainForm/RestoredLeft', True).AsInteger   := ScaleFormTo96(RestoredLeft);
    Find('MainForm/RestoredTop', True).AsInteger    := ScaleFormTo96(RestoredTop);
    Find('MainForm/RestoredWidth', True).AsInteger  := ScaleFormTo96(RestoredWidth);
    Find('MainForm/RestoredHeight', True).AsInteger := ScaleFormTo96(RestoredHeight);

    Find('MainForm/WindowState', True).AsInteger := integer(WindowState);
  end;

end;

procedure TfMain.ReadConfig;
var
  LastWindowState: TWindowState;
begin
  with ConfigObj.ConfigHolder do
  begin
    LastWindowState := TWindowState(GetValueDef('MainForm/WindowState', integer(WindowState)));

    if LastWindowState = wsMaximized then
    begin
      WindowState := wsNormal;
      BoundsRect  := Bounds(Scale96ToForm(GetValueDef('MainForm/RestoredLeft', RestoredLeft)),
        Scale96ToForm(GetValueDef('MainForm/RestoredTop', RestoredTop)), Scale96ToForm(
        GetValueDef('MainForm/RestoredWidth', RestoredWidth)), Scale96ToForm(
        GetValueDef('MainForm/RestoredHeight', RestoredHeight)));
      WindowState := wsMaximized;
      Application.ProcessMessages;
      WindowState := wsMaximized;
    end
    else
    begin
      WindowState := wsNormal;
      BoundsRect  := Bounds(Scale96ToForm(GetValueDef('MainForm/NormalLeft', Left)),
        Scale96ToForm(GetValueDef('MainForm/NormalTop', Top)), Scale96ToForm(GetValueDef('MainForm/NormalWidth', Width)),
        Scale96ToForm(GetValueDef('MainForm/NormalHeight', Height)));
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
    mnuitem     := TMenuItem.Create(mnuTabs);
    mnuitem.Caption := EditorFactory.Pages[i].Caption;
    mnuitem.Tag := i;
    mnuitem.OnClick := @ShowTabs;
    mnuTabs.Add(mnuitem);
  end;

end;

procedure TfMain.ReplaceDialogFind(Sender: TObject);
var
  ed: TEditor;
  Options: TMySynSearchOptions;
begin
  ed      := EditorFactory.CurrentEditor;
  Options := ReplaceDialog.Options;
  Exclude(Options, ssoReplace);
  if ssoExtended in Options then
    FindText := DecodeExtendedSearch(ReplaceDialog.FindText)
  else
    FindText := ReplaceDialog.FindText;

  if Ed.SearchReplace(FindText, '', TSynSearchOptions(Options)) = 0 then
    ShowMessage(Format(RSTextNotfound, [ReplaceDialog.FindText]));
end;

procedure TfMain.ReplaceDialogReplace(Sender: TObject);
var
  ed: TEditor;
  Options: TMySynSearchOptions;
begin

  ed      := EditorFactory.CurrentEditor;
  Options := ReplaceDialog.Options;

  if ssoExtended in Options then
  begin
    FindText    := DecodeExtendedSearch(ReplaceDialog.FindText);
    ReplaceText := DecodeExtendedSearch(ReplaceDialog.ReplaceText);
  end
  else
  begin
    FindText    := ReplaceDialog.FindText;
    ReplaceText := ReplaceDialog.ReplaceText;
  end;

  if Ed.SearchReplace(FindText, ReplaceText, TSynSearchOptions(Options)) = 0 then
    ShowMessage(Format(RSTextNotfound, [ReplaceDialog.FindText]))
  else
  if (ssoReplace in Options) and not (ssoReplaceAll in Options) then
  begin
    Exclude(Options, ssoReplace);
    Ed.SearchReplace(FindText, '', TSynSearchOptions(Options));
  end;

  if Assigned(ed.OnSearchReplace) then
    ed.OnSearchReplace(Ed, ReplaceDialog.FindText, ReplaceDialog.ReplaceText, Options);

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

  if Assigned(Editor.Highlighter) then
    StatusBar.panels[0].Text := Editor.Highlighter.LanguageName
  else
    StatusBar.panels[0].Text := RSNormalText;

  if (scCaretX in Changes) or (scCaretY in Changes) then
    StatusBar.Panels[1].Text := Format(RSStatusBarPos, [Editor.CaretY, Editor.CaretX]);

  if (scSelection in Changes) then
    StatusBar.Panels[2].Text :=
      Format(RSStatusBarSel, [Editor.SelEnd - Editor.SelStart]);

  if (scModified in Changes) then
    if Editor.Modified then
      Editor.Sheet.ImageIndex := IDX_IMG_MODIFIED
    else
      Editor.Sheet.ImageIndex := IDX_IMG_STANDARD;

  case Editor.LineEndingType of
    sfleCrLf: StatusBar.Panels[4].Text := mnuCRLF.Caption;
    sfleLf: StatusBar.Panels[4].Text   := mnuLF.Caption;
    sfleCr: StatusBar.Panels[4].Text   := mnuCR.Caption;
    else
      StatusBar.Panels[4].Text := '';
  end;

  StatusBar.Panels[5].Text := Editor.DiskEncoding;

  if (scInsertMode in Changes) then
    if Editor.InsertMode then
      StatusBar.Panels[6].Text := RSStatusBarInsMode
    else
      StatusBar.Panels[6].Text := RSStatusBarOvrMode;
end;

procedure TfMain.RecentFileEvent(Sender: TObject; const AFileName: string; const AData: TObject);
begin
  EditorFactory.AddEditor(AFileName);
  MRU.AddToRecent(AFileName);
end;

procedure TfMain.RecentMacroEvent(Sender: TObject; const AFileName: string; const AData: TObject);
begin
  SynMacroRec.PlayBack(TMacro(Adata));
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
    ReplaceDialog.FindText := Editor.SelText
  else
    ReplaceDialog.FindText := Editor.GetWordAtRowCol(Editor.CaretXY);

  ReplaceDialog.Options := ReplaceDialog.Options - [ssoReplace, ssoReplaceAll];
  ReplaceDialog.Show;
end;

procedure TfMain.SearchFindNextExecute(Sender: TObject);
var
  sOpt: TMySynSearchOptions;
  Ed: TEditor;
begin
  if (FindText = EmptyStr) then
    Exit;
  Ed := EditorFactory.CurrentEditor;
  if Assigned(Ed) then
  begin
    sOpt := SynOption;
    sOpt := sOpt - [ssoBackWards];
    if Ed.SearchReplace(FindText, '', TSynSearchOptions(sOpt)) = 0 then
      ShowMessage(Format(RSTextNotfound, [FindText]));
  end;

end;

procedure TfMain.SearchFindPreviousExecute(Sender: TObject);
var
  sOpt: TMySynSearchOptions;
  Ed: TEditor;
begin
  if (FindText = EmptyStr) then
    Exit;

  Ed := EditorFactory.CurrentEditor;
  if Assigned(Ed) then
  begin
    sOpt := SynOption;
    sOpt := sOpt + [ssoBackWards];
    if Ed.SearchReplace(FindText, '', TSynSearchOptions(sOpt)) = 0 then
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
    ReplaceDialog.FindText := Editor.SelText
  else
    ReplaceDialog.FindText := Editor.GetWordAtRowCol(Editor.CaretXY);

  ReplaceDialog.cbReplace.Checked := True;
  ReplaceDialog.Show;

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


//case Dialog.SearchMode of
//  smRegexp : begin
//              include(SynOption, ssoRegExpr);
//              ReplaceText := Dialog.ReplaceText;
//             end;
//  smNormal : begin
//               ReplaceText := Dialog.ReplaceText;
//             end;
//  smExtended: begin
//                FindText := JSONStringToString(Dialog.FindText);
//                ReplaceText := JSONStringToString(Dialog.ReplaceText);
//              end;
//end;



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
      mrNo: Cancel     := False;
      mrCancel: Cancel := True;
    end;

end;

procedure TfMain.LoadDir(Path: string);
begin
  pnlLeft.Visible := True;
  splLeftBar.Visible := True;
  BrowsingPath := Path;
  FilesTree.Items.Clear;
  ExpandNode(nil, Path);
end;

procedure TfMain.ExpandNode(NodeDir: TFileTreeNode; const Path: string);
var
  DirList: TStringList;
  FileList: TStringList;
  i, j: integer;
  CurrentPath: string;
  NewNode: TFileTreeNode;
begin
  DirList     := TStringList.Create;
  FileList    := TStringList.Create;
  FileList.OwnsObjects := True;
  CurrentPath := IncludeTrailingPathDelimiter(Path);
  try
    BuildFolderList(CurrentPath, DirList);
    DirList.Sort;
    for i := 0 to DirList.Count - 1 do
    begin
      NewNode := TFileTreeNode(FilesTree.items.AddChild(NodeDir, ExtractFileName(DirList[i])));
      NewNode.FullPath := DirList[i];
      NewNode.isDir := True;
      NewNode.HasChildren := True;
    end;
    FileList.Clear;
    BuildFileList(IncludeTrailingPathDelimiter(CurrentPath) + AllFilesMask,
      faAnyFile, FileList, False);
    FileList.Sort;

    for j := 0 to FileList.Count - 1 do
    begin
      NewNode := TFileTreeNode(FilesTree.items.AddChild(NodeDir, ExtractFileName(FileList[j])));
      NewNode.FullPath := FileList[j];
      NewNode.isDir := False;
    end;

  finally
    DirList.Free;
    FileList.Free;
  end;

  //  if PathHistory.IndexOf(Path) < 0 then
  //     PathIndex := PathHistory.Add(Path);

end;

procedure TfMain.FilesTreeCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := TFileTreeNode;
end;

procedure TfMain.FilesTreeDblClick(Sender: TObject);
var
  Node: TFileTreeNode;
begin
  Node := TFileTreeNode(FilesTree.Selected);
  if Node = nil then
    exit;

  if Node.isDir then
    exit
  else
  begin
    EditorFactory.AddEditor(Node.FullPath);
  end;

end;

procedure TfMain.FilesTreeExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: boolean);
var
  myNode: TFileTreeNode;
begin
  myNode := TFileTreeNode(Node);
  if myNode = nil then
    exit;

  if (myNode.isDir) then
    ExpandNode(MyNode, myNode.FullPath);

end;

procedure TfMain.FilesTreeGetImageIndex(Sender: TObject; Node: TTreeNode);
var
  myNode: TFileTreeNode;
begin
  myNode := TFileTreeNode(Node);
  if myNode = nil then
    exit;

  if (myNode.isDir) then
    myNode.ImageIndex := 11
  else
    myNode.ImageIndex := 12;

end;

procedure TfMain.FilesTreeGetSelectedIndex(Sender: TObject; Node: TTreeNode);
var
  myNode: TFileTreeNode;
begin
  myNode := TFileTreeNode(Node);
  if myNode = nil then
    exit;
  myNode.SelectedIndex := myNode.ImageIndex;
end;


end.
