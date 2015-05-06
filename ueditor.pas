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
unit ueditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Dialogs, ComCtrls, LCLProc,
  SynEditTypes, SynEdit, SynGutter, SynGutterMarks, SynGutterLineNumber,SynGutterChanges,
  Stringcostants, Forms, Graphics, Config, udmmain, uCheckFileChange;

type

  TEditor = class;
  TEditorTabSheet = class;
  TEditorFactory = class;

  TTextOperation = function(const Param:string):string;

  { TEditorFactory }

  TOnBeforeClose = procedure(Editor: TEditor; var Cancel: boolean) of object;
  TOnEditorEvent = procedure(Editor: TEditor) of object;

  TEditor = class(TSynEdit)
  private
    FFileName: TFilename;
    FSheet: TEditorTabSheet;
    FUntitled: boolean;
    fCaretPos, fSel: TPoint;
    procedure CreateDefaultGutterParts;
    procedure QuickSort(L, R: Integer; CompareFn: TStringsSortCompare);
    procedure SetFileName(AValue: TFileName);
    procedure SetUntitled(AValue: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    property Sheet: TEditorTabSheet read FSheet;
    //-- Helper functions//
    procedure SetLineText(Index: integer; NewText: string);
    // -- File handling//
    property FileName: TFileName read FFileName write SetFileName;
    property Untitled: boolean read FUntitled write SetUntitled;
    procedure LoadFromFile(AFileName: TFileName);
    Procedure Sort(Ascending:boolean);
    procedure TextOperation(Operation: TTextOperation);
    procedure PushPos;
    procedure PopPos;
    //
    function Save: boolean;
    function SaveAs(AFileName: TFileName): boolean;
  end;

  { TEditorTabSheet }

  TEditorTabSheet = class(TTabSheet)
  private
    FEditor: TEditor;
  protected
    procedure DoShow; override;

  public
    property Editor: TEditor read FEditor;
    //--//
  end;


  TEditorFactory = class(TPageControl)
  private
    FOnBeforeClose: TOnBeforeClose;
    FOnNewEditor: TOnEditorEvent;
    FonStatusChange: TStatusChangeEvent;
    fUntitledCounter: integer;
    FWatcher : TFileWatcher;
    function GetCurrentEditor: TEditor;
    procedure SetOnBeforeClose(AValue: TOnBeforeClose);
    procedure SetOnNewEditor(AValue: TOnEditorEvent);
    procedure ShowHintEvent(Sender: TObject; HintInfo: PHintInfo);
    function CreateEmptyFile(AFileName: TFileName): boolean;
    procedure OnFileChange(Sender: TObject; FileName :TFileName; Data:Pointer; State:TFWStateChange);
  protected
    procedure DoChange; override;
  public
    property CurrentEditor: TEditor read GetCurrentEditor;
    property OnStatusChange: TStatusChangeEvent read FonStatusChange write FOnStatusChange;
    property OnBeforeClose: TOnBeforeClose read FOnBeforeClose write SetOnBeforeClose;
    property OnNewEditor: TOnEditorEvent read FOnNewEditor write SetOnNewEditor;
    //--//
    procedure DoCloseTabClicked(APage: TCustomPage); override;
    function AddEditor(FileName: TFilename = ''): TEditor;
    function CloseEditor(Editor: TEditor; Force:boolean=false): boolean;
    function CloseAll: boolean;
    function SaveAll: boolean;
    procedure DoCheckFileChanges;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TEditorTabSheet }

procedure TEditorTabSheet.DoShow;
begin
  inherited DoShow;

end;

{ TEditor }

procedure TEditor.SetFileName(AValue: TFileName);
begin
  if FFileName = AValue then
    Exit;

  FFileName := AValue;
  if FFileName <> EmptyStr then
    begin
      FUntitled := False;
      Highlighter := dmMain.getHighLighter(ExtractFileExt(fFileName));
      FSheet.Caption := ExtractFileName(fFileName);
    end
  else
    FUntitled:=true;

end;

procedure TEditor.SetUntitled(AValue: boolean);
begin
  if FUntitled = AValue then
    Exit;

  FUntitled := AValue;
  if FUntitled then
    FFileName := EmptyStr;
end;

constructor TEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateDefaultGutterParts;
end;

procedure TEditor.SetLineText(Index: integer; NewText: string);
begin
  TextBetweenPoints[Point(1, Index + 1), PhysicalToLogicalPos(Point(Length(Lines[Index]) + 1, Index + 1))] := NewText;
end;

procedure TEditor.LoadFromFile(AFileName: TFileName);
begin
  SetFileName(AFileName);
  Lines.LoadFromFile(FFileName);

end;

procedure TEditor.Sort(Ascending: boolean);
var
  f : TStringsSortCompare;
begin
  f := @CompareStr;
  QuickSort(0, Lines.Count -1, f);
end;

function TEditor.Save: boolean;
begin
  Result := SaveAs(FFileName);
  TEditorFactory(Sheet.Owner).FWatcher.Update(FFileName);
end;

function TEditor.SaveAs(AFileName: TFileName): boolean;
var
  Retry: boolean;
begin
  repeat
    Retry := False;
    try
      if FFileName <> EmptyStr then
         TEditorFactory(Sheet.Owner).FWatcher.RemoveFile(FFileName);
      SetFileName(AFileName);
      Lines.SaveToFile(AFileName);
      TEditorFactory(Sheet.Owner).FWatcher.AddFile(FFileName,Self);
      Result := True;
      FUntitled := False;
      Modified := False;
    except
      Result := False;
    end;

    if not Result then
      begin
        case MessageDlg(RSError, Format(RSCannotSave, [fFileName]), mtError, [mbRetry, mbCancel, mbIgnore], 0) of
          mrAbort: Result := False;
          mrIgnore: Result := True;
          mrRetry: Retry := True;
        end;
      end;
  until not Retry;

end;

function TEditorFactory.CreateEmptyFile(AFileName: TFileName): boolean;
var
  fs: TFileStream;
  Retry: boolean;
begin
  repeat
    Retry := False;
    try
      fs := TFileStream.Create(AFileName, fmCreate);
      fs.Free;
      Result := True;
    except
      Result := False;
    end;

    if not Result then
      begin
        case MessageDlg(RSError, Format(RSCannotCreate, [AFileName]), mtError, [mbRetry, mbAbort], 0) of
          mrAbort: Result := False;
          mrRetry: Retry := True;
        end;
      end;
  until not Retry;

end;

procedure TEditorFactory.OnFileChange(Sender: TObject; FileName: TFileName;
  Data: Pointer; State: TFWStateChange);
var
  ed: TEditor;
  dlgText: String;
begin
  ed := TEditor(Data);
  case State of
    fwscModified : begin
                     if ed.Modified then
                       dlgText:= RSReloadModified
                     else
                       dlgText:= RSReloadsimple;

                     if MessageDlg(RSReload, Format(dlgText, [FileName]), mtConfirmation, [mbyes, mbno], 0) = mrYes then
                       begin
                         ed.PushPos;
                         ed.LoadFromFile(FileName);
                         ed.Modified:=false;
                         ed.PopPos;
                       end
                     else
                       ed.Modified:=true;

                   end;
    fwscDeleted : begin
                    if MessageDlg(RSReload, Format(RSKeepDeleted, [FileName]), mtConfirmation, [mbyes, mbno], 0) = mrYes then
                       begin
                          ed.Modified:=true;
                          FWatcher.Update(FileName);
                       end
                     else
                       CloseEditor(Ed, True);
                   end;
  end;
  FWatcher.Update(FileName);
end;

procedure TEditor.CreateDefaultGutterParts;
var
  SpecialAttr : TFontAttributes;
  DefaultAttr : TFontAttributes;
begin
  Gutter.Parts.Clear;
  DefaultAttr := ConfigObj.ReadFontAttributes('Default/Gutter/', FontAttributes());
  Gutter.Color:= DefaultAttr.Background;

  with TSynGutterMarks.Create(Gutter.Parts) do
    Begin
      Name := 'SynGutterMarks1';
      MarkupInfo.Background := DefaultAttr.Background;
      MarkupInfo.Foreground:= DefaultAttr.Foreground;
      MarkupInfo.Style := DefaultAttr.Styles;
    end;
  with TSynGutterLineNumber.Create(Gutter.Parts) do
    begin
       Name := 'SynGutterLineNumber1';
       SpecialAttr := ConfigObj.ReadFontAttributes('Default/LineNumber/', DefaultAttr);
       MarkupInfo.Background := SpecialAttr.Background;
       MarkupInfo.Foreground:= SpecialAttr.Foreground;
       MarkupInfo.Style := SpecialAttr.Styles;
    end;
  with TSynGutterSeparator.Create(Gutter.Parts) do
    begin
       Name := 'SynGutterSeparator1';
       SpecialAttr := ConfigObj.ReadFontAttributes('Default/LineNumber/', DefaultAttr);
       MarkupInfo.Background := SpecialAttr.Background;
       MarkupInfo.Foreground:= SpecialAttr.Foreground;
       LineWidth:=1;
       Width:=2;
    end;
  with TSynGutterChanges.Create(Gutter.Parts) do
    begin
       Name := 'SynGutterChanges';
       Visible:=false;
    end;

end;

procedure TEditor.QuickSort(L, R: Integer; CompareFn: TStringsSortCompare);
var
  Pivot, vL, vR: Integer;
begin
  //if ExchangeItems is override call that, else call (faster) ExchangeItemsInt
  if R - L <= 1 then begin // a little bit of time saver
    if L < R then
      if CompareFn(Lines[L], Lines[R]) > 0 then
        Lines.Exchange(L, R);
    Exit;
  end;

  vL := L;
  vR := R;

  Pivot := L + Random(R - L); // they say random is best

  while vL < vR do begin
    while (vL < Pivot) and (CompareFn(Lines[vL], Lines[Pivot]) <= 0) do
      Inc(vL);

    while (vR > Pivot) and (CompareFn(Lines[vR], Lines[Pivot]) > 0) do
      Dec(vR);

    Lines.Exchange(vL, vR);

    if Pivot = vL then // swap pivot if we just hit it from one side
      Pivot := vR
    else if Pivot = vR then
      Pivot := vL;
  end;

  if Pivot - 1 >= L then
    QuickSort(L, Pivot - 1, CompareFn);
  if Pivot + 1 <= R then
    QuickSort(Pivot + 1, R, CompareFn);
end;

procedure TEditor.TextOperation(Operation: TTextOperation);
var
  i: integer;
  tmpst: TStringList;
begin

  if SelAvail then
    begin
      tmpst := TStringList.Create;
      tmpst.Text:=SelText;
      for i := 0 to tmpst.Count - 1 do
        tmpst[i] := Operation(tmpst[i]);
      SelText:= copy(tmpst.Text, 1, Length(tmpst.Text)-Length(LineEnding));

    end
  else
    begin
      BeginUpdate(True);
      try
        for i := 0 to Lines.Count - 1 do
        begin
          SetLineText(i, Operation(Lines[i]));
        end;

      finally
        EndUpdate;
      end;
    end;
end;

procedure TEditor.PushPos;
begin
  fCaretPos := CaretXY;
end;

procedure TEditor.PopPos;
begin
  CaretXY := fCaretPos;
end;


{ TEditorFactory }

function TEditorFactory.GetCurrentEditor: TEditor;
begin
  Result := nil;
  if (PageCount > 0) and (ActivePageIndex >= 0) then
    Result := TEditorTabSheet(ActivePage).Editor;

end;

procedure TEditorFactory.SetOnBeforeClose(AValue: TOnBeforeClose);
begin
  if FOnBeforeClose = AValue then
    Exit;
  FOnBeforeClose := AValue;
end;

procedure TEditorFactory.SetOnNewEditor(AValue: TOnEditorEvent);
begin
  if FOnNewEditor = AValue then
    Exit;
  FOnNewEditor := AValue;
end;

procedure TEditorFactory.DoChange;
begin
  inherited DoChange;
  //  Hint := TEditorTabSheet(ActivePage).Editor.FileName;
  if Assigned(OnStatusChange) then
    OnStatusChange(GetCurrentEditor, [scCaretX, scCaretY, scModified, scInsertMode]);

  TEditorTabSheet(ActivePage).Editor.SetFocus;
end;

procedure TEditorFactory.DoCloseTabClicked(APage: TCustomPage);
begin
  inherited DoCloseTabClicked(APage);
  if Assigned(APage) and (APage is TEditorTabSheet) then
    CloseEditor(TEditorTabSheet(APage).FEditor);
end;



function TEditorFactory.AddEditor(FileName: TFilename = ''): TEditor;
var
  Sheet: TEditorTabSheet;
  i: integer;
  DefaultAttr: TFontAttributes;
begin
  if FileName <> EmptyStr then
    begin
    // do not reopen same file
    for i := 0 to PageCount - 1 do
      begin
        Sheet := TEditorTabSheet(Pages[i]);
        if Sheet.Editor.FileName = FileName then
          begin
            ActivePageIndex := i;
            exit;
          end;
      end;

    if (FileName <> EmptyStr) and not FileExists(FileName) then
      begin
        case MessageDlg('', format(RSAskFileCreation, [FileName]), mtConfirmation, [mbYes, mbNo], 0) of
          mrNo: Exit;
          mrYes: if not CreateEmptyFile(FileName) then
              Exit;
        end;
      end;

    // try to reuse an empty sheet
    for i := 0 to PageCount - 1 do
      begin
        Sheet := TEditorTabSheet(Pages[i]);
        if (Sheet.Editor.Untitled) and not Sheet.Editor.Modified then
          begin
            Sheet.Editor.LoadFromfile(FileName);
            FWatcher.AddFile(FileName, Sheet.Editor);
            ActivePageIndex := i;
            exit;
          end;
        end;

    end;

  Sheet := TEditorTabSheet.Create(Self);
  Sheet.PageControl := Self;

  Result := TEditor.Create(Sheet);
  Result.Font.Assign(ConfigObj.Font);
  DefaultAttr := ConfigObj.ReadFontAttributes('Default/Text/', FontAttributes());

  Result.FSheet := Sheet;

  Result.Align := alClient;
  Sheet.FEditor := Result;

  Result.Font.Color := DefaultAttr.Foreground;
  Result.Font.Style := DefaultAttr.Styles;

  Result.Color := DefaultAttr.Background;

  Result.Options := Result.Options + [eoHideRightMargin];
  Result.BookMarkOptions.BookmarkImages := dmMain.imgBookMark;

  Result.OnStatusChange := OnStatusChange;
  if Assigned(OnStatusChange) then
    OnStatusChange(Result, [scCaretX, scCaretY, scModified, scInsertMode]);

  Result.Parent := Sheet;
  if FileName = EmptyStr then
    begin
      Sheet.Caption := Format(RSNewFile, [fUntitledCounter]);
      Result.FUntitled := True;
      Inc(fUntitledCounter);
      Text:=EmptyStr;
    end
  else
    begin
      Result.LoadFromfile(FileName);
      FWatcher.AddFile(FileName, Result);
    end;

  ActivePage := Sheet;

  if Assigned(FOnNewEditor) then
    FOnNewEditor(Result);

end;

function TEditorFactory.CloseEditor(Editor: TEditor; Force:boolean=false): boolean;
var
  Sheet: TEditorTabSheet;
  Cancel: boolean;
begin
  Result := True;
  // if last tab in unused
  if (PageCount = 1) and Editor.Untitled and not Editor.Modified and not ConfigObj.AppSettings.CloseWithLastTab then
    exit;

  Cancel := false;
  if Assigned(FOnBeforeClose) and not Force then
    FOnBeforeClose(Editor, Cancel);

  if (not Cancel) or Force then
    begin
      Sheet := Editor.FSheet;
      Editor.PopupMenu := nil;
      FWatcher.RemoveFile(Editor.FileName);
      Application.ReleaseComponent(Editor);
      Application.ReleaseComponent(Sheet);
      Application.ProcessMessages;
      if (PageCount = 0) and not ConfigObj.AppSettings.CloseWithLastTab then
        AddEditor();
    end
  else
    Result := False;

end;

function TEditorFactory.CloseAll: boolean;
var
  i: integer;
begin
  result:= true;
  for i := PageCount - 1 downto 0 do
    if not CloseEditor(TEditorTabSheet(Pages[i]).Editor) then
      begin
        Result:= false;
        break;
      end;
end;

function TEditorFactory.SaveAll: boolean;
var
  i: integer;
begin
  result:= true;
  for i := PageCount - 1 downto 0 do
    if not TEditorTabSheet(Pages[i]).Editor.Save then
      begin
        Result:= false;
        break;
      end;

end;

procedure TEditorFactory.DoCheckFileChanges;
begin
  FWatcher.CheckFiles;
end;

procedure TEditorFactory.ShowHintEvent(Sender: TObject; HintInfo: PHintInfo);
var
  Tab: integer;

begin
  if (PageCount = 0) or (HintInfo = nil) then
    Exit;
  Tab := TabIndexAtClientPos(ScreenToClient(Mouse.CursorPos));

  if Tab < 0 then
    Exit;

  HintInfo^.HintStr := TEditorTabSheet(Pages[Tab]).Editor.FileName;

end;

constructor TEditorFactory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWatcher := TFileWatcher.Create;
  FWatcher.OnFileStateChange:=@OnFileChange;
  fUntitledCounter := 0;
  Options := Options + [nboShowCloseButtons];
  OnShowHint := @ShowHintEvent;
end;

destructor TEditorFactory.Destroy;
begin
  FWatcher.Free;
  inherited Destroy;
end;



end.
