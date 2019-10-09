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
unit ReplaceDialog;

interface

uses
  Classes, SysUtils, TypInfo, Forms, Controls, Graphics, Dialogs, lcltype,
  Buttons, StdCtrls, ExtCtrls, ButtonPanel, Arrow, ComCtrls, Config, SynEditTypes;

type

  { TCustomReplaceDialog }

  { TMyReplaceDialog }
  TSearchMode = (smNormal, smExtended, smRegexp);

  TMySynSearchOption = (ssoMatchCase, ssoWholeWord,
      ssoBackwards,
      ssoEntireScope, ssoSelectedOnly,
      ssoReplace, ssoReplaceAll,
      ssoPrompt,
      ssoSearchInReplacement,    // continue search-replace in replacement (with ssoReplaceAll) // replace recursive
      ssoRegExpr, ssoRegExprMultiLine,
      ssoFindContinue, ssoExtended);
  TMySynSearchOptions = set of TMySynSearchOption;


  TCustomReplaceDialog = class(TForm)
    BackwardRadioButton: TRadioButton;
    BtnPanel: TButtonPanel;
    CaseSensitiveCheckBox: TCheckBox;
    cbReplace: TCheckBox;
    DirectionGroupBox: TGroupBox;
    EditFind: TComboBox;
    EditReplace: TComboBox;
    EntireScopeCheckBox: TCheckBox;
    ForwardRadioButton: TRadioButton;
    GlobalRadioButton: TRadioButton;
    gbOptions: TGroupBox;
    PromptOnReplaceCheckBox: TCheckBox;
    ScopeGroupBox: TGroupBox;
    rgSearchMode: TRadioGroup;
    PanelButtons: TPanel;
    SelectedRadioButton: TRadioButton;
    TextLabel: TLabel;
    ReplaceLabel: TLabel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    WholeWordsOnlyCheckBox: TCheckBox;
    procedure CancelButtonClick(Sender: TObject);
    procedure cbReplaceChange(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure EditFindKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FOnFind: TNotifyEvent;
    FOnReplace: TNotifyEvent;
    fReplaceAllClickedLast: boolean;
    FOptions: TMYSynSearchOptions;
    procedure Find;
    function GetFindText: string;
    function GetOptions: TMySynSearchOptions;
    function GetReplaceText: string;
    procedure LoadHistory;
    procedure Replace;
    procedure SaveHistory;
    procedure SetFindText(AValue: string);
    procedure SetOptions(AValue: TMySynSearchOptions);
    procedure SetReplaceText(AValue: string);

  public
    constructor Create(TheOwner: TComponent); override;
    property FindText: string read GetFindText write SetFindText;
    property ReplaceText: string read GetReplaceText write SetReplaceText;
    property OnReplace: TNotifyEvent read FOnReplace write FOnReplace;
    property OnFind: TNotifyEvent read FOnFind write FOnFind;
    property Options: TMySynSearchOptions read GetOptions write SetOptions;

  end;

ResourceString
  rsFIND = 'Find';
  rsREPLACE = 'Replace';
  lisUEReplaceThisOccurrenceOfWith = 'Replace this occurrence of "%s"%s with "%s"?';

implementation

{$R *.lfm}

procedure TCustomReplaceDialog.SaveHistory;
begin
  ConfigObj.WriteStrings('History','Find', EditFind.Items);
  ConfigObj.WriteStrings('History','Replace', EditReplace.Items);
  ConfigObj.ConfigHolder.Find('History/Options', true).AsString:=SetToString(PTypeInfo(TypeInfo(TmySynSearchOptions)), LongInt(GetOptions), true);

end;

Procedure TCustomReplaceDialog.LoadHistory;
var
  tmp: string;
begin
  ConfigObj.ReadStrings('History','Find', EditFind.Items);
  ConfigObj.ReadStrings('History','Replace', EditReplace.Items);
  try
    tmp := Configobj.ConfigHolder.GetValueDef('History/Options', '');
    FOptions := TMySynSearchOptions(StringToSet(PTypeInfo(TypeInfo(TMySynSearchOptions)), tmp));
  except
    FOptions := [ssoReplace, ssoEntireScope];
  end;

  SetOptions(FOptions);

end;

procedure TCustomReplaceDialog.Replace;
begin
  SaveHistory;
  if Assigned(FOnReplace) then
    FOnReplace(Self);
end;

procedure TCustomReplaceDialog.SetFindText(AValue: string);
begin
  EditFind.Text := AValue;
end;

procedure TCustomReplaceDialog.OKButtonClick(Sender: TObject);
begin
  fReplaceAllClickedLast := false;
  EditFind.AddHistoryItem(EditFind.Text,10,true,true);
  EditReplace.AddHistoryItem(EditReplace.Text,10,true,true);
  Replace;
  ModalResult := mrNone;
end;

procedure TCustomReplaceDialog.CloseButtonClick(Sender: TObject);
begin
  fReplaceAllClickedLast := True;
  EditFind.AddHistoryItem(EditFind.Text,10,true,true);
  EditReplace.AddHistoryItem(EditReplace.Text,10,true,true);
  Replace;
  ModalResult := mrNone;
end;

procedure TCustomReplaceDialog.EditFindKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and not cbReplace.Checked then
    Find;
end;

procedure TCustomReplaceDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveHistory;
end;

procedure TCustomReplaceDialog.FormCreate(Sender: TObject);
begin
  LoadHistory;
end;

procedure TCustomReplaceDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
  SaveHistory;
  Close;
end;

procedure TCustomReplaceDialog.cbReplaceChange(Sender: TObject);
begin
  if cbReplace.Checked then
    Options:=Options + [ssoReplace]
  else
    Options:=Options - [ssoReplace];

end;

procedure TCustomReplaceDialog.Find;
begin
  SaveHistory;
  if Assigned(FOnFind) then
    FOnFind(Self);
end;

function TCustomReplaceDialog.GetFindText: string;
begin
  Result := EditFind.Text;
end;


procedure TCustomReplaceDialog.SetOptions(AValue: TMySynSearchOptions);
begin
  CaseSensitiveCheckBox.Checked := ssoMatchCase in AValue;
  WholeWordsOnlyCheckBox.Checked := ssoWholeWord in AValue;
  if ssoRegExpr in AValue then
     rgSearchMode.ItemIndex := 2
  else
  if ssoExtended in AValue then
     rgSearchMode.ItemIndex := 1
  else
     rgSearchMode.ItemIndex := 0;

  cbReplace.Checked := ssoReplace in AValue;
  PromptOnReplaceCheckBox.Checked := ssoPrompt in AValue;

  if ssoSelectedOnly in AValue then
    SelectedRadioButton.Checked := True
  else
    GlobalRadioButton.Checked := True;

  if ssoBackwards in AValue then
    BackwardRadioButton.Checked := True
  else
    ForwardRadioButton.Checked := True;

  if ssoReplace in AValue then
    BtnPanel.ShowButtons := BtnPanel.ShowButtons + [pbClose]
  else
    BtnPanel.ShowButtons := BtnPanel.ShowButtons - [pbClose];

  EditReplace.Enabled := ssoReplace in AValue;
  PromptOnReplaceCheckBox.Enabled := ssoReplace in AValue;
  fReplaceAllClickedLast := ssoReplaceAll in AValue;

  if ssoReplace in AValue then
  begin
    Caption := rsREPLACE;
    BtnPanel.OKButton.Caption := rsREPLACE;
  end
  else
  begin
    Caption := rsFIND;
    BtnPanel.OKButton.Caption := rsFIND;
  end;

end;

procedure TCustomReplaceDialog.SetReplaceText(AValue: string);
begin
  EditReplace.Text := AValue;
end;

constructor TCustomReplaceDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  EditFind.Text := '';
  EditReplace.Text := '';
  BtnPanel.CloseButton.Kind := bkCustom;
  BtnPanel.OkButton.Kind := bkCustom;
end;

function TCustomReplaceDialog.GetOptions: TMySynSearchOptions;
begin
  Result := [];
  if CaseSensitiveCheckBox.Checked then
    Include(Result, ssoMatchCase);
  if WholeWordsOnlyCheckBox.Checked then
    Include(Result, ssoWholeWord);

  case rgSearchMode.ItemIndex of
  0:;
  1: Include(Result, ssoExtended);
  2: Include(Result, ssoRegExpr);
  end;

  if PromptOnReplaceCheckBox.Checked then
    Include(Result, ssoPrompt);

  if SelectedRadioButton.Checked then
    include(Result, ssoSelectedOnly);
  if BackwardRadioButton.Checked then
    include(Result, ssoBackwards);
  if pbClose in BtnPanel.ShowButtons then
    include(Result, ssoReplace);
  if fReplaceAllClickedLast then
    include(Result, ssoReplaceAll);
end;

function TCustomReplaceDialog.GetReplaceText: string;
begin
  Result := EditReplace.Text;
end;


end.
