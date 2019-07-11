unit ReplaceDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, ExtCtrls, ButtonPanel, SynEditTypes;

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
    EntireScopeCheckBox: TCheckBox;
    ForwardRadioButton: TRadioButton;
    GlobalRadioButton: TRadioButton;
    gbOptions: TGroupBox;
    PromptOnReplaceCheckBox: TCheckBox;
    ScopeGroupBox: TGroupBox;
    rgSearchMode: TRadioGroup;
    PanelButtons: TPanel;
    EditFind: TEdit;
    SelectedRadioButton: TRadioButton;
    TextLabel: TLabel;
    EditReplace: TEdit;
    ReplaceLabel: TLabel;
    WholeWordsOnlyCheckBox: TCheckBox;
    procedure CancelButtonClick(Sender: TObject);
    procedure cbReplaceChange(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
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
    procedure Replace;
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

procedure TCustomReplaceDialog.Replace;
begin
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
  Replace;
  ModalResult := mrNone;
end;

procedure TCustomReplaceDialog.CloseButtonClick(Sender: TObject);
begin
  fReplaceAllClickedLast := True;
  Replace;
  ModalResult := mrNone;
end;

procedure TCustomReplaceDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
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
