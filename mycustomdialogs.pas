unit mycustomdialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, controls, ExtCtrls, forms, Dialogs, LazLogger;

type

  { TMyReplaceDialog }
  TSearchMode = (smNormal, smExtended, smRegexp);

  TMyReplaceDialog = class(TReplaceDialog)
  private
    rgSearchMode: TRadioGroup;
    FSearchMode: TSearchMode;
    procedure SetSearchMode(AValue: TSearchMode);
  protected
    function CreateForm: TForm; override;
    procedure SetFormValues; override;
    procedure GetFormValues; override;
  public
    property SearchMode: TSearchMode read FSearchMode write SetSearchMode;
  end;

implementation

procedure TMyReplaceDialog.SetSearchMode(AValue: TSearchMode);
begin
  if FSearchMode = AValue then Exit;
  FSearchMode := AValue;

end;

{ TMyReplaceDialog }
function TMyReplaceDialog.CreateForm: TForm;
var
  i: Integer;
begin
  Result := inherited CreateForm;
  rgSearchMode := TRadioGroup.Create(Result);
  with rgSearchMode do
    begin
      AnchorSideLeft.Side := asrBottom;
      Parent := result;
      AnchorSideTop.Side := asrBottom;
      AnchorSideBottom.Side := asrCenter;
      Left := 8;
      Height := 107;
      Top := 197;
      Width := 168;
      AutoFill := True;
      BorderSpacing.Left := 12;
      Caption := 'Search Mode';
      ChildSizing.LeftRightSpacing := 6;
      ChildSizing.TopBottomSpacing := 6;
      ChildSizing.EnlargeHorizontal := crsHomogenousChildResize;
      ChildSizing.EnlargeVertical := crsHomogenousChildResize;
      ChildSizing.ShrinkHorizontal := crsScaleChilds;
      ChildSizing.ShrinkVertical := crsScaleChilds;
      ChildSizing.Layout := cclLeftToRightThenTopToBottom;
      ChildSizing.ControlsPerLine := 1;
      ClientHeight := 87;
      ClientWidth := 164;
      Items.AddStrings(['Normal','Extended','Grep search']);
      TabOrder := 5;
    end;

end;

procedure TMyReplaceDialog.SetFormValues;
begin
  inherited SetFormValues;
  rgSearchMode.ItemIndex:= Integer(FSearchMode);

end;

procedure TMyReplaceDialog.GetFormValues;
begin
  inherited GetFormValues;
  FSearchMode := TSearchMode(rgSearchMode.ItemIndex);
end;

end.

