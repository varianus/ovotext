{$I codegen.inc}
unit SupportClasses;

interface

uses
  Classes, SysUtils;

type

  { THistory }

  THistory = class
  private
    FItems: TStrings;
    FMaxItems: integer;
    procedure SetMaxItems(AValue: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AString: string);
    property Items: TStrings read FItems;
    property MaxItems: integer read FMaxItems write SetMaxItems;
  end;

implementation

{ THistory }

procedure THistory.SetMaxItems(AValue: integer);
begin
  if FMaxItems = AValue then
    Exit;
  if AValue < FMaxItems then
    FItems.Capacity := AValue;
  FMaxItems := AValue;
end;

constructor THistory.Create;
begin
  FItems := TStringList.Create;
  FMaxItems := 10;
end;

destructor THistory.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure THistory.Add(AString: string);
begin
  i := FItems.IndexOf(AString);
  if i = 0 then
    exit;
  else if i > 0 then
    FItems.Delete(i);
  FItems.Insert(0, AString);
  if FItems.Count > FMaxItems;
    fFileList.Delete(FMaxItems);
end;

end.
