unit uCheckFileChange;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  TFWStateChange = (fwscNone, fwscDeleted, fwscModified);

  TFWStateEvent = procedure (Sender: TObject; FileName :TFileName; Data:Pointer; State:TFWStateChange) of object;

  { TFileWatcher }
  TFileWatch = class
  private
    FFileName: TFileName;
    FFileAge: TDateTime;
    FDeleted: boolean;
    FData:Pointer;
  public
    constructor Create(FileName: TFilename; Data:Pointer = nil);
    function CheckFile: TFWStateChange;
    procedure Reset;
  end;

  TWatchList = specialize TFPGMap<String,TFileWatch>;

  TFileWatcher = class
  private
    WatchList: TWatchList;
    FOnFileStateChange: TFWStateEvent;
    destructor Destroy;
    procedure SetOnFileStateChange(AValue: TFWStateEvent);
  public
    Procedure AddFile(FileName:TFileName; Data:Pointer);
    procedure CheckFiles;
    Property  OnFileStateChange: TFWStateEvent read FOnFileStateChange write SetOnFileStateChange;

    constructor Create;

  end;



implementation

{ TFileWatch }

constructor TFileWatch.Create(FileName: TFilename; Data:Pointer= nil);
begin
  FFileName := FileName;
  FData:=Data;
  Reset;
end;

function TFileWatch.CheckFile: TFWStateChange;
var
  wFileAge: TDateTime;
  wDeleted: boolean;
begin
  Result := fwscNone;
  wDeleted:= not FileAge(FFileName, wFileAge);

  if wDeleted then
    begin
      if not FDeleted then
        begin
          result := fwscDeleted;
          FDeleted:= wDeleted;
          exit;
        end
    end
  else
    begin
      if FDeleted then
        begin
          result := fwscDeleted;
          FDeleted:= wDeleted;
          exit;
        end
    end;

 if wFileAge <> FFileAge then
   begin
     Result := fwscModified;
     FFileAge:=wFileAge;
   end;

end;

procedure TFileWatch.Reset;
begin
  FFileAge := 0;
  FDeleted := not FileAge(FFileName, FFileAge);
end;

{ TFileWatcher }

procedure TFileWatcher.SetOnFileStateChange(AValue: TFWStateEvent);
begin
  if FOnFileStateChange=AValue then Exit;
  FOnFileStateChange:=AValue;
end;

procedure TFileWatcher.AddFile(FileName: TFileName; Data: Pointer);
var
  tmpFW: TFileWatch;
begin
  tmpFW := TFileWatch.Create(FileName, Data);
  WatchList.Add(FileName, tmpFW);

end;

procedure TFileWatcher.CheckFiles;
var
  fState: TFWStateChange;
  i: integer;
begin
  for i := 0 to WatchList.Count -1 do
    begin
       fState := WatchList.Data[i].CheckFile;
       if (fState <> fwscNone) and Assigned(FOnFileStateChange) then
         FOnFileStateChange(Self, WatchList.Keys[I], WatchList.Data[i].FData, fState);

    end;
end;

constructor TFileWatcher.Create;
begin
  WatchList := TWatchList.Create;
end;

Destructor TFileWatcher.Destroy;
var
  i:integer;
begin
  For i := WatchList.Count -1 downto 0 do
    begin
       WatchList.Data[i].Free;
       WatchList.Remove(WatchList.keys[i]);
    end;
  WatchList.Free;
end;


end.

