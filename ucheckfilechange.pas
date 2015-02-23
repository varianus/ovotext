unit uCheckFileChange;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ueditor, fgl;

type

  TFWStateChange = (fwscNone, fwscDeleted, fwscModified);

  { TFileWatcher }
  TFileWatch = class
  private
    FFileName: TFileName;
    FFileAge: TDateTime;
    FDeleted: boolean;
  public
    constructor Create(FileName: TFilename);
    function CheckFile: TFWStateChange;
    procedure Reset;
  end;

  TCustomWatcher = specialize TFPGObjectList<TFileWatch>;

  TFileWatcher = class(TCustomWatcher)
  public
    procedure CheckFiles;
  end;



implementation

{ TFileWatch }

constructor TFileWatch.Create(FileName: TFilename);
begin
  FFileName := FileName;
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

procedure TFileWatcher.CheckFiles;
begin
  for i := 0 to Count -1 do
    begin
       Items[i].CheckFile;
    end;
end;

end.

