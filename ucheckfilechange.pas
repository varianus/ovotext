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
unit uCheckFileChange;

interface

uses
  Classes, SysUtils, Generics.Collections;

type

  TFileInfo = record
    Exists : Boolean;
    Time : Longint;
    Size : Int64;
  end;

  TFWStateChange = (fwscNone, fwscDeleted, fwscModified);

  TFWStateEvent = procedure (Sender: TObject; FileName :TFileName; Data:Pointer; State:TFWStateChange) of object;

  { TFileWatcher }
  TFileWatch = class
  private
    FFileName: String;
    FileInfo: TFileInfo;
    FData:Pointer;
    function GetFileInfo(const FileName: string): TFileInfo;
  public
    constructor Create(FileName: TFilename; Data:Pointer = nil);
    function CheckFile: TFWStateChange;
    procedure Reset;
  end;

  TWatchList = specialize TFastObjectHashMap<String,TFileWatch>;

  TFileWatcher = class
  private
    WatchList: TWatchList;
    FOnFileStateChange: TFWStateEvent;
    procedure SetOnFileStateChange(AValue: TFWStateEvent);
  public
    Procedure AddFile(FileName:TFileName; Data:Pointer);
    Procedure RemoveFile(FileName:TFileName);
    Procedure Update(FileName:TFileName);
    procedure CheckFiles;
    Property  OnFileStateChange: TFWStateEvent read FOnFileStateChange write SetOnFileStateChange;

    constructor Create;
    destructor Destroy; override;

  end;


implementation

{ TFileWatch }

function TFileWatch.GetFileInfo(const FileName: string): TFileInfo;
Var
  Info : TSearchRec;
  A : Integer;

begin
  A:=0;
  Result.Exists := FindFirst(FileName,A,Info)=0;
  if Result.Exists then
    begin
      result.Time:=Info.Time;
      result.Size:=Info.Size;
      FindClose(Info);
    end;
end;

constructor TFileWatch.Create(FileName: TFilename; Data:Pointer= nil);
begin
  FFileName := FileName;
  FData:=Data;
  Reset;
end;

function TFileWatch.CheckFile: TFWStateChange;
var
  wFileInfo : TFileInfo;
begin
  Result := fwscNone;

  wFileInfo:= GetFileInfo(FFileName);

  try
    if not wFileInfo.Exists then
      begin
        if FileInfo.Exists then
          begin
            result := fwscDeleted;
            exit;
          end
      end
    else
      begin
        if not FileInfo.Exists then
          begin
            result := fwscModified;
            exit;
          end
      end;

   if wFileInfo.Exists and
      ((wFileInfo.Size <> FileInfo.Size) or
       (wFileInfo.Time <> FileInfo.Time))
    then
      begin
        Result := fwscModified;
      end;
  finally
    FileInfo := wFileInfo;
  end;

end;

procedure TFileWatch.Reset;
begin
  FileInfo := GetFileInfo(FFileName);
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
  if WatchList.ContainsKey(FileName) then
    exit;

  tmpFW := TFileWatch.Create(FileName, Data);
  WatchList.Add(FileName, tmpFW);

end;

procedure TFileWatcher.RemoveFile(FileName: TFileName);
begin
  WatchList.Remove(FileName);
end;

procedure TFileWatcher.Update(FileName: TFileName);
var
  Data : TFileWatch;
begin

  if WatchList.TryGetValue(FileName, Data) then
    Data.Reset;
end;

procedure TFileWatcher.CheckFiles;
var
  fState: TFWStateChange;
  i: integer;
  Data : specialize TPair<string,TFileWatch>;
begin
  for Data in WatchList do
    begin
      fState := Data.Value.CheckFile;
      if (fState <> fwscNone) and Assigned(FOnFileStateChange) then
        FOnFileStateChange(Self, Data.Key, Data.Value, fState);

    end;
end;

constructor TFileWatcher.Create;
begin
  WatchList := TWatchList.Create;
end;

destructor TFileWatcher.Destroy;
begin
  WatchList.Clear;
  WatchList.Free;
end;


end.

