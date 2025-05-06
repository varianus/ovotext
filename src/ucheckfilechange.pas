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
  Classes, SysUtils, Forms, Generics.Collections, syncobjs, platformpath;

type

  TFileInfo = record
    Exists: boolean;
    Time: longint;
    Size: int64;
  end;

  TFWStateChange = (fwscNone, fwscDeleted, fwscModified, fwscRenamed);

  TFWStateEvent = procedure(Sender: TObject; FileName: TFileName; Data: Pointer; State: TFWStateChange) of object;

  TFWStrategy = (fwsOnDemand, fwsRealTime);
  TWatcherThread = class;

  { TFileWatcher }
  TFileWatch = class
  private
    FFileName: string;
    FileInfo: TFileInfo;
    FData: Pointer;
    FStrategy: TFWStrategy;
    function GetFileInfo(const FileName: string): TFileInfo;
  public
    property Data: Pointer read FData;
    constructor Create(AFileName: TFilename; Strategy: TFWStrategy; AData: Pointer = nil);
    function CheckFile: TFWStateChange;
    procedure Reset;
  end;

  TWatchList = specialize TFastObjectHashMap<string, TFileWatch>;

  TFileWatcher = class
  private
    procedure EnsureMonitor;
  protected
    WatchList: TWatchList;
    FOnFileStateChange: TFWStateEvent;
    Monitor: TWatcherThread;
    procedure SetOnFileStateChange(AValue: TFWStateEvent);
  public
    procedure AddFile(const FileName: TFileName; Strategy: TFWStrategy; Data: Pointer);
    procedure RemoveFile(const FileName: TFileName);
    procedure Update(const FileName: TFileName);
    function GetWatch(const FileName: TFileName): TFileWatch;
    procedure ChangeStrategy(const FileName: TFileName; Strategy: TFWStrategy);

    procedure CheckFiles;
    property OnFileStateChange: TFWStateEvent read FOnFileStateChange write SetOnFileStateChange;

    constructor Create;
    destructor Destroy; override;

  end;

type

  { TWatcherThread }
  TWatcherThread = class(TThread)
  type
    TPaths = specialize TFastObjectHashMap<string, TPlatformPath>;
    REventData = record
      Filename: string;
      NewName: string;
      Event: TFWStateChange;
      Data: pointer;
    end;


  private
    procedure DoWatcherEvent;
  protected
    lock: TCriticalSection;
    Paths: TPaths;
    FCurrentEventData: REventData;
    FMasterList: TFileWatcher;
    function Init: boolean; virtual; abstract;
    procedure Cleanup; virtual; abstract;
    procedure StartMonitoringPath(aPath: string; Data: TPlatformPath); virtual; abstract;
    procedure StopMonitoringPath(aPath: string; Data: TPlatformPath); virtual; abstract;
    procedure DoMonitor; virtual; abstract;
    procedure TriggerTerminateEvent; virtual; abstract;
  public
    procedure Execute; override;
    function AddWatch(aWatch: TFileWatch): integer;
    function RemoveWatch(aWatch: TFileWatch): integer;
    procedure SyncDoWatcherEvent;
    constructor Create(MasterList: TFileWatcher);
    destructor Destroy; override;
    procedure Terminate;

  end;

implementation

uses monitoringthread;

  { TWatcherThread }

procedure TWatcherThread.Execute;
begin
  DoMonitor;
end;

function TWatcherThread.AddWatch(aWatch: TFileWatch): integer;
var
  aPath: string;
  Data: TPlatformPath;
begin
  lock.Acquire;
  try

    aPath := ExtractFilePath(aWatch.FFileName);
    if Paths.TryGetValue(aPath, Data) then
    begin
      Data.RefCount := Data.RefCount + 1;
      exit;
    end;

    Data      := TPlatformPath.Create;
    Data.Path := aPath;
    Paths.Add(aPath, Data);
    StartMonitoringPath(aPath, Data);
  finally
    lock.Release;
  end;

end;

function TWatcherThread.RemoveWatch(aWatch: TFileWatch): integer;
var
  aPath: string;
  Data: TPlatformPath;
begin
  lock.Acquire;
  try
    aPath := ExtractFilePath(aWatch.FFileName);
    if not Paths.TryGetValue(aPath, Data) then
      exit;

    if Data.RefCount > 1 then
      Data.RefCount := Data.RefCount - 1
    else
    begin
      StopMonitoringPath(aPath, Data);
      Paths.Remove(aPath);
      if Paths.Count = 0 then
      begin
        Terminate;
        TriggerTerminateEvent;
      end;
    end;
  finally
    lock.Release;
  end;

end;

procedure TWatcherThread.SyncDoWatcherEvent;
begin
  if not Application.Terminated then
    Synchronize(@DoWatcherEvent);

end;

procedure TWatcherThread.DoWatcherEvent;
begin
  FMasterList.OnFileStateChange(FMasterList, FCurrentEventData.Filename, FCurrentEventData.Data, FCurrentEventData.Event);
end;

constructor TWatcherThread.Create(MasterList: TFileWatcher);
begin
  FMasterList := MasterList;
  Lock  := TCriticalSection.Create;
  Paths := TPaths.Create([doOwnsValues]);
  Init;
  inherited Create(False);
end;

destructor TWatcherThread.Destroy;
begin
  Cleanup;
  lock.Free;
  Paths.Free;
  inherited Destroy;
end;

procedure TWatcherThread.Terminate;
begin
  inherited Terminate;
  TriggerTerminateEvent;

end;

{ TFileWatch }

function TFileWatch.GetFileInfo(const FileName: string): TFileInfo;
var
  Info: TSearchRec;
  A: integer;
begin
  A := 0;
  Result.Exists := FindFirst(FileName, A, Info) = 0;
  if Result.Exists then
  begin
    Result.Time := Info.Time;
    Result.Size := Info.Size;
    FindClose(Info);
  end;
end;

constructor TFileWatch.Create(AFileName: TFilename; Strategy: TFWStrategy;
  AData: Pointer);
begin
  FFileName := AFileName;
  FData     := AData;
  FStrategy := Strategy;
  if Strategy = fwsOnDemand then
    Reset;
end;

function TFileWatch.CheckFile: TFWStateChange;
var
  wFileInfo: TFileInfo;
begin
  Result    := fwscNone;
  wFileInfo := GetFileInfo(FFileName);

  try
    if not wFileInfo.Exists then
    begin
      if FileInfo.Exists then
      begin
        Result := fwscDeleted;
        exit;
      end;
    end
    else
    begin
      if not FileInfo.Exists then
      begin
        Result := fwscModified;
        exit;
      end;
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
  if FOnFileStateChange = AValue then Exit;
  FOnFileStateChange := AValue;
end;

procedure TFileWatcher.AddFile(const FileName: TFileName;
  Strategy: TFWStrategy; Data: Pointer);
var
  tmpFW: TFileWatch;
begin
  if WatchList.ContainsKey(FileName) then
    exit;

  tmpFW := TFileWatch.Create(FileName, Strategy, Data);
  WatchList.Add(FileName, tmpFW);

end;

procedure TFileWatcher.RemoveFile(const FileName: TFileName);
var
  Data: TFileWatch;
begin

  if WatchList.TryGetValue(FileName, Data) then
  begin
    if Data.FStrategy = fwsRealTime then
      Monitor.RemoveWatch(Data);
    WatchList.Remove(FileName);
  end;

end;

procedure TFileWatcher.Update(const FileName: TFileName);
var
  Data: TFileWatch;
begin

  if WatchList.TryGetValue(FileName, Data) then
    Data.Reset;
end;

function TFileWatcher.GetWatch(const FileName: TFileName): TFileWatch;
begin
  if not WatchList.TryGetValue(FileName, Result) then
    Result := nil;
end;

procedure TFileWatcher.ChangeStrategy(const FileName: TFileName;
  Strategy: TFWStrategy);
var
  Data: TFileWatch;
begin

  if WatchList.TryGetValue(FileName, Data) then
  begin
    if Data.FStrategy = Strategy then
      exit;
    case Strategy of
      fwsOnDemand:
      begin
        Monitor.RemoveWatch(Data);
      end;
      fwsRealTime:
      begin
        EnsureMonitor;
        Monitor.AddWatch(Data);
      end;
    end;

    Data.FStrategy := Strategy;
  end;
end;

procedure TFileWatcher.EnsureMonitor;
begin
  if not Assigned(Monitor) then
    Monitor := TPlatformMonitoring.Create(Self);

end;

procedure TFileWatcher.CheckFiles;
var
  fState: TFWStateChange;
  i: integer;
  Data: specialize TPair<string, TFileWatch>;
begin
  for Data in WatchList do
  begin
    if Data.Value.FStrategy = fwsOnDemand then
    begin
      fState := Data.Value.CheckFile;
      if (fState <> fwscNone) and Assigned(FOnFileStateChange) then
        FOnFileStateChange(Self, Data.Key, Data.Value.Data, fState);
    end;
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
