unit monitoringthread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uCheckFileChange, Windows, JwaWinNT, JwaWinBase, platformpath;

type

  { TPlatformMonitoring }

  TPlatformMonitoring = class(TWatcherThread)
  private
    FCompletionPort: HANDLE;
    procedure ReadChanges(const DirectoryHandle: THandle; var Overlapped: TOverlapped; Buffer: pbyte);
  protected
    function Init: boolean; override;
    procedure Cleanup; override;
    procedure TriggerTerminateEvent; override;
    procedure DoMonitor; override;
    procedure StartMonitoringPath(aPath: string; Data: TPlatformPath); override;
    procedure StopMonitoringPath(aPath: string; Data: TPlatformPath); override;

  end;

implementation


{ TPlatformMonitoring }

function TPlatformMonitoring.Init: boolean;
begin
  FCompletionPort := CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0, 0);
end;

procedure TPlatformMonitoring.Cleanup;
begin
  CloseHandle(FCompletionPort);
  FCompletionPort := 0;
end;

procedure TerminateProc(dwParam: ULONG_PTR); stdcall;
begin
end;

procedure TPlatformMonitoring.TriggerTerminateEvent;
begin
  PostQueuedCompletionStatus(fCompletionPort, 0, 0, Nil);
end;

procedure TPlatformMonitoring.DoMonitor;
var
  BytesTransferred: DWORD;
  CompletionKey: nativeuint;
  aOverlapped: POverlapped;
  Buffer: pbyte;
  NotifyInfo: PFILE_NOTIFY_INFORMATION;
  Offset: DWORD;
  aPath: TPlatformPath;
  pData: TFileWatch;
  FileName: string;
begin
  while not Terminated do
    if GetQueuedCompletionStatus(FCompletionPort, BytesTransferred, CompletionKey, aOverlapped, INFINITE) then
    begin
      // Retrieve the buffer from the OVERLAPPED structure
      IF BytesTransferred = 0 THEN
        Continue;
      aPath := TPlatformPath(CompletionKey);
      Buffer := pbyte(APath.Buffer);
      NotifyInfo := PFILE_NOTIFY_INFORMATION(Buffer);

      // Process notifications
      repeat
        // Extract the file name
        with FCurrentEventData do
        begin
          SetString(FileName, NotifyInfo^.FileName, NotifyInfo^.FileNameLength div SizeOf(widechar));

          pData := FMasterList.GetWatch(IncludeTrailingPathDelimiter(aPath.path) + Filename);
          if Assigned(pData) then
          begin
            Data := pData.Data;
            case NotifyInfo^.Action of
              FILE_ACTION_MODIFIED: Event := fwscModified;
              FILE_ACTION_REMOVED: Event := fwscDeleted;
              FILE_ACTION_RENAMED_OLD_NAME: Event := fwscRenamed;
              else
                Event := fwscNone
            end;
            SyncDoWatcherEvent;
          end;

          // Move to the next notification
          Offset := NotifyInfo^.NextEntryOffset;
          if Offset = 0 then
            Break;
          NotifyInfo := PFILE_NOTIFY_INFORMATION(pbyte(NotifyInfo) + Offset);

        end;

      until False;

      // Reissue ReadDirectoryChangesW
      ReadChanges(aPath.Handle, aPath.fOverlapped, aPath.Buffer);
    end
    else
    begin
      OutputDebugString('GetQueuedCompletionStatus failed');
      Terminate;
    end;
end;

procedure TPlatformMonitoring.StartMonitoringPath(aPath: string; Data: TPlatformPath);
begin

  Data.Handle := CreateFileW(pwidechar(UTF8Decode('\\?\' + Data.Path)),
    FILE_LIST_DIRECTORY,
    FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
    nil,
    OPEN_EXISTING,
    FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED,
    0);

  if Data.Handle = INVALID_HANDLE_VALUE then
    Data.Handle := feInvalidHandle
  else
  begin
    if CreateIoCompletionPort(Data.Handle, FCompletionPort, nativeuint(Data), 0) = 0 then
    begin
      CloseHandle(Data.Handle);
      raise Exception.CreateFmt('Failed to associate directory with IOCP: %s', [Data.Path]);
    end;
    ZeroMemory(@(Data.fOverlapped), SizeOf(TOverlapped));
    ReadChanges(Data.Handle, Data.fOverlapped, Data.Buffer);
  end;

end;

procedure TPlatformMonitoring.ReadChanges(const DirectoryHandle: THandle; var Overlapped: TOverlapped; Buffer: pbyte);
begin
  ZeroMemory(@Overlapped, SizeOf(TOverlapped));
  Overlapped.hEvent := CreateEvent(nil, True, False, nil);

  if not ReadDirectoryChangesW(
    DirectoryHandle,
    Buffer,
    4096,
    True, // Include subdirectories
    FILE_NOTIFY_CHANGE_FILE_NAME or
    FILE_NOTIFY_CHANGE_DIR_NAME or
    FILE_NOTIFY_CHANGE_ATTRIBUTES or
    FILE_NOTIFY_CHANGE_SIZE or
    FILE_NOTIFY_CHANGE_LAST_WRITE,
    nil,
    @Overlapped,
    nil
    ) then
    raise Exception.Create('Failed to issue ReadDirectoryChangesW '+ SysErrorMessage(GetLastError));
end;

procedure TPlatformMonitoring.StopMonitoringPath(aPath: string; Data: TPlatformPath);
var
  tmpHandle: THANDLE;
begin
  if Data.Handle <> feInvalidHandle then
  begin
    tmpHandle := Data.Handle;
    Data.Handle := feInvalidHandle;
    CloseHandle(tmpHandle);
    CloseHandle(Data.fOverlapped.hEvent);
  end;

end;


end.
