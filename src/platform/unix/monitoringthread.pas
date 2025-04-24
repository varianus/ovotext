unit monitoringthread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uCheckFileChange, BaseUnix, Linux;

type

  { TPlatformMonitoring }

  TPlatformMonitoring = class(TWatcherThread)
  private
    FNotifyHandle: THandle;
    FEventPipe: TFilDes;
  protected
    function Init: boolean; override;
    procedure Cleanup; override;
    procedure TriggerTeminateEvent; override;
    procedure DoMonitor; override;
    procedure StartMonitoringPath(aPath: string; Data: TPath); override;
    procedure StopMonitoringPath(aPath: string; Data: TPath); override;

  end;

implementation

{ TPlatformMonitoring }

function TPlatformMonitoring.Init: boolean;
begin

  FNotifyHandle := inotify_init();
  Result := FNotifyHandle >= 0;
  if not Result then
    exit;
  // create pipe for user triggered fake event
  FEventPipe[0] := -1;
  FEventPipe[1] := -1;
  if FpPipe(FEventPipe) = 0 then
  begin
    FpFcntl(FEventPipe[0], F_SETFD, FpFcntl(FEventPipe[0], F_GETFD) or 1);
    FpFcntl(FEventPipe[1], F_SETFD, FpFcntl(FEventPipe[1], F_GETFD) or 1);
    FpFcntl(FEventPipe[0], F_SetFl, FpFcntl(FEventPipe[0], F_GetFl) or O_NONBLOCK);
    FpFcntl(FEventPipe[1], F_SetFl, FpFcntl(FEventPipe[1], F_GetFl) or O_NONBLOCK);
  end;
end;

procedure TPlatformMonitoring.Cleanup;
begin
  if FEventPipe[0] <> -1 then
  begin
    FileClose(FEventPipe[0]);
    FEventPipe[0] := -1;
  end;
  if FEventPipe[1] <> -1 then
  begin
    FileClose(FEventPipe[1]);
    FEventPipe[1] := -1;
  end;
  if FNotifyHandle <> feInvalidHandle then
  begin
    FileClose(FNotifyHandle);
    FNotifyHandle := feInvalidHandle;
  end;

end;

procedure TPlatformMonitoring.TriggerTeminateEvent;
var
  buf: char;
begin

  if Self.FNotifyHandle <> feInvalidHandle then
  begin
    buf := #0;
    FileWrite(FEventPipe[1], buf, 1);
  end;

end;

procedure TPlatformMonitoring.DoMonitor;
const
  // Buffer size passed to read() must be at least the size of the first event
  // to be read from the file descriptor, otherwise Invalid Parameter is returned.
  // Event record size is variable, we use maximum possible for a single event.
  // Usually it is big enough so that multiple events can be read with single read().
  // The 'name' field is always padded up to multiple of 16 bytes with NULLs.
  buffer_size = (sizeof(inotify_event) + MAX_PATH) * 8;
var
  bytes_to_parse, p, k, i: integer;
  buf: pchar = nil;
  ev, v: pinotify_event;
  fds: array[0..1] of tpollfd;
  pData: pointer;
  ret: cint;
  Data: TPath;
begin
  if (FNotifyHandle = feInvalidHandle) or
    (FEventPipe[0] = -1) or
    (FEventPipe[1] = -1) then
    Exit;

  try
    buf := GetMem(buffer_size);

    // set file descriptors
    fds[0].fd     := FEventPipe[0];
    fds[0].events := POLLIN;

    fds[1].fd     := FNotifyHandle;
    fds[1].events := POLLIN;

    while not Terminated do
    begin
      // wait for events
      repeat
        ret := fpPoll(@fds[0], Length(fds), -1);
      until (ret <> -1) or (fpGetErrNo <> ESysEINTR);

      if ret = -1 then
      begin
        Exit;
      end; { if }

      if (fds[0].revents and POLLIN <> 0) then
      begin
        // clear pipe
        while FileRead(FEventPipe[0], buf^, 1) <> -1 do ;
      end; { if }

      if (fds[1].revents and POLLIN = 0) then // inotify handle didn't change, so user triggered
        Continue;

      // Read events.
      bytes_to_parse := FileRead(FNotifyHandle, buf^, buffer_size);
      if bytes_to_parse = -1 then
      begin
        Continue;
      end; { if }

      // parse events and print them
      p := 0;
      while p < bytes_to_parse do
      begin
        ev := pinotify_event(buf + p);

        for Data in Paths.Values do
        begin
          if ev^.wd = Data.Handle then
          begin
            with FCurrentEventData do
            begin
              FileName := StrPas(PChar(@ev^.Name));
              Data := FMasterList.GetWatch(Filename);
              if not Assigned(pData) then
                Continue;

              // IN_MOVED_FROM is converted to FileDelete.
              // IN_MOVED_TO is converted to FileCreate.
              // There is no guarantee we will receive as sequence of
              // IN_MOVED_FROM, IN_MOVED_TO as the events are only sent
              // if the source and destination directories respectively
              // are being watched.

              if (ev^.mask and (IN_IGNORED or
                IN_Q_OVERFLOW)) <> 0 then
              begin
                // Ignore this event.
                Break;
              end
              else if (ev^.mask and (IN_ACCESS or
                IN_MODIFY or
                IN_ATTRIB or
                IN_CLOSE or
                IN_OPEN or
                IN_CLOSE_WRITE or
                IN_CLOSE_NOWRITE)) <> 0 then
              begin
                Event := fwscModified;
              end
              else if (ev^.mask and IN_DELETE) <> 0 then
              begin
                Event := fwscDeleted;
              end
              else if (ev^.mask and IN_MOVED_FROM) <> 0 then
              begin
                Event := fwscDeleted;
                // Try to find related event
                k := p + sizeof(inotify_event) + ev^.len;
                while (k < bytes_to_parse) do
                begin
                  v := pinotify_event(buf + k);
                  if (v^.mask and IN_MOVED_TO) <> 0 then
                  begin
                    // Same cookie and path
                    if (v^.cookie = ev^.cookie) and (v^.wd = ev^.wd) then
                    begin
                      v^.mask     := IN_IGNORED;
                      Event   := fwscRenamed;
                      NewName := StrPas(PChar(@v^.Name));
                      Break;
                    end;
                  end;
                  k := k + sizeof(inotify_event) + v^.len;
                end;
              end
              else if (ev^.mask and (IN_DELETE_SELF or
                IN_MOVE_SELF)) <> 0 then
              begin
                // Watched file/directory was deleted or moved.
              //  Event := fswSelfDeleted;
              end
              else
              begin
                Event := fwscNone;
              end;
              ;
            end;

            // call event handler
            SyncDoWatcherEvent;

            Break;
          end; { if }
        end; { for }

        p := p + sizeof(inotify_event) + ev^.len;
      end; { while }

    end; { while }

  finally
    if Assigned(buf) then
      FreeMem(buf);
  end; { try - finally }

end;

procedure TPlatformMonitoring.StartMonitoringPath(aPath: string; Data: TPath);
var
  hNotifyFilter: cuint32 = IN_DELETE_SELF or IN_MOVE_SELF;
begin
  hNotifyFilter := hNotifyFilter or IN_CREATE or IN_DELETE or IN_MOVE;
  hNotifyFilter := hNotifyFilter or IN_ATTRIB or IN_MODIFY;

  Data.Handle := inotify_add_watch(FNotifyHandle, PChar(aPath), hNotifyFilter);
  if Data.Handle < 0 then
  begin
    Data.Handle := feInvalidHandle;
  end;
end;

procedure TPlatformMonitoring.StopMonitoringPath(aPath: string; Data: TPath);
begin
  if Data.Handle <> feInvalidHandle then
  begin
    inotify_rm_watch(FNotifyHandle, Data.Handle);
  end;

end;


end.
