unit platformpath;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Windows, jwawinbase;

type

  { TPlatformPath }

  TPlatformPath = class
    RefCount: integer;
    Path: string;
    fOverlapped: TOverlapped;
    Buffer: pbyte;
    Handle: THandle;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

const
  READDIRECTORYCHANGESW_BUFFERSIZE = 4096;  { TPlatformPath }

constructor TPlatformPath.Create;
begin
  RefCount := 0;
  Handle := feInvalidHandle;
  Buffer := GetMem(READDIRECTORYCHANGESW_BUFFERSIZE);
end;

destructor TPlatformPath.Destroy;
begin
  Freemem(Buffer);
  inherited Destroy;
end;

end.
