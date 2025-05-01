unit platformpath;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TPlatformPath }

  TPlatformPath = class
    RefCount: integer;
    Handle: Thandle;
    Path: string;
    constructor Create;
  end;

implementation

{ TPlatformPath }

constructor TPlatformPath.Create;
begin
  RefCount := 0;
  Handle   := feInvalidHandle;
end;

end.
