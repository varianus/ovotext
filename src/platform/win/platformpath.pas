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
unit platformpath;
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
