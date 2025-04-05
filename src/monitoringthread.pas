unit monitoringthread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TMonitoringThread = class(TThread)
  private

  protected
    procedure Execute; override;
  end;

implementation

{ TMonitoringThread }

procedure TMonitoringThread.Execute;
begin
  { Write your thread code here }
end;

end.

