unit Stringcostants;
{$mode objfpc}{$H+}

interface
uses
  LCLVersion;
const
  DisplayAppName = 'OvoPlayer';
  AppVersion = '0.0.1';//{$i version.inc};
  BuildDate = {$I %DATE%};
  lazVersion  = lcl_version;         // Lazarus version (major.minor.micro)
  fpcVersion  = {$I %FPCVERSION%};   // FPC version (major.minor.micro)
  TargetCPU   = {$I %FPCTARGETCPU%}; // Target CPU of FPC
  TargetOS    = {$I %FPCTARGETOS%};  // Target Operating System of FPC


AppName  = 'ovoplayer';

Resourcestring
  RSNewFile = '<new   %d>';
  RSStatusBarPos = 'Line: %d  Col:%d';
  RSStatusBarSel = 'Sel: %d ';
  RSStatusBarInsMode = 'INS ';
  RSStatusBarOvrMode = 'OVR ';
  RSSaveChanges = 'Save changes to "%s"?';

implementation

end.

