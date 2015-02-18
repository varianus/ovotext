unit Stringcostants;

{$mode objfpc}{$H+}

interface

uses
  LCLVersion;

const
  DisplayAppName = 'OvoText';
  AppVersion = '0.0.1';//{$i version.inc};
  BuildDate = {$I %DATE%};
  lazVersion  = lcl_version;         // Lazarus version (major.minor.micro)
  fpcVersion  = {$I %FPCVERSION%};   // FPC version (major.minor.micro)
  TargetCPU   = {$I %FPCTARGETCPU%}; // Target CPU of FPC
  TargetOS    = {$I %FPCTARGETOS%};  // Target Operating System of FPC
  AppName = 'ovotext';

resourcestring
  RSError = 'Error';
  RSNewFile = '<new   %d>';
  RSStatusBarPos = 'Line: %d  Col:%d';
  RSStatusBarSel = 'Sel: %d ';
  RSStatusBarInsMode = 'INS ';
  RSStatusBarOvrMode = 'OVR ';
  //-- Files
  RSSaveChanges = 'Save changes to'+LineEnding+
                  ' "%s"?';
  RSCannotSave = 'Can not save changes to'+LineEnding+
                 ' "%s"';
  RSAskFileCreation = '"%s"'+LineEnding+
                      'does not exists. Do you want to create it?';
  RSCannotCreate = 'Can not create'+LineEnding+
                 ' "%s"';
  //--
  RSTextNotFound = 'Text not found:'+LineEnding+'"%s"';

implementation

end.
