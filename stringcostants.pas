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
unit Stringcostants;

interface

uses
  LCLVersion;

const
  DisplayAppName = 'OvoText';
  AppVersion = {$i version.inc};
  BuildDate = {$I %DATE%};
  lazVersion  = lcl_version;         // Lazarus version (major.minor.micro)
  fpcVersion  = {$I %FPCVERSION%};   // FPC version (major.minor.micro)
  TargetCPU   = {$I %FPCTARGETCPU%}; // Target CPU of FPC
  TargetOS    = {$I %FPCTARGETOS%};  // Target Operating System of FPC
  AppName = 'ovotext';

resourcestring
  RSNormalText = 'Normal text file';
  RSAllFile = 'All files';
  RSError = 'Error';
  RSReload = 'Reload';
  RSNewFile = '<new   %d>';
  RSStatusBarPos = 'Line: %d  Col:%d';
  RSStatusBarSel = 'Sel: %d';
  RSStatusBarInsMode = 'INS';
  RSStatusBarOvrMode = 'OVR';
  RSAdministrativeRights = 'Warning, you are using the root account, you may harm your system!';
  //-- Files
  RSSaveChanges = 'Save changes to'+LineEnding+
                  ' "%s"?';
  RSCannotSave = 'Can not save changes to'+LineEnding+
                 ' "%s" '+LineEnding+
                 ' Error: %d - %s';

  RSCannotOpen = 'Cannot open file '+LineEnding+
                 ' "%s" '+LineEnding+
                 ' Error: %d - %s';

  RSAskFileCreation = '"%s"'+LineEnding+
                      'does not exists. Do you want to create it?';
  RSCannotCreate = 'Can not create'+LineEnding+
                 ' "%s"';
  RSReloadSimple = '%s' +LineEnding+LineEnding+
                   'This file has been modified by another application.'+LineEnding+
                   'Do you want to reload it?';

  RSReloadModified = '%s' +LineEnding+LineEnding+
                     'This file has been modified by another application.'+LineEnding+
                     'Do you want to reload it and lose changes?';

  RSReloadFile     = '%s' +LineEnding+LineEnding+
                     'This file has been modified.'+LineEnding+
                     'Do you want to reload it and lose changes?';

  RSKeepDeleted = '%s' +LineEnding+LineEnding+
                     'This file has been deleted by another application.'+LineEnding+
                     'Do you want to keep it in the editor?';

  //--
  RSTextNotFound = 'Text not found:'+LineEnding+'"%s"';

  RSMacro        = 'Macro';
  RSMacroDefaultName = 'Current recorded macro';
  RSMacroSaving = 'Select a name for macro';
  RSMacroNewName = 'Select a new name for macro';

  RSMacroDelete  = 'Delete macro "%s" ?';



implementation

end.
