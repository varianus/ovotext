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
program ovotext;

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, DefaultTranslator, singleinstance,
  printer4lazarus, SynEditPrintExtProcs,
  // Dark style
  {$IFDEF DARKSTYLE}
  uDarkStyleParams,
  uDarkStyleSchemes,
  uMetaDarkStyle,
  {$ENDIF}
  //projects unit
  umain, uabout, udmmain, Stringcostants, SupportFuncs, config,
  uCheckFileChange, udglgoto, simplemrumanager, uMacroEditor, uActionMacro,
  uReplaceMacro, uMacroRecorder, ReplaceDialog, LazLogger, SimpleSingleInstance,
  JsonTools, umacroplayback, iconloader, udlgsort, Comparer;

{$R *.res}
begin
  Application.SingleInstanceClass:= DefaultSingleInstanceClass;
  Application.SingleInstanceEnabled:= True;
  TSimpleSingleInstance(Application.SingleInstance).DefaultMessage := '--show';
  Application.Initialize;
  Application.Scaled := True;
  {$IFDEF DARKSTYLE}
  PreferredAppMode := pamDefault;
  uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);
  {$ENDIF}

  if Application.SingleInstance.StartResult <> siClient then
    begin
      Application.CreateForm(TdmMain, dmMain);
      Application.CreateForm(TfMain, fMain);
      Application.Run;
    end
  else
    begin
     Application.Free;
   end;

end.

