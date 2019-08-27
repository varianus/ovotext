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

program ovotext;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, DefaultTranslator,  singleinstance,
  umain, uabout, udmmain, Stringcostants, SupportFuncs, config,
  uCheckFileChange, udglgoto, printer4lazarus, SynEditPrintExtProcs,
  simplemrumanager, uMacroEditor, uActionMacro, uReplaceMacro, uMacroRecorder, ReplaceDialog, LazLogger, SimpleSingleInstance;

{$R *.res}

begin
  DebugLogger.LogName := 'test.txt';
  Application.SingleInstanceClass:= DefaultSingleInstanceClass;
  Application.SingleInstanceEnabled:= True;
  TSimpleSingleInstance(Application.SingleInstance).DefaultMessage := 'show';
  Application.Initialize;
  Application.Scaled:=True;
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

