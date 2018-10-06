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
unit udmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, Controls,
  SupportFuncs, SynEditHighlighter, SynExportHTML, Graphics, config,
  Stringcostants, SynExportRTF;


type


{Comment#Documentation|Rpl comment|SASM Comment|
Symbol#Brackets|Round Bracket|Square Bracket|Symbol|
Key#Key|Rpl key|SASM Key|SQL keyword|SQL*Plus command|TeX Command|
Reserved word#Reserved word|Reserved word (PL/SQL)|Second reserved word|
Directive#IDE Directive|Include|Preprocessor|Processing Instruction|
Variable#Special variable|
Attribute Name#Element Name|Namespace Attribute Name|
Attribute Value#CDATA|DOCTYPE|Namespace Attribute Value|Val|
Macro#Pragma|
Text#Embedded text|
Section#Asp|CDATA Section|DOCTYPE Section
}


  TdmMain = class(TDataModule)
    imgIcons: TImageList;
    imgBookMark: TImageList;
  private
    fRTFExporter : TSynExporterRTF;
    fHTMLExporter : TSynExporterHTML;
    function GetSynExporterHTML: TSynExporterHTML;
    function GetSynExporterRTF: TSynExporterRTF;
  public
    Property RFTExporter: TSynExporterRTF read GetSynExporterRTF;
    Property HTMLExporter: TSynExporterHTML read GetSynExporterHTML;
  end;

var
  dmMain: TdmMain;


implementation

uses lclproc;

{$R *.lfm}

{ TdmMain }

function TdmMain.GetSynExporterHTML: TSynExporterHTML;
begin
  if not Assigned(fHTMLExporter) then
    fHTMLExporter:= TSynExporterHTML.Create(Self);
  result := fHTMLExporter;
end;

function TdmMain.GetSynExporterRTF: TSynExporterRTF;
begin
  if not Assigned(fRTFExporter) then
    fRTFExporter:= TSynExporterRTF.Create(Self);
  result := fRTFExporter;
end;


end.
