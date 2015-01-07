unit udmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Dialogs,
  SynExportHTML, SynHighlighterPas, SynHighlighterCpp, SynHighlighterJava,
  SynHighlighterPerl, SynHighlighterHTML, SynHighlighterXML, SynHighlighterLFM,
  synhighlighterunixshellscript, SynHighlighterCss, SynHighlighterPHP,
  SynHighlighterTeX, SynHighlighterSQL, SynHighlighterPython, SynHighlighterVB,
  SynHighlighterBat, SynHighlighterIni, SynHighlighterPo, SynPluginSyncroEdit;

type

  { TdmMain }

  TdmMain = class(TDataModule)
    SynBatSyn1: TSynBatSyn;
    SynCppSyn1: TSynCppSyn;
    SynCssSyn1: TSynCssSyn;
    SynExporterHTML: TSynExporterHTML;
    SynFreePascalSyn1: TSynFreePascalSyn;
    SynHTMLSyn1: TSynHTMLSyn;
    SynIniSyn1: TSynIniSyn;
    SynJavaSyn1: TSynJavaSyn;
    SynLFMSyn1: TSynLFMSyn;
    SynPasSyn1: TSynPasSyn;
    SynPerlSyn1: TSynPerlSyn;
    SynPHPSyn1: TSynPHPSyn;
    SynPoSyn1: TSynPoSyn;
    SynPythonSyn1: TSynPythonSyn;
    SynSQLSyn1: TSynSQLSyn;
    SynTeXSyn1: TSynTeXSyn;
    SynUNIXShellScriptSyn1: TSynUNIXShellScriptSyn;
    SynVBSyn1: TSynVBSyn;
    SynXMLSyn1: TSynXMLSyn;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  dmMain: TdmMain;

implementation

{$R *.lfm}

end.

