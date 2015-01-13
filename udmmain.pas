unit udmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Dialogs,
  SupportFuncs, SynEditHighlighter, SynExportHTML, SynHighlighterPas,
  SynHighlighterCpp, SynHighlighterJava, SynHighlighterPerl, SynHighlighterHTML,
  SynHighlighterXML, SynHighlighterLFM, synhighlighterunixshellscript,
  SynHighlighterCss, SynHighlighterPHP, SynHighlighterTeX, SynHighlighterSQL,
  SynHighlighterPython, SynHighlighterVB, SynHighlighterBat, SynHighlighterIni,
  SynHighlighterPo, fgl;

type

  { TdmMain }
  THiglighterList = specialize TFPGMap<string, TSynCustomHighlighter>;


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
    SynPerlSyn1: TSynPerlSyn;
    SynPHPSyn1: TSynPHPSyn;
    SynPoSyn1: TSynPoSyn;
    SynPythonSyn1: TSynPythonSyn;
    SynSQLSyn1: TSynSQLSyn;
    SynTeXSyn1: TSynTeXSyn;
    SynUNIXShellScriptSyn1: TSynUNIXShellScriptSyn;
    SynVBSyn1: TSynVBSyn;
    SynXMLSyn1: TSynXMLSyn;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    fHighlighters: THiglighterList;
    procedure LoadHighlighters;
  public
    function getHighLighter(Extension:string): TSynCustomHighlighter;

  end;

var
  dmMain: TdmMain;

implementation

{$R *.lfm}

{ TdmMain }

procedure TdmMain.DataModuleCreate(Sender: TObject);
begin
  fHighlighters := THiglighterList.Create;
  LoadHighlighters;
end;

procedure TdmMain.DataModuleDestroy(Sender: TObject);
begin
  fHighlighters.Free;
end;

procedure TdmMain.LoadHighlighters;
var
  i, j: integer;
  Highlighter: TSynCustomHighlighter;
  filter: string;
  stList:TStringList;
begin

  fHighlighters.clear;
  for i := ComponentCount - 1 downto 0 do
    begin
      if not (Components[i] is TSynCustomHighlighter) then
        continue;
      Highlighter := Components[i] as TSynCustomHighlighter;
      Highlighter := TSynCustomHighlighter(Components[i]);
      Filter := LowerCase(Highlighter.DefaultFilter);
      j := Pos('|', Filter);
      if j > 0 then
        begin
          Delete(Filter, 1, j);
          stList:=TStringList.create;
          StrToStrings(filter,';', stList ,false);
          for j := 0 to  stList.Count -1 do
            fHighlighters.Add(ExtractFileExt(stList[j]),Highlighter);
          stList.Free;
        end;
      end;

end;

function TdmMain.getHighLighter(Extension: string): TSynCustomHighlighter;
var
 tmp: integer;
begin
  tmp := fHighlighters.IndexOf(Extension);
  if tmp > -1 then
    Result := fHighlighters.Data[tmp]
  else
    Result := nil;
end;

end.

