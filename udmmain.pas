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
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Dialogs,
  SupportFuncs, SynEditHighlighter, SynExportHTML, fgl, Graphics, config,
  Stringcostants, SynExportRTF, SynEditStrConst,  SynEditStrConstExtra,
  // included with Lazarus
  SynHighlighterPas,
  SynHighlighterCpp, SynHighlighterJava, SynHighlighterPerl, SynHighlighterHTML,
  SynHighlighterXML, SynHighlighterLFM, synhighlighterunixshellscript,
  SynHighlighterCss, SynHighlighterPHP, SynHighlighterTeX, SynHighlighterSQL,
  SynHighlighterPython, SynHighlighterVB, SynHighlighterBat, SynHighlighterIni,
  SynHighlighterPo,
  // from other people
  SynHighlighterTclTk, SynHighlighterRuby, SynHighlighterCS,
  SynHighlighterHaskell, SynHighlighterFoxpro, SynHighlighterInno,
  SynHighlighterDml, SynHighlighterCAC, SynHighlighterModelica,
  SynHighlighterVrml97, SynHighlighterHP48, SynHighlighterAWK,
  SynHighlighterProgress, SynHighlighterEiffel, SynHighlighterBaan,
  SynHighlighterDiff, SynHighlighterJScript,  SynHighlighterJSon,
  SynHighlighterM3, SynHighlighterLDraw, SynHighlighterVBScript,
  SynHighlighterSml, SynHighlighterIDL, SynHighlighterCobol, SynHighlighterGWS,
  SynHighlighterAsm, SynHighlighterLua, SynHighlighterFortran,
  SynHighlighterProlog, SynHighlighterRC, SynHighlighterR;

type

  RHighlighter = record
    HLClass: TSynCustomHighlighterClass;
    Filter: string;
    HL: TSynCustomHighlighter;
  end;

const
  HIGHLIGHTERCOUNT = 48;
  ARHighlighter: array [0..HIGHLIGHTERCOUNT - 1] of RHighlighter = (
  (HLClass: TSynAWKSyn; Filter: SYNS_FilterAWK; HL: nil),
  (HLClass: TSynBaanSyn; Filter: SYNS_FilterBaan; HL: nil),
  (HLClass: TSynCppSyn; Filter: SYNS_FilterCPP; HL: nil),
  (HLClass: TSynCACSyn; Filter: SYNS_FilterCAClipper; HL: nil),
  (HLClass: TSynCssSyn; Filter: SYNS_FilterCSS; HL: nil),
  (HLClass: TSynCobolSyn; Filter: SYNS_FilterCOBOL; HL: nil),
  (HLClass: TSynIdlSyn; Filter: SYNS_FilterCORBAIDL; HL: nil),
  (HLClass: TSynCSSyn; Filter: SYNS_FilterCS; HL: nil),
  (HLClass: TSynDiffSyn; Filter: SYNS_FilterDiff; HL: nil),
  (HLClass: TSynEiffelSyn; Filter: SYNS_FilterEiffel; HL: nil),
  (HLClass: TSynFortranSyn; Filter: SYNS_FilterFortran; HL: nil),
  (HLClass: TSynFoxproSyn; Filter: SYNS_FilterFoxpro; HL: nil),
  (HLClass: TSynDmlSyn; Filter: SYNS_FilterGembase; HL: nil),
  (HLClass: TSynGWScriptSyn; Filter: SYNS_FilterGWS; HL: nil),
  (HLClass: TSynHaskellSyn; Filter: SYNS_FilterHaskell; HL: nil),
  (HLClass: TSynHP48Syn; Filter: SYNS_FilterHP48; HL: nil),
  (HLClass: TSynHTMLSyn; Filter: SYNS_FilterHTML; HL: nil),
  (HLClass: TSynIniSyn; Filter: SYNS_FilterINI; HL: nil),
  (HLClass: TSynInnoSyn; Filter: SYNS_FilterInno; HL: nil),
  (HLClass: TSynJavaSyn; Filter: SYNS_FilterJava; HL: nil),
  (HLClass: TSynJScriptSyn; Filter: SYNS_FilterJScript; HL: nil),
  (HLClass: TSynJSONSyn; Filter: SYNS_FilterJSON;    HL: nil),
  (HLClass: TSynLFMSyn; Filter: SYNS_FilterLFM; HL: nil),
  (HLClass: TSynLDRSyn; Filter: SYNS_FilterLDraw; HL: nil),
  (HLClass: TSynLuaSyn; Filter: SYNS_FilterLua; HL: nil),
  (HLClass: TSynModelicaSyn; Filter: SYNS_FilterModelica; HL: nil),
  (HLClass: TSynM3Syn; Filter: SYNS_FilterModula3; HL: nil),
  (HLClass: TSynVBScriptSyn; Filter: SYNS_FilterVBScript; HL: nil),
  (HLClass: TSynBatSyn; Filter: SYNS_FilterBatch; HL: nil),
  (HLClass: TSynPasSyn; Filter: SYNS_FilterPascal; HL: nil),
  (HLClass: TSynPerlSyn; Filter: SYNS_FilterPerl; HL: nil),
  (HLClass: TSynPHPSyn; Filter: SYNS_FilterPHP; HL: nil),
  (HLClass: TSynPoSyn; Filter: SYNS_FilterPo; HL: nil),
  (HLClass: TSynProgressSyn; Filter: SYNS_FilterProgress; HL: nil),
  (HLClass: TSynPrologSyn; Filter: SYNS_FilterProlog;HL: nil),
  (HLClass: TSynPythonSyn; Filter: SYNS_FilterPython; HL: nil),
  (HLClass: TSynRSyn; Filter: SYNS_FilterR;    HL: nil),
  (HLClass: TSynRCSyn; Filter: SYNS_FilterRC; HL: nil),
  (HLClass: TSynRubySyn; Filter: SYNS_FilterRuby; HL: nil),
  (HLClass: TSynSQLSyn; Filter: SYNS_FilterSQL; HL: nil),
  (HLClass: TSynSMLSyn; Filter: SYNS_FilterSML; HL: nil),
  (HLClass: TSynTclTkSyn; Filter: SYNS_FilterTclTk; HL: nil),
  (HLClass: TSynTeXSyn; Filter: SYNS_FilterTclTk; HL: nil),
  (HLClass: TSynUNIXShellScriptSyn; Filter: SYNS_FilterUNIXShellScript; HL: nil),
  (HLClass: TSynVBSyn; Filter: SYNS_FilterVisualBASIC; HL: nil),
  (HLClass: TSynVrml97Syn; Filter:SYNS_FilterVrml97; HL: nil),
  (HLClass: TSynAsmSyn; Filter: SYNS_FilterX86Asm; HL: nil),
  (HLClass: TSynXMLSyn; Filter: SYNS_FilterXML;    HL: nil)
  );

type

  { TdmMain }
  THighLighterList = specialize TFPGMap<string, integer>;


  TdmMain = class(TDataModule)
    imgBookMark: TImageList;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    fHighlighters: THighLighterList;
    fRTFExporter : TSynExporterRTF;
    fHTMLExporter : TSynExporterHTML;
    procedure FontAttribToAttribute(Attribute: TSynHighlighterAttributes;
      Attrib: TFontAttributes);
    function GetSynExporterHTML: TSynExporterHTML;
    function GetSynExporterRTF: TSynExporterRTF;
    procedure LoadHighlighters;
    procedure SetAttribute(AttrName: string;
      Attribute: TSynHighlighterAttributes; DefaultAttrib: TFontAttributes);
  public
    procedure InitializeHighlighter(Highlighter: TSynCustomHighlighter);

    Property RFTExporter: TSynExporterRTF read GetSynExporterRTF;
    Property HTMLExporter: TSynExporterHTML read GetSynExporterHTML;
    function getHighLighter(Extension: string): TSynCustomHighlighter;
    function GetFiters: string;

  end;

var
  dmMain: TdmMain;


implementation

uses lclproc;

{$R *.lfm}

{ TdmMain }

procedure TdmMain.DataModuleCreate(Sender: TObject);
begin
  fHighlighters := THighlighterList.Create;
  LoadHighlighters;
end;

procedure TdmMain.DataModuleDestroy(Sender: TObject);
begin
  fHighlighters.Free;
end;

procedure TdmMain.LoadHighlighters;
var
  i, j: integer;
  filter: string;
  stList: TStringList;
begin

  fHighlighters.Clear;
  for i := 0 to HIGHLIGHTERCOUNT - 1 do
  begin
    Filter := LowerCase(ARHighlighter[i].Filter);
    j := Pos('|', Filter);
    if j > 0 then
    begin
      Delete(Filter, 1, j);
      stList := TStringList.Create;
      StrToStrings(filter, ';', stList, False);
      for j := 0 to stList.Count - 1 do
        fHighlighters.Add(ExtractFileExt(stList[j]), i);
      stList.Free;
    end;
  end;

end;


procedure TdmMain.FontAttribToAttribute( Attribute: TSynHighlighterAttributes; Attrib: TFontAttributes);
begin
  Attribute.Foreground:=Attrib.Foreground;
  Attribute.Background:=Attrib.Background;
  Attribute.Style:=Attrib.Styles;

end;

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

procedure TdmMain.SetAttribute(AttrName:string; Attribute: TSynHighlighterAttributes; DefaultAttrib: TFontAttributes);
var
  tmpAttribs: TFontAttributes;
begin
  if not Assigned(Attribute) then
    exit;
  tmpAttribs := ConfigObj.ReadFontAttributes(AttrName,DefaultAttrib);
  FontAttribToAttribute(Attribute, tmpAttribs);
end;

procedure TdmMain.InitializeHighlighter(Highlighter: TSynCustomHighlighter);
var
  i: integer;
  AttrName: string;
  DefaultAttrib: TFontAttributes;
begin
  DefaultAttrib:= ConfigObj.ReadFontAttributes('Schema/Default/Text', FontAttributes());

  if Configobj.XMLConfigExtended.PathExists('Schema/'+CleanupName(Highlighter.GetLanguageName)) then
    begin
      for i := 0 to Highlighter.AttrCount - 1 do
        begin
          AttrName:=CleanupName(Highlighter.GetLanguageName)+'/'+CleanupName(Highlighter.Attribute[i].Name)+'/';
          SetAttribute(AttrName, Highlighter.Attribute[i], DefaultAttrib);
        end;
    end
  else
    begin
      for i := 0 to Highlighter.AttrCount - 1 do
          FontAttribToAttribute(Highlighter.Attribute[i], DefaultAttrib);

      SetAttribute('Schema/DefaultLang/String/', Highlighter.StringAttribute, DefaultAttrib);
      SetAttribute('Schema/DefaultLang/Comment/', Highlighter.CommentAttribute, DefaultAttrib);
      SetAttribute('Schema/DefaultLang/Identifier/', Highlighter.IdentifierAttribute, DefaultAttrib);
      SetAttribute('Schema/DefaultLang/Keyword/', Highlighter.KeywordAttribute, DefaultAttrib);
      SetAttribute('Schema/DefaultLang/Symbol/', Highlighter.SymbolAttribute, DefaultAttrib);
      SetAttribute('Schema/DefaultLang/Whitespace/', Highlighter.WhitespaceAttribute, DefaultAttrib);

    end;
    ConfigObj.XMLConfigExtended.CloseKey;

end;

function TdmMain.getHighLighter(Extension: string): TSynCustomHighlighter;
var
  tmp: integer;
  idx: integer;

begin
  tmp := fHighlighters.IndexOf(Extension);
  if tmp > -1 then
  begin
    idx := fHighlighters.Data[tmp];
    if not Assigned(ARHighlighter[idx].HL) then
    begin
      ARHighlighter[idx].HL := ARHighlighter[idx].HLClass.Create(Self);
      InitializeHighlighter(ARHighlighter[idx].HL);
    end;
    Result := ARHighlighter[idx].HL;
  end
  else
    Result := nil;
end;

function TdmMain.GetFiters: string;
var
  i: integer;
begin
  Result := RSAllFile + ' ('+GetAllFilesMask+')|' + GetAllFilesMask;
  for i := 0 to HIGHLIGHTERCOUNT - 1 do
   result := Result+'|' +ARHighlighter[i].Filter;

end;

end.
