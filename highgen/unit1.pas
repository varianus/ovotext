unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  SynEditHighlighter,
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
  SynHighlighterDiff, SynHighlighterJScript,
  SynHighlighterM3, SynHighlighterLDraw, SynHighlighterVBScript,
  SynHighlighterSml, SynHighlighterIDL, SynHighlighterCobol, SynHighlighterGWS,
  SynHighlighterAsm, SynHighlighterLua, SynHighlighterFortran,
  SynHighlighterProlog, SynHighlighterRC,
  XMLPropStorage, typinfo;

type
  RHighlighter = record
    HLClass: TSynCustomHighlighterClass;
    Filter: string;
    HL: TSynCustomHighlighter;
  end;

const
  HIGHLIGHTERCOUNT = 46;
  ARHighlighter: array [0..HIGHLIGHTERCOUNT - 1] of RHighlighter = (
    (HLClass: TSynPasSyn; Filter: 'Pascal Files (*.pas,*.dpr,*.dpk,*.inc)|*.pas;*.dpr;*.dpk;*.inc'; HL: nil),
    (HLClass: TSynCppSyn; Filter: 'C++ Files (*.c,*.cpp,*.h,*.hpp,*.hh)|*.c;*.cpp;*.h;*.hpp;*.hh'; HL: nil),
    (HLClass: TSynJavaSyn; Filter: 'Java Files (*.java)|*.java'; HL: nil),
    (HLClass: TSynPerlSyn; Filter: 'Perl Files (*.pl,*.pm,*.cgi)|*.pl;*.pm;*.cgi'; HL: nil),
    (HLClass: TSynHTMLSyn; Filter: 'HTML Document (*.htm,*.html)|*.htm;*.html'; HL: nil),
    (HLClass: TSynXMLSyn; Filter: 'XML Document (*.xml,*.xsd,*.xsl,*.xslt,*.dtd)|*.xml;*.xsd;*.xsl;*.xslt;*.dtd';
    HL: nil),
    (HLClass: TSynLFMSyn; Filter: 'Lazarus Form Files (*.lfm)|*.lfm'; HL: nil),
    (HLClass: TSynUNIXShellScriptSyn; Filter: 'UNIX Shell Scripts (*.sh)|*.sh'; HL: nil),
    (HLClass: TSynCssSyn; Filter: 'Cascading Stylesheets (*.css)|*.css'; HL: nil),
    (HLClass: TSynPHPSyn; Filter: 'PHP Files (*.php,*.php3,*.phtml,*.inc)|*.php;*.php3;*.phtml;*.inc'; HL: nil),
    (HLClass: TSynTeXSyn; Filter: 'TeX Files (*.tex)|*.tex'; HL: nil),
    (HLClass: TSynSQLSyn; Filter: 'SQL Files (*.sql)|*.sql'; HL: nil),
    (HLClass: TSynPythonSyn; Filter: 'Python Files (*.py)|*.py'; HL: nil),
    (HLClass: TSynVBSyn; Filter: 'Visual Basic Files (*.bas)|*.bas'; HL: nil),
    (HLClass: TSynBatSyn; Filter: 'MS-DOS Batch Files (*.bat;*.cmd)|*.bat;*.cmd'; HL: nil),
    (HLClass: TSynIniSyn; Filter: 'INI Files (*.ini)|*.ini'; HL: nil),
    (HLClass: TSynPoSyn; Filter: 'Po Files (*.po)|*.po'; HL: nil),
    (HLClass: TSynTclTkSyn; Filter: 'Tcl/Tk Files (*.tcl)|*.tcl'; HL: nil),
    (HLClass: TSynRubySyn; Filter: 'Ruby Files (*.rb;*.rbw)|*.rb;*.rbw'; HL: nil),
    (HLClass: TSynCSSyn; Filter: 'C# Files (*.cs)|*.cs'; HL: nil),
    (HLClass: TSynHaskellSyn; Filter: 'Haskell Files (*.hs;*.lhs)|*.hs;*.lhs'; HL: nil),
    (HLClass: TSynFoxproSyn; Filter: 'Foxpro Files (*.prg)|*.prg'; HL: nil),
    (HLClass: TSynInnoSyn; Filter: 'Inno Setup Scripts (*.iss)|*.iss'; HL: nil),
    (HLClass: TSynDmlSyn; Filter: 'GEMBASE Files (*.dml,*.gem)|*.DML;*.GEM'; HL: nil),
    (HLClass: TSynCACSyn; Filter: 'CA-Clipper Files (*.prg,*.ch,*.inc)|*.prg;*.ch;*.inc'; HL: nil),
    (HLClass: TSynModelicaSyn; Filter: 'Modelica Files (*.mo)|*.mo'; HL: nil),
    (HLClass: TSynVrml97Syn; Filter: 'Vrml97/X3D World (*.wrl;*.wrml;*.vrl;*.vrml;*.x3d)|*.wrl;*.wrml;*.vrl;*.vrml;*.x3d';
    HL: nil),
    (HLClass: TSynHP48Syn; Filter: 'HP48 Files (*.s,*.sou,*.a,*.hp)|*.s;*.sou;*.a;*.hp'; HL: nil),
    (HLClass: TSynAWKSyn; Filter: 'AWK Script (*.awk)|*.awk'; HL: nil),
    (HLClass: TSynProgressSyn; Filter: 'Progress Files (*.w,*.p,*.i)|*.w;*.p;*.i'; HL: nil),
    (HLClass: TSynEiffelSyn; Filter: 'Eiffel (*.e;*.ace)|*.e;*.ace'; HL: nil),
    (HLClass: TSynBaanSyn; Filter: 'Baan 4GL Files (*.cln)|*.cln'; HL: nil),
    (HLClass: TSynDiffSyn; Filter: 'Diff Files (*.diff)|*.diff'; HL: nil),
    (HLClass: TSynJScriptSyn; Filter: 'Javascript Files (*.js)|*.js'; HL: nil),
    (HLClass: TSynM3Syn; Filter: 'Modula-3 Files (*.m3)|*.m3'; HL: nil),
    (HLClass: TSynLDRSyn; Filter: 'LEGO LDraw Files (*.ldr)|*.ldr'; HL: nil),
    (HLClass: TSynVBScriptSyn; Filter: 'VBScript Files (*.vbs)|*.vbs'; HL: nil),
    (HLClass: TSynSMLSyn; Filter: 'Standard ML Files (*.sml)|*.sml'; HL: nil),
    (HLClass: TSynIdlSyn; Filter: 'CORBA IDL files (*.idl)|*.idl'; HL: nil),
    (HLClass: TSynCobolSyn; Filter: 'COBOL Files (*.cbl;*.cob)|*.cbl;*.cob'; HL: nil),
    (HLClass: TSynGWScriptSyn; Filter: 'GW-TEL Script Files (*.gws)|*.gws'; HL: nil),
    (HLClass: TSynAsmSyn; Filter: 'x86 Assembly Files (*.asm)|*.ASM'; HL: nil),
    (HLClass: TSynLuaSyn; Filter: 'Lua Script File (*.Lua)|*.Lua'; HL: nil),
    (HLClass: TSynFortranSyn; Filter: 'Fortran Files (*.for)|*.for'; HL: nil),
    (HLClass: TSynPrologSyn; Filter: 'Prolog Files (*.pl;*.pro;*.prl)|*.pl;*.pro;*.prl'; HL: nil),
    (HLClass: TSynRCSyn; Filter: 'Resource Files (*.rc)|*.rc'; HL: nil)
    );


type

  { TForm1 }

  TForm1 = class(TForm)
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

function RGBtoBGR(InColor: string): string;
var
  rgbcolor: longint;
begin
  Delete(InColor, 1, 1);
  if tryStrToInt('$' + InColor, rgbcolor) then
    Result := ColorToString(RGBToColor(rgbcolor shr 16, (rgbcolor and $00ff00) shr 8, rgbcolor and $ff))
  else
    Result := ColorToString(StringToColor('cl' + InColor));

end;

procedure CleanupName(var aName:string);
var
  c: integer;
begin
  for c:= 1 to Length(aName) do
  if not (upcase(aName[c])  in['A'..'Z','0'..'9','_']) then
    aName[c] := '_';
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  doc: TXMLConfigStorage;
  cfg: TXMLConfigStorage;
  st: TStringList;
  schemaName: string;
  i,j: integer;
  tmps: string;
  HlName:string;
  HLAttr:string;
  hl :TSynCustomHighlighter;

  procedure InOut(inPath: string; OutPath: string);
  var
    tm: string;
  begin
    tm := doc.GetValue(inPath + 'Style', '');
    if tm <> EmptyStr then
      cfg.SetValue(OutPath + 'Style', tm);
    tm := doc.GetValue(inPath + 'Background', '');
    if tm <> EmptyStr then
      cfg.SetValue(OutPath+ 'Background', tm);
    tm := doc.GetValue(inPath + 'Foreground', '');
    if tm <> EmptyStr then
      cfg.SetValue(OutPath+ 'Foreground', tm);

  end;

begin
  st := TStringList.Create;
  if not (OpenDialog1.Execute and SaveDialog1.Execute) then
     exit;

  doc := TXMLConfigStorage.Create(OpenDialog1.filename, True);
  cfg := TXMLConfigStorage.Create(SaveDialog1.filename, False);

  doc.GetValue('Lazarus/ColorSchemes/Names/', st);
  schemaName := st[0];
  cfg.SetValue('Schema/Name', schemaName);

  tmps := 'Lazarus/ColorSchemes/Globals/Scheme' + schemaName + '/ahaDefault/';
  InOut(tmps, 'Schema/Default/Text/');
  tmps := 'Lazarus/ColorSchemes/Globals/Scheme' + schemaName + '/ahaGutter/';
  InOut(tmps, 'Schema/Default/Gutter/');
  tmps := 'Lazarus/ColorSchemes/Globals/Scheme' + schemaName + '/ahaLineNumber/';
  InOut(tmps, 'Schema/Default/LineNumber/');

  st.clear;
  for i := 0 to HIGHLIGHTERCOUNT -1 do
    begin
      HlName:=ARHighlighter[i].HLClass.GetLanguageName;
      CleanupName(HlName);
      hl:= ARHighlighter[i].HLClass.Create(nil);
      for j:= 0 to hl.AttrCount -1 do
        begin
          HLAttr:= hl.Attribute[j].Name;
          CleanupName(HLAttr);
          inOut('Lazarus/ColorSchemes/Lang'+HlName+'/Scheme'+schemaName+'/'+HLattr+'/','Schema/'+hlname+'/'+HLAttr+'/');
        end;
    end;

  inOut('Lazarus/ColorSchemes/LangObjectPascal/Scheme'+schemaName+'/Comment/','Schema/DefaultLang/Comment/');
  inOut('Lazarus/ColorSchemes/LangObjectPascal/Scheme'+schemaName+'/String/','Schema/DefaultLang/String/');
  inOut('Lazarus/ColorSchemes/LangObjectPascal/Scheme'+schemaName+'/Reserved_word/','Schema/DefaultLang/Keyword/');
  inOut('Lazarus/ColorSchemes/LangObjectPascal/Scheme'+schemaName+'/Symbol/','Schema/DefaultLang/Symbol/');
  inOut('Lazarus/ColorSchemes/LangObjectPascal/Scheme'+schemaName+'/Whitespace/','Schema/DefaultLang/Whitespace/');
  inOut('Lazarus/ColorSchemes/LangObjectPascal/Scheme'+schemaName+'/Identifier/','Schema/DefaultLang/Identifier/');

  cfg.WriteToDisk;
  cfg.Free;
  doc.Free;
  st.Free;

end;

end.
