unit Unit1;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  SynEditHighlighter, SynEdit, generics.Collections,
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
  SynHighlighterProlog, SynHighlighterRC, SupportFuncs,
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
    bConvert: TButton;
    bConvert1: TButton;
    bTemplate: TButton;
    bCreateSamples: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SelectDirectoryDialogOut: TSelectDirectoryDialog;
    SelectDirectoryDialogIn: TSelectDirectoryDialog;
    procedure bConvert1Click(Sender: TObject);
    procedure bConvertClick(Sender: TObject);
    procedure bCreateSamplesClick(Sender: TObject);
    procedure bTemplateClick(Sender: TObject);
  private
    procedure ConvertFile(InFile, OutFile: TFileName);
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

procedure TForm1.bConvertClick(Sender: TObject);
begin
  if not (OpenDialog1.Execute and SaveDialog1.Execute) then
     exit;

  ConvertFile(OpenDialog1.filename, SaveDialog1.filename);

end;

procedure TForm1.bConvert1Click(Sender: TObject);
var
  St: TStringList;
  i: Integer;
  Outfile: string;
begin

 if not (SelectDirectoryDialogIn.Execute and SelectDirectoryDialogOut.Execute) then
   exit;

 st := TStringList.Create;

 BuildFileList(IncludeTrailingPathDelimiter(SelectDirectoryDialogIn.FileName)+'color*.xml', faAnyFile, St, false);

 for i:= 0 to pred(st.count) do
   begin
     outfile := 'schema-'+ copy(ExtractFileName(st[i]),6,MaxInt);
     ConvertFile(st[i], IncludeTrailingPathDelimiter(SelectDirectoryDialogOut.FileName)+OutFile);

   end;

end;

procedure TForm1.ConvertFile(InFile, OutFile: TFileName);
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
  dict: TDictionary<String,String>;
  vl:string;

  procedure WriteColor(const Ident: string; const Value: TColor);
  var
    tmp: string;
  begin

    if not ColorToIdent(Value, tmp) then
      tmp := '$' + IntToHex(Value, 8);
    cfg.setValue(Ident, tmp);
  end;

  function ReadColor(const Section, Ident: string; const Default: TColor): TColor;
  var
    tmpString: string;
  begin
    try
      tmpString := doc.GetValue(Section + Ident, IntToHex(Default, 8));
      if not IdentToColor(tmpString, Result) then
        if not TryStrToInt(tmpString, Result) then
          Result := Default;

    except
      Result := Default;
    end;
  end;

  procedure InOut(inPath: string; OutPath: string);
  var
    tm: string;
    tmi:integer;
  begin
    tmi := ReadColor(inPath,'Foreground', clnone);
    if tmi <> clnone then
      WriteColor(OutPath+ 'Foreground', tmi);

    tmi := ReadColor(inPath, 'Background', clnone);
    if tmi <> clnone then
      WriteColor(OutPath+ 'Background', tmi);

    tm := doc.GetValue(inPath + 'Style', '');
    if tm <> EmptyStr then
      cfg.SetValue(OutPath + 'Style', tm);

  end;

begin
  dict:= TDictionary<String,String>.Create;
  //Default
  dict.Add('Assembler','');
  dict.Add('Comment','');
  dict.Add('Directive','');
  dict.Add('Number','');
  dict.Add('Reserved_word','');
  dict.Add('String','');
  dict.Add('Symbol','');
  dict.Add('Text','');
  dict.Add('Special','');
  dict.Add('Error','');
  dict.Add('Space','');
  dict.Add('Identifier','');
  //Alias
  dict.Add('Key','Reserved_word');
  dict.Add('Attribute_Value','String');
  dict.Add('Attribute_Name','Text');
  dict.Add('CDATA_Section','Assembler');
  dict.Add('DOCTYPE_Section','Directive');
  dict.Add('Element_Name','Text');
  dict.Add('Entity_Reference','Reserved_word');
  dict.Add('Namespace_Attribute_Name','Text');
  dict.Add('Namespace_Attribute_Value','String');
  dict.Add('Processing_Instruction','Assembler');
  dict.Add('ASP','Assembler');
  dict.Add('Escape_ampersand','Special');
  dict.Add('Unknown_word','Error');
  dict.Add('Value','Text');
  dict.Add('Preprocessor','Directive');
  dict.Add('Pragma','Directive');
  dict.Add('Variable','Identifier');
  dict.Add('Documentation','Space');

  st := TStringList.Create;

  doc := TXMLConfigStorage.Create(Infile, True);
  cfg := TXMLConfigStorage.Create(OutFile, False);

  doc.GetValue('Lazarus/ColorSchemes/Names/', st);
  schemaName := st[0];
  cfg.SetValue('Schema/Name', schemaName);

  tmps := 'Lazarus/ColorSchemes/Globals/Scheme' + schemaName + '/ahaDefault/';
  InOut(tmps, 'Schema/Default/Text/');
  tmps := 'Lazarus/ColorSchemes/Globals/Scheme' + schemaName + '/ahaGutter/';
  InOut(tmps, 'Schema/Default/Gutter/');
  tmps := 'Lazarus/ColorSchemes/Globals/Scheme' + schemaName + '/ahaLineNumber/';
  InOut(tmps, 'Schema/Default/LineNumber/');
  inOut('Lazarus/ColorSchemes/LangObjectPascal/Scheme'+schemaName+'/Assembler/','Schema/DefaultLang/Assembler/');
  inOut('Lazarus/ColorSchemes/LangObjectPascal/Scheme'+schemaName+'/Comment/','Schema/DefaultLang/Comment/');
  inOut('Lazarus/ColorSchemes/LangObjectPascal/Scheme'+schemaName+'/Directive/','Schema/DefaultLang/Directive/');
  inOut('Lazarus/ColorSchemes/LangObjectPascal/Scheme'+schemaName+'/Number/','Schema/DefaultLang/Number/');
  inOut('Lazarus/ColorSchemes/LangObjectPascal/Scheme'+schemaName+'/Reserved_word/','Schema/DefaultLang/Reserved_word/');
  inOut('Lazarus/ColorSchemes/LangObjectPascal/Scheme'+schemaName+'/String/','Schema/DefaultLang/String/');
  inOut('Lazarus/ColorSchemes/LangObjectPascal/Scheme'+schemaName+'/Symbol/','Schema/DefaultLang/Symbol/');
  inOut('Lazarus/ColorSchemes/Globals/Scheme'+schemaName+'/ahaDefault/','Schema/DefaultLang/Text/');
  inOut('Lazarus/ColorSchemes/LangHTML_document/Scheme'+schemaName+'/Escape_ampersand/','Schema/DefaultLang/Special/');
  inOut('Lazarus/ColorSchemes/LangHTML_document/Scheme'+schemaName+'/Unknown_word/','Schema/DefaultLang/Error/');
  inOut('Lazarus/ColorSchemes/LangC__/Scheme'+schemaName+'/Space/','Schema/DefaultLang/Space/');
  inOut('Lazarus/ColorSchemes/Globals/Scheme'+schemaName+'/ahaDefault/','Schema/DefaultLang/Identifier/');



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
          if not Dict.TrygetValue(HLAttr,vl) then
             inOut('Lazarus/ColorSchemes/Lang'+HlName+'/Scheme'+schemaName+'/'+HLattr+'/','Schema/'+hlname+'/'+HLAttr+'/');

        end;
      hl.Free;
    end;

  cfg.WriteToDisk;
  cfg.Free;
  doc.Free;
  st.Free;
  dict.free;

end;

procedure TForm1.bCreateSamplesClick(Sender: TObject);
var
  i,j: integer;
  HlName:string;
  tmps: string;
  hl :TSynCustomHighlighter;
  st: TStringList;

begin
  if not (SelectDirectoryDialogOut.Execute) then
     exit;

  st := TStringList.Create;
  for i := 0 to HIGHLIGHTERCOUNT -1 do
  begin
    HlName:=ARHighlighter[i].HLClass.GetLanguageName;
    CleanupName(HlName);
    hl:= ARHighlighter[i].HLClass.Create(nil);
    st.Text:= hl.SampleSource;
    tmps := Copy(ARHighlighter[i].filter,pos('|', ARHighlighter[i].filter)+2, Length(ARHighlighter[i].filter));
    j := pos(';', tmps)-1;
    if j < 1 then
       j := Length(tmps);
    tmps := Copy(tmps, 1, j);
    st.SaveToFile(IncludeTrailingPathDelimiter(SelectDirectoryDialogOut.FileName)+HlName+tmps);
    hl.Free;
  end;

  st.free;
end;

procedure TForm1.bTemplateClick(Sender: TObject);
var
  cfg: TXMLConfigStorage;
  st: TStringList;
  sy: TSynEdit;
  i,j: integer;
  tmps: string;
  HlName:string;
  HLAttr:string;
  hl :TSynCustomHighlighter;

  procedure WriteColor(const Ident: string; const Value: TColor);
  var
    tmp: string;
  begin

    if not ColorToIdent(Value, tmp) then
      tmp := '$' + IntToHex(Value, 8);
    cfg.setValue(Ident, tmp);
  end;

  procedure OutAttr( OutPath: string; Attr: TSynHighlighterAttributes);
  var
    tm: string;
  begin
    if attr = nil then exit;
    cfg.SetValue(OutPath + 'Style', SetToString(getpropinfo(Attr,'Style'), Integer(Attr.Style)));
    WriteColor(OutPath+ 'Background', Attr.Background);
    WriteColor(OutPath+ 'Foreground', Attr.Foreground);

  end;

begin
  st := TStringList.Create;
  if not (SaveDialog1.Execute) then
     exit;

  cfg := TXMLConfigStorage.Create(SaveDialog1.filename, False);

  cfg.SetValue('Schema/Name', 'DefaultSchema');

  sy := TSynEdit.Create(self);

  cfg.SetValue('Schema/Default/Text/Style', SetToString(GetPropInfo(sy.font,'Style'), Integer(sy.Font.Style)));
  WriteColor('Schema/Default/Text/Background', sy.Color);
  WriteColor('Schema/Default/Text/Foreground', sy.Font.Color);

//  cfg.SetValue('Schema/Default/Gutter/Style', SetToString(TypeInfo(TFontstyles), Integer(sy.Gutter. Font.Style)));
  WriteColor('Schema/Default/Gutter/Background', sy.Gutter.Color);
  WriteColor('Schema/Default/Gutter/Foreground', sy.Gutter.LineNumberPart(0).MarkupInfo.Foreground);

  cfg.SetValue('Schema/Default/LineNumber/Style', SetToString(GetPropInfo(sy.Gutter.LineNumberPart(0).MarkupInfo,'Style'), Integer(sy.Gutter.LineNumberPart(0).MarkupInfo.Style)));
  WriteColor('Schema/Default/LineNumber/Background', sy.Gutter.LineNumberPart(0).MarkupInfo.Background);
  WriteColor('Schema/Default/LineNumber/Foreground', sy.Gutter.LineNumberPart(0).MarkupInfo.Foreground);

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
          OutAttr('Schema/'+hlname+'/'+HLAttr+'/',hl.Attribute[j]);
        end;
      if hl is TSynPasSyn then
         begin
           OutAttr('Schema/DefaultLang/Comment/', hl.CommentAttribute);
           OutAttr('Schema/DefaultLang/String/',hl.StringAttribute);
           OutAttr('Schema/DefaultLang/Keyword/',hl.KeywordAttribute);
           OutAttr('Schema/DefaultLang/Symbol/',hl.SymbolAttribute);
           OutAttr('Schema/DefaultLang/Whitespace/',hl.WhitespaceAttribute);
           OutAttr('Schema/DefaultLang/Identifier/',hl.IdentifierAttribute);
         end;
      hl.Free;
    end;


  cfg.WriteToDisk;
  cfg.Free;
  st.Free;
end;

end.
