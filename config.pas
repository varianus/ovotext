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
{$modeswitch ADVANCEDRECORDS}
unit Config;
interface

uses
  Classes, SysUtils, Graphics, JsonTools,  dom,
  LResources, FGL, SupportFuncs, Stringcostants,
  SynEditHighlighter, SynEditStrConst, SynEditStrConstExtra,
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
  SynHighlighterDiff, SynHighlighterJScript, SynHighlighterJSon,
  SynHighlighterM3, SynHighlighterLDraw, SynHighlighterVBScript,
  SynHighlighterSml, SynHighlighterIDL, SynHighlighterCobol, SynHighlighterGWS,
  SynHighlighterAsm, SynHighlighterLua, SynHighlighterFortran,
  SynHighlighterProlog, SynHighlighterRC, SynHighlighterR,
  SynHighlighterMD,
  // from Pinote Editor
  MySEHighlighterGO, MySEHighlighterRust
  ;

type
  RHighlighter = record
    HLClass: TSynCustomHighlighterClass;
    Filter: string;
    HL: TSynCustomHighlighter;
  end;

const
  HIGHLIGHTERCOUNT = 51;
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
    (HLClass: TMySEHighlighterGo; Filter: SYNS_FilterGO; HL: nil),
    (HLClass: TSynGWScriptSyn; Filter: SYNS_FilterGWS; HL: nil),
    (HLClass: TSynHaskellSyn; Filter: SYNS_FilterHaskell; HL: nil),
    (HLClass: TSynHP48Syn; Filter: SYNS_FilterHP48; HL: nil),
    (HLClass: TSynHTMLSyn; Filter: SYNS_FilterHTML; HL: nil),
    (HLClass: TSynIniSyn; Filter: SYNS_FilterINI; HL: nil),
    (HLClass: TSynInnoSyn; Filter: SYNS_FilterInno; HL: nil),
    (HLClass: TSynJavaSyn; Filter: SYNS_FilterJava; HL: nil),
    (HLClass: TSynJScriptSyn; Filter: SYNS_FilterJScript; HL: nil),
    (HLClass: TSynJSONSyn; Filter: SYNS_FilterJSON; HL: nil),
    (HLClass: TSynLFMSyn; Filter: SYNS_FilterLFM; HL: nil),
    (HLClass: TSynLDRSyn; Filter: SYNS_FilterLDraw; HL: nil),
    (HLClass: TSynLuaSyn; Filter: SYNS_FilterLua; HL: nil),
    (HLClass: TSynModelicaSyn; Filter: SYNS_FilterModelica; HL: nil),
    (HLClass: TSynM3Syn; Filter: SYNS_FilterModula3; HL: nil),
    (HLClass: TSynMDSyn; Filter: SYNS_FilterMarkdown; HL: nil),
    (HLClass: TSynVBScriptSyn; Filter: SYNS_FilterVBScript; HL: nil),
    (HLClass: TSynBatSyn; Filter: SYNS_FilterBatch; HL: nil),
    (HLClass: TSynPasSyn; Filter: SYNS_FilterPascal; HL: nil),
    (HLClass: TSynPerlSyn; Filter: SYNS_FilterPerl; HL: nil),
    (HLClass: TSynPHPSyn; Filter: SYNS_FilterPHP; HL: nil),
    (HLClass: TSynPoSyn; Filter: SYNS_FilterPo; HL: nil),
    (HLClass: TSynProgressSyn; Filter: SYNS_FilterProgress; HL: nil),
    (HLClass: TSynPrologSyn; Filter: SYNS_FilterProlog; HL: nil),
    (HLClass: TSynPythonSyn; Filter: SYNS_FilterPython; HL: nil),
    (HLClass: TSynRSyn; Filter: SYNS_FilterR; HL: nil),
    (HLClass: TSynRCSyn; Filter: SYNS_FilterRC; HL: nil),
    (HLClass: TSynRubySyn; Filter: SYNS_FilterRuby; HL: nil),
    (HLClass: TMySEHighlighterRust; Filter: SYNS_FilterRust; HL: nil),
    (HLClass: TSynSQLSyn; Filter: SYNS_FilterSQL; HL: nil),
    (HLClass: TSynSMLSyn; Filter: SYNS_FilterSML; HL: nil),
    (HLClass: TSynTclTkSyn; Filter: SYNS_FilterTclTk; HL: nil),
    (HLClass: TSynTeXSyn; Filter: SYNS_FilterTclTk; HL: nil),
    (HLClass: TSynUNIXShellScriptSyn; Filter: SYNS_FilterUNIXShellScript; HL: nil),
    (HLClass: TSynVBSyn; Filter: SYNS_FilterVisualBASIC; HL: nil),
    (HLClass: TSynVrml97Syn; Filter: SYNS_FilterVrml97; HL: nil),
    (HLClass: TSynAsmSyn; Filter: SYNS_FilterX86Asm; HL: nil),
    (HLClass: TSynXMLSyn; Filter: SYNS_FilterXML; HL: nil)
     );


type

  THighLighterList = specialize TFPGMap<string, integer>;
  TStringDictionary = specialize TFPGMap<string, string>;



  { TConfig }
  RAppSettings = record
    CloseWithLastTab: boolean;
    ColorSchema: string;
  end;

  { RFontAttributes }

  TFontAttributes = record
    Foreground: TColor;
    Background: TColor;
    Styles: TFontStyles;
  end;

  { TXMLConfigExtended }

  TConfig = class
  private
    fHighlighters: THighLighterList;
    FAppSettings: RAppSettings;
    FConfigFile: string;
    fConfigDir: string;
    FDirty: boolean;
    FFont: TFont;
    FShowRowNumber: boolean;
    FShowToolbar: boolean;
    ResourcesPath: string;
//    fXMLConfigExtended: TXMLConfigExtended;
    fConfigHolder: TJsonNode;
    fColorSchema: TJsonNode;
    fSchemaName: string;
    fThemesList: TStringDictionary;
    fAttributeAliases : TStringDictionary;


    function GetBackGroundColor: TColor;
    procedure LoadAliases;
    procedure SetDirty(AValue: boolean);
    procedure SetFont(AValue: TFont);
    procedure SetShowRowNumber(AValue: boolean);
    procedure SetShowToolbar(AValue: boolean);
    procedure WriteColor(const Section, Ident: string; const Value: TColor);
    procedure InitializeHighlighter(Highlighter: TSynCustomHighlighter);
    procedure FontAttribToAttribute(Attribute: TSynHighlighterAttributes; Attrib: TFontAttributes);
    procedure LoadHighlighters;
    procedure LoadThemes;
    procedure SetAttribute(AttrName: string; Attribute: TSynHighlighterAttributes; DefaultAttrib: TFontAttributes);

  public
    constructor Create;
    procedure ReadConfig;
    procedure SaveConfig;
    procedure WriteStrings(Section: string; Name: string; Values: TStrings);
    function ReadStrings(Section: string; Name: string; Values: TStrings): integer;
    function GetResourcesPath: string;
    procedure Flush;
    function ReadColor(const Section, Ident: string; const Default: TColor): TColor;
    function ReadFontStyle(const Section, Ident: string; const default: TFontStyles): TFontstyles;
    function ReadFontAttributes(AttibuteName: string; const Default: TFontAttributes): TFontAttributes;
    function getHighLighter(Extension: string): TSynCustomHighlighter; overload;
    function getHighLighter(Index: integer): TSynCustomHighlighter; overload;
    destructor Destroy; override;
    function GetFiters: string;
    Procedure SetTheme(Index: integer);

    // -- //
    property ThemeList: TStringDictionary read fThemesList;
    property Dirty: boolean read FDirty write SetDirty;
    property ConfigHolder: TJsonNode read  fConfigHolder;
    property ConfigDir: string read fConfigDir;
    property ConfigFile: string read FConfigFile;
    property Font: TFont read FFont write SetFont;
    property ShowRowNumber: boolean read FShowRowNumber write SetShowRowNumber;
    property ShowToolbar: boolean read FShowToolbar write SetShowToolbar;
    property AppSettings: RAppSettings read FAppSettings write FAppSettings;
    property BackGroundColor: TColor read GetBackGroundColor;
  end;


function FontAttributes(const Foreground: TColor = clDefault; BackGround: Tcolor = clNone; const Styles: TFontStyles = []): TFontAttributes; inline;

function ConfigObj: TConfig;

implementation

{ TConfig }
uses
  Fileutil, lclproc, typinfo,
  // only for default font !
  Synedit
{$ifdef Darwin}
  , MacOSAll
{$endif}  ;

var
  FConfigObj: TConfig;

const
  SectionUnix = 'UNIX';
  IdentResourcesPath = 'ResourcesPath';
  ResourceSubDirectory = 'Resources';

const
 {$ifdef UNIX}
  DefaultDirectory = '/usr/share/ovotext/';
  {$DEFINE NEEDCFGSUBDIR}
 {$endif}

 {$ifdef DARWIN}
  BundleResourcesDirectory = '/Contents/Resources/';
 {$endif}

  SectionGeneral = 'General';


function NextToken(const S: string; var SeekPos: integer; const TokenDelim: char): string;
var
  TokStart: integer;
begin
  repeat
    if SeekPos > Length(s) then
    begin
      Result := '';
      Exit;
    end;
    if S[SeekPos] = TokenDelim then
      Inc(SeekPos)
    else
      Break;
  until False;
  TokStart := SeekPos; { TokStart := first character not in TokenDelims }

  while (SeekPos <= Length(s)) and not (S[SeekPos] = TokenDelim) do
    Inc(SeekPos);

  { Calculate result := s[TokStart, ... , SeekPos-1] }
  Result := Copy(s, TokStart, SeekPos - TokStart);

    { We don't have to do Inc(seekPos) below. But it's obvious that searching
      for next token can skip SeekPos, since we know S[SeekPos] is TokenDelim. }
  Inc(SeekPos);
end;

function GetConfigDir: string;
var
  Path: string;
begin
  Path := GetAppConfigDir(False);
  ForceDirectories(Path);
  Result := IncludeTrailingPathDelimiter(Path);

end;

function FontAttributes(const Foreground: TColor; BackGround: Tcolor; const Styles: TFontStyles = []): TFontAttributes; inline;
begin
  Result.Foreground := Foreground;
  Result.Background := Background;
  Result.Styles := Styles;
end;

function ConfigObj: TConfig;
begin
  if not Assigned(FConfigObj) then
    FConfigObj := TConfig.Create;
  Result := FConfigObj;
end;

constructor TConfig.Create;
begin
  FFont := Tfont.Create;
  FConfigFile := GetAppConfigFile(False
{$ifdef NEEDCFGSUBDIR}
    , True
{$ENDIF}
    );

  FConfigFile := ChangeFileExt(FConfigFile,'.json');
  fConfigDir := GetConfigDir;
  fConfigHolder := TJsonNode.Create;
  if FileExists(FConfigFile) then
   fConfigHolder.LoadFromFile(FConfigFile);

  fHighlighters := THighlighterList.Create;
  fThemesList := TStringDictionary.Create;
  ReadConfig;

  LoadHighlighters;
  LoadThemes;

  fAttributeAliases := TStringDictionary.Create;
  LoadAliases;

  if (FAppSettings.ColorSchema <> '') then
    if not FileExists(FAppSettings.ColorSchema) then
      FAppSettings.ColorSchema := '';

  if (FAppSettings.ColorSchema = '') then
    if FileExists(IncludeTrailingPathDelimiter(ResourcesPath) + 'schema-Default.json') then
      FAppSettings.ColorSchema := IncludeTrailingPathDelimiter(ResourcesPath) + 'schema-Default.json';

  fColorSchema := TJsonNode.Create;
  if FAppSettings.ColorSchema <> '' then
    fColorSchema.LoadFromFile(FAppSettings.ColorSchema);

end;


procedure TConfig.FontAttribToAttribute(Attribute: TSynHighlighterAttributes; Attrib: TFontAttributes);
begin
  Attribute.Foreground := Attrib.Foreground;
  Attribute.Background := Attrib.Background;
  Attribute.Style := Attrib.Styles;

end;

procedure TConfig.SetAttribute(AttrName: string; Attribute: TSynHighlighterAttributes; DefaultAttrib: TFontAttributes);
var
  tmpAttribs: TFontAttributes;
begin
  if not Assigned(Attribute) then
    exit;
  tmpAttribs := ReadFontAttributes(AttrName, DefaultAttrib);
  FontAttribToAttribute(Attribute, tmpAttribs);
end;

procedure TConfig.InitializeHighlighter(Highlighter: TSynCustomHighlighter);
var
  i: integer;
  AttrName: string;
  AttrPath: string;
  AttributeAlias :string;
  DefaultAttrib: TFontAttributes;
  style : TFontStyles;
const
  DefaultPath = 'Schema/DefaultLang/';

begin
   if FAppSettings.ColorSchema = '' then
     exit;

  DefaultAttrib := ReadFontAttributes('Schema/Default/Text/', FontAttributes());
  AttrPath := 'Schema/' + CleanupName(Highlighter.GetLanguageName) + '/';

  for i := 0 to Highlighter.AttrCount - 1 do
  begin
    AttrName := CleanupName(Highlighter.Attribute[i].Name);
    if Assigned(FcolorSchema.find(AttrPath + AttrName + '/')) then
      SetAttribute(AttrPath + AttrName + '/', Highlighter.Attribute[i], DefaultAttrib)
    else
      begin
      if fAttributeAliases.TryGetData(AttrName, AttributeAlias) then
        SetAttribute(DefaultPath + AttributeAlias+ '/', Highlighter.Attribute[i], DefaultAttrib)
      else
        begin
          Style := Highlighter.Attribute[i].Style;
          SetAttribute(DefaultPath + AttrName + '/', Highlighter.Attribute[i], DefaultAttrib);
          Highlighter.Attribute[i].Style :=  style;
        end
      end
  end;

end;

function TConfig.getHighLighter(Extension: string): TSynCustomHighlighter;
var
  tmp: integer;
  idx: integer;

begin
  tmp := fHighlighters.IndexOf(lowercase(Extension));
  if tmp > -1 then
  begin
    idx := fHighlighters.Data[tmp];
    Result := getHighLighter(idx);
  end
  else
    Result := nil;

end;

function TConfig.getHighLighter(Index: integer): TSynCustomHighlighter;
begin
  if not Assigned(ARHighlighter[Index].HL) then
  begin
    ARHighlighter[Index].HL := ARHighlighter[Index].HLClass.Create(nil);
    InitializeHighlighter(ARHighlighter[Index].HL);
  end;
  Result := ARHighlighter[Index].HL;
end;

function TConfig.GetFiters: string;
var
  i: integer;
begin
  Result := RSAllFile + ' (' + GetAllFilesMask + ')|' + GetAllFilesMask;
  for i := 0 to HIGHLIGHTERCOUNT - 1 do
    Result := Result + '|' + ARHighlighter[i].Filter;

end;

procedure TConfig.SetTheme(Index: integer);
var
  i: Integer;
begin

  fColorSchema.LoadFromFile(ThemeList.Data[Index]);
  FAppSettings.ColorSchema := ThemeList.Data[Index];
  Dirty := TRUE;
  for i := 0 to HIGHLIGHTERCOUNT - 1 do
   if Assigned(ARHighlighter[i].HL) then
     InitializeHighlighter(ARHighlighter[i].HL);

end;


procedure TConfig.LoadHighlighters;
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

procedure TConfig.LoadThemes;
var
  FileList: TStringList;
  I: integer;
  Doc: TJsonNode;
  SchemaName: string;

begin
  fThemesList.Clear;
  FileList := TStringList.Create;

  BuildFileList(ResourcesPath + 'schema-*.json', faAnyFile, FileList, False);
  BuildFileList(ConfigDir + 'schema-*.json', faAnyFile, FileList, False);

  for I := 0 to Pred(FileList.Count) do
  begin
    try
      Doc := TJsonNode.Create();
      doc.LoadFromFile( FileList[i]);
      SchemaName := doc.GetValueDef('Schema/Name', '');
      if SchemaName <> '' then
        fThemesList.Add(SchemaName, FileList[i]);
    except
    end;
    FreeAndNil(doc);
  end;
  FileList.Free;
end;

procedure TConfig.LoadAliases;
begin
  //Default
  //fAttributeAliases.Add('Assembler','');
  //fAttributeAliases.Add('Comment','');
  //fAttributeAliases.Add('Directive','');
  //fAttributeAliases.Add('Number','');
  //fAttributeAliases.Add('Reserved_word','');
  //fAttributeAliases.Add('String','');
  //fAttributeAliases.Add('Symbol','');
  //fAttributeAliases.Add('Text','');
  //fAttributeAliases.Add('Special','');
  //fAttributeAliases.Add('Error','');
  //fAttributeAliases.Add('Space','');
  //fAttributeAliases.Add('Identifier','');
  //Alias
  fAttributeAliases.Add('Key','Reserved_word');
  fAttributeAliases.Add('Attribute_Value','String');
  fAttributeAliases.Add('Attribute_Name','Text');
  fAttributeAliases.Add('CDATA_Section','Assembler');
  fAttributeAliases.Add('DOCTYPE_Section','Directive');
  fAttributeAliases.Add('Element_Name','Reserved_word');
  fAttributeAliases.Add('Entity_Reference','Reserved_word');
  fAttributeAliases.Add('Namespace_Attribute_Name','Text');
  fAttributeAliases.Add('Namespace_Attribute_Value','String');
  fAttributeAliases.Add('Processing_Instruction','Assembler');
  fAttributeAliases.Add('ASP','Assembler');
  fAttributeAliases.Add('Escape_ampersand','Special');
  fAttributeAliases.Add('Unknown_word','Error');
  fAttributeAliases.Add('Value','Text');
  fAttributeAliases.Add('Preprocessor','Directive');
  fAttributeAliases.Add('Pragma','Directive');
  fAttributeAliases.Add('Variable','Identifier');
  fAttributeAliases.Add('Documentation','Space');
  fAttributeAliases.Add('Bullet','Directive');
  fAttributeAliases.Add('Subheading','Directive');
  fAttributeAliases.Add('Monospace','Assembler');
  fAttributeAliases.Add('Code','Assembler');

end;

destructor TConfig.Destroy;
var
  i: Integer;
begin
  SaveConfig;
  fConfigHolder.SaveToFile(FConfigFile, true);
  fConfigHolder.Free;
  fColorSchema.Free;
  FFont.Free;
  fHighlighters.Free;
  fThemesList.Free;
  fAttributeAliases.free;
  for i := 0 to HIGHLIGHTERCOUNT - 1 do
    ARHighlighter[i].HL.Free;

  inherited Destroy;
end;

procedure TConfig.SaveConfig;
begin
  if not FDirty then
    Exit;

  fConfigHolder.Find(SectionUnix + '/' + IdentResourcesPath, true).AsString := ResourcesPath;
  fConfigHolder.Find('Application/CloseWithLastTab', true).AsBoolean := FAppSettings.CloseWithLastTab;
  fConfigHolder.Find('Application/ColorSchema/Name', true).AsString := FAppSettings.ColorSchema;

  fConfigHolder.Find('Editor/Font/Name', true).AsString := FFont.Name;
  fConfigHolder.Find('Editor/Font/Size', true).AsInteger := FFont.Size;
  fConfigHolder.Find('Editor/ShowRowNumber', true).AsBoolean := FShowRowNumber;
  fConfigHolder.Find('Editor/ShowToolbar', true).AsBoolean := FShowToolbar;


  FDirty := false;
end;

procedure TConfig.ReadConfig;
var
  fontName: string;
begin
  ResourcesPath := IncludeTrailingPathDelimiter(fConfigHolder.GetValueDef(SectionUnix + '/' + IdentResourcesPath, GetResourcesPath));

  FAppSettings.CloseWithLastTab := fConfigHolder.GetValueDef('Application/CloseWithLastTab', False);
  FAppSettings.ColorSchema := fConfigHolder.GetValueDef('Application/ColorSchema/Name', '');

  fontName := fConfigHolder.GetValueDef('Editor/Font/Name', EmptyStr);
  if fontName = EmptyStr then
  begin
    FFont.Name := SynDefaultFontName;
    FFont.Size := SynDefaultFontSize;
  end
  else
  begin
    FFont.Name := fontName;
    FFont.Size := fConfigHolder.GetValueDef('Editor/Font/Size', 0);
  end;

  FShowRowNumber := fConfigHolder.GetValueDef('Editor/ShowRowNumber', True);
  FShowToolbar := fConfigHolder.GetValueDef('Editor/ShowToolbar', True);

  FDirty := False;
end;

function TConfig.ReadStrings(Section: string; Name: string; Values: TStrings): integer;
var
  Node: TJsonNode;
  i: Integer;
begin
  Values.Clear;
  Node := fConfigHolder.find(Section + '/' + Name);
  if Assigned(Node) then
    begin
      for i := 0 to node.Count -1 do
       Values.Add(Node.Child(i).AsString);
    end;

  Result := Values.Count;
end;


procedure TConfig.WriteStrings(Section: string; Name: string; Values: TStrings);
var
  Node: TJsonNode;
  i: Integer;
begin
  Node := fConfigHolder.find(Section + '/' + Name);
  if Assigned(Node) then
    begin
     Node.Clear;
     for i := 0 to Values.Count -1 do
       node.Add('',Values[i]);
    end
  else
    begin
      Node := fConfigHolder.find(Section + '/' + Name, true);  // fConfigHolder.Add(APath, nkArray);
      node.Kind:=nkArray;
      for i := 0 to Values.Count -1 do
        node.Add('',Values[i]);

    end;
end;

function TConfig.GetBackGroundColor: TColor;
begin
  Result := ReadColor('Default/Text', 'Background', clWindow);
end;

procedure TConfig.SetDirty(AValue: boolean);
begin
  if FDirty = AValue then
    Exit;
  FDirty := AValue;
end;

procedure TConfig.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
  FDirty := True;
end;

procedure TConfig.SetShowRowNumber(AValue: boolean);
begin
  if FShowRowNumber=AValue then Exit;
  FShowRowNumber:=AValue;
  FDirty := True;
end;

procedure TConfig.SetShowToolbar(AValue: boolean);
begin
  if FShowToolbar = AValue then Exit;
  FShowToolbar := AValue;
  FDirty := True;
end;

function TConfig.ReadColor(const Section, Ident: string; const Default: TColor): TColor;
var
  tmpString: string;
begin
  try
    tmpString := fColorSchema.GetValueDef(Section + Ident, IntToHex(Default, 8));
    if not IdentToColor(tmpString, Result) then
      if not TryStrToInt(tmpString, Result) then
        Result := Default;

  except
    Result := Default;
  end;
end;

function TConfig.ReadFontStyle(const Section, Ident: string; const default: TFontStyles): TFontstyles;
var
  tmp: string;
begin
  try
    tmp := fColorSchema.GetValueDef(Section + Ident, '');
    Result := TFontStyles(StringToSet(PTypeInfo(TypeInfo(TFontstyles)), tmp));
  except
    Result := default;
  end;

end;

function TConfig.ReadFontAttributes(AttibuteName: string; const Default: TFontAttributes): TFontAttributes;
begin
  Result.Foreground := ReadColor(AttibuteName, 'Foreground', Default.Foreground);
  Result.Background := ReadColor(AttibuteName, 'Background', Default.Background);
  Result.Styles := ReadFontStyle(AttibuteName, 'Style', Default.Styles);

end;

procedure TConfig.WriteColor(const Section, Ident: string; const Value: TColor);
var
  tmp: string;
begin

  if not ColorToIdent(Value, tmp) then
    tmp := '$' + IntToHex(Value, 8);
  fConfigHolder.Find(Section + '/' + Ident, true).AsString := tmp;
end;

procedure TConfig.Flush;
begin
  fConfigHolder.SaveToFile(FConfigFile);
end;

function TConfig.GetResourcesPath: string;
{$ifdef DARWIN}
var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$endif}
begin
{$ifdef UNIX}
{$ifdef DARWIN}
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);

  Result := pathStr + BundleResourcesDirectory;
{$else}
  Result := DefaultDirectory;
{$endif}
{$endif}

{$ifdef WINDOWS}
  Result := ExtractFilePath(ExtractFilePath(ParamStr(0))) + ResourceSubDirectory + PathDelim;
{$endif}

end;

initialization
  FConfigObj := nil;

finalization
  if Assigned(FConfigObj) then
  begin
    FConfigObj.SaveConfig;
    FConfigObj.Free;
  end;

end.
