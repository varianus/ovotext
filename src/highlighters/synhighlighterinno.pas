{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterInno.pas, released 2000-05-01.
The Initial Author of this file is Satya.
Portions created by Satya are Copyright 2000 Satya.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynHighlighterInno.pas,v 1.23 2005/01/28 16:53:23 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides an Inno script file highlighter for SynEdit)
@author(Satya)
@created(2000-05-01)
@lastmod(2001-01-23)
The SynHighlighterInno unit provides an Inno script file highlighter for SynEdit.
Check out http://www.jrsoftware.org for the free Inno Setup program,
and http://www.wintax.nl/isx/ for My Inno Setup Extensions.
}
unit SynHighlighterInno;

{.$I synedit.inc}
  //SynDefines.inc is the synedit.inc from laz 1.2.0 synedit package source if it has changed
  //in newer version you might need to copy it again. REmeber to redclare the syn_lazarus define.
{$I SynDefines.inc}

interface

uses
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynHighlighterHashEntries,
  Classes;

type
  TtkTokenKind = (tkComment, tkConstant, tkIdentifier, tkKey, tkKeyOrParameter,
    tkNull, tkNumber, tkParameter, tkSection, tkSpace, tkString, tkSymbol,
    tkUnknown);

  TProcTableProc = procedure of object;

  TSynInnoSyn = class(TSynCustomHighlighter)
  private
    fLine: PChar;
    fLineNumber: integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fConstantAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fSectionAttri: TSynHighlighterAttributes;
    fParamAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fInvalidAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fKeywords: TSynHashEntryList;
    function KeyHash(ToHash: PChar): integer;
    function KeyComp(const aKey: string): Boolean;
    procedure SymbolProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SectionProc;
    procedure SpaceProc;
    procedure EqualProc;
    procedure ConstantProc;
    procedure SemiColonProc;
    procedure StringProc;
    procedure UnknownProc;

    procedure DoAddKeyword(AKeyword: string; AKind: integer);
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetToken: string; override;
    {$IFDEF SYN_LAZARUS}
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    {$ENDIF}
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetLine(const NewValue: string; LineNumber:Integer); override;
    function GetSampleSource :string; override;
  published
    property ConstantAttri: TSynHighlighterAttributes read fConstantAttri
      write fConstantAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property InvalidAttri: TSynHighlighterAttributes read fInvalidAttri
      write fInvalidAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property ParameterAttri: TSynHighlighterAttributes read fParamAttri
      write fParamAttri;
    property SectionAttri: TSynHighlighterAttributes read fSectionAttri
      write fSectionAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
  end;

implementation

uses
  SynEditStrConst,SynEditStrConstExtra;

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;

const
  {Note: new 'Section names' and the new 'Constants' need not be added
         as they are highlighted automatically}

  {Ref:  Keywords and Parameters are updated as they last appeared in
         Inno Setup / ISX version 1.3.26}

  Keywords: string =
    'AdminPrivilegesRequired,AllowNoIcons,AllowRootDirectory,AllowUNCPath,' +
    'AlwaysCreateUninstallIcon,AlwaysRestart,AlwaysShowComponentsList,' +
    'AlwaysShowDirOnReadyPage,AlwaysShowGroupOnReadyPage,' +
    'AlwaysUsePersonalGroup,AppCopyright,AppId,AppMutex,AppName,AppPublisher,' +
    'AppPublisherURL,AppSupportURL,AppUpdatesURL,AppVerName,AppVersion,' +
    'Attribs,BackColor,BackColor2,BackColorDirection,BackSolid,Bits,' +
    'ChangesAssociations,Check,CodeFile,Comment,Components,Compression,CompressLevel,CopyMode,'+
    'CreateAppDir,CreateUninstallRegKey,DefaultDirName,DefaultGroupName,' +
    'Description,DestDir,DestName,DirExistsWarning,DisableAppendDir,' +
    'DisableDirExistsWarning,DisableDirPage,DisableFinishedPage,' +
    'DisableProgramGroupPage,DisableReadyMemo,DisableReadyPage,' +
    'DisableStartupPrompt,DiskClusterSize,DiskSize,DiskSpaceMBLabel,' +
    'DiskSpanning,DontMergeDuplicateFiles,EnableDirDoesntExistWarning,' +
    'ExtraDiskSpaceRequired,Filename,Flags,FlatComponentsList,FontInstall,' +
    'GroupDescription,HotKey,IconFilename,IconIndex,InfoAfterFile,InfoBeforeFile,' +
    'InstallMode,InternalCompressLevel,Key,LicenseFile,MessagesFile,MinVersion,Name,' +
    'OnlyBelowVersion,OutputBaseFilename,OutputDir,OverwriteUninstRegEntries,' +
    'Parameters,Password,ReserveBytes,Root,RunOnceId,Section,' +
    'ShowComponentSizes,Source,SourceDir,StatusMsg,Subkey,Tasks,Type,Types,' +
    'UninstallDisplayIcon,UninstallDisplayName,UninstallFilesDir,' +
    'UninstallIconName,UninstallLogMode,UninstallStyle,Uninstallable,' +
    'UpdateUninstallLogAppName,UsePreviousAppDir,UsePreviousGroup,' +
    'UsePreviousTasks,UsePreviousSetupType,UseSetupLdr,ValueData,ValueName,' +
    'ValueType,WindowResizable,WindowShowCaption,WindowStartMaximized,' +
    'WindowVisible,WizardImageBackColor,WizardImageFile,WizardSmallImageFile,' +
    'WizardStyle,WorkingDir';

  Parameters: string =
    'HKCC,HKCR,HKCU,HKLM,HKU,alwaysoverwrite,alwaysskipifsameorolder,append,' +
    'binary,classic,closeonexit,comparetimestampalso,confirmoverwrite,' +
    'createkeyifdoesntexist,createonlyiffileexists,createvalueifdoesntexist,' +
    'deleteafterinstall,deletekey,deletevalue,dirifempty,dontcloseonexit,' +
    'dontcreatekey,disablenouninstallwarning,dword,exclusive,expandsz,' +
    'external,files,filesandordirs,fixed,fontisnttruetype,iscustom,isreadme,' +
    'modern,multisz,new,noerror,none,normal,nowait,onlyifdestfileexists,' +
    'onlyifdoesntexist,overwrite,overwritereadonly,postinstall,' +
    'preservestringtype,regserver,regtypelib,restart,restartreplace,' +
    'runmaximized,runminimized,sharedfile,shellexec,showcheckbox,' +
    'skipifnotsilent,skipifsilent,silent,skipifdoesntexist,' +
    'skipifsourcedoesntexist,unchecked,uninsalwaysuninstall,' +
    'uninsclearvalue,uninsdeleteentry,uninsdeletekey,uninsdeletekeyifempty,' +
    'uninsdeletesection,uninsdeletesectionifempty,uninsdeletevalue,' +
    'uninsneveruninstall,useapppaths,verysilent,waituntilidle';

  KeyOrParameter: string = 'string';

procedure MakeIdentTable;
var
  c: char;
begin
  FillChar(Identifiers, SizeOf(Identifiers), 0);
  for c := 'a' to 'z' do
    Identifiers[c] := TRUE;
  for c := 'A' to 'Z' do
    Identifiers[c] := TRUE;
  for c := '0' to '9' do
    Identifiers[c] := TRUE;
  Identifiers['_'] := TRUE;

  FillChar(mHashTable, SizeOf(mHashTable), 0);
  mHashTable['_'] := 1;
  for c := 'a' to 'z' do
    mHashTable[c] := 2 + Ord(c) - Ord('a');
  for c := 'A' to 'Z' do
    mHashTable[c] := 2 + Ord(c) - Ord('A');
end;

function TSynInnoSyn.KeyHash(ToHash: PChar): integer;
begin
  Result := 0;
  while Identifiers[ToHash^] do begin
{$IFOPT Q-}
    Result := 7 * Result + mHashTable[ToHash^];
{$ELSE}
    Result := (7 * Result + mHashTable[ToHash^]) and $FFFFFF;
{$ENDIF}
    inc(ToHash);
  end;
  Result := Result and $1FF; // 511
  fStringLen := ToHash - fToIdent;
end;

function TSynInnoSyn.KeyComp(const aKey: string): Boolean;
var
  i: integer;
  pKey1, pKey2: PChar;
begin
  pKey1 := fToIdent;
  // Note: fStringLen is always > 0 !
  pKey2 := pointer(aKey);
  for i := 1 to fStringLen do
  begin
    if mHashTable[pKey1^] <> mHashTable[pKey2^] then
    begin
      Result := FALSE;
      exit;
    end;
    Inc(pKey1);
    Inc(pKey2);
  end;
  Result := TRUE;
end;

function TSynInnoSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
begin
  fToIdent := MayBe;
  Entry := fKeywords[KeyHash(MayBe)];
  while Assigned(Entry) do begin
    if Entry.KeywordLen > fStringLen then
      break
    else if Entry.KeywordLen = fStringLen then
      if KeyComp(Entry.Keyword) then begin
        Result := TtkTokenKind(Entry.Kind);
        exit;
      end;
    Entry := Entry.Next;
  end;
  Result := tkIdentifier;
end;

procedure TSynInnoSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #13: fProcTable[I] := @CRProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := @IdentProc;
      #10: fProcTable[I] := @LFProc;
      #0: fProcTable[I] := @NullProc;
      '0'..'9': fProcTable[I] := @NumberProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := @SpaceProc;
      #59 {';'}: fProcTable[I] := @SemiColonProc;
      #61 {=} : fProcTable[I] := @EqualProc;
      #34: fProcTable[I] := @StringProc;
      '#', ':', ',', '(', ')': fProcTable[I] := @SymbolProc;
      '{': fProcTable[I] := @ConstantProc;
      #91 {[} : fProcTable[i] := @SectionProc;
    else
      fProcTable[I] := @UnknownProc;
    end;
end;

constructor TSynInnoSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fKeywords := TSynHashEntryList.Create;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clGray;
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fInvalidAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar);
  AddAttribute(fInvalidAttri);

  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := clNavy;
  AddAttribute(fKeyAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  fNumberAttri.Foreground := clMaroon;
  AddAttribute(fNumberAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  fStringAttri.Foreground := clBlue;
  AddAttribute(fStringAttri);

  fConstantAttri := TSynHighlighterAttributes.Create(SYNS_AttrDirective);
  fConstantAttri.Style := [fsBold, fsItalic];
  fConstantAttri.Foreground := clTeal;
  AddAttribute(fConstantAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);

  //Parameters
  fParamAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor);
  fParamAttri.Style := [fsBold];
  fParamAttri.Foreground := clOlive;
  AddAttribute(fParamAttri);

  fSectionAttri := TSynHighlighterAttributes.Create(SYNS_AttrSection);
  fSectionAttri.Style := [fsBold];
  fSectionAttri.Foreground := clRed;
  AddAttribute(fSectionAttri);

  SetAttributesOnChange(@DefHighlightChange);
  EnumerateKeywords(Ord(tkKey), Keywords, IdentChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkParameter), Parameters, IdentChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkKeyOrParameter), KeyOrParameter, IdentChars,
    @DoAddKeyword);
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterInno;
end;

destructor TSynInnoSyn.Destroy;
begin
  fKeywords.Free;
  inherited Destroy;
end;

procedure TSynInnoSyn.SetLine(const NewValue: string; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

function TSynInnoSyn.GetSampleSource :string;
begin
  Result:='; Inno Setup '+LineEnding+
          '; Copyright (C) 1997-2012 Jordan Russell. All rights reserved.'+LineEnding+
          '; Portions by Martijn Laan'+LineEnding+
          '; For conditions of distribution and use, see LICENSE.TXT.'+LineEnding+
          ';'+LineEnding+
          '; Setup script'+LineEnding+LineEnding+
          '[Setup]'+LineEnding+
          'AppName=Inno Setup'+LineEnding+
          'AppId=Inno Setup 5'+LineEnding+
          'AppVersion=5.5.5'+LineEnding+
          'AppPublisher=jrsoftware.org'+LineEnding+
          'AppPublisherURL=http://www.innosetup.com/'+LineEnding+
          'AppSupportURL=http://www.innosetup.com/'+LineEnding+
          'AppUpdatesURL=http://www.innosetup.com/'+LineEnding+
          'VersionInfoCopyright=Copyright (C) 1997-2012 Jordan Russell. Portions Copyright (C) 2000-2012 Martijn Laan.'+LineEnding+
          'AppMutex=InnoSetupCompilerAppMutex,Global\InnoSetupCompilerAppMutex'+LineEnding+
          'MinVersion=0,5.0'+LineEnding+
          'DefaultDirName={pf}\Inno Setup 5'+LineEnding+
          'DefaultGroupName=Inno Setup 5'+LineEnding+
          'AllowNoIcons=yes'+LineEnding+
          'Compression=lzma2/max'+LineEnding+
          'SolidCompression=yes'+LineEnding+
          'Uninstallable=not PortableCheck'+LineEnding+
          'UninstallDisplayIcon={app}\Compil32.exe'+LineEnding+
          'LicenseFile=license.txt'+LineEnding+
          'TimeStampsInUTC=yes'+LineEnding+
          'TouchDate=none'+LineEnding+
          'TouchTime=00:00'+LineEnding+
          'WizardImageFile=compiler:WizModernImage-IS.bmp'+LineEnding+
          'WizardSmallImageFile=compiler:WizModernSmallImage-IS.bmp'+LineEnding+
          '#ifndef NOSIGNTOOL'+LineEnding+
          'SignTool=issigntool'+LineEnding+
          'SignedUninstaller=yes'+LineEnding+
          '#endif'+LineEnding+LineEnding+
          '[Languages]'+LineEnding+
          'Name: en; MessagesFile: "files\Default.isl"'+LineEnding+
          'Name: br; MessagesFile: "files\Languages\BrazilianPortuguese.isl"'+LineEnding+
          'Name: ca; MessagesFile: "files\Languages\Catalan.isl"'+LineEnding+
          'Name: co; MessagesFile: "files\Languages\Corsican.isl"'+LineEnding+
          'Name: cz; MessagesFile: "files\Languages\Czech.isl"'+LineEnding+
          'Name: da; MessagesFile: "files\Languages\Danish.isl"'+LineEnding+
          'Name: nl; MessagesFile: "files\Languages\Dutch.isl"'+LineEnding+
          'Name: fi; MessagesFile: "files\Languages\Finnish.isl"'+LineEnding+
          'Name: fr; MessagesFile: "files\Languages\French.isl"'+LineEnding+
          'Name: de; MessagesFile: "files\Languages\German.isl"'+LineEnding+
          'Name: gr; MessagesFile: "files\Languages\Greek.isl"'+LineEnding+
          'Name: he; MessagesFile: "files\Languages\Hebrew.isl"'+LineEnding+
          'Name: hu; MessagesFile: "files\Languages\Hungarian.isl"'+LineEnding+
          'Name: it; MessagesFile: "files\Languages\Italian.isl"'+LineEnding+
          'Name: ja; MessagesFile: "files\Languages\Japanese.isl"'+LineEnding+
          '#ifdef UNICODE'+LineEnding+
          'Name: nep; MessagesFile: "files\Languages\Nepali.islu"'+LineEnding+
          '#endif'+LineEnding+
          'Name: no; MessagesFile: "files\Languages\Norwegian.isl"'+LineEnding+
          'Name: pl; MessagesFile: "files\Languages\Polish.isl"'+LineEnding+
          'Name: pt; MessagesFile: "files\Languages\Portuguese.isl"'+LineEnding+
          'Name: ru; MessagesFile: "files\Languages\Russian.isl"'+LineEnding+
          'Name: sg; MessagesFile: "files\Languages\ScottishGaelic.isl"'+LineEnding+
          'Name: se; MessagesFile: "files\Languages\SerbianLatin.isl"'+LineEnding+
          'Name: se2; MessagesFile: "files\Languages\SerbianCyrillic.isl"'+LineEnding+
          'Name: sl2; MessagesFile: "files\Languages\Slovenian.isl"'+LineEnding+
          'Name: sp; MessagesFile: "files\Languages\Spanish.isl"'+LineEnding+
          'Name: tu; MessagesFile: "files\Languages\Turkish.isl"'+LineEnding+
          'Name: uk; MessagesFile: "files\Languages\Ukrainian.isl"'+LineEnding+LineEnding+
          '[Messages]'+LineEnding+
          '; two "Setup" on the same line looks weird, so put a line break in between'+LineEnding+
          'en.WelcomeLabel1=Welcome to the Inno Setup%nSetup Wizard'+LineEnding+LineEnding+
          '[Tasks]'+LineEnding+
          'Name: desktopicon; Description: "{cm:CreateDesktopIcon}"; Flags: unchecked'+LineEnding+
          'Name: fileassoc; Description: "{cm:AssocFileExtension,Inno Setup,.iss}"'+LineEnding+LineEnding+
          '[InstallDelete]'+LineEnding+
          '; Remove Unicode-only files if needed'+LineEnding+
          '#ifndef UNICODE'+LineEnding+
          'Type: files; Name: "{app}\Languages\Nepali.islu"'+LineEnding+
          '#endif'+LineEnding+
          '; Remove ISPP files if needed'+LineEnding+
          'Type: files; Name: "{app}\ISPP.dll"; Check: not ISPPCheck'+LineEnding+
          'Type: files; Name: "{app}\ISPPBuiltins.iss"; Check: not ISPPCheck'+LineEnding+
          '; Remove old ISPP files'+LineEnding+
          'Type: files; Name: "{app}\ISCmplr.dls"'+LineEnding+
          'Type: files; Name: "{app}\Builtins.iss"'+LineEnding+
          '; Older versions created the desktop icon under {userdesktop}'+LineEnding+
          'Type: files; Name: "{userdesktop}\Inno Setup Compiler.lnk"'+LineEnding+LineEnding+
          '[Files]'+LineEnding+
          '; Files used by [Code] first so these can be quickly decompressed despite solid compression'+LineEnding+
          'Source: "files\ISPP.ico"; Flags: dontcopy'+LineEnding+
          '; Other files'+LineEnding+
          'Source: "license.txt"; DestDir: "{app}"; Flags: ignoreversion touch'+LineEnding+
          'Source: "ishelp\Staging\ISetup.chm"; DestDir: "{app}"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Compil32.exe"; DestDir: "{app}"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\isscint.dll"; DestDir: "{app}"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\ISCC.exe"; DestDir: "{app}"; Flags: ignoreversion touch; Check: not ISPPCheck'+LineEnding+
          'Source: "files\ISCmplr.dll"; DestDir: "{app}"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Setup.e32"; DestDir: "{app}"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\SetupLdr.e32"; DestDir: "{app}"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Default.isl"; DestDir: "{app}"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\BrazilianPortuguese.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\Catalan.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\Corsican.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\Czech.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\Danish.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\Dutch.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\French.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\Finnish.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\German.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\Greek.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\Hebrew.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\Hungarian.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\Italian.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\Japanese.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          '#ifdef UNICODE'+LineEnding+
          'Source: "files\Languages\Nepali.islu"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          '#endif'+LineEnding+
          'Source: "files\Languages\Norwegian.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\Polish.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\Portuguese.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\Russian.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\ScottishGaelic.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\SerbianCyrillic.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\SerbianLatin.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\Slovenian.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\Spanish.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\Turkish.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\Languages\Ukrainian.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\WizModernImage.bmp"; DestDir: "{app}"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\WizModernImage-IS.bmp"; DestDir: "{app}"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\WizModernSmallImage.bmp"; DestDir: "{app}"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\WizModernSmallImage-IS.bmp"; DestDir: "{app}"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\iszlib.dll"; DestDir: "{app}"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\isunzlib.dll"; DestDir: "{app}"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\isbzip.dll"; DestDir: "{app}"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\isbunzip.dll"; DestDir: "{app}"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\islzma.dll"; DestDir: "{app}"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\islzma32.exe"; DestDir: "{app}"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\islzma64.exe"; DestDir: "{app}"; Flags: ignoreversion touch'+LineEnding+
          'Source: "whatsnew.htm"; DestDir: "{app}"; Flags: ignoreversion touch'+LineEnding+
          'Source: "ishelp\isfaq.htm"; DestDir: "{app}"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\Example1.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\Example2.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\Example3.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\64Bit.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\64BitThreeArch.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\64BitTwoArch.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\Components.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\Languages.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\MyProg.exe"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\MyProg-x64.exe"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\MyProg-IA64.exe"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\MyProg.chm"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\Readme.txt"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\Readme-Dutch.txt"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\Readme-German.txt"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\CodeExample1.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\CodeDlg.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\CodeClasses.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\CodeDll.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\CodeAutomation.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\CodeAutomation2.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\CodePrepareToInstall.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\UninstallCodeExample1.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\MyDll.dll"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\MyDll\C\MyDll.c"; DestDir: "{app}\Examples\MyDll\C"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\MyDll\C\MyDll.def"; DestDir: "{app}\Examples\MyDll\C"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\MyDll\C\MyDll.dsp"; DestDir: "{app}\Examples\MyDll\C"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\MyDll\Delphi\MyDll.dpr"; DestDir: "{app}\Examples\MyDll\Delphi"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\ISPPExample1.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          'Source: "Examples\ISPPExample1License.txt"; DestDir: "{app}\Examples"; Flags: ignoreversion touch'+LineEnding+
          '; ISPP files'+LineEnding+
          'Source: "Projects\ISPP\Help\Staging\ISPP.chm"; DestDir: "{app}"; Flags: ignoreversion touch'+LineEnding+
          'Source: "files\ISPPCC.exe"; DestDir: "{app}"; DestName: "ISCC.exe"; Flags: ignoreversion touch; Check: ISPPCheck'+LineEnding+
          'Source: "files\ISPP.dll"; DestDir: "{app}"; Flags: ignoreversion touch; Check: ISPPCheck'+LineEnding+
          'Source: "files\ISPPBuiltins.iss"; DestDir: "{app}"; Flags: ignoreversion touch; Check: ISPPCheck'+LineEnding+LineEnding+
          '[Icons]'+LineEnding+
          'Name: "{group}\Inno Setup Compiler"; Filename: "{app}\Compil32.exe"; WorkingDir: "{app}"; AppUserModelID: "JR'+
          '.InnoSetup.IDE.5"'+LineEnding+
          'Name: "{group}\Inno Setup Documentation"; Filename: "{app}\ISetup.chm"'+LineEnding+
          'Name: "{group}\Inno Setup Example Scripts"; Filename: "{app}\Examples\"'+LineEnding+
          'Name: "{group}\Inno Setup FAQ"; Filename: "{app}\isfaq.htm"'+LineEnding+
          'Name: "{group}\Inno Setup Revision History"; Filename: "{app}\whatsnew.htm"'+LineEnding+
          'Name: "{commondesktop}\Inno Setup Compiler"; Filename: "{app}\Compil32.exe"; WorkingDir: "{app}"; AppUserMode'+
          'lID: "JR.InnoSetup.IDE.5"; Tasks: desktopicon'+LineEnding+LineEnding+
          '[Run]'+LineEnding+
          'Filename: "{app}\Compil32.exe"; Parameters: "/ASSOC"; StatusMsg: "{cm:AssocingFileExtension,Inno Setup,.iss}"'+
          '; Tasks: fileassoc'+LineEnding+
          'Filename: "{app}\Compil32.exe"; WorkingDir: "{app}"; Description: "{cm:LaunchProgram,Inno Setup}"; Flags: now'+
          'ait postinstall skipifsilent'+LineEnding+LineEnding+
          '[UninstallRun]'+LineEnding+
          'Filename: "{app}\Compil32.exe"; Parameters: "/UNASSOC"; RunOnceId: "RemoveISSAssoc"'+LineEnding+LineEnding+
          '[CustomMessages]'+LineEnding+
          'ISPPTitle=Inno Setup Preprocessor'+LineEnding+
          'ISPPSubtitle=Would you like to install Inno Setup Preprocessor?'+LineEnding+
          'ISPPText=Inno Setup Preprocessor (ISPP) is an official add-on for Inno Setup. ISPP allows you to conditionall'+
          'y compile parts of scripts, to use compile time variables in your scripts and to use built-in functions which'+
          'for example can read from the registry or INI files at compile time.%n%nISPP also contains a special version '+
          'of the ISCC command line compiler which can take variable definitions as command line parameters and use them'+
          'during compilation.'+LineEnding+
          'ISPPText2=Select whether you would like to install ISPP, then click Next.'+LineEnding+
          'ISPPCheck=&Install Inno Setup Preprocessor'+LineEnding+LineEnding+
          '[Code]'+LineEnding+
          'var'+LineEnding+
          '  ISPPPage: TWizardPage;'+LineEnding+
          '  ISPPCheckBox: TCheckBox;'+LineEnding+LineEnding+
          'function GetModuleHandle(lpModuleName: LongInt): LongInt;'+LineEnding+
          'external ''GetModuleHandleA@kernel32.dll stdcall'';'+LineEnding+
          'function ExtractIcon(hInst: LongInt; lpszExeFileName: AnsiString; nIconIndex: LongInt): LongInt;'+LineEnding+
          'external ''ExtractIconA@shell32.dll stdcall'';'+LineEnding+
          'function DrawIconEx(hdc: LongInt; xLeft, yTop: Integer; hIcon: LongInt; cxWidth, cyWidth: Integer; istepIfAni'+
          'Cur: LongInt; hbrFlickerFreeDraw, diFlags: LongInt): LongInt;'+LineEnding+
          'external ''DrawIconEx@user32.dll stdcall'';'+LineEnding+
          'function DestroyIcon(hIcon: LongInt): LongInt;'+LineEnding+
          'external ''DestroyIcon@user32.dll stdcall'';'+LineEnding+LineEnding+
          'const'+LineEnding+
          '  DI_NORMAL = 3;'+LineEnding+LineEnding+
          'function CreateCustomOptionPage(AAfterId: Integer; ACaption, ASubCaption, AIconFileName, ALabel1Caption, ALab'+
          'el2Caption,'+LineEnding+
          '  ACheckCaption: String; var CheckBox: TCheckBox): TWizardPage;'+LineEnding+
          'var'+LineEnding+
          '  Page: TWizardPage;'+LineEnding+
          '  Rect: TRect;'+LineEnding+
          '  hIcon: LongInt;'+LineEnding+
          '  Label1, Label2: TNewStaticText;'+LineEnding+
          'begin'+LineEnding+
          '  Page := CreateCustomPage(AAfterID, ACaption, ASubCaption);'+LineEnding+LineEnding+
          '  try'+LineEnding+
          '    AIconFileName := ExpandConstant(''{tmp}\'' + AIconFileName);'+LineEnding+
          '    if not FileExists(AIconFileName) then'+LineEnding+
          '      ExtractTemporaryFile(ExtractFileName(AIconFileName));'+LineEnding+LineEnding+
          '    Rect.Left := 0;'+LineEnding+
          '    Rect.Top := 0;'+LineEnding+
          '    Rect.Right := 32;'+LineEnding+
          '    Rect.Bottom := 32;'+LineEnding+LineEnding+
          '    hIcon := ExtractIcon(GetModuleHandle(0), AIconFileName, 0);'+LineEnding+
          '    try'+LineEnding+
          '      with TBitmapImage.Create(Page) do begin'+LineEnding+
          '        with Bitmap do begin'+LineEnding+
          '          Width := 32;'+LineEnding+
          '          Height := 32;'+LineEnding+
          '          Canvas.Brush.Color := WizardForm.Color;'+LineEnding+
          '          Canvas.FillRect(Rect);'+LineEnding+
          '          DrawIconEx(Canvas.Handle, 0, 0, hIcon, 32, 32, 0, 0, DI_NORMAL);'+LineEnding+
          '        end;'+LineEnding+
          '        Parent := Page.Surface;'+LineEnding+
          '      end;'+LineEnding+
          '    finally'+LineEnding+
          '      DestroyIcon(hIcon);'+LineEnding+
          '    end;'+LineEnding+
          '  except'+LineEnding+
          '  end;'+LineEnding+LineEnding+
          '  Label1 := TNewStaticText.Create(Page);'+LineEnding+
          '  with Label1 do begin'+LineEnding+
          '    AutoSize := False;'+LineEnding+
          '    Left := WizardForm.SelectDirLabel.Left;'+LineEnding+
          '    Width := Page.SurfaceWidth - Left;'+LineEnding+
          '    WordWrap := True;'+LineEnding+
          '    Caption := ALabel1Caption;'+LineEnding+
          '    Parent := Page.Surface;'+LineEnding+
          '  end;'+LineEnding+
          '  WizardForm.AdjustLabelHeight(Label1);'+LineEnding+LineEnding+
          '  Label2 := TNewStaticText.Create(Page);'+LineEnding+
          '  with Label2 do begin'+LineEnding+
          '    Top := Label1.Top + Label1.Height + ScaleY(12);'+LineEnding+
          '    Caption := ALabel2Caption;'+LineEnding+
          '    Parent := Page.Surface;'+LineEnding+
          '  end;'+LineEnding+
          '  WizardForm.AdjustLabelHeight(Label2);'+LineEnding+LineEnding+
          '  CheckBox := TCheckBox.Create(Page);'+LineEnding+
          '  with CheckBox do begin'+LineEnding+
          '    Top := Label2.Top + Label2.Height + ScaleY(12);'+LineEnding+
          '    Width := Page.SurfaceWidth;'+LineEnding+
          '    Caption := ACheckCaption;'+LineEnding+
          '    Parent := Page.Surface;'+LineEnding+
          '  end;'+LineEnding+LineEnding+
          '  Result := Page;'+LineEnding+
          'end;'+LineEnding+LineEnding+
          'procedure CreateCustomPages;'+LineEnding+
          'var'+LineEnding+
          '  Caption, SubCaption1, IconFileName, Label1Caption, Label2Caption, CheckCaption: String;'+LineEnding+
          'begin'+LineEnding+
          '  Caption := CustomMessage(''ISPPTitle'');'+LineEnding+
          '  SubCaption1 := CustomMessage(''ISPPSubtitle'');'+LineEnding+
          '  IconFileName := ''ISPP.ico'';'+LineEnding+
          '  Label1Caption := CustomMessage(''ISPPText'');'+LineEnding+
          '  Label2Caption := CustomMessage(''ISPPText2'');'+LineEnding+
          '  CheckCaption := CustomMessage(''ISPPCheck'');'+LineEnding+LineEnding+
          '  ISPPPage := CreateCustomOptionPage(wpSelectProgramGroup, Caption, SubCaption1, IconFileName, Label1Caption,'+
          ' Label2Caption, CheckCaption, ISPPCheckBox);'+LineEnding+
          'end;'+LineEnding+LineEnding+
          'procedure InitializeWizard;'+LineEnding+
          'begin'+LineEnding+
          '  CreateCustomPages;'+LineEnding+LineEnding+
          '  ISPPCheckBox.Checked := (GetPreviousData(''ISPP'', ''1'') = ''1'') or (ExpandConstant(''{param:ispp|0}'') ='+
          ' ''1'');'+LineEnding+
          'end;'+LineEnding+LineEnding+
          'procedure RegisterPreviousData(PreviousDataKey: Integer);'+LineEnding+
          'begin'+LineEnding+
          '  SetPreviousData(PreviousDataKey, ''ISPP'', IntToStr(Ord(ISPPCheckBox.Checked)));'+LineEnding+
          'end;'+LineEnding+LineEnding+
          'function ISPPCheck: Boolean;'+LineEnding+
          'begin'+LineEnding+
          '  Result := ISPPCheckBox.Checked;'+LineEnding+
          'end;'+LineEnding+LineEnding+
          'function PortableCheck: Boolean;'+LineEnding+
          'begin'+LineEnding+
          '  Result := ExpandConstant(''{param:portable|0}'') = ''1'';'+LineEnding+
          'end;'+LineEnding+LineEnding
;
end;

procedure TSynInnoSyn.SymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
end;

procedure TSynInnoSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then inc(Run);
end;

procedure TSynInnoSyn.EqualProc;
begin
// If any word has equal (=) symbol,
// then the immediately followed text is treated as string
// (though it does not have quotes)
  fTokenID := tkString;
  repeat
    Inc(Run);
    if fLine[Run] = ';' then begin
      Inc(Run);
      break;
    end;
  until fLine[Run] in [#0, #10, #13];
end;

procedure TSynInnoSyn.IdentProc;
var
  LookAhead: integer;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  if fTokenID = tkKeyOrParameter then begin
    LookAhead := Run;
    while fLine[LookAhead] in [#9, ' '] do
      Inc(LookAhead);
    if fLine[LookAhead] = ':' then
      fTokenID := tkKey
    else
      fTokenID := tkParameter;
  end;
end;

procedure TSynInnoSyn.SectionProc;
begin
  // if it is not column 0 mark as tkParameter and get out of here
  if Run > 0 then
  begin
    fTokenID := tkUnknown;
    inc(Run);
    Exit;
  end;

  // this is column 0 ok it is a Section
  fTokenID := tkSection;
  repeat
    Inc(Run);
    if fLine[Run] = ']' then
    begin
      Inc(Run);
      break;
    end;
  until fLine[Run] in [#0, #10, #13];
end;

procedure TSynInnoSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynInnoSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynInnoSyn.NumberProc;
begin
  fTokenID := tkNumber;
  repeat
    Inc(Run);
  until not (fLine[Run] in ['0'..'9']);
end;

procedure TSynInnoSyn.ConstantProc;
var
  BraceLevel, LastOpenBrace: Integer;
begin
  { Much of this is based on code from the SkipPastConst function in IS's
    CmnFunc2 unit. [jr] }
  if fLine[Run + 1] = '{' then begin
    fTokenID := tkUnknown;
    Inc(Run, 2);
    Exit;
  end;
  fTokenID := tkConstant;
  BraceLevel := 1;
  LastOpenBrace := Low(Integer);
  repeat
    Inc(Run);
    case fLine[Run] of
      '{': begin
             if LastOpenBrace <> Run-1 then begin
               Inc(BraceLevel);
               LastOpenBrace := Run;
             end
             else
               Dec(BraceLevel);
           end;
      '}': begin
             Dec (BraceLevel);
             if BraceLevel = 0 then begin
               Inc(Run);
               Break;
             end;
           end;
    end;
  until fLine[Run] in [#0, #10, #13];
end;

procedure TSynInnoSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or (fLine[Run] in [#0, #10, #13]);
end;

procedure TSynInnoSyn.SemiColonProc;
var
  I: Integer;
begin
  for I := Run-1 downto 0 do
    if fLine[I] > ' ' then begin
      // If the semicolon is not the first non-whitespace character on the
      // line, then it isn't the start of a comment.
      fTokenID := tkUnknown;
      inc(Run);
      Exit;
    end;
  fTokenID := tkComment;
  repeat
    Inc(Run);
  until (fLine[Run] in [#0, #10, #13]);
end;

procedure TSynInnoSyn.StringProc;
begin
  fTokenID := tkString;
  repeat
    Inc(Run);
    if fLine[Run] = '"' then begin
      Inc(Run);
      if fLine[Run] <> '"' then // embedded "" does not end the string
        break;
    end;
  until fLine[Run] in [#0, #10, #13];
end;

procedure TSynInnoSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynInnoSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TSynInnoSyn.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynInnoSyn.GetEol: Boolean;
begin
  Result := (fTokenId = tkNull);
end;

function TSynInnoSyn.GetToken :string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

{$IFDEF SYN_LAZARUS}
procedure TSynInnoSyn.GetTokenEx(out TokenStart :PChar; out TokenLength :integer);
begin
  TokenLength := Run - fTokenPos;
  TokenStart  := FLine + fTokenPos;
end;
{$ENDIF}

function TSynInnoSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkParameter: Result := fParamAttri;
    tkSection: Result := fSectionAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkConstant: Result := fConstantAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynInnoSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynInnoSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynInnoSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynInnoSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSynInnoSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterInno;
end;

class function TSynInnoSyn.GetLanguageName: string;
begin
  Result := SYNS_LangInno;
end;

procedure TSynInnoSyn.DoAddKeyword(AKeyword: string; AKind: integer);
var
  HashValue: integer;
begin
  HashValue := KeyHash(PChar(AKeyword));
  fKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

initialization
  MakeIdentTable;
  RegisterPlaceableHighlighter(TSynInnoSyn);

end.
