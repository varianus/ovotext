{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighter8051.pas, the Initial
Author of this file is Zhou Kan.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynHighlighter8051.pas,v 1.00 2005/01/24 17:58:27 Kan Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a 8051 Assembler highlighter for SynEdit)
@author(Zhou Kan [textrush@tom.com])
@created(June 2004)
@lastmod(2005-01-24)
The SynHighlighter8051 unit provides SynEdit with a 8051 Assembler (*.a51;*.asm;*.s03) highlighter.
The highlighter formats 8051 source code highlighting keywords, strings, numbers and characters.
}

unit SynHighlighter8051;

  //SynDefines.inc is the synedit.inc from laz 1.2.0 synedit package source if it has changed
  //in newer version you might need to copy it again. REmeber to redclare the syn_lazarus define.
{$I SynDefines.inc}

interface

uses
  SysUtils, Classes, Graphics, SynEditHighlighter, SynEditTypes, 
  SynEditStrConst, SynHighlighterHashEntries;

type
  TtkTokenKind = (tkComment, tkDirective, tkIdentifier, tkKey, tkNull, 
    tkNumber, tkRegister, tkSpace, tkString, tkSymbol, tkUnknown); //Kan

  TProcTableProc = procedure of object;

type
  TSyn8051Syn = class(TSynCustomHighlighter)
  private
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fDirectiveAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fRegisterAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fKeywords: TSynHashEntryList;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    procedure CommentProc;
    procedure CRProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SingleQuoteStringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    procedure DoAddKeyword(AKeyword: string; AKind: integer);
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function IsFilterStored :boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(const NewValue: String; LineNumber:Integer); override;
    function GetToken: String; override;
   {$IFDEF SYN_LAZARUS}
   procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
   {$ENDIF}
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    class function GetLanguageName :string; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property DirectiveAttri: TSynHighlighterAttributes read fDirectiveAttri write fDirectiveAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property RegisterAttri: TSynHighlighterAttributes read fRegisterAttri write fRegisterAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
  end;

implementation
uses
  SynEditStrConstExtra;

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;

//Keywords
const
  OpCodes: string = 
    'acall,add,addc,ajmp,anl,call,cjne,clr,cpl,da,dec,div,djnz,inc,jb,jbc,jc,jmp,' +
    'jnc,jnb,jnz,jz,lcall,ljmp,mov,movc,movx,mul,nop,orl,pop,push,ret,reti,rl,rlc,' +
    'rr,rrc,setb,sjmp,subb,swap,xch,xrl';
  
  RegCodes: string = 
    'a,ab,acc,b,c,dph,dpl,dptr,r0,r1,r2,r3,r4,r5,r6,r7,sp,psw';

  DirectCodes: string = 
    'aseg,bseg,common,cseg,db,dbit,ds,dseg,dw,end,endif,endmod,else,equ,extern,' +
    'extrn,high,iseg,low,lstpag,module,name,org,page,pagsiz,public,rseg,segment,' +
    'set,titel,titl,using,xseg';

procedure MakeIdentTable;
var
  c: char;
begin
  FillChar(Identifiers, SizeOf(Identifiers), 0);
  for c := 'a' to 'z' do
    Identifiers[c] := True;
  for c := 'A' to 'Z' do
    Identifiers[c] := True;
  for c := '0' to '9' do
    Identifiers[c] := True;
  Identifiers['_'] := True;

  FillChar(mHashTable, SizeOf(mHashTable), 0);
  for c := 'a' to 'z' do
    mHashTable[c] := 1 + Ord(c) - Ord('a');
  for c := 'A' to 'Z' do
    mHashTable[c] := 1 + Ord(c) - Ord('A');
  for c := '0' to '9' do
    mHashTable[c] := 27 + Ord(c) - Ord('0');
end;

function TSyn8051Syn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while Identifiers[ToHash^] do
  begin
{$IFOPT Q-}
    Result := 7 * Result + mHashTable[ToHash^];
{$ELSE}
    Result := (7 * Result + mHashTable[ToHash^]) and $FFFFFF;
{$ENDIF}
    inc(ToHash);
  end;
  Result := Result and $3FF;
  fStringLen := ToHash - fToIdent;
end;

function TSyn8051Syn.KeyComp(const aKey: String): Boolean;
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

procedure TSyn8051Syn.DoAddKeyword(AKeyword: string; AKind: integer);
var
  HashValue: integer;
begin
  HashValue := KeyHash(PChar(AKeyword));
  fKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

function TSyn8051Syn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
begin
  fToIdent := MayBe;
  Entry := fKeywords[KeyHash(MayBe)];
  while Assigned(Entry) do 
  begin
    if Entry.KeywordLen > fStringLen then
      break
    else if Entry.KeywordLen = fStringLen then
      if KeyComp(Entry.Keyword) then 
      begin
        Result := TtkTokenKind(Entry.Kind);
        exit;
      end;
    Entry := Entry.Next;
  end;
  Result := tkIdentifier;
end;

procedure TSyn8051Syn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
       #0 : fProcTable[I] := {$IFDEF FPC}@{$ENDIF}NullProc;
      #10 : fProcTable[I] := {$IFDEF FPC}@{$ENDIF}LFProc;
      #13 : fProcTable[I] := {$IFDEF FPC}@{$ENDIF}CRProc;
      #34 : fProcTable[I] := {$IFDEF FPC}@{$ENDIF}StringProc;
      #39 : fProcTable[I] := {$IFDEF FPC}@{$ENDIF}SingleQuoteStringProc;
      '>' : fProcTable[I] := {$IFDEF FPC}@{$ENDIF}GreaterProc;
      '<' : fProcTable[I] := {$IFDEF FPC}@{$ENDIF}LowerProc;
      '/' : fProcTable[I] := {$IFDEF FPC}@{$ENDIF}SlashProc;
      'A'..'Z', 'a'..'z', '_':
        fProcTable[I] := {$IFDEF FPC}@{$ENDIF}IdentProc;
      '0'..'9':
        fProcTable[I] := {$IFDEF FPC}@{$ENDIF}NumberProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[I] := {$IFDEF FPC}@{$ENDIF}SpaceProc;
      ';':
        fProcTable[I] := {$IFDEF FPC}@{$ENDIF}CommentProc;
      '.', ':', '&', '{', '}', '=', '^', '-', '+', '(', ')', '*', '#':
        fProcTable[I] := {$IFDEF FPC}@{$ENDIF}SymbolProc;
      else
        fProcTable[I] := {$IFDEF FPC}@{$ENDIF}UnknownProc;
    end;
end;

constructor TSyn8051Syn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fKeywords := TSynHashEntryList.Create;

  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Foreground  := clGreen;
  AddAttribute(fCommentAttri);
  
  fDirectiveAttri := TSynHighLighterAttributes.Create(SYNS_AttrDirective);
  fDirectiveAttri.Foreground  := $00A00000;
  AddAttribute(fDirectiveAttri);
  
  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier);
  fIdentifierAttri.Foreground := clWindowText;
  AddAttribute(fIdentifierAttri);
  
  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Foreground := clBlue;
  AddAttribute(fKeyAttri);
  
  fNumberAttri := TSynHighLighterAttributes.Create(SYNS_AttrNumber);
  fNumberAttri.Foreground := clPurple;
  AddAttribute(fNumberAttri);
  
  fRegisterAttri := TSynHighLighterAttributes.Create(SYNS_AttrRegister);
  fRegisterAttri.Foreground := $00C05000;
  AddAttribute(fRegisterAttri);
  
  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrWhitespace);
  AddAttribute(fSpaceAttri);
  
  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString);
  fStringAttri.Foreground := clMaroon;
  AddAttribute(fStringAttri);
  
  fSymbolAttri := TSynHighLighterAttributes.Create(SYNS_AttrSymbol);
  fSymbolAttri.Foreground := clNavy;
  AddAttribute(fSymbolAttri);

  MakeMethodTables;
  
  //Keywords list   //Kan
  EnumerateKeywords(Ord(tkKey),       OpCodes,     IdentChars, {$IFDEF FPC}@{$ENDIF}DoAddKeyword);
  EnumerateKeywords(Ord(tkRegister),  RegCodes,    IdentChars, {$IFDEF FPC}@{$ENDIF}DoAddKeyword);
  EnumerateKeywords(Ord(tkDirective), DirectCodes, IdentChars, {$IFDEF FPC}@{$ENDIF}DoAddKeyword);
  
  SetAttributesOnChange({$IFDEF FPC}@{$ENDIF}DefHighlightChange);
  fDefaultFilter:= SYNS_FilterX86Asm;
end;

destructor TSyn8051Syn.Destroy;
begin
  fKeywords.Free;
  inherited Destroy;
end;

procedure TSyn8051Syn.SetLine(const NewValue :String; LineNumber :Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSyn8051Syn.CommentProc;
begin
  fTokenID := tkComment;
  repeat
    Inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

procedure TSyn8051Syn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSyn8051Syn.GreaterProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fLine[Run] = '=' then Inc(Run);
end;

procedure TSyn8051Syn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSyn8051Syn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSyn8051Syn.LowerProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fLine[Run] in ['=', '>'] then Inc(Run);
end;

procedure TSyn8051Syn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSyn8051Syn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'a'..'f', 'h', 'A'..'F', 'H'] do
    Inc(Run);
end;

procedure TSyn8051Syn.SlashProc;
begin
  Inc(Run);
  if fLine[Run] = '/' then begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end else
    fTokenID := tkSymbol;
end;

procedure TSyn8051Syn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or (fLine[Run] in [#0, #10, #13]);
end;

procedure TSyn8051Syn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then
    inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSyn8051Syn.SingleQuoteStringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then
    inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSyn8051Syn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSyn8051Syn.UnknownProc;
begin
  {$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run,2)
  else
  {$ENDIF}
  Inc(Run);
  fTokenID := tkIdentifier;
end;

procedure TSyn8051Syn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

class function TSyn8051Syn.GetLanguageName :string;
begin
  Result := SYNS_Lang8051;
end;

function TSyn8051Syn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TSyn8051Syn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSyn8051Syn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

{$IFDEF SYN_LAZARUS}
procedure TSyn8051Syn.GetTokenEx(out TokenStart :PChar; out TokenLength :integer);
begin
  TokenLength := Run - fTokenPos;
  TokenStart  := FLine + fTokenPos;
end;
{$ENDIF}

function TSyn8051Syn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkDirective: Result := fDirectiveAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkRegister: Result := fRegisterAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSyn8051Syn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSyn8051Syn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSyn8051Syn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSyn8051Syn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSyn8051Syn.IsFilterStored :boolean;
begin
  Result := (fDefaultFilter <> SYNS_FilterX86Asm);
end;

initialization
  MakeIdentTable;
  {$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSyn8051Syn);
  {$ENDIF}
end.
