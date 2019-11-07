(**

  This module contains a SynEdit highlighter for the Markdown language.

  @Author  David Hoyle
  @Version 1.0
  @Date    06 Nov 2018

**)
Unit SynHighlighterMD;

{$I SynDefines.inc}

Interface

Uses
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SysUtils,
  Classes;

Type
  (** An enumerate to define the types of token that can be highlighted. **)
  TMDTokenKind = (
    tkMDLineComment,
    tkMDSubheading,
    tkMDItalic,
    tkMDBold,
    tkMDMonospace,
    tkMDBullet,
    tkMDNumber,
    tkMDCode,
    //: @todo [link](http://example.com)
    //: @todo [Image](Image_icon.png)
    //: @todo In-line HTML support
    tkMDSpace,
    tkMDSymbol,
    tkMDText
  );

  (** An enumerate to define the token ranges in the text. **)
  TMDRangeState = (
    rsMDLineComment,
    rsMDSubHeading,
    rsMDItalic,
    rsMDBold,
    rsMDMonospace,
    rsMDBullet,
    rsMDNumber,
    rsMDText,
    rsMDCode
  );

  (** A pointer to a function table procedure. **)
  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  (** A function table procedure. **)
  TIdentFuncTableFunc = Function(Const Index: Integer): TMDTokenKind Of Object;

Type
  (** A class to define the Markdown language highlighter. **)

  { TSynMDSyn }

  TSynMDSyn = Class(TSynCustomHighlighter)
  Private
    fLine: PChar;
    fLineRef :string;
    fLineNumber: Integer;
    fLineLen :Integer;

    FRange:          TMDRangeState;
    FTokenID:        TMDTokenKind;
    fTokenPos :Integer;
    FAttributeChar: WideChar;
    FIdentFuncTable: Array [0 .. 1] Of TIdentFuncTableFunc;
    FTokenAttri:     Array [Low(TMDTokenKind) .. High(TMDTokenKind)] Of TSynHighlighterAttributes;
    procedure EscapeProc;
    function IsLineEnd(_Run: Integer): Boolean;
    function MatchingAttribute(const iIndex: Integer): Boolean;
    procedure MDCodeOpenProc;
    procedure MDCodeProc;
   Protected
    Run: LongInt;
    Function  FuncKeyword(Const Index: Integer): TMDTokenKind;
    Function  AltFunc(Const Index: Integer): TMDTokenKind;
    Procedure InitIdent;
    Procedure NullProc;
    Procedure SpaceProc;
    Procedure CRProc;
    Procedure LFProc;
    Procedure MDCommentSubHeadingOpenProc;
    Procedure MDGenericLineProc(Const eTokenID : TMDTokenKind);
    Procedure MDItalicOpenProc;
    Procedure MDItalicProc;
    Procedure MDBoldBulletOpenProc;
    Procedure MDBoldProc;
    Procedure MDNumberOpenProc;
    Procedure MDMonospaceOpenProc;
    Procedure MDMonospaceProc;
    procedure MDTextAttibuteOpenProc;
    Procedure SymbolProc;
    Procedure TextProc;
    procedure SetLine(const NewValue: string; LineNumber: Integer); override;
    Function  GetSampleSource: String; Override;
    Function  IsFilterStored: Boolean; Override;
    Procedure AddAndUpdateAttributes(Const _Attribute: TSynHighlighterAttributes; Const ForeColour,
      BackColour: TColor; Const FontStyle: TFontStyles);
    Function  GetAttri(Const eTokenKind: TMDTokenKind): TSynHighlighterAttributes;
    Procedure SetAttri(Const eTokenKind: TMDTokenKind; Const Attri: TSynHighlighterAttributes);
    Function  IsFirstCharOnLine(Const iIndex : Integer) : Boolean;
    function GetTokenPos: Integer; override;
    function GetToken: string; override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Class Function GetFriendlyLanguageName: String;
    Class Function GetLanguageName: String; Override;
    Function GetRange: Pointer; Override;
    Procedure ResetRange; Override;
    Procedure SetRange(Value: Pointer); Override;
    Function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; Override;
    Function GetEol: Boolean; Override;
    Function GetTokenID: TMDTokenKind;
    Function GetTokenAttribute: TSynHighlighterAttributes; Override;
    Function GetTokenKind: Integer; Override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    Function IsIdentChar(AChar: WideChar): Boolean;
    Procedure Next; Override;
  Published
    (**
      A property to hold the MD Comment character attributes.
      @precon  None.
      @postcon Gets and sets the MD Comment character attributes.
      @return  a TSynHighlighterAttributes
    **)
    Property Comment: TSynHighlighterAttributes Index tkMDLineComment Read GetAttri Write SetAttri;
    (**
      A property to hold the Subheading character attributes.
      @precon  None.
      @postcon Gets and sets the Subheading character attributes.
      @return  a TSynHighlighterAttributes
    **)
    Property SubHeading: TSynHighlighterAttributes Index tkMDSubheading Read GetAttri Write SetAttri;
    (**
      A property to hold the Italic character attributes.
      @precon  None.
      @postcon Gets and sets the Italic character attributes.
      @return  a TSynHighlighterAttributes
    **)
    Property Italic : TSynHighlighterAttributes Index tkMDItalic Read GetAttri Write SetAttri;
    (**
      A property to hold the Bold character attributes.
      @precon  None.
      @postcon Gets and sets the Bold character attributes.
      @return  a TSynHighlighterAttributes
    **)
    Property Bold : TSynHighlighterAttributes Index tkMDBold Read GetAttri Write SetAttri;
    (**
      A property to hold the Monospace character attributes.
      @precon  None.
      @postcon Gets and sets the Monospace character attributes.
      @return  a TSynHighlighterAttributes
    **)
    Property Monospace : TSynHighlighterAttributes Index tkMDMonospace Read GetAttri Write SetAttri;
    (**
      A property to hold the Bullet character attributes.
      @precon  None.
      @postcon Gets and sets the Bullet character attributes.
      @return  a TSynHighlighterAttributes
    **)
    Property Bullet : TSynHighlighterAttributes Index tkMDBullet Read GetAttri Write SetAttri;
    (**
      A property to hold the Number character attributes.
      @precon  None.
      @postcon Gets and sets the Number character attributes.
      @return  a TSynHighlighterAttributes
    **)
    Property Number : TSynHighlighterAttributes Index tkMDNumber Read GetAttri Write SetAttri;
    (**
      A property to hold the space character attributes.
      @precon  None.
      @postcon Gets and sets the space character attributes.
      @return  a TSynHighlighterAttributes
    **)
    Property Space: TSynHighlighterAttributes Index tkMDSpace Read GetAttri Write SetAttri;
    (**
      A property to hold the symbol character attributes.
      @precon  None.
      @postcon Gets and sets the symbol character attributes.
      @return  a TSynHighlighterAttributes
    **)
    Property Symbols: TSynHighlighterAttributes Index tkMDSymbol Read GetAttri Write SetAttri;
    (**
      A property to hold the text character attributes.
      @precon  None.
      @postcon Gets and sets the text character attributes.
      @return  a TSynHighlighterAttributes
    **)
    Property Text: TSynHighlighterAttributes Index tkMDText Read GetAttri Write SetAttri;
  End;

const
  (** A resource string for the mark down default file filter. **)
  SYNS_FilterMarkdown = 'Markdown Files (*.md)|*.md';

Implementation

Uses
  {$IFDEF SYN_CLX}
  QSynEditStrConst;
  {$ELSE}
  SynEditStrConst;
{$ENDIF}


const
  (** A resource string for the language name. **)
  SYNS_LangMarkdown = 'Markdown';
  (** A resource string for the Comment attribute name. **)
  SYNS_AttrComment = 'Comment';
  (** A resource string for the SubHeading attribute name. **)
  SYNS_AttrSubheading = 'Subheading';
  (** A resource string for the Italic attribute name. **)
  SYNS_AttrItalic = 'Italic';
  (** A resource string for the Bold attribute name. **)
  SYNS_AttrBold = 'Bold';
  (** A resource string for the Monospac attribute name. **)
  SYNS_AttrMonospace = 'Monospace';
  (** A resource string for the Bullet attribute name. **)
  SYNS_AttrBullet = 'Bullet';
  (** A resource string for the Number attribute name. **)
  SYNS_AttrNumber = 'Number';
  (** A resource string for the MD text attribute name. **)
  SYNS_AttrText = 'Text';
  (** A resource string for the MD text attribute name. **)
  SYNS_AttrCode = 'Code';

Const
  (** Table function index. **)
  KeyIndices: Array [0 .. 1] Of Integer = (0, - 1);
  (** A constant to define the characters that start an MD comment **)
  acMDCommentChars = ['#'];
  (** A constant to define the characters that are identifier characters. **)
  acIdentifier = ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '_'];
  (** A constant to define the characters that are space characters. **)
  acSpace = [#32, #9];
  (** A constant to define the characters that should be considered as symbols. **)
  acSymbols = [#32 .. #128] - acSpace - acIdentifier - acMDCommentChars;
  acNumberedLists = ['.',')'];

{$Q-}

(**

  This method sets the given attribute with the given colours and font style.

  @precon  None.
  @postcon The given attribute has its font colour, background colour and font style set.

  @param   Attribute  as a TSynHighlighterAttributes as a constant
  @param   ForeColour as a TColor as a constant
  @param   BackColour as a TColor as a constant
  @param   FontStyle  as a TFontStyles as a constant

**)
procedure TSynMDSyn.AddAndUpdateAttributes(const _Attribute: TSynHighlighterAttributes; const ForeColour,
  BackColour: TColor; const FontStyle: TFontStyles);

Begin
  _Attribute.Foreground := ForeColour;
  _Attribute.Background := BackColour;
  _Attribute.Style := FontStyle;
  AddAttribute(_Attribute);
End;

(**

  This method returns Text as the default attribute.

  @precon  None.
  @postcon Returns Text as the default attribute.

  @nohint  Index
  
  @param   Index as an Integer as a constant
  @return  a TMDTokenKind

**)
function TSynMDSyn.AltFunc(const Index: Integer): TMDTokenKind;

Begin
  Result := tkMDText;
End;

(**

  A constructor for the TSynMDSyn class.

  @precon  None.
  @postcon The control is initialised.

  @nocheck MissingCONSTINParam

  @param   AOwner as a TComponent

**)
constructor TSynMDSyn.Create(AOwner: TComponent);

Begin
  Inherited Create(AOwner);
//  fCaseSensitive := False;
  FTokenAttri[tkMDLineComment] := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_AttrComment);
  AddAndUpdateAttributes(FTokenAttri[tkMDLineComment], clNone, clNone, [fsItalic]);
  FTokenAttri[tkMDSubHeading] := TSynHighlighterAttributes.Create(SYNS_AttrSubheading, SYNS_AttrSubheading);
  AddAndUpdateAttributes(FTokenAttri[tkMDSubHeading], clNone, clNone, [fsBold, fsUnderline]);
  FTokenAttri[tkMDItalic] := TSynHighlighterAttributes.Create(SYNS_AttrItalic, SYNS_AttrItalic);
  AddAndUpdateAttributes(FTokenAttri[tkMDItalic], clNone, clNone, [fsItalic]);
  FTokenAttri[tkMDBold] := TSynHighlighterAttributes.Create(SYNS_AttrBold, SYNS_AttrBold);
  AddAndUpdateAttributes(FTokenAttri[tkMDBold], clNone, clNone, [fsBold]);
  FTokenAttri[tkMDMonospace] := TSynHighlighterAttributes.Create(SYNS_AttrMonospace, SYNS_AttrMonospace);
  AddAndUpdateAttributes(FTokenAttri[tkMDMonospace], clNone, clNone, []);
  FTokenAttri[tkMDBullet] := TSynHighlighterAttributes.Create(SYNS_AttrBullet, SYNS_AttrBullet);
  AddAndUpdateAttributes(FTokenAttri[tkMDBullet], clNone, clNone, []);
  FTokenAttri[tkMDNumber] := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_AttrNumber);
  AddAndUpdateAttributes(FTokenAttri[tkMDNumber], clNone, clNone, []);
  FTokenAttri[tkMDSymbol] := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_AttrSymbol);
  AddAndUpdateAttributes(FTokenAttri[tkMDSymbol], clMaroon, clNone, []);
  FTokenAttri[tkMDText] := TSynHighlighterAttributes.Create(SYNS_AttrText, SYNS_AttrText);
  AddAndUpdateAttributes(FTokenAttri[tkMDText], clNone, clNone, []);
  FTokenAttri[tkMDSpace] := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_AttrSpace);
  AddAndUpdateAttributes(FTokenAttri[tkMDSpace], clNone, clNone, []);
  FTokenAttri[tkMDCode] := TSynHighlighterAttributes.Create(SYNS_AttrCode, SYNS_AttrCode);
  AddAndUpdateAttributes(FTokenAttri[tkMDCode], clNone, clNone, []);

  SetAttributesOnChange(@DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterMarkdown;
  FRange := rsMDText;
End;

(**

  This method processes a carriage return character.

  @precon  None.
  @postcon A carriage return chacracter is processed.

**)
procedure TSynMDSyn.CRProc;

Begin
  If Not (FRange In [rsMDItalic, rsMDBold, rsMDMonospace]) Then
    Begin
      FTokenID := tkMDSpace;
      FRange := rsMDText;
    End;
  Inc(Run);
  If FLine[Run] = #10 Then
    Inc(Run);
End;

(**

  A destructor for the TSynMDSyn class.

  @precon  None.
  @postcon Nothing.

**)
destructor TSynMDSyn.Destroy;

Begin
  Inherited Destroy;
End;

function TSynMDSyn.IsLineEnd(_Run: Integer): Boolean;
begin
  Result := (_Run >= FLineLen) or (FLine[_Run] = #10) or (FLine[_Run] = #13);
end;

(**

  Not sure but is required by the highlighter...

  @precon  None.
  @postcon Returns the text enumerate as a default.

  @nohint  Index
  
  @param   Index as an Integer as a constant
  @return  a TMDTokenKind

**)
function TSynMDSyn.FuncKeyword(const Index: Integer): TMDTokenKind;

Begin
  Result := tkMDText;
End;

(**

  This is a getter method for the XxxxxxAttri properties.

  @precon  None.
  @postcon Returns the instance of the given attribute set.

  @param   eTokenKind as a TMDTokenKind as a constant
  @return  a TSynHighlighterAttributes

**)
function TSynMDSyn.GetAttri(const eTokenKind: TMDTokenKind): TSynHighlighterAttributes;

Begin
  Result := FTokenAttri[eTokenKind];
End;

(**

  This is a getter method for the DefaultAttribute property.

  @precon  None.
  @postcon Returns the indexed default attribute.

  @nocheck MissingConstInParam

  @param   Index as an Integer
  @return  a TSynHighlighterAttributes

**)
function TSynMDSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;

Begin
  Case Index Of
    SYN_ATTR_COMMENT: Result := FTokenAttri[tkMDLineComment];
    SYN_ATTR_WHITESPACE: Result := FTokenAttri[tkMDSpace];
  Else
    Result := Nil;
  End;
End;

(**

  This is a getter method for the Eol property.

  @precon  None.
  @postcon Returns true if the Run property is 1 beyond the end of the line.

  @return  a Boolean

**)
function TSynMDSyn.GetEol: Boolean;

Begin
  Result := Run = fLineLen + 1;
End;

(**

  This is a getter method for the Friendlt=yLanguageName property.

  @precon  None.
  @postcon Returns the name of the language being highlighted.

  @return  an UnicodeString

**)
class function TSynMDSyn.GetFriendlyLanguageName: String;

Begin
  Result := SYNS_LangMarkdown;
End;

(**

  This is a getter method for the LanguageName property.

  @precon  None.
  @postcon Returns the name of the language.

  @return  a String

**)
class function TSynMDSyn.GetLanguageName: String;

Begin
  Result := SYNS_LangMarkdown;
End;

(**

  This is a getter method for the Range property.

  @precon  None.
  @postcon returns the range enumerate as a pointer.

  @return  a Pointer

**)
function TSynMDSyn.GetRange: Pointer;

Begin
  Result := pointer(PtrInt(FRange));
End;

(**

  This is a getter method for the SampleSource property.

  @precon  None.
  @postcon Returns a sample of text for the language.

  @return  a String

**)
function TSynMDSyn.GetSampleSource: String;
Begin
  result :=
    '#'#13#10 +
    '# This is a comment header for the Markdown file'#13#10 +
    '#'#13#10 +
    ''#13#10 +
    'Heading'#13#10 +
    '======='#13#10 +
    ''#13#10 +
    '## Sub-heading'#13#10 +
    ''#13#10 +
    'Paragraphs are separated by a blank line.'#13#10 +
    ''#13#10 +
    'Two spaces at the end of a line leave a line break.'#13#10 +
    ''#13#10 +
    'Text attributes _italic_, **bold**, `monospace`.'#13#10 +
    ''#13#10 +
    'Horizontal rule:'#13#10 +
    ''#13#10 +
    '---'#13#10 +
    ''#13#10 +
    'Bullet list:'#13#10 +
    ''#13#10 +
    '  * apples'#13#10 +
    '  * oranges'#13#10 +
    '  * pears'#13#10 +
    ''#13#10 +
    'Numbered list:'#13#10 +
    ''#13#10 +
    '  1. wash'#13#10 +
    '  2. rinse'#13#10 +
    '  3. repeat'#13#10 +
    ''#13#10 +
    'A [link](http://example.com).'#13#10 +
    ''#13#10 +
    '![Image](Image_icon.png)'#13#10 +
    ''#13#10 +
    '> Markdown uses email-style > characters for blockquoting.'#13#10 +
    ''#13#10 +
    'Inline <abbr title="Hypertext Markup Language">HTML</abbr> is supported.';

End;

(**

  This is a getter method for the TokenAttribute property.

  @precon  None.
  @postcon Returns the attribute associated with the current Token ID.

  @return  a TSynHighlighterAttributes

**)
function TSynMDSyn.GetTokenAttribute: TSynHighlighterAttributes;

Begin
  Result := FTokenAttri[GetTokenID];
End;

(**

  This is a getter method for the TokenID property.

  @precon  None.
  @postcon Returns the current token ID.

  @return  a TMDTokenKind

**)
function TSynMDSyn.GetTokenID: TMDTokenKind;

Begin
  Result := FTokenID;
End;

(**

  This is a getter method for the TokenKind property.

  @precon  None.
  @postcon Returns the token ID as an integer.

  @return  an Integer

**)
function TSynMDSyn.GetTokenKind: Integer;

Begin
  Result := Ord(FTokenID);
End;

procedure TSynMDSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := Run - fTokenPos;
  TokenStart  := FLine + fTokenPos;
end;

(**

  This method initialises the function table.

  @precon  None.
  @postcon The function table is initialised.

**)
procedure TSynMDSyn.InitIdent;

Var
  i: Integer;

Begin
  For i := Low(FIdentFuncTable) To High(FIdentFuncTable) Do
    If KeyIndices[i] = - 1 Then
      FIdentFuncTable[i] := @AltFunc;
  FIdentFuncTable[0] := @FuncKeyword;
End;

(**

  This method returns true if the filter has changed and needs to be stored.

  @precon  None.
  @postcon Returns true if the filter has changed and needs to be stored.

  @return  a Boolean

**)
function TSynMDSyn.IsFilterStored: Boolean;

Begin
  Result := fDefaultFilter <> SYNS_FilterMarkdown;
End;

(**

  This method checks that there are only spaces or tabs before the gievn character index on the current
  line.

  @precon  None.
  @postcon Returns 

  @param   iIndex as an Integer as a constant
  @return  a Boolean

**)
function TSynMDSyn.IsFirstCharOnLine(const iIndex: Integer): Boolean;

Var
  iChar : Integer;
  
Begin
  Result := True;
  For iChar := Pred(iIndex) DownTo 1 Do
    If Not CharInSet(FLine[iChar], [#32, #9]) Then
      Begin
        Result := False;
        Break;
      End;
End;

(**

  This method search for matching text attibutes char
  line.

  @precon  None.
  @postcon Returns

  @param   iIndex as an Integer as a constant
  @return  a Boolean

**)
function TSynMDSyn.MatchingAttribute(const iIndex: Integer): Boolean;
Var
  iChar : Integer;
Begin
  Result := False;
  iChar := iIndex;
  Repeat
    Inc(iChar);
  Until IsLineEnd(iChar) Or ((FLine[iChar] = FAttributeChar) and (fLine[pred(iChar)] <> '\'));
  if (FLine[iChar] = FAttributeChar) then
    begin
      result := not ( (fLine[pred(iChar)] in acSpace) and not (fLine[succ(iChar)] in acSpace));
    end;


End;

(**

  This method returns true if the given character is an identifier character.

  @precon  None.
  @postcon Returns true if the given character is an identifier character.

  @nocheck MissingConstInParam

  @param   AChar as a WideChar
  @return  a Boolean

**)
function TSynMDSyn.IsIdentChar(AChar: WideChar): Boolean;

Begin
  Case AChar Of
    '!', '_', '0' .. '9', 'a' .. 'z', 'A' .. 'Z':
      Result := True;
  Else
    Result := False;
  End;
End;

(**

  This method processes a Line Feed character and sets the TokenID and Range if not within amy other
  ranges.

  @precon  None.
  @postcon the Line Feed is processed and the TokenID and Range set if appropriate.

**)
procedure TSynMDSyn.LFProc;

Begin
  If Not (FRange In [rsMDItalic, rsMDBold, rsMDMonospace]) Then
    Begin
      FTokenID := tkMDSpace;
      FRange := rsMDText;
    End;
  Inc(Run);
End;

(**

  This method checks to see if the asterik character that triggered this call is followed by another and
  therefore we are string a BOLD section or is just a symbol.

  @precon  None.
  @postcon Starts a bold section of there are more than one asterik.

**)
procedure TSynMDSyn.MDTextAttibuteOpenProc;

Begin
  FAttributeChar := FLine[Run];
  Inc(Run);
  If FLine[Run] = FAttributeChar Then
    Begin
      Inc(Run);
      fRange := rsMDBold;
      fTokenID := tkMDBold;
    End Else
  If IsFirstCharOnLine(Pred(Run)) and (FLine[Run] in acSpace) Then
    Begin
      fTokenID := tkMDBullet;
      FRange := rsMDBullet;
    End Else
     begin
       if MatchingAttribute(Run) then
         begin
          FTokenID := tkMDItalic;
          fRange := rsMDItalic;
        end
       else
         FTokenID := tkMDSymbol;
     end;
End;

procedure TSynMDSyn.MDCodeOpenProc;

Begin
  FAttributeChar := FLine[Run];
  Inc(Run);
  If (FLine[Run] = FAttributeChar) and (FLine[Run+1] = FAttributeChar) Then
    Begin
      Inc(Run, 2);
      fRange := rsMDCode;
      fTokenID := tkMDCode;
    End;
end;

procedure TSynMDSyn.MDBoldBulletOpenProc;

Begin
  Inc(Run);
  If IsFirstCharOnLine(Pred(Run)) Then
    Begin
      fTokenID := tkMDBullet;
      FRange := rsMDBullet;
    End Else
      FTokenID := tkMDSymbol;
End;

(**

  This method process the BOLD chacratcer sequence until it finds an ending doube asterik.

  @precon  None.
  @postcon Process the BOLD charcater sequence until completion.

**)
procedure TSynMDSyn.MDBoldProc;

Begin
  Case FLine[Run] Of
    #0:  NullProc;
    #10: LFProc;
    #13: CRProc;
  Else
    Begin
      fTokenID := tkMDBold;
      Repeat
        If (FLine[Run] = FAttributeChar) And (FLine[Run - 1] = FAttributeChar) Then
          Begin
            inc(Run);
            fRange := rsMDText;
            Break;
          End;
        If Not IsLineEnd(Run) Then
          inc(Run);
      Until IsLineEnd(Run);
    End;
  End;
End;

procedure TSynMDSyn.MDCodeProc;

Begin
  fTokenID := tkMDCode;
  Repeat
    If (FLine[Run] = FAttributeChar) And (FLine[Run + 1] = FAttributeChar) and (FLine[Run + 2] = FAttributeChar) Then
      Begin
        inc(Run,3);
        fRange := rsMDText;
        Break;
      End
    else
      inc(run);
  Until  (Run >= FLineLen);
End;

(**

  This method checks to see if the sequence of characters is a comment or a sub-heading.

  @precon  None.
  @postcon A comment of subheading is started.

**)
procedure TSynMDSyn.MDCommentSubHeadingOpenProc;

Begin
  Inc(Run);
//  If FLine[Run] = '#' Then
    Begin
      FRange := rsMDSubHeading;
      FTokenID := tkMDSubheading;
//    End Else
//    Begin
//      FRange := rsMDLineComment;
//      FTokenID := tkMDLineComment;
    End;
End;

procedure TSynMDSyn.MDGenericLineProc(const eTokenID: TMDTokenKind);

Begin
  Case FLine[Run] Of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  Else
    Begin
      FTokenID := eTokenID;
      Repeat
        If Not IsLineEnd(Run) Then
          Inc(Run);
      Until IsLineEnd(Run);
    End;
  End;
End;

procedure TSynMDSyn.MDItalicOpenProc;

Begin
  Inc(Run);
  FRange := rsMDItalic;
  FTokenID := tkMDItalic;
End;

procedure TSynMDSyn.MDItalicProc;

Begin
  Case FLine[Run] Of
    #0:  NullProc;
    #10: LFProc;
    #13: CRProc;
  Else
    Begin
      FTokenID := tkMDItalic;
      Repeat
        Inc(Run);
      Until IsLineEnd(Run) Or (FLine[Run] = FAttributeChar);
      If FLine[Run] = FAttributeChar Then
        Begin
          Inc(Run);
          FRange := rsMDText;
        End;
    End;
  End;
End;

procedure TSynMDSyn.MDMonospaceOpenProc;

Begin
  Inc(Run);
  FRange := rsMDMonospace;
  FTokenID := tkMDMonospace;
End;

procedure TSynMDSyn.MDMonospaceProc;

Begin
  Case FLine[Run] Of
    #0:  NullProc;
    #10: LFProc;
    #13: CRProc;
  Else
    Begin
      FTokenID := tkMDMonospace;
      Repeat
        Inc(Run);
      Until IsLineEnd(Run) Or (FLine[Run] = '`');
      If FLine[Run] = '`' Then
        Begin
          Inc(Run);
          FRange := rsMDText;
        End;
    End;
  End;
End;

procedure TSynMDSyn.MDNumberOpenProc;

Begin
  Inc(Run);
  If IsFirstCharOnLine(Pred(Run)) and (fLine[run] in acNumberedLists) Then
    Begin
      FTokenID := tkMDNumber;
      FRange := rsMDNumber;
    End Else
      FTokenID := tkMDText;
End;

(**

  This method is called to process a character in the editor text and mark it as a specific type of
  text.

  @precon  None.
  @postcon The current character in the editor is attributed a character attribute.

**)
procedure TSynMDSyn.Next;

Begin
  fTokenPos := Run;
  Case FRange Of
    rsMDLineComment: MDGenericLineProc(tkMDLineComment);
    rsMDSubHeading:  MDGenericLineProc(tkMDSubheading);
    rsMDItalic:      MDItalicProc;
    rsMDBold:        MDBoldProc;
    rsMDMonospace :  MDMonospaceProc;
    rsMDCode:        MDCodeProc;
  //  rsMDBullet:      MDGenericLineProc(tkMDBullet);
  //  rsMDNumber:      MDGenericLineProc(tkMDNumber);
  Else
    Case FLine[Run] Of
      #00: NullProc;
      #10: LFProc;
      #13: CRProc;
      '#': MDCommentSubHeadingOpenProc;
      '\': EscapeProc;
      '_','*' : MDTextAttibuteOpenProc;
      '+': MDBoldBulletOpenProc;
      '~': MDCodeOpenProc;
      '0'..'9': MDNumberOpenProc;
      '`': MDMonospaceOpenProc;
      #1 .. #9, #11, #12, #14..#32: SpaceProc;
    Else
      If CharInSet(FLine[Run], acSymbols) Then
        SymbolProc
      Else
        TextProc;
    End;
  End;
End;

(**

  This method processes a null character.

  @precon  None.
  @postcon Reset the token ID and Range to text if not in a Italic, Bold or Monospace sequence.

**)
procedure TSynMDSyn.NullProc;

Begin
  If Not (FRange In [rsMDItalic, rsMDBold, rsMDMonospace]) Then
    Begin
      FTokenID := tkMDText;
      FRange := rsMDText;
    End;
  Inc(Run);
End;

procedure TSynMDSyn.EscapeProc;

Begin
  Inc(Run,2);
End;

(**

  This method resets the range to text.

  @precon  None.
  @postcon The range is reset to text.

**)
procedure TSynMDSyn.ResetRange;

Begin
  FRange := rsMDText;          
End;

(**

  This is a setter method for the XxxxxxxAttri property.

  @precon  None.
  @postcon The given attribute is set.

  @param   eTokenKind as a TMDTokenKind as a constant
  @param   Attri      as a TSynHighlighterAttributes as a constant

**)
procedure TSynMDSyn.SetAttri(const eTokenKind: TMDTokenKind; const Attri: TSynHighlighterAttributes);

Begin
  FTokenAttri[eTokenKind] := Attri;
End;

(**

  This is a setter method for the Range property.

  @precon  None.
  @postcon Sets the range property.

  @nocheck MissingConstInParam

  @param   Value as a Pointer

**)
procedure TSynMDSyn.SetRange(Value: Pointer);

Begin
  FRange := TMDRangeState(UIntPtr(Value));
End;

(**

  This method processes a space token.

  @precon  None.
  @postcon The run property is incremented and TokenID set to space. If there are more spaces these are
           processed.

**)
procedure TSynMDSyn.SpaceProc;

Begin
  Inc(Run);
  FTokenID := tkMDSpace;
  While (FLine[Run] <= #32) And Not IsLineEnd(Run) Do
    Inc(Run);
End;

(**

  This method processes a symbol token.

  @precon  None.
  @postcon The run property is incremented and TokenID set to symbol. If there are more symbosl these are
           processed.

**)
procedure TSynMDSyn.SymbolProc;

Begin
  Inc(Run);
  FTokenID := tkMDSymbol;
  While CharInSet(FLine[Run], acSymbols) And Not IsLineEnd(Run) Do
    Inc(Run);
End;

(**

  This method processes a text range.

  @precon  None.
  @postcon The token and range are set to text and the run property incremented.

**)
procedure TSynMDSyn.TextProc;

Begin
  FTokenID := tkMDText;
  FRange := rsMDText;
  Inc(Run);
End;

procedure TSynMDSyn.SetLine(const NewValue: string; LineNumber: Integer);
begin
  fLineRef := NewValue;
  fLine := PChar(fLineRef);
  fLineLen :=  Length(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

function TSynMDSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynMDSyn.GetToken: string;
var
  Len: LongInt;
begin
  Result := '';
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

(** Registers the highlighter. **)
Initialization
  {$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynMDSyn);
  {$ENDIF}
End.
