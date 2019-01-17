{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPython.pas, released 2000-06-23.
The Original Code is based on the odPySyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Olivier Deckmyn.
Portions created by M.Utku Karatas and Dennis Chuah.
Unicode translation by Maël Hörz.
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


$Id: SynHighlighterR.pas,v 1.0.0.0 2013/10/27 00:17:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(A R language highlighter for SynEdit)
@author(Di Rienzo Julio A. <dirienzo@agro.unc.edu.ar>)
@created(2010,4,12)
@lastmod(2013,10,27)
The SynHighlighterR implements a highlighter for R for the SynEdit projects.
It includes the LoadExternalKeyWords(ExternalKeyFileName:String;KeyType:TtkTokenKind;AddWords:boolean)
which let the user to change the list of TtkTokenKind or add new items;
}

{$IFNDEF QSYNHIGHLIGHTERR}
unit SynHighlighterR;
{$ENDIF}

{$I SynDefines.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditHighlighter,
  QSynEditTypes,
  QSynUnicode,
{$ELSE}
  Graphics,
  SynEditHighlighter,
  SynEditTypes,
{$ENDIF}
  SysUtils,
  Classes;


ResourceString
  SYNS_AttrModifier='Modifier keywords';
  SYNS_FriendlyAttrModifier='Aditional arguments for some function';

const
//  ALPHA_CHARS = ['_', 'a'..'z', 'A'..'Z'];
  SYNS_FilterR='R Files (*.r)|*.r';

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkNonKeyword,tkModifier, tkTrippleQuotedString,
    tkSystemDefined, tkHex, tkOct, tkFloat, tkUnknown);

  TRangeState = (rsANil, rsComment, rsUnKnown, rsMultilineString, rsMultilineString2,
                 rsMultilineString3 //this is to indicate if a string is made multiline by backslash char at line end (as in C++ highlighter)
                );

type

  { TSynRSyn }

  TSynRSyn = class(TSynCustomHighLighter)
  private
    fLine: PChar;
    fLineNumber: Integer;

    fToIdent: PChar;
    fStringStarter: WideChar;  // used only for rsMultilineString3 stuff
    fRange: TRangeState;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    FKeywords: TStringList;
    fStringAttri: TSynHighlighterAttributes;
    fDocStringAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fHexAttri: TSynHighlighterAttributes;
    fOctalAttri: TSynHighlighterAttributes;
    fFloatAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNonKeyAttri: TSynHighlighterAttributes;
    fModifierAttri: TSynHighlighterAttributes;
    fSystemAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fErrorAttri: TSynHighlighterAttributes;


    function IdentKind(MayBe: PChar): TtkTokenKind;
    function isLineEnd(rn: Longint): boolean;
    procedure SymbolProc;
    procedure CRProc;
    procedure CommentProc;
    procedure GreaterProc;
    procedure IdentProc;
    function IsIdentChar(AChar: WideChar): Boolean;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SpaceProc;
    procedure PreStringProc;
    procedure StringProc;
    procedure String2Proc;
    procedure StringEndProc(EndChar: WideChar);
    procedure UnknownProc;
  protected
    Run: LongInt;
    fStringLen: Integer;
    function GetSampleSource: String; override;
    function IsFilterStored: Boolean; override;
    procedure GetKeywordIdentifiers(TheWords: TStringList);
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    property Keywords: TStringList read FKeywords;
    property TokenID: TtkTokenKind read FTokenID;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetToken: string; override;
    function GetTokenPos: Integer; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    procedure LoadExternalKeyWords(ExternalKeyFileName:String;KeyType:TtkTokenKind;AddWords:boolean);
    procedure SetLine(const NewValue: string;
                      LineNumber: Integer); override;

  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
    write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
    write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NonKeyAttri: TSynHighlighterAttributes read fNonKeyAttri
      write fNonKeyAttri;
    property ModifiersAttri: TSynHighlighterAttributes read fModifierAttri
      write fModifierAttri;
    property SystemAttri: TSynHighlighterAttributes read fSystemAttri
      write fSystemAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
    write fNumberAttri;
    property HexAttri: TSynHighlighterAttributes read fHexAttri
      write fHexAttri;
    property OctalAttri: TSynHighlighterAttributes read fOctalAttri
      write fOctalAttri;
    property FloatAttri: TSynHighlighterAttributes read fFloatAttri
      write fFloatAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
    write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
    write fStringAttri;
    property DocStringAttri: TSynHighlighterAttributes read fDocStringAttri
      write fDocStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
    write fSymbolAttri;
    property ErrorAttri: TSynHighlighterAttributes read fErrorAttri
      write fErrorAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

//var
  //GlobalKeywords: TStringList;

procedure TSynRSyn.GetKeywordIdentifiers(TheWords: TStringList);
var L:TstringList;
  GlobalKeywords: TStringList;
const
  // No need to localise keywords!
 // List of keywords
  KEYWORDCOUNT = 26;
  KEYWORDSIdents: array [1..KEYWORDCOUNT] of String =
    (
'break',
'class',
'do',
'done',
'else',
'F',
'FALSE',
'for',
'function',
'if',
'ifelse',
'in',
'Inf',
'inherits',
'NA',
'NaN',
'next',
'NULL',
'repeat',
'return',
'switch',
'T',
'then',
'TRUE',
'unclass',
'while'
    );

  // List of non-keyword identifiers
  NONKEYWORDCOUNT = 4285;
  NONKEYWORDS: array [1..NONKEYWORDCOUNT] of String =
    (
'.Alias',
'.C',
'.Call',
'.Call.graphics',
'.checkMFClasses',
'.decode_package_version',
'.Defunct',
'.deparseOpts',
'.Deprecated',
'.doTracePrint',
'.dynLibs',
'.encode_package_version',
'.Export',
'.External',
'.External.graphics',
'.find.package',
'.First.lib',
'.First.sys',
'.Fortran',
'.getRequiredPackages',
'.getRequiredPackages2',
'.getXlevels',
'.guiCat',
'.guiCmd',
'.guiObjCallback',
'.guiWrite',
'.handleSimpleError',
'.helpForCall',
'.Import',
'.ImportFrom',
'.Internal',
'.isMethodsDispatchOn',
'.Last.lib',
'.libPaths',
'.mergeExportMethods',
'.MFclass',
'.NotYetImplemented',
'.NotYetUsed',
'.packages',
'.path.package',
'.Primitive',
'.primTrace',
'.primUntrace',
'.readRDS',
'.S3method',
'.saveRDS',
'.Script',
'.signalSimpleWarning',
'.subset',
'.subset2',
'.Tcl',
'.Tcl.args',
'.Tcl.args.objv',
'.Tcl.callback',
'.Tcl.objv',
'.Tk.ID',
'.Tk.newwin',
'.Tk.subwin',
'.TraceWithMethods',
'.tryHelp',
'.untracedFunction',
'.valueClassTest',
'abbey',
'abbreviate',
'abc.ci',
'ability.cov',
'abs',
'absolute',
'absolute.size',
'absolute.units',
'accdeaths',
'acf',
'ACF',
'ACF.gls',
'ACF.lme',
'acf2AR',
'acme',
'acos',
'acosh',
'add.net',
'add.scope',
'add1',
'add1.multinom',
'addGrob',
'addmargins',
'addNextMethod',
'addTaskCallback',
'addTclPath',
'addterm',
'aggregate',
'aggregate.data.frame',
'aggregate.default',
'aggregate.ts',
'agnes',
'agnes.object',
'agrep',
'agriculture',
'AIC',
'AIC.gls',
'AIC.lme',
'AIC.lmList',
'AIC.logLik',
'AIC.nlme',
'AIC.nls',
'aids',
'Aids2',
'aircondit',
'aircondit7',
'airmiles',
'AirPassengers',
'airquality',
'Alfalfa',
'alias',
'alist',
'all',
'all.equal',
'all.equal.character',
'all.equal.default',
'all.equal.factor',
'all.equal.formula',
'all.equal.language',
'all.equal.list',
'all.equal.numeric',
'all.equal.POSIXct',
'all.names',
'all.vars',
'allCoef',
'allGenerics',
'allNames',
'amis',
'aml',
'animals',
'anorexia',
'anova',
'anova.coxph',
'anova.coxphlist',
'anova.glm',
'anova.glmlist',
'anova.gls',
'anova.lm',
'anova.lme',
'anova.lmlist',
'anova.loess',
'anova.loglm',
'anova.mlm',
'anova.multinom',
'anova.negbin',
'anova.nls',
'anova.survreg',
'anova.survreglist',
'anova.trls',
'anovalist.lm',
'anovalist.nls',
'anovalist.trls',
'ansari.test',
'anscombe',
'any',
'anyDuplicated',
'aov',
'aperm',
'append',
'apply','apply_pb','by_pb',
'applyEdit',
'applyEdits',
'approx',
'approxfun',
'apropos',
'ar',
'ar.burg',
'ar.mle',
'ar.ols',
'ar.yw',
'area',
'Arg',
'args',
'Args',
'arima',
'arima.sim',
'arima0',
'arima0.diag',
'Arith',
'Arithmetic',
'ARMAacf',
'ARMAtoMA',
'array',
'arrowsGrob',
'as',
'as.array',
'as.call',
'as.character',
'as.character.condition',
'as.character.date',
'as.character.Date',
'as.character.default',
'as.character.error',
'as.character.factor',
'as.character.fractions',
'as.character.octmode',
'as.character.package_version',
'as.character.POSIXt',
'as.character.Surv',
'as.character.tclObj',
'as.character.tclVar',
'as.character.unit',
'as.complex',
'as.complex.default',
'as.data.frame',
'as.data.frame.array',
'as.data.frame.AsIs',
'as.data.frame.character',
'as.data.frame.complex',
'as.data.frame.data.frame',
'as.data.frame.date',
'as.data.frame.Date',
'as.data.frame.default',
'as.data.frame.factor',
'as.data.frame.groupedData',
'as.data.frame.integer',
'as.data.frame.list',
'as.data.frame.logical',
'as.data.frame.logLik',
'as.data.frame.matrix',
'as.data.frame.model.matrix',
'as.data.frame.numeric',
'as.data.frame.ordered',
'as.data.frame.package_version',
'as.data.frame.POSIXct',
'as.data.frame.POSIXlt',
'as.data.frame.shingle',
'as.data.frame.Surv',
'as.data.frame.table',
'as.data.frame.ts',
'as.data.frame.vector',
'as.data.frame.xyVector',
'as.Date',
'as.Date.character',
'as.Date.date',
'as.Date.dates',
'as.Date.default',
'as.Date.factor',
'as.Date.POSIXct',
'as.Date.POSIXlt',
'as.dendrogram',
'as.difftime',
'as.dist',
'as.double',
'as.double.default',
'as.double.tclObj',
'as.environment',
'as.expression',
'as.expression.default',
'as.factor',
'as.factorOrShingle',
'as.formula',
'as.fractions',
'as.function',
'as.function.default',
'as.hclust',
'as.integer',
'as.integer.default',
'as.integer.tclObj',
'as.is',
'as.latex',
'as.list',
'as.list.data.frame',
'as.list.default',
'as.list.environment',
'as.logical',
'as.logical.default',
'as.matrix',
'as.matrix.corStruct',
'as.matrix.data.frame',
'as.matrix.default',
'as.matrix.dist',
'as.matrix.noquote',
'as.matrix.pdMat',
'as.matrix.POSIXlt',
'as.matrix.reStruct',
'as.name',
'as.null',
'as.null.default',
'as.numeric',
'as.ordered',
'as.package_version',
'as.pairlist',
'as.person',
'as.personList',
'as.polySpline',
'as.POSIX',
'as.POSIXct',
'as.POSIXct.date',
'as.POSIXct.dates',
'as.POSIXct.default',
'as.POSIXct.POSIXlt',
'as.POSIXlt',
'as.qr',
'as.raw',
'as.real',
'as.shingle',
'as.single',
'as.single.default',
'as.stepfun',
'as.symbol',
'as.table',
'as.table.default',
'as.table.ftable',
'as.tclObj',
'as.title',
'as.ts',
'as.vector',
'as.vector.date',
'as.vector.factor',
'asin',
'asinh',
'AsIs',
'asMethodDefinition',
'asNamespace',
'asOneFormula',
'asOneSidedFormula',
'Assay',
'assign',
'assignClassDef',
'assignInNamespace',
'assignMethodsMetaData',
'assignOps',
'assignTemp',
'assocplot',
'asTable',
'asVector',
'atan',
'atan2',
'atanh',
'attach',
'attachNamespace',
'attenu',
'attitude',
'attr',
'attr.all.equal',
'attrassign',
'attributes',
'augPred',
'austres',
'autoload',
'autoloader',
'ave',
'axis',
'axis.Date',
'axis.POSIXct',
'axTicks',
'backsolve',
'backSpline',
'bacteria',
'balancedGrouped',
'balanceMethodsList',
'bandwidth',
'bandwidth.kernel',
'bandwidth.nrd',
'banking',
'bannerplot',
'barchart',
'barley',
'barplot',
'barplot.default',
'bartlett.test',
'base',
'basehaz',
'basename',
'basic.ci',
'BasicClasses',
'BATCH',
'batchSOM',
'bca.ci',
'bcv',
'bdf',
'beav1',
'beav2',
'beaver',
'beaver1',
'beaver2',
'beavers',
'Bessel',
'besselI',
'besselJ',
'besselK',
'besselY',
'beta',
'Beta',
'BIC',
'BIC.logLik',
'BIC.nlme',
'BIC.nls',
'BIC.nlsList',
'bigcity',
'bindenv',
'bindingIsActive',
'bindingIsLocked',
'binom.test',
'binomial',
'Binomial',
'biopsy',
'biplot',
'biplot.correspondence',
'biplot.princomp',
'birthday',
'birthwt',
'bitmap',
'BJsales',
'bkde',
'bkde2D',
'bkfe',
'bladder',
'bladder2',
'blank.lines.skip',
'bmp',
'BOD',
'body',
'BodyWeight',
'boot',
'boot.array',
'boot.ci',
'boot.object',
'boot.return',
'bootci.object',
'Boston',
'box',
'Box.test',
'boxcox',
'boxplot',
'boxplot.default',
'boxplot.formula',
'boxplot.stats',
'bquote',
'brambles',
'breslow',
'bringToTop',
'browseEnv',
'browser',
'browseURL',
'bs',
'bug.report',
'build',
'buildVignettes',
'builtins',
'bw.bcv',
'bw.nrd',
'bw.nrd0',
'bw.SJ',
'bw.ucv',
'bwplot',
'bxp',
'by',
'by.data.frame',
'by.default',
'bzfile',
'c',
'C',
'c.Date',
'c.noquote',
'c.package_version',
'c.POSIXct',
'c.POSIXlt',
'cabbages',
'cacheGenericsMetaData',
'cacheMetaData',
'cacheMethod',
'caith',
'calcium',
'call',
'callGeneric',
'callNextMethod',
'CallTip',
'cancer',
'cancor',
'cane',
'canonical.theme',
'capabilities',
'capability',
'capture.output',
'car.test.frame',
'cars',
'Cars93',
'case',
'case.names',
'casefold',
'cat',
'category',
'cats',
'catsM',
'Cauchy',
'cav',
'cbind',
'cbind.data.frame',
'cbind.ts',
'ccf',
'cd4',
'cd4.nested',
'Cefamandole',
'ceiling',
'cement',
'cens.return',
'censboot',
'censorReg',
'channing',
'char.expand',
'character',
'charmatch',
'charToRaw',
'chartr',
'check',
'check.options',
'checkAssignFuns',
'checkDocArgs',
'checkDocStyle',
'checkFF',
'checkMD5sums',
'checkMethods',
'checkSlotAssignment',
'checkTnF',
'checkVignettes',
'cheek.names',
'chem',
'ChickWeight',
'chickwts',
'childNames',
'chisq.test',
'Chisquare',
'chol',
'chol2inv',
'choose',
'choose.files',
'chull',
'circleGrob',
'citation',
'citEntry',
'citFooter',
'citHeader',
'city',
'clara',
'clara.object',
'claridge',
'class.ind',
'Classes',
'classMetaName',
'classRepresentation',
'clear.display.list',
'clearNames',
'clearpage',
'clipboard',
'clippaste',
'clogit',
'close',
'close.connection',
'close.screen',
'close.socket',
'closeAllConnections',
'cloth',
'cloud',
'clusplot',
'clusplot.default',
'clusplot.partition',
'cluster',
'cm',
'cm.colors',
'cmdscale',
'co.intervals',
'co.transfer',
'co2',
'CO2',
'coal',
'codes',
'codes.factor',
'codes.ordered',
'codoc',
'coef',
'coef.corAR1',
'coef.corARMA',
'coef.corARMAd',
'coef.corCAR1',
'coef.corCompSymm',
'coef.corHF',
'coef.corIdent',
'coef.corLin',
'coef.corNatural',
'coef.corSpatial',
'coef.corSpher',
'coef.corStruct',
'coef.corSymm',
'coef.fitdistr',
'coef.gls',
'coef.gnls',
'coef.hclust',
'coef.lda',
'coef.lme',
'coef.lmList',
'coef.loglm',
'coef.modelStruct',
'coef.multinom',
'coef.nls',
'coef.nnet',
'coef.pdBlocked',
'coef.pdCompSymm',
'coef.pdDiag',
'coef.pdIdent',
'coef.pdMat',
'coef.pdNatural',
'coef.pdSymm',
'coef.reStruct',
'coef.summary.nlsList',
'coef.tukeyline',
'coef.varComb',
'coef.varConstPower',
'coef.varExp',
'coef.varFixed',
'coef.varFunc',
'coef.varIdent',
'coef.varPower',
'coefficients',
'coefficients.glm',
'coefficients.lm',
'coerce',
'col.names',
'col.spec',
'col.whitebg',
'col2rgb',
'colClasses',
'collapse',
'collapse.groupedData',
'colMeans',
'colnames',
'colon',
'colors',
'colours',
'colSums',
'combn',
'commandArgs',
'comment',
'comment.char',
'common.draw.axis',
'Compare',
'compareFits',
'comparePred',
'compareRVersion',
'compareVersion',
'Comparison',
'Complete',
'complete.cases',
'completeClassDefinition',
'completeExtends',
'completeSubclasses',
'complex',
'Complex',
'computeRestarts',
'con2tr',
'condense',
'conditionCall',
'conditionCall.condition',
'conditionMessage',
'conditionMessage.condition',
'conditions',
'configuration',
'confint',
'confint.profile.glm',
'confint.profile.nls',
'conflicts',
'conformMethod',
'Conj',
'connection',
'connections',
'const',
'Constants',
'constrOptim',
'contour',
'contour.default',
'contourLines',
'contourplot',
'contr.Dunnett',
'contr.helmert',
'contr.poly',
'contr.SAS',
'contr.sdif',
'contr.sum',
'contr.treatment',
'contrast',
'contrasts',
'contrib.url',
'contributors',
'contrMat',
'Control',
'convert.gpar',
'convertHeight',
'convertNative',
'convertUnit',
'convertWidth',
'convertX',
'convertY',
'convolve',
'cooks.distance',
'coop',
'cophenetic',
'coplot',
'copy',
'copyright',
'cor',
'cor.test',
'corAR1',
'corARMA',
'corCAR1',
'corClasses',
'corCompSymm',
'corExp',
'corFactor',
'corFactor.compSymm',
'corFactor.corAR1',
'corFactor.corARMA',
'corFactor.corCAR1',
'corFactor.corNatural',
'corFactor.corSpatial',
'corFactor.corStruct',
'corFactor.corSymm',
'corGaus',
'corIdent',
'corLin',
'corMatrix',
'corMatrix.compSymm',
'corMatrix.corAR1',
'corMatrix.corARMA',
'corMatrix.corCAR1',
'corMatrix.corCompSymm',
'corMatrix.corIdent',
'corMatrix.corNatural',
'corMatrix.corSpatial',
'corMatrix.corStruct',
'corMatrix.corSymm',
'corMatrix.pdBlocked',
'corMatrix.pdCompSymm',
'corMatrix.pdDiag',
'corMatrix.pdIdent',
'corMatrix.pdMat',
'corMatrix.pdSymm',
'corMatrix.reStruct',
'corNatural',
'corr',
'corRatio',
'correlogram',
'corresp',
'corSpatial',
'corSpher',
'corSymm',
'cos',
'cosh',
'count.fields',
'cov',
'cov.mcd',
'cov.mve',
'cov.rob',
'cov.trob',
'cov.wt',
'cov2cor',
'covratio',
'cox.zph',
'coxph',
'coxph.control',
'coxph.detail',
'coxph.getdata',
'coxph.object',
'coxph.penalty',
'cpgram',
'cpus',
'crabs',
'CRAN.packages',
'createCallTipFile',
'createSyntaxFile',
'crossprod',
'tcrossprod',
'csimint',
'csimtest',
'ct',
'ctest',
'cu.summary',
'cum3',
'cummax',
'cummin',
'cumprod',
'cumsum',
'current.transform',
'current.viewport',
'current.vpTree',
'curve',
'Cushings',
'cut',
'cut.Date',
'cut.default',
'cut.dendrogram',
'cut.POSIXt',
'cutree',
'cv.glm',
'cycle',
'D',
'daisy',
'darwin',
'data',
'data.class',
'data.entry',
'data.frame',
'data.matrix',
'data.restore',
'dataentry',
'dataframeHelpers',
'dataViewport',
'date',
'date.ddmmmyy',
'date.mdy',
'date.mmddyy',
'date.mmddyyyy',
'Dates',
'DateTimeClasses',
'dbeta',
'dbinom',
'dcauchy',
'dcf',
'dchisq',
'DDT',
'de',
'de.ncols',
'de.restore',
'de.setup',
'deaths',
'debug',
'debugger',
'dec',
'decompose',
'defaultDumpName',
'defaultPrototype',
'Defunct',
'delay',
'delete.response',
'delimMatch',
'deltat',
'demo',
'dendrapply',
'dendrogram',
'density',
'densityplot',
'denumerate',
'denumerate.formula',
'deparse',
'Deprecated',
'deriv',
'deriv.default',
'deriv.formula',
'deriv3',
'deriv3.default',
'deriv3.formula',
'det',
'detach',
'determinant',
'determinant.matrix',
'dev.control',
'dev.copy',
'dev.copy2eps',
'dev.cur',
'dev.interactive',
'dev.list',
'dev.next',
'dev.off',
'dev.prev',
'dev.print',
'dev.set',
'dev2bitmap',
'deviance',
'deviance.nls',
'deviance.trls',
'device',
'Devices',
'dexp',
'df',
'df.kernel',
'df.residual',
'df.residual.nls',
'df.residual.trls',
'dfbeta',
'dfbetas',
'dffits',
'dgamma',
'dgeom',
'dget',
'dhyper',
'diag',
'Dialyzer',
'diana',
'diana.object',
'diff',
'diff.Date',
'diff.default',
'diff.POSIXt',
'diff.ts',
'diffinv',
'difftime',
'digamma',
'dim',
'Dim.corSpatial',
'Dim.corStruct',
'dim.data.frame',
'Dim.default',
'Dim.pdCompSymm',
'Dim.pdDiag',
'Dim.pdIdent',
'Dim.pdMat',
'Dim.pdNatural',
'Dim.pdSymm',
'dimnames',
'dimnames.data.frame',
'dir',
'dir.create',
'dirname',
'discoveries',
'display',
'dissimilarity.object',
'dist',
'DLL.version',
'dlnorm',
'dlogis',
'dmultinom',
'dmvnorm',
'DNase',
'dnbinom',
'dnorm',
'do.breaks',
'do.call',
'Documentation',
'dogs',
'doPrimitiveMethod',
'dose.p',
'dotchart',
'dotplot',
'double',
'download.file',
'download.packages',
'downs.bc',
'downViewport',
'dpih',
'dpik',
'dpill',
'dpois',
'dput',
'dQuote',
'draw.all',
'draw.colorkey',
'draw.details',
'draw.details.line.to',
'draw.details.lines',
'draw.details.move.to',
'draw.details.points',
'draw.details.polygon',
'draw.details.rect',
'draw.details.segments',
'draw.details.text',
'draw.details.viewport',
'draw.details.xaxis',
'draw.details.yaxis',
'draw.frame.child',
'draw.key',
'drawDetails',
'drivers',
'drop',
'drop.scope',
'drop.terms',
'drop1',
'drop1.multinom',
'dropterm',
'dsignrank',
'dt',
'ducks',
'dummy.coef',
'dump',
'dump.frames',
'dumpMethod',
'dumpMethods',
'dunif',
'duplicated',
'duplicated.array',
'duplicated.data.frame',
'duplicated.default',
'duplicated.matrix',
'dweibull',
'dwilcox',
'dyn.load',
'dyn.unload',
'eagles',
'eapply',
'Earthquake',
'ecdf',
'eda',
'edit',
'edit.data.frame',
'edit.default',
'edit.matrix',
'editDetails',
'editGrob',
'EEF.profile',
'eff.aovlist',
'effects',
'eigen',
'el',
'EL.profile',
'ellipsoidhull',
'ellipsoidPoints',
'elNamed',
'emacs',
'embed',
'empinf',
'empty.dump',
'emptyMethodsList',
'EmptyMethodsList',
'end',
'engine.display.list',
'enlist',
'envelope',
'environment',
'environmental',
'environmentIsLocked',
'epil',
'eqscplot',
'equal.count',
'erase.screen',
'ergoStool',
'esoph',
'ethanol',
'euro',
'eurodist',
'EuStockMarkets',
'eval',
'eval.nn',
'eval.parent',
'evalq',
'example',
'exists',
'existsFunction',
'existsMethod',
'existsTemp',
'exp',
'exp.tilt',
'expand.grid',
'expand.model.frame',
'expcov',
'expm1',
'Exponential',
'export',
'expression',
'extends',
'Extract',
'Extract.data.frame',
'Extract.factor',
'extractAIC',
'extractAIC.loglm',
'extractAIC.multinom',
'extractAIC.polr',
'extractAIC.trls',
'Extremes',
'factanal',
'factanal.fit.mle',
'factor',
'factor.scope',
'factorial',
'faithful',
'family',
'Family',
'family.glm',
'family.lm',
'family.negbin',
'fanny',
'fanny.object',
'farms',
'Fatigue',
'fbeta',
'fdeaths',
'fdHess',
'FDist',
'fft',
'fgl',
'fifo',
'file',
'file.access',
'file.append',
'file.choose',
'file.copy',
'file.create',
'file.edit',
'file.exists',
'file.info',
'file.path',
'file.remove',
'file.rename',
'file.show',
'file.symlink',
'files',
'fileutils',
'fill',
'filled.contour',
'filter',
'finalDefaultMethod',
'find',
'findClass',
'findFunction',
'findhtmlhelp',
'findInterval',
'findMethod',
'findRestart',
'findUnique',
'fir',
'fisher.test',
'fitdistr',
'fitted',
'fitted.gls',
'fitted.glsStruct',
'fitted.gnls',
'fitted.gnlsStruct',
'fitted.lme',
'fitted.lmeStruct',
'fitted.lmList',
'fitted.loglm',
'fitted.nlmeStruct',
'fitted.nls',
'fitted.trls',
'fitted.tukeyline',
'fitted.values',
'fitted.values.glm',
'fitted.values.lm',
'fivenum',
'fix',
'fixed.effects',
'fixed.effects.lme',
'fixed.effects.lmList',
'fixef',
'fixef.lme',
'fixef.lmList',
'fixInNamespace',
'fixPre1.8',
'fixup.libraries.URLs',
'fixup.package.URLs',
'fligner.test',
'floor',
'flower',
'flush',
'flush.connection',
'flush.console',
'forbes',
'force',
'Foreign',
'formalArgs',
'Formaldehyde',
'formals',
'format',
'format.AsIs',
'format.char',
'format.data.frame',
'format.Date',
'format.default',
'format.dist',
'format.factor',
'format.info',
'format.octmode',
'format.POSIXct',
'format.POSIXlt',
'format.pval',
'format.Surv',
'formatC',
'formatDL',
'formatg',
'formula',
'formula.corStruct',
'formula.gls',
'formula.gnls',
'formula.groupedData',
'formula.lm',
'formula.lme',
'formula.lmList',
'formula.modelStruct',
'formula.nlme',
'formula.nls',
'formula.nlsList',
'formula.pdBlocked',
'formula.pdMat',
'formula.reStruct',
'formula.terms',
'formula.varComb',
'formula.varFunc',
'forwardsolve',
'fourfoldplot',
'fractions',
'frailty',
'frame',
'frameGrob',
'freeny',
'freq.array',
'frequency',
'frequency.polygon',
'frets',
'friedman.test',
'ftable',
'ftable.formula',
'functionBody',
'GAGurine',
'galaxies',
'gam',
'gam.check',
'gam.control',
'gam.fit',
'gam.models',
'gam.nbut',
'gam.parser',
'gam.selection',
'gam.setup',
'gam.side.conditions',
'gamma.dispersion',
'gamma.shape',
'gammaCody',
'GammaDist',
'GAMsetup',
'gapply',
'Gasoline',
'gaucov',
'gaussian',
'gc',
'gc.time',
'gcinfo',
'gctorture',
'gEdit',
'gEditList',
'gehan',
'generic.skeleton',
'genericFunction',
'GenericFunctions',
'genotype',
'Geometric',
'get',
'get.family',
'get.gpar',
'get.value',
'getAccess',
'getAllConnections',
'getAllMethods',
'getAllSuperClasses',
'getAnywhere',
'getCallingDLL',
'getCConverterDescriptions',
'getCConverterStatus',
'getClass',
'getClassDef',
'getClasses',
'getClassName',
'getClassPackage',
'getConnection',
'getCovariate',
'getCovariate.corSpatial',
'getCovariate.corStruct',
'getCovariate.data.frame',
'getCovariate.varFunc',
'getCovariateFormula',
'getData',
'getData.gls',
'getData.gnls',
'getData.lme',
'getData.lmList',
'getData.nlme',
'getData.nls',
'getDataPart',
'getDepList',
'getDLLRegisteredRoutines',
'getDLLRegisteredRoutines.character',
'getDLLRegisteredRoutines.DLLInfo',
'getenv',
'getEnvironment',
'geterrmessage',
'getExportedValue',
'getExtends',
'getFromNamespace',
'getFunction',
'getFunctions',
'getGeneric',
'getGenerics',
'getGrob',
'getGroup',
'getGroups',
'getGroups.corStruct',
'getGroups.data.frame',
'getGroups.gls',
'getGroups.lme',
'getGroups.lmList',
'getGroups.varFunc',
'getGroupsFormula',
'getHook',
'getIdentification',
'getInitial',
'getKeywords',
'getLoadedDLLs',
'getMethod',
'getMethods',
'getMethodsForDispatch',
'getMethodsMetaData',
'getNamespace',
'getNamespaceExports',
'getNamespaceImports',
'getNamespaceInfo',
'getNamespaceName',
'getNamespaceUsers',
'getNamespaceVersion',
'getNativeSymbolInfo',
'getNumCConverters',
'getOption',
'getPackageName',
'getpid',
'getProperties',
'getPrototype',
'getResponse',
'getResponse.data.frame',
'getResponse.gls',
'getResponse.lme',
'getResponse.lmList',
'getResponseFormula',
'getRversion',
'getS3method',
'getSlots',
'getSocketClients',
'getSocketServerName',
'getSocketServers',
'getSubclasses',
'getTaskCallbackNames',
'getTemp',
'getValidity',
'getVarCov',
'getVirtual',
'getwd',
'getWindowsHandle',
'getWindowTitle',
'geyser',
'gilgais',
'ginv',
'gl',
'gList',
'glm',
'glm.control',
'glm.convert',
'glm.diag',
'glm.diag.plots',
'glm.fit',
'glm.fit.null',
'glm.nb',
'glm.summaries',
'glmmPQL',
'globalenv',
'gls',
'glsControl',
'glsObject',
'glsStruct',
'Glucose',
'Glucose2',
'gnls',
'gnlsControl',
'gnlsObject',
'gnlsStruct',
'gpar',
'gPath',
'graphics.off',
'grav',
'gravity',
'gray',
'grep',
'grey',
'grid',
'Grid',
'grid.add',
'grid.arrows',
'grid.Call',
'grid.circle',
'grid.collection',
'grid.convert',
'grid.convertHeight',
'grid.convertWidth',
'grid.convertX',
'grid.convertY',
'grid.copy',
'grid.current.viewport',
'grid.display.list',
'grid.draw',
'grid.edit',
'grid.frame',
'grid.get',
'grid.grab',
'grid.grill',
'grid.grob',
'grid.height',
'grid.init.viewport.stack',
'grid.layout',
'grid.legend',
'grid.line.to',
'grid.lines',
'grid.locator',
'grid.move.to',
'grid.multipanel',
'grid.newpage',
'grid.pack',
'grid.panel',
'grid.place',
'grid.plot.and.legend',
'grid.points',
'grid.polygon',
'grid.pretty',
'grid.prompt',
'grid.prop.list',
'grid.rect',
'grid.remove',
'grid.segments',
'grid.set',
'grid.show.layout',
'grid.show.viewport',
'grid.strip',
'grid.text',
'grid.top.level.vp',
'grid.width',
'grid.xaxis',
'grid.yaxis',
'grob',
'grobHeight',
'grobWidth',
'groupedData',
'groupGeneric',
'gsub',
'gsummary',
'gTree',
'guiCallTip',
'guiCmd',
'guiComplete',
'guiDDEInstall',
'guiDlg',
'guiDlgAssistant',
'guiDlgColor',
'guiDlgDir',
'guiDlgDoubleList',
'guiDlgFont',
'guiDlgFormula',
'guiDlgFunction',
'guiDlgGraphOptions',
'guiDlgGrid',
'guiDlgI',
'guiDlgItemSel',
'guiDlgList',
'guiDlgMessage',
'guiDlgOpen',
'guiDlgOptions',
'guiDlgProgress',
'guiDlgSave',
'guiDlgText',
'guiDlgVarSel',
'guiDlgView',
'guiInfo',
'guiInstall',
'guiObjBrowse',
'guiObjClear',
'guiObjDir',
'guiObjList',
'guiObjMenu',
'guiObjSearch',
'guiSave',
'guiSource',
'guiStart',
'guiStop',
'guiUninstall',
'guiViewsCmd',
'guiViewsCSS',
'guiViewsCSSChange',
'guiViewsDir',
'guiViewsDisplay',
'guiViewsFile',
'Gun',
'gzcon',
'gzfile',
'HairEyeColor',
'Harman23.cor',
'Harman74.cor',
'hasArg',
'hasMethod',
'hasTsp',
'hat',
'hatvalues',
'hatvalues.lm',
'hclust',
'head',
'header',
'heart',
'heat.colors',
'heatmap',
'height',
'height.details',
'height.details.default',
'height.details.frame',
'height.details.rect',
'height.details.text',
'height.details.viewport',
'height.frame',
'height.lines',
'height.post',
'height.post.details.default',
'height.pre',
'height.pre.details.default',
'height.text',
'heightDetails',
'help',
'help.search',
'help.search.web',
'help.start',
'Hershey',
'hills',
'hirose',
'hist',
'hist.default',
'hist.FD',
'hist.POSIXt',
'hist.scott',
'histogram',
'history',
'HoltWinters',
'housing',
'hsv',
'HTML',
'HTML.cormat',
'HTML.title',
'HTMLbr',
'HTMLChangeCSS',
'HTMLCSS',
'HTMLEndFile',
'HTMLgrid',
'HTMLgrid_inline',
'HTMLgrid_references',
'HTMLgrid_summary',
'HTMLhr',
'HTMLInitFile',
'HTMLInsertGraph',
'HTMLli',
'HTMLplot',
'HTMLStart',
'HTMLstem',
'HTMLStop',
'httpclient',
'huber',
'hubers',
'Hyperbolic',
'Hypergeometric',
'I',
'iden',
'identical',
'identify',
'identify.hclust',
'IGF',
'Im',
'image',
'image.default',
'immer',
'Imp.Estimates',
'imp.moments',
'imp.prob',
'imp.quantile',
'imp.reg',
'imp.weights',
'importIntoEnv',
'inc.display.list',
'index.search',
'Indometh',
'infert',
'influence',
'influence.measures',
'initialize',
'initialize.corAR1',
'initialize.corARMA',
'initialize.corCAR1',
'initialize.corCompSymm',
'initialize.corHF',
'initialize.corIdent',
'initialize.corLin',
'initialize.corNatural',
'initialize.corSpatial',
'initialize.corSpher',
'initialize.corStruct',
'initialize.corSymm',
'initialize.glsStruct',
'initialize.gnlsStruct',
'initialize.lmeStruct',
'initialize.reStruct',
'initialize.varComb',
'initialize.varConstPower',
'initialize.varExp',
'initialize.varFixed',
'initialize.varFunc',
'initialize.varIdent',
'initialize.varPower',
'InsectSprays',
'insertMethod',
'INSTALL',
'install.packages',
'installed.packages',
'installFoundDepends',
'installLocalPackages',
'Insurance',
'integer',
'integrate',
'interaction',
'interaction.plot',
'interactive',
'Internal',
'InternalMethods',
'interpSpline',
'intersect',
'intervals',
'intervals.gls',
'intervals.lme',
'intervals.lmList',
'intToBits',
'inv.logit',
'inverse.gaussian',
'inverse.rle',
'invisible',
'invokeRestart',
'invokeRestartInteractively',
'IQR',
'iris',
'iris3',
'is',
'is.array',
'is.atomic',
'is.call',
'is.character',
'is.complex',
'is.data.frame',
'is.date',
'is.double',
'is.element',
'is.empty.model',
'is.environment',
'is.even',
'is.expression',
'is.factor',
'is.finite',
'is.fractions',
'is.function',
'is.gpar',
'is.grob',
'is.infinite',
'is.integer',
'is.integer.factor',
'is.language',
'is.layout',
'is.leaf',
'is.list',
'is.loaded',
'is.logical',
'is.matrix',
'is.mts',
'is.na',
'is.na.data.frame',
'is.na.date',
'is.na.POSIXlt',
'is.na.ratetable',
'is.na.Surv',
'is.name',
'is.nan',
'is.null',
'is.numeric',
'is.numeric.factor',
'is.object',
'is.odd',
'is.ordered',
'is.package_version',
'is.pairlist',
'is.primitive',
'is.qr',
'is.R',
'is.ratetable',
'is.real',
'is.recursive',
'is.shingle',
'is.single',
'is.stepfun',
'is.Surv',
'is.symbol',
'is.table',
'is.tclObj',
'is.tkwin',
'is.ts',
'is.tskernel',
'is.unit',
'is.unsorted',
'is.vector',
'is.viewport',
'isBalanced',
'isBaseNamespace',
'isClass',
'isClassDef',
'isClassUnion',
'isGeneric',
'isGrammarSymbol',
'isGroup',
'isIncomplete',
'isInitialized',
'isInitialized.reStruct',
'islands',
'islay',
'isNamespace',
'ISOdate',
'ISOdatetime',
'ISOLatin1',
'isoMDS',
'isOpen',
'isoreg',
'isRestart',
'isSDI',
'isSealedClass',
'isSealedMethod',
'isSeekable',
'isVirtualClass',
'isWin',
'jack.after.boot',
'Japanese',
'jasa',
'jasa1',
'jitter',
'JohnsonJohnson',
'jpeg',
'julian',
'julian.Date',
'julian.POSIXt',
'justifyX',
'justifyY',
'k3.linear',
'KalmanForecast',
'KalmanLike',
'KalmanRun',
'KalmanSmooth',
'kappa',
'kappa.default',
'kappa.lm',
'kappa.qr',
'kappa.tri',
'Kaver',
'kde2d',
'Kenvl',
'kernapply',
'kernel',
'KernSmooth',
'Kfn',
'kidney',
'kmeans',
'knn',
'knn.cv',
'knn1',
'knots',
'kronecker',
'kruskal.test',
'ks.test',
'ksmooth',
'kyphosis',
'La.chol',
'La.chol2inv',
'La.eigen',
'La.svd',
'labels',
'labels.default',
'labels.lm',
'labels.rpart',
'labels.survreg',
'labels.terms',
'lag',
'lag.plot',
'LakeHuron',
'language',
'languageEl',
'lapply','lapply_pb',
'larrows',
'Last.value',
'Lattice',
'lattice.getOption',
'lattice.options',
'lattice.theme',
'latticeParseFormula',
'layout',
'layout.heights',
'layout.ncol',
'layout.nrow',
'layout.respect',
'layout.show',
'layout.torture',
'layout.widths',
'layoutRegion',
'lazyLoad',
'lbeta',
'lchoose',
'lcm',
'lda',
'ldahist',
'ldeaths',
'LDEsysMat',
'legend',
'length',
'LETTERS',
'leuk',
'leukemia',
'levelplot',
'levels',
'lfactorial',
'lgamma',
'lh',
'library',
'library.dynam',
'library.dynam.unload',
'licence',
'license',
'LifeCycleSavings',
'limitedLabels',
'line',
'linear.approx',
'linearizeMlist',
'LinearMethodsList',
'lines',
'lines.default',
'lines.formula',
'lines.histogram',
'lines.saddle.distn',
'lines.survfit',
'lines.ts',
'linesGrob',
'lineToGrob',
'link.html.help',
'list',
'list.files',
'listCustoms',
'listFromMlist',
'listMethods',
'llines',
'lm',
'lm.fit',
'lm.fit.null',
'lm.gls',
'lm.influence',
'lm.ridge',
'lm.summaries',
'lm.wfit',
'lm.wfit.null',
'lme4',
'lmer',
'glmer',
'lsmeans',
'lme',
'lme.formula',
'lme.groupedData',
'lme.lmList',
'lmeControl',
'lmeObject',
'lmeScale',
'lmeStruct',
'lmList',
'lmList.formula',
'lmList.groupedData',
'lmsreg',
'lmwork',
'load',
'loadedNamespaces',
'loadhistory',
'loadingNamespaceInfo',
'loadings',
'loadMethod',
'loadNamespace',
'loadURL',
'Loblolly',
'local',
'localeconv',
'locales',
'localPackages',
'locator',
'lockBinding',
'lockEnvironment',
'locpoly',
'loess',
'loess.control',
'loess.smooth',
'log',
'log10',
'log1p',
'log2',
'logb',
'logDet',
'logDet.corIdent',
'logDet.corStruct',
'logDet.pdBlocked',
'logDet.pdCompSymm',
'logDet.pdDiag',
'logDet.pdIdent',
'logDet.pdMat',
'logDet.pdNatural',
'logDet.pdSymm',
'logDet.reStruct',
'Logic',
'logical',
'Logistic',
'logit',
'logLik',
'logLik.corStruct',
'logLik.glm',
'logLik.gls',
'logLik.glsStruct',
'logLik.gnls',
'logLik.gnlsStruct',
'logLik.lm',
'logLik.lme',
'logLik.lmeStruct',
'logLik.lmeStructInt',
'logLik.lmList',
'logLik.nls',
'logLik.reStruct',
'logLik.varComb',
'logLik.varFunc',
'loglin',
'loglm',
'loglm1',
'Lognormal',
'logtrans',
'longley',
'lookup.xport',
'lower.to.upper.tri.inds',
'lower.tri',
'lowess',
'lplot.xy',
'lpoints',
'lqs',
'lqs.formula',
'ls',
'ls.diag',
'ls.print',
'ls.str',
'lsegments',
'lset',
'lsf.str',
'lsfit',
'ltext',
'ltransform3dMatrix',
'ltransform3dto3d',
'ltsreg',
'lung',
'lvq1',
'lvq2',
'lvq3',
'lvqinit',
'lvqtest',
'lynx',
'machine',
'Machine',
'Machines',
'MacRoman',
'mad',
'mahalanobis',
'make.link',
'make.names',
'make.packages.html',
'make.search.html',
'make.socket',
'make.tables',
'make.unique',
'make.xaxis.labels',
'make.xaxis.major',
'make.xaxis.ticks',
'make.yaxis.labels',
'make.yaxis.major',
'make.yaxis.ticks',
'makeActiveBinding',
'makeARIMA',
'makeClassRepresentation',
'makeExtends',
'makeGeneric',
'makeMethodsList',
'makepredictcall',
'makepredictcall.bs',
'makepredictcall.ns',
'makepredictcall.poly',
'makePrototypeFromClassDef',
'makeStandardGeneric',
'mammals',
'manaus',
'manglePackageName',
'manova',
'mantelhaen.test',
'mapply',
'margin.table',
'MASS',
'mat.or.vec',
'match',
'match.arg',
'match.call',
'match.fun',
'matchSignature',
'Math',
'Math.data.frame',
'Math.date',
'Math.Date',
'Math.difftime',
'Math.factor',
'Math.fractions',
'Math.POSIXlt',
'Math.POSIXt',
'Math.ratetable',
'Math.Surv',
'Math2',
'MathAchieve',
'MathAchSchool',
'matlines',
'matmult',
'matplot',
'matpoints',
'matrix',
'max',
'max.col',
'maxCol',
'mca',
'mcnemar.test',
'mcycle',
'md5sum',
'mdeaths',
'mdy.date',
'mean',
'mean.data.frame',
'mean.Date',
'mean.default',
'mean.difftime',
'mean.POSIXct',
'mean.POSIXlt',
'meanabsdev',
'meanvar',
'Meat',
'MEdecomp',
'median',
'MEdims',
'medpolish',
'MEEM',
'MEestimate',
'melanoma',
'mem.limits',
'Memory',
'memory.limit',
'memory.profile',
'memory.size',
'menarche',
'menu',
'merge',
'merge.data.frame',
'merge.default',
'mergeMethods',
'message',
'metaNameUndo',
'MethodAddCoerce',
'MethodDefinition',
'methods',
'Methods',
'MethodsList',
'MethodsListSelect',
'methodsPackageMetaName',
'MethodSupport',
'methodUtilities',
'MethodWithNext',
'mgcv',
'mget',
'mginv',
'michelson',
'Milk',
'min',
'minn38',
'missing',
'missingArg',
'mlistMetaName',
'Mod',
'mod.dims',
'mode',
'model.extract',
'model.frame',
'model.frame.aovlist',
'model.frame.coxph',
'model.frame.default',
'model.frame.glm',
'model.frame.lda',
'model.frame.lm',
'model.frame.multinom',
'model.frame.polr',
'model.frame.qda',
'model.frame.rpart',
'model.frame.survreg',
'model.matrix',
'model.matrix.default',
'model.matrix.glm.null',
'model.matrix.lm',
'model.matrix.reStruct',
'model.offset',
'model.response',
'model.tables',
'model.weights',
'modreg',
'mona',
'mona.object',
'mono.con',
'month.abb',
'month.name',
'monthplot',
'months',
'months.Date',
'months.POSIXt',
'mood.test',
'morley',
'mosaicplot',
'motor',
'motors',
'moveToGrob',
'mtcars',
'multiedit',
'multinom',
'Multinomial',
'muscle',
'mva',
'mvfft',
'mvrnorm',
'mvt',
'n2mfrow',
'na.action',
'na.contiguous',
'na.exclude',
'na.fail',
'na.omit',
'na.omit.ts',
'na.pass',
'na.rm',
'na.rpart',
'na.strings',
'name',
'names',
'names.default',
'names.dist',
'Names.formula',
'Names.listForm',
'Names.pdBlocked',
'Names.pdMat',
'Names.reStruct',
'namespaceExport',
'namespaceImport',
'namespaceImportClasses',
'namespaceImportFrom',
'namespaceImportMethods',
'napredict',
'naprint',
'naresid',
'nargs',
'native.enc',
'nchar',
'nclass.FD',
'nclass.freq',
'nclass.scott',
'nclass.Sturges',
'ncol',
'NCOL',
'needUpdate',
'needUpdate.corStruct',
'needUpdate.default',
'needUpdate.modelStruct',
'needUpdate.reStruct',
'needUpdate.varComb',
'needUpdate.varIdent',
'neg.bin',
'neg.binom',
'negative.binomial',
'NegBinomial',
'negexp.SSival',
'neuro',
'new.col',
'new.env',
'new.row',
'newBasic',
'newClassRepresentation',
'newcomb',
'newEmptyObject',
'newestVersion',
'NextMethod',
'nextn',
'nfGroupedData',
'nhtemp',
'Nile',
'Nitrendipene',
'nitrofen',
'nlevels',
'nlm',
'nlme',
'nlme.formula',
'nlme.nlsList',
'nlmeControl',
'nlmeObject',
'nlmeStruct',
'nls',
'nls.control',
'nlschools',
'nlsList',
'nlsList.formula',
'nlsList.selfStart',
'nlsModel',
'NLSstAsymptotic',
'NLSstClosestX',
'NLSstLfAsymptote',
'NLSstRtAsymptote',
'nmGroupedData',
'nnet',
'nnet.Hess',
'nodal',
'noquote',
'norm.ci',
'norm.net',
'Normal',
'normalize',
'nottem',
'NotYet',
'NotYetImplemented',
'NotYetUsed',
'npk',
'npr1',
'nrow',
'NROW',
'nrows',
'ns',
'nuclear',
'Null',
'null.space.basis.labels',
'null.space.basis.powers',
'null.space.dimension',
'num.col.specs',
'num.row.specs',
'numeric',
'numericDeriv',
'oats',
'Oats',
'object.size',
'objects',
'ObjectsWithPackage',
'octmode',
'offset',
'old.packages',
'oldClass',
'oldGet',
'olvq1',
'OME',
'on.exit',
'oneway',
'oneway.test',
'open',
'open.connection',
'Ops',
'Ops.data.frame',
'Ops.date',
'Ops.Date',
'Ops.difftime',
'Ops.factor',
'Ops.fractions',
'Ops.ordered',
'Ops.package_version',
'Ops.POSIXct',
'Ops.POSIXlt',
'Ops.POSIXt',
'Ops.ratetable',
'Ops.Surv',
'Ops.ts',
'Ops.unit',
'optim',
'optimise',
'optimize',
'options',
'Orange',
'OrchardSprays',
'order',
'order.dendrogram',
'ordered',
'origin.bottom',
'origin.left',
'origin.right',
'origin.top',
'Orthodont',
'osition',
'ost',
'outer',
'ovarian',
'Ovary',
'Oxboys',
'Oxide',
'p.adjust',
'pacf',
'package.contents',
'package.dependencies',
'package.description',
'package.skeleton',
'package_version',
'packageDescription',
'packageEvent',
'packageHasNamespace',
'packageStatus',
'packBits',
'packGrob',
'page',
'painters',
'pairlist',
'pairs',
'pairs.compareFits',
'pairs.default',
'pairs.formula',
'pairs.lda',
'pairs.lme',
'pairs.lmList',
'pairwise.prop.test',
'pairwise.t.test',
'pairwise.table',
'pairwise.wilcox.test',
'palette',
'Palettes',
'pam',
'pam.object',
'panel.3dscatter',
'panel.3dwire',
'panel.abline',
'panel.arrows',
'panel.axis',
'panel.barchart',
'panel.bwplot',
'panel.cloud',
'panel.contourplot',
'panel.curve',
'panel.densityplot',
'panel.dotplot',
'panel.fill',
'panel.grid',
'panel.histogram',
'panel.identify',
'panel.levelplot',
'panel.linejoin',
'panel.lines',
'panel.lmline',
'panel.loess',
'panel.mathdensity',
'panel.pairs',
'panel.parallel',
'panel.points',
'panel.qq',
'panel.qqmath',
'panel.qqmathline',
'panel.rug',
'panel.segments',
'panel.smooth',
'panel.splom',
'panel.stripplot',
'panel.superpose',
'panel.superpose.2',
'panel.text',
'panel.tmd',
'panel.wireframe',
'panel.xyplot',
'par',
'parallel',
'parcoord',
'Paren',
'parent.env',
'parent.frame',
'parse',
'parse.dcf',
'parseNamespaceFile',
'partition.object',
'paste',
'path.expand',
'path.rpart',
'paulsen',
'pbc',
'pbeta',
'PBG',
'pbinom',
'pbirthday',
'pcauchy',
'pchisq',
'pcls',
'pdBlocked',
'pdClasses',
'pdCompSymm',
'pdConstruct',
'pdConstruct.pdBlocked',
'pdConstruct.pdCompSymm',
'pdConstruct.pdDiag',
'pdConstruct.pdIdent',
'pdConstruct.pdLogChol',
'pdConstruct.pdMat',
'pdConstruct.pdNatural',
'pdConstruct.pdSymm',
'pdDiag',
'pdf',
'pdFactor',
'pdFactor.reStruct',
'pdIdent',
'pdLogChol',
'pdMat',
'pdMatrix',
'pdMatrix.reStruct',
'pdNatural',
'pdSymm',
'pentagamma',
'perc.ci',
'periodicSpline',
'person',
'personList',
'persp',
'persp.gam',
'petrol',
'pexp',
'pf',
'pgamma',
'pgeom',
'Phenobarb',
'phenoModel',
'phones',
'phyper',
'pi',
'pico',
'pictex',
'pie',
'piechart',
'Pima.te',
'Pima.tr',
'Pima.tr2',
'pipe',
'Pixel',
'PkgUtils',
'placeGrob',
'PlantGrowth',
'Platform',
'plclust',
'plnorm',
'plogis',
'plot',
'plot.acf',
'plot.ACF',
'plot.agnes',
'plot.augPred',
'plot.boot',
'plot.compareFits',
'plot.correspondence',
'plot.cox.zph',
'plot.data.frame',
'plot.date',
'plot.decomposed.ts',
'plot.default',
'plot.dendrogram',
'plot.density',
'plot.design',
'plot.diana',
'plot.ecdf',
'plot.factor',
'plot.formula',
'plot.function',
'plot.gam',
'plot.gls',
'plot.hclust',
'plot.histogram',
'plot.HoltWinters',
'plot.intervals.lmList',
'plot.isoreg',
'plot.lda',
'plot.lm',
'plot.lme',
'plot.lmList',
'plot.mca',
'plot.medpolish',
'plot.mlm',
'plot.mona',
'plot.mts',
'plot.new',
'plot.nffGroupedData',
'plot.nfnGroupedData',
'plot.nls',
'plot.nmGroupedData',
'plot.partition',
'plot.pdMat',
'plot.POSIXct',
'plot.POSIXlt',
'plot.ppr',
'plot.prcomp',
'plot.princomp',
'plot.profile.nls',
'plot.ranef.lme',
'plot.ranef.lmList',
'plot.ridgelm',
'plot.rpart',
'plot.shingle',
'plot.simulate.lme',
'plot.SOM',
'plot.somgrid',
'plot.spec',
'plot.spec.coherency',
'plot.spec.phase',
'plot.spline',
'plot.stepfun',
'plot.stl',
'plot.survfit',
'plot.table',
'plot.trls',
'plot.ts',
'plot.tskernel',
'plot.TukeyHSD',
'plot.Variogram',
'plot.window',
'plot.xy',
'plot.xyVector',
'plotcp',
'plotmath',
'plot-methods',
'plotNode',
'plotNodeLimit',
'plotViewport',
'pltree',
'pltree.twins',
'pluton',
'pmatch',
'pmax',
'pmin',
'pmvnorm',
'pmvt',
'pnbinom',
'png',
'pnorm',
'points',
'points.default',
'points.formula',
'points.survfit',
'pointsGrob',
'poisons',
'poisson',
'Poisson',
'polar',
'polr',
'poly',
'polygon',
'polygonGrob',
'polym',
'polyroot',
'polySpline',
'pooledSD',
'pop.saved.gpars',
'pop.saved.Rpars',
'pop.viewport',
'pop.vp',
'popViewport',
'pos.to.env',
'POSIXct',
'POSIXlt',
'possibleExtends',
'post',
'post.rpart',
'postDrawDetails',
'postscript',
'postscriptFont',
'postscriptFonts',
'power',
'power.anova.test',
'power.prop.test',
'power.t.test',
'PP.test',
'ppgetregion',
'ppinit',
'pplik',
'ppoints',
'ppois',
'ppr',
'ppregion',
'prcomp',
'precip',
'pred.rpart',
'predict',
'predict.ar',
'predict.Arima',
'predict.arima0',
'predict.bs',
'predict.bSpline',
'predict.coxph',
'predict.ellipsoid',
'predict.gam',
'predict.glm',
'predict.gls',
'predict.gnls',
'predict.HoltWinters',
'predict.lda',
'predict.lm',
'predict.lme',
'predict.lmList',
'predict.loess',
'predict.lqs',
'predict.mca',
'predict.mlm',
'predict.multinom',
'predict.nbSpline',
'predict.nlme',
'predict.nls',
'predict.nnet',
'predict.npolySpline',
'predict.ns',
'predict.pbSpline',
'predict.polr',
'predict.poly',
'predict.polySpline',
'predict.ppolySpline',
'predict.ppr',
'predict.princomp',
'predict.qda',
'predict.rpart',
'predict.smooth.spline',
'predict.StructTS',
'predict.survreg',
'predict.trls',
'preDrawDetails',
'prepanel.lmline',
'prepanel.loess',
'prepanel.qqmathline',
'preplot',
'presidents',
'pressure',
'pretty',
'prettyNum',
'Primitive',
'princomp',
'print',
'print.agnes',
'print.anova',
'print.anova.lme',
'print.anova.loglm',
'print.aov',
'print.aovlist',
'print.ar',
'print.Arima',
'print.arima0',
'print.AsIs',
'print.atomic',
'print.boot',
'print.bootci',
'print.bSpline',
'print.by',
'print.checkAssignFuns',
'print.checkDocArgs',
'print.checkDocStyle',
'print.checkFF',
'print.checkMethods',
'print.checkTnF',
'print.checkVignettes',
'print.clara',
'print.codoc',
'print.coefmat',
'print.compareFits',
'print.condition',
'print.connection',
'print.corNatural',
'print.correspondence',
'print.corStruct',
'print.corSymm',
'print.cox.zph',
'print.coxph',
'print.coxph.null',
'print.coxph.penal',
'print.data.frame',
'print.date',
'print.Date',
'print.default',
'print.density',
'print.diana',
'print.difftime',
'print.dissimilarity',
'print.dist',
'print.DLLInfo',
'print.DLLInfoList',
'print.DLLRegisteredRoutines',
'print.dummy.coef',
'print.ecdf',
'print.ellipsoid',
'print.factanal',
'print.factor',
'print.family',
'print.fanny',
'print.fitdistr',
'print.formula',
'print.fractions',
'print.ftable',
'print.gam',
'print.gamma.shape',
'print.glm',
'print.glm.dose',
'print.gls',
'print.grob',
'print.groupedData',
'print.hclust',
'print.HoltWinters',
'print.hsearch',
'print.htest',
'print.infl',
'print.integrate',
'print.intervals.gls',
'print.intervals.lme',
'print.intervals.lmList',
'print.lda',
'print.libraryIQR',
'print.listof',
'print.lm',
'print.lme',
'print.lmList',
'print.loadings',
'print.loess',
'print.logLik',
'print.loglm',
'print.lqs',
'print.matrix',
'print.mca',
'print.medpolish',
'print.modelStruct',
'print.mona',
'print.mtable',
'print.multinom',
'print.NativeRoutineList',
'print.nls',
'print.nnet',
'print.noquote',
'print.octmode',
'print.ordered',
'print.package_version',
'print.packageInfo',
'print.packageIQR',
'print.packageStatus',
'print.pairwise.htest',
'print.pam',
'print.pdMat',
'print.polr',
'print.polySpline',
'print.POSIXct',
'print.POSIXlt',
'print.power.htest',
'print.ppolySpline',
'print.ppr',
'print.prcomp',
'print.princomp',
'print.qda',
'print.ranef',
'print.ranef.lme',
'print.ratetable',
'print.recordedplot',
'print.restart',
'print.reStruct',
'print.ridgelm',
'print.rle',
'print.rlm',
'print.rms.curv',
'print.rpart',
'print.saddle.distn',
'print.SavedPlots',
'print.shingle',
'print.shingleLevel',
'print.simple.list',
'print.simplex',
'print.simulate.lme',
'print.smooth.spline',
'print.socket',
'print.stepfun',
'print.stl',
'print.StructTS',
'print.summary.agnes',
'print.summary.aov',
'print.summary.aovlist',
'print.summary.clara',
'print.summary.corNatural',
'print.summary.corStruct',
'print.summary.corSymm',
'print.summary.diana',
'print.summary.fanny',
'print.summary.gam',
'print.summary.glm',
'print.summary.gls',
'print.summary.lm',
'print.summary.lme',
'print.summary.lmList',
'print.summary.loess',
'print.summary.loglm',
'print.summary.manova',
'print.summary.modelStruct',
'print.summary.mona',
'print.summary.multinom',
'print.summary.negbin',
'print.summary.nls',
'print.summary.nnet',
'print.summary.pam',
'print.summary.pdMat',
'print.summary.polr',
'print.summary.ppr',
'print.summary.prcomp',
'print.summary.princomp',
'print.summary.rlm',
'print.summary.survfit',
'print.summary.survreg',
'print.summary.table',
'print.summary.varComb',
'print.summary.varFixed',
'print.summary.varFunc',
'print.Surv',
'print.survdiff',
'print.survexp',
'print.survfit',
'print.survreg',
'print.survreg.penal',
'print.table',
'print.tables.aov',
'print.terms',
'print.trellis',
'print.ts',
'print.tskernel',
'print.TukeyHSD',
'print.tukeyline',
'print.tukeysmooth',
'print.undoc',
'print.unit',
'print.varComb',
'print.VarCorr.lme',
'print.VarCov',
'print.varFunc',
'print.viewport',
'print.xtabs',
'printCoefmat',
'printcp',
'printNoClass',
'prmat',
'prmatrix',
'proc.time',
'processSocket',
'prod',
'profile',
'profile.glm',
'profile.nls',
'profiler',
'profiler.nls',
'progress',
'proj',
'promax',
'prompt',
'promptClass',
'promptData',
'promptMethods',
'prop.table',
'prop.test',
'prop.trend.test',
'prototype',
'provide',
'prune',
'pruneLevels',
'ps.options',
'psi.bisquare',
'psi.hampel',
'psi.huber',
'psigamma',
'psignrank',
'Psim',
'pspline',
'pt',
'ptukey',
'punif',
'Puromycin',
'push.saved.gpars',
'push.saved.Rpars',
'push.viewport',
'push.vp',
'pushBack',
'pushBackLength',
'pushViewport',
'pweibull',
'pwilcox',
'pyears',
'q',
'QA',
'qbeta',
'qbinom',
'qbirthday',
'QC',
'qcauchy',
'qchisq',
'qda',
'qexp',
'qf',
'qgamma',
'qgeom',
'qhyper',
'qlnorm',
'qlogis',
'qmvnorm',
'qmvt',
'qnbinom',
'qnorm',
'qpois',
'qq',
'qqline',
'qqmath',
'qqnorm',
'qqnorm.default',
'qqnorm.gls',
'qqnorm.lm',
'qqnorm.lme',
'qqnorm.lmList',
'qqnorm.nls',
'qqplot',
'qr',
'QR.Auxiliaries',
'qr.coef',
'qr.fitted',
'qr.Q',
'qr.qty',
'qr.qy',
'qr.R',
'qr.resid',
'qr.solve',
'qr.X',
'qsignrank',
'qt',
'qtukey',
'quade.test',
'quakes',
'quantile',
'quantile.default',
'quarters',
'quarters.Date',
'quarters.POSIXt',
'quasi',
'quasibinomial',
'quasipoisson',
'quine',
'Quinidine',
'quinModel',
'quit',
'qunif',
'quote',
'Quote',
'qweibull',
'qwilcox',
'R.home',
'R.Version',
'R_LIBS',
'r2dtable',
'Rabbit',
'Rail',
'rainbow',
'Random',
'random.effects',
'random.effects.lme',
'random.effects.lmList',
'Random.user',
'randu',
'ranef',
'ranef.lme',
'ranef.lmList',
'range',
'range.default',
'rank',
'ratetable',
'ratetables',
'rational',
'RatPupWeight',
'rats',
'raw',
'rawShift',
'rawToBits',
'rawToChar',
'rbeta',
'rbind',
'rbind.data.frame',
'rbinom',
'rcauchy',
'rchisq',
'RClassUtils',
'Rconsole',
'Rd2dvi',
'Rd2txt',
'Rdconv',
'Rdevga',
'Rdindex',
'RdUtils',
'Re',
'read.00Index',
'read.csv',
'read.csv2',
'read.dcf',
'read.delim',
'read.delim2',
'read.dta',
'read.epiinfo',
'read.fortran',
'read.ftable',
'read.fwf',
'read.mtp',
'read.S',
'read.socket',
'read.spss',
'read.ssd',
'read.table',
'read.table.url',
'read.xport',
'readBin',
'readChar',
'readCitationFile',
'readClipboard',
'readline',
'readLines',
'real',
'recalc',
'recalc.corAR1',
'recalc.corARMA',
'recalc.corCAR1',
'recalc.corCompSymm',
'recalc.corHF',
'recalc.corIdent',
'recalc.corNatural',
'recalc.corSpatial',
'recalc.corStruct',
'recalc.corSymm',
'recalc.modelStruct',
'recalc.reStruct',
'recalc.varFunc',
'recalc.varIdent',
'Recall',
'reconcilePropertiesAndPrototype',
'record',
'recordPlot',
'recover',
'rect',
'rect.hclust',
'rectGrob',
'recycle.data',
'reduce.nn',
'reformulate',
'reg.finalizer',
'regex',
'regexpr',
'registerS3method',
'registerS3methods',
'Relaxin',
'relevel',
'rematchDefinition',
'Remifentanil',
'remission',
'remove',
'REMOVE',
'remove.packages',
'removeCConverter',
'removeClass',
'removeGeneric',
'removeGrob',
'removeMethod',
'removeMethods',
'removeMethodsObject',
'removeTaskCallback',
'renumerate',
'renumerate.formula',
'Renviron.site',
'reorder',
'rep',
'rep.Date',
'rep.default',
'rep.int',
'rep.POSIXct',
'rep.POSIXlt',
'replace',
'replayPlot',
'replicate',
'replications',
'report',
'reportGraph',
'representation',
'require',
'Require',
'requireMethods',
'resetClass',
'resetGeneric',
'reshape',
'reshapeLong',
'reshapeWide',
'resid',
'residuals',
'residuals.coxph',
'residuals.default',
'residuals.gam',
'residuals.glm',
'residuals.gls',
'residuals.glsStruct',
'residuals.gnls',
'residuals.gnlsStruct',
'residuals.HoltWinters',
'residuals.lm',
'residuals.lme',
'residuals.lmeStruct',
'residuals.lmList',
'residuals.loglm',
'residuals.nlmeStruct',
'residuals.nls',
'residuals.nnet',
'residuals.rpart',
'residuals.survreg',
'residuals.trls',
'residuals.tukeyline',
'restart',
'restartDescription',
'restartFormals',
'reStruct',
'rev',
'rev.default',
'rexp',
'rf',
'rfs',
'rgamma',
'rgb',
'rgb2hsv',
'rgeom',
'rhyper',
'ridge',
'rivers',
'rle',
'rlm',
'rlnorm',
'rlogis',
'rm',
'RMethodUtils',
'rms.curv',
'rmTemp',
'rmultinom',
'rmvnorm',
'rmvt',
'rnbinom',
'rnegbin',
'RNG',
'RNGkind',
'RNGversion',
'rnorm',
'road',
'rock',
'rotifer',
'round',
'Round',
'round.Date',
'round.difftime',
'round.POSIXt',
'row',
'row.names',
'row.names.data.frame',
'row.names.default',
'row.spec',
'rowMeans',
'rownames',
'Rows',
'rowsum',
'rowsum.data.frame',
'rowsum.default',
'rowSums',
'rpart',
'rpart.anova',
'rpart.branch',
'rpart.class',
'rpart.control',
'rpart.exp',
'rpart.matrix',
'rpart.object',
'rpart.poisson',
'rpartcallback',
'rpartco',
'rpconvert',
'rpois',
'Rprof',
'Rprofile',
'rsignrank',
'rsq.rpart',
'rstandard',
'rstandard.glm',
'rstandard.lm',
'rstudent',
'rstudent.glm',
'rstudent.lm',
'rt',
'Rtangle',
'RtangleSetup',
'RtangleWritedoc',
'Rubber',
'rug',
'runif',
'runmed',
'ruspini',
'RweaveChunkPrefix',
'RweaveEvalWithOpt',
'RweaveHTML',
'RweaveHTMLFinish',
'RweaveHTMLOptions',
'RweaveHTMLRuncode',
'RweaveHTMLSetup',
'RweaveHTMLWritedoc',
'RweaveLatex',
'RweaveLatexOptions',
'RweaveLatexSetup',
'RweaveTryStop',
'rweibull',
'rwilcox',
'Rwin',
's',
'saddle',
'saddle.distn',
'saddle.distn.object',
'SafePrediction',
'salinity',
'sammon',
'sample',
'SANtest',
'sapply','sapply_pb',
'save',
'save.image',
'saved.pars',
'savehistory',
'saveNamespaceImage',
'savePlot',
'scale',
'scale.default',
'scan',
'scan.url',
'scatter.smooth',
'SClassExtension',
'screen',
'screeplot',
'sd',
'Sd2Rd',
'se.aov',
'se.contrast',
'sealClass',
'search',
'searchpaths',
'Seatbelts',
'seek',
'seek.connection',
'seekViewport',
'segments',
'segmentsGrob',
'select',
'select.list',
'selectMethod',
'selfStart',
'selfStart.default',
'selfStart.formula',
'semat',
'sendSocket',
'sep',
'seq',
'seq.Date',
'seq.default',
'seq.POSIXt',
'sequence',
'serialize',
'Session',
'sessionData',
'sessionInfo',
'set.gpar',
'set.seed',
'set.value.grob',
'set.viewport',
'setAs',
'setCConverterStatus',
'setChildren',
'setClass',
'setClassUnion',
'setDataPart',
'setdiff',
'setequal',
'setGeneric',
'setGrob',
'setGroupGeneric',
'setHook',
'setIs',
'setMethod',
'setNames',
'setNamespaceInfo',
'setOldClass',
'setPackageName',
'setPrimitiveMethods',
'setReplaceMethod',
'sets',
'setSClass',
'setValidity',
'setwd',
'setWindowTitle',
'shapiro.test',
'shell',
'shell.exec',
'Shepard',
'shingle',
'ships',
'SHLIB',
'shoes',
'show',
'show.settings',
'showClass',
'showConnections',
'showDefault',
'showExtends',
'showMethods',
'showMlist',
'shQuote',
'shrimp',
'shuttle',
'sign',
'signalCondition',
'signature',
'SignatureMethod',
'signif',
'SignRank',
'sigToEnv',
'silhouette',
'simint',
'simint.default',
'simint.formula',
'simint.glm',
'simint.lm',
'simpleCondition',
'simpleError',
'simpleKey',
'simpleWarning',
'simplex',
'simplex.object',
'simtest',
'simtest.default',
'simtest.formula',
'simtest.glm',
'simtest.lm',
'simulate.lme',
'sin',
'singer',
'single',
'sinh',
'sink',
'sink.number',
'Sitka',
'Sitka89',
'sizeDiss',
'skip',
'Skye',
'sleep',
'slice.index',
'slot',
'slotNames',
'slotOp',
'SModeNames',
'smooth',
'smooth.f',
'smooth.spline',
'smoothEnds',
'snails',
'snip.rpart',
'snip.rpart.mouse',
'socketConnection',
'socketSelect',
'solder',
'solve',
'solve.default',
'solve.pdBlocked',
'solve.pdDiag',
'solve.pdIdent',
'solve.pdLogChol',
'solve.pdMat',
'solve.pdNatural',
'solve.pdSymm',
'solve.qr',
'solve.reStruct',
'SOM',
'somgrid',
'sort',
'sort.list',
'sortedXyData',
'sortSilhouette',
'source',
'source.url',
'Soybean',
'SP500',
'spatial',
'spec',
'spec.ar',
'spec.pgram',
'spec.taper',
'Special',
'spectrum',
'sphercov',
'spline',
'spline.des',
'splineDesign',
'splinefun',
'splineKnots',
'splineOrder',
'splines',
'split',
'split.data.frame',
'split.default',
'split.screen',
'splitFormula',
'splom',
'sprintf',
'Spruce',
'sqrt',
'sQuote',
'SSasymp',
'SSasympOff',
'SSasympOrig',
'SSbiexp',
'SSfol',
'SSfpl',
'SSgompertz',
'SSI',
'SSlogis',
'SSmicmen',
'SSweibull',
'stack',
'stack.data.frame',
'stack.default',
'stack.loss',
'stack.viewports',
'stack.x',
'stackloss',
'standardGeneric',
'stanford2',
'Stangle',
'stars',
'start',
'startSocketServer',
'Startup',
'stat.anova',
'state',
'stderr',
'stdin',
'stdout',
'stdres',
'steam',
'stem',
'step',
'stepAIC',
'stepfun',
'stl',
'stlmethods',
'stop',
'stopifnot',
'stopSocketServer',
'storage.mode',
'stormer',
'str',
'str.logLik',
'str.POSIXt',
'strata',
'Strauss',
'strftime',
'strheight',
'stringHeight',
'stringWidth',
'strip.custom',
'strip.default',
'strip.white',
'stripchart',
'stripplot',
'strptime',
'strsplit',
'StructTS',
'structure',
'StructureClasses',
'strwidth',
'strwrap',
'stud.ci',
'studres',
'sub',
'Subscript',
'subset',
'subset.data.frame',
'subset.default',
'substitute',
'substituteDirect',
'substituteFunctionArgs',
'substr',
'substring',
'sum',
'summary',
'Summary',
'summary.agnes',
'summary.aov',
'summary.aovlist',
'summary.clara',
'summary.connection',
'summary.corAR1',
'summary.corARMA',
'summary.corCAR1',
'summary.corCompSymm',
'summary.corExp',
'summary.corGaus',
'summary.corHF',
'summary.corIdent',
'summary.corLin',
'summary.corNatural',
'summary.corRatio',
'summary.corSpher',
'summary.corStruct',
'summary.corSymm',
'summary.coxph',
'summary.coxph.penal',
'summary.data.frame',
'Summary.data.frame',
'summary.Date',
'Summary.date',
'summary.default',
'summary.diana',
'Summary.difftime',
'summary.dissimilarity',
'summary.ecdf',
'summary.factor',
'Summary.factor',
'summary.fanny',
'Summary.fractions',
'summary.gam',
'summary.glm',
'summary.gls',
'summary.infl',
'summary.lm',
'summary.lme',
'summary.lmList',
'summary.loess',
'summary.loglm',
'summary.manova',
'summary.matrix',
'summary.mlm',
'summary.modelStruct',
'summary.mona',
'summary.multinom',
'summary.negbin',
'summary.nls',
'summary.nlsList',
'summary.nnet',
'Summary.package_version',
'summary.packageStatus',
'summary.pam',
'summary.pdBlocked',
'summary.pdCompSymm',
'summary.pdDiag',
'summary.pdIdent',
'summary.pdLogChol',
'summary.pdMat',
'summary.pdNatural',
'summary.pdSymm',
'summary.polr',
'summary.POSIXct',
'Summary.POSIXct',
'summary.POSIXlt',
'Summary.POSIXlt',
'summary.ppr',
'summary.prcomp',
'summary.princomp',
'summary.ratetable',
'summary.reStruct',
'summary.rlm',
'summary.rpart',
'summary.shingle',
'summary.stepfun',
'summary.stl',
'Summary.Surv',
'summary.survfit',
'summary.survreg',
'summary.table',
'summary.trls',
'summary.tukeysmooth',
'Summary.unit',
'summary.varComb',
'summary.varConstPower',
'summary.VarCorr.lme',
'summary.varExp',
'summary.varFixed',
'summary.varFunc',
'summary.varIdent',
'summary.varPower',
'summaryRprof',
'sunflowerplot',
'sunspot',
'sunspots',
'superClassDepth',
'suppressWarnings',
'supsmu',
'surf.gls',
'surf.ls',
'Surv',
'survdiff',
'survexp',
'survexp.az',
'survexp.azr',
'survexp.cfit',
'survexp.fit',
'survexp.fl',
'survexp.flr',
'survexp.mn',
'survexp.mnwhite',
'survexp.us',
'survexp.usr',
'survexp.uswhite',
'survexp.wnc',
'survey',
'survfit',
'survfit.coxph.null',
'survfit.km',
'survfit.object',
'survival',
'survobrien',
'survReg',
'survreg.control',
'survreg.distributions',
'survreg.object',
'survreg.old',
'svd',
'swap.origin.horizontal',
'swap.origin.vertical',
'Sweave',
'SweaveHooks',
'SweaveSyntaxLatex',
'SweaveSyntaxNoweb',
'SweaveSyntConv',
'sweep',
'swiss',
'symbol.C',
'symbol.For',
'symbols',
'symnum',
'Syntax',
'synth.te',
'synth.tr',
'sys.call',
'sys.calls',
'Sys.Date',
'sys.frame',
'sys.frames',
'sys.function',
'Sys.getenv',
'Sys.getlocale',
'Sys.getpid',
'Sys.info',
'sys.load.image',
'Sys.localeconv',
'sys.nframe',
'sys.on.exit',
'sys.parent',
'sys.parents',
'Sys.putenv',
'sys.save.image',
'Sys.setlocale',
'Sys.sleep',
'sys.source',
'sys.status',
'Sys.time',
'Sys.timezone',
'system',
'system.file',
'system.time',
't',
't.data.frame',
't.default',
't.fractions',
't.test',
'table',
'tabulate',
'tail',
'tan',
'tanh',
'tapply',
'taskCallback',
'taskCallbackManager',
'taskCallbackNames',
'tau',
'tclArray',
'tclclose',
'TclInterface',
'tclObj',
'tclopen',
'tclputs',
'tclread',
'tclRequire',
'tcltk',
'tclvalue',
'tclVar',
'tcut',
'TDist',
'tempdir',
'tempdirWin',
'TempEnv',
'tempfile',
'tempvar',
'termplot',
'terms',
'terms.aovlist',
'terms.default',
'terms.formula',
'terms.object',
'terms.terms',
'terrain.colors',
'testPlatformEquivalence',
'testVirtual',
'Tetracycline1',
'Tetracycline2',
'tetragamma',
'texi2dvi',
'text',
'text.default',
'text.rpart',
'textConnection',
'textGrob',
'Theoph',
'theta.maxl',
'theta.md',
'theta.ml',
'theta.mm',
'tilde',
'tilt.boot',
'time',
'Titanic',
'title',
'tkactivate',
'tkadd',
'tkaddtag',
'tkbbox',
'tkbell',
'tkbind',
'tkbindtags',
'tkbutton',
'tkcanvas',
'tkcanvasx',
'tkcanvasy',
'tkcget',
'tkcheckbutton',
'tkchooseDirectory',
'tkclipboard.append',
'tkclipboard.clear',
'tkclose',
'tkcmd',
'TkCommands',
'tkcompare',
'tkconfigure',
'tkcoords',
'tkcreate',
'tkcurselection',
'tkdchars',
'tkdebug',
'tkdelete',
'tkdelta',
'tkdeselect',
'tkdestroy',
'tkdialog',
'tkdlineinfo',
'tkdtag',
'tkdump',
'tkentry',
'tkentrycget',
'tkentryconfigure',
'tkevent.add',
'tkevent.delete',
'tkevent.generate',
'tkevent.info',
'tkfile.dir',
'tkfile.tail',
'tkfind',
'tkflash',
'tkfocus',
'tkfont.actual',
'tkfont.configure',
'tkfont.create',
'tkfont.delete',
'tkfont.families',
'tkfont.measure',
'tkfont.metrics',
'tkfont.names',
'tkfraction',
'tkframe',
'tkget',
'tkgetOpenFile',
'tkgetSaveFile',
'tkgettags',
'tkgrab',
'tkgrab.current',
'tkgrab.release',
'tkgrab.set',
'tkgrab.status',
'tkgrid',
'tkgrid.bbox',
'tkgrid.columnconfigure',
'tkgrid.configure',
'tkgrid.forget',
'tkgrid.info',
'tkgrid.location',
'tkgrid.propagate',
'tkgrid.remove',
'tkgrid.rowconfigure',
'tkgrid.size',
'tkgrid.slaves',
'tkicursor',
'tkidentify',
'tkimage.cget',
'tkimage.configure',
'tkimage.create',
'tkimage.names',
'tkindex',
'tkinsert',
'tkinvoke',
'tkitembind',
'tkitemcget',
'tkitemconfigure',
'tkitemfocus',
'tkitemlower',
'tkitemraise',
'tkitemscale',
'tklabel',
'tklistbox',
'tklower',
'tkmark.gravity',
'tkmark.names',
'tkmark.next',
'tkmark.previous',
'tkmark.set',
'tkmark.unset',
'tkmenu',
'tkmenubutton',
'tkmessage',
'tkmessageBox',
'tkmove',
'tknearest',
'tkopen',
'tkpack',
'tkpack.configure',
'tkpack.forget',
'tkpack.info',
'tkpack.propagate',
'tkpack.slaves',
'tkpager',
'tkplace',
'tkplace.configure',
'tkplace.forget',
'tkplace.info',
'tkplace.slaves',
'tkpopup',
'tkpost',
'tkpostcascade',
'tkpostscript',
'tkputs',
'tkradiobutton',
'tkraise',
'tkread',
'tkscale',
'tkscan.dragto',
'tkscan.mark',
'tkscrollbar',
'tksearch',
'tksee',
'tkselect',
'tkselection.adjust',
'tkselection.anchor',
'tkselection.clear',
'tkselection.from',
'tkselection.includes',
'tkselection.present',
'tkselection.range',
'tkselection.set',
'tkselection.to',
'tkset',
'tksize',
'tkStartGUI',
'tktag.add',
'tktag.bind',
'tktag.cget',
'tktag.configure',
'tktag.delete',
'tktag.lower',
'tktag.names',
'tktag.nextrange',
'tktag.prevrange',
'tktag.raise',
'tktag.ranges',
'tktag.remove',
'tktext',
'tktitle',
'tktoggle',
'tktoplevel',
'tktype',
'tku',
'tkunpost',
'tkwait.variable',
'tkwait.visibility',
'tkwait.window',
'tkwidget',
'TkWidgetcmds',
'TkWidgets',
'tkwindow.cget',
'tkwindow.configure',
'tkwindow.create',
'tkwindow.names',
'tkwinfo',
'tkwm.aspect',
'tkwm.client',
'tkwm.colormapwindows',
'tkwm.command',
'tkwm.deiconify',
'tkwm.focusmodel',
'tkwm.frame',
'tkwm.geometry',
'tkwm.grid',
'tkwm.group',
'tkwm.ico',
'tkwm.iconbitmap',
'tkwm.iconify',
'tkwm.iconmask',
'tkwm.iconname',
'tkwm.iconposition',
'tkwm.iconwindow',
'tkwm.maxsize',
'tkwm.minsize',
'tkwm.overrideredirect',
'tkwm.positionfrom',
'tkwm.protocol',
'tkwm.resizable',
'tkwm.sizefrom',
'tkwm.state',
'tkwm.title',
'tkwm.transient',
'tkwm.withdraw',
'tkXselection.clear',
'tkXselection.get',
'tkXselection.handle',
'tkXselection.own',
'tkxview',
'tkxview.moveto',
'tkxview.scroll',
'tkyposition',
'tkyview',
'tkyview.moveto',
'tkyview.scroll',
'tmd',
'toBibtex',
'toeplitz',
'toLatex',
'tolower',
'ToothGrowth',
'topenv',
'topicName',
'topo',
'topo.colors',
'toString',
'toString.default',
'toupper',
'trace',
'traceback',
'TraceClasses',
'traceOff',
'traceOn',
'tracingState',
'Traffic',
'transform',
'transform.data.frame',
'transform.default',
'treering',
'trees',
'trellis.device',
'trellis.focus',
'trellis.grobname',
'trellis.last.object',
'trellis.object',
'trellis.panelArgs',
'trellis.par.get',
'trellis.par.set',
'trellis.switchFocus',
'trellis.unfocus',
'trellis.vpname',
'Trig',
'trigamma',
'trls.influence',
'trmat',
'truehist',
'trunc',
'trunc.Date',
'trunc.POSIXt',
'truncate',
'truncate.connection',
'try',
'tryCatch',
'tryNew',
'trySilent',
'ts',
'ts.intersect',
'ts.plot',
'ts.return',
'ts.union',
'tsboot',
'tsdiag',
'tsp',
'tsSmooth',
'Tukey',
'TukeyHSD',
'TukeyHSD.aov',
'tuna',
'twins.object',
'type.convert',
'typeof',
'UCBAdmissions',
'ucv',
'UKDriverDeaths',
'UKgas',
'UKLungDeaths',
'undebug',
'undoc',
'Uniform',
'union',
'unique',
'unique.array',
'unique.data.frame',
'unique.default',
'unique.matrix',
'uniquecombs',
'uniroot',
'unit',
'unit.arithmetic',
'unit.c',
'unit.length',
'unit.list',
'unit.list.from.list',
'unit.list.length',
'unit.list.rep',
'unit.pmax',
'unit.pmin',
'unit.rep',
'units',
'unix',
'unix.time',
'unlink',
'unlist',
'unloadNamespace',
'unlockBinding',
'unname',
'unRematchDefinition',
'unserialize',
'unset.gpar',
'unsplit',
'unstack',
'unstack.data.frame',
'unstack.default',
'unstack.viewports',
'untangle.specials',
'untrace',
'unz',
'update',
'update.corStruct',
'update.default',
'update.formula',
'update.gls',
'update.gnls',
'update.groupedData',
'update.lme',
'update.lmList',
'update.loglm',
'update.modelStruct',
'update.nlme',
'update.nlsList',
'update.packages',
'update.packageStatus',
'update.reStruct',
'update.trellis',
'update.varComb',
'update.varConstPower',
'update.varExp',
'update.varExpon',
'update.varFunc',
'update.varPower',
'updateCol',
'updateLocalPackages',
'updateRow',
'upgrade',
'upper.to.lower.tri.inds',
'upper.tri',
'upViewport',
'urine',
'url',
'url.show',
'USAccDeaths',
'USArrests',
'UScereal',
'UScrime',
'UseMethod',
'userdir',
'UserHooks',
'USJudgeRatings',
'USPersonalExpenditure',
'uspop',
'ut',
'VA',
'VADeaths',
'valid.data',
'valid.just',
'valid.layout',
'valid.origin',
'valid.pch',
'valid.unit',
'valid.units',
'valid.viewport',
'validDetails',
'validGP',
'validObject',
'validSlotNames',
'var',
'var.linear',
'var.test',
'varClasses',
'varComb',
'varConstPower',
'VarCorr',
'varExp',
'varFixed',
'varFunc',
'variable.names',
'varIdent',
'varimax',
'Variogram',
'Variogram.corExp',
'Variogram.corGaus',
'Variogram.corLin',
'Variogram.corRatio',
'Variogram.corSpatial',
'Variogram.corSpher',
'Variogram.default',
'Variogram.gls',
'Variogram.lme',
'varPower',
'varWeights',
'varWeights.glsStruct',
'varWeights.lmeStruct',
'varWeights.varComb',
'varWeights.varFunc',
'vcov',
'vcov.multinom',
'vcov.polr',
'vector',
'version',
'Version',
'veteran',
'vi',
'view',
'viewHTMLinit',
'viewport',
'viewport.layout',
'viewport.transform',
'Viewports',
'vignette',
'vignetteDepends',
'volcano',
'volume',
'votes.repub',
'vpList',
'vpPath',
'vpStack',
'vpTree',
'waders',
'Wafer',
'warning',
'warnings',
'warpbreaks',
'weekdays',
'weekdays.Date',
'weekdays.POSIXt',
'Weibull',
'weighted.mean',
'weighted.residuals',
'weights',
'weights.glm',
'weights.lm',
'weights.nls',
'Wheat',
'Wheat2',
'which',
'which.is.max',
'which.max',
'which.min',
'whiteside',
'width',
'width.details',
'width.details.default',
'width.details.frame',
'width.details.rect',
'width.details.text',
'width.details.viewport',
'width.frame',
'width.lines',
'width.post',
'width.post.details.default',
'width.pre',
'width.pre.details.default',
'width.SJ',
'width.text',
'widthDetails',
'wilcox.test',
'Wilcoxon',
'win.graph',
'win.metafile',
'win.print',
'win.version',
'WinAnsi',
'winDialog',
'winDialogString',
'window',
'windows',
'windowsFont',
'windowsFonts',
'winextras',
'winMenuAdd',
'winMenuAddItem',
'winMenuDel',
'winMenuDelItem',
'winMenuItems',
'winMenuNames',
'winMenus',
'wireframe',
'with',
'with.default',
'withCallingHandlers',
'withRestarts',
'women',
'write',
'write.dcf',
'write.dta',
'write.ftable',
'write.matrix',
'write.socket',
'write.table',
'writeBin',
'writeChar',
'writeClipboard',
'writeLines',
'wsbrowser',
'wtloss',
'WWWusage',
'x11',
'X11',
'xaxisGrob',
'xclara',
'xedit',
'xemacs',
'xfig',
'xinch',
'xor',
'xpdrows.data.frame',
'xpred.rpart',
'xtabs',
'xy.coords',
'xyinch',
'xyplot',
'xyVector',
'xyz.coords',
'yaxisGrob',
'yinch',
'zapsmall',
'zcbind',
'zero',
'zip.file.extract',
'zip.unpack',

'read.maimages',
'modelMatrix',
'readGAL',
'plotMA3by2',
'backgroundCorrect',
'plotDensities',
'normalizeWithinArrays',
'plotDensities',
'normalizeBetweenArrays',
'duplicateCorrelation',
'lmFit',
'Test_Contrast',
'contrasts.fit',
'eBayes',
'topTable',
'byrow'
);
 MODIFIERSWORDCOUNT = 102;
 MODIFIERWORDS: array [1..MODIFIERSWORDCOUNT] of String =
///
(
'abline',
'adj',
'angle',
'ann',
'arrows',
'ask',
'axes',
'axis.lty',
'axisnames',
'beside',
'bg',
'border',
'br',
'breaks',
'bty',
'cex',
'cex.axis',
'cex.lab',
'cex.main',
'cex.names',
'cex.sub',
'cin',
'col',
'col.axis',
'col.lab',
'col.main',
'col.sub',
'cra',
'crt',
'csi',
'cxy',
'din',
'err',
'fg',
'fig',
'fin',
'font',
'font.axis',
'font.lab',
'font.main',
'font.sub',
'freq',
'gamma',
'h',
'horiz',
'include.lowest',
'inside',
'lab',
'las',
'legend.text',
'lend',
'lheight',
'ljoin',
'lmitre',
'lty',
'lwd',
'mai',
'main',
'mar',
'mex',
'mfcol',
'mfg',
'mfrow',
'mgp',
'mkh',
'mtext',
'names.arg',
'nclass',
'notch',
'new',
'oma',
'omd',
'omi',
'pch',
'pin',
'plt',
'prob',
'probability',
'ps',
'pty',
'right',
'smo',
'space',
'srt',
'tck',
'tcl',
'tmag',
'type',
'usr',
'xaxp',
'xaxs',
'xaxt',
'xlab',
'xlim',
'xlog',
'xpd',
'yaxp',
'yaxs',
'yaxt',
'ylab',
'ylim',
'ylog'
);
var
  f: Integer;
begin

//if not Assigned (GlobalKeywords) then
  begin
    // Create the string list of keywords - only once
    GlobalKeywords := TStringList.Create;
    GlobalKeywords.CaseSensitive:=true;
    GlobalKeywords.Sorted:=true;
//    GlobalKeywords.Duplicates:=dupIgnore;
    for f := 1 to KEYWORDCOUNT do
      GlobalKeywords.AddObject(KEYWORDSIdents[f], TObject(Ord(tkKey)));
    for f := 1 to NONKEYWORDCOUNT do
      GlobalKeywords.AddObject(NONKEYWORDS[f], TObject(Ord(tkNonKeyword)));
    for f := 1 to MODIFIERSWORDCOUNT do
      GlobalKeywords.AddObject(MODIFIERWORDS[f], TObject(Ord(tkModifier)));
  end; // if
  TheWords.assign(GlobalKeywords);
  GlobalKeywords.free;
  GlobalKeywords:=nil;
end;

function TSynRSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  i: Integer;
  temp: PChar;
  s: String;
begin
  // Extract the identifier out - it is assumed to terminate in a
  //   non-alphanumeric character
  fToIdent := MayBe;
  temp := MayBe;
  while IsIdentChar(temp^) do
    Inc(temp);
  fStringLen := temp - fToIdent;

  // Check to see if it is a keyword
  SetString(s, fToIdent, fStringLen);
  if FKeywords.Find(s, i) then
  begin
    // TStringList is not case sensitive!
    if s <> FKeywords[i] then
      i := -1;
  end
  else
    i := -1;

  if i <> -1 then
    Result := TtkTokenKind(PtrInt(FKeywords.Objects[i]))

  // Check if it is a system identifier (__*__)
  else if (fStringLen >= 5) and
     (MayBe[0] = '_') and (MayBe[1] = '_') and (MayBe[2] <> '_') and
     (MayBe[fStringLen - 1] = '_') and (MayBe[fStringLen - 2] = '_') and
     (MayBe[fStringLen - 3] <> '_') then
    Result := tkSystemDefined

  // Else, hey, it is an ordinary run-of-the-mill identifier!
  else
    Result := tkIdentifier;
end;

constructor TSynRSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

//  fCaseSensitive := True;

  FKeywords := TStringList.Create;
  FKeywords.Sorted := True;
  FKeywords.Duplicates := dupError;
  FKeywords.CaseSensitive:=True;
//  FKeywords.Assign (GetKeywordIdentifiers);
  GetKeywordIdentifiers(FKeywords);

  fRange := rsUnknown;
  fCommentAttri := TSynHighlighterAttributes.Create(@SYNS_AttrComment, SYNS_XML_AttrComment);
  fCommentAttri.Foreground := clGray;
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(@SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(@SYNS_AttrReservedWord, SYNS_XML_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);

  fNonKeyAttri := TSynHighlighterAttributes.Create (@SYNS_AttrNonReservedKeyword, SYNS_XML_AttrNonReservedKeyword);
  fNonKeyAttri.Foreground := clNavy;
  fNonKeyAttri.Style := [fsBold];
  AddAttribute (fNonKeyAttri);

  fModifierAttri := TSynHighlighterAttributes.Create (@SYNS_AttrModifier, SYNS_AttrModifier);
  fModifierAttri.Foreground := clfuchsia;
  fModifierAttri.Style := [fsBold];
  AddAttribute (fModifierAttri);

  fSystemAttri := TSynHighlighterAttributes.Create (@SYNS_AttrSystem, SYNS_xml_AttrSystem);
  fSystemAttri.Style := [fsBold];
  AddAttribute (fSystemAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(@SYNS_AttrNumber, SYNS_xml_AttrNumber);
  fNumberAttri.Foreground := clBlue;
  AddAttribute(fNumberAttri);
  fHexAttri := TSynHighlighterAttributes.Create(@SYNS_AttrHexadecimal, SYNS_xml_AttrHexadecimal);
  fHexAttri.Foreground := clBlue;
  AddAttribute(fHexAttri);
  fOctalAttri := TSynHighlighterAttributes.Create(@SYNS_AttrOctal, SYNS_xml_AttrOctal);
  fOctalAttri.Foreground := clBlue;
  AddAttribute(fOctalAttri);
  fFloatAttri := TSynHighlighterAttributes.Create(@SYNS_AttrFloat, SYNS_xml_AttrFloat);
  fFloatAttri.Foreground := clBlue;
  AddAttribute(fFloatAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(@SYNS_AttrSpace, SYNS_xml_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(@SYNS_AttrString, SYNS_xml_AttrString);
  fStringAttri.Foreground := clBlue;
  AddAttribute(fStringAttri);
  fDocStringAttri := TSynHighlighterAttributes.Create(@SYNS_AttrDocumentation, SYNS_xml_AttrDocumentation);
  fDocStringAttri.Foreground := clTeal;
  AddAttribute(fDocStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(@SYNS_AttrSymbol, SYNS_xml_AttrSymbol);
  AddAttribute(fSymbolAttri);
  fErrorAttri := TSynHighlighterAttributes.Create(@SYNS_AttrSyntaxError, SYNS_xml_AttrSyntaxError);
  fErrorAttri.Foreground := clRed;
  AddAttribute(fErrorAttri);
  SetAttributesOnChange(@DefHighlightChange);
  fDefaultFilter := SYNS_FilterR;
end; { Create }

destructor TSynRSyn.Destroy;
begin
  FKeywords.Free;
  inherited;
end;

procedure TSynRSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynRSyn.CRProc;
begin
  fTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: inc(Run, 2);
  else
    inc(Run);
  end;
end;

function TSynRSyn.isLineEnd(rn:Longint):boolean; inline;
begin
  result :=  Fline[rn] in [#13, #10, #0];
end;

procedure TSynRSyn.CommentProc;
begin
  fTokenID := tkComment;
  inc(Run);
  while not IsLineEnd(Run) do
    inc(Run);
end;

procedure TSynRSyn.GreaterProc;
begin
  case FLine[Run + 1] of
    '=': begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynRSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
end;

procedure TSynRSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynRSyn.LowerProc;
begin
  case FLine[Run + 1] of
    '=': begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>': begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end
  else begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynRSyn.NullProc;
begin
  fTokenID := tkNull;
//inc(Run);
end;

procedure TSynRSyn.NumberProc;
type
  TNumberState =
    (
    nsStart,
    nsDotFound,
    nsFloatNeeded,
    nsHex,
    nsOct,
    nsExpFound
    );

var
  temp: WideChar;
  State: TNumberState;

  function CheckSpecialCases: Boolean;
  begin
    case temp of
      // Look for dot (.)
      '.': begin
        // .45
        if CharInSet(FLine[Run], ['0'..'9']) then
        begin
          Inc (Run);
          fTokenID := tkFloat;
          State := nsDotFound;

        // Non-number dot
        end else begin
          // Ellipsis
          if (FLine[Run] = '.') and (FLine[Run+1] = '.') then
            Inc (Run, 2);
          fTokenID := tkSymbol;
          Result := False;
          Exit;
        end; // if
      end; // DOT

      // Look for zero (0)
      '0': begin
        temp := FLine[Run];
        // 0x123ABC
        if CharInSet(temp, ['x', 'X']) then begin
          Inc (Run);
          fTokenID := tkHex;
          State := nsHex;
        // 0.45
        end else if temp = '.' then begin
          Inc (Run);
          State := nsDotFound;
          fTokenID := tkFloat;
        end else if CharInSet(temp, ['0'..'9']) then begin
          Inc (Run);
          // 0123 or 0123.45
          if CharInSet(temp, ['0'..'7']) then begin
            fTokenID := tkOct;
            State := nsOct;
          // 0899.45
          end else begin
            fTokenID := tkFloat;
            State := nsFloatNeeded;
          end; // if
        end; // if
      end; // ZERO
    end; // case

    Result := True;
  end; // CheckSpecialCases

  function HandleBadNumber: Boolean;
  begin
    Result := False;
    fTokenID := tkUnknown;
    // Ignore all tokens till end of "number"
    while IsIdentChar(FLine[Run]) or (FLine[Run] = '.') do
      Inc (Run);
  end; // HandleBadNumber

  function HandleExponent: Boolean;
  begin
    State := nsExpFound;
    fTokenID := tkFloat;
    // Skip e[+/-]
    if CharInSet(FLine[Run+1], ['+', '-']) then
      Inc (Run);
    // Invalid token : 1.0e
    if not CharInSet(FLine[Run+1], ['0'..'9']) then begin
      Inc (Run);
      Result := HandleBadNumber;
      Exit;
    end; // if

    Result := True;
  end; // HandleExponent

  function HandleDot: Boolean;
  begin
    // Check for ellipsis
    Result := (FLine[Run+1] <> '.') or (FLine[Run+2] <> '.');
    if Result then begin
      State := nsDotFound;
      fTokenID := tkFloat;
    end; // if
  end; // HandleDot

  function CheckStart: Boolean;
  begin
    // 1234
    if CharInSet(temp, ['0'..'9']) then begin
      Result := True;
    //123e4
    end else if CharInSet(temp, ['e', 'E']) then begin
      Result := HandleExponent;
    // 123.45j
    end else if CharInSet(temp, ['j', 'J']) then begin
      Inc (Run);
      fTokenID := tkFloat;
      Result := False;
    // 123.45
    end else if temp = '.' then begin
      Result := HandleDot;
    // Error!
    end else if IsIdentChar(temp) then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckStart

  function CheckDotFound: Boolean;
  begin
    // 1.0e4
    if CharInSet(temp, ['e', 'E']) then begin
      Result := HandleExponent;
    // 123.45
    end else if CharInSet(temp, ['0'..'9']) then begin
      Result := True;
    // 123.45j
    end else if CharInSet(temp, ['j', 'J']) then begin
      Inc (Run);
      Result := False;
    // 123.45.45: Error!
    end else if temp = '.' then begin
      Result := False;
      if HandleDot then
        HandleBadNumber;
    // Error!
    end else if IsIdentChar(temp) then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckDotFound

  function CheckFloatNeeded: Boolean;
  begin
    // 091.0e4
    if CharInSet(temp, ['e', 'E']) then begin
      Result := HandleExponent;
    // 0912345
    end else if CharInSet(temp, ['0'..'9']) then begin
      Result := True;
    // 09123.45
    end else if temp = '.' then begin
      Result := HandleDot or HandleBadNumber; // Bad octal
    // 09123.45j
    end else if CharInSet(temp, ['j', 'J']) then begin
      Inc (Run);
      Result := False;
    // End of number (error: Bad oct number) 0912345
    end else begin
      Result := HandleBadNumber;
    end;
  end; // CheckFloatNeeded

  function CheckHex: Boolean;
  begin
    // 0x123ABC
    if CharInSet(temp, ['a'..'f', 'A'..'F', '0'..'9']) then
    begin
      Result := True;
    // 0x123ABCL
    end else if CharInSet(temp, ['l', 'L']) then begin
      Inc (Run);
      Result := False;
    // 0x123.45: Error!
    end else if temp = '.' then begin
      Result := False;
      if HandleDot then
        HandleBadNumber;
    // Error!
    end else if IsIdentChar(temp) then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckHex

  function CheckOct: Boolean;
  begin
    // 012345
    if CharInSet(temp, ['0'..'9']) then begin
      if not CharInSet(temp, ['0'..'7']) then begin
        State := nsFloatNeeded;
        fTokenID := tkFloat;
      end; // if
      Result := True;
    // 012345L
    end else if CharInSet(temp, ['l', 'L']) then begin
      Inc (Run);
      Result := False;
    // 0123e4
    end else if CharInSet(temp, ['e', 'E']) then begin
      Result := HandleExponent;
    // 0123j
    end else if CharInSet(temp, ['j', 'J']) then begin
      Inc (Run);
      fTokenID := tkFloat;
      Result := False;
    // 0123.45
    end else if temp = '.' then begin
      Result := HandleDot;
    // Error!
    end else if IsIdentChar(temp) then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckOct

  function CheckExpFound: Boolean;
  begin
    // 1e+123
    if CharInSet(temp, ['0'..'9']) then begin
      Result := True;
    // 1e+123j
    end else if CharInSet(temp, ['j', 'J']) then begin
      Inc (Run);
      Result := False;
    // 1e4.5: Error!
    end else if temp = '.' then begin
      Result := False;
      if HandleDot then
        HandleBadNumber;
    // Error!
    end else if IsIdentChar(temp) then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckExpFound

begin
  State := nsStart;
  fTokenID := tkNumber;

  temp := FLine[Run];
  Inc (Run);

  // Special cases
  if not CheckSpecialCases then
    Exit;

  // Use a state machine to parse numbers
  while True do begin
    temp := FLine[Run];

    case State of
      nsStart:
        if not CheckStart then Exit;
      nsDotFound:
        if not CheckDotFound then Exit;
      nsFloatNeeded:
        if not CheckFloatNeeded then Exit;
      nsHex:
        if not CheckHex then Exit;
      nsOct:
        if not CheckOct then Exit;
      nsExpFound:
        if not CheckExpFound then Exit;
    end; // case

    Inc (Run);
  end; // while
end;

procedure TSynRSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynRSyn.String2Proc;
var
  fBackslashCount: Integer;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = '"') and (FLine[Run + 2] = '"') then
  begin
    fTokenID := tkTrippleQuotedString;
    inc(Run, 3);

    fRange := rsMultilineString2;
    while fLine[Run] <> #0 do
    begin
      case fLine[Run] of

        '\':begin
               { If we're looking at a backslash, and the following character is an
               end quote, and it's preceeded by an odd number of backslashes, then
               it shouldn't mark the end of the string.  If it's preceeded by an
               even number, then it should. !!!THIS RULE DOESNT APPLY IN RAW STRINGS}
               if FLine[Run + 1] = '"' then
                 begin
                   fBackslashCount := 1;

                   while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
                     fBackslashCount := fBackslashCount + 1;

                   if (fBackslashCount mod 2 = 1) then inc(Run)
               end;
               inc(Run);
            end;// '\':

        '"':
          if (fLine[Run + 1] = '"') and (fLine[Run + 2] = '"') then begin
            fRange := rsUnKnown;
            inc(Run, 3);
            exit;
          end else
            inc(Run);
        #10: exit;
        #13: exit;
        else
          inc(Run);
      end;
    end;
  end
      else //if short string
  repeat
    case FLine[Run] of
      #0, #10, #13:
        begin
          if FLine[Run-1] = '\' then
          begin
            fStringStarter := '"';
            fRange := rsMultilineString3;
          end;
          Break;
        end;
      {The same backslash stuff above...}
      '\':begin
             if FLine[Run + 1] = '"' then
               begin
                 fBackslashCount := 1;

                 while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
                   fBackslashCount := fBackslashCount + 1;

                 if (fBackslashCount mod 2 = 1) then inc(Run)
             end;
             inc(Run);
          end;// '\':

      else inc(Run);
    end; //case
  until (FLine[Run] = '"');
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynRSyn.PreStringProc;
var
  temp: WideChar;
begin
  // Handle R raw strings
  // r""
  temp := FLine[Run + 1];
  if temp = '''' then
  begin
    Inc (Run);
    StringProc;
  end
  else if temp = '"' then
  begin
    Inc (Run);
    String2Proc;
  end
  else
  begin
    // If not followed by quote char, must be ident
    IdentProc;
  end; // if
end;

procedure TSynRSyn.StringProc;
var
  fBackslashCount: Integer;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then begin
    fTokenID := tkTrippleQuotedString;
    inc(Run, 3);

    fRange:=rsMultilineString;
    while fLine[Run] <> #0 do begin
      case fLine[Run] of

        '\': begin
             { If we're looking at a backslash, and the following character is an
             end quote, and it's preceeded by an odd number of backslashes, then
             it shouldn't mark the end of the string.  If it's preceeded by an
             even number, then it should. !!!THIS RULE DOESNT APPLY IN RAW STRINGS}
              if FLine[Run + 1] = #39 then
                begin
                  fBackslashCount := 1;

                  while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
                    fBackslashCount := fBackslashCount + 1;

                  if (fBackslashCount mod 2 = 1) then inc(Run)
              end;
              inc(Run);
            end;// '\':

        #39:
          if (fLine[Run + 1] = #39) and (fLine[Run + 2] = #39) then begin
            fRange := rsUnKnown;
            inc(Run, 3);
            EXIT;
          end else
            inc(Run);
        #10: EXIT;
        #13: EXIT;
        else
          inc(Run);
      end;
    end;
  end
      else //if short string
  repeat
    case FLine[Run] of
      #0, #10, #13 : begin
        if FLine[Run-1] = '\' then begin
          fStringStarter := #39;
          fRange := rsMultilineString3;
        end;
        BREAK;
        end;

      {The same backslash stuff above...}
      '\':begin
             if FLine[Run + 1] = #39 then
               begin
                 fBackslashCount := 1;

                 while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
                   fBackslashCount := fBackslashCount + 1;

                 if (fBackslashCount mod 2 = 1) then inc(Run)
             end;
             inc(Run);
          end;// '\':

      else inc(Run);
    end; //case
  until (FLine[Run] = #39);
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynRSyn.StringEndProc(EndChar: WideChar);
var
  fBackslashCount: Integer;
begin
  if fRange = rsMultilineString3 then
    fTokenID := tkString
  else
    fTokenID := tkTrippleQuotedString;

  case FLine[Run] of
    #0:
      begin
        NullProc;
        EXIT;
      end;
    #10:
      begin
        LFProc;
        EXIT;
    end;
    #13:
      begin
        CRProc;
        EXIT;
      end;
  end;

  if fRange = rsMultilineString3 then begin
    repeat
      if FLine[Run]=fStringStarter then begin
        inc(Run);
        fRange:=rsUnknown;
        EXIT;
      end else if FLine[Run]='\' then ;  {The same backslash stuff above...}
          begin
             if FLine[Run + 1] = fStringStarter then
               begin
                 fBackslashCount := 1;

                 while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
                   fBackslashCount := fBackslashCount + 1;

                 if (fBackslashCount mod 2 = 1) then inc(Run);
             end;
           end;// if FLine[Run]...

      inc(Run);
    until IsLineEnd(Run);
    if FLine[Run-1]<>'\' then begin
      fRange:=rsUnknown;
      EXIT;
    end;
  end else
  repeat
    if FLine[Run] = '\' then
    begin
       if FLine[Run + 1] = EndChar then
         begin
           fBackslashCount := 1;

           while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
             fBackslashCount := fBackslashCount + 1;

           if (fBackslashCount mod 2 = 1) then inc(Run, 2);
       end;
     end;// if FLine[Run]...
    if (FLine[Run]=EndChar) and (FLine[Run+1]=EndChar) and (FLine[Run+2]=EndChar) then begin
      inc(Run,3);
      fRange:=rsUnknown;
      EXIT;
    end;
    inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynRSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynRSyn.Next;
begin
  fTokenPos := Run;

  case fRange of
    rsMultilineString:
      StringEndProc(#39);
    rsMultilineString2:
      StringEndProc('"');
    rsMultilineString3:
      StringEndProc(fStringStarter);
    else
      case fLine[Run] of
        '&', '}', '{', ':', ',', ']', '[', '*', '`',
        '^', ')', '(', ';', '/', '=', '-', '+', '!', '\',
        '%', '|', '~':
          SymbolProc;
        #13: CRProc;
        '#': CommentProc;
        '>': GreaterProc;
        'A'..'Z', 'a'..'z', '_','.': IdentProc;
        #10: LFProc;
        '<': LowerProc;
        #0: NullProc;
        '0'..'9': NumberProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
//        't', 'R': PreStringProc;
//        'u', 'U': StringProc;
        '''': StringProc;
        '"': String2Proc;
        else UnknownProc;
      end;
  end;

end;

function TSynRSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynRSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
//  Result := Run = fStringLen + 1;
end;

procedure TSynRSyn.LoadExternalKeyWords(ExternalKeyFileName:String;KeyType:TtkTokenKind;AddWords:boolean);
  var f:integer;L:TstringList;
  s:string;
    GlobalKeywords: TStringList;

begin
//    if assigned (GlobalKeywords) then GlobalKeywords.Free;
    GlobalKeywords := TStringList.Create;
    GlobalKeywords.CaseSensitive:=true;
    GlobalKeywords.Sorted:=true;
    L:=TstringList.create;
    s:=format('%s',[ExternalKeyFileName]);
    if FileExists(s) then begin
                    L.LoadFromFile(s);
                    for f := 1 to L.COUNT do GlobalKeywords.AddObject(L[f-1], TObject(UIntPtr(Ord(KeyType))));
                    end;
    L.free;
    if  not  AddWords then FKeywords.Assign (GlobalKeywords)
        else FKeywords.AddStrings(GlobalKeywords);
  GlobalKeywords.free;
  GlobalKeywords:=nil;
end;

procedure TSynRSyn.SetLine(const NewValue: string; LineNumber: Integer);
begin
  inherited;
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

function TSynRSyn.GetRange: Pointer;
begin
  Result := Pointer(ord(fRange));
end;

function TSynRSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynRSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynRSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNonKeyword: Result := fNonKeyAttri;
    tkModifier: Result := fModifierAttri;
    tkSystemDefined: Result := fSystemAttri;
    tkNumber: Result := fNumberAttri;
    tkHex: Result := fHexAttri;
    tkOct: Result := fOctalAttri;
    tkFloat: Result := fFloatAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkTrippleQuotedString: Result := fDocStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fErrorAttri;
  else
    Result := nil;
  end;
end;

function TSynRSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynRSyn.GetToken: string;
var
  Len: LongInt;
begin
  Result := '';
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

procedure TSynRSyn.GetTokenEx(out TokenStart: PChar;
  out TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  TokenStart:=FLine + fTokenPos;
end;

procedure TSynRSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynRSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(UIntPtr(Value));
end;

function TSynRSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterR;
end;

function TSynRSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', '.','0'..'9', 'A'..'Z', 'a'..'z':
      Result := True;
    else
      Result := False;
  end;
end;

class function TSynRSyn.GetLanguageName: string;
begin
  Result := 'R';
end;

function TSynRSyn.GetSampleSource: String;
begin
  result :=
    '''R sample'''#13#10                                     +
    '#!Notes and observations'#13#10                         +
    '[]{}()                           #Brackets'#13#10       +
    'pA = 2i                          #Complex number'#13#10 +
    'pB = 3.5E2                       #Float number'#13#10   +
    '0 1 2 3 4 5 6 8 9                #Numbers'#13#10        +
    'variable_a = 1:100               #Identifier'#13#10     +
    '+ - * / ^ = <- -> <> ~ $ ? ! & : #Operators'#13#10      +
    '() {} [] ; ,                     #Symbol'#13#10         +
    'NA, NULL, TRUE, FALSE, if        #Reserved_1'#13#10     +
    'mean(Y); var(Y); sd(Y)           #Reserved_2'#13#10     +
    'plot(Y ~ X, ylab = ''Y'', xlab = ''X'','#13#10          +
    '     col = ''blue'', cex = 0.5)    #Reserved_3'#13#10   +
    'cat(''\nI - Basics measures:''); cat(''\n\n'')'#13#10   +
    'string = "This is" ''a string'''#13#10;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynRSyn);
{$ENDIF}
finalization
 // GlobalKeywords.Free;
end.
