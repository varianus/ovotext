unit Config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, inifiles, XMLPropStorage;


type
  { TConfig }
  RAppSettings= record
    CloseWithLastTab : boolean;
  end;

  TConfig = class
  private
    FAppSettings: RAppSettings;
    FConfigFile:    string;
    fConfigDir:    string;
    FFont: TFont;
    ResourcesPath: string;
    fConfigHolder:     TXMLConfigStorage;
    function ReadColor(const Section, Ident: string; const Default: TColor): TColor;
    procedure SetFont(AValue: TFont);
    procedure WriteColor(const Section, Ident: string; const Value: TColor);
  public
    constructor Create;
    procedure ReadConfig;
    procedure SaveConfig;
    procedure WriteStrings(Section: string; Name: string; Values: TStrings);
    function ReadStrings(Section: string; Name: string; Values: TStrings): integer;
    function GetResourcesPath: string;
    procedure Flush;
    destructor Destroy; override;
    // -- //
    Property ConfigDir: string read fConfigDir;
    property ConfigFile: string read FConfigFile;
    Property Font : TFont read FFont write SetFont;
    property AppSettings: RAppSettings read FAppSettings write FAppSettings;

  end;

function ConfigObj: TConfig;

implementation

{ TConfig }
uses
  Fileutil, lclproc,
// only for default font !
  Synedit
{$ifdef Darwin}
  , MacOSAll
{$endif}  ;

var
  FConfigObj : TConfig;

const
  SectionUnix    = 'UNIX';
  IdentResourcesPath = 'ResourcesPath';
  ResourceSubDirectory     = 'Resources';

const
 {$ifdef UNIX}
  DefaultDirectory = '/usr/share/ovotext/';
  {$DEFINE NEEDCFGSUBDIR}
 {$endif}

 {$ifdef DARWIN}
  BundleResourcesDirectory = '/Contents/Resources/';
 {$endif}

  SectionGeneral = 'General';

function GetConfigDir: string;
var
  Path: string;
begin
  Path := GetAppConfigDirUTF8(False);
  ForceDirectoriesUTF8(Path);
  Result := IncludeTrailingPathDelimiter(Path);

end;

function ConfigObj: TConfig;
begin
  if not Assigned(FConfigObj) then
    FConfigObj := TConfig.Create;
  result := FConfigObj;
end;

constructor TConfig.Create;
begin
  FFont := Tfont.Create;
  FConfigFile := GetAppConfigFile(False {$ifdef NEEDCFGSUBDIR} , true{$ENDIF} );
  fConfigDir :=  GetConfigDir;
  fConfigHolder  := TXMLConfigStorage.Create(FConfigFile, FileExistsUTF8(FConfigFile));
  ReadConfig;
end;

destructor TConfig.Destroy;
begin
  SaveConfig;
  fConfigHolder.WriteToDisk;
  fConfigHolder.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TConfig.SaveConfig;
begin
  fConfigHolder.SetValue(SectionUnix+'/'+ IdentResourcesPath, ResourcesPath);
  fConfigHolder.SetValue('Application/CloseWithLastTab',FAppSettings.CloseWithLastTab);

  fConfigHolder.SetValue('Editor/Font/Name',FFont.name);
  fConfigHolder.SetValue('Editor/Font/Height',FFont.Height);
end;

procedure TConfig.ReadConfig;
var
  fontName:String;
begin

{$ifdef WINDOWS}
  ResourcesPath := fConfigHolder.GetValue(SectionUnix+'/'+IdentResourcesPath, ExtractFilePath(ExtractFilePath(ParamStr(0))));
{$else}
  {$ifndef DARWIN}
  ResourcesPath := fIniFiles.ReadString(SectionUnix, IdentResourcesPath, DefaultDirectory);
  {$endif}
{$endif}
  FAppSettings.CloseWithLastTab:=fConfigHolder.GetValue('Application/CloseWithLastTab',false);

  fontName := fConfigHolder.GetValue('Editor/Font/Name',EmptyStr);
  if fontName = EmptyStr then
    begin
      FFont.Name:=SynDefaultFontName;
      FFont.Height:=SynDefaultFontHeight;
    end
  else
    begin
       FFont.Name:= fontName;
       FFont.Height := fConfigHolder.GetValue('Editor/Font/Height',0);
    end;

end;

procedure TConfig.WriteStrings(Section: string; Name: string; Values: TStrings);
begin
  fConfigHolder.SetValue(Section+'/'+Name,Values);
end;

function TConfig.ReadStrings(Section: string; Name: string; Values: TStrings): integer;
begin
  fConfigHolder.GetValue(Section+'/'+Name, Values);
  result := Values.Count;
end;

function TConfig.ReadColor(const Section, Ident: string; const Default: TColor): TColor;
var
  tmpString: string;
begin
  tmpString := fConfigHolder.GetValue(Section+'/'+Ident, IntToHex(Default, 8));
  if not TryStrToInt(tmpString, Result) then
    Result := Default;
end;

procedure TConfig.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TConfig.WriteColor(const Section, Ident: string; const Value: TColor);
begin
  fConfigHolder.setValue(Section+'/'+Ident, '$' + IntToHex(Value, 8));
end;

procedure TConfig.Flush;
begin
  fConfigHolder.WriteToDisk;
end;

function TConfig.GetResourcesPath: string;
{$ifdef DARWIN}
var
  pathRef:   CFURLRef;
  pathCFStr: CFStringRef;
  pathStr:   shortstring;
{$endif}
begin
{$ifdef UNIX}
{$ifdef DARWIN}
  pathRef   := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);

  Result := pathStr + BundleResourcesDirectory;
{$else}
  Result := ResourcesPath;
{$endif}
{$endif}

{$ifdef WINDOWS}
  Result := ExtractFilePath(ExtractFilePath(ParamStr(0))) + ResourceSubDirectory + PathDelim;
{$endif}

end;

initialization
  FConfigObj:=nil;

finalization
  if Assigned(FConfigObj) then
    begin
      FConfigObj.SaveConfig;
      FConfigObj.Free;
    end;

end.
