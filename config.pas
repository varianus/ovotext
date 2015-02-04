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
    ResourcesPath: string;
    fConfigHolder:     TXMLConfigStorage;
    function ReadColor(const Section, Ident: string; const Default: TColor): TColor;
    procedure WriteColor(const Section, Ident: string; const Value: TColor);
  public
    constructor Create;
    procedure ReadConfig;
    procedure SaveConfig;
    procedure WriteStrings(Section: string; Name: string; Values: TStrings);
    function ReadStrings(Section: string; Name: string; Values: TStrings): integer;
    Procedure ReadCustomParams(const Section:string; Params:TStrings);
    procedure SaveCustomParams(const Section:string; Params:TStrings);
    procedure RemoveSection(const Section:string);
    function GetResourcesPath: string;
    procedure Flush;
    destructor Destroy; override;
    // -- //
    Property ConfigDir: string read fConfigDir;
    property ConfigFile: string read FConfigFile;
    property AppSettings: RAppSettings read FAppSettings write FAppSettings;

  end;

function ConfigObj: TConfig;

implementation

{ TConfig }
uses
  Fileutil, lclproc
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
  inherited Destroy;
end;

procedure TConfig.SaveConfig;
begin
  fConfigHolder.SetValue(SectionUnix+'/'+ IdentResourcesPath, ResourcesPath);
  fConfigHolder.SetValue('Application/CloseWithLastTab',FAppSettings.CloseWithLastTab);

end;



procedure TConfig.RemoveSection(const Section: string);
begin
//  fConfigHolder.EraseSection(Section);
end;
procedure TConfig.ReadCustomParams(const Section:string; Params: TStrings);
begin
  Params.Clear;
//  fConfigHolder.GetValue()); Object(Section, Params)
end;

procedure TConfig.SaveCustomParams(const Section:string; Params: TStrings);
var
  i :Integer;
begin
for i := 0 to Params.Count -1 do
  begin
//     fConfigHolder.WriteString(Section, Params.Names[i], Params.ValueFromIndex[i]);
  end;
end;

procedure TConfig.ReadConfig;
begin

{$ifdef WINDOWS}
  ResourcesPath := fConfigHolder.GetValue(SectionUnix+'/'+IdentResourcesPath, ExtractFilePath(ExtractFilePath(ParamStr(0))));
{$else}
  {$ifndef DARWIN}
  ResourcesPath := fConfigHolder.GetValue(SectionUnix+'/'+ IdentResourcesPath, DefaultDirectory);
  {$endif}
{$endif}
  FAppSettings.CloseWithLastTab:=fConfigHolder.GetValue('Application/CloseWithLastTab',false);

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
