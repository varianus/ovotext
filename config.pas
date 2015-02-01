unit Config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, inifiles;


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
    fIniFiles:     TMemIniFile;
    function ReadColor(const Section, Ident: string; const Default: TColor): TColor;
    procedure WriteColor(const Section, Ident: string; const Value: TColor);
  public
    constructor Create;
    procedure ReadConfig;
    procedure SaveConfig;
    procedure WriteStrings(Section: string; BaseName: string; Values: TStrings);
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
  Fileutil
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
  fIniFiles  := TMemIniFile.Create(FConfigFile, False);
  ReadConfig;
end;

destructor TConfig.Destroy;
begin
  SaveConfig;
  fIniFiles.UpdateFile;
  fIniFiles.Free;
  inherited Destroy;
end;

procedure TConfig.SaveConfig;
begin
  fIniFiles.WriteString(SectionUnix, IdentResourcesPath, ResourcesPath);
  fIniFiles.WriteBool('Application','CloseWithLastTab',FAppSettings.CloseWithLastTab);

end;

procedure TConfig.ReadCustomParams(const Section:string; Params: TStrings);
begin
  Params.Clear;
  fIniFiles.ReadSectionValues(Section, Params)
end;

procedure TConfig.SaveCustomParams(const Section:string; Params: TStrings);
var
  i :Integer;
begin
for i := 0 to Params.Count -1 do
  begin
     fIniFiles.WriteString(Section, Params.Names[i], Params.ValueFromIndex[i]);
  end;
end;

procedure TConfig.RemoveSection(const Section: string);
begin
  fIniFiles.EraseSection(Section);
end;


procedure TConfig.ReadConfig;
begin

{$ifdef WINDOWS}
  ResourcesPath := fIniFiles.ReadString(SectionUnix, IdentResourcesPath, ExtractFilePath(
    ExtractFilePath(ParamStr(0))));
{$else}
  {$ifndef DARWIN}
  ResourcesPath := fIniFiles.ReadString(SectionUnix, IdentResourcesPath, DefaultDirectory);
  {$endif}
{$endif}

  FAppSettings.CloseWithLastTab:=fIniFiles.ReadBool('Application','CloseWithLastTab',false);

end;

procedure TConfig.WriteStrings(Section: string; BaseName: string;
  Values: TStrings);
var
  i: integer;
begin
  fIniFiles.EraseSection(Section);
  for i := 0 to Values.Count - 1 do
    begin
    fIniFiles.WriteString(Section, BaseName + IntToStr(i), Values[i]);
    end;
end;

function TConfig.ReadStrings(Section: string; Name: string; Values: TStrings): integer;
var
  strs: TStringList;
  i:    integer;
begin
  Values.Clear;
  Strs := TStringList.Create;
  fIniFiles.ReadSectionValues(Section, Strs);
  for i := 0 to strs.Count - 1 do
    Values.Add(strs.ValueFromIndex[i]);
  strs.Free;
  Result := Values.Count;

end;

function TConfig.ReadColor(const Section, Ident: string; const Default: TColor): TColor;
var
  tmpString: string;
begin
  tmpString := fIniFiles.ReadString(Section, Ident, IntToHex(Default, 8));
  if not TryStrToInt(tmpString, Result) then
    Result := Default;
end;

procedure TConfig.WriteColor(const Section, Ident: string; const Value: TColor);
begin
  fIniFiles.WriteString(Section, Ident, '$' + IntToHex(Value, 8));
end;

procedure TConfig.Flush;
begin
  fIniFiles.UpdateFile;
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
