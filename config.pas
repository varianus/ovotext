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
unit Config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, XMLPropStorage;

type
  { TConfig }
  RAppSettings = record
    CloseWithLastTab: boolean;
  end;

  { RFontAttributes }

  TFontAttributes = record
    Foreground: TColor;
    Background: TColor;
    Styles: TFontStyles;
  end;

  TConfig = class
  private
    FAppSettings: RAppSettings;
    FConfigFile: string;
    fConfigDir: string;
    FFont: TFont;
    ResourcesPath: string;
    fConfigHolder: TXMLConfigStorage;
    fColorSchema: TXMLConfigStorage;
    function GetBackGroundColor: TColor;
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
    function ReadColor(const Section, Ident: string; const Default: TColor): TColor;
    function ReadFontStyle(const Section, Ident: string; const default: TFontStyles): TFontstyles;
    function ReadFontAttributes(AttibuteName: string; const Default: TFontAttributes): TFontAttributes;
    destructor Destroy; override;
    // -- //
    property ConfigDir: string read fConfigDir;
    property ConfigFile: string read FConfigFile;
    property Font: TFont read FFont write SetFont;
    property AppSettings: RAppSettings read FAppSettings write FAppSettings;
    property BackGroundColor: TColor read GetBackGroundColor;
  end;


function FontAttributes(const Foreground: TColor = clNone; BackGround: Tcolor = clNone; const Styles: TFontStyles = []): TFontAttributes; inline;

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

function GetConfigDir: string;
var
  Path: string;
begin
  Path := GetAppConfigDirUTF8(False);
  ForceDirectoriesUTF8(Path);
  Result := IncludeTrailingPathDelimiter(Path);

end;

function FontAttributes(const Foreground: TColor = clNone; BackGround: Tcolor = clNone; const Styles: TFontStyles = []): TFontAttributes; inline;
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
  fConfigDir := GetConfigDir;
  fConfigHolder := TXMLConfigStorage.Create(FConfigFile, FileExistsUTF8(FConfigFile));
  ReadConfig;

  fColorSchema := TXMLConfigStorage.Create(IncludeTrailingPathDelimiter(ResourcesPath) + 'color-schema.xml', True);

end;

destructor TConfig.Destroy;
begin
  SaveConfig;
  fConfigHolder.WriteToDisk;
  fConfigHolder.Free;
  fColorSchema.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TConfig.SaveConfig;
begin
  fConfigHolder.SetValue(SectionUnix + '/' + IdentResourcesPath, ResourcesPath);
  fConfigHolder.SetValue('Application/CloseWithLastTab', FAppSettings.CloseWithLastTab);

  fConfigHolder.SetValue('Editor/Font/Name', FFont.Name);
  fConfigHolder.SetValue('Editor/Font/Height', FFont.Height);
end;

procedure TConfig.ReadConfig;
var
  fontName: string;
begin

{$ifdef WINDOWS}
  ResourcesPath := fConfigHolder.GetValue(SectionUnix + '/' + IdentResourcesPath,
    ExtractFilePath(ExtractFilePath(ParamStr(0))));
{$else}
  {$ifndef DARWIN}
  ResourcesPath := fConfigHolder.GetValue(SectionUnix + '/' + IdentResourcesPath, DefaultDirectory);
  {$endif}
{$endif}
  FAppSettings.CloseWithLastTab :=
    fConfigHolder.GetValue('Application/CloseWithLastTab', False);

  fontName := fConfigHolder.GetValue('Editor/Font/Name', EmptyStr);
  if fontName = EmptyStr then
  begin
    FFont.Name := SynDefaultFontName;
    FFont.Height := SynDefaultFontHeight;
  end
  else
  begin
    FFont.Name := fontName;
    FFont.Height := fConfigHolder.GetValue('Editor/Font/Height', 0);
  end;

end;

procedure TConfig.WriteStrings(Section: string; Name: string; Values: TStrings);
begin
  fConfigHolder.SetValue(Section + '/' + Name, Values);
end;

function TConfig.ReadStrings(Section: string; Name: string; Values: TStrings): integer;
begin
  fConfigHolder.GetValue(Section + '/' + Name, Values);
  Result := Values.Count;
end;

function TConfig.GetBackGroundColor: TColor;
begin
  Result := ReadColor('Default/Text', 'Background', clWindow);
end;

procedure TConfig.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

function TConfig.ReadColor(const Section, Ident: string; const Default: TColor): TColor;
var
  tmpString: string;
begin
  try
    tmpString := fColorSchema.GetValue(Section + Ident, IntToHex(Default, 8));
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
    tmp := fColorSchema.GetValue(Section + Ident, '');
    Result := TFontStyles(StringToSet(PTypeInfo(TypeInfo(TFontstyles)), tmp));
  except
    Result := default;
  end;

end;

function TConfig.ReadFontAttributes(AttibuteName: string; const Default: TFontAttributes): TFontAttributes;
begin
  Result.Foreground := ReadColor('Schema/' + AttibuteName, 'Foreground', Default.Foreground);
  Result.Background := ReadColor('Schema/' + AttibuteName, 'Background', Default.Background);
  Result.Styles := ReadFontStyle('Schema/' + AttibuteName, 'Style', Default.Styles);

end;

procedure TConfig.WriteColor(const Section, Ident: string; const Value: TColor);
var
  tmp: string;
begin

  if not ColorToIdent(Value, tmp) then
    tmp := '$' + IntToHex(Value, 8);
  fConfigHolder.setValue(Section + '/' + Ident, tmp);
end;

procedure TConfig.Flush;
begin
  fConfigHolder.WriteToDisk;
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
  Result := ResourcesPath;
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
