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
unit CmdLineParser;

interface

uses
  SysUtils, Classes, Generics.Collections;

type
  // Option definition for mapping short and long forms
  TOptionDef = record
    ShortForm: string;
    LongForm: string;
    Description: string;
    HasValue: boolean;
  end;

  // Class to handle Linux-style command line parameters

  { TCmdLineParser }

  TCmdLineParser = class
  private
  type tIntDictionary = specialize TDictionary<string, string>;
  private
    FOptions: TIntDictionary;
    FArguments: TStringList;
    FOptionDefs: specialize TArray<TOptionDef>;
    FParsed: boolean;

    procedure ParseParameters(Params: TStringList);
    function IsShortOption(const AParam: string): boolean;
    function IsLongOption(const AParam: string): boolean;
    function ExtractOptionName(const AParam: string): string;
    function ExtractOptionValue(const AParam: string): string;
    function FindOptionDef(const AOption: string): integer;
    function NormalizeOptionName(const AOption: string): string;

  public
    constructor Create;
    destructor Destroy; override;

    // Define option mappings (call this before Parse)
    procedure DefineOption(const AShort, ALong, ADescription: string; AHasValue: boolean = False);

    // Parse command line parameters
    procedure ParseParamStr;

    // Parse From an already available list
    procedure Parse(Params: TStringList);

    // Check if an option exists (works with both short and long forms)
    function HasOption(const AOption: string): boolean;

    // Get option value (returns empty string if not found)
    function GetOptionValue(const AOption: string): string;

    // Get option value with default
    function GetOptionValue(const AOption: string; const ADefault: string): string; overload;

    // Get all non-option arguments
    function GetArguments: TStringList;

    // Get argument by index
    function GetArgument(AIndex: integer): string;

    // Get number of arguments
    function ArgumentCount: integer;

    // Properties
    property Options: TIntDictionary read FOptions;
    property Arguments: TStringList read FArguments;
  end;

implementation

constructor TCmdLineParser.Create;
begin
  inherited Create;
  FOptions := TIntDictionary.Create;
  FArguments := TStringList.Create;
  FParsed := False;

  // Define some common options by default
  SetLength(FOptionDefs, 0);

end;

destructor TCmdLineParser.Destroy;
begin
  FOptions.Free;
  FArguments.Free;
  inherited Destroy;
end;

procedure TCmdLineParser.DefineOption(const AShort, ALong, ADescription: string; AHasValue: boolean = False);
var
  optDef: TOptionDef;
begin
  optDef.ShortForm := AShort;
  optDef.LongForm := ALong;
  optDef.Description := ADescription;
  optDef.HasValue := AHasValue;

  SetLength(FOptionDefs, Length(FOptionDefs) + 1);
  FOptionDefs[High(FOptionDefs)] := optDef;
end;

function TCmdLineParser.FindOptionDef(const AOption: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to High(FOptionDefs) do
    if (FOptionDefs[i].ShortForm = AOption) or (FOptionDefs[i].LongForm = AOption) then
    begin
      Result := i;
      Break;
    end;
end;

function TCmdLineParser.NormalizeOptionName(const AOption: string): string;
var
  idx: integer;
begin
  // Always return the long form as the normalized name
  idx := FindOptionDef(AOption);
  if idx >= 0 then
    Result := FOptionDefs[idx].LongForm
  else
    Result := AOption; // If not found in definitions, use as-is
end;

procedure TCmdLineParser.ParseParamStr;
var
  Params: TStringList;
  i: Integer;
begin
  if not FParsed then
  begin
    Params := TStringList.Create;
    try
      for i := 0 to ParamCount - 1 do
        Params.Add(ParamStr(i));

      ParseParameters(Params);
    finally
      Params.Free;
    end;
    FParsed := True;
  end;
end;

procedure TCmdLineParser.Parse(Params: TStringList);
begin
  ParseParameters(Params);
  FParsed := True;

end;

procedure TCmdLineParser.ParseParameters(Params: TStringList);
var
  i: integer;
  param, nextParam: string;
  optionName, optionValue: string;
begin
  FOptions.Clear;
  FArguments.Clear;

  i := 1;
  while i < Params.Count do
  begin
    param := Params[i];

    if IsShortOption(param) or IsLongOption(param) then
    begin
      optionName := NormalizeOptionName(ExtractOptionName(param));
      optionValue := ExtractOptionValue(param);

      // If no value in current parameter, check next parameter
      if (optionValue = '') and (i < Params.Count) then
      begin
        nextParam := Params[i + 1];
        // If next param is not an option, use it as value
        if not (IsShortOption(nextParam) or IsLongOption(nextParam)) then
        begin
          optionValue := nextParam;
          Inc(i); // Skip next parameter since we used it as value
        end;
      end;

      FOptions.AddOrSetValue(optionName, optionValue);
    end
    else
      FArguments.Add(param)// Regular argument
    ;

    Inc(i);
  end;
end;

function TCmdLineParser.IsShortOption(const AParam: string): boolean;
begin
  Result := (Length(AParam) >= 2) and (AParam[1] = '-') and (AParam[2] <> '-');
end;

function TCmdLineParser.IsLongOption(const AParam: string): boolean;
begin
  Result := (Length(AParam) >= 3) and (Copy(AParam, 1, 2) = '--');
end;

function TCmdLineParser.ExtractOptionName(const AParam: string): string;
var
  equalPos: integer;
begin
  if IsLongOption(AParam) then
    Result := Copy(AParam, 3, Length(AParam) - 2)// Remove --

  else if IsShortOption(AParam) then
    Result := Copy(AParam, 2, Length(AParam) - 1)// Remove -

  else
    Result := AParam;

  // Handle --option=value format
  equalPos := Pos('=', Result);
  if equalPos > 0 then
    Result := Copy(Result, 1, equalPos - 1);
end;

function TCmdLineParser.ExtractOptionValue(const AParam: string): string;
var
  equalPos: integer;
  optionPart: string;
begin
  Result := '';

  if IsLongOption(AParam) then
    optionPart := Copy(AParam, 3, Length(AParam) - 2)
  else if IsShortOption(AParam) then
    optionPart := Copy(AParam, 2, Length(AParam) - 1)
  else
    Exit;

  equalPos := Pos('=', optionPart);
  if equalPos > 0 then
    Result := Copy(optionPart, equalPos + 1, Length(optionPart) - equalPos);
end;

function TCmdLineParser.HasOption(const AOption: string): boolean;
var
  normalizedName: string;
begin
  ParseParamStr;
  normalizedName := NormalizeOptionName(AOption);
  Result := FOptions.ContainsKey(normalizedName);
end;

function TCmdLineParser.GetOptionValue(const AOption: string): string;
var
  normalizedName: string;
begin
  ParseParamStr;
  normalizedName := NormalizeOptionName(AOption);
  if FOptions.ContainsKey(normalizedName) then
    Result := FOptions[normalizedName]
  else
    Result := '';
end;

function TCmdLineParser.GetOptionValue(const AOption: string; const ADefault: string): string;
begin
  Result := GetOptionValue(AOption);
  if Result = '' then
    Result := ADefault;
end;

function TCmdLineParser.GetArguments: TStringList;
begin
  ParseParamStr;
  Result := FArguments;
end;

function TCmdLineParser.GetArgument(AIndex: integer): string;
begin
  ParseParamStr;
  if (AIndex >= 0) and (AIndex < FArguments.Count) then
    Result := FArguments[AIndex]
  else
    Result := '';
end;

function TCmdLineParser.ArgumentCount: integer;
begin
  ParseParamStr;
  Result := FArguments.Count;
end;


end.

