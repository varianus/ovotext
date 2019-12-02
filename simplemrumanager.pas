{ MRU (Most Recent Used) menu item manager
  Stripped down version of mrumanager (included in package lazmrumenu) by Marco Caselli

  Copyright (C) 2011 Michael Van Canneyt (michael@freepascal.org)
                     Modifications by Werner Pamler

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
{$I codegen.inc}
unit simplemrumanager;
interface

uses
  Classes, SysUtils, menus;

Type
  { TRecentMenuItem }

  TRecentMenuItem = Class(TMenuItem)
  Private
    FData: TObject;
    FFileName : string;
  Public
    Property FileName : String Read FFileName;
    property Data: TObject read FData;
  end;
  TRecentMenuItemClass = Class of TRecentMenuItem;

  { TMRUMenuManager }

  TOnRecentFileEvent = Procedure(Sender : TObject; Const AFileName : String; const AData: TObject) of object;

  TMRUMenuManager = Class(TComponent)
  Private
    FOnRecent: TOnRecentFileEvent;
    FRecent : TStrings;
    FMaxRecent : Integer;
    FMenuCaptionMask : string;
    FMIRecent : TMenuItem;
    FPMRecent : TPopupMenu;
    FMaxItemLength : integer;
    procedure SetMaxItemLength(const AValue:integer);
    procedure SetMenuCaptionMask(const AValue:string);
    procedure SetMIRecent(const AValue: TMenuItem);
    procedure SetPMRecent(const AValue: TPopupMenu);
    procedure SetRecent(const AValue: TStrings);
  protected
    // Overrides.
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // Called when menu item is clicked.
    procedure DoOnRecentClick(Sender: TObject); virtual;
    // Override this if you want to create a custom class of menu itel.
    function CreateMenuItem(AOwner: TComponent): TRecentMenuItem; virtual;
    // Create a menu caption. Default is index followed by filename.
    // Override if you want to customize.
    Function CreateMenuCaption(AIndex : Integer; Const AFileName : String) : String; virtual;
  Public
    Constructor Create(AOwner : TComponent);override;
    Destructor Destroy; override;
    // Add a filename to the list of files.
    // If an existing file is added, it is moved first in the list.
    // If MaxRecent is attained, the last one is removed.
    // Calls ShowRecentFiles.
    procedure AddToRecent(AFileName: String);
    // Re-populate the menu.
    procedure ShowRecentFiles;
  Published
    // Max. items to be kept in the list.
    Property MaxRecent : Integer Read FMaxRecent write FMaxRecent default 10;
    // Menu item to create a submenu under. Existing items will be removed.
    Property MenuItem : TMenuItem Read FMIRecent Write SetMIRecent;
    // Popupmenu attached to a toolbar button. Existing items will be removed.
    Property PopupMenu : TPopupMenu Read FPMRecent Write SetPMRecent;
    // Maximum length of recent menu item
    Property MaxItemLength : integer Read FMaxItemLength Write SetMaxItemLength default 80;
    // Format mask for MenuCaption: first placeholder must be %d, second %s, e.g. '%d - %s'
    Property MenuCaptionMask : string read FMenuCaptionMask Write SetMenuCaptionMask;
    // Recent items. If adding manually to the list, ShowRecentFiles must be called manually.
    Property Recent : TStrings Read FRecent Write SetRecent;
    // Called when the user clicks an recent meu item.
    Property OnRecentFile : TOnRecentFileEvent Read FOnRecent Write FOnRecent;
  end;
  EMRUManager = Class(Exception);

Const
  KeyMaxRecent    = 'MaxRecent';
  KeyCount        = 'Count';
  KeyFile         = 'File%d';

implementation

const
  DEFAULT_MASK = '%0:d.  %1:s';

function MinimizeFileName(const AFileName:string; AMaxLen:integer) : string;

  procedure SplitPath(const APath:String; Parts: TStrings);
  { Splits the provided path into constituent folder names }
  var
    i, j : Integer;
  begin
    if APath = '' then exit;
    if not Assigned(Parts) then exit;

    i := Length(APath);
    j := i;
    while (i >= 1) do begin
      if APath[i] = DirectorySeparator then begin
        Parts.Insert(0, copy(APath, i+1, j-i));
        j := i;
      end;
      dec(i);
    end;
    Parts.Insert(0, copy(APath, 1, j));
  end;

  function AddStringsFromTo(AList:TStrings; FromIndex,ToIndex:integer) : string;
  var
    i : integer;
  begin
    result := '';
    for i:=FromIndex to ToIndex do
      result := result + AList[i];
  end;

var
  Parts : TStringList;
  i : integer;
  tmp : string;
begin
  result := AFileName;
  if Length(AFileName) > AMaxLen then begin
    Parts := TStringList.Create;
    try
      SplitPath(AFileName, Parts);
      i := Parts.Count div 2;
      while (i < Parts.Count) do begin
        tmp := Format('%s...%s%s', [
          AddStringsFromTo(Parts, 0, i-1),
          DirectorySeparator,
          AddStringsFromTo(Parts, i+1, Parts.Count-1)
        ]);
        if Length(tmp) < AMaxLen then begin
          result := tmp;
          exit;
        end else
          Parts.Delete(i);
        i := Parts.Count div 2;
      end;
      result := ExtractFileName(AFileName);
    finally
      Parts.Free;
    end;
  end;
end;

{ TRecentMenuItem }

procedure TMRUMenuManager.AddToRecent(AFileName : String);
Var
  J : Integer;

begin
  AFileName:=ExpandFileName(AFileName);
  With FRecent do
    begin
    J:=IndexOf(AFileName);
    If (J<>-1) then
      begin
      if (J>0) then
        Exchange(0,J)
      end
    else
      begin
      While (Count>=FMaxRecent) do
        Delete(Count-1);
      Insert(0,AFileName)
      end;
    end;
  ShowRecentFiles;
end;

function TMRUMenuManager.CreateMenuItem(AOwner :TComponent) : TRecentMenuItem;

begin
  Result:=TRecentMenuItem.Create(AOwner);
end;

function TMRUMenuManager.CreateMenuCaption(AIndex: Integer;
  const AFileName: String): String;
var
  fn : string;
  mask : string;
begin
  if FMaxItemLength > 0 then
    fn := MinimizeFileName(AFileName, FMaxItemLength)
  else
    fn := AFileName;
  if FMenuCaptionMask = '' then
    mask := DEFAULT_MASK
  else
    mask := FMenuCaptionMask;
  Result:=Format(mask, [AIndex+1,fn]);
end;

procedure TMRUMenuManager.ShowRecentFiles;

Var
  I : Integer;
  M : TRecentMenuItem;

begin
  if Assigned(FMIRecent) then begin
    FMIRecent.clear;
    For I:=0 to FRecent.Count-1 do
    begin
      M:=CreateMenuItem(Self.Owner);
      M.Caption:=CreateMenuCaption(I,FRecent[i]);
      M.FFileName:=FRecent[i];
      m.FData := FRecent.Objects[i];
      M.OnClick:=@DoOnRecentClick;
      FMIRecent.Add(M);
    end;
  end;
  if Assigned(FPMRecent) then begin
    FPMRecent.Items.Clear;
    for i:=0 to FRecent.Count-1 do
    begin
      M := CreateMenuItem(Self.Owner);
      M.Caption := CreateMenuCaption(I, Recent[i]);
      M.FFileName := FRecent[i];
      M.FData := FRecent.Objects[i];
      M.OnClick := @DoOnRecentClick;
      FPMRecent.Items.Add(M);
    end;
  end;
end;

procedure TMRUMenuManager.SetMaxItemLength(const AValue:integer);
begin
  if FMaxItemLength <> AValue then begin
    FMaxItemLength := AValue;
    ShowRecentFiles;
  end;
end;

procedure TMRUMenuManager.SetMenuCaptionMask(const AValue:string);
begin
  if FMenuCaptionMask <> AValue then begin
    FMenuCaptionMask := AValue;
    ShowRecentFiles;
  end;
end;

procedure TMRUMenuManager.SetMIRecent(const AValue: TMenuItem);
begin
  if FMIRecent=AValue then exit;
  If Assigned(FMIRecent) then
    FMIRecent.RemoveFreeNotification(Self);
  FMIRecent:=AValue;
  If Assigned(FMIRecent) then
    FMIRecent.FreeNotification(Self);
  ShowRecentFiles;
end;

procedure TMRUMenuManager.SetPMRecent(const AValue: TPopupMenu);
begin
  if FPMRecent=AValue then exit;
  if Assigned(FPMRecent) then
    FPMRecent.RemoveFreeNotification(self);
  FPMRecent := AValue;
  if Assigned(FPMRecent) then
    FPMRecent.FreeNotification(self);
  ShowRecentFiles;
end;

procedure TMRUMenuManager.SetRecent(const AValue: TStrings);
begin
  if FRecent=AValue then exit;
  FRecent.Assign(AValue);
  ShowRecentFiles;
end;

constructor TMRUMenuManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRecent:=TStringList.Create;
  FMaxRecent := 10;
  FMaxItemLength := 80;
  FMenuCaptionMask := DEFAULT_MASK;
end;

destructor TMRUMenuManager.Destroy;
begin
  FreeAndNil(FRecent);
  inherited Destroy;
end;

procedure TMRUMenuManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then begin
    if AComponent = FMIRecent then FMIRecent := nil;
    if AComponent = FPMRecent then FPMRecent := nil;
  end;
  { original code - I think this is not correct:
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and ((AComponent=FMIRecent) or (AComponent=FPMRecent)) then
     exit;
  }
end;

procedure TMRUMenuManager.DoOnRecentClick(Sender: TObject);
var
  FN: string;
begin
  With (Sender as TRecentMenuItem) do
    FN:=FileName;

  if (FN<>'') and (OnRecentFile<>Nil) then
    OnRecentFile(Self,FN,(Sender as TRecentMenuItem).Data);
end;

end.

