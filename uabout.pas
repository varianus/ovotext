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
unit uabout;

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, LCLIntf;

type

  { TfAbout }

  TfAbout = class(TForm)
    bClose: TBitBtn;
    bLicense: TBitBtn;
    Image1: TImage;
    Label1: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lbBuildDate: TLabel;
    lbEngine: TLabel;
    lbFPCVersion: TLabel;
    lbLazVersion: TLabel;
    lHomePage: TLabel;
    lHomePage2: TLabel;
    lVersion: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure lHomePageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lHomePageMouseEnter(Sender: TObject);
    procedure lHomePageMouseLeave(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fAbout: TfAbout;

implementation
uses Stringcostants;
{$R *.lfm}

{ TfAbout }

procedure TfAbout.lHomePageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  OpenURL(TLabel(Sender).Caption);
end;

procedure TfAbout.FormShow(Sender: TObject);
var
  i:Integer;
begin
  lVersion.caption     := AppVersion;
  lbFPCVersion.Caption := fpcVersion;
  lbLazVersion.Caption := lazVersion;
  lbBuildDate.Caption  := BuildDate;
//  lbSVNRev.Caption     := ovoRevision;

  for i := 0 to ComponentCount -1 do
     if Components[i] is TLabel then
       if Tlabel (Components[i]).OptimalFill then
          Tlabel (Components[i]).AdjustFontForOptimalFill;
end;

procedure TfAbout.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=cafree;
end;

procedure TfAbout.lHomePageMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsUnderLine];
  TLabel(Sender).Font.Color := clRed;
  TLabel(Sender).Cursor := crHandPoint;
end;

procedure TfAbout.lHomePageMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [];
  TLabel(Sender).Font.Color := clBlue;
  TLabel(Sender).Cursor := crDefault;
end;

end.

