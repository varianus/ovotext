unit uabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons;

type

  { TfAbout }

  TfAbout = class(TForm)
    BitBtn1: TBitBtn;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fAbout: TfAbout;

implementation

{$R *.lfm}

end.

