unit uEditorFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, forms, Controls, comctrls, uEditor, Graphics;

Type

   { TEditorFactory }

   TEditorFactory = class (Tobject)
    fEditors:TObjectList;
   public
     MasterForm:TForm;
     constructor Create;
     destructor Destroy;  override;
     function CreateTabSheet(AOwner: TPageControl): TfEditor;
   end;

   TEditorTabSheet = class (TTabSheet)
   end;
implementation

constructor TEditorFactory.Create;
begin
   fEditors := TObjectList.Create;
   fEditors.OwnsObjects:=true;
end;

destructor TEditorFactory.Destroy;
begin
   fEditors.Free;
   inherited Destroy;
end;

function TEditorFactory.CreateTabSheet(AOwner: TPageControl): TfEditor;
var
   Sheet: TTabSheet;
begin
   Sheet := TEditorTabSheet.Create(AOwner);
   Result:= TfEditor.Create(MasterForm);
   try
      Sheet.PageControl := AOwner;
      Sheet.ImageIndex := Sheet.TabIndex;
      Result := TfEditor.Create(Sheet);
      with Result do
        begin
         MasterForm := Self.MasterForm;
         BorderStyle := bsNone;
         Parent := Sheet;
         Align := alClient;
         Visible := TRUE;
         AOwner.ActivePage := Sheet;
         SetFocus;
        end;
      Sheet.Tag := PtrUInt(Result);
      Result.Realign;
      if Result <> NIL then
         fEditors.Add(Result);
   except
      Sheet.Free;
   end;
end;
end.
