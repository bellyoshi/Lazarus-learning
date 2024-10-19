unit Unit2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TForm2 }

  TForm2 = class(TForm)
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private

  public
    procedure SetBitmap(Bitmap: TBitmap);

  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}
procedure TForm2.FormCreate(Sender: TObject);
begin
  // Image1のStretchプロパティをTrueに設定して画像が伸縮するようにする
  Image1.Stretch := True;
end;

procedure TForm2.FormResize(Sender: TObject);
begin
  // フォームのクライアント領域にImage1をフィットさせる
  Image1.Top:= 0;
  Image1.Left:= 0;
  Image1.Width := ClientWidth;
  Image1.Height := ClientHeight;
end;

procedure TForm2.Image1Click(Sender: TObject);
begin

end;


procedure TForm2.SetBitmap(Bitmap : TBitmap);
  begin
  if Assigned(Bitmap) then
  begin
    Image1.Picture.Bitmap.Assign(Bitmap);
  end;
end;
end.

