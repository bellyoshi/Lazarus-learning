unit Unit2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

type

  { TForm2 }

  TForm2 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private

  public

  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
      // 例: 線を描く
  Form2.Canvas.Pen.Color := clRed;
  Form2.Canvas.MoveTo(10, 10);  // 開始座標
  Form2.Canvas.LineTo(100, 100); // 終了座標

  // 例: 円を描く
  Form2.Canvas.Brush.Color := clBlue;
  Form2.Canvas.Ellipse(50, 50, 150, 150);
end;

procedure TForm2.FormPaint(Sender: TObject);
begin
  // 例: 線を描く
  Canvas.Pen.Color := clRed;
  Canvas.MoveTo(10, 10);  // 開始座標
  Canvas.LineTo(100, 100); // 終了座標

  // 例: 円を描く
  Canvas.Brush.Color := clBlue;
  Canvas.Ellipse(50, 50, 150, 150);
end;

end.

