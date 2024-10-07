unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LazHelpHTML,
  Unit2, fpvtocanvas;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);

begin

  Form2.BorderStyle := bsNone;     // ボーダーを非表示
  Form2.WindowState := wsMaximized; // 最大化
  Form2.Show;                      // フォームを表示

end;

procedure TForm1.Button2Click(Sender: TObject);
begin

    // 例: 線を描く
  Form2.Canvas.Pen.Color := clRed;
  Form2.Canvas.MoveTo(10, 10);  // 開始座標
  Form2.Canvas.LineTo(100, 100); // 終了座標

  // 例: 円を描く
  Form2.Canvas.Brush.Color := clBlue;
  Form2.Canvas.Ellipse(50, 50, 150, 150);
    // First check the in input
    //if not CheckInput() then Exit;



end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

end.

