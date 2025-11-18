unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    DiceValue: Integer; // サイコロの目の値（1-6）
    procedure DrawDice;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.DrawDice;
var
  DotSize: Integer;
  Spacing: Integer;
  CenterX, CenterY: Integer;
begin
  with PaintBox1.Canvas do
  begin
    // 背景を白に
    Brush.Color := clWhite;
    FillRect(0, 0, PaintBox1.Width, PaintBox1.Height);
    
    // 枠を描画
    Pen.Color := clBlack;
    Pen.Width := 2;
    Brush.Color := clWhite;
    Rectangle(10, 10, PaintBox1.Width - 10, PaintBox1.Height - 10);
    
    // サイコロの目の位置を計算
    DotSize := 20;
    Spacing := 60;
    CenterX := PaintBox1.Width div 2;
    CenterY := PaintBox1.Height div 2;
    
    // 点を描画
    Brush.Color := clBlack;
    Pen.Color := clBlack;
    
    case DiceValue of
      1: // 中央
        Ellipse(CenterX - DotSize div 2, CenterY - DotSize div 2,
                CenterX + DotSize div 2, CenterY + DotSize div 2);
      2: // 左上と右下
        begin
          Ellipse(CenterX - Spacing - DotSize div 2, CenterY - Spacing - DotSize div 2,
                  CenterX - Spacing + DotSize div 2, CenterY - Spacing + DotSize div 2);
          Ellipse(CenterX + Spacing - DotSize div 2, CenterY + Spacing - DotSize div 2,
                  CenterX + Spacing + DotSize div 2, CenterY + Spacing + DotSize div 2);
        end;
      3: // 左上、中央、右下
        begin
          Ellipse(CenterX - Spacing - DotSize div 2, CenterY - Spacing - DotSize div 2,
                  CenterX - Spacing + DotSize div 2, CenterY - Spacing + DotSize div 2);
          Ellipse(CenterX - DotSize div 2, CenterY - DotSize div 2,
                  CenterX + DotSize div 2, CenterY + DotSize div 2);
          Ellipse(CenterX + Spacing - DotSize div 2, CenterY + Spacing - DotSize div 2,
                  CenterX + Spacing + DotSize div 2, CenterY + Spacing + DotSize div 2);
        end;
      4: // 四隅
        begin
          Ellipse(CenterX - Spacing - DotSize div 2, CenterY - Spacing - DotSize div 2,
                  CenterX - Spacing + DotSize div 2, CenterY - Spacing + DotSize div 2);
          Ellipse(CenterX + Spacing - DotSize div 2, CenterY - Spacing - DotSize div 2,
                  CenterX + Spacing + DotSize div 2, CenterY - Spacing + DotSize div 2);
          Ellipse(CenterX - Spacing - DotSize div 2, CenterY + Spacing - DotSize div 2,
                  CenterX - Spacing + DotSize div 2, CenterY + Spacing + DotSize div 2);
          Ellipse(CenterX + Spacing - DotSize div 2, CenterY + Spacing - DotSize div 2,
                  CenterX + Spacing + DotSize div 2, CenterY + Spacing + DotSize div 2);
        end;
      5: // 四隅と中央
        begin
          Ellipse(CenterX - Spacing - DotSize div 2, CenterY - Spacing - DotSize div 2,
                  CenterX - Spacing + DotSize div 2, CenterY - Spacing + DotSize div 2);
          Ellipse(CenterX + Spacing - DotSize div 2, CenterY - Spacing - DotSize div 2,
                  CenterX + Spacing + DotSize div 2, CenterY - Spacing + DotSize div 2);
          Ellipse(CenterX - DotSize div 2, CenterY - DotSize div 2,
                  CenterX + DotSize div 2, CenterY + DotSize div 2);
          Ellipse(CenterX - Spacing - DotSize div 2, CenterY + Spacing - DotSize div 2,
                  CenterX - Spacing + DotSize div 2, CenterY + Spacing + DotSize div 2);
          Ellipse(CenterX + Spacing - DotSize div 2, CenterY + Spacing - DotSize div 2,
                  CenterX + Spacing + DotSize div 2, CenterY + Spacing + DotSize div 2);
        end;
      6: // 左右に3つずつ
        begin
          Ellipse(CenterX - Spacing - DotSize div 2, CenterY - Spacing - DotSize div 2,
                  CenterX - Spacing + DotSize div 2, CenterY - Spacing + DotSize div 2);
          Ellipse(CenterX - Spacing - DotSize div 2, CenterY - DotSize div 2,
                  CenterX - Spacing + DotSize div 2, CenterY + DotSize div 2);
          Ellipse(CenterX - Spacing - DotSize div 2, CenterY + Spacing - DotSize div 2,
                  CenterX - Spacing + DotSize div 2, CenterY + Spacing + DotSize div 2);
          Ellipse(CenterX + Spacing - DotSize div 2, CenterY - Spacing - DotSize div 2,
                  CenterX + Spacing + DotSize div 2, CenterY - Spacing + DotSize div 2);
          Ellipse(CenterX + Spacing - DotSize div 2, CenterY - DotSize div 2,
                  CenterX + Spacing + DotSize div 2, CenterY + DotSize div 2);
          Ellipse(CenterX + Spacing - DotSize div 2, CenterY + Spacing - DotSize div 2,
                  CenterX + Spacing + DotSize div 2, CenterY + Spacing + DotSize div 2);
        end;
    end;
  end;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  DrawDice;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // 初期表示で6の目を表示
  DiceValue := 6;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // ランダムなサイコロの目を生成（1-6）
  Randomize;
  DiceValue := Random(6) + 1;
  PaintBox1.Invalidate; // 再描画
end;

end.

