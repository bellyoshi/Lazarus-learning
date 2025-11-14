program grex04;
{$mode fpc}{$H+} // TurboPASCAL互換モードの指定

uses
  Crt, Graph;

var
  KeyChar: char;
  GraphDriver, GraphMode, TextY: integer;

begin
  TextY := 30;
  GraphDriver := D8bit;
  GraphMode := m640x480; // 640x480サイズ指定
  initgraph(GraphDriver, GraphMode, ''); // 初期化
  setColor(red); // 赤色指定
  
  // タイトル表示
  outtextxy(50, 1, 'Press keys, press "q" to end.');
  
  Repeat
    KeyChar := ReadKey; // キー入力の取得
    Outtextxy(1, 1, 'Get key event with ');
    
    // 文字表示
    setColor(white); // 白色指定
    outtextxy(100, TextY, KeyChar);
    TextY := TextY + 10;
  Until (KeyChar = 'q'); // qキー入力で終了
  
  Closegraph;
end.
