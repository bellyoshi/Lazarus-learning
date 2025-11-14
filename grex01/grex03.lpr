program grex03;
{$mode fpc}{$H+} // TurboPASCAL互換モードの指定

uses
  Graph, Keyboard; // キーボード入力イベントのユニット追加

var
  K: TKeyEvent; // キーイベント変数
  gd, gm, y: integer;

begin
  InitKeyBoard; // キーボードの初期化
  y := 30;
  gd := D8bit;
  gm := m640x480; // 640x480サイズ指定
  initgraph(gd, gm, ''); // 初期化
  setColor(cyan); // シアン色指定
  
  // タイトル表示
  outtextxy(50, 1, 'Press keys, press "q" to end.');
  
  Repeat
    K := GetKeyEvent; // キーイベントの取得
    K := TranslateKeyEvent(K); // 変換
    Outtextxy(1, 1, 'Get key event with ');
    
    Case GetKeyEventFlags(K) of
      kbASCII: outtextxy(1, 20, 'ASCII key');
      kbUniCode: outtextxy(1, 20, 'Unicode key');
      kbFnKey: outtextxy(1, 20, 'Function key');
      kbPhys: outtextxy(1, 20, 'Physical key');
      kbReleased: outtextxy(1, 20, 'Released key event');
    end;
    
    // 文字列へ変換して表示
    setColor(white); // 白色指定
    outtextxy(100, y, KeyEventToString(K));
    y := y + 10;
  Until (GetKeyEventChar(K) = 'q'); // qキー入力で終了
  
  Closegraph;
  DoneKeyBoard;
end.
