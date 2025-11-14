program grex01;
{$mode fpc}{$H+} // モード指定

uses
  SysUtils, Graph;

var
  GraphDriver, GraphMode: integer;

// 初期化処理
procedure InitializeGraphics;
begin
  GraphDriver := Detect;
  InitGraph(GraphDriver, GraphMode, '');
  ClearDevice; // 画面のクリア
end;

// 線を描く処理
procedure DrawYellowLine;
begin
  SetColor(Yellow); // 黄色
  SetLineStyle(SolidLn, 0, NormWidth);
  Line(250, 50, 100, 150);
end;

procedure DrawWhiteDottedLine;
begin
  SetColor(White); // 白色
  SetLineStyle(DottedLn, 0, NormWidth);
  MoveTo(100, 150);
  LineTo(250, 150);
end;

procedure DrawGreenThickLine;
begin
  SetColor(Green); // 緑色
  SetLineStyle(CenterLn, 0, ThickWidth);
  MoveRel(0, 0);
  LineRel(0, -100);
end;

// 円・楕円を描く処理
procedure DrawRedCircle;
begin
  SetColor(Red); // 赤色
  SetLineStyle(SolidLn, 0, ThickWidth);
  Circle(400, 100, 80);
end;

procedure DrawYellowFilledCircle;
begin
  SetColor(White); // 白色
  SetFillStyle(SolidFill, Yellow); // 塗りつぶしを黄色
  FillEllipse(400, 300, 80, 80);
end;

procedure DrawWhiteEllipse;
begin
  SetColor(White); // 白色
  Ellipse(800, 300, 0, 360, 120, 80);
end;

// 扇を描く処理
procedure DrawPieSlice;
begin
  SetColor(White); // 白色
  SetFillStyle(LineFill, Blue); // 塗りつぶしを青色
  PieSlice(800, 200, 80, 120, 80);
end;

// 四角形を描く処理
procedure DrawCyanBar;
begin
  SetColor(Cyan); // シアン色
  SetFillStyle(SolidFill, Cyan); // 塗りつぶしをシアン色
  Bar(100, 220, 300, 400);
end;

procedure DrawYellowBar;
begin
  SetColor(Yellow); // 黄色
  SetFillStyle(CloseDotFill, White); // 塗りつぶしを白色
  Bar(500, 220, 600, 400);
end;

// メイン処理
begin
  InitializeGraphics;

  // 三角形を描く3本の線
  DrawYellowLine;
  DrawWhiteDottedLine;
  DrawGreenThickLine;

  GraphDefaults; // Graph設定リセット

  // 円を描く
  DrawRedCircle;
  DrawYellowFilledCircle;
  DrawWhiteEllipse; // 白線の楕円

  // 四角形を描く
  DrawCyanBar;
  DrawYellowBar;

  // 扇を描く
  DrawPieSlice;

  // 一時入力を入れないと描画せずに終了する為の処置
  readln;

  // Graphモード終了
  CloseGraph;
end.

