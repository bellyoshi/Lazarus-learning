program gr_geter;
{$mode fpc}{$H+} 
uses
  SysUtils, Graph, Keyboard, Crt;

const
  ScreenWidth = 640;
  ScreenHeight = 480;
  PlayerSize = 10;
  TreasureSize = 8;
  InitialSpeed = 2;
  SpeedIncrement = 0.5;
  MinSpeed = 1;
  MaxSpeed = 10;

type
  Direction = (DirNone, DirUp, DirDown, DirLeft, DirRight);

var
  GraphDriver, GraphMode: integer;
  PlayerX, PlayerY: integer;
  TreasureX, TreasureY: integer;
  CurrentDirection: Direction;
  GameSpeed: real;
  Score: integer;
  GameOver: boolean;
  KeyEvent: TKeyEvent;

// グラフィック初期化
procedure InitializeGraphics;
begin
  GraphDriver := D8bit;
  GraphMode := m640x480;
  initgraph(GraphDriver, GraphMode, '');
  ClearDevice;
end;

// プレイヤーの描画
procedure DrawPlayer;
begin
  SetColor(White);
  SetFillStyle(SolidFill, Cyan);
  Bar(PlayerX - PlayerSize div 2, PlayerY - PlayerSize div 2,
      PlayerX + PlayerSize div 2, PlayerY + PlayerSize div 2);
end;

// お宝の描画
procedure DrawTreasure;
begin
  SetColor(Yellow);
  SetFillStyle(SolidFill, Yellow);
  FillEllipse(TreasureX, TreasureY, TreasureSize, TreasureSize);
end;

// お宝の位置をランダムに生成
procedure GenerateTreasure;
begin
  TreasureX := Random(ScreenWidth - 50) + 25;
  TreasureY := Random(ScreenHeight - 50) + 25;
end;

// 衝突判定（プレイヤーとお宝）
function CheckCollision: boolean;
var
  Distance: real;
begin
  Distance := sqrt(sqr(PlayerX - TreasureX) + sqr(PlayerY - TreasureY));
  CheckCollision := Distance < (PlayerSize + TreasureSize);
end;

// 境界チェック
function CheckBoundary: boolean;
begin
  CheckBoundary := (PlayerX < PlayerSize div 2) or 
                   (PlayerX > ScreenWidth - PlayerSize div 2) or
                   (PlayerY < PlayerSize div 2) or 
                   (PlayerY > ScreenHeight - PlayerSize div 2);
end;

// キー入力処理
procedure ProcessInput;
var
  KeyCode: word;
begin
  if PollKeyEvent <> 0 then
  begin
    KeyEvent := GetKeyEvent;
    KeyEvent := TranslateKeyEvent(KeyEvent);
    KeyCode := GetKeyEventCode(KeyEvent);
    
    // 矢印キーの処理
    case KeyCode of
      377: CurrentDirection := DirUp;    // 上矢印
      378: CurrentDirection := DirDown;  // 下矢印
      375: CurrentDirection := DirLeft;  // 左矢印
      376: CurrentDirection := DirRight; // 右矢印
    end;
  end;
end;

// プレイヤーの移動
procedure MovePlayer;
var
  MoveX, MoveY: integer;
begin
  if CurrentDirection = DirNone then
    exit;
    
  MoveX := 0;
  MoveY := 0;
  
  case CurrentDirection of
    DirUp: MoveY := -round(GameSpeed);
    DirDown: MoveY := round(GameSpeed);
    DirLeft: MoveX := -round(GameSpeed);
    DirRight: MoveX := round(GameSpeed);
  end;
  
  PlayerX := PlayerX + MoveX;
  PlayerY := PlayerY + MoveY;
end;

// 画面のクリアと再描画
procedure RedrawScreen;
begin
  ClearDevice;
  
  // スコア表示
  SetColor(White);
  OutTextXY(10, 10, 'Score: ' + IntToStr(Score));
  OutTextXY(10, 25, 'Speed: ' + FormatFloat('0.0', GameSpeed));
  
  // プレイヤーとお宝を描画
  DrawTreasure;
  DrawPlayer;
end;

// ゲームオーバー画面
procedure ShowGameOver;
begin
  ClearDevice;
  SetColor(Red);
  SetTextStyle(DefaultFont, HorizDir, 3);
  OutTextXY(ScreenWidth div 2 - 100, ScreenHeight div 2 - 20, 'GAME OVER');
  SetTextStyle(DefaultFont, HorizDir, 1);
  SetColor(White);
  OutTextXY(ScreenWidth div 2 - 80, ScreenHeight div 2 + 20, 'Final Score: ' + IntToStr(Score));
  OutTextXY(ScreenWidth div 2 - 100, ScreenHeight div 2 + 40, 'Press any key to exit...');
end;

// メイン処理
begin
  Randomize;
  InitKeyBoard;
  InitializeGraphics;
  
  // ゲーム初期化
  PlayerX := ScreenWidth div 2;
  PlayerY := ScreenHeight div 2;
  CurrentDirection := DirNone;
  GameSpeed := InitialSpeed;
  Score := 0;
  GameOver := false;
  
  GenerateTreasure;
  RedrawScreen;
  
  // ゲームループ
  repeat
    ProcessInput;
    
    if not GameOver then
    begin
      MovePlayer;
      
      // 境界チェック
      if CheckBoundary then
      begin
        GameOver := true;
        ShowGameOver;
      end
      else
      begin
        // 衝突判定
        if CheckCollision then
        begin
          Inc(Score);
          // スピードアップ（最大速度まで）
          if GameSpeed < MaxSpeed then
            GameSpeed := GameSpeed + SpeedIncrement;
          GenerateTreasure;
        end;
        
        RedrawScreen;
      end;
    end;
    
    // 少し待機（フレームレート制御）
    Delay(50);
  until GameOver and (PollKeyEvent <> 0);
  
  // キー入力待ち
  if GameOver then
  begin
    repeat
      if PollKeyEvent <> 0 then
      begin
        KeyEvent := GetKeyEvent;
        KeyEvent := TranslateKeyEvent(KeyEvent);
      end;
      Delay(100);
    until PollKeyEvent <> 0;
  end;
  
  CloseGraph;
  DoneKeyBoard;
end.

