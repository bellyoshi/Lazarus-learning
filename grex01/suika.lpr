program suika;
{$mode fpc}{$H+}

uses
  crt, graph, math, Windows;

const
  ScreenWidth = 640;
  ScreenHeight = 480;
  GroundY = 450;
  CeilingY = 50;
  LeftWall = 50;
  RightWall = 590;
  
  ESC = #27;
  Left = #75;
  Right = #77;
  Space = #32;
  
  // フルーツの種類
  FRUIT_CHERRY = 0;
  FRUIT_STRAWBERRY = 1;
  FRUIT_GRAPE = 2;
  FRUIT_ORANGE = 3;
  FRUIT_PINEAPPLE = 4;
  FRUIT_MELON = 5;
  FRUIT_WATERMELON = 6;
  MAX_FRUIT = 6;
  
  // フルーツのサイズ（半径）
  FRUIT_SIZE: array[0..6] of integer = (15, 20, 25, 30, 35, 40, 45);
  // フルーツの色（メイン）
  FRUIT_COLOR: array[0..6] of integer = (Red, Red, Magenta, Yellow, Yellow, Green, Green);
  // フルーツの色（サブ）
  FRUIT_COLOR2: array[0..6] of integer = (Red, Green, Magenta, Yellow, DarkGray, LightGreen, Red);
  // フルーツのスコア
  FRUIT_SCORE: array[0..6] of integer = (1, 3, 6, 10, 15, 21, 28);

type
  TFruit = record
    x, y: real;
    fruitType: integer;
    vx, vy: real;
    active: boolean;
  end;

var
  gd, gm: integer;
  fruits: array[0..100] of TFruit;
  currentFruit: TFruit;
  fruitCount: integer;
  score: integer;
  gameOver: boolean;
  key: char;
  i, j: integer;
  merged: boolean;

function IsKeyPressed(vKey: integer): boolean;
begin
  IsKeyPressed := (GetAsyncKeyState(vKey) and $8000) <> 0;
end;

procedure initGraphMode;
begin
  gd := D8bit;
  gm := m640x480;
  initgraph(gd, gm, '');
  cleardevice;
end;

procedure drawFrame;
begin
  SetColor(White);
  SetLineStyle(SolidLn, 0, NormWidth);
  // 天井
  Line(LeftWall, CeilingY, RightWall, CeilingY);
  // 左壁
  Line(LeftWall, CeilingY, LeftWall, GroundY);
  // 右壁
  Line(RightWall, CeilingY, RightWall, GroundY);
  // 地面
  Line(LeftWall, GroundY, RightWall, GroundY);
  
  // スコア表示
  SetColor(Yellow);
  OutTextXY(LeftWall + 10, 10, 'SCORE:');
end;

procedure dispScore;
var
  s: string;
begin
  SetColor(Black);
  SetFillStyle(SolidFill, Black);
  Bar(LeftWall + 80, 5, LeftWall + 200, 25);
  str(score, s);
  SetColor(Yellow);
  OutTextXY(LeftWall + 80, 10, s);
end;

procedure drawFruit(fruit: TFruit);
var
  size: integer;
  x, y: integer;
  i: integer;
begin
  if not fruit.active then exit;
  
  size := FRUIT_SIZE[fruit.fruitType];
  x := round(fruit.x);
  y := round(fruit.y);
  
  case fruit.fruitType of
    FRUIT_CHERRY: // さくらんぼ
    begin
      SetColor(Red);
      SetFillStyle(SolidFill, Red);
      FillEllipse(x - size div 2, y, size div 2, size div 2);
      FillEllipse(x + size div 2, y, size div 2, size div 2);
      SetColor(DarkGray);
      SetLineStyle(SolidLn, 0, 2);
      Line(x, y - size div 2, x, y - size);
    end;
    
    FRUIT_STRAWBERRY: // いちご
    begin
      SetColor(Red);
      SetFillStyle(SolidFill, Red);
      FillEllipse(x, y, size, size * 4 div 3);
      SetColor(Green);
      SetFillStyle(SolidFill, Green);
      PieSlice(x, y - size * 2 div 3, 0, 360, size div 3);
      SetColor(Yellow);
      SetFillStyle(SolidFill, Yellow);
      for i := 0 to 3 do
        FillEllipse(x - size div 2 + (i * size div 3), y - size div 4, 2, 2);
    end;
    
    FRUIT_GRAPE: // ぶどう
    begin
      SetColor(Magenta);
      SetFillStyle(SolidFill, Magenta);
      for i := 0 to 2 do
      begin
        FillEllipse(x - size div 2 + (i * size div 2), y - size div 2, size div 2, size div 2);
        FillEllipse(x - size div 2 + (i * size div 2), y + size div 4, size div 2, size div 2);
      end;
    end;
    
    FRUIT_ORANGE: // オレンジ
    begin
      SetColor(Yellow);
      SetFillStyle(SolidFill, Yellow);
      FillEllipse(x, y, size, size);
      SetColor(White);
      SetLineStyle(SolidLn, 0, 1);
      for i := 0 to 3 do
      begin
        Line(x - size div 2, y - size div 2 + (i * size div 3), 
             x + size div 2, y - size div 2 + (i * size div 3));
        Line(x - size div 2 + (i * size div 3), y - size div 2, 
             x - size div 2 + (i * size div 3), y + size div 2);
      end;
    end;
    
    FRUIT_PINEAPPLE: // パイナップル
    begin
      SetColor(Yellow);
      SetFillStyle(SolidFill, Yellow);
      FillEllipse(x, y, size, size * 4 div 3);
      SetColor(DarkGray);
      SetLineStyle(SolidLn, 0, 1);
      for i := 0 to 2 do
      begin
        Line(x - size div 2, y - size div 2 + (i * size div 2), 
             x + size div 2, y - size div 2 + (i * size div 2));
        Line(x - size div 2 + (i * size div 2), y - size div 2, 
             x - size div 2 + (i * size div 2), y + size div 2);
      end;
      SetColor(Green);
      SetFillStyle(SolidFill, Green);
      PieSlice(x, y - size, 0, 360, size div 3);
    end;
    
    FRUIT_MELON: // メロン
    begin
      SetColor(Green);
      SetFillStyle(SolidFill, Green);
      FillEllipse(x, y, size, size * 4 div 3);
      SetColor(LightGreen);
      SetLineStyle(SolidLn, 0, 2);
      for i := 0 to 2 do
        Line(x - size div 2 + (i * size div 2), y - size div 2, 
             x - size div 2 + (i * size div 2), y + size div 2);
    end;
    
    FRUIT_WATERMELON: // スイカ
    begin
      SetColor(Green);
      SetFillStyle(SolidFill, Green);
      FillEllipse(x, y, size, size * 4 div 3);
      SetColor(Red);
      SetFillStyle(SolidFill, Red);
      FillEllipse(x, y, size * 3 div 4, size);
      SetColor(Black);
      SetFillStyle(SolidFill, Black);
      for i := 0 to 2 do
        FillEllipse(x - size div 3 + (i * size div 3), y, 2, 2);
      SetColor(Green);
      SetLineStyle(SolidLn, 0, 2);
      for i := 0 to 2 do
        Line(x - size div 2 + (i * size div 2), y - size div 2, 
             x - size div 2 + (i * size div 2), y + size div 2);
    end;
  end;
end;

procedure eraseFruit(fruit: TFruit);
var
  size: integer;
  x, y: integer;
  height: integer;
begin
  if not fruit.active then exit;
  
  size := FRUIT_SIZE[fruit.fruitType];
  x := round(fruit.x);
  y := round(fruit.y);
  
  // 各フルーツの実際の描画範囲に合わせて消去
  case fruit.fruitType of
    FRUIT_CHERRY: // さくらんぼは横に広がっている
    begin
      SetColor(Black);
      SetFillStyle(SolidFill, Black);
      Bar(x - size - 5, y - size - 5, x + size + 5, y + size div 2 + 5);
    end;
    
    FRUIT_STRAWBERRY, FRUIT_PINEAPPLE, FRUIT_MELON, FRUIT_WATERMELON: // 縦に長いフルーツ
    begin
      height := (size * 4 div 3) + size div 3 + 8; // いちごの葉も含める
      SetColor(Black);
      SetFillStyle(SolidFill, Black);
      Bar(x - size - 5, y - height div 2 - 5, x + size + 5, y + height div 2 + 5);
    end;
    
    else // その他のフルーツ
    begin
      SetColor(Black);
      SetFillStyle(SolidFill, Black);
      Bar(x - size - 5, y - size - 5, x + size + 5, y + size + 5);
    end;
  end;
end;

function checkCollision(f1, f2: TFruit): boolean;
var
  dx, dy, dist, minDist: real;
begin
  checkCollision := false;
  if not f1.active or not f2.active then exit;
  if f1.fruitType <> f2.fruitType then exit;
  
  dx := f1.x - f2.x;
  dy := f1.y - f2.y;
  dist := sqrt(dx * dx + dy * dy);
  minDist := FRUIT_SIZE[f1.fruitType] + FRUIT_SIZE[f2.fruitType];
  
  if dist < minDist then
    checkCollision := true;
end;

function checkWallCollision(fruit: TFruit): boolean;
var
  size: integer;
begin
  checkWallCollision := false;
  size := FRUIT_SIZE[fruit.fruitType];
  
  if (fruit.x - size < LeftWall) or (fruit.x + size > RightWall) then
    checkWallCollision := true;
end;

function checkGroundCollision(fruit: TFruit): boolean;
var
  size: integer;
begin
  checkGroundCollision := false;
  size := FRUIT_SIZE[fruit.fruitType];
  
  if fruit.y + size >= GroundY then
    checkGroundCollision := true;
end;

function checkCeilingCollision(fruit: TFruit): boolean;
var
  size: integer;
begin
  checkCeilingCollision := false;
  size := FRUIT_SIZE[fruit.fruitType];
  
  if fruit.y - size <= CeilingY then
    checkCeilingCollision := true;
end;

procedure mergeFruits(var f1, f2: TFruit);
var
  newX, newY: real;
  newType: integer;
begin
  if f1.fruitType >= MAX_FRUIT then exit;
  
  newX := (f1.x + f2.x) / 2;
  newY := (f1.y + f2.y) / 2;
  newType := f1.fruitType + 1;
  
  eraseFruit(f1);
  eraseFruit(f2);
  
  f1.active := false;
  f2.active := false;
  
  // 新しいフルーツを作成
  if fruitCount < 100 then
  begin
    fruits[fruitCount].x := newX;
    fruits[fruitCount].y := newY;
    fruits[fruitCount].fruitType := newType;
    fruits[fruitCount].vx := 0;
    fruits[fruitCount].vy := 0;
    fruits[fruitCount].active := true;
    fruitCount := fruitCount + 1;
    score := score + FRUIT_SCORE[newType];
    merged := true;
  end;
end;

procedure updateFruit(var fruit: TFruit);
var
  i: integer;
  size: integer;
begin
  if not fruit.active then exit;
  
  size := FRUIT_SIZE[fruit.fruitType];
  
  // 重力
  fruit.vy := fruit.vy + 1.5;
  
  // 移動
  fruit.x := fruit.x + fruit.vx;
  fruit.y := fruit.y + fruit.vy;
  
  // 壁との衝突
  if checkWallCollision(fruit) then
  begin
    fruit.vx := -fruit.vx * 0.5;
    if fruit.x - size < LeftWall then
      fruit.x := LeftWall + size
    else if fruit.x + size > RightWall then
      fruit.x := RightWall - size;
  end;
  
  // 地面との衝突
  if checkGroundCollision(fruit) then
  begin
    fruit.y := GroundY - size;
    fruit.vy := -fruit.vy * 0.3;
    fruit.vx := fruit.vx * 0.8;
    if abs(fruit.vy) < 1.0 then
      fruit.vy := 0;
  end;
  
  // 他のフルーツとの衝突チェック
  for i := 0 to fruitCount - 1 do
  begin
    if (fruits[i].active) and (@fruits[i] <> @fruit) then
    begin
      if checkCollision(fruit, fruits[i]) then
      begin
        mergeFruits(fruit, fruits[i]);
        exit;
      end;
    end;
  end;
end;

procedure createNewFruit;
begin
  currentFruit.x := (LeftWall + RightWall) div 2;
  currentFruit.y := CeilingY + 30;
  currentFruit.fruitType := random(3); // 最初の3種類からランダム
  currentFruit.vx := 0;
  currentFruit.vy := 0;
  currentFruit.active := true;
end;

procedure dropCurrentFruit;
begin
  if not currentFruit.active then exit;
  
  currentFruit.vy := 6.0; // 落下開始
  fruits[fruitCount] := currentFruit;
  fruitCount := fruitCount + 1;
  currentFruit.active := false;
end;

procedure initGame;
begin
  randomize;
  fruitCount := 0;
  score := 0;
  gameOver := false;
  currentFruit.active := false;
  
  for i := 0 to 100 do
    fruits[i].active := false;
  
  createNewFruit;
end;

procedure gameOverScreen;
var
  s: string;
begin
  cleardevice;
  SetColor(Cyan);
  OutTextXY(ScreenWidth div 2 - 80, ScreenHeight div 2 - 30, '<<< GAME OVER >>>');
  SetColor(Yellow);
  str(score, s);
  OutTextXY(ScreenWidth div 2 - 50, ScreenHeight div 2, 'SCORE: ' + s);
  SetColor(White);
  OutTextXY(ScreenWidth div 2 - 100, ScreenHeight div 2 + 30, 'Press ESC to exit');
end;

begin
  initGraphMode;
  initGame;
  
  repeat
    drawFrame;
    dispScore;
    
    // 現在のフルーツの更新（移動前の座標で消去→更新→移動後の座標で描画）
    if currentFruit.active then
    begin
      eraseFruit(currentFruit);
    end;
    
    // 落ちているフルーツの更新（移動前の座標で消去）
    merged := false;
    for i := 0 to fruitCount - 1 do
    begin
      if fruits[i].active then
      begin
        eraseFruit(fruits[i]);
      end;
    end;
    
    delay(20);
    
    // キー入力処理（GetAsyncKeyStateを使用してフォーカスがなくても検出可能）
    if IsKeyPressed(VK_LEFT) then
    begin
      if currentFruit.active and (currentFruit.x - FRUIT_SIZE[currentFruit.fruitType] > LeftWall + 5) then
        currentFruit.x := currentFruit.x - 15;
    end;
    
    if IsKeyPressed(VK_RIGHT) then
    begin
      if currentFruit.active and (currentFruit.x + FRUIT_SIZE[currentFruit.fruitType] < RightWall - 5) then
        currentFruit.x := currentFruit.x + 15;
    end;
    
    if IsKeyPressed(VK_SPACE) then
    begin
      if currentFruit.active then
      begin
        dropCurrentFruit;
        delay(500);
        createNewFruit;
      end;
    end;
    
    if IsKeyPressed(VK_ESCAPE) then
      gameOver := true;
    
    // 自動落下（一定時間経過後）
    if currentFruit.active then
    begin
      currentFruit.y := currentFruit.y + 3;
      if currentFruit.y + FRUIT_SIZE[currentFruit.fruitType] >= GroundY - 10 then
      begin
        dropCurrentFruit;
        delay(300);
        createNewFruit;
      end;
    end;
    
    // 落ちているフルーツの更新（座標を更新）
    for i := 0 to fruitCount - 1 do
    begin
      if fruits[i].active then
      begin
        updateFruit(fruits[i]);
      end;
    end;
    
    // 更新後の座標で描画
    if currentFruit.active then
    begin
      drawFruit(currentFruit);
    end;
    
    for i := 0 to fruitCount - 1 do
    begin
      if fruits[i].active then
      begin
        drawFruit(fruits[i]);
      end;
    end;
    
    // 天井衝突チェック
    for i := 0 to fruitCount - 1 do
    begin
      if fruits[i].active and checkCeilingCollision(fruits[i]) then
      begin
        gameOver := true;
        break;
      end;
    end;
    
  until gameOver;
  
  gameOverScreen;
  
  repeat
    if IsKeyPressed(VK_ESCAPE) then
      break;
    delay(100);
  until false;
  
  closegraph;
end.

