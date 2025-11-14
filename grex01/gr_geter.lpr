program gr_geter;
{$mode fpc}{$H+}

uses
  crt, graph, math;
const
  wxx = 639;
  hyy = 479;

  ESC = #27;
  UP = #72;
  Down = #80;
  Left = #75;
  Right = #77;

  HMin = 100;
  WMin = 20;

type
  Tplay = record
    x, y: integer;
  end;

  Tpoint = record
    x, y: integer;
  end;

var
  gd, gm: integer;
  direct, newdirect : byte;
  play: Tplay;
  score, speed: integer;
  point: Tpoint;
  key: char;
  endflg, exitflg: boolean;

procedure initGraphMode;
begin
  gd := D8bit;
  gm := m640x480;
  initgraph(gd, gm, '');
  cleardevice;
end;

procedure soundGet;
begin
  sound(1000);
  delay(120);
  nosound;
end;

procedure drawFrame(title: string);
begin
  SetColor(yellow);
  OutTextXY(200, 30, title);
  SetColor(cyan);
  OutTextXY(50, hyy-10, 'SCORE:');
  SetLineStyle(SolidLn, 0, NormWidth);
  Line(WMin, hMin, wxx-WMin, HMin);
  MoveTo(wxx-Wmin, HMin);
  LineTo(wxx-WMin, hyy-WMin);
  MoveTo(wxx-WMin, hyy-WMin);
  LineTo(WMin, hyy-WMin);
  MoveTo(WMin, hyy-WMin);
  LineTo(WMin, HMin);
end;

procedure dispScore(flg: boolean);
var
  c: integer;
  s: string;
begin
  if flg then
    c:= yellow
  else
    c:= black;
  str(score, s);
  SetColor(c);
  OutTextxy(120, hyy- 10, s);
end;

procedure dispPlay(flg: boolean);
var
  c: integer;
begin
  if flg then
    c:= green
  else
    c:= black;

  SetColor(c);
  Bar(play.x-10, play.y-10, play.x+10, play.y+10);
end;

procedure dispPoint(flg: boolean);
var
  c: integer;
begin
  if flg then
  begin
    c:= yellow;
    SetColor(c);
    SetFillStyle(SolidFill, c);
    PieSlice(point.x, point.y, 0, 360, 5);
  end
  else
  begin
    c:= black;
    SetColor(c);
    SetFillStyle(SolidFill, c);
    Bar(point.x - 6, point.y - 6, point.x + 6, point.y + 6);
  end;
end;

function checkPlay(x, y: integer): boolean;
begin
  checkPlay := false;

  if(play.x = x) and (play.y = y) then
    checkPlay := true;
end;

procedure newPoint;
var
  x, y: integer;
begin
  randomize;
  repeat
    x := random(wxx - WMin);
    y := random(hyy - HMin);
  until not checkPlay(x, y);
  if x < Wmin then x := x + 100;
  if y < Hmin then y := y + 100;
  point.x := x;
  point.y := y;
end;

procedure checkPoint;
begin
  if(play.x <= point.x + 10) and (play.x >= point.x - 10) 
  and (play.y <= point.y + 10) and (play.y >= point.y - 10) then
  begin
    dispScore(false);
    score := score + 1;
    speed := speed + 1;
    SoundGet;
    dispPoint(false);
    newPoint;
    dispPoint(true);
    dispScore(true);
  end;
end;

function collision: boolean;
begin
  collision := false;

  if(play.x < WMin + 1) or (play.x > wxx- 1) or
    (play.y < HMin + 1) or (play.y > hyy- 1) then
  begin

    collision := true;
  end;
end;

procedure movePlay;
var
  x,y : integer;
begin
 case direct of
   1: begin x:= 1 * speed; y:= 0 * speed; end;
   2: begin x:= 0 * speed; y:= 1 * speed; end;
   3: begin x:= -1 * speed; y:= 0 * speed; end;
   4: begin x:= 0 * speed; y:= -1 * speed; end;
  end;
  play.x := play.x + x;
  play.y := play.y + y;
  checkPoint;
  if collision then
  begin
    exitflg := true;
  end;
end;

procedure GaeOver;
var
  x, y : integer;
  s : string;
  c :char;
begin
  cleardevice;
  SetColor(cyan);
  x := round(GetMaxX div 2);
  y := round(GetMaxY div 2);
  str(score, s);
  OutTextXY(x -50, y - 50, '<<<GAME OVER>>>');
  SetColor(yellow);
  OutTextXY(x -50, y - 30, 'YOU SCORE: ');
  OutTextXY(x -40, y - 10, s);
  c := readkey;
  if (c = ESC) then exitflg := true;
end;

procedure initGame;
var
  x, y : integer;
begin
  direct := 1;
  speed := 1;
  score := 0;
  endflg := false;
  exitflg := false;
  drawFrame('Point Get Game for Graph Unit');
  x := round(GetMaxX div 2);
  y := round(GetMaxY div 2);
  play.x := x;
  play.y := y;
  newPoint;
  dispPoint(true);
  dispScore(true);
end;

begin
  initGraphMode;
  initGame;
  repeat
    dispPlay(true);
    delay(60);
    if (keypressed) then begin
      key := readkey;
      if (key =#0 ) then begin
        key := readkey;
        case ord(key) of
          ord(UP) : begin newdirect := 4; end;
          ord(Down) : begin newdirect := 2; end;
          ord(Left) : begin newdirect := 3; end;
          ord(Right) : begin newdirect := 1; end;
        end;

        if (direct = 1) and (newdirect <> 3) then
          direct := newdirect;
        if (direct = 2) and (newdirect <> 4) then
          direct := newdirect;
        if (direct = 3) and (newdirect <> 1) then
          direct := newdirect;
        if (direct = 4) and (newdirect <> 2) then
          direct := newdirect;
      end;
    if (key = ESC) then exitflg := true;
    end;
    dispPlay(false);
    checkPoint;
    movePlay;
    if endflg then GaeOver;
  until exitflg;
  closegraph;
end.
