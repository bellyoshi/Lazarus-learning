program grex08;
{$mode fpc}{$H+} 

uses
  SysUtils, Graph;

const 
  ScreenWidth = 640;
  centerX = ScreenWidth div 2;

var
  GraphDriver, GraphMode: integer;
  x, y: integer;

procedure InitializeGraphics;
begin
  GraphDriver := D8bit;
  GraphMode := m640x480;
  initgraph(GraphDriver, GraphMode, '');
  ClearDevice;
end;

begin
  InitializeGraphics;

  //3つのサインカーブ
  for x := 1 to ScreenWidth do begin
    y := round(60 * sin(x / 6));
    PutPixel(x, y + centerX, Cyan);

    y := round(60 * sin(x / 60));
    PutPixel(x, y + centerX, Yellow);

    y := round(60 * sin(x / 45));
    putpixel(x, y + centerX, Red);
  end;

  readln;
  CloseGraph;
end.