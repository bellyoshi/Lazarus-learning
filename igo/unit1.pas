unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Math, GoBoard;

type
  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FGoBoard: TGoBoard;
    FBoardSize: Integer;
    FCellSize: Integer;
    FOffsetX, FOffsetY: Integer;
    
    procedure DrawBoard;
    procedure DrawStone(X, Y: Integer; StoneColor: TStoneColor);
    procedure UpdateStatus;
    function ScreenToBoard(X, Y: Integer): TGoPoint;
    function BoardToScreen(const Point: TGoPoint): TPoint;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FBoardSize := 19;
  FGoBoard := TGoBoard.Create(FBoardSize);
  UpdateStatus;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FGoBoard.Free;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  DrawBoard;
end;

procedure TForm1.DrawBoard;
var
  i, j: Integer;
  X, Y: Integer;
  Point: TGoPoint;
  StoneColor: TStoneColor;
begin
  with PaintBox1.Canvas do
  begin
    // 背景を塗りつぶし
    Brush.Color := clCream;
    FillRect(0, 0, PaintBox1.Width, PaintBox1.Height);
    
    // セルサイズを計算
    FCellSize := Min(PaintBox1.Width div (FBoardSize + 1), 
                     PaintBox1.Height div (FBoardSize + 1));
    FOffsetX := (PaintBox1.Width - FCellSize * (FBoardSize - 1)) div 2;
    FOffsetY := (PaintBox1.Height - FCellSize * (FBoardSize - 1)) div 2;
    
    // 碁盤の線を描画
    Pen.Color := clBlack;
    Pen.Width := 1;
    
    for i := 0 to FBoardSize - 1 do
    begin
      // 縦線
      X := FOffsetX + i * FCellSize;
      MoveTo(X, FOffsetY);
      LineTo(X, FOffsetY + (FBoardSize - 1) * FCellSize);
      
      // 横線
      Y := FOffsetY + i * FCellSize;
      MoveTo(FOffsetX, Y);
      LineTo(FOffsetX + (FBoardSize - 1) * FCellSize, Y);
    end;
    
    // 星の位置を描画（天元と星）
    Brush.Color := clBlack;
    for i := 0 to FBoardSize - 1 do
    begin
      for j := 0 to FBoardSize - 1 do
      begin
        if ((i = 3) or (i = 9) or (i = 15)) and 
           ((j = 3) or (j = 9) or (j = 15)) then
        begin
          X := FOffsetX + i * FCellSize;
          Y := FOffsetY + j * FCellSize;
          Ellipse(X - 3, Y - 3, X + 3, Y + 3);
        end;
      end;
    end;
    
    // 石を描画
    for i := 0 to FBoardSize - 1 do
    begin
      for Y := 0 to FBoardSize - 1 do
      begin
        Point.X := i;
        Point.Y := Y;
        StoneColor := FGoBoard.GetStoneAt(i, Y);
        if StoneColor <> scNone then
          DrawStone(i, Y, StoneColor);
      end;
    end;
  end;
end;

procedure TForm1.DrawStone(X, Y: Integer; StoneColor: TStoneColor);
var
  ScreenPos: TPoint;
  Point: TGoPoint;
  Radius: Integer;
begin
  Point.X := X;
  Point.Y := Y;
  ScreenPos := BoardToScreen(Point);
  Radius := FCellSize div 2 - 2;
  
  with PaintBox1.Canvas do
  begin
    if StoneColor = scBlack then
    begin
      Brush.Color := clBlack;
      Pen.Color := clBlack;
    end
    else if StoneColor = scWhite then
    begin
      Brush.Color := clWhite;
      Pen.Color := clBlack;
    end
    else
      Exit;
    
    Ellipse(ScreenPos.X - Radius, ScreenPos.Y - Radius,
            ScreenPos.X + Radius, ScreenPos.Y + Radius);
  end;
end;

procedure TForm1.UpdateStatus;
var
  PlayerName: String;
begin
  if FGoBoard.CurrentPlayer = scBlack then
    PlayerName := '黒'
  else
    PlayerName := '白';
  
  Label1.Caption := Format('手番: %s  着手数: %d', 
    [PlayerName, FGoBoard.MoveCount]);
end;

function TForm1.ScreenToBoard(X, Y: Integer): TGoPoint;
var
  BoardX, BoardY: Integer;
begin
  BoardX := Round((X - FOffsetX) / FCellSize);
  BoardY := Round((Y - FOffsetY) / FCellSize);
  
  // 範囲チェック
  if BoardX < 0 then BoardX := 0;
  if BoardX >= FBoardSize then BoardX := FBoardSize - 1;
  if BoardY < 0 then BoardY := 0;
  if BoardY >= FBoardSize then BoardY := FBoardSize - 1;
  
  Result.X := BoardX;
  Result.Y := BoardY;
end;

function TForm1.BoardToScreen(const Point: TGoPoint): TPoint;
begin
  Result.X := FOffsetX + Point.X * FCellSize;
  Result.Y := FOffsetY + Point.Y * FCellSize;
end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  BoardPoint: TGoPoint;
begin
  if Button = mbLeft then
  begin
    BoardPoint := ScreenToBoard(X, Y);
    if FGoBoard.PlaceStone(BoardPoint) then
    begin
      PaintBox1.Invalidate;
      UpdateStatus;
    end;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FGoBoard.Pass;
  UpdateStatus;
  PaintBox1.Invalidate;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FGoBoard.Clear;
  UpdateStatus;
  PaintBox1.Invalidate;
end;

end.

