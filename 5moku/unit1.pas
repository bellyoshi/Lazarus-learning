unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  GomokuUnit;

type
  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    Label1: TLabel;
    LabelPlayer: TLabel;
    LabelStatus: TLabel;
    ButtonReset: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonResetClick(Sender: TObject);
  private
    FGomokuGame: TGomokuGame;
    FCellSize: Integer;
    FBoardOffset: Integer;
    
    procedure DrawBoard;
    procedure DrawStone(X, Y: Integer; Stone: TStone);
    procedure UpdateStatus;
    function ScreenToBoard(X, Y: Integer; out BoardX, BoardY: Integer): Boolean;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FGomokuGame := TGomokuGame.Create;
  FCellSize := 30;
  FBoardOffset := 20;
  PaintBox1.Width := FGomokuGame.GetBoardSize * FCellSize + FBoardOffset * 2;
  PaintBox1.Height := FGomokuGame.GetBoardSize * FCellSize + FBoardOffset * 2;
  UpdateStatus;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FGomokuGame) then
    FGomokuGame.Free;
end;

procedure TForm1.DrawBoard;
var
  I, X, Y: Integer;
  ACanvas: TCanvas;
begin
  ACanvas := PaintBox1.Canvas;
  
  // 背景を白に
  ACanvas.Brush.Color := clCream;
  ACanvas.FillRect(0, 0, PaintBox1.Width, PaintBox1.Height);
  
  // 盤面の線を描画
  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Width := 1;
  
  for I := 0 to FGomokuGame.GetBoardSize - 1 do
  begin
    // 横線
    ACanvas.MoveTo(FBoardOffset, FBoardOffset + I * FCellSize);
    ACanvas.LineTo(FBoardOffset + (FGomokuGame.GetBoardSize - 1) * FCellSize,
                  FBoardOffset + I * FCellSize);
    
    // 縦線
    ACanvas.MoveTo(FBoardOffset + I * FCellSize, FBoardOffset);
    ACanvas.LineTo(FBoardOffset + I * FCellSize,
                  FBoardOffset + (FGomokuGame.GetBoardSize - 1) * FCellSize);
  end;
  
  // 星の位置を描画（天元と四隅の星）
  ACanvas.Brush.Color := clBlack;
  ACanvas.FillRect(FBoardOffset + 7 * FCellSize - 3, FBoardOffset + 7 * FCellSize - 3,
                  FBoardOffset + 7 * FCellSize + 3, FBoardOffset + 7 * FCellSize + 3);
  ACanvas.FillRect(FBoardOffset + 3 * FCellSize - 3, FBoardOffset + 3 * FCellSize - 3,
                  FBoardOffset + 3 * FCellSize + 3, FBoardOffset + 3 * FCellSize + 3);
  ACanvas.FillRect(FBoardOffset + 11 * FCellSize - 3, FBoardOffset + 3 * FCellSize - 3,
                  FBoardOffset + 11 * FCellSize + 3, FBoardOffset + 3 * FCellSize + 3);
  ACanvas.FillRect(FBoardOffset + 3 * FCellSize - 3, FBoardOffset + 11 * FCellSize - 3,
                  FBoardOffset + 3 * FCellSize + 3, FBoardOffset + 11 * FCellSize + 3);
  ACanvas.FillRect(FBoardOffset + 11 * FCellSize - 3, FBoardOffset + 11 * FCellSize - 3,
                  FBoardOffset + 11 * FCellSize + 3, FBoardOffset + 11 * FCellSize + 3);
  
  // 石を描画
  for Y := 0 to FGomokuGame.GetBoardSize - 1 do
    for X := 0 to FGomokuGame.GetBoardSize - 1 do
      DrawStone(X, Y, FGomokuGame.GetStone(X, Y));
end;

procedure TForm1.DrawStone(X, Y: Integer; Stone: TStone);
var
  ACanvas: TCanvas;
  CenterX, CenterY: Integer;
  Radius: Integer;
begin
  ACanvas := PaintBox1.Canvas;
  CenterX := FBoardOffset + X * FCellSize;
  CenterY := FBoardOffset + Y * FCellSize;
  Radius := FCellSize div 2 - 2;
  
  if Stone = stBlack then
  begin
    ACanvas.Brush.Color := clBlack;
    ACanvas.Pen.Color := clBlack;
    ACanvas.Ellipse(CenterX - Radius, CenterY - Radius,
                   CenterX + Radius, CenterY + Radius);
  end
  else if Stone = stWhite then
  begin
    ACanvas.Brush.Color := clWhite;
    ACanvas.Pen.Color := clBlack;
    ACanvas.Pen.Width := 2;
    ACanvas.Ellipse(CenterX - Radius, CenterY - Radius,
                   CenterX + Radius, CenterY + Radius);
    ACanvas.Pen.Width := 1;
  end;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  DrawBoard;
end;

function TForm1.ScreenToBoard(X, Y: Integer; out BoardX, BoardY: Integer): Boolean;
var
  AdjX, AdjY: Integer;
begin
  Result := False;
  AdjX := X - FBoardOffset;
  AdjY := Y - FBoardOffset;
  
  if (AdjX < 0) or (AdjY < 0) then
    Exit;
  
  BoardX := Round(AdjX / FCellSize);
  BoardY := Round(AdjY / FCellSize);
  
  if (BoardX >= 0) and (BoardX < FGomokuGame.GetBoardSize) and
     (BoardY >= 0) and (BoardY < FGomokuGame.GetBoardSize) then
    Result := True;
end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  BoardX, BoardY: Integer;
begin
  if Button <> mbLeft then
    Exit;
  
  if ScreenToBoard(X, Y, BoardX, BoardY) then
  begin
    if FGomokuGame.PlaceStone(BoardX, BoardY) then
    begin
      PaintBox1.Invalidate;
      UpdateStatus;
    end;
  end;
end;

procedure TForm1.UpdateStatus;
begin
  if FGomokuGame.IsGameOver then
  begin
    if FGomokuGame.GetWinner = stBlack then
    begin
      LabelStatus.Caption := '黒の勝ち！';
      LabelStatus.Font.Color := clBlack;
    end
    else if FGomokuGame.GetWinner = stWhite then
    begin
      LabelStatus.Caption := '白の勝ち！';
      LabelStatus.Font.Color := clGray;
    end;
    LabelPlayer.Caption := '';
  end
  else
  begin
    LabelStatus.Caption := '';
    if FGomokuGame.GetCurrentPlayer = plBlack then
    begin
      LabelPlayer.Caption := '黒';
      LabelPlayer.Font.Color := clBlack;
    end
    else
    begin
      LabelPlayer.Caption := '白';
      LabelPlayer.Font.Color := clGray;
    end;
  end;
end;

procedure TForm1.ButtonResetClick(Sender: TObject);
begin
  FGomokuGame.ClearBoard;
  PaintBox1.Invalidate;
  UpdateStatus;
end;

end.

