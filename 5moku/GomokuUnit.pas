unit GomokuUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // 石の種類
  TStone = (stNone, stBlack, stWhite);
  
  // プレイヤー
  TPlayer = (plBlack, plWhite);
  
  // 盤面の状態
  TBoard = array[0..14, 0..14] of TStone;
  
  // 五目並べゲームクラス
  TGomokuGame = class
  private
    FBoard: TBoard;
    FCurrentPlayer: TPlayer;
    FGameOver: Boolean;
    FWinner: TStone;
    FBoardSize: Integer;
    
    // 勝敗判定（指定位置から5つ連続しているかチェック）
    function CheckWin(X, Y: Integer): Boolean;
    // 方向ごとに連続数をカウント
    function CountStones(X, Y: Integer; DirX, DirY: Integer): Integer;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // 石を配置
    function PlaceStone(X, Y: Integer): Boolean;
    // 盤面をクリア
    procedure ClearBoard;
    // 指定位置の石を取得
    function GetStone(X, Y: Integer): TStone;
    // 現在のプレイヤーを取得
    function GetCurrentPlayer: TPlayer;
    // ゲーム終了状態を取得
    function IsGameOver: Boolean;
    // 勝者を取得
    function GetWinner: TStone;
    // 盤面サイズを取得
    function GetBoardSize: Integer;
    // 次のプレイヤーに切り替え
    procedure SwitchPlayer;
  end;

implementation

constructor TGomokuGame.Create;
begin
  inherited Create;
  FBoardSize := 15;
  FCurrentPlayer := plBlack;
  FGameOver := False;
  FWinner := stNone;
  ClearBoard;
end;

destructor TGomokuGame.Destroy;
begin
  inherited Destroy;
end;

procedure TGomokuGame.ClearBoard;
var
  X, Y: Integer;
begin
  for Y := 0 to FBoardSize - 1 do
    for X := 0 to FBoardSize - 1 do
      FBoard[X, Y] := stNone;
  FCurrentPlayer := plBlack;
  FGameOver := False;
  FWinner := stNone;
end;

function TGomokuGame.GetStone(X, Y: Integer): TStone;
begin
  if (X >= 0) and (X < FBoardSize) and (Y >= 0) and (Y < FBoardSize) then
    Result := FBoard[X, Y]
  else
    Result := stNone;
end;

function TGomokuGame.GetCurrentPlayer: TPlayer;
begin
  Result := FCurrentPlayer;
end;

function TGomokuGame.IsGameOver: Boolean;
begin
  Result := FGameOver;
end;

function TGomokuGame.GetWinner: TStone;
begin
  Result := FWinner;
end;

function TGomokuGame.GetBoardSize: Integer;
begin
  Result := FBoardSize;
end;

procedure TGomokuGame.SwitchPlayer;
begin
  if FCurrentPlayer = plBlack then
    FCurrentPlayer := plWhite
  else
    FCurrentPlayer := plBlack;
end;

function TGomokuGame.PlaceStone(X, Y: Integer): Boolean;
var
  Stone: TStone;
begin
  Result := False;
  
  // 範囲チェック
  if (X < 0) or (X >= FBoardSize) or (Y < 0) or (Y >= FBoardSize) then
    Exit;
  
  // 既に石が置かれているかチェック
  if FBoard[X, Y] <> stNone then
    Exit;
  
  // ゲーム終了チェック
  if FGameOver then
    Exit;
  
  // 石を配置
  if FCurrentPlayer = plBlack then
    Stone := stBlack
  else
    Stone := stWhite;
  
  FBoard[X, Y] := Stone;
  
  // 勝敗判定
  if CheckWin(X, Y) then
  begin
    FGameOver := True;
    FWinner := Stone;
  end;
  
  // 次のプレイヤーに切り替え（ゲームが終了していない場合）
  if not FGameOver then
    SwitchPlayer;
  
  Result := True;
end;

function TGomokuGame.CountStones(X, Y: Integer; DirX, DirY: Integer): Integer;
var
  Stone: TStone;
  Count: Integer;
  NewX, NewY: Integer;
begin
  Count := 0;
  Stone := FBoard[X, Y];
  
  if Stone = stNone then
  begin
    Result := 0;
    Exit;
  end;
  
  // 指定方向に連続する石をカウント
  NewX := X;
  NewY := Y;
  while (NewX >= 0) and (NewX < FBoardSize) and
        (NewY >= 0) and (NewY < FBoardSize) and
        (FBoard[NewX, NewY] = Stone) do
  begin
    Inc(Count);
    NewX := NewX + DirX;
    NewY := NewY + DirY;
  end;
  
  Result := Count;
end;

function TGomokuGame.CheckWin(X, Y: Integer): Boolean;
var
  Stone: TStone;
  Count: Integer;
begin
  Result := False;
  Stone := FBoard[X, Y];
  
  if Stone = stNone then
    Exit;
  
  // 8方向をチェック（4方向のペア）
  // 水平方向（左右）
  Count := CountStones(X, Y, -1, 0) + CountStones(X, Y, 1, 0) - 1;
  if Count >= 5 then
  begin
    Result := True;
    Exit;
  end;
  
  // 垂直方向（上下）
  Count := CountStones(X, Y, 0, -1) + CountStones(X, Y, 0, 1) - 1;
  if Count >= 5 then
  begin
    Result := True;
    Exit;
  end;
  
  // 斜め方向（左上-右下）
  Count := CountStones(X, Y, -1, -1) + CountStones(X, Y, 1, 1) - 1;
  if Count >= 5 then
  begin
    Result := True;
    Exit;
  end;
  
  // 斜め方向（右上-左下）
  Count := CountStones(X, Y, 1, -1) + CountStones(X, Y, -1, 1) - 1;
  if Count >= 5 then
  begin
    Result := True;
    Exit;
  end;
end;

end.

