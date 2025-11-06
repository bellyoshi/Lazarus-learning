unit Unit_Reversi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPiece = (Empty, Black, White);
  TBoard = array[0..7, 0..7] of TPiece;
  TPlayer = (PlayerBlack, PlayerWhite);
  TPosition = record
    X, Y: Integer;
  end;
  TDirection = record
    X, Y: Integer;
  end;
  TDirectionArray = array of TDirection;
  TPositionArray = array of TPosition;

  { TReversiGame }

  TReversiGame = class
  private
    FBoard: TBoard;
    FCurrentPlayer: TPlayer;
    FGameOver: Boolean;
    FBlackCount: Integer;
    FWhiteCount: Integer;
    procedure InitializeBoard;
    function GetPieceAt(X, Y: Integer): TPiece;
    procedure SetPieceAt(X, Y: Integer; Piece: TPiece);
    function IsValidPosition(X, Y: Integer): Boolean;
    function GetPieceForPlayer(Player: TPlayer): TPiece;
    function GetOpponentPiece(Piece: TPiece): TPiece;
    function GetDirectionsToCheck: TDirectionArray;
    function CheckDirection(X, Y: Integer; Dir: TDirection; Player: TPlayer): Boolean;
    function FlipPiecesInDirection(X, Y: Integer; Dir: TDirection; Player: TPlayer): Integer;
    procedure UpdateCounts;
  public
    constructor Create;
    destructor Destroy; override;
    function IsValidMove(X, Y: Integer; Player: TPlayer): Boolean;
    function MakeMove(X, Y: Integer; Player: TPlayer): Boolean;
    function GetValidMoves(Player: TPlayer): TPositionArray;
    function HasValidMoves(Player: TPlayer): Boolean;
    procedure SwitchPlayer;
    function CheckGameOver: Boolean;
    function GetWinner: TPlayer;
    procedure Reset;
    
    property Board: TBoard read FBoard;
    property CurrentPlayer: TPlayer read FCurrentPlayer write FCurrentPlayer;
    property GameOver: Boolean read FGameOver;
    property BlackCount: Integer read FBlackCount;
    property WhiteCount: Integer read FWhiteCount;
    property PieceAt[X, Y: Integer]: TPiece read GetPieceAt;
  end;

const
  Directions: array[0..7] of TDirection = (
    (X: -1; Y: -1), // 左上
    (X:  0; Y: -1), // 上
    (X:  1; Y: -1), // 右上
    (X: -1; Y:  0), // 左
    (X:  1; Y:  0), // 右
    (X: -1; Y:  1), // 左下
    (X:  0; Y:  1), // 下
    (X:  1; Y:  1)  // 右下
  );

implementation

{ TReversiGame }

constructor TReversiGame.Create;
begin
  inherited Create;
  InitializeBoard;
  FCurrentPlayer := PlayerBlack;
  FGameOver := False;
  UpdateCounts;
end;

destructor TReversiGame.Destroy;
begin
  inherited Destroy;
end;

procedure TReversiGame.InitializeBoard;
var
  X, Y: Integer;
begin
  // ボードを空で初期化
  for X := 0 to 7 do
    for Y := 0 to 7 do
      FBoard[X, Y] := Empty;
  
  // 初期配置（中央4マス）
  FBoard[3, 3] := White;
  FBoard[4, 4] := White;
  FBoard[3, 4] := Black;
  FBoard[4, 3] := Black;
end;

function TReversiGame.GetPieceAt(X, Y: Integer): TPiece;
begin
  if IsValidPosition(X, Y) then
    Result := FBoard[X, Y]
  else
    Result := Empty;
end;

procedure TReversiGame.SetPieceAt(X, Y: Integer; Piece: TPiece);
begin
  if IsValidPosition(X, Y) then
    FBoard[X, Y] := Piece;
end;

function TReversiGame.IsValidPosition(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (X <= 7) and (Y >= 0) and (Y <= 7);
end;

function TReversiGame.GetPieceForPlayer(Player: TPlayer): TPiece;
begin
  if Player = PlayerBlack then
    Result := Black
  else
    Result := White;
end;

function TReversiGame.GetOpponentPiece(Piece: TPiece): TPiece;
begin
  if Piece = Black then
    Result := White
  else if Piece = White then
    Result := Black
  else
    Result := Empty;
end;

function TReversiGame.GetDirectionsToCheck: TDirectionArray;
var
  I: Integer;
begin
  SetLength(Result, 8);
  for I := 0 to 7 do
    Result[I] := Directions[I];
end;

function TReversiGame.CheckDirection(X, Y: Integer; Dir: TDirection; Player: TPlayer): Boolean;
var
  NextX, NextY: Integer;
  PlayerPiece, OpponentPiece: TPiece;
  FoundOpponent: Boolean;
begin
  Result := False;
  PlayerPiece := GetPieceForPlayer(Player);
  OpponentPiece := GetOpponentPiece(PlayerPiece);
  
  // 隣接マスをチェック
  NextX := X + Dir.X;
  NextY := Y + Dir.Y;
  
  // 隣接マスが相手の駒でなければ無効
  if not IsValidPosition(NextX, NextY) or (FBoard[NextX, NextY] <> OpponentPiece) then
    Exit;
  
  FoundOpponent := False;
  
  // 相手の駒を追跡
  while IsValidPosition(NextX, NextY) do
  begin
    if FBoard[NextX, NextY] = OpponentPiece then
    begin
      FoundOpponent := True;
      NextX := NextX + Dir.X;
      NextY := NextY + Dir.Y;
    end
    else if FBoard[NextX, NextY] = PlayerPiece then
    begin
      // 自分の駒に到達したら有効な手
      Result := FoundOpponent;
      Exit;
    end
    else
      Exit; // 空きマスに到達したら無効
  end;
end;

function TReversiGame.IsValidMove(X, Y: Integer; Player: TPlayer): Boolean;
var
  Dir: TDirection;
  DirectionsToCheck: TDirectionArray;
begin
  Result := False;
  
  // マスが有効で、空きでなければ無効
  if not IsValidPosition(X, Y) or (FBoard[X, Y] <> Empty) then
    Exit;
  
  // 8方向をチェック
  DirectionsToCheck := GetDirectionsToCheck;
  for Dir in DirectionsToCheck do
  begin
    if CheckDirection(X, Y, Dir, Player) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TReversiGame.FlipPiecesInDirection(X, Y: Integer; Dir: TDirection; Player: TPlayer): Integer;
var
  NextX, NextY: Integer;
  PlayerPiece, OpponentPiece: TPiece;
  Flipped: Integer;
begin
  Result := 0;
  PlayerPiece := GetPieceForPlayer(Player);
  OpponentPiece := GetOpponentPiece(PlayerPiece);
  
  NextX := X + Dir.X;
  NextY := Y + Dir.Y;
  
  // 反転する駒を探す
  while IsValidPosition(NextX, NextY) and (FBoard[NextX, NextY] = OpponentPiece) do
  begin
    FBoard[NextX, NextY] := PlayerPiece;
    Inc(Result);
    NextX := NextX + Dir.X;
    NextY := NextY + Dir.Y;
  end;
end;

function TReversiGame.MakeMove(X, Y: Integer; Player: TPlayer): Boolean;
var
  Dir: TDirection;
  DirectionsToCheck: TDirectionArray;
  FlippedCount: Integer;
  PlayerPiece: TPiece;
begin
  Result := False;
  
  // 有効な手でなければ終了
  if not IsValidMove(X, Y, Player) then
    Exit;
  
  PlayerPiece := GetPieceForPlayer(Player);
  
  // 駒を配置
  FBoard[X, Y] := PlayerPiece;
  FlippedCount := 0;
  
  // 8方向の駒を反転
  DirectionsToCheck := GetDirectionsToCheck;
  for Dir in DirectionsToCheck do
  begin
    if CheckDirection(X, Y, Dir, Player) then
      FlippedCount := FlippedCount + FlipPiecesInDirection(X, Y, Dir, Player);
  end;
  
  Result := True;
  UpdateCounts;
end;

function TReversiGame.GetValidMoves(Player: TPlayer): TPositionArray;
var
  X, Y: Integer;
  Count: Integer;
  Pos: TPosition;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for X := 0 to 7 do
    for Y := 0 to 7 do
    begin
      if IsValidMove(X, Y, Player) then
      begin
        SetLength(Result, Count + 1);
        Pos.X := X;
        Pos.Y := Y;
        Result[Count] := Pos;
        Inc(Count);
      end;
    end;
end;

function TReversiGame.HasValidMoves(Player: TPlayer): Boolean;
var
  X, Y: Integer;
begin
  Result := False;
  for X := 0 to 7 do
    for Y := 0 to 7 do
    begin
      if IsValidMove(X, Y, Player) then
      begin
        Result := True;
        Exit;
      end;
    end;
end;

procedure TReversiGame.SwitchPlayer;
begin
  if FCurrentPlayer = PlayerBlack then
    FCurrentPlayer := PlayerWhite
  else
    FCurrentPlayer := PlayerBlack;
end;

function TReversiGame.CheckGameOver: Boolean;
var
  CurrentHasMoves, OpponentHasMoves: Boolean;
  Opponent: TPlayer;
begin
  Result := False;
  
  CurrentHasMoves := HasValidMoves(FCurrentPlayer);
  
  if FCurrentPlayer = PlayerBlack then
    Opponent := PlayerWhite
  else
    Opponent := PlayerBlack;
  
  OpponentHasMoves := HasValidMoves(Opponent);
  
  // 両プレイヤーが手を打てない場合はゲーム終了
  if not CurrentHasMoves and not OpponentHasMoves then
  begin
    FGameOver := True;
    Result := True;
  end
  else if not CurrentHasMoves then
  begin
    // 現在のプレイヤーが手を打てない場合はパス（自動的に相手のターン）
    SwitchPlayer;
  end;
end;

function TReversiGame.GetWinner: TPlayer;
begin
  if not FGameOver then
  begin
    Result := PlayerBlack; // デフォルト値（ゲームが終了していない場合）
    Exit;
  end;
  
  if FBlackCount > FWhiteCount then
    Result := PlayerBlack
  else if FWhiteCount > FBlackCount then
    Result := PlayerWhite
  else
    Result := PlayerBlack; // 引き分けの場合（デフォルトで黒を返す）
end;

procedure TReversiGame.UpdateCounts;
var
  X, Y: Integer;
begin
  FBlackCount := 0;
  FWhiteCount := 0;
  
  for X := 0 to 7 do
    for Y := 0 to 7 do
    begin
      if FBoard[X, Y] = Black then
        Inc(FBlackCount)
      else if FBoard[X, Y] = White then
        Inc(FWhiteCount);
    end;
end;

procedure TReversiGame.Reset;
begin
  InitializeBoard;
  FCurrentPlayer := PlayerBlack;
  FGameOver := False;
  UpdateCounts;
end;

end.

