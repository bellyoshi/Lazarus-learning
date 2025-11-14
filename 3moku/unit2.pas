unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // マスの状態
  TCellState = (csEmpty, csX, csO);
  
  // プレイヤー
  TPlayer = (pX, pO);
  
  // ゲームの状態
  TGameState = (gsPlaying, gsXWon, gsOWon, gsDraw);
  
  // ゲームボード（3x3）
  TBoard = array[0..2, 0..2] of TCellState;
  
  { TThreeMoku }
  TThreeMoku = class
  private
    FBoard: TBoard;
    FCurrentPlayer: TPlayer;
    FGameState: TGameState;
    FMoveCount: Integer;
    
    procedure InitializeBoard;
    function CheckWin: Boolean;
    function CheckDraw: Boolean;
    function GetCellState(Row, Col: Integer): TCellState;
    procedure SetCellState(Row, Col: Integer; Value: TCellState);
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // ゲーム操作
    function MakeMove(Row, Col: Integer): Boolean;
    procedure Reset;
    
    // プロパティ
    property Board[Row, Col: Integer]: TCellState read GetCellState write SetCellState;
    property CurrentPlayer: TPlayer read FCurrentPlayer;
    property GameState: TGameState read FGameState;
    property MoveCount: Integer read FMoveCount;
    
    // ユーティリティ
    function IsValidMove(Row, Col: Integer): Boolean;
    function GetPlayerSymbol(Player: TPlayer): String;
    function GetCurrentPlayerSymbol: String;
  end;

implementation

{ TThreeMoku }

constructor TThreeMoku.Create;
begin
  inherited Create;
  InitializeBoard;
  FCurrentPlayer := pX;
  FGameState := gsPlaying;
  FMoveCount := 0;
end;

destructor TThreeMoku.Destroy;
begin
  inherited Destroy;
end;

procedure TThreeMoku.InitializeBoard;
var
  i, j: Integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      FBoard[i, j] := csEmpty;
end;

function TThreeMoku.GetCellState(Row, Col: Integer): TCellState;
begin
  if (Row >= 0) and (Row <= 2) and (Col >= 0) and (Col <= 2) then
    Result := FBoard[Row, Col]
  else
    Result := csEmpty;
end;

procedure TThreeMoku.SetCellState(Row, Col: Integer; Value: TCellState);
begin
  if (Row >= 0) and (Row <= 2) and (Col >= 0) and (Col <= 2) then
    FBoard[Row, Col] := Value;
end;

function TThreeMoku.IsValidMove(Row, Col: Integer): Boolean;
begin
  Result := (FGameState = gsPlaying) and
            (Row >= 0) and (Row <= 2) and
            (Col >= 0) and (Col <= 2) and
            (FBoard[Row, Col] = csEmpty);
end;

function TThreeMoku.MakeMove(Row, Col: Integer): Boolean;
begin
  Result := False;
  
  if not IsValidMove(Row, Col) then
    Exit;
  
  // マスに現在のプレイヤーのマークを配置
  if FCurrentPlayer = pX then
    FBoard[Row, Col] := csX
  else
    FBoard[Row, Col] := csO;
  
  Inc(FMoveCount);
  
  // 勝敗判定
  if CheckWin then
  begin
    if FCurrentPlayer = pX then
      FGameState := gsXWon
    else
      FGameState := gsOWon;
  end
  else if CheckDraw then
  begin
    FGameState := gsDraw;
  end;
  
  // プレイヤーを切り替え
  if FGameState = gsPlaying then
  begin
    if FCurrentPlayer = pX then
      FCurrentPlayer := pO
    else
      FCurrentPlayer := pX;
  end;
  
  Result := True;
end;

function TThreeMoku.CheckWin: Boolean;
var
  i: Integer;
  checkState: TCellState;
begin
  Result := False;
  
  // 現在のプレイヤーのマークを確認
  if FCurrentPlayer = pX then
    checkState := csX
  else
    checkState := csO;
  
  // 横の行をチェック
  for i := 0 to 2 do
  begin
    if (FBoard[i, 0] = checkState) and
       (FBoard[i, 1] = checkState) and
       (FBoard[i, 2] = checkState) then
    begin
      Result := True;
      Exit;
    end;
  end;
  
  // 縦の列をチェック
  for i := 0 to 2 do
  begin
    if (FBoard[0, i] = checkState) and
       (FBoard[1, i] = checkState) and
       (FBoard[2, i] = checkState) then
    begin
      Result := True;
      Exit;
    end;
  end;
  
  // 斜め（左上から右下）をチェック
  if (FBoard[0, 0] = checkState) and
     (FBoard[1, 1] = checkState) and
     (FBoard[2, 2] = checkState) then
  begin
    Result := True;
    Exit;
  end;
  
  // 斜め（右上から左下）をチェック
  if (FBoard[0, 2] = checkState) and
     (FBoard[1, 1] = checkState) and
     (FBoard[2, 0] = checkState) then
  begin
    Result := True;
    Exit;
  end;
end;

function TThreeMoku.CheckDraw: Boolean;
begin
  Result := (FMoveCount >= 9) and (FGameState = gsPlaying);
end;

procedure TThreeMoku.Reset;
begin
  InitializeBoard;
  FCurrentPlayer := pX;
  FGameState := gsPlaying;
  FMoveCount := 0;
end;

function TThreeMoku.GetPlayerSymbol(Player: TPlayer): String;
begin
  if Player = pX then
    Result := 'X'
  else
    Result := 'O';
end;

function TThreeMoku.GetCurrentPlayerSymbol: String;
begin
  Result := GetPlayerSymbol(FCurrentPlayer);
end;

end.

