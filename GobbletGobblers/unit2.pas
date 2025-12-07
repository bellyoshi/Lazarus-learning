unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // 駒のサイズ
  TPieceSize = (psSmall, psMedium, psLarge);
  
  // プレイヤー
  TPlayer = (pNone, pPlayer1, pPlayer2);
  
  // 駒の情報
  TPiece = record
    Owner: TPlayer;      // 所有者
    Size: TPieceSize;    // サイズ
  end;
  
  // 盤面の位置
  TBoardPosition = record
    X: Integer;  // 0-2
    Y: Integer;  // 0-2
  end;
  
  // 盤面のセル（スタック構造：最大3つの駒を積み重ね可能）
  TCell = array[0..2] of TPiece;
  
  // ゲーム状態
  TGameState = (gsPlaying, gsPlayer1Wins, gsPlayer2Wins, gsDraw);
  
  // ゴブレット・ゴブラーズ ゲームクラス
  TGobbletGobblers = class
  private
    FBoard: array[0..2, 0..2] of TCell;  // 3x3の盤面
    FCurrentPlayer: TPlayer;              // 現在のプレイヤー
    FGameState: TGameState;               // ゲーム状態
    FPlayer1Pieces: array[TPieceSize, 0..1] of Boolean;  // プレイヤー1の駒の使用状況
    FPlayer2Pieces: array[TPieceSize, 0..1] of Boolean;  // プレイヤー2の駒の使用状況
    
    function GetTopPiece(X, Y: Integer): TPiece;
    function GetTopPieceIndex(X, Y: Integer): Integer;
    function CheckWinCondition(Player: TPlayer): Boolean;
    function CheckLine(X1, Y1, X2, Y2, X3, Y3: Integer; Player: TPlayer): Boolean;
    function IsBoardFull: Boolean;
    
  public
    constructor Create;
    procedure Reset;
    
    // 駒の配置
    function PlacePiece(X, Y: Integer; Size: TPieceSize; Player: TPlayer): Boolean;
    
    // 駒の移動（既に盤面にある駒を別の位置に移動）
    function MovePiece(FromX, FromY, ToX, ToY: Integer; Player: TPlayer): Boolean;
    
    // 駒の取得（盤面から駒を取り除く）
    function RemovePiece(X, Y: Integer; Player: TPlayer): Boolean;
    
    // 勝敗判定
    function CheckGameOver: TGameState;
    
    // プロパティ
    property CurrentPlayer: TPlayer read FCurrentPlayer;
    property GameState: TGameState read FGameState;
    
    // 盤面の状態を取得
    function GetCell(X, Y: Integer): TCell;
    function GetTopPieceAt(X, Y: Integer): TPiece;
    
    // プレイヤーの残り駒を取得
    function GetAvailablePieces(Player: TPlayer; Size: TPieceSize): Integer;
    
    // 次のプレイヤーに交代
    procedure SwitchPlayer;
  end;

implementation

constructor TGobbletGobblers.Create;
begin
  Reset;
end;

procedure TGobbletGobblers.Reset;
var
  X, Y, I: Integer;
  Size: TPieceSize;
begin
  // 盤面をクリア
  for X := 0 to 2 do
    for Y := 0 to 2 do
      for I := 0 to 2 do
      begin
        FBoard[X, Y][I].Owner := pNone;
        FBoard[X, Y][I].Size := psSmall;
      end;
  
  // プレイヤーの駒をリセット（各サイズ2つずつ）
  for Size := Low(TPieceSize) to High(TPieceSize) do
  begin
    FPlayer1Pieces[Size, 0] := False;  // 未使用
    FPlayer1Pieces[Size, 1] := False;
    FPlayer2Pieces[Size, 0] := False;
    FPlayer2Pieces[Size, 1] := False;
  end;
  
  FCurrentPlayer := pPlayer1;
  FGameState := gsPlaying;
end;

function TGobbletGobblers.GetTopPiece(X, Y: Integer): TPiece;
var
  Index: Integer;
begin
  Index := GetTopPieceIndex(X, Y);
  if Index >= 0 then
    Result := FBoard[X, Y][Index]
  else
  begin
    Result.Owner := pNone;
    Result.Size := psSmall;
  end;
end;

function TGobbletGobblers.GetTopPieceIndex(X, Y: Integer): Integer;
var
  I: Integer;
begin
  // 上から下に検索して、最初に見つかった駒のインデックスを返す
  for I := 2 downto 0 do
    if FBoard[X, Y][I].Owner <> pNone then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;  // 空のセル
end;

function TGobbletGobblers.PlacePiece(X, Y: Integer; Size: TPieceSize; Player: TPlayer): Boolean;
var
  TopIndex: Integer;
  TopPiece: TPiece;
  PieceIndex: Integer;
  I: Integer;
begin
  Result := False;
  
  // バリデーション
  if (X < 0) or (X > 2) or (Y < 0) or (Y > 2) then
    Exit;
  
  if FGameState <> gsPlaying then
    Exit;
  
  if Player <> FCurrentPlayer then
    Exit;
  
  // 使用可能な駒があるかチェック
  if Player = pPlayer1 then
  begin
    PieceIndex := -1;
    for I := 0 to 1 do
      if not FPlayer1Pieces[Size, I] then
      begin
        PieceIndex := I;
        Break;
      end;
    if PieceIndex = -1 then
      Exit;  // 使用可能な駒がない
  end
  else
  begin
    PieceIndex := -1;
    for I := 0 to 1 do
      if not FPlayer2Pieces[Size, I] then
      begin
        PieceIndex := I;
        Break;
      end;
    if PieceIndex = -1 then
      Exit;  // 使用可能な駒がない
  end;
  
  // セルの最上部の駒を取得
  TopIndex := GetTopPieceIndex(X, Y);
  
  if TopIndex >= 0 then
  begin
    TopPiece := FBoard[X, Y][TopIndex];
    // 既存の駒の上に置く場合、自分の駒でなければならない、かつ大きい駒でなければならない
    if (TopPiece.Owner <> Player) and (Size <= TopPiece.Size) then
      Exit;  // 相手の駒の上に小さい駒は置けない
  end;
  
  // 駒を配置
  if TopIndex < 2 then
  begin
    FBoard[X, Y][TopIndex + 1].Owner := Player;
    FBoard[X, Y][TopIndex + 1].Size := Size;
    
    // 駒を使用済みにマーク
    if Player = pPlayer1 then
      FPlayer1Pieces[Size, PieceIndex] := True
    else
      FPlayer2Pieces[Size, PieceIndex] := True;
    
    Result := True;
    
    // 勝敗判定
    FGameState := CheckGameOver;
  end;
end;

function TGobbletGobblers.MovePiece(FromX, FromY, ToX, ToY: Integer; Player: TPlayer): Boolean;
var
  FromIndex, ToIndex: Integer;
  Piece: TPiece;
  I: Integer;
begin
  Result := False;
  
  // バリデーション
  if (FromX < 0) or (FromX > 2) or (FromY < 0) or (FromY > 2) then
    Exit;
  if (ToX < 0) or (ToX > 2) or (ToY < 0) or (ToY > 2) then
    Exit;
  
  if (FromX = ToX) and (FromY = ToY) then
    Exit;  // 同じ位置への移動は不可
  
  if FGameState <> gsPlaying then
    Exit;
  
  if Player <> FCurrentPlayer then
    Exit;
  
  // 移動元の最上部の駒を取得
  FromIndex := GetTopPieceIndex(FromX, FromY);
  if FromIndex < 0 then
    Exit;  // 移動元に駒がない
  
  Piece := FBoard[FromX, FromY][FromIndex];
  if Piece.Owner <> Player then
    Exit;  // 自分の駒でない
  
  // 移動先の最上部の駒をチェック
  ToIndex := GetTopPieceIndex(ToX, ToY);
  if ToIndex >= 0 then
  begin
    // 既存の駒の上に置く場合、大きい駒でなければならない
    if Piece.Size <= FBoard[ToX, ToY][ToIndex].Size then
      Exit;  // 小さい駒の上には置けない
  end;
  
  // 駒を移動
  // 移動元から削除
  FBoard[FromX, FromY][FromIndex].Owner := pNone;
  
  // 移動先に配置
  if ToIndex < 2 then
  begin
    FBoard[ToX, ToY][ToIndex + 1] := Piece;
    Result := True;
    
    // 勝敗判定
    FGameState := CheckGameOver;
  end
  else
  begin
    // スタックが満杯の場合は移動できない
    FBoard[FromX, FromY][FromIndex] := Piece;  // 元に戻す
  end;
end;

function TGobbletGobblers.RemovePiece(X, Y: Integer; Player: TPlayer): Boolean;
var
  Index: Integer;
  Piece: TPiece;
  Size: TPieceSize;
  I: Integer;
begin
  Result := False;
  
  // バリデーション
  if (X < 0) or (X > 2) or (Y < 0) or (Y > 2) then
    Exit;
  
  if FGameState <> gsPlaying then
    Exit;
  
  if Player <> FCurrentPlayer then
    Exit;
  
  // 最上部の駒を取得
  Index := GetTopPieceIndex(X, Y);
  if Index < 0 then
    Exit;  // 駒がない
  
  Piece := FBoard[X, Y][Index];
  if Piece.Owner <> Player then
    Exit;  // 自分の駒でない
  
  Size := Piece.Size;
  
  // 駒を削除
  FBoard[X, Y][Index].Owner := pNone;
  
  // 駒を未使用に戻す
  if Player = pPlayer1 then
  begin
    for I := 0 to 1 do
      if FPlayer1Pieces[Size, I] then
      begin
        FPlayer1Pieces[Size, I] := False;
        Break;
      end;
  end
  else
  begin
    for I := 0 to 1 do
      if FPlayer2Pieces[Size, I] then
      begin
        FPlayer2Pieces[Size, I] := False;
        Break;
      end;
  end;
  
  Result := True;
end;

function TGobbletGobblers.CheckWinCondition(Player: TPlayer): Boolean;
begin
  // 縦・横・斜めの8通りをチェック
  Result := CheckLine(0, 0, 0, 1, 0, 2, Player) or  // 縦1列目
            CheckLine(1, 0, 1, 1, 1, 2, Player) or  // 縦2列目
            CheckLine(2, 0, 2, 1, 2, 2, Player) or  // 縦3列目
            CheckLine(0, 0, 1, 0, 2, 0, Player) or  // 横1行目
            CheckLine(0, 1, 1, 1, 2, 1, Player) or  // 横2行目
            CheckLine(0, 2, 1, 2, 2, 2, Player) or  // 横3行目
            CheckLine(0, 0, 1, 1, 2, 2, Player) or  // 斜め（左上→右下）
            CheckLine(0, 2, 1, 1, 2, 0, Player);     // 斜め（左下→右上）
end;

function TGobbletGobblers.CheckLine(X1, Y1, X2, Y2, X3, Y3: Integer; Player: TPlayer): Boolean;
var
  P1, P2, P3: TPiece;
begin
  P1 := GetTopPiece(X1, Y1);
  P2 := GetTopPiece(X2, Y2);
  P3 := GetTopPiece(X3, Y3);
  
  Result := (P1.Owner = Player) and (P2.Owner = Player) and (P3.Owner = Player);
end;

function TGobbletGobblers.IsBoardFull: Boolean;
var
  X, Y: Integer;
begin
  Result := True;
  for X := 0 to 2 do
    for Y := 0 to 2 do
      if GetTopPieceIndex(X, Y) < 0 then
      begin
        Result := False;
        Exit;
      end;
end;

function TGobbletGobblers.CheckGameOver: TGameState;
begin
  if CheckWinCondition(pPlayer1) then
    Result := gsPlayer1Wins
  else if CheckWinCondition(pPlayer2) then
    Result := gsPlayer2Wins
  else if IsBoardFull then
    Result := gsDraw
  else
    Result := gsPlaying;
end;

function TGobbletGobblers.GetCell(X, Y: Integer): TCell;
var
  I: Integer;
begin
  if (X >= 0) and (X <= 2) and (Y >= 0) and (Y <= 2) then
    Result := FBoard[X, Y]
  else
  begin
    // 無効な位置の場合は空のセルを返す
    for I := 0 to 2 do
    begin
      Result[I].Owner := pNone;
      Result[I].Size := psSmall;
    end;
  end;
end;

function TGobbletGobblers.GetTopPieceAt(X, Y: Integer): TPiece;
begin
  Result := GetTopPiece(X, Y);
end;

function TGobbletGobblers.GetAvailablePieces(Player: TPlayer; Size: TPieceSize): Integer;
var
  I: Integer;
begin
  Result := 0;
  if Player = pPlayer1 then
  begin
    for I := 0 to 1 do
      if not FPlayer1Pieces[Size, I] then
        Inc(Result);
  end
  else
  begin
    for I := 0 to 1 do
      if not FPlayer2Pieces[Size, I] then
        Inc(Result);
  end;
end;

procedure TGobbletGobblers.SwitchPlayer;
begin
  if FCurrentPlayer = pPlayer1 then
    FCurrentPlayer := pPlayer2
  else
    FCurrentPlayer := pPlayer1;
end;

end.

