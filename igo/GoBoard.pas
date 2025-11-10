unit GoBoard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // 石の色
  TStoneColor = (scNone, scBlack, scWhite);
  
  // 座標
  TGoPoint = record
    X, Y: Integer;
  end;
  
  // 囲碁盤クラス
  TGoBoard = class
  private
    FSize: Integer;  // 盤のサイズ（通常19）
    FBoard: array of array of TStoneColor;  // 盤面
    FCurrentPlayer: TStoneColor;  // 現在の手番
    FLastMove: TGoPoint;  // 最後の着手（コウ判定用）
    FMoveCount: Integer;  // 着手数
    
    function IsValidPoint(const APoint: TGoPoint): Boolean;
    function GetStone(const APoint: TGoPoint): TStoneColor;
    procedure SetStone(const APoint: TGoPoint; AColor: TStoneColor);
    function GetOpponentColor(AColor: TStoneColor): TStoneColor;
    function HasLiberty(const APoint: TGoPoint; AColor: TStoneColor; 
      var Visited: array of Boolean): Boolean;
    procedure RemoveGroup(const APoint: TGoPoint; AColor: TStoneColor);
    function IsKo(const APoint: TGoPoint): Boolean;
    function IsSuicide(const APoint: TGoPoint; AColor: TStoneColor): Boolean;
    
  public
    constructor Create(ASize: Integer = 19);
    destructor Destroy; override;
    
    // プロパティ
    property Size: Integer read FSize;
    property CurrentPlayer: TStoneColor read FCurrentPlayer;
    property MoveCount: Integer read FMoveCount;
    
    // メソッド
    function PlaceStone(const APoint: TGoPoint): Boolean;  // 石を置く
    function CanPlaceStone(const APoint: TGoPoint): Boolean;  // 石を置けるか判定
    function GetStoneAt(X, Y: Integer): TStoneColor;  // 指定位置の石を取得
    procedure Clear;  // 盤面をクリア
    procedure Pass;  // パス
    function IsEmpty(const APoint: TGoPoint): Boolean;  // 空きかどうか
    function GetLibertyCount(const APoint: TGoPoint): Integer;  // 呼吸点の数を取得
  end;

implementation

{ TGoBoard }

constructor TGoBoard.Create(ASize: Integer = 19);
begin
  inherited Create;
  FSize := ASize;
  SetLength(FBoard, FSize, FSize);
  Clear;
end;

destructor TGoBoard.Destroy;
begin
  inherited Destroy;
end;

function TGoBoard.IsValidPoint(const APoint: TGoPoint): Boolean;
begin
  Result := (APoint.X >= 0) and (APoint.X < FSize) and
            (APoint.Y >= 0) and (APoint.Y < FSize);
end;

function TGoBoard.GetStone(const APoint: TGoPoint): TStoneColor;
begin
  if IsValidPoint(APoint) then
    Result := FBoard[APoint.X, APoint.Y]
  else
    Result := scNone;
end;

procedure TGoBoard.SetStone(const APoint: TGoPoint; AColor: TStoneColor);
begin
  if IsValidPoint(APoint) then
    FBoard[APoint.X, APoint.Y] := AColor;
end;

function TGoBoard.GetOpponentColor(AColor: TStoneColor): TStoneColor;
begin
  if AColor = scBlack then
    Result := scWhite
  else if AColor = scWhite then
    Result := scBlack
  else
    Result := scNone;
end;

function TGoBoard.HasLiberty(const APoint: TGoPoint; AColor: TStoneColor;
  var Visited: array of Boolean): Boolean;
var
  Neighbors: array[0..3] of TGoPoint;
  i: Integer;
  Neighbor: TGoPoint;
  Index: Integer;
begin
  if not IsValidPoint(APoint) then
  begin
    Result := False;
    Exit;
  end;
  
  Index := APoint.X * FSize + APoint.Y;
  if Visited[Index] then
  begin
    Result := False;
    Exit;
  end;
  
  Visited[Index] := True;
  
  // 空き点があれば呼吸点あり
  if GetStone(APoint) = scNone then
  begin
    Result := True;
    Exit;
  end;
  
  // 異なる色の石なら呼吸点なし
  if GetStone(APoint) <> AColor then
  begin
    Result := False;
    Exit;
  end;
  
  // 隣接点をチェック
  Neighbors[0].X := APoint.X - 1; Neighbors[0].Y := APoint.Y;
  Neighbors[1].X := APoint.X + 1; Neighbors[1].Y := APoint.Y;
  Neighbors[2].X := APoint.X; Neighbors[2].Y := APoint.Y - 1;
  Neighbors[3].X := APoint.X; Neighbors[3].Y := APoint.Y + 1;
  
  Result := False;
  for i := 0 to 3 do
  begin
    Neighbor := Neighbors[i];
    if HasLiberty(Neighbor, AColor, Visited) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TGoBoard.RemoveGroup(const APoint: TGoPoint; AColor: TStoneColor);
var
  Neighbors: array[0..3] of TGoPoint;
  i: Integer;
  Neighbor: TGoPoint;
begin
  if not IsValidPoint(APoint) or (GetStone(APoint) <> AColor) then
    Exit;
  
  SetStone(APoint, scNone);
  
  Neighbors[0].X := APoint.X - 1; Neighbors[0].Y := APoint.Y;
  Neighbors[1].X := APoint.X + 1; Neighbors[1].Y := APoint.Y;
  Neighbors[2].X := APoint.X; Neighbors[2].Y := APoint.Y - 1;
  Neighbors[3].X := APoint.X; Neighbors[3].Y := APoint.Y + 1;
  
  for i := 0 to 3 do
  begin
    Neighbor := Neighbors[i];
    if GetStone(Neighbor) = AColor then
      RemoveGroup(Neighbor, AColor);
  end;
end;

function TGoBoard.IsKo(const APoint: TGoPoint): Boolean;
begin
  // コウの判定（簡易版：直前の着手と同じ位置に打てない）
  Result := (FMoveCount > 0) and 
            (APoint.X = FLastMove.X) and 
            (APoint.Y = FLastMove.Y);
end;

function TGoBoard.IsSuicide(const APoint: TGoPoint; AColor: TStoneColor): Boolean;
var
  Visited: array of Boolean;
  i, j: Integer;
  Neighbors: array[0..3] of TGoPoint;
  Neighbor: TGoPoint;
  OpponentColor: TStoneColor;
  HasOpponentGroup: Boolean;
  k: Integer;
  Index: Integer;
begin
  // 既に石がある場合は自殺手ではない（そもそも打てない）
  if not IsEmpty(APoint) then
  begin
    Result := False;
    Exit;
  end;
  
  // 一時的に石を置いて判定
  SetStone(APoint, AColor);
  
  // 訪問済み配列を初期化
  SetLength(Visited, FSize * FSize);
  for i := 0 to FSize * FSize - 1 do
    Visited[i] := False;
  
  // 自分の石に呼吸点があるかチェック
  Result := not HasLiberty(APoint, AColor, Visited);
  
  // 相手の石を取れるかチェック
  OpponentColor := GetOpponentColor(AColor);
  HasOpponentGroup := False;
  
  Neighbors[0].X := APoint.X - 1; Neighbors[0].Y := APoint.Y;
  Neighbors[1].X := APoint.X + 1; Neighbors[1].Y := APoint.Y;
  Neighbors[2].X := APoint.X; Neighbors[2].Y := APoint.Y - 1;
  Neighbors[3].X := APoint.X; Neighbors[3].Y := APoint.Y + 1;
  
  for k := 0 to 3 do
  begin
    Neighbor := Neighbors[k];
    if GetStone(Neighbor) = OpponentColor then
    begin
      HasOpponentGroup := True;
      // 訪問済み配列をリセット
      for i := 0 to FSize * FSize - 1 do
        Visited[i] := False;
      
      if HasLiberty(Neighbor, OpponentColor, Visited) then
      begin
        // 相手の石に呼吸点がある場合は取れない
        Result := Result and True;  // 自殺手の可能性が残る
      end
      else
      begin
        // 相手の石を取れる場合は自殺手ではない
        Result := False;
        Break;
      end;
    end;
  end;
  
  // 石を元に戻す
  SetStone(APoint, scNone);
end;

function TGoBoard.PlaceStone(const APoint: TGoPoint): Boolean;
var
  OpponentColor: TStoneColor;
  Neighbors: array[0..3] of TGoPoint;
  i: Integer;
  Neighbor: TGoPoint;
  Visited: array of Boolean;
  j: Integer;
begin
  Result := False;
  
  // 有効な位置かチェック
  if not IsValidPoint(APoint) then
    Exit;
  
  // 既に石がある場合は打てない
  if not IsEmpty(APoint) then
    Exit;
  
  // コウのチェック
  if IsKo(APoint) then
    Exit;
  
  // 自殺手のチェック
  if IsSuicide(APoint, FCurrentPlayer) then
    Exit;
  
  // 石を置く
  SetStone(APoint, FCurrentPlayer);
  
  // 相手の石を取り除く
  OpponentColor := GetOpponentColor(FCurrentPlayer);
  Neighbors[0].X := APoint.X - 1; Neighbors[0].Y := APoint.Y;
  Neighbors[1].X := APoint.X + 1; Neighbors[1].Y := APoint.Y;
  Neighbors[2].X := APoint.X; Neighbors[2].Y := APoint.Y - 1;
  Neighbors[3].X := APoint.X; Neighbors[3].Y := APoint.Y + 1;
  
  for i := 0 to 3 do
  begin
    Neighbor := Neighbors[i];
    if GetStone(Neighbor) = OpponentColor then
    begin
      // 訪問済み配列を初期化
      SetLength(Visited, FSize * FSize);
      for j := 0 to FSize * FSize - 1 do
        Visited[j] := False;
      
      // 呼吸点がない場合は取り除く
      if not HasLiberty(Neighbor, OpponentColor, Visited) then
        RemoveGroup(Neighbor, OpponentColor);
    end;
  end;
  
  // 手番を変更
  FCurrentPlayer := GetOpponentColor(FCurrentPlayer);
  FLastMove := APoint;
  Inc(FMoveCount);
  
  Result := True;
end;

function TGoBoard.CanPlaceStone(const APoint: TGoPoint): Boolean;
begin
  Result := IsValidPoint(APoint) and 
            IsEmpty(APoint) and 
            not IsKo(APoint) and 
            not IsSuicide(APoint, FCurrentPlayer);
end;

function TGoBoard.GetStoneAt(X, Y: Integer): TStoneColor;
var
  Point: TGoPoint;
begin
  Point.X := X;
  Point.Y := Y;
  Result := GetStone(Point);
end;

procedure TGoBoard.Clear;
var
  i, j: Integer;
begin
  for i := 0 to FSize - 1 do
    for j := 0 to FSize - 1 do
      FBoard[i, j] := scNone;
  
  FCurrentPlayer := scBlack;
  FLastMove.X := -1;
  FLastMove.Y := -1;
  FMoveCount := 0;
end;

procedure TGoBoard.Pass;
begin
  FCurrentPlayer := GetOpponentColor(FCurrentPlayer);
  FLastMove.X := -1;
  FLastMove.Y := -1;
  Inc(FMoveCount);
end;

function TGoBoard.IsEmpty(const APoint: TGoPoint): Boolean;
begin
  Result := GetStone(APoint) = scNone;
end;

function TGoBoard.GetLibertyCount(const APoint: TGoPoint): Integer;
var
  Neighbors: array[0..3] of TGoPoint;
  k: Integer;
  Neighbor: TGoPoint;
  Color: TStoneColor;
begin
  Result := 0;
  
  if not IsValidPoint(APoint) then
    Exit;
  
  Color := GetStone(APoint);
  if Color = scNone then
    Exit;
  
  // 隣接点の空き点を数える
  Neighbors[0].X := APoint.X - 1; Neighbors[0].Y := APoint.Y;
  Neighbors[1].X := APoint.X + 1; Neighbors[1].Y := APoint.Y;
  Neighbors[2].X := APoint.X; Neighbors[2].Y := APoint.Y - 1;
  Neighbors[3].X := APoint.X; Neighbors[3].Y := APoint.Y + 1;
  
  for k := 0 to 3 do
  begin
    Neighbor := Neighbors[k];
    if IsValidPoint(Neighbor) and (GetStone(Neighbor) = scNone) then
      Inc(Result);
  end;
end;

end.

