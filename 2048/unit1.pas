unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls, ExtCtrls;

type
  TGameBoard = array[0..3, 0..3] of Integer;

  TForm1 = class(TForm)
    GameGrid: TStringGrid;
    ScoreLabel: TLabel;
    TitleLabel: TLabel;
    RestartButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GameGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure GameGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RestartButtonClick(Sender: TObject);
  private
    Board: TGameBoard;
    Score: Integer;
    procedure InitializeBoard;
    procedure AddRandomTile;
    function IsBoardFull: Boolean;
    function CanMove: Boolean;
    function MoveLeft: Boolean;
    function MoveRight: Boolean;
    function MoveUp: Boolean;
    function MoveDown: Boolean;
    procedure UpdateDisplay;
    function GetTileColor(Value: Integer): TColor;
    function GetTileTextColor(Value: Integer): TColor;
    procedure HandleKeyDown(var Key: Word; Shift: TShiftState);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Score := 0;
  InitializeBoard;
  UpdateDisplay;
  GameGrid.Options := GameGrid.Options - [goEditing];
  GameGrid.DefaultColWidth := 80;
  GameGrid.DefaultRowHeight := 80;
  GameGrid.ColCount := 4;
  GameGrid.RowCount := 4;
  GameGrid.Font.Size := 18;
  GameGrid.Font.Style := [fsBold];
  TitleLabel.Caption := '2048';
  ScoreLabel.Caption := 'スコア: 0';
  // キーボードイベントを受け取るための設定
  KeyPreview := True;
  GameGrid.TabStop := True;
end;

procedure TForm1.InitializeBoard;
var
  i, j: Integer;
begin
  // ボードを0で初期化
  for i := 0 to 3 do
    for j := 0 to 3 do
      Board[i, j] := 0;
  
  // 2つのランダムなタイルを追加
  AddRandomTile;
  AddRandomTile;
end;

procedure TForm1.AddRandomTile;
var
  EmptyCells: array[0..15] of record x, y: Integer; end;
  EmptyCount, i, j, RandomIndex, Value: Integer;
begin
  EmptyCount := 0;
  
  // 空のセルを探す
  for i := 0 to 3 do
    for j := 0 to 3 do
      if Board[i, j] = 0 then
      begin
        EmptyCells[EmptyCount].x := i;
        EmptyCells[EmptyCount].y := j;
        Inc(EmptyCount);
      end;
  
  if EmptyCount = 0 then
    Exit;
  
  // ランダムな空のセルを選ぶ
  RandomIndex := Random(EmptyCount);
  
  // 90%の確率で2、10%の確率で4を追加
  if Random(10) < 9 then
    Value := 2
  else
    Value := 4;
  
  Board[EmptyCells[RandomIndex].x, EmptyCells[RandomIndex].y] := Value;
end;

function TForm1.IsBoardFull: Boolean;
var
  i, j: Integer;
begin
  Result := True;
  for i := 0 to 3 do
    for j := 0 to 3 do
      if Board[i, j] = 0 then
      begin
        Result := False;
        Exit;
      end;
end;

function TForm1.CanMove: Boolean;
var
  i, j: Integer;
begin
  // 空のセルがあるかチェック
  if not IsBoardFull then
  begin
    Result := True;
    Exit;
  end;
  
  // 隣接するセルで同じ値があるかチェック
  for i := 0 to 3 do
    for j := 0 to 3 do
    begin
      // 右隣
      if (j < 3) and (Board[i, j] = Board[i, j + 1]) then
      begin
        Result := True;
        Exit;
      end;
      // 下隣
      if (i < 3) and (Board[i, j] = Board[i + 1, j]) then
      begin
        Result := True;
        Exit;
      end;
    end;
  
  Result := False;
end;

function TForm1.MoveLeft: Boolean;
var
  i, j, k: Integer;
  Merged: array[0..3] of Boolean;
  NewRow: array[0..3] of Integer;
  HasChanged: Boolean;
begin
  Result := False;
  HasChanged := False;
  
  for i := 0 to 3 do
  begin
    // マージフラグをリセット
    for j := 0 to 3 do
      Merged[j] := False;
    
    // 新しい行を初期化
    for j := 0 to 3 do
      NewRow[j] := 0;
    
    // 新しい行を構築
    k := 0;
    for j := 0 to 3 do
    begin
      if Board[i, j] <> 0 then
      begin
        if (k > 0) and (NewRow[k - 1] = Board[i, j]) and not Merged[k - 1] then
        begin
          // マージ
          NewRow[k - 1] := NewRow[k - 1] * 2;
          Score := Score + NewRow[k - 1];
          Merged[k - 1] := True;
          HasChanged := True;
        end
        else
        begin
          NewRow[k] := Board[i, j];
          if k <> j then
            HasChanged := True;
          Inc(k);
        end;
      end;
    end;
    
    // ボードを更新
    for j := 0 to 3 do
      Board[i, j] := NewRow[j];
  end;
  
  Result := HasChanged;
end;

function TForm1.MoveRight: Boolean;
var
  i, j, k: Integer;
  Merged: array[0..3] of Boolean;
  NewRow: array[0..3] of Integer;
  HasChanged: Boolean;
begin
  Result := False;
  HasChanged := False;
  
  for i := 0 to 3 do
  begin
    // マージフラグをリセット
    for j := 0 to 3 do
      Merged[j] := False;
    
    // 新しい行を初期化
    for j := 0 to 3 do
      NewRow[j] := 0;
    
    // 新しい行を構築（右から）
    k := 3;
    for j := 3 downto 0 do
    begin
      if Board[i, j] <> 0 then
      begin
        if (k < 3) and (NewRow[k + 1] = Board[i, j]) and not Merged[k + 1] then
        begin
          // マージ
          NewRow[k + 1] := NewRow[k + 1] * 2;
          Score := Score + NewRow[k + 1];
          Merged[k + 1] := True;
          HasChanged := True;
        end
        else
        begin
          NewRow[k] := Board[i, j];
          if k <> j then
            HasChanged := True;
          Dec(k);
        end;
      end;
    end;
    
    // ボードを更新
    for j := 0 to 3 do
      Board[i, j] := NewRow[j];
  end;
  
  Result := HasChanged;
end;

function TForm1.MoveUp: Boolean;
var
  i, j, k: Integer;
  Merged: array[0..3] of Boolean;
  NewCol: array[0..3] of Integer;
  HasChanged: Boolean;
begin
  Result := False;
  HasChanged := False;
  
  for j := 0 to 3 do
  begin
    // マージフラグをリセット
    for i := 0 to 3 do
      Merged[i] := False;
    
    // 新しい列を初期化
    for i := 0 to 3 do
      NewCol[i] := 0;
    
    // 新しい列を構築
    k := 0;
    for i := 0 to 3 do
    begin
      if Board[i, j] <> 0 then
      begin
        if (k > 0) and (NewCol[k - 1] = Board[i, j]) and not Merged[k - 1] then
        begin
          // マージ
          NewCol[k - 1] := NewCol[k - 1] * 2;
          Score := Score + NewCol[k - 1];
          Merged[k - 1] := True;
          HasChanged := True;
        end
        else
        begin
          NewCol[k] := Board[i, j];
          if k <> i then
            HasChanged := True;
          Inc(k);
        end;
      end;
    end;
    
    // ボードを更新
    for i := 0 to 3 do
      Board[i, j] := NewCol[i];
  end;
  
  Result := HasChanged;
end;

function TForm1.MoveDown: Boolean;
var
  i, j, k: Integer;
  Merged: array[0..3] of Boolean;
  NewCol: array[0..3] of Integer;
  HasChanged: Boolean;
begin
  Result := False;
  HasChanged := False;
  
  for j := 0 to 3 do
  begin
    // マージフラグをリセット
    for i := 0 to 3 do
      Merged[i] := False;
    
    // 新しい列を初期化
    for i := 0 to 3 do
      NewCol[i] := 0;
    
    // 新しい列を構築（下から）
    k := 3;
    for i := 3 downto 0 do
    begin
      if Board[i, j] <> 0 then
      begin
        if (k < 3) and (NewCol[k + 1] = Board[i, j]) and not Merged[k + 1] then
        begin
          // マージ
          NewCol[k + 1] := NewCol[k + 1] * 2;
          Score := Score + NewCol[k + 1];
          Merged[k + 1] := True;
          HasChanged := True;
        end
        else
        begin
          NewCol[k] := Board[i, j];
          if k <> i then
            HasChanged := True;
          Dec(k);
        end;
      end;
    end;
    
    // ボードを更新
    for i := 0 to 3 do
      Board[i, j] := NewCol[i];
  end;
  
  Result := HasChanged;
end;

procedure TForm1.UpdateDisplay;
var
  i, j: Integer;
  Has2048: Boolean;
begin
  Has2048 := False;
  
  for i := 0 to 3 do
    for j := 0 to 3 do
    begin
      if Board[i, j] = 0 then
        GameGrid.Cells[j, i] := ''
      else
        GameGrid.Cells[j, i] := IntToStr(Board[i, j]);
      
      if Board[i, j] = 2048 then
        Has2048 := True;
    end;
  
  ScoreLabel.Caption := 'スコア: ' + IntToStr(Score);
  GameGrid.Invalidate;
  
  // 勝利チェック
  if Has2048 then
  begin
    ShowMessage('おめでとうございます！2048に到達しました！');
  end
  // ゲームオーバーチェック
  else if not CanMove then
  begin
    ShowMessage('ゲームオーバー！もう動かせません。');
  end;
end;

function TForm1.GetTileColor(Value: Integer): TColor;
begin
  case Value of
    0: Result := $00EEEEEE;      // 薄いグレー
    2: Result := $00EEEEEE;      // 薄いグレー
    4: Result := $00EEEDE0;      // 薄いベージュ
    8: Result := $00EDE0C8;      // ベージュ
    16: Result := $00EDCC61;     // 黄色
    32: Result := $00EDC850;     // オレンジ
    64: Result := $00EDC53F;     // オレンジ
    128: Result := $00EDC22E;    // オレンジ
    256: Result := $00EDC22E;    // オレンジ
    512: Result := $00EDC22E;    // オレンジ
    1024: Result := $00EDC22E;   // オレンジ
    2048: Result := $00EDC22E;   // オレンジ
    else Result := $00000000;    // 黒
  end;
end;

function TForm1.GetTileTextColor(Value: Integer): TColor;
begin
  if Value <= 4 then
    Result := $00776655  // ダークブラウン
  else
    Result := $00F9F6F2; // 白
end;

procedure TForm1.GameGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  Value: Integer;
  CellText: String;
begin
  Value := Board[aRow, aCol];
  
  // 背景色を設定
  GameGrid.Canvas.Brush.Color := GetTileColor(Value);
  GameGrid.Canvas.FillRect(aRect);
  
  // テキスト色を設定
  GameGrid.Canvas.Font.Color := GetTileTextColor(Value);
  
  // テキストを描画
  if Value <> 0 then
  begin
    CellText := IntToStr(Value);
    GameGrid.Canvas.TextRect(aRect, 
      aRect.Left + (aRect.Width - GameGrid.Canvas.TextWidth(CellText)) div 2,
      aRect.Top + (aRect.Height - GameGrid.Canvas.TextHeight(CellText)) div 2,
      CellText);
  end;
end;

procedure TForm1.HandleKeyDown(var Key: Word; Shift: TShiftState);
var
  Moved: Boolean;
begin
  Moved := False;
  
  case Key of
    37, 65: Moved := MoveLeft;   // 左矢印またはA
    38, 87: Moved := MoveUp;     // 上矢印またはW
    39, 68: Moved := MoveRight;  // 右矢印またはD
    40, 83: Moved := MoveDown;   // 下矢印またはS
  end;
  
  if Moved then
  begin
    AddRandomTile;
    UpdateDisplay;
    Key := 0; // イベントを消費
  end;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  HandleKeyDown(Key, Shift);
end;

procedure TForm1.GameGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  HandleKeyDown(Key, Shift);
end;

procedure TForm1.RestartButtonClick(Sender: TObject);
begin
  Score := 0;
  InitializeBoard;
  UpdateDisplay;
end;

end.

