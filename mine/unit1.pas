unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls;

type
  TCellState = (csClosed, csOpen, csFlagged);
  
  TCell = record
    IsMine: Boolean;
    State: TCellState;
    AdjacentMines: Integer;
  end;

  TForm1 = class(TForm)
    StringGrid1: TStringGrid;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure Button1Click(Sender: TObject);
  private
    Grid: array[0..9, 0..9] of TCell;
    MineCount: Integer;
    FlagCount: Integer;
    GameOver: Boolean;
    MinesPlaced: Boolean;
    procedure InitializeGame;
    procedure PlaceMines(FirstClickCol, FirstClickRow: Integer);
    procedure CalculateAdjacentMines;
    procedure OpenCell(Col, Row: Integer);
    procedure OpenAdjacentCells(Col, Row: Integer);
    procedure ToggleFlag(Col, Row: Integer);
    procedure UpdateDisplay;
    procedure CheckWinCondition;
    procedure GameLost;
    procedure GameWon;
    function IsValidCell(Col, Row: Integer): Boolean;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitializeGame;
end;

procedure TForm1.InitializeGame;
var
  i, j: Integer;
begin
  GameOver := False;
  MineCount := 10;
  FlagCount := 0;
  MinesPlaced := False;
  
  // グリッドを初期化
  for i := 0 to 9 do
    for j := 0 to 9 do
    begin
      Grid[i, j].IsMine := False;
      Grid[i, j].State := csClosed;
      Grid[i, j].AdjacentMines := 0;
    end;
  
  UpdateDisplay;
end;

procedure TForm1.PlaceMines(FirstClickCol, FirstClickRow: Integer);
var
  i, Col, Row: Integer;
  Placed: Integer;
begin
  Placed := 0;
  Randomize;
  
  while Placed < MineCount do
  begin
    Col := Random(10);
    Row := Random(10);
    
    // 最初にクリックしたセルとその周囲には地雷を置かない
    if (Abs(Col - FirstClickCol) <= 1) and (Abs(Row - FirstClickRow) <= 1) then
      Continue;
    
    if not Grid[Col, Row].IsMine then
    begin
      Grid[Col, Row].IsMine := True;
      Inc(Placed);
    end;
  end;
  
  CalculateAdjacentMines;
  MinesPlaced := True;
end;

procedure TForm1.CalculateAdjacentMines;
var
  i, j, di, dj: Integer;
  Count: Integer;
begin
  for i := 0 to 9 do
    for j := 0 to 9 do
    begin
      if Grid[i, j].IsMine then
        Continue;
      
      Count := 0;
      for di := -1 to 1 do
        for dj := -1 to 1 do
          if IsValidCell(i + di, j + dj) and Grid[i + di, j + dj].IsMine then
            Inc(Count);
      
      Grid[i, j].AdjacentMines := Count;
    end;
end;

procedure TForm1.OpenCell(Col, Row: Integer);
begin
  if not IsValidCell(Col, Row) then
    Exit;
  
  if Grid[Col, Row].State <> csClosed then
    Exit;
  
  if Grid[Col, Row].IsMine then
  begin
    GameLost;
    Exit;
  end;
  
  Grid[Col, Row].State := csOpen;
  
  // 周囲に地雷がない場合は自動的に周囲のセルを開く
  if Grid[Col, Row].AdjacentMines = 0 then
    OpenAdjacentCells(Col, Row);
  
  UpdateDisplay;
  CheckWinCondition;
end;

procedure TForm1.OpenAdjacentCells(Col, Row: Integer);
var
  di, dj: Integer;
begin
  for di := -1 to 1 do
    for dj := -1 to 1 do
      if (di <> 0) or (dj <> 0) then
        if IsValidCell(Col + di, Row + dj) then
          OpenCell(Col + di, Row + dj);
end;

procedure TForm1.ToggleFlag(Col, Row: Integer);
begin
  if not IsValidCell(Col, Row) then
    Exit;
  
  if Grid[Col, Row].State = csOpen then
    Exit;
  
  if Grid[Col, Row].State = csFlagged then
  begin
    Grid[Col, Row].State := csClosed;
    Dec(FlagCount);
  end
  else
  begin
    Grid[Col, Row].State := csFlagged;
    Inc(FlagCount);
  end;
  
  UpdateDisplay;
end;

procedure TForm1.UpdateDisplay;
var
  i, j: Integer;
begin
  for i := 0 to 9 do
    for j := 0 to 9 do
    begin
      case Grid[i, j].State of
        csClosed:
          StringGrid1.Cells[i, j] := '';
        csFlagged:
          StringGrid1.Cells[i, j] := '🚩';
        csOpen:
          if Grid[i, j].IsMine then
            StringGrid1.Cells[i, j] := '💣'
          else if Grid[i, j].AdjacentMines > 0 then
            StringGrid1.Cells[i, j] := IntToStr(Grid[i, j].AdjacentMines)
          else
            StringGrid1.Cells[i, j] := '';
      end;
    end;
  
  Label1.Caption := '地雷数: ' + IntToStr(MineCount);
  Label2.Caption := 'フラグ: ' + IntToStr(FlagCount);
  
  // グリッドを再描画
  StringGrid1.Invalidate;
end;

procedure TForm1.CheckWinCondition;
var
  i, j: Integer;
  OpenCount: Integer;
begin
  OpenCount := 0;
  
  for i := 0 to 9 do
    for j := 0 to 9 do
      if (Grid[i, j].State = csOpen) and (not Grid[i, j].IsMine) then
        Inc(OpenCount);
  
  // 90個のセル（100 - 10個の地雷）が開かれれば勝利
  if OpenCount = 90 then
    GameWon;
end;

procedure TForm1.GameLost;
var
  i, j: Integer;
begin
  GameOver := True;
  
  // 全ての地雷を表示
  for i := 0 to 9 do
    for j := 0 to 9 do
      if Grid[i, j].IsMine then
        Grid[i, j].State := csOpen;
  
  UpdateDisplay;
  ShowMessage('ゲームオーバー！地雷を踏んでしまいました。');
end;

procedure TForm1.GameWon;
begin
  GameOver := True;
  ShowMessage('おめでとうございます！全ての地雷を避けました！');
end;

function TForm1.IsValidCell(Col, Row: Integer): Boolean;
begin
  Result := (Col >= 0) and (Col < 10) and (Row >= 0) and (Row < 10);
end;

procedure TForm1.StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
begin
  if GameOver then
    Exit;
  
  StringGrid1.MouseToCell(X, Y, Col, Row);
  
  if not IsValidCell(Col, Row) then
    Exit;
  
  // 最初のクリックで地雷を配置（左クリックの場合のみ）
  if not MinesPlaced and (Button = mbLeft) then
  begin
    PlaceMines(Col, Row);
  end;
  
  if Button = mbLeft then
  begin
    if Grid[Col, Row].State <> csFlagged then
      OpenCell(Col, Row);
  end
  else if Button = mbRight then
  begin
    // 最初のクリックが右クリックの場合、地雷を配置してからフラグを立てる
    if not MinesPlaced then
    begin
      PlaceMines(Col, Row);
    end;
    ToggleFlag(Col, Row);
  end;
end;

procedure TForm1.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  CellText: String;
begin
  if not IsValidCell(aCol, aRow) then
    Exit;
  
  CellText := StringGrid1.Cells[aCol, aRow];
  
  // 開いたセル（安全圏）の背景色を変更
  if Grid[aCol, aRow].State = csOpen then
  begin
    StringGrid1.Canvas.Brush.Color := $E0E0E0; // 薄いグレー
    StringGrid1.Canvas.Font.Color := clBlack;
  end
  else if Grid[aCol, aRow].State = csFlagged then
  begin
    StringGrid1.Canvas.Brush.Color := $FFE0E0; // 薄いピンク
    StringGrid1.Canvas.Font.Color := clBlack;
  end
  else
  begin
    // 閉じたセルはデフォルトの色
    StringGrid1.Canvas.Brush.Color := clBtnFace;
    StringGrid1.Canvas.Font.Color := clBlack;
  end;
  
  // 背景を描画
  StringGrid1.Canvas.FillRect(aRect);
  
  // 境界線を描画
  StringGrid1.Canvas.Pen.Color := clGray;
  StringGrid1.Canvas.Rectangle(aRect);
  
  // テキストを描画
  if CellText <> '' then
  begin
    StringGrid1.Canvas.TextRect(aRect, aRect.Left + 8, aRect.Top + 4, CellText);
  end;
end;

procedure TForm1.StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  CanSelect := False; // セル選択を無効化
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  InitializeGame;
end;

end.

