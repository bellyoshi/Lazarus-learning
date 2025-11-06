unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Unit_Reversi;

type
  TForm1 = class(TForm)
    MainPanel: TPanel;
    BoardPanel: TPanel;
    InfoPanel: TPanel;
    lblCurrentPlayer: TLabel;
    lblBlackCount: TLabel;
    lblWhiteCount: TLabel;
    btnReset: TButton;
    lblStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
  private
    FGame: TReversiGame;
    FBoardPanels: array[0..7, 0..7] of TPanel;
    procedure CreateBoard;
    procedure UpdateBoard;
    procedure UpdateInfo;
    procedure BoardCellClick(Sender: TObject);
    procedure HighlightValidMoves;
    procedure ClearHighlights;
    function GetCellColor(X, Y: Integer): TColor;
    procedure CheckAndAutoPass;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FGame := TReversiGame.Create;
  CreateBoard;
  CheckAndAutoPass; // ゲーム開始時に自動パスをチェック
  UpdateBoard;
  UpdateInfo;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FGame.Free;
end;

procedure TForm1.CreateBoard;
var
  X, Y: Integer;
  Cell: TPanel;
  CellSize: Integer;
begin
  // ボードパネルのサイズ設定
  CellSize := 50;
  BoardPanel.Width := CellSize * 8 + 16;
  BoardPanel.Height := CellSize * 8 + 16;
  
  // 8x8のマスを作成
  for X := 0 to 7 do
  begin
    for Y := 0 to 7 do
    begin
      Cell := TPanel.Create(Self);
      Cell.Parent := BoardPanel;
      Cell.Left := X * CellSize + 8;
      Cell.Top := Y * CellSize + 8;
      Cell.Width := CellSize - 2;
      Cell.Height := CellSize - 2;
      Cell.Color := $00D4A460; // ベージュ色（ボードの色）
      Cell.BevelOuter := bvLowered;
      Cell.Cursor := crHandPoint;
      Cell.Tag := X * 8 + Y; // 位置情報を保存
      Cell.OnClick := @BoardCellClick;
      FBoardPanels[X, Y] := Cell;
    end;
  end;
end;

procedure TForm1.UpdateBoard;
var
  X, Y: Integer;
  Cell: TPanel;
  Piece: TPiece;
begin
  ClearHighlights;
  
  for X := 0 to 7 do
  begin
    for Y := 0 to 7 do
    begin
      Cell := FBoardPanels[X, Y];
      Piece := FGame.PieceAt[X, Y];
      
      // 駒の表示
      case Piece of
        Black:
        begin
          Cell.Caption := '●';
          Cell.Font.Color := clBlack;
          Cell.Font.Size := 24;
        end;
        White:
        begin
          Cell.Caption := '○';
          Cell.Font.Color := clWhite;
          Cell.Font.Size := 24;
        end;
        Empty:
        begin
          Cell.Caption := '';
        end;
      end;
    end;
  end;
  
  HighlightValidMoves;
end;

procedure TForm1.UpdateInfo;
var
  Winner: TPlayer;
begin
  // 駒数の表示
  lblBlackCount.Caption := '黒: ' + IntToStr(FGame.BlackCount);
  lblWhiteCount.Caption := '白: ' + IntToStr(FGame.WhiteCount);
  
  // 現在のプレイヤー表示
  if FGame.GameOver then
  begin
    Winner := FGame.GetWinner;
    if FGame.BlackCount = FGame.WhiteCount then
      lblStatus.Caption := '引き分け'
    else if Winner = PlayerBlack then
      lblStatus.Caption := '黒の勝ち'
    else
      lblStatus.Caption := '白の勝ち';
    lblCurrentPlayer.Caption := 'ゲーム終了';
  end
  else
  begin
    if FGame.CurrentPlayer = PlayerBlack then
    begin
      lblCurrentPlayer.Caption := '現在のプレイヤー: 黒';
      lblCurrentPlayer.Font.Color := clBlack;
    end
    else
    begin
      lblCurrentPlayer.Caption := '現在のプレイヤー: 白';
      lblCurrentPlayer.Font.Color := clGray;
    end;
    lblStatus.Caption := '';
  end;
end;

procedure TForm1.BoardCellClick(Sender: TObject);
var
  X, Y: Integer;
  CellTag: Integer;
begin
  if FGame.GameOver then
    Exit;
  
  CellTag := (Sender as TPanel).Tag;
  X := CellTag div 8;
  Y := CellTag mod 8;
  
  // 手を打つ
  if FGame.MakeMove(X, Y, FGame.CurrentPlayer) then
  begin
    // ゲーム終了チェック
    if not FGame.CheckGameOver then
    begin
      // 次のプレイヤーに切り替え
      FGame.SwitchPlayer;
    end;
    
    // プレイヤー切り替え後に自動パスをチェック（必要に応じて自動的にパス）
    CheckAndAutoPass;
    
    // プレイヤー切り替え後にボードを更新（正しいプレイヤーの有効手を表示）
    UpdateBoard;
    UpdateInfo;
  end;
end;

procedure TForm1.HighlightValidMoves;
var
  X, Y: Integer;
  ValidMoves: TPositionArray;
  Pos: TPosition;
begin
  ValidMoves := FGame.GetValidMoves(FGame.CurrentPlayer);
  for Pos in ValidMoves do
  begin
    X := Pos.X;
    Y := Pos.Y;
    if (X >= 0) and (X <= 7) and (Y >= 0) and (Y <= 7) then
    begin
      // 有効な手のマスを薄い緑色でハイライト
      FBoardPanels[X, Y].Color := $00B0FFB0; // 薄い緑色
    end;
  end;
end;

procedure TForm1.ClearHighlights;
var
  X, Y: Integer;
begin
  for X := 0 to 7 do
    for Y := 0 to 7 do
      FBoardPanels[X, Y].Color := $00D4A460; // ベージュ色に戻す
end;

function TForm1.GetCellColor(X, Y: Integer): TColor;
begin
  // チェッカーパターン（オプション）
  if (X + Y) mod 2 = 0 then
    Result := $00D4A460
  else
    Result := $00C8A050;
end;

procedure TForm1.btnResetClick(Sender: TObject);
begin
  FGame.Reset;
  CheckAndAutoPass; // リセット後も自動パスをチェック
  UpdateBoard;
  UpdateInfo;
end;

procedure TForm1.CheckAndAutoPass;
var
  PreviousPlayer: TPlayer;
begin
  // ゲームが終了していない間、現在のプレイヤーが手を打てない場合は自動的にパス
  while not FGame.GameOver do
  begin
    if not FGame.HasValidMoves(FGame.CurrentPlayer) then
    begin
      // パスする前のプレイヤーを記録
      PreviousPlayer := FGame.CurrentPlayer;
      
      // パスを実行（CheckGameOverが自動的にプレイヤーを切り替える）
      FGame.CheckGameOver;
      
      // ゲームが終了した場合はループを抜ける
      if FGame.GameOver then
        Break;
      
      // パスしたことを通知（前のプレイヤーがパスした）
      if PreviousPlayer = PlayerBlack then
        lblStatus.Caption := '黒はパスしました'
      else
        lblStatus.Caption := '白はパスしました';
      
      // 次のプレイヤーも手を打てない可能性があるので、ループを継続
    end
    else
    begin
      // 手を打てる場合はループを抜ける
      Break;
    end;
  end;
end;

end.

