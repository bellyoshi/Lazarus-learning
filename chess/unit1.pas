unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Types, StdCtrls;

type
  // チェスの駒の種類
  TPieceType = (ptNone, ptPawn, ptRook, ptKnight, ptBishop, ptQueen, ptKing);
  
  // チェスの駒の色
  TPieceColor = (pcWhite, pcBlack);
  
  // チェスの駒
  TChessPiece = record
    PieceType: TPieceType;
    Color: TPieceColor;
  end;
  
  // チェスボード（8x8）
  TChessBoard = array[0..7, 0..7] of TChessPiece;

type
  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    LabelTurn: TLabel;
    ButtonStart: TButton;
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonStartClick(Sender: TObject);
  private
    FBoard: TChessBoard;
    FCellSize: Integer;
    FCurrentPlayer: TPieceColor;  // 現在の手番
    FSelectedRow, FSelectedCol: Integer;  // 選択されたマス（-1は未選択）
    FGameStarted: Boolean;  // ゲームが開始されているか
    FGameEnded: Boolean;  // ゲームが終了しているか
    // キャスリング用：キングとルークが動いたかどうか
    FKingMoved: array[TPieceColor] of Boolean;
    FRookMoved: array[TPieceColor, 0..1] of Boolean;  // [色, 0=キングサイド, 1=クイーンサイド]
    procedure InitializeBoard;
    procedure DrawBoard(ACanvas: TCanvas);
    procedure DrawPiece(ACanvas: TCanvas; X, Y: Integer; Piece: TChessPiece);
    function GetPieceSymbol(PieceType: TPieceType; AColor: TPieceColor): String;
    function GetSquareColor(Row, Col: Integer): TColor;
    procedure UpdateTurnDisplay;
    procedure MovePiece(FromRow, FromCol, ToRow, ToCol: Integer);
    function IsValidMove(FromRow, FromCol, ToRow, ToCol: Integer): Boolean;
    function IsPathClear(FromRow, FromCol, ToRow, ToCol: Integer): Boolean;
    function IsPawnFirstMove(Row, Col: Integer): Boolean;
    function CanCastle(Kingside: Boolean): Boolean;
    procedure PerformCastle(Kingside: Boolean);
    function CheckKingCaptured: Boolean;
    procedure EndGame(Winner: TPieceColor);
  public

  end;

var
  Form1: TForm1;

implementation

uses Unit2;

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCellSize := 60;
  PaintBox1.Width := FCellSize * 8;
  PaintBox1.Height := FCellSize * 8;
  ClientWidth := PaintBox1.Width + 40;
  ClientHeight := PaintBox1.Height + 80;
  Caption := 'チェスボード';
  
  // 初期状態ではボードを空にする（ゲーム開始ボタンで初期化）
  FCurrentPlayer := pcWhite;
  FSelectedRow := -1;  // 未選択
  FSelectedCol := -1;
  FGameStarted := False;
  FGameEnded := False;
  
  // キャスリング用の初期化
  FKingMoved[pcWhite] := False;
  FKingMoved[pcBlack] := False;
  FRookMoved[pcWhite, 0] := False;  // 白のキングサイドルーク
  FRookMoved[pcWhite, 1] := False;  // 白のクイーンサイドルーク
  FRookMoved[pcBlack, 0] := False;  // 黒のキングサイドルーク
  FRookMoved[pcBlack, 1] := False;  // 黒のクイーンサイドルーク
  
  // 初期表示では手番を表示しない
  LabelTurn.Caption := '';
end;

procedure TForm1.InitializeBoard;
var
  Row, Col: Integer;
begin
  // ボードを初期化（すべて空に）
  for Row := 0 to 7 do
    for Col := 0 to 7 do
    begin
      FBoard[Row, Col].PieceType := ptNone;
      FBoard[Row, Col].Color := pcWhite;
    end;
  
  // 白の駒を配置
  FBoard[7, 0].PieceType := ptRook;
  FBoard[7, 0].Color := pcWhite;
  FBoard[7, 1].PieceType := ptKnight;
  FBoard[7, 1].Color := pcWhite;
  FBoard[7, 2].PieceType := ptBishop;
  FBoard[7, 2].Color := pcWhite;
  FBoard[7, 3].PieceType := ptQueen;
  FBoard[7, 3].Color := pcWhite;
  FBoard[7, 4].PieceType := ptKing;
  FBoard[7, 4].Color := pcWhite;
  FBoard[7, 5].PieceType := ptBishop;
  FBoard[7, 5].Color := pcWhite;
  FBoard[7, 6].PieceType := ptKnight;
  FBoard[7, 6].Color := pcWhite;
  FBoard[7, 7].PieceType := ptRook;
  FBoard[7, 7].Color := pcWhite;
  
  for Col := 0 to 7 do
  begin
    FBoard[6, Col].PieceType := ptPawn;
    FBoard[6, Col].Color := pcWhite;
  end;
  
  // 黒の駒を配置
  FBoard[0, 0].PieceType := ptRook;
  FBoard[0, 0].Color := pcBlack;
  FBoard[0, 1].PieceType := ptKnight;
  FBoard[0, 1].Color := pcBlack;
  FBoard[0, 2].PieceType := ptBishop;
  FBoard[0, 2].Color := pcBlack;
  FBoard[0, 3].PieceType := ptQueen;
  FBoard[0, 3].Color := pcBlack;
  FBoard[0, 4].PieceType := ptKing;
  FBoard[0, 4].Color := pcBlack;
  FBoard[0, 5].PieceType := ptBishop;
  FBoard[0, 5].Color := pcBlack;
  FBoard[0, 6].PieceType := ptKnight;
  FBoard[0, 6].Color := pcBlack;
  FBoard[0, 7].PieceType := ptRook;
  FBoard[0, 7].Color := pcBlack;
  
  for Col := 0 to 7 do
  begin
    FBoard[1, Col].PieceType := ptPawn;
    FBoard[1, Col].Color := pcBlack;
  end;
end;

procedure TForm1.DrawBoard(ACanvas: TCanvas);
var
  Row, Col: Integer;
  Rect: TRect;
begin
  for Row := 0 to 7 do
    for Col := 0 to 7 do
    begin
      Rect.Left := Col * FCellSize;
      Rect.Top := Row * FCellSize;
      Rect.Right := Rect.Left + FCellSize;
      Rect.Bottom := Rect.Top + FCellSize;
      
      // マスの色を設定して塗りつぶし
      ACanvas.Brush.Color := GetSquareColor(Row, Col);
      ACanvas.Brush.Style := bsSolid;
      
      // 選択されたマスをハイライト
      if (Row = FSelectedRow) and (Col = FSelectedCol) then
      begin
        // 選択されたマスを黄色でハイライト
        ACanvas.Brush.Color := clYellow;
      end;
      
      ACanvas.FillRect(Rect);
      
      // 枠線を描画（駒の前に枠線を描画）
      ACanvas.Pen.Color := clGray;
      ACanvas.Pen.Width := 1;
      ACanvas.Brush.Style := bsClear;
      ACanvas.Rectangle(Rect);
      
      // 選択されたマスに太い枠線を描画
      if (Row = FSelectedRow) and (Col = FSelectedCol) then
      begin
        ACanvas.Pen.Color := clRed;
        ACanvas.Pen.Width := 3;
        ACanvas.Rectangle(Rect);
        ACanvas.Pen.Width := 1;
      end;
      
      // 駒を描画（最後に描画して、枠線の上に表示）
      if FBoard[Row, Col].PieceType <> ptNone then
        DrawPiece(ACanvas, Rect.Left, Rect.Top, FBoard[Row, Col]);
    end;
end;

procedure TForm1.DrawPiece(ACanvas: TCanvas; X, Y: Integer; Piece: TChessPiece);
var
  Symbol: String;
  TextWidth, TextHeight: Integer;
  TextX, TextY: Integer;
  OldFontSize: Integer;
  OldFontName: String;
  OldBrushStyle: TBrushStyle;
begin
  Symbol := GetPieceSymbol(Piece.PieceType, Piece.Color);
  
  // フォント設定（Unicode対応フォントを使用）
  OldFontSize := ACanvas.Font.Size;
  OldFontName := ACanvas.Font.Name;
  OldBrushStyle := ACanvas.Brush.Style;
  
  // Unicode対応フォントを試す（優先順位順）
  // Windows標準のUnicode対応フォント
  ACanvas.Font.Name := 'Segoe UI Symbol';
  ACanvas.Font.Size := 42;
  ACanvas.Font.Style := [fsBold];
  
  // 駒の色を設定（Unicode文字は元々色が付いているので、濃い色で表示）
  if Piece.Color = pcWhite then
    ACanvas.Font.Color := clNavy  // 白の駒は濃い青で表示
  else
    ACanvas.Font.Color := clBlack;  // 黒の駒は黒で表示
  
  // ブラシを透明に設定（背景を塗りつぶさない）
  ACanvas.Brush.Style := bsClear;
  
  // テキストのサイズを取得
  TextWidth := ACanvas.TextWidth(Symbol);
  TextHeight := ACanvas.TextHeight(Symbol);
  
  // テキストを中央に配置
  TextX := X + (FCellSize - TextWidth) div 2;
  TextY := Y + (FCellSize - TextHeight) div 2;
  
  // TextOutで描画
  ACanvas.TextOut(TextX, TextY, Symbol);
  
  // 設定を元に戻す
  ACanvas.Font.Size := OldFontSize;
  ACanvas.Font.Name := OldFontName;
  ACanvas.Brush.Style := OldBrushStyle;
end;

function TForm1.GetPieceSymbol(PieceType: TPieceType; AColor: TPieceColor): String;
begin
  // Unicode文字で表示（チェスの駒の絵文字）
  if AColor = pcWhite then
  begin
    // 白の駒
    case PieceType of
      ptKing: Result := '♔';    // U+2654
      ptQueen: Result := '♕';   // U+2655
      ptRook: Result := '♖';    // U+2656
      ptBishop: Result := '♗';  // U+2657
      ptKnight: Result := '♘';  // U+2658
      ptPawn: Result := '♙';    // U+2659
      else Result := '';
    end;
  end
  else
  begin
    // 黒の駒
    case PieceType of
      ptKing: Result := '♚';    // U+265A
      ptQueen: Result := '♛';   // U+265B
      ptRook: Result := '♜';    // U+265C
      ptBishop: Result := '♝';  // U+265D
      ptKnight: Result := '♞';  // U+265E
      ptPawn: Result := '♟';    // U+265F
      else Result := '';
    end;
  end;
end;

function TForm1.GetSquareColor(Row, Col: Integer): TColor;
begin
  // チェッカーパターン
  if (Row + Col) mod 2 = 0 then
    Result := $F0D9B5  // ライトブラウン
  else
    Result := $B58863; // ダークブラウン
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  DrawBoard(PaintBox1.Canvas);
end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
  ClickedPiece: TChessPiece;
begin
  if Button = mbLeft then
  begin
    Col := X div FCellSize;
    Row := Y div FCellSize;
    
    if (Col >= 0) and (Col < 8) and (Row >= 0) and (Row < 8) then
    begin
      ClickedPiece := FBoard[Row, Col];
      
      // 既にマスが選択されている場合
      if (FSelectedRow >= 0) and (FSelectedCol >= 0) then
      begin
        // 同じマスをクリックした場合は選択を解除
        if (Row = FSelectedRow) and (Col = FSelectedCol) then
        begin
          FSelectedRow := -1;
          FSelectedCol := -1;
        end
        // 別のマスをクリックした場合、移動を試みる
        else
        begin
          // ゲームが終了している場合は移動できない
          if FGameEnded or not FGameStarted then
          begin
            FSelectedRow := -1;
            FSelectedCol := -1;
            Exit;
          end;
          
          // 移動が有効かチェック
          if IsValidMove(FSelectedRow, FSelectedCol, Row, Col) then
          begin
            // キャスリングかチェック（キングが2マス横に移動）
            if (FBoard[FSelectedRow, FSelectedCol].PieceType = ptKing) and
               (FSelectedRow = Row) and (Abs(Col - FSelectedCol) = 2) then
            begin
              // キャスリングを実行
              if Col > FSelectedCol then
                PerformCastle(True)  // キングサイド
              else
                PerformCastle(False);  // クイーンサイド
            end
            else
            begin
              // 通常の移動
              MovePiece(FSelectedRow, FSelectedCol, Row, Col);
              
              // ポーンのプロモーション（成り）チェック
              if (FBoard[Row, Col].PieceType = ptPawn) then
              begin
                // 白のポーンが1列目（Row=0）に到達、または黒のポーンが8列目（Row=7）に到達
                if ((FBoard[Row, Col].Color = pcWhite) and (Row = 0)) or
                   ((FBoard[Row, Col].Color = pcBlack) and (Row = 7)) then
                begin
                  // プロモーション選択フォームを開く
                  if Assigned(Form2) then
                  begin
                    FBoard[Row, Col].PieceType := Form2.SelectPromotion(FBoard[Row, Col].Color);
                  end;
                end;
              end;
            end;
            
            // キングが取られたかチェック
            if CheckKingCaptured then
            begin
              // 勝者は現在の手番のプレイヤー（キングを取った側）
              EndGame(FCurrentPlayer);
            end
            else
            begin
              // 手番を切り替え
              if FCurrentPlayer = pcWhite then
                FCurrentPlayer := pcBlack
              else
                FCurrentPlayer := pcWhite;
              UpdateTurnDisplay;
            end;
          end;
          // 選択を解除
          FSelectedRow := -1;
          FSelectedCol := -1;
        end;
      end
      // マスが選択されていない場合
      else
      begin
        // ゲームが終了している場合は選択できない
        if FGameEnded or not FGameStarted then
          Exit;
        
        // クリックしたマスに駒があり、それが現在の手番の駒の場合
        if (ClickedPiece.PieceType <> ptNone) and (ClickedPiece.Color = FCurrentPlayer) then
        begin
          FSelectedRow := Row;
          FSelectedCol := Col;
        end;
      end;
      
      PaintBox1.Invalidate;  // ボードを再描画
    end;
  end;
end;

procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  // ゲームを開始：ボードを初期化
  InitializeBoard;
  FCurrentPlayer := pcWhite;  // 白から開始
  FSelectedRow := -1;  // 選択を解除
  FSelectedCol := -1;
  FGameStarted := True;
  FGameEnded := False;
  
  // キャスリング用の初期化
  FKingMoved[pcWhite] := False;
  FKingMoved[pcBlack] := False;
  FRookMoved[pcWhite, 0] := False;  // 白のキングサイドルーク
  FRookMoved[pcWhite, 1] := False;  // 白のクイーンサイドルーク
  FRookMoved[pcBlack, 0] := False;  // 黒のキングサイドルーク
  FRookMoved[pcBlack, 1] := False;  // 黒のクイーンサイドルーク
  
  UpdateTurnDisplay;
  PaintBox1.Invalidate;  // ボードを再描画
end;

procedure TForm1.MovePiece(FromRow, FromCol, ToRow, ToCol: Integer);
var
  MovedPiece: TChessPiece;
begin
  MovedPiece := FBoard[FromRow, FromCol];
  
  // キングの移動を記録
  if MovedPiece.PieceType = ptKing then
    FKingMoved[MovedPiece.Color] := True;
  
  // ルークの移動を記録
  if MovedPiece.PieceType = ptRook then
  begin
    // 白のルーク
    if MovedPiece.Color = pcWhite then
    begin
      if (FromRow = 7) and (FromCol = 0) then  // クイーンサイド
        FRookMoved[pcWhite, 1] := True
      else if (FromRow = 7) and (FromCol = 7) then  // キングサイド
        FRookMoved[pcWhite, 0] := True;
    end
    // 黒のルーク
    else
    begin
      if (FromRow = 0) and (FromCol = 0) then  // クイーンサイド
        FRookMoved[pcBlack, 1] := True
      else if (FromRow = 0) and (FromCol = 7) then  // キングサイド
        FRookMoved[pcBlack, 0] := True;
    end;
  end;
  
  // 駒を移動
  FBoard[ToRow, ToCol] := MovedPiece;
  // 元のマスを空にする
  FBoard[FromRow, FromCol].PieceType := ptNone;
  FBoard[FromRow, FromCol].Color := pcWhite;
end;

function TForm1.IsValidMove(FromRow, FromCol, ToRow, ToCol: Integer): Boolean;
var
  FromPiece, ToPiece: TChessPiece;
  RowDiff, ColDiff: Integer;
begin
  Result := False;
  
  // 範囲チェック
  if (FromRow < 0) or (FromRow > 7) or (FromCol < 0) or (FromCol > 7) or
     (ToRow < 0) or (ToRow > 7) or (ToCol < 0) or (ToCol > 7) then
    Exit;
  
  FromPiece := FBoard[FromRow, FromCol];
  ToPiece := FBoard[ToRow, ToCol];
  
  // 移動元に駒がない場合は無効
  if FromPiece.PieceType = ptNone then
    Exit;
  
  // 移動元の駒が現在の手番の駒でない場合は無効
  if FromPiece.Color <> FCurrentPlayer then
    Exit;
  
  // 移動先に同じ色の駒がある場合は無効
  if (ToPiece.PieceType <> ptNone) and (ToPiece.Color = FCurrentPlayer) then
    Exit;
  
  // 同じマスへの移動は無効
  if (FromRow = ToRow) and (FromCol = ToCol) then
    Exit;
  
  RowDiff := ToRow - FromRow;
  ColDiff := ToCol - FromCol;
  
  // 各駒の移動ルールをチェック
  case FromPiece.PieceType of
    ptPawn:
    begin
      // ポーンの移動ルール
      if FromPiece.Color = pcWhite then
      begin
        // 白のポーンは上方向（Rowが減る方向）に進む（Row=6からRow=5,4,3...）
        // 前進のみ
        if ColDiff <> 0 then
        begin
          // 斜め前進は敵の駒を取る時のみ（1マス斜め前）
          if (RowDiff = -1) and (Abs(ColDiff) = 1) then
            Result := (ToPiece.PieceType <> ptNone) and (ToPiece.Color = pcBlack)
          else
            Result := False;
        end
        else
        begin
          // 真っ直ぐ前進
          if RowDiff = -1 then
            // 1マス前進（移動先が空）
            Result := ToPiece.PieceType = ptNone
          else if (RowDiff = -2) and IsPawnFirstMove(FromRow, FromCol) then
            // 最初の移動は2マス前進可能（経路と移動先が空）
            Result := (ToPiece.PieceType = ptNone) and (FBoard[FromRow - 1, FromCol].PieceType = ptNone)
          else
            Result := False;
        end;
      end
      else
      begin
        // 黒のポーンは下方向（Rowが増える方向）に進む（Row=1からRow=2,3,4...）
        if ColDiff <> 0 then
        begin
          // 斜め前進は敵の駒を取る時のみ（1マス斜め前）
          if (RowDiff = 1) and (Abs(ColDiff) = 1) then
            Result := (ToPiece.PieceType <> ptNone) and (ToPiece.Color = pcWhite)
          else
            Result := False;
        end
        else
        begin
          // 真っ直ぐ前進
          if RowDiff = 1 then
            // 1マス前進（移動先が空）
            Result := ToPiece.PieceType = ptNone
          else if (RowDiff = 2) and IsPawnFirstMove(FromRow, FromCol) then
            // 最初の移動は2マス前進可能（経路と移動先が空）
            Result := (ToPiece.PieceType = ptNone) and (FBoard[FromRow + 1, FromCol].PieceType = ptNone)
          else
            Result := False;
        end;
      end;
    end;
    
    ptRook:
    begin
      // ルークは縦横に直線移動
      if (RowDiff = 0) or (ColDiff = 0) then
        Result := IsPathClear(FromRow, FromCol, ToRow, ToCol)
      else
        Result := False;
    end;
    
    ptKnight:
    begin
      // ナイトはL字型移動（2マス+1マス）
      Result := ((Abs(RowDiff) = 2) and (Abs(ColDiff) = 1)) or
                ((Abs(RowDiff) = 1) and (Abs(ColDiff) = 2));
      // ナイトは他の駒を飛び越えられるので経路チェック不要
    end;
    
    ptBishop:
    begin
      // ビショップは斜めに直線移動
      if Abs(RowDiff) = Abs(ColDiff) then
        Result := IsPathClear(FromRow, FromCol, ToRow, ToCol)
      else
        Result := False;
    end;
    
    ptQueen:
    begin
      // クイーンは縦横斜めに直線移動
      if ((RowDiff = 0) or (ColDiff = 0)) or (Abs(RowDiff) = Abs(ColDiff)) then
        Result := IsPathClear(FromRow, FromCol, ToRow, ToCol)
      else
        Result := False;
    end;
    
    ptKing:
    begin
      // キングは全方向に1マス移動、またはキャスリング
      if (Abs(RowDiff) <= 1) and (Abs(ColDiff) <= 1) then
        Result := True  // キングは1マスなので経路チェック不要
      // キャスリングのチェック（キングが2マス横に移動）
      else if (RowDiff = 0) and (Abs(ColDiff) = 2) then
      begin
        // キングサイド（右に2マス）またはクイーンサイド（左に2マス）
        if ColDiff = 2 then
          Result := CanCastle(True)  // キングサイド
        else
          Result := CanCastle(False);  // クイーンサイド
      end
      else
        Result := False;
    end;
    
    else
      Result := False;
  end;
end;

function TForm1.IsPathClear(FromRow, FromCol, ToRow, ToCol: Integer): Boolean;
var
  RowStep, ColStep: Integer;
  CurrentRow, CurrentCol: Integer;
begin
  Result := True;
  
  // 移動方向を決定
  if ToRow > FromRow then
    RowStep := 1
  else if ToRow < FromRow then
    RowStep := -1
  else
    RowStep := 0;
  
  if ToCol > FromCol then
    ColStep := 1
  else if ToCol < FromCol then
    ColStep := -1
  else
    ColStep := 0;
  
  // 移動元から移動先の直前まで経路をチェック
  CurrentRow := FromRow + RowStep;
  CurrentCol := FromCol + ColStep;
  
  while (CurrentRow <> ToRow) or (CurrentCol <> ToCol) do
  begin
    // 経路上に駒がある場合は無効
    if FBoard[CurrentRow, CurrentCol].PieceType <> ptNone then
    begin
      Result := False;
      Exit;
    end;
    
    CurrentRow := CurrentRow + RowStep;
    CurrentCol := CurrentCol + ColStep;
  end;
end;

function TForm1.IsPawnFirstMove(Row, Col: Integer): Boolean;
begin
  // ポーンが初期位置にいるかチェック
  // 白のポーンはRow=6（2列目）、黒のポーンはRow=1（7列目）
  if FBoard[Row, Col].Color = pcWhite then
    Result := Row = 6
  else
    Result := Row = 1;
end;

function TForm1.CanCastle(Kingside: Boolean): Boolean;
var
  KingRow, KingCol, RookCol: Integer;
  i: Integer;
begin
  Result := False;
  
  // キングが動いていないかチェック
  if FKingMoved[FCurrentPlayer] then
    Exit;
  
  // 現在の手番のキングの位置を取得
  if FCurrentPlayer = pcWhite then
    KingRow := 7
  else
    KingRow := 0;
  
  KingCol := 4;  // キングは常にe列（Col=4）
  
  // ルークが動いていないかチェック
  if Kingside then
  begin
    // キングサイドキャスリング
    if FRookMoved[FCurrentPlayer, 0] then
      Exit;
    RookCol := 7;  // キングサイドルークはh列（Col=7）
  end
  else
  begin
    // クイーンサイドキャスリング
    if FRookMoved[FCurrentPlayer, 1] then
      Exit;
    RookCol := 0;  // クイーンサイドルークはa列（Col=0）
  end;
  
  // ルークが正しい位置にいるかチェック
  if (FBoard[KingRow, RookCol].PieceType <> ptRook) or
     (FBoard[KingRow, RookCol].Color <> FCurrentPlayer) then
    Exit;
  
  // キングとルークの間に駒がないかチェック
  if Kingside then
  begin
    // キングサイド：f列とg列（Col=5, 6）が空
    for i := 5 to 6 do
      if FBoard[KingRow, i].PieceType <> ptNone then
        Exit;
  end
  else
  begin
    // クイーンサイド：b列、c列、d列（Col=1, 2, 3）が空
    for i := 1 to 3 do
      if FBoard[KingRow, i].PieceType <> ptNone then
        Exit;
  end;
  
  // チェック状態のチェックは省略（将来的に実装可能）
  // ここでは基本的な条件のみチェック
  
  Result := True;
end;

procedure TForm1.PerformCastle(Kingside: Boolean);
var
  KingRow, KingCol, RookCol, NewKingCol, NewRookCol: Integer;
begin
  // 現在の手番のキングの位置を取得
  if FCurrentPlayer = pcWhite then
    KingRow := 7
  else
    KingRow := 0;
  
  KingCol := 4;  // キングは常にe列（Col=4）
  
  if Kingside then
  begin
    // キングサイドキャスリング
    RookCol := 7;  // キングサイドルークはh列（Col=7）
    NewKingCol := 6;  // キングはg列（Col=6）に移動
    NewRookCol := 5;  // ルークはf列（Col=5）に移動
  end
  else
  begin
    // クイーンサイドキャスリング
    RookCol := 0;  // クイーンサイドルークはa列（Col=0）
    NewKingCol := 2;  // キングはc列（Col=2）に移動
    NewRookCol := 3;  // ルークはd列（Col=3）に移動
  end;
  
  // キングを移動
  FBoard[KingRow, NewKingCol] := FBoard[KingRow, KingCol];
  FBoard[KingRow, KingCol].PieceType := ptNone;
  FBoard[KingRow, KingCol].Color := pcWhite;
  
  // ルークを移動
  FBoard[KingRow, NewRookCol] := FBoard[KingRow, RookCol];
  FBoard[KingRow, RookCol].PieceType := ptNone;
  FBoard[KingRow, RookCol].Color := pcWhite;
  
  // キングとルークが動いたことを記録
  FKingMoved[FCurrentPlayer] := True;
  if Kingside then
    FRookMoved[FCurrentPlayer, 0] := True
  else
    FRookMoved[FCurrentPlayer, 1] := True;
end;

procedure TForm1.UpdateTurnDisplay;
begin
  // 手番を表示（ゲーム開始時のみ）
  if FGameStarted and not FGameEnded then
  begin
    if FCurrentPlayer = pcWhite then
      LabelTurn.Caption := '手番: 白 (White)'
    else
      LabelTurn.Caption := '手番: 黒 (Black)';
  end;
end;

function TForm1.CheckKingCaptured: Boolean;
var
  Row, Col: Integer;
  WhiteKingFound, BlackKingFound: Boolean;
begin
  Result := False;
  WhiteKingFound := False;
  BlackKingFound := False;
  
  // ボード全体をチェックしてキングが存在するか確認
  for Row := 0 to 7 do
    for Col := 0 to 7 do
    begin
      if FBoard[Row, Col].PieceType = ptKing then
      begin
        if FBoard[Row, Col].Color = pcWhite then
          WhiteKingFound := True
        else
          BlackKingFound := True;
      end;
    end;
  
  // どちらかのキングが取られた場合
  if not WhiteKingFound then
  begin
    // 白のキングが取られた → 黒の勝ち
    Result := True;
  end
  else if not BlackKingFound then
  begin
    // 黒のキングが取られた → 白の勝ち
    Result := True;
  end;
end;

procedure TForm1.EndGame(Winner: TPieceColor);
var
  WinnerText: String;
begin
  FGameEnded := True;
  
  // 勝者を決定
  if Winner = pcWhite then
    WinnerText := '白 (White)'
  else
    WinnerText := '黒 (Black)';
  
  // 勝者を表示
  LabelTurn.Caption := '勝者: ' + WinnerText + ' の勝利！';
  
  // メッセージボックスで勝者を表示
  ShowMessage('ゲーム終了！' + #13#10 + '勝者: ' + WinnerText);
  
  // ボードを再描画
  PaintBox1.Invalidate;
end;

end.

