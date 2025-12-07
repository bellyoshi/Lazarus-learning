unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Math;

type
  TPieceSize = (psSmall, psMedium, psLarge);
  TPieceColor = (pcRed, pcBlue);
  
  TGamePiece = record
    Size: TPieceSize;
    Color: TPieceColor;
    Image: TBitmap;
  end;
  
  TSelectionType = (stNone, stHandPiece, stBoardPiece);
  
  TSelection = record
    SelectionType: TSelectionType;
    HandIndex: Integer;  // 持ち駒のインデックス（サイズと色から計算）
    BoardCol, BoardRow: Integer;  // ボード上の位置
    Piece: TGamePiece;
  end;

  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    PaintBoxRed: TPaintBox;
    PaintBoxBlue: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxRedPaint(Sender: TObject);
    procedure PaintBoxRedMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxBluePaint(Sender: TObject);
    procedure PaintBoxBlueMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FPieces: array[TPieceSize, TPieceColor] of TBitmap;
    FBoard: array[0..2, 0..2] of TGamePiece;
    FHandPieces: array[TPieceColor, TPieceSize, 0..1] of Boolean;  // 持ち駒の有無
    FCurrentPlayer: TPieceColor;
    FSelection: TSelection;
    procedure LoadPieces;
    procedure DrawBoard;
    procedure DrawHandPieces(ACanvas: TCanvas; AColor: TPieceColor; AWidth, AHeight: Integer);
    procedure DrawPiece(ACanvas: TCanvas; X, Y: Integer; Piece: TGamePiece; Selected: Boolean = False);
    function GetCellFromPos(X, Y: Integer; out Col, Row: Integer): Boolean;
    function GetHandPieceFromPos(X, Y: Integer; AColor: TPieceColor; out Size: TPieceSize; out Index: Integer): Boolean;
    function CanPlacePiece(ASize: TPieceSize; ACol, ARow: Integer): Boolean;
    procedure InitializeHandPieces;
    procedure UpdateSelectionDisplay;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  i, j: Integer;
begin
  // ボードを初期化
  for i := 0 to 2 do
    for j := 0 to 2 do
    begin
      FBoard[i, j].Size := psSmall;
      FBoard[i, j].Color := pcRed;
      FBoard[i, j].Image := nil;
    end;
  
  // 選択を初期化
  FSelection.SelectionType := stNone;
  FSelection.HandIndex := -1;
  FSelection.BoardCol := -1;
  FSelection.BoardRow := -1;
  
  FCurrentPlayer := pcRed;
  
  // 持ち駒を初期化
  InitializeHandPieces;
  
  // 駒の画像を読み込む
  LoadPieces;
  
  Label1.Caption := '現在のプレイヤー: 赤';
  Label2.Caption := '持ち駒をクリックして選択、ボードをクリックして配置';
end;

procedure TForm1.InitializeHandPieces;
var
  PieceColor: TPieceColor;
  Size: TPieceSize;
  i: Integer;
begin
  // 各プレイヤーが大・中・小を2つずつ持つ
  for PieceColor := Low(TPieceColor) to High(TPieceColor) do
    for Size := Low(TPieceSize) to High(TPieceSize) do
      for i := 0 to 1 do
        FHandPieces[PieceColor, Size, i] := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  Size: TPieceSize;
  PieceColor: TPieceColor;
begin
  // 画像を解放
  for Size := Low(TPieceSize) to High(TPieceSize) do
    for PieceColor := Low(TPieceColor) to High(TPieceColor) do
      if Assigned(FPieces[Size, PieceColor]) then
        FPieces[Size, PieceColor].Free;
end;

procedure TForm1.LoadPieces;
var
  BasePath: String;
  SizeNames: array[TPieceSize] of String = ('small', 'medium', 'large');
  ColorNames: array[TPieceColor] of String = ('red', 'blue');
  Size: TPieceSize;
  PieceColor: TPieceColor;
  FileName: String;
begin
  // 実行ファイルのディレクトリを取得
  BasePath := ExtractFilePath(Application.ExeName);
  BasePath := BasePath + 'pieces_bmp' + PathDelim;
  
  // 各サイズと色の組み合わせで画像を読み込む
  for Size := Low(TPieceSize) to High(TPieceSize) do
  begin
    for PieceColor := Low(TPieceColor) to High(TPieceColor) do
    begin
      FileName := BasePath + SizeNames[Size] + '_' + ColorNames[PieceColor] + '.bmp';
      FPieces[Size, PieceColor] := TBitmap.Create;
      try
        if FileExists(FileName) then
          FPieces[Size, PieceColor].LoadFromFile(FileName)
        else
        begin
          // ファイルが見つからない場合は空のビットマップを作成
          FPieces[Size, PieceColor].Width := 60;
          FPieces[Size, PieceColor].Height := 60;
          FPieces[Size, PieceColor].Canvas.Brush.Color := 
            IfThen(PieceColor = pcRed, clRed, clBlue);
          FPieces[Size, PieceColor].Canvas.FillRect(0, 0, 60, 60);
        end;
      except
        // エラーが発生した場合も空のビットマップを作成
        FPieces[Size, PieceColor].Width := 60;
        FPieces[Size, PieceColor].Height := 60;
        FPieces[Size, PieceColor].Canvas.Brush.Color := 
          IfThen(PieceColor = pcRed, clRed, clBlue);
        FPieces[Size, PieceColor].Canvas.FillRect(0, 0, 60, 60);
      end;
    end;
  end;
end;

procedure TForm1.DrawBoard;
var
  i, j: Integer;
  CellWidth, CellHeight: Integer;
  X, Y: Integer;
  IsSelected: Boolean;
begin
  with PaintBox1.Canvas do
  begin
    // 背景を白に
    Brush.Color := clWhite;
    FillRect(0, 0, PaintBox1.Width, PaintBox1.Height);
    
    // ボードのグリッドを描画
    CellWidth := PaintBox1.Width div 3;
    CellHeight := PaintBox1.Height div 3;
    
    Pen.Color := clBlack;
    Pen.Width := 3;
    
    // 縦線
    for i := 1 to 2 do
    begin
      MoveTo(i * CellWidth, 0);
      LineTo(i * CellWidth, PaintBox1.Height);
    end;
    
    // 横線
    for i := 1 to 2 do
    begin
      MoveTo(0, i * CellHeight);
      LineTo(PaintBox1.Width, i * CellHeight);
    end;
    
    // 駒を描画
    for i := 0 to 2 do
    begin
      for j := 0 to 2 do
      begin
        X := j * CellWidth + CellWidth div 2;
        Y := i * CellHeight + CellHeight div 2;
        
        if Assigned(FBoard[i, j].Image) then
        begin
          IsSelected := (FSelection.SelectionType = stBoardPiece) and
                       (FSelection.BoardRow = i) and (FSelection.BoardCol = j);
          DrawPiece(PaintBox1.Canvas, X, Y, FBoard[i, j], IsSelected);
        end;
      end;
    end;
  end;
end;

procedure TForm1.DrawHandPieces(ACanvas: TCanvas; AColor: TPieceColor; AWidth, AHeight: Integer);
var
  Size: TPieceSize;
  i, Index: Integer;
  X, Y: Integer;
  PieceWidth, PieceHeight: Integer;
  Spacing: Integer;
  Piece: TGamePiece;
  IsSelected: Boolean;
  SizeNames: array[TPieceSize] of String = ('小', '中', '大');
begin
  // 背景を描画
  if AColor = pcRed then
    ACanvas.Brush.Color := $E0E0E0
  else
    ACanvas.Brush.Color := $E0E0FF;
  ACanvas.FillRect(0, 0, AWidth, AHeight);
  
  // 枠線
  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Width := 2;
  ACanvas.Rectangle(0, 0, AWidth, AHeight);
  
  // タイトル
  ACanvas.Font.Size := 12;
  ACanvas.Font.Style := [fsBold];
  if AColor = pcRed then
    ACanvas.TextOut(10, 5, '赤の持ち駒')
  else
    ACanvas.TextOut(10, 5, '青の持ち駒');
  
  // 駒のサイズ
  PieceWidth := 80;
  PieceHeight := 80;
  Spacing := 10;
  
  Index := 0;
  for Size := Low(TPieceSize) to High(TPieceSize) do
  begin
    for i := 0 to 1 do
    begin
      if FHandPieces[AColor, Size, i] then
      begin
        X := 10 + (Index mod 3) * (PieceWidth + Spacing);
        Y := 30 + (Index div 3) * (PieceHeight + Spacing);
        
        Piece.Size := Size;
        Piece.Color := AColor;
        Piece.Image := FPieces[Size, AColor];
        
        IsSelected := (FSelection.SelectionType = stHandPiece) and
                     (FSelection.Piece.Color = AColor) and
                     (FSelection.Piece.Size = Size) and
                     (FSelection.HandIndex = Index);
        
        DrawPiece(ACanvas, X + PieceWidth div 2, Y + PieceHeight div 2, Piece, IsSelected);
        
        // サイズラベル
        ACanvas.Font.Size := 9;
        ACanvas.Font.Style := [];
        ACanvas.TextOut(X + 5, Y + PieceHeight - 15, SizeNames[Size]);
      end;
      Inc(Index);
    end;
  end;
end;

procedure TForm1.DrawPiece(ACanvas: TCanvas; X, Y: Integer; Piece: TGamePiece; Selected: Boolean = False);
var
  Bitmap: TBitmap;
  DrawX, DrawY: Integer;
begin
  if not Assigned(Piece.Image) then
    Exit;
  
  Bitmap := Piece.Image;
  DrawX := X - Bitmap.Width div 2;
  DrawY := Y - Bitmap.Height div 2;
  
  // 選択されている場合は枠を描画
  if Selected then
  begin
    ACanvas.Pen.Color := clYellow;
    ACanvas.Pen.Width := 4;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle(DrawX - 5, DrawY - 5, 
                      DrawX + Bitmap.Width + 5, DrawY + Bitmap.Height + 5);
  end;
  
  ACanvas.Draw(DrawX, DrawY, Bitmap);
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  DrawBoard;
end;

function TForm1.GetCellFromPos(X, Y: Integer; out Col, Row: Integer): Boolean;
var
  CellWidth, CellHeight: Integer;
begin
  CellWidth := PaintBox1.Width div 3;
  CellHeight := PaintBox1.Height div 3;
  
  Col := X div CellWidth;
  Row := Y div CellHeight;
  
  Result := (Col >= 0) and (Col < 3) and (Row >= 0) and (Row < 3);
end;

function TForm1.GetHandPieceFromPos(X, Y: Integer; AColor: TPieceColor; out Size: TPieceSize; out Index: Integer): Boolean;
var
  PieceWidth, PieceHeight: Integer;
  Spacing: Integer;
  Col, Row: Integer;
  SizeIndex, PieceIndex: Integer;
begin
  Result := False;
  PieceWidth := 80;
  PieceHeight := 80;
  Spacing := 10;
  
  // タイトル部分をスキップ
  if Y < 30 then
    Exit;
  
  Col := (X - 10) div (PieceWidth + Spacing);
  Row := (Y - 30) div (PieceHeight + Spacing);
  
  if (Col < 0) or (Col >= 3) or (Row < 0) or (Row >= 2) then
    Exit;
  
  Index := Row * 3 + Col;
  
  // インデックスからサイズとピース番号を計算
  SizeIndex := Index div 2;
  PieceIndex := Index mod 2;
  
  if SizeIndex > Ord(High(TPieceSize)) then
    Exit;
  
  Size := TPieceSize(SizeIndex);
  
  // その駒が存在するか確認
  if FHandPieces[AColor, Size, PieceIndex] then
    Result := True;
end;

function TForm1.CanPlacePiece(ASize: TPieceSize; ACol, ARow: Integer): Boolean;
var
  ExistingPiece: TGamePiece;
begin
  Result := False;
  
  // ボードの範囲チェック
  if (ACol < 0) or (ACol > 2) or (ARow < 0) or (ARow > 2) then
    Exit;
  
  ExistingPiece := FBoard[ARow, ACol];
  
  // 空いているか、既存の駒より大きい場合は配置可能
  if not Assigned(ExistingPiece.Image) then
    Result := True
  else if Ord(ASize) > Ord(ExistingPiece.Size) then
    Result := True;
end;

procedure TForm1.UpdateSelectionDisplay;
begin
  PaintBox1.Invalidate;
  PaintBoxRed.Invalidate;
  PaintBoxBlue.Invalidate;
end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
  NewPiece: TGamePiece;
  ExistingPiece: TGamePiece;
begin
  if Button <> mbLeft then
    Exit;
  
  if GetCellFromPos(X, Y, Col, Row) then
  begin
    // 既存の駒を選択した場合
    if Assigned(FBoard[Row, Col].Image) and 
       (FBoard[Row, Col].Color = FCurrentPlayer) then
    begin
      // ボード上の駒を選択
      FSelection.SelectionType := stBoardPiece;
      FSelection.BoardCol := Col;
      FSelection.BoardRow := Row;
      FSelection.Piece := FBoard[Row, Col];
      UpdateSelectionDisplay;
      Label2.Caption := 'ボード上の駒を選択しました。移動先をクリックしてください。';
      Exit;
    end;
    
    // 持ち駒が選択されている場合
    if FSelection.SelectionType = stHandPiece then
    begin
      // 配置可能かチェック
      if CanPlacePiece(FSelection.Piece.Size, Col, Row) then
      begin
        // 既存の駒がある場合は取り除く（持ち駒に戻す）
        ExistingPiece := FBoard[Row, Col];
        if Assigned(ExistingPiece.Image) then
        begin
          // 持ち駒に戻す（簡略化：最初の空きスロットに戻す）
          // 実際のゲームでは、取り除いた駒を適切に管理する必要があります
        end;
        
        // 新しい駒を配置
        NewPiece := FSelection.Piece;
        FBoard[Row, Col] := NewPiece;
        
        // 持ち駒から削除
        FHandPieces[FSelection.Piece.Color, FSelection.Piece.Size, 
                   FSelection.HandIndex mod 2] := False;
        
        // 選択をクリア
        FSelection.SelectionType := stNone;
        
        // プレイヤーを切り替え
        if FCurrentPlayer = pcRed then
        begin
          FCurrentPlayer := pcBlue;
          Label1.Caption := '現在のプレイヤー: 青';
        end
        else
        begin
          FCurrentPlayer := pcRed;
          Label1.Caption := '現在のプレイヤー: 赤';
        end;
        
        UpdateSelectionDisplay;
        Label2.Caption := '持ち駒をクリックして選択、ボードをクリックして配置';
      end
      else
      begin
        Label2.Caption := 'その位置には配置できません（既存の駒が大きすぎます）';
      end;
    end
    // ボード上の駒が選択されている場合（移動）
    else if FSelection.SelectionType = stBoardPiece then
    begin
      // 同じ位置をクリックした場合は選択解除
      if (FSelection.BoardCol = Col) and (FSelection.BoardRow = Row) then
      begin
        FSelection.SelectionType := stNone;
        UpdateSelectionDisplay;
        Label2.Caption := '持ち駒をクリックして選択、ボードをクリックして配置';
      end
      // 移動先が有効な場合
      else if CanPlacePiece(FSelection.Piece.Size, Col, Row) then
      begin
        // 元の位置をクリア
        FBoard[FSelection.BoardRow, FSelection.BoardCol].Image := nil;
        
        // 新しい位置に配置
        FBoard[Row, Col] := FSelection.Piece;
        
        // 選択をクリア
        FSelection.SelectionType := stNone;
        
        // プレイヤーを切り替え
        if FCurrentPlayer = pcRed then
        begin
          FCurrentPlayer := pcBlue;
          Label1.Caption := '現在のプレイヤー: 青';
        end
        else
        begin
          FCurrentPlayer := pcRed;
          Label1.Caption := '現在のプレイヤー: 赤';
        end;
        
        UpdateSelectionDisplay;
        Label2.Caption := '持ち駒をクリックして選択、ボードをクリックして配置';
      end
      else
      begin
        Label2.Caption := 'その位置には移動できません（既存の駒が大きすぎます）';
      end;
    end;
  end;
end;

procedure TForm1.PaintBoxRedPaint(Sender: TObject);
begin
  DrawHandPieces(PaintBoxRed.Canvas, pcRed, PaintBoxRed.Width, PaintBoxRed.Height);
end;

procedure TForm1.PaintBoxRedMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Size: TPieceSize;
  Index: Integer;
  Piece: TGamePiece;
  SizeIndex, PieceIndex: Integer;
begin
  if Button <> mbLeft then
    Exit;
  
  if GetHandPieceFromPos(X, Y, pcRed, Size, Index) then
  begin
    // 現在のプレイヤーでない場合は無視
    if FCurrentPlayer <> pcRed then
      Exit;
    
    SizeIndex := Index div 2;
    PieceIndex := Index mod 2;
    
    Piece.Size := Size;
    Piece.Color := pcRed;
    Piece.Image := FPieces[Size, pcRed];
    
    FSelection.SelectionType := stHandPiece;
    FSelection.HandIndex := Index;
    FSelection.Piece := Piece;
    FSelection.BoardCol := -1;
    FSelection.BoardRow := -1;
    
    UpdateSelectionDisplay;
    Label2.Caption := '赤の持ち駒を選択しました。ボードをクリックして配置してください。';
  end;
end;

procedure TForm1.PaintBoxBluePaint(Sender: TObject);
begin
  DrawHandPieces(PaintBoxBlue.Canvas, pcBlue, PaintBoxBlue.Width, PaintBoxBlue.Height);
end;

procedure TForm1.PaintBoxBlueMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Size: TPieceSize;
  Index: Integer;
  Piece: TGamePiece;
  SizeIndex, PieceIndex: Integer;
begin
  if Button <> mbLeft then
    Exit;
  
  if GetHandPieceFromPos(X, Y, pcBlue, Size, Index) then
  begin
    // 現在のプレイヤーでない場合は無視
    if FCurrentPlayer <> pcBlue then
      Exit;
    
    SizeIndex := Index div 2;
    PieceIndex := Index mod 2;
    
    Piece.Size := Size;
    Piece.Color := pcBlue;
    Piece.Image := FPieces[Size, pcBlue];
    
    FSelection.SelectionType := stHandPiece;
    FSelection.HandIndex := Index;
    FSelection.Piece := Piece;
    FSelection.BoardCol := -1;
    FSelection.BoardRow := -1;
    
    UpdateSelectionDisplay;
    Label2.Caption := '青の持ち駒を選択しました。ボードをクリックして配置してください。';
  end;
end;

end.

