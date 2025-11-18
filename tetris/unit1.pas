unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  LCLType, Math;

type
  TTetrominoType = (ttI, ttO, ttT, ttS, ttZ, ttJ, ttL);
  TCellState = (csEmpty, csFilled);
  
  TPoint = record
    X, Y: Integer;
  end;
  
  TTetromino = record
    Shape: array[0..3, 0..3] of Boolean;
    Color: TColor;
    Size: Integer;
  end;
  
  TGameBoard = array[0..19, 0..9] of TCellState;
  TColorBoard = array[0..19, 0..9] of TColor;

  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    Timer1: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    LabelScore: TLabel;
    LabelLines: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    Board: TGameBoard;
    ColorBoard: TColorBoard;
    CurrentPiece: TTetromino;
    CurrentX, CurrentY: Integer;
    CurrentRotation: Integer;
    Score: Integer;
    Lines: Integer;
    GameOver: Boolean;
    FallingSpeed: Integer;
    FallCounter: Integer;
    
    procedure InitializeBoard;
    procedure InitializeTetrominoes;
    function GetTetromino(AType: TTetrominoType): TTetromino;
    function CanPlacePiece(AX, AY, ARotation: Integer): Boolean;
    procedure PlacePiece;
    procedure ClearLines;
    procedure SpawnNewPiece;
    procedure DrawBoard;
    procedure DrawPiece(AX, AY: Integer; APiece: TTetromino; ARotation: Integer);
    function RotatePiece(APiece: TTetromino; ARotation: Integer): TTetromino;
    procedure GameOverScreen;
  public

  end;

var
  Form1: TForm1;
  Tetrominoes: array[TTetrominoType] of TTetromino;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  PaintBox1.Width := 300;
  PaintBox1.Height := 600;
  Form1.Width := 400;
  Form1.Height := 700;
  Form1.Caption := 'テトリス';
  Label1.Caption := 'スコア:';
  Label2.Caption := 'ライン:';
  LabelScore.Caption := '0';
  LabelLines.Caption := '0';
  
  InitializeBoard;
  InitializeTetrominoes;
  Randomize;
  Score := 0;
  Lines := 0;
  GameOver := False;
  FallingSpeed := 30;
  FallCounter := 0;
  SpawnNewPiece;
  
  // 初期描画
  PaintBox1.Invalidate;
end;

procedure TForm1.InitializeBoard;
var
  X, Y: Integer;
begin
  for Y := 0 to 19 do
    for X := 0 to 9 do
    begin
      Board[Y, X] := csEmpty;
      ColorBoard[Y, X] := clBlack;
    end;
end;

procedure TForm1.InitializeTetrominoes;
begin
  // I型
  Tetrominoes[ttI].Size := 4;
  Tetrominoes[ttI].Color := clAqua;
  FillChar(Tetrominoes[ttI].Shape, SizeOf(Tetrominoes[ttI].Shape), False);
  Tetrominoes[ttI].Shape[1, 0] := True;
  Tetrominoes[ttI].Shape[1, 1] := True;
  Tetrominoes[ttI].Shape[1, 2] := True;
  Tetrominoes[ttI].Shape[1, 3] := True;
  
  // O型
  Tetrominoes[ttO].Size := 2;
  Tetrominoes[ttO].Color := clYellow;
  FillChar(Tetrominoes[ttO].Shape, SizeOf(Tetrominoes[ttO].Shape), False);
  Tetrominoes[ttO].Shape[0, 0] := True;
  Tetrominoes[ttO].Shape[0, 1] := True;
  Tetrominoes[ttO].Shape[1, 0] := True;
  Tetrominoes[ttO].Shape[1, 1] := True;
  
  // T型
  Tetrominoes[ttT].Size := 3;
  Tetrominoes[ttT].Color := clPurple;
  FillChar(Tetrominoes[ttT].Shape, SizeOf(Tetrominoes[ttT].Shape), False);
  Tetrominoes[ttT].Shape[0, 1] := True;
  Tetrominoes[ttT].Shape[1, 0] := True;
  Tetrominoes[ttT].Shape[1, 1] := True;
  Tetrominoes[ttT].Shape[1, 2] := True;
  
  // S型
  Tetrominoes[ttS].Size := 3;
  Tetrominoes[ttS].Color := clGreen;
  FillChar(Tetrominoes[ttS].Shape, SizeOf(Tetrominoes[ttS].Shape), False);
  Tetrominoes[ttS].Shape[0, 1] := True;
  Tetrominoes[ttS].Shape[0, 2] := True;
  Tetrominoes[ttS].Shape[1, 0] := True;
  Tetrominoes[ttS].Shape[1, 1] := True;
  
  // Z型
  Tetrominoes[ttZ].Size := 3;
  Tetrominoes[ttZ].Color := clRed;
  FillChar(Tetrominoes[ttZ].Shape, SizeOf(Tetrominoes[ttZ].Shape), False);
  Tetrominoes[ttZ].Shape[0, 0] := True;
  Tetrominoes[ttZ].Shape[0, 1] := True;
  Tetrominoes[ttZ].Shape[1, 1] := True;
  Tetrominoes[ttZ].Shape[1, 2] := True;
  
  // J型
  Tetrominoes[ttJ].Size := 3;
  Tetrominoes[ttJ].Color := clBlue;
  FillChar(Tetrominoes[ttJ].Shape, SizeOf(Tetrominoes[ttJ].Shape), False);
  Tetrominoes[ttJ].Shape[0, 0] := True;
  Tetrominoes[ttJ].Shape[1, 0] := True;
  Tetrominoes[ttJ].Shape[1, 1] := True;
  Tetrominoes[ttJ].Shape[1, 2] := True;
  
  // L型
  Tetrominoes[ttL].Size := 3;
  Tetrominoes[ttL].Color := RGBToColor(255, 165, 0); // オレンジ
  FillChar(Tetrominoes[ttL].Shape, SizeOf(Tetrominoes[ttL].Shape), False);
  Tetrominoes[ttL].Shape[0, 2] := True;
  Tetrominoes[ttL].Shape[1, 0] := True;
  Tetrominoes[ttL].Shape[1, 1] := True;
  Tetrominoes[ttL].Shape[1, 2] := True;
end;

function TForm1.GetTetromino(AType: TTetrominoType): TTetromino;
begin
  Result := Tetrominoes[AType];
end;

function TForm1.RotatePiece(APiece: TTetromino; ARotation: Integer): TTetromino;
var
  I, J, K, Size: Integer;
  Temp: array[0..3, 0..3] of Boolean;
begin
  Result := APiece;
  Size := APiece.Size;
  ARotation := ARotation mod 4;
  
  // 回転を適用（90度ずつ時計回り）
  for K := 0 to ARotation - 1 do
  begin
    Temp := Result.Shape;
    for I := 0 to Size - 1 do
      for J := 0 to Size - 1 do
        Result.Shape[J, Size - 1 - I] := Temp[I, J];
  end;
end;

function TForm1.CanPlacePiece(AX, AY, ARotation: Integer): Boolean;
var
  I, J: Integer;
  RotatedPiece: TTetromino;
begin
  Result := True;
  RotatedPiece := RotatePiece(CurrentPiece, ARotation);
  
  for I := 0 to RotatedPiece.Size - 1 do
    for J := 0 to RotatedPiece.Size - 1 do
      if RotatedPiece.Shape[I, J] then
      begin
        // ボードの範囲外チェック
        if (AX + J < 0) or (AX + J >= 10) or (AY + I >= 20) then
        begin
          Result := False;
          Exit;
        end;
        
        // 上方向はチェックしない（新しいピースの生成時）
        if (AY + I >= 0) and (Board[AY + I, AX + J] = csFilled) then
        begin
          Result := False;
          Exit;
        end;
      end;
end;

procedure TForm1.PlacePiece;
var
  I, J: Integer;
  RotatedPiece: TTetromino;
begin
  RotatedPiece := RotatePiece(CurrentPiece, CurrentRotation);
  
  for I := 0 to RotatedPiece.Size - 1 do
    for J := 0 to RotatedPiece.Size - 1 do
      if RotatedPiece.Shape[I, J] then
      begin
        if (CurrentY + I >= 0) and (CurrentY + I < 20) and
           (CurrentX + J >= 0) and (CurrentX + J < 10) then
        begin
          Board[CurrentY + I, CurrentX + J] := csFilled;
          ColorBoard[CurrentY + I, CurrentX + J] := RotatedPiece.Color;
        end;
      end;
  
  ClearLines;
  SpawnNewPiece;
end;

procedure TForm1.ClearLines;
var
  Y, X, I: Integer;
  FullLine: Boolean;
  LinesCleared: Integer;
begin
  LinesCleared := 0;
  Y := 19;
  
  while Y >= 0 do
  begin
    FullLine := True;
    for X := 0 to 9 do
      if Board[Y, X] = csEmpty then
      begin
        FullLine := False;
        Break;
      end;
    
    if FullLine then
    begin
      // ラインを削除
      for I := Y downto 1 do
      begin
        for X := 0 to 9 do
        begin
          Board[I, X] := Board[I - 1, X];
          ColorBoard[I, X] := ColorBoard[I - 1, X];
        end;
      end;
      
      // 最上部をクリア
      for X := 0 to 9 do
      begin
        Board[0, X] := csEmpty;
        ColorBoard[0, X] := clBlack;
      end;
      
      Inc(LinesCleared);
      // Yはそのまま（次の行をチェック）
    end
    else
      Dec(Y);
  end;
  
  if LinesCleared > 0 then
  begin
    Lines := Lines + LinesCleared;
    Score := Score + LinesCleared * 100 * LinesCleared; // 複数ラインでボーナス
    LabelScore.Caption := IntToStr(Score);
    LabelLines.Caption := IntToStr(Lines);
    
    // スピードアップ
    if Lines mod 10 = 0 then
      FallingSpeed := Max(5, FallingSpeed - 2);
  end;
end;

procedure TForm1.SpawnNewPiece;
var
  PieceType: TTetrominoType;
begin
  PieceType := TTetrominoType(Random(7));
  CurrentPiece := GetTetromino(PieceType);
  CurrentX := 3;
  CurrentY := 0;
  CurrentRotation := 0;
  
  // ゲームオーバーチェック
  if not CanPlacePiece(CurrentX, CurrentY, CurrentRotation) then
  begin
    GameOver := True;
    Timer1.Enabled := False;
  end;
end;

procedure TForm1.DrawBoard;
var
  X, Y: Integer;
  CellSize: Integer;
  Rect: TRect;
begin
  if PaintBox1.Height <= 0 then Exit;
  
  CellSize := PaintBox1.Height div 20;
  if CellSize <= 0 then Exit;
  
  with PaintBox1.Canvas do
  begin
    Brush.Style := bsSolid;
    Pen.Style := psSolid;
    
    // ボードの描画（背景はPaintBox1Paintで既にクリア済み）
    for Y := 0 to 19 do
      for X := 0 to 9 do
      begin
        Rect.Left := X * CellSize;
        Rect.Top := Y * CellSize;
        Rect.Right := (X + 1) * CellSize;
        Rect.Bottom := (Y + 1) * CellSize;
        
        if Board[Y, X] = csFilled then
        begin
          Brush.Color := ColorBoard[Y, X];
          FillRect(Rect);
          Pen.Color := clWhite;
          Pen.Width := 1;
          Rectangle(Rect);
        end
        else
        begin
          // 空のセルは薄いグレーで表示
          Brush.Color := RGBToColor(40, 40, 40);
          FillRect(Rect);
          Pen.Color := RGBToColor(60, 60, 60);
          Pen.Width := 1;
          Rectangle(Rect);
        end;
      end;
  end;
end;

procedure TForm1.DrawPiece(AX, AY: Integer; APiece: TTetromino; ARotation: Integer);
var
  I, J: Integer;
  CellSize: Integer;
  Rect: TRect;
  RotatedPiece: TTetromino;
  DrawX, DrawY: Integer;
begin
  if PaintBox1.Height <= 0 then Exit;
  
  CellSize := PaintBox1.Height div 20;
  if CellSize <= 0 then Exit;
  
  RotatedPiece := RotatePiece(APiece, ARotation);
  
  with PaintBox1.Canvas do
  begin
    Brush.Style := bsSolid;
    Pen.Style := psSolid;
    Pen.Width := 1;
    
    // ピースの各ブロックを描画
    for I := 0 to RotatedPiece.Size - 1 do
      for J := 0 to RotatedPiece.Size - 1 do
        if RotatedPiece.Shape[I, J] then
        begin
          DrawX := AX + J;
          DrawY := AY + I;
          
          // ボード内のセルのみ描画
          if (DrawX >= 0) and (DrawX < 10) and 
             (DrawY >= 0) and (DrawY < 20) then
          begin
            Rect.Left := DrawX * CellSize;
            Rect.Top := DrawY * CellSize;
            Rect.Right := (DrawX + 1) * CellSize;
            Rect.Bottom := (DrawY + 1) * CellSize;
            
            Brush.Color := RotatedPiece.Color;
            FillRect(Rect);
            Pen.Color := clWhite;
            Rectangle(Rect);
          end;
        end;
  end;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  with PaintBox1.Canvas do
  begin
    // まず背景をクリア
    Brush.Style := bsSolid;
    Brush.Color := clBlack;
    FillRect(0, 0, PaintBox1.Width, PaintBox1.Height);
  end;
  
  // ボードを描画
  DrawBoard;
  
  // その上に現在のピースを描画（ゲームオーバーでない場合）
  if not GameOver then
    DrawPiece(CurrentX, CurrentY, CurrentPiece, CurrentRotation)
  else
    GameOverScreen;
end;

procedure TForm1.GameOverScreen;
begin
  with PaintBox1.Canvas do
  begin
    Font.Size := 24;
    Font.Color := clRed;
    Font.Style := [fsBold];
    Brush.Style := bsClear;
    TextOut(50, 250, 'ゲームオーバー');
    Font.Size := 12;
    TextOut(80, 300, 'スペースキーで再開');
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if GameOver then
    Exit;
    
  Inc(FallCounter);
  if FallCounter >= FallingSpeed then
  begin
    FallCounter := 0;
    
    if CanPlacePiece(CurrentX, CurrentY + 1, CurrentRotation) then
      Inc(CurrentY)
    else
      PlacePiece;
    
    PaintBox1.Invalidate;
  end;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  NewX, NewY, NewRotation: Integer;
begin
  if GameOver then
  begin
    if Key = VK_SPACE then
    begin
      InitializeBoard;
      Score := 0;
      Lines := 0;
      FallingSpeed := 30;
      FallCounter := 0;
      GameOver := False;
      Timer1.Enabled := True;
      SpawnNewPiece;
      LabelScore.Caption := '0';
      LabelLines.Caption := '0';
      PaintBox1.Invalidate;
    end;
    Exit;
  end;
  
  case Key of
    VK_LEFT, Ord('A'), Ord('a'):
    begin
      NewX := CurrentX - 1;
      if CanPlacePiece(NewX, CurrentY, CurrentRotation) then
        CurrentX := NewX;
      PaintBox1.Invalidate;
    end;
    
    VK_RIGHT, Ord('D'), Ord('d'):
    begin
      NewX := CurrentX + 1;
      if CanPlacePiece(NewX, CurrentY, CurrentRotation) then
        CurrentX := NewX;
      PaintBox1.Invalidate;
    end;
    
    VK_DOWN, Ord('S'), Ord('s'):
    begin
      NewY := CurrentY + 1;
      if CanPlacePiece(CurrentX, NewY, CurrentRotation) then
      begin
        CurrentY := NewY;
        Score := Score + 1; // 下キーでスコア追加
        LabelScore.Caption := IntToStr(Score);
      end
      else
        PlacePiece;
      PaintBox1.Invalidate;
    end;
    
    VK_UP, Ord('W'), Ord('w'):
    begin
      NewRotation := CurrentRotation + 1;
      if CanPlacePiece(CurrentX, CurrentY, NewRotation) then
        CurrentRotation := NewRotation;
      PaintBox1.Invalidate;
    end;
    
    VK_SPACE:
    begin
      // ハードドロップ
      while CanPlacePiece(CurrentX, CurrentY + 1, CurrentRotation) do
        Inc(CurrentY);
      PlacePiece;
      PaintBox1.Invalidate;
    end;
  end;
end;

end.
