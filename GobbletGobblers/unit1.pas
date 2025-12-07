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

  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FPieces: array[TPieceSize, TPieceColor] of TBitmap;
    FBoard: array[0..2, 0..2] of TGamePiece;
    FSelectedPiece: TGamePiece;
    FCurrentPlayer: TPieceColor;
    procedure LoadPieces;
    procedure DrawBoard;
    procedure DrawPiece(ACanvas: TCanvas; X, Y: Integer; Piece: TGamePiece);
    function GetCellFromPos(X, Y: Integer; out Col, Row: Integer): Boolean;
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
  
  // 選択中の駒を初期化
  FSelectedPiece.Size := psSmall;
  FSelectedPiece.Color := pcRed;
  FSelectedPiece.Image := nil;
  
  FCurrentPlayer := pcRed;
  
  // 駒の画像を読み込む
  LoadPieces;
  
  Label1.Caption := '現在のプレイヤー: 赤';
  Label2.Caption := '駒をクリックして選択、ボードをクリックして配置';
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
          DrawPiece(PaintBox1.Canvas, X, Y, FBoard[i, j]);
      end;
    end;
  end;
end;

procedure TForm1.DrawPiece(ACanvas: TCanvas; X, Y: Integer; Piece: TGamePiece);
var
  Bitmap: TBitmap;
  DrawX, DrawY: Integer;
begin
  if not Assigned(Piece.Image) then
    Exit;
  
  Bitmap := Piece.Image;
  DrawX := X - Bitmap.Width div 2;
  DrawY := Y - Bitmap.Height div 2;
  
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

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
  NewPiece: TGamePiece;
begin
  if Button <> mbLeft then
    Exit;
  
  if GetCellFromPos(X, Y, Col, Row) then
  begin
    // 新しい駒を作成
    NewPiece.Size := FSelectedPiece.Size;
    NewPiece.Color := FCurrentPlayer;
    NewPiece.Image := FPieces[NewPiece.Size, NewPiece.Color];
    
    // ボードに配置
    FBoard[Row, Col] := NewPiece;
    
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
    
    // 選択中の駒を更新
    FSelectedPiece.Size := TPieceSize((Ord(FSelectedPiece.Size) + 1) mod 3);
    FSelectedPiece.Color := FCurrentPlayer;
    FSelectedPiece.Image := FPieces[FSelectedPiece.Size, FSelectedPiece.Color];
    
    PaintBox1.Invalidate;
  end;
end;

end.

