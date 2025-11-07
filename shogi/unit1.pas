unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ShogiUnit, Types;

type
  TForm1 = class(TForm)
    MainPanel: TPanel;
    BoardPanel: TPanel;
    StatusLabel: TLabel;
    SenteMochigomaPanel: TPanel;
    GoteMochigomaPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BoardCellClick(Sender: TObject);
    procedure MochigomaClick(Sender: TObject);
  private
    FShogiGame: TShogiGame;
    FBoardCells: array[1..9, 1..9] of TPanel;
    FBoardImages: array[1..9, 1..9] of TImage;
    FKomaBitmaps: array[TKomaKind] of TBitmap;
    FSenteMochigomaButtons: array[TKomaKind] of TPanel;
    FGoteMochigomaButtons: array[TKomaKind] of TPanel;
    FSenteMochigomaLabels: array[TKomaKind] of TLabel;
    FGoteMochigomaLabels: array[TKomaKind] of TLabel;
    FSelectedX, FSelectedY: Integer;
    FSelectedMochigoma: TKomaKind;
    procedure InitializeBoard;
    procedure InitializeMochigoma;
    procedure InitializeKomaImages;
    procedure LoadKomaImage(Kind: TKomaKind; const FileName: String);
    procedure UpdateBoard;
    procedure UpdateMochigoma;
    procedure UpdateStatus;
    procedure DrawKomaOnImage(Image: TImage; Koma: TKoma);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  K: TKomaKind;
begin
  FShogiGame := TShogiGame.Create;
  FSelectedX := 0;
  FSelectedY := 0;
  FSelectedMochigoma := kkNone;
  
  // 駒の画像を初期化
  for K := Low(TKomaKind) to High(TKomaKind) do
    FKomaBitmaps[K] := nil;
  InitializeKomaImages;
  
  InitializeBoard;
  InitializeMochigoma;
  UpdateBoard;
  UpdateMochigoma;
  UpdateStatus;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  K: TKomaKind;
begin
  FShogiGame.Free;
  
  // 画像リソースを解放
  for K := Low(TKomaKind) to High(TKomaKind) do
    if Assigned(FKomaBitmaps[K]) then
      FKomaBitmaps[K].Free;
end;

procedure TForm1.InitializeBoard;
var
  X, Y: Integer;
  Cell: TPanel;
  KomaImage: TImage;
  CellSize: Integer;
begin
  CellSize := 50;
  
  // 盤面パネルの設定（先手の駒台の右側に配置）
  BoardPanel.Width := CellSize * 9 + 20;
  BoardPanel.Height := CellSize * 9 + 20;
  BoardPanel.Left := 170; // 先手の駒台（150） + マージン（10） + 10
  BoardPanel.Top := 50;
  BoardPanel.Color := clCream;
  BoardPanel.BevelOuter := bvLowered;
  
  // 9x9のセルを作成
  for Y := 1 to 9 do
  begin
    for X := 1 to 9 do
    begin
      // パネルを作成
      Cell := TPanel.Create(Self);
      Cell.Parent := BoardPanel;
      Cell.Left := 10 + (X - 1) * CellSize;
      Cell.Top := 10 + (9 - Y) * CellSize; // 将棋盤は下から上に1-9
      Cell.Width := CellSize;
      Cell.Height := CellSize;
      Cell.Caption := '';
      Cell.Color := clWhite;
      Cell.BevelOuter := bvRaised;
      Cell.OnClick := @BoardCellClick;
      Cell.Tag := (X - 1) * 10 + (Y - 1); // XとYを保存
      FBoardCells[X, Y] := Cell;
      
      // 画像コンポーネントを作成
      KomaImage := TImage.Create(Self);
      KomaImage.Parent := Cell;
      KomaImage.Left := 2;
      KomaImage.Top := 2;
      KomaImage.Width := CellSize - 4;
      KomaImage.Height := CellSize - 4;
      KomaImage.Stretch := True;
      KomaImage.Proportional := True;
      KomaImage.Center := True;
      KomaImage.OnClick := @BoardCellClick;
      KomaImage.Tag := (X - 1) * 10 + (Y - 1);
      FBoardImages[X, Y] := KomaImage;
    end;
  end;
  
  // フォームのサイズ調整（先手の駒台 + 盤面 + 後手の駒台）
  // 後手の駒台の位置はInitializeMochigomaで設定されるため、ここでは仮の値を使用
  Width := 170 + BoardPanel.Width + 10 + 150 + 20; // 先手駒台(150+10) + 盤面 + 後手駒台(150+10) + マージン
  Height := BoardPanel.Height + 120;
  Caption := '将棋';
end;

procedure TForm1.InitializeMochigoma;
var
  K: TKomaKind;
  Button: TPanel;
  KomaImage: TImage;
  CountLabel: TLabel;
  ButtonSize: Integer;
  X, Y: Integer;
  KomaOrder: array[0..6] of TKomaKind = (kkHisha, kkKaku, kkKin, kkGin, kkKei, kkKyo, kkFu);
begin
  ButtonSize := 40;
  
  // 先手の駒台（画面左側）
  SenteMochigomaPanel.Left := 10;
  SenteMochigomaPanel.Top := BoardPanel.Top;
  SenteMochigomaPanel.Width := 150; // 枚数表示のスペースを追加
  SenteMochigomaPanel.Height := BoardPanel.Height;
  SenteMochigomaPanel.Caption := '先手';
  SenteMochigomaPanel.Color := clSilver;
  SenteMochigomaPanel.BevelOuter := bvLowered;
  
  // 後手の駒台（画面右側）
  GoteMochigomaPanel.Left := BoardPanel.Left + BoardPanel.Width + 10;
  GoteMochigomaPanel.Top := BoardPanel.Top;
  GoteMochigomaPanel.Width := 150; // 枚数表示のスペースを追加
  GoteMochigomaPanel.Height := BoardPanel.Height;
  GoteMochigomaPanel.Caption := '後手';
  GoteMochigomaPanel.Color := clSilver;
  GoteMochigomaPanel.BevelOuter := bvLowered;
  
  // 先手の持ち駒ボタンを作成
  for K := Low(TKomaKind) to High(TKomaKind) do
  begin
    FSenteMochigomaButtons[K] := nil;
    FSenteMochigomaLabels[K] := nil;
  end;
  
  for X := 0 to 6 do
  begin
    K := KomaOrder[X];
    Button := TPanel.Create(Self);
    Button.Parent := SenteMochigomaPanel;
    Button.Left := 10;
    Button.Top := 30 + X * (ButtonSize + 5);
    Button.Width := ButtonSize + 30; // 枚数表示のスペースを追加
    Button.Height := ButtonSize;
    Button.Caption := '';
    Button.Color := clWhite;
    Button.BevelOuter := bvRaised;
    Button.OnClick := @MochigomaClick;
    Button.Tag := Ord(K);
    FSenteMochigomaButtons[K] := Button;
    
    KomaImage := TImage.Create(Self);
    KomaImage.Parent := Button;
    KomaImage.Left := 2;
    KomaImage.Top := 2;
    KomaImage.Width := ButtonSize - 4;
    KomaImage.Height := ButtonSize - 4;
    KomaImage.Stretch := True;
    KomaImage.Proportional := True;
    KomaImage.Center := True;
    KomaImage.OnClick := @MochigomaClick;
    KomaImage.Tag := Ord(K);
    
    // 枚数表示用のLabelを作成
    CountLabel := TLabel.Create(Self);
    CountLabel.Parent := Button;
    CountLabel.Left := ButtonSize + 2;
    CountLabel.Top := (ButtonSize - 20) div 2; // フォントサイズ12を考慮した高さ
    CountLabel.Caption := '';
    CountLabel.Font.Size := 12;
    CountLabel.Font.Style := [fsBold];
    CountLabel.Font.Color := clRed;
    CountLabel.AutoSize := True;
    FSenteMochigomaLabels[K] := CountLabel;
  end;
  
  // 後手の持ち駒ボタンを作成
  for K := Low(TKomaKind) to High(TKomaKind) do
  begin
    FGoteMochigomaButtons[K] := nil;
    FGoteMochigomaLabels[K] := nil;
  end;
  
  for X := 0 to 6 do
  begin
    K := KomaOrder[X];
    Button := TPanel.Create(Self);
    Button.Parent := GoteMochigomaPanel;
    Button.Left := 10;
    Button.Top := 30 + X * (ButtonSize + 5);
    Button.Width := ButtonSize + 30; // 枚数表示のスペースを追加
    Button.Height := ButtonSize;
    Button.Caption := '';
    Button.Color := clWhite;
    Button.BevelOuter := bvRaised;
    Button.OnClick := @MochigomaClick;
    Button.Tag := Ord(K) + 100; // 後手は100を加算
    FGoteMochigomaButtons[K] := Button;
    
    KomaImage := TImage.Create(Self);
    KomaImage.Parent := Button;
    KomaImage.Left := 2;
    KomaImage.Top := 2;
    KomaImage.Width := ButtonSize - 4;
    KomaImage.Height := ButtonSize - 4;
    KomaImage.Stretch := True;
    KomaImage.Proportional := True;
    KomaImage.Center := True;
    KomaImage.OnClick := @MochigomaClick;
    KomaImage.Tag := Ord(K) + 100;
    
    // 枚数表示用のLabelを作成
    CountLabel := TLabel.Create(Self);
    CountLabel.Parent := Button;
    CountLabel.Left := ButtonSize + 2;
    CountLabel.Top := (ButtonSize - 20) div 2; // フォントサイズ12を考慮した高さ
    CountLabel.Caption := '';
    CountLabel.Font.Size := 12;
    CountLabel.Font.Style := [fsBold];
    CountLabel.Font.Color := clRed;
    CountLabel.AutoSize := True;
    FGoteMochigomaLabels[K] := CountLabel;
  end;
end;

procedure TForm1.UpdateBoard;
var
  X, Y: Integer;
  Koma: TKoma;
  Cell: TPanel;
begin
  for Y := 1 to 9 do
  begin
    for X := 1 to 9 do
    begin
      Cell := FBoardCells[X, Y];
      Koma := FShogiGame.GetKoma(X, Y);
      
      // 選択中のセルをハイライト
      if (FSelectedX = X) and (FSelectedY = Y) then
        Cell.Color := clYellow
      else
        Cell.Color := clWhite;
      
      // 駒の画像を描画
      DrawKomaOnImage(FBoardImages[X, Y], Koma);
    end;
  end;
  UpdateMochigoma;
end;

procedure TForm1.UpdateMochigoma;
var
  K: TKomaKind;
  Mochigoma: TMochigoma;
  Button: TPanel;
  Koma: TKoma;
  Count: Integer;
  KomaOrder: array[0..6] of TKomaKind = (kkHisha, kkKaku, kkKin, kkGin, kkKei, kkKyo, kkFu);
  X: Integer;
begin
  // 先手の持ち駒を更新
  Mochigoma := FShogiGame.GetMochigoma(True);
  for X := 0 to 6 do
  begin
    K := KomaOrder[X];
    Button := FSenteMochigomaButtons[K];
    if Assigned(Button) then
    begin
      case K of
        kkHisha: Count := Mochigoma.Hisha;
        kkKaku: Count := Mochigoma.Kaku;
        kkKin: Count := Mochigoma.Kin;
        kkGin: Count := Mochigoma.Gin;
        kkKei: Count := Mochigoma.Kei;
        kkKyo: Count := Mochigoma.Kyo;
        kkFu: Count := Mochigoma.Fu;
        else Count := 0;
      end;
      
      if Count > 0 then
      begin
        Button.Visible := True;
        Koma.Kind := K;
        Koma.IsSente := True;
        Koma.IsNari := False;
        // 画像を描画（最初の子要素のTImageを取得）
        if Button.ControlCount > 0 then
          DrawKomaOnImage(TImage(Button.Controls[0]), Koma);
        
        // 2枚以上あるときだけ枚数を表示（Labelに表示）
        if Assigned(FSenteMochigomaLabels[K]) then
        begin
          if Count >= 2 then
            FSenteMochigomaLabels[K].Caption := IntToStr(Count)
          else
            FSenteMochigomaLabels[K].Caption := '';
        end;
        
        // 選択中のハイライト
        if FSelectedMochigoma = K then
          Button.Color := clYellow
        else
          Button.Color := clWhite;
      end
      else
      begin
        Button.Visible := False;
      end;
    end;
  end;
  
  // 後手の持ち駒を更新
  Mochigoma := FShogiGame.GetMochigoma(False);
  for X := 0 to 6 do
  begin
    K := KomaOrder[X];
    Button := FGoteMochigomaButtons[K];
    if Assigned(Button) then
    begin
      case K of
        kkHisha: Count := Mochigoma.Hisha;
        kkKaku: Count := Mochigoma.Kaku;
        kkKin: Count := Mochigoma.Kin;
        kkGin: Count := Mochigoma.Gin;
        kkKei: Count := Mochigoma.Kei;
        kkKyo: Count := Mochigoma.Kyo;
        kkFu: Count := Mochigoma.Fu;
        else Count := 0;
      end;
      
      if Count > 0 then
      begin
        Button.Visible := True;
        Koma.Kind := K;
        Koma.IsSente := False;
        Koma.IsNari := False;
        // 画像を描画
        if Button.ControlCount > 0 then
          DrawKomaOnImage(TImage(Button.Controls[0]), Koma);
        
        // 2枚以上あるときだけ枚数を表示（Labelに表示）
        if Assigned(FGoteMochigomaLabels[K]) then
        begin
          if Count >= 2 then
            FGoteMochigomaLabels[K].Caption := IntToStr(Count)
          else
            FGoteMochigomaLabels[K].Caption := '';
        end;
        
        // 選択中のハイライト
        if FSelectedMochigoma = K then
          Button.Color := clYellow
        else
          Button.Color := clWhite;
      end
      else
      begin
        Button.Visible := False;
      end;
    end;
  end;
end;

procedure TForm1.UpdateStatus;
begin
  if FShogiGame.GetTurn = ttSente then
    StatusLabel.Caption := '手番: 先手'
  else
    StatusLabel.Caption := '手番: 後手';
end;

procedure TForm1.BoardCellClick(Sender: TObject);
var
  X, Y: Integer;
  CellTag: Integer;
  Koma: TKoma;
  Panel: TPanel;
  FromKoma: TKoma;
  DoNaru: Boolean;
begin
  // TPanelまたはTImageのどちらからでもクリックを受け付ける
  if Sender is TPanel then
  begin
    Panel := TPanel(Sender);
    CellTag := Panel.Tag;
  end
  else if Sender is TImage then
  begin
    // TImageの場合は親のTPanelを取得
    Panel := TPanel(TImage(Sender).Parent);
    CellTag := Panel.Tag;
  end
  else
    Exit;
  X := (CellTag div 10) + 1;
  Y := (CellTag mod 10) + 1;
  
  Koma := FShogiGame.GetKoma(X, Y);
  
  // 持ち駒が選択されている場合
  if FSelectedMochigoma <> kkNone then
  begin
    // 持ち駒を打つ
    if FShogiGame.DropKoma(FSelectedMochigoma, X, Y) then
    begin
      FShogiGame.ChangeTurn;
      FSelectedMochigoma := kkNone;
      UpdateStatus;
      UpdateMochigoma;
    end;
  end
  // 既に選択されている場合
  else if (FSelectedX = X) and (FSelectedY = Y) then
  begin
    // 選択を解除
    FSelectedX := 0;
    FSelectedY := 0;
  end
  // 駒が選択されている場合
  else if (FSelectedX > 0) and (FSelectedY > 0) then
  begin
    // 移動先が敵陣で成れるかチェック
    FromKoma := FShogiGame.GetKoma(FSelectedX, FSelectedY);
    DoNaru := False;
    
    // 強制成りでない場合、敵陣に入ったら成り確認
    if not FShogiGame.MustNaru(FromKoma, Y) and 
       FShogiGame.CanNaru(FromKoma, Y, FromKoma.IsSente) then
    begin
      // 成り確認ダイアログ
      if MessageDlg('成りますか？', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        DoNaru := True;
    end;
    
    // 移動を試みる
    if FShogiGame.MoveKoma(FSelectedX, FSelectedY, X, Y, DoNaru) then
    begin
      FShogiGame.ChangeTurn;
      FSelectedX := 0;
      FSelectedY := 0;
      UpdateStatus;
      UpdateMochigoma;
    end
    else
    begin
      // 移動できない場合は新しい選択
      if Koma.Kind <> kkNone then
      begin
        FSelectedX := X;
        FSelectedY := Y;
      end
      else
      begin
        FSelectedX := 0;
        FSelectedY := 0;
      end;
    end;
  end
  // 新しい駒を選択
  else if Koma.Kind <> kkNone then
  begin
    // 手番チェック（簡易版）
    if (FShogiGame.GetTurn = ttSente) and Koma.IsSente then
    begin
      FSelectedX := X;
      FSelectedY := Y;
    end
    else if (FShogiGame.GetTurn = ttGote) and not Koma.IsSente then
    begin
      FSelectedX := X;
      FSelectedY := Y;
    end;
  end;
  
  UpdateBoard;
  UpdateMochigoma;
end;

procedure TForm1.MochigomaClick(Sender: TObject);
var
  Kind: TKomaKind;
  IsSente: Boolean;
  Button: TPanel;
begin
  if not (Sender is TPanel) and not (Sender is TImage) then Exit;
  
  // クリックされたボタンを特定
  if Sender is TPanel then
    Button := TPanel(Sender)
  else if Sender is TImage then
    Button := TPanel(TImage(Sender).Parent)
  else
    Exit;
  
  // 先手か後手かを判定
  if Button.Parent = SenteMochigomaPanel then
  begin
    IsSente := True;
    // タグから駒の種類を取得（先手はそのまま）
    Kind := TKomaKind(Button.Tag);
  end
  else if Button.Parent = GoteMochigomaPanel then
  begin
    IsSente := False;
    // タグから駒の種類を取得（後手は100を引く）
    Kind := TKomaKind(Button.Tag - 100);
  end
  else
    Exit;
  
  // 手番チェック
  if (FShogiGame.GetTurn = ttSente) and not IsSente then Exit;
  if (FShogiGame.GetTurn = ttGote) and IsSente then Exit;
  
  // 持ち駒があるかチェック
  if not FShogiGame.HasMochigoma(Kind, IsSente) then Exit;
  
  // 既に選択されている場合は解除、そうでなければ選択
  if FSelectedMochigoma = Kind then
    FSelectedMochigoma := kkNone
  else
  begin
    FSelectedMochigoma := Kind;
    FSelectedX := 0;
    FSelectedY := 0;
  end;
  
  UpdateMochigoma;
  UpdateBoard;
end;

procedure TForm1.InitializeKomaImages;
begin
  // 駒の画像を初期化（画像ファイルがない場合はテキストで描画）
  // 実際の画像ファイルがある場合は、LoadKomaImageを使用して読み込む
  // 例: LoadKomaImage(kkOu, 'koma_ou.bmp');
  // 今回は画像がない場合でも動作するように、テキスト描画で対応
end;

procedure TForm1.LoadKomaImage(Kind: TKomaKind; const FileName: String);
var
  Bitmap: TBitmap;
begin
  if not FileExists(FileName) then Exit;
  
  Bitmap := TBitmap.Create;
  try
    Bitmap.LoadFromFile(FileName);
    if Assigned(FKomaBitmaps[Kind]) then
      FKomaBitmaps[Kind].Free;
    FKomaBitmaps[Kind] := Bitmap;
  except
    Bitmap.Free;
  end;
end;

procedure TForm1.DrawKomaOnImage(Image: TImage; Koma: TKoma);
var
  Bitmap: TBitmap;
  RotatedBitmap: TBitmap;
  KomaName: String;
  TextRect: TRect;
  OldFont: TFont;
  TextSize: TSize;
  TextX, TextY: Integer;
begin
  Image.Picture.Clear;
  
  if Koma.Kind = kkNone then
    Exit;
  
  // 画像がある場合は画像を使用
  if Assigned(FKomaBitmaps[Koma.Kind]) then
  begin
    Bitmap := FKomaBitmaps[Koma.Kind];
    
    // 先手の場合は180度回転
    if Koma.IsSente then
    begin
      RotatedBitmap := TBitmap.Create;
      try
        RotatedBitmap.Width := Bitmap.Width;
        RotatedBitmap.Height := Bitmap.Height;
        RotatedBitmap.Canvas.Brush.Color := clWhite;
        RotatedBitmap.Canvas.FillRect(0, 0, RotatedBitmap.Width, RotatedBitmap.Height);
        
        // 180度回転（StretchDrawで上下左右反転）
        RotatedBitmap.Canvas.StretchDraw(
          Rect(Bitmap.Width, Bitmap.Height, 0, 0),
          Bitmap
        );
        
        Image.Picture.Bitmap.Assign(RotatedBitmap);
      finally
        RotatedBitmap.Free;
      end;
    end
    else
    begin
      Image.Picture.Bitmap.Assign(Bitmap);
    end;
  end
  else
  begin
    // 画像がない場合はテキストで描画
    Bitmap := TBitmap.Create;
    try
      Bitmap.Width := Image.Width;
      Bitmap.Height := Image.Height;
      Bitmap.Canvas.Brush.Color := clWhite;
      Bitmap.Canvas.FillRect(0, 0, Bitmap.Width, Bitmap.Height);
      
      KomaName := FShogiGame.GetKomaName(Koma);
      
      // フォント設定
      OldFont := TFont.Create;
      try
        OldFont.Assign(Bitmap.Canvas.Font);
        Bitmap.Canvas.Font.Name := 'MS UI Gothic';
        Bitmap.Canvas.Font.Size := 14;
        Bitmap.Canvas.Font.Style := [fsBold];
        
        if Koma.IsSente then
          Bitmap.Canvas.Font.Color := clBlue
        else
          Bitmap.Canvas.Font.Color := clRed;
        
        // テキストを中央に配置
        TextRect := Rect(0, 0, Bitmap.Width, Bitmap.Height);
        // テキストサイズを取得して中央配置
        TextSize := Bitmap.Canvas.TextExtent(KomaName);
        TextX := (Bitmap.Width - TextSize.cx) div 2;
        TextY := (Bitmap.Height - TextSize.cy) div 2;
        Bitmap.Canvas.TextOut(TextX, TextY, KomaName);
        
        // 先手の場合は180度回転
        if Koma.IsSente then
        begin
          RotatedBitmap := TBitmap.Create;
          try
            RotatedBitmap.Width := Bitmap.Width;
            RotatedBitmap.Height := Bitmap.Height;
            RotatedBitmap.Canvas.Brush.Color := clWhite;
            RotatedBitmap.Canvas.FillRect(0, 0, RotatedBitmap.Width, RotatedBitmap.Height);
            
            // 180度回転（StretchDrawで上下左右反転）
            RotatedBitmap.Canvas.StretchDraw(
              Rect(Bitmap.Width, Bitmap.Height, 0, 0),
              Bitmap
            );
            
            Image.Picture.Bitmap.Assign(RotatedBitmap);
          finally
            RotatedBitmap.Free;
          end;
        end
        else
        begin
          Image.Picture.Bitmap.Assign(Bitmap);
        end;
        
        Bitmap.Canvas.Font.Assign(OldFont);
      finally
        OldFont.Free;
      end;
    finally
      Bitmap.Free;
    end;
  end;
end;

end.

