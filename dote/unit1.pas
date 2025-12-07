unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, ExtDlgs, Menus;

type
  TGridSize = (gs16x16, gs32x32, gs48x48, gs64x64);
  
  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    btnColor: TButton;
    ColorDialog1: TColorDialog;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    Label1: TLabel;
    PaintBoxPreview: TPaintBox;
    Label2: TLabel;
    RadioGroup1: TRadioGroup;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure btnColorClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure PaintBoxPreviewPaint(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
  private
    FGrid: array of array of TColor;
    FCurrentColor: TColor;
    FCellSize: Integer;
    FIsDrawing: Boolean;
    FGridSize: TGridSize;
    FGridWidth: Integer;
    FGridHeight: Integer;
    procedure DrawGrid;
    procedure DrawPreview;
    procedure SetPixel(GridX, GridY: Integer);
    function GetGridPos(X, Y: Integer; out GridX, GridY: Integer): Boolean;
    procedure InitializeGrid;
    procedure SetGridSize(Size: TGridSize);
    function GetGridSizeValue: Integer;
    function HasUnsavedData: Boolean;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCurrentColor := clBlack;
  FIsDrawing := False;
  FCellSize := 10; // 各セルのサイズ（ピクセル）
  FGridSize := gs64x64;
  
  RadioGroup1.Items.Add('16x16');
  RadioGroup1.Items.Add('32x32');
  RadioGroup1.Items.Add('48x48');
  RadioGroup1.Items.Add('64x64');
  RadioGroup1.ItemIndex := 3;  // 64x64を初期選択
  
  SetGridSize(gs64x64);
  Caption := 'ドット絵エディタ';
end;

function TForm1.GetGridSizeValue: Integer;
begin
  case FGridSize of
    gs16x16: Result := 16;
    gs32x32: Result := 32;
    gs48x48: Result := 48;
    gs64x64: Result := 64;
  else
    Result := 16;
  end;
end;

procedure TForm1.SetGridSize(Size: TGridSize);
begin
  FGridSize := Size;
  FGridWidth := GetGridSizeValue;
  FGridHeight := GetGridSizeValue;
  
  InitializeGrid;
  
  // フォームサイズを調整
  PaintBox1.Width := FGridWidth * FCellSize + 1;
  PaintBox1.Height := FGridHeight * FCellSize + 1;
  
  // プレビュー用PaintBoxのサイズ設定
  PaintBoxPreview.Width := 160;
  PaintBoxPreview.Height := 160;
  
  // プレビューとRadioGroupを編集エリアの右側に配置（余白20ピクセル）
  PaintBoxPreview.Left := PaintBox1.Left + PaintBox1.Width + 20;
  Label2.Left := PaintBoxPreview.Left;
  
  // RadioGroupの位置をプレビューの下に配置（余白を確保）
  RadioGroup1.Left := PaintBoxPreview.Left;
  RadioGroup1.Top := PaintBoxPreview.Top + PaintBoxPreview.Height + 16;
  
  // Panel1の位置を調整（PaintBox1と右側のコントロールの下に配置）
  Panel1.Top := PaintBox1.Top + PaintBox1.Height + 8;
  if (PaintBoxPreview.Top + PaintBoxPreview.Height + RadioGroup1.Height + 16) > Panel1.Top then
    Panel1.Top := PaintBoxPreview.Top + PaintBoxPreview.Height + RadioGroup1.Height + 16;
  
  // フォームサイズを調整（プレビューとRadioGroupの高さを考慮）
  Width := PaintBoxPreview.Left + PaintBoxPreview.Width + 20;
  // フォームの高さは、PaintBox1の高さ、Panel1の高さ、余白を考慮
  // 右側のプレビューとRadioGroupの高さも考慮する
  Height := Panel1.Top + Panel1.Height + 40;
  
  PaintBox1.Invalidate;
  PaintBoxPreview.Invalidate;
end;

procedure TForm1.InitializeGrid;
var
  i, j: Integer;
begin
  // グリッドサイズを変更
  SetLength(FGrid, FGridWidth, FGridHeight);
  
  // グリッドを白で初期化
  for i := 0 to FGridWidth - 1 do
    for j := 0 to FGridHeight - 1 do
      FGrid[i, j] := clWhite;
end;

procedure TForm1.DrawGrid;
var
  i, j: Integer;
  x, y: Integer;
begin
  with PaintBox1.Canvas do
  begin
    // 背景を白で塗りつぶし
    Brush.Color := clWhite;
    FillRect(0, 0, PaintBox1.Width, PaintBox1.Height);
    
    // グリッドを描画
    Pen.Color := clGray;
    Pen.Width := 1;
    
    // 縦線
    for i := 0 to FGridWidth do
    begin
      x := i * FCellSize;
      MoveTo(x, 0);
      LineTo(x, PaintBox1.Height);
    end;
    
    // 横線
    for i := 0 to FGridHeight do
    begin
      y := i * FCellSize;
      MoveTo(0, y);
      LineTo(PaintBox1.Width, y);
    end;
    
    // セルを描画
    for i := 0 to FGridWidth - 1 do
      for j := 0 to FGridHeight - 1 do
      begin
        if FGrid[i, j] <> clWhite then
        begin
          Brush.Color := FGrid[i, j];
          FillRect(i * FCellSize + 1, j * FCellSize + 1,
                   (i + 1) * FCellSize, (j + 1) * FCellSize);
        end;
      end;
  end;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  DrawGrid;
end;

function TForm1.GetGridPos(X, Y: Integer; out GridX, GridY: Integer): Boolean;
begin
  GridX := X div FCellSize;
  GridY := Y div FCellSize;
  Result := (GridX >= 0) and (GridX < FGridWidth) and (GridY >= 0) and (GridY < FGridHeight);
end;

procedure TForm1.SetPixel(GridX, GridY: Integer);
begin
  if (GridX >= 0) and (GridX < FGridWidth) and (GridY >= 0) and (GridY < FGridHeight) then
  begin
    FGrid[GridX, GridY] := FCurrentColor;
    PaintBox1.Invalidate;
    PaintBoxPreview.Invalidate;  // プレビューも更新
  end;
end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  GridX, GridY: Integer;
begin
  if Button = mbLeft then
  begin
    if GetGridPos(X, Y, GridX, GridY) then
    begin
      FIsDrawing := True;
      SetPixel(GridX, GridY);
    end;
  end;
end;

procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  GridX, GridY: Integer;
begin
  if FIsDrawing and (ssLeft in Shift) then
  begin
    if GetGridPos(X, Y, GridX, GridY) then
      SetPixel(GridX, GridY);
  end;
end;

procedure TForm1.btnColorClick(Sender: TObject);
begin
  ColorDialog1.Color := FCurrentColor;
  if ColorDialog1.Execute then
  begin
    FCurrentColor := ColorDialog1.Color;
    btnColor.Color := FCurrentColor;
  end;
end;

procedure TForm1.btnSaveClick(Sender: TObject);
var
  Bitmap: TBitmap;
  IconFile: TIcon;
  i, j: Integer;
  Ext: String;
begin
  SaveDialog1.Filter := 'PNG画像|*.png|BMP画像|*.bmp|ICOファイル|*.ico';
  SaveDialog1.DefaultExt := 'png';
  
  if SaveDialog1.Execute then
  begin
    Ext := LowerCase(ExtractFileExt(SaveDialog1.FileName));
    
    if Ext = '.ico' then
    begin
      // ICOファイルとして保存
      IconFile := TIcon.Create;
      Bitmap := TBitmap.Create;
      try
        // 現在のグリッドサイズのビットマップを作成
        Bitmap.Width := FGridWidth;
        Bitmap.Height := FGridHeight;
        Bitmap.PixelFormat := pf24bit;
        
        // グリッドデータをビットマップにコピー
        for i := 0 to FGridWidth - 1 do
          for j := 0 to FGridHeight - 1 do
          begin
            Bitmap.Canvas.Pixels[i, j] := FGrid[i, j];
          end;
        
        // ビットマップからアイコンを作成
        IconFile.Assign(Bitmap);
        
        // ICOファイルとして保存
        IconFile.SaveToFile(SaveDialog1.FileName);
        ShowMessage('ICOファイルを保存しました: ' + SaveDialog1.FileName);
      finally
        Bitmap.Free;
        IconFile.Free;
      end;
    end
    else
    begin
      // PNGまたはBMPとして保存
      Bitmap := TBitmap.Create;
      try
        // 現在のグリッドサイズのビットマップを作成
        Bitmap.Width := FGridWidth;
        Bitmap.Height := FGridHeight;
        Bitmap.PixelFormat := pf24bit;
        
        // グリッドデータをビットマップにコピー
        for i := 0 to FGridWidth - 1 do
          for j := 0 to FGridHeight - 1 do
          begin
            Bitmap.Canvas.Pixels[i, j] := FGrid[i, j];
          end;
        
        // ファイルに保存
        Bitmap.SaveToFile(SaveDialog1.FileName);
        ShowMessage('画像を保存しました: ' + SaveDialog1.FileName);
      finally
        Bitmap.Free;
      end;
    end;
  end;
end;

function TForm1.HasUnsavedData: Boolean;
var
  i, j: Integer;
begin
  Result := False;
  for i := 0 to FGridWidth - 1 do
    for j := 0 to FGridHeight - 1 do
    begin
      if FGrid[i, j] <> clWhite then
      begin
        Result := True;
        Exit;
      end;
    end;
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
var
  i, j: Integer;
  SaveResult: Integer;
begin
  // 編集中のデータがあるかチェック
  if HasUnsavedData then
  begin
    SaveResult := MessageDlg(
      '編集中のデータがあります。'#13#10 +
      '保存しますか？',
      mtConfirmation,
      [mbYes, mbNo, mbCancel],
      0);
    
    case SaveResult of
      mrYes:
        begin
          // 保存を実行
          SaveDialog1.Filter := 'PNG画像|*.png|BMP画像|*.bmp|ICOファイル|*.ico';
          SaveDialog1.DefaultExt := 'png';
          if SaveDialog1.Execute then
          begin
            // 実際の保存処理を実行
            btnSaveClick(Sender);
          end
          else
          begin
            // 保存ダイアログがキャンセルされた場合は新規作成もキャンセル
            Exit;
          end;
        end;
      mrNo:
        begin
          // 保存せずに新規作成を続行
        end;
      mrCancel:
        begin
          // 新規作成をキャンセル
          Exit;
        end;
    end;
  end;
  
  // グリッドを白でクリア
  for i := 0 to FGridWidth - 1 do
    for j := 0 to FGridHeight - 1 do
      FGrid[i, j] := clWhite;
  PaintBox1.Invalidate;
  PaintBoxPreview.Invalidate;  // プレビューも更新
end;

procedure TForm1.DrawPreview;
var
  i, j: Integer;
  PixelSizeX, PixelSizeY: Integer;
  ScaleX, ScaleY: Double;
  PreviewWidth, PreviewHeight: Integer;
begin
  // プレビューサイズを計算（アスペクト比を維持）
  if FGridWidth >= FGridHeight then
  begin
    PreviewWidth := PaintBoxPreview.Width;
    PreviewHeight := Round(PaintBoxPreview.Width * FGridHeight / FGridWidth);
  end
  else
  begin
    PreviewHeight := PaintBoxPreview.Height;
    PreviewWidth := Round(PaintBoxPreview.Height * FGridWidth / FGridHeight);
  end;
  
  PixelSizeX := PreviewWidth div FGridWidth;
  PixelSizeY := PreviewHeight div FGridHeight;
  
  with PaintBoxPreview.Canvas do
  begin
    // 背景を白で塗りつぶし
    Brush.Color := clWhite;
    FillRect(0, 0, PaintBoxPreview.Width, PaintBoxPreview.Height);
    
    // グリッドの内容を拡大表示（グリッド線なし）
    for i := 0 to FGridWidth - 1 do
      for j := 0 to FGridHeight - 1 do
      begin
        Brush.Color := FGrid[i, j];
        FillRect(i * PixelSizeX, j * PixelSizeY,
                 (i + 1) * PixelSizeX, (j + 1) * PixelSizeY);
      end;
    
    // プレビュー枠を描画
    Pen.Color := clGray;
    Pen.Width := 2;
    Brush.Style := bsClear;
    Rectangle(0, 0, PreviewWidth, PreviewHeight);
  end;
end;

procedure TForm1.PaintBoxPreviewPaint(Sender: TObject);
begin
  DrawPreview;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
    0: SetGridSize(gs16x16);
    1: SetGridSize(gs32x32);
    2: SetGridSize(gs48x48);
    3: SetGridSize(gs64x64);
  end;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
var
  Bitmap, ResizedBitmap: TBitmap;
  i, j: Integer;
  LoadedWidth, LoadedHeight: Integer;
  ResizeResult: Integer;
begin
  OpenDialog1.Filter := '画像ファイル|*.png;*.bmp;*.jpg;*.jpeg|PNG画像|*.png|BMP画像|*.bmp|JPEG画像|*.jpg;*.jpeg';
  OpenDialog1.DefaultExt := 'png';
  
  if OpenDialog1.Execute then
  begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.LoadFromFile(OpenDialog1.FileName);
      LoadedWidth := Bitmap.Width;
      LoadedHeight := Bitmap.Height;
      
      // 64x64より大きい画像の場合、縮小するか確認
      if (LoadedWidth > 64) or (LoadedHeight > 64) then
      begin
        ResizeResult := MessageDlg(
          Format('大きな画像です。'#13#10 +
                 '読み込んだ画像: %dx%d'#13#10 +
                 '縮小しますか？', [LoadedWidth, LoadedHeight]),
          mtConfirmation,
          [mbYes, mbNo],
          0);
        
        if ResizeResult = mrYes then
        begin
          // 64x64にリサイズ
          ResizedBitmap := TBitmap.Create;
          try
            ResizedBitmap.Width := 64;
            ResizedBitmap.Height := 64;
            ResizedBitmap.PixelFormat := pf24bit;
            ResizedBitmap.Canvas.StretchDraw(Rect(0, 0, 64, 64), Bitmap);
            
            // リサイズした画像を使用
            Bitmap.Assign(ResizedBitmap);
            LoadedWidth := 64;
            LoadedHeight := 64;
          finally
            ResizedBitmap.Free;
          end;
        end
        else
        begin
          // 縮小しない場合は終了
          Exit;
        end;
      end;
      
      // サイズチェック（16x16, 32x32, 48x48, 64x64のみ許可）
      if not ((LoadedWidth = 16) and (LoadedHeight = 16) or
              (LoadedWidth = 32) and (LoadedHeight = 32) or
              (LoadedWidth = 48) and (LoadedHeight = 48) or
              (LoadedWidth = 64) and (LoadedHeight = 64)) then
      begin
        ShowMessage(Format('エラー: 画像サイズが対応していません。'#13#10 +
                          '読み込んだ画像: %dx%d'#13#10 +
                          '対応サイズ: 16x16, 32x32, 48x48, 64x64',
                          [LoadedWidth, LoadedHeight]));
        Exit;
      end;
      
      // サイズに応じてグリッドサイズを変更
      if (LoadedWidth = 16) and (LoadedHeight = 16) then
      begin
        SetGridSize(gs16x16);
        RadioGroup1.ItemIndex := 0;
      end
      else if (LoadedWidth = 32) and (LoadedHeight = 32) then
      begin
        SetGridSize(gs32x32);
        RadioGroup1.ItemIndex := 1;
      end
      else if (LoadedWidth = 48) and (LoadedHeight = 48) then
      begin
        SetGridSize(gs48x48);
        RadioGroup1.ItemIndex := 2;
      end
      else if (LoadedWidth = 64) and (LoadedHeight = 64) then
      begin
        SetGridSize(gs64x64);
        RadioGroup1.ItemIndex := 3;
      end;
      
      // 画像データをグリッドに読み込む
      for i := 0 to FGridWidth - 1 do
        for j := 0 to FGridHeight - 1 do
        begin
          if (i < LoadedWidth) and (j < LoadedHeight) then
            FGrid[i, j] := Bitmap.Canvas.Pixels[i, j]
          else
            FGrid[i, j] := clWhite;
        end;
      
      PaintBox1.Invalidate;
      PaintBoxPreview.Invalidate;
    except
      on E: Exception do
        ShowMessage('エラー: 画像の読み込みに失敗しました。'#13#10 + E.Message);
    end;
    Bitmap.Free;
  end;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  btnSaveClick(Sender);
end;

end.

