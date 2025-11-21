unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, DelphiZXIngQRCode, Math;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Image1: TImage;
    Label2: TLabel;
    Label3: TLabel;
    SpinEdit1: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FQRBitmap: TBitmap;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FQRBitmap := nil;
  Label2.Caption := '';
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  QRCode: TDelphiZXingQRCode;
  QRSize: Integer;
  MaxWidth, MaxHeight: Integer;
  QRWidth, QRHeight: Integer;
  ScaleX, ScaleY, Scale: Double;
  Row, Column: Integer;
begin
  if Edit1.Text = '' then
  begin
    ShowMessage('テキストを入力してください。');
    Exit;
  end;

  try
    QRSize := SpinEdit1.Value;
    
    // 既存のビットマップを解放
    if Assigned(FQRBitmap) then
    begin
      FQRBitmap.Free;
      FQRBitmap := nil;
    end;

    // DelphiZXingQRCodeを使用してQRコードを生成
    QRCode := TDelphiZXingQRCode.Create;
    try
      QRCode.Data := Edit1.Text;
      QRCode.Encoding := qrAuto;
      QRCode.QuietZone := 4;
      
      // ビットマップを作成
      FQRBitmap := TBitmap.Create;
      FQRBitmap.SetSize(QRCode.Rows * QRSize, QRCode.Columns * QRSize);
      FQRBitmap.Canvas.Brush.Color := clWhite;
      FQRBitmap.Canvas.FillRect(0, 0, FQRBitmap.Width, FQRBitmap.Height);
      
      // QRコードを描画
      for Row := 0 to QRCode.Rows - 1 do
      begin
        for Column := 0 to QRCode.Columns - 1 do
        begin
          if QRCode.IsBlack[Row, Column] then
            FQRBitmap.Canvas.Brush.Color := clBlack
          else
            FQRBitmap.Canvas.Brush.Color := clWhite;
          
          FQRBitmap.Canvas.FillRect(
            Column * QRSize,
            Row * QRSize,
            (Column + 1) * QRSize,
            (Row + 1) * QRSize
          );
        end;
      end;
    finally
      QRCode.Free;
    end;
    
    if Assigned(FQRBitmap) then
    begin
      Image1.Picture.Bitmap.Assign(FQRBitmap);
      
      // 利用可能な領域を計算
      MaxWidth := Self.ClientWidth - Image1.Left - 16;
      MaxHeight := Self.ClientHeight - Image1.Top - 20;
      
      // QRコードの実際のサイズ
      QRWidth := FQRBitmap.Width;
      QRHeight := FQRBitmap.Height;
      
      // フォーム内に収まるようにスケーリング
      if (QRWidth <= MaxWidth) and (QRHeight <= MaxHeight) then
      begin
        // QRコードがフォーム内に収まる場合は、そのまま表示
        Image1.Stretch := False;
        Image1.AutoSize := True;
        Image1.Proportional := False;
        Image1.Center := False;
      end
      else
      begin
        // QRコードが大きい場合は、フォーム内に収まるようにスケーリング
        ScaleX := MaxWidth / QRWidth;
        ScaleY := MaxHeight / QRHeight;
        Scale := Min(ScaleX, ScaleY);
        
        Image1.Width := Trunc(QRWidth * Scale);
        Image1.Height := Trunc(QRHeight * Scale);
        Image1.Stretch := True;
        Image1.AutoSize := False;
        Image1.Proportional := False;
        Image1.Center := False;
      end;
      
      Label2.Caption := 'QRコードを生成しました。';
    end
    else
    begin
      Label2.Caption := 'QRコードの生成に失敗しました。';
    end;
  except
    on E: Exception do
    begin
      ShowMessage('エラー: ' + E.Message);
      Label2.Caption := 'エラーが発生しました。';
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  SaveDialog: TSaveDialog;
begin
  if not Assigned(FQRBitmap) then
  begin
    ShowMessage('まずQRコードを生成してください。');
    Exit;
  end;

  SaveDialog := TSaveDialog.Create(Self);
  try
    SaveDialog.Filter := 'PNG画像|*.png|BMP画像|*.bmp|JPEG画像|*.jpg';
    SaveDialog.DefaultExt := 'png';
    SaveDialog.FileName := 'qrcode';
    
    if SaveDialog.Execute then
    begin
      FQRBitmap.SaveToFile(SaveDialog.FileName);
      Label2.Caption := '保存しました: ' + SaveDialog.FileName;
    end;
  finally
    SaveDialog.Free;
  end;
end;

end.
