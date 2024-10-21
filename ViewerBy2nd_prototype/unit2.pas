unit Unit2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  PdfiumCore;

type

  { TForm2 }

  TForm2 = class(TForm)
    Image1: TImage;
    MenuItemFullScreen: TMenuItem;
    MenuItemWindowMode: TMenuItem;
    MenuItemClose: TMenuItem;
    PopupMenu1: TPopupMenu;



    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);

    procedure FormContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);

    procedure MenuItemCloseClick(Sender: TObject);
    procedure MenuItemFullScreenClick(Sender: TObject);
    procedure MenuItemWindowModeClick(Sender: TObject);
  private
    HasBitmap : Boolean;
    FIsFullScreen: Boolean;
    Ratio: Double;
    Page: TPdfPage;
    procedure SetIsFullScreen(Value: Boolean);
    procedure StretchImage();
    procedure SetBitmap(Bitmap: TBitmap);
  public
    procedure SetPage(aPage : TPdfPage);
    property IsFullScreen: Boolean read FIsFullScreen write SetIsFullScreen;

  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}
procedure LoadBitmapFromRawImage(Bitmap: TBitmap; const AIMarge: TBytes; Width, Height: Integer);
var
  Y, RowSize: Integer;
  SrcPtr: PByte;
  DestPtr: Pointer;
begin
  // ビットマップのサイズとピクセルフォーマットを設定
  Bitmap.Width := Width;
  Bitmap.Height := Height;
  Bitmap.PixelFormat := pf32bit; // 32bit (RGBA) の画像データ

  // 1行あたりのデータサイズ (4バイト/ピクセル * 幅)
  RowSize := Width * 4;

  // 生データの先頭ポインタ
  SrcPtr := @AIMarge[0];

  // 行ごとにデータをコピー
  for Y := 0 to Height - 1 do
  begin
    // 現在の行の先頭のメモリポインタを取得
    DestPtr := Bitmap.ScanLine[Y];
    // 1行分のデータをコピー
    Move(SrcPtr^, DestPtr^, RowSize);
    // 次の行に進む
    Inc(SrcPtr, RowSize);
  end;
end;
procedure DrawToBitmap(Page: TPdfPage; Bitmap: TBitmap; w,h : Integer);
var
  SizeInt: Integer;
  PdfBitmap: TPdfBitmap;
  AIMarge: TBytes;
  buffer: Pointer;
begin


  // PDFium ビットマップを作成
  PdfBitmap := TPdfBitmap.Create(w, h, bfBGRA);
  try
    PdfBitmap.FillRect(0, 0, w, h, $FFFFFFFF); // 背景を白色で塗りつぶし
    Page.DrawToPdfBitmap(PdfBitmap, 0, 0, w, h,prNormal, [proLCDOptimized]);

    // PDFium ビットマップのバッファサイズを計算
    SizeInt := w * h * 4; // 4バイト/ピクセル (RGBA)

    // バイト配列を初期化
    SetLength(AIMarge, SizeInt);

    // PDFium のバッファを取得して、バイト配列にコピー
    buffer := PdfBitmap.GetBuffer;
    Move(buffer^, AIMarge[0], SizeInt);

    // バイト配列から Delphi のビットマップにデータをコピー
    LoadBitmapFromRawImage(Bitmap, AIMarge, w, h);
  finally
    PdfBitmap.Free;
  end;
end;


function ConvertBitmap32To24Bit(SourceBitmap: TBitmap): TBitmap;
var
  X, Y: Integer;
  SourcePtr, DestPtr: PByte;
  SourceLine, DestLine: PByteArray;
  TempBitmap: TBitmap;
begin
  if SourceBitmap.PixelFormat <> pf32bit then
    raise Exception.Create('Source bitmap is not 32-bit.');

  // 新しい24ビットのTBitmapを作成
  TempBitmap := TBitmap.Create;
  TempBitmap.PixelFormat := pf24bit;  // 24ビットに設定
  TempBitmap.SetSize(SourceBitmap.Width, SourceBitmap.Height);  // 元の画像サイズを保持

  // 各ラインのピクセルを直接コピーする
  for Y := 0 to SourceBitmap.Height - 1 do
  begin
    SourceLine := SourceBitmap.ScanLine[Y];  // 32ビットの行データ
    DestLine := TempBitmap.ScanLine[Y];      // 24ビットの行データ

    SourcePtr := @SourceLine[0];
    DestPtr := @DestLine[0];

    // 各ピクセルをコピー（RGBデータのみコピーしてアルファを無視）
    for X := 0 to SourceBitmap.Width - 1 do
    begin
      DestPtr^ := SourcePtr^;      // B
      Inc(DestPtr);
      Inc(SourcePtr);

      DestPtr^ := SourcePtr^;      // G
      Inc(DestPtr);
      Inc(SourcePtr);

      DestPtr^ := SourcePtr^;      // R
      Inc(DestPtr);

      Inc(SourcePtr);  // アルファチャンネル（32ビット目）を無視
      Inc(SourcePtr);
    end;
  end;

  // 新しい24ビットビットマップを返す
  Result := TempBitmap;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  HasBitmap := False;

end;

procedure TForm2.StretchImage();
var
  formRatio : Double;
  NewWidth, NewHeight: Integer;
  Bitmap : TBitmap;
begin
  if not HasBitmap then Exit;
  formRatio := ClientWidth / ClientHeight;
  if formRatio > Ratio then
  begin
    // 縦が基準
    NewHeight := ClientHeight;
    NewWidth := Round(NewHeight * Ratio);
  end
  else
  begin
    // 横が基準
    NewWidth := ClientWidth;
    NewHeight := Round(NewWidth / Ratio);
  end;

  // Stretchしておく
  //Image1.Stretch := False;
  // フォームのクライアント領域にImage1をフィットさせる
  Image1.Width := NewWidth;
  Image1.Height := NewHeight;

  // 中央に配置
  Image1.Left := (ClientWidth - NewWidth) div 2;
  Image1.Top := (ClientHeight - NewHeight) div 2;

  try
      // PDFium ページを Delphi ビットマップに描画
    Bitmap := TBitmap.Create;
    DrawToBitmap(Self.Page, Bitmap, NewWidth,NewHeight);

  SetBitmap(Bitmap);

    finally
      Bitmap.Free;
    end;


end;


procedure TForm2.FormResize(Sender: TObject);
begin
  StretchImage;
end;

procedure TForm2.SetIsFullScreen(Value: Boolean);
begin
  if FIsFullScreen = Value then Exit; // 既にフルスクリーン状態なら何もしない

  FIsFullScreen := Value;

  if FIsFullScreen then
  begin
    // フルスクリーンモードに切り替える
    BorderStyle := bsNone;
    WindowState := wsMaximized;
  end
  else
  begin
    // ウィンドウモードに戻す
    BorderStyle := bsSizeable;
    WindowState := wsNormal;
  end;
end;

procedure TForm2.FormContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  PopupMenu1.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  Handled := True;  // イベントを処理済みにする
end;




procedure TForm2.MenuItemFullScreenClick(Sender: TObject);
begin
  IsFullScreen := True;  // フルスクリーンに切り替える
end;

procedure TForm2.MenuItemWindowModeClick(Sender: TObject);
begin
  IsFullScreen := False;  // ウインドウモードに切り替える
end;

procedure TForm2.MenuItemCloseClick(Sender: TObject);
begin
  Close;  // フォームを閉じる
end;

procedure TForm2.SetPage(aPage : TPdfPage);
begin
      HasBitmap := True;
     Self.Page := aPage;
     Ratio := Page.Width / Page.Height;     // 画像のアスペクト比を計算
         StretchImage;
end;

procedure TForm2.SetBitmap(Bitmap : TBitmap);
var
  TempBitmap: TBitmap;
begin
  // 新しい24ビットのTBitmapを作成
  TempBitmap := ConvertBitmap32To24Bit(Bitmap);

  Image1.Picture.Bitmap.Assign(Bitmap);
  Image1.Canvas.Draw(0,0, TempBitmap);

  TempBitmap.Free;

end;

end.

