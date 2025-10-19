unit PdfRenderer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PdfPage, PdfBitmap, Graphics, GraphType, PdfiumLib;

procedure DrawToBitmap(Page: TPdfPage; Bitmap: TBitmap);


implementation

// ヘルパー関数の前方宣言
procedure DrawToPdfBitmap(Page: TPdfPage; APdfBitmap: TPdfBitmap; X, Y, Width, Height: Integer);forward;
function CreateEmptyPdfBitmap(Width, Height: Integer): TPdfBitmap; forward;
function ExtractImageDataFromPdfBitmap(PdfBitmap: TPdfBitmap; Width, Height: Integer): TBytes; forward;
function CreateRawImageFromData(const ImageData: TBytes; Width, Height: Integer): TRawImage; forward;

procedure DrawToBitmap(Page: TPdfPage; Bitmap: TBitmap);
var
  width : Integer;
  height : Integer;
  PdfBitmap: TPdfBitmap;
  ImageData: TBytes;
  RawImage: TRawImage;
begin
  width := Bitmap.Width;
  height := Bitmap.Height;
  PdfBitmap := CreateEmptyPdfBitmap(Width, Height);
  try
    DrawToPdfBitmap(Page, PdfBitmap, 0, 0, Width, Height);
    ImageData := ExtractImageDataFromPdfBitmap(PdfBitmap, Width, Height);
    RawImage := CreateRawImageFromData(ImageData, Width, Height);
    Bitmap.LoadFromRawImage(RawImage, false);
  finally
    PdfBitmap.Free;
  end;
end;

function CreateEmptyPdfBitmap(Width, Height: Integer): TPdfBitmap;
begin
  Result := TPdfBitmap.Create(Width, Height, BitmapFormat_bfBGRA);
  Result.FillRect(0, 0, Width, Height, $FFFFFFFF);
end;

function ExtractImageDataFromPdfBitmap(PdfBitmap: TPdfBitmap; Width, Height: Integer): TBytes;
var
  SizeInt: Integer;
  buffer: Pointer;
begin
  Result := nil; // 明示的に初期化
  SizeInt := Width * Height * 4;
  SetLength(Result, SizeInt);
  buffer := PdfBitmap.GetBuffer;
  Move(buffer^, Result[0], SizeInt);
end;

function CreateRawImageFromData(const ImageData: TBytes; Width, Height: Integer): TRawImage;
begin
  Result.Init;
  Result.Description.Init_BPP32_B8G8R8A8_M1_BIO_TTB(Width, Height);
  Result.CreateData(true);
  Result.Data := @ImageData[0];
end;

procedure DrawToPdfBitmap(Page: TPdfPage; APdfBitmap: TPdfBitmap; X, Y, Width, Height: Integer);
const
  // FPDF_RenderPageBitmap の回転角度定数
  ROTATE_NONE = 0;        // 回転なし
  // ROTATE_90 = 1;          // 90度回転
  // ROTATE_180 = 2;         // 180度回転
  // ROTATE_270 = 3;         // 270度回転
  
  // FPDF_RenderPageBitmap のフラグ定数
  // RENDER_FLAG_NONE = 0;   // 通常のレンダリング（アノテーション無視）
  RENDER_FLAG_ANNOT = 1;  // アノテーションを含むレンダリング
begin
  // PDFページをビットマップにレンダリング
  // 
  // パラメータ説明:
  // - rotate: ROTATE_NONE (0) = 回転なし（ページをそのまま表示）
  // - flags: RENDER_FLAG_ANNOT (1) = アノテーションを含むレンダリング
  // 
  // アノテーションとは:
  // - PDF文書に追加される注釈やマークアップ（ハイライト、コメント、図形など）
  // - ユーザーが文書に追加した付箋、下線、矢印、スタンプなど
  // - RENDER_FLAG_ANNOT = 1 により、これらの注釈も一緒に表示される
  // - RENDER_FLAG_NONE = 0 にすると、元の文書内容のみ表示（注釈は無視）
  FPDF_RenderPageBitmap(APdfBitmap.Bitmap, Page.Page, X, Y, Width, Height, ROTATE_NONE, RENDER_FLAG_ANNOT);
end;

end.
