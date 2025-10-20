unit PdfRenderer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PdfPage, PdfBitmap, Graphics, GraphType, PdfiumLib;

procedure DrawToBitmap(Page: TPdfPage; Bitmap: TBitmap);

function GetBitmap(Page: TPdfPage; Width, Height: Integer): TBitmap;

implementation

// ヘルパー関数の前方宣言
procedure DrawToPdfBitmap(
  {from}Page: TPdfPage;
  {to}APdfBitmap: TPdfBitmap);forward;
function CreateEmptyPdfBitmap(Width, Height: Integer): TPdfBitmap; forward;
function ExtractImageDataFromPdfBitmap(PdfBitmap: TPdfBitmap): TBytes; forward;
function CreateRawImageFromData(const ImageData: TBytes; Width, Height: Integer): TRawImage; forward;

function GetPdfBitmap(Page: TPdfPage; Width, Height: Integer) : TPdfBitmap;
begin
  Result:= CreateEmptyPdfBitmap(Width , Height);
  DrawToPdfBitmap(Page, Result);
end;


procedure DrawToBitmap(Page: TPdfPage; Bitmap: TBitmap);
var
  PdfBitmap: TPdfBitmap;
  ImageData: TBytes;
  RawImage: TRawImage;
begin
  try
    PdfBitmap := GetPdfBitmap(Page, Bitmap.Width , Bitmap.Height);
    ImageData := ExtractImageDataFromPdfBitmap(PdfBitmap);
    RawImage := CreateRawImageFromData(ImageData, PdfBitmap.Width, PdfBitmap.Height);
    Bitmap.LoadFromRawImage(RawImage, false);
  finally
    PdfBitmap.Free;
  end;
end;

function GetBitmap(Page: TPdfPage; Width, Height: Integer): TBitmap;
begin
  Result := TBitmap.Create;
  Result.Width := Width;
  Result.Height := Height;
  DrawToBitmap(Page, Result);
end;

function CreateEmptyPdfBitmap(Width, Height: Integer): TPdfBitmap;
begin
  Result := TPdfBitmap.Create(Width, Height, BitmapFormat_bfBGRA);
  Result.FillRect(0, 0, Width, Height, $FFFFFFFF);
end;

function ExtractImageDataFromPdfBitmap(PdfBitmap: TPdfBitmap): TBytes;
var
  SizeInt: Integer;
  buffer: Pointer;
begin
  Result := nil; // 明示的に初期化
  SizeInt := pdfBitmap.Width * pdfBitmap.Height * 4;
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

procedure DrawToPdfBitmap(Page: TPdfPage; APdfBitmap: TPdfBitmap);
const
  ROTATE_NONE = 0;        // 回転なし
  RENDER_FLAG_ANNOT = 1;  // アノテーションを含むレンダリング
  locate_X = 0;
  locate_Y = 0;
begin
  // PDFページをビットマップにレンダリング（アノテーション含む）
  FPDF_RenderPageBitmap(APdfBitmap.Bitmap, Page.Page, locate_X, locate_Y, APdfBitmap.Width, APdfBitmap.Height, ROTATE_NONE, RENDER_FLAG_ANNOT);
end;

end.
