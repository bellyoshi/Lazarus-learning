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
begin
  FPDF_RenderPageBitmap(APdfBitmap.Bitmap, Page.Page, X, Y, Width, Height, 0, 1);
end;

end.
