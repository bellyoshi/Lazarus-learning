unit PdfRenderer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PdfPage, PdfBitmap, Graphics, GraphType, PdfiumLib;

procedure DrawToBitmap(Page: TPdfPage; Bitmap: TBitmap; Width, Height: Integer);
procedure DrawToPdfBitmap(Page: TPdfPage; APdfBitmap: TPdfBitmap; X, Y, Width, Height: Integer);

implementation

procedure DrawToBitmap(Page: TPdfPage; Bitmap: TBitmap; Width, Height: Integer);
var
  SizeInt: Integer;
  PdfBitmap: TPdfBitmap;
  ImageData: TBytes;
  buffer: Pointer;
  RawImage: TRawImage;
begin
  PdfBitmap := TPdfBitmap.Create(Width, Height, BitmapFormat_bfBGRA);
  try
    PdfBitmap.FillRect(0, 0, Width, Height, $FFFFFFFF);
    DrawToPdfBitmap(Page, PdfBitmap, 0, 0, Width, Height);

    SizeInt := Width * Height * 4;

    ImageData := nil;
    SetLength(ImageData, SizeInt);

    buffer := PdfBitmap.GetBuffer;
    Move(buffer^, ImageData[0], SizeInt);

    RawImage.Init;
    RawImage.Description.Init_BPP32_B8G8R8A8_M1_BIO_TTB(Width, Height);
    RawImage.CreateData(true);
    RawImage.Data:=@ImageData[0];

    Bitmap.LoadFromRawImage(RawImage, false);
  finally
    PdfBitmap.Free;
  end;
end;

procedure DrawToPdfBitmap(Page: TPdfPage; APdfBitmap: TPdfBitmap; X, Y, Width, Height: Integer);
begin
  FPDF_RenderPageBitmap(APdfBitmap.Bitmap, Page.Page, X, Y, Width, Height, 0, 1);
end;

end.
