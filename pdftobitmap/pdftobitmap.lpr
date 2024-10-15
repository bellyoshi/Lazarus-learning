program pdftobitmap;
{$MODE DelphiUnicode}
//{$mode objfpc}{$H+}


uses

  SysUtils, Windows, Graphics;

const
  External_library = 'DLL1';

  {$ifdef windows}
    libext = '.dll';
  {$endif}
  {$ifdef unix}
    libext = '.so';
  {$endif}
  {$ifdef macos}
    libext = '.dylib';
  {$endif}

type
    FPDF_STRING = PAnsiChar;
       FPDF_BYTESTRING = PAnsiChar;
    // Helper data type for type safety.
  __FPDF_PTRREC = record end;
  __PFPDF_PTRREC = ^__FPDF_PTRREC;
    FPDF_BITMAP             = type __PFPDF_PTRREC;
  FPDF_LINK_ARRAY = ^FPDF_LINK; // array
  FPDF_PAGE_ARRAY = ^FPDF_PAGE; // array

  FPDF_PAGE               = type __PFPDF_PTRREC;
    FPDF_DOCUMENT           = type __PFPDF_PTRREC;
      FPDF_LINK               = type __PFPDF_PTRREC;




procedure FPDF_InitLibrary; cdecl; external External_library + libext;
procedure FPDF_DestroyLibrary; cdecl; external External_library + libext;
function FPDF_LoadDocument(file_path: FPDF_STRING; password: FPDF_BYTESTRING): FPDF_DOCUMENT; cdecl; external External_library + libext;
function FPDF_GetPageCount(Document: FPDF_DOCUMENT): Integer; cdecl; external External_library + libext;
function FPDF_LoadPage(document: FPDF_DOCUMENT; page_index: Integer): FPDF_PAGE; cdecl; external External_library + libext;
procedure FPDF_ClosePage(Page: FPDF_PAGE); cdecl; external External_library + libext;
procedure FPDF_CloseDocument(Document: FPDF_DOCUMENT); cdecl; external External_library + libext;
function FPDFBitmap_Create(Width, Height, Alpha: Integer): Pointer; cdecl; external External_library + libext;
procedure FPDF_RenderPageBitmap(bitmap: FPDF_BITMAP; page: FPDF_PAGE; start_x, start_y, size_x, size_y: Integer; rotate: Integer; flags: Integer); cdecl; external External_library + libext;
procedure FPDFBitmap_FillRect(Bitmap: FPDF_BITMAP; Left, Top, Width, Height: Integer; Color: Cardinal); cdecl; external External_library + libext;
procedure FPDFBitmap_Destroy(Bitmap: FPDF_BITMAP); cdecl; external External_library + libext;
function FPDFBitmap_GetBuffer(Bitmap: FPDF_BITMAP): Pointer; cdecl; external External_library + libext;
function FPDFBitmap_GetStride(Bitmap: FPDF_BITMAP): Integer; cdecl; external External_library + libext;








function SaveBitmapToFile(BitmapBuffer: Pointer; Width, Height, Stride, PageIndex: Integer): Boolean;
var
  Bitmap: TBitmap;
  Row, Col: Integer;
  PixelPtr: PByte;
  Filename: string;
  ColorValue: TColor;
begin
  // Create a new TBitmap
  Bitmap := TBitmap.Create;
  try
    // Set bitmap size and pixel format
    Bitmap.Width := Width;
    Bitmap.Height := Height;
    Bitmap.PixelFormat := pf24bit; // Use 24-bit format (RGB)

    // Fill in the bitmap with the buffer data using Canvas.Pixels
    for Row := 0 to Height - 1 do
    begin
      PixelPtr := PByte(BitmapBuffer) + Row * Stride;

      for Col := 0 to Width - 1 do
      begin
        // Extract RGB values from the buffer (ignoring Alpha)
        ColorValue := RGB(PixelPtr[2], PixelPtr[1], PixelPtr[0]);  // Windows stores RGB in reverse order
        Bitmap.Canvas.Pixels[Col, Row] := ColorValue;

        Inc(PixelPtr, 4);  // Move to the next pixel (RGBA format, skip 4 bytes)
      end;
    end;

    // Create the filename
    Filename := 'page_' + IntToStr(PageIndex) + '.bmp';

    // Save the bitmap as a BMP file
    Bitmap.SaveToFile(Filename);
    WriteLn('Saved bitmap to BMP: ', Filename);
    Result := True;
  finally
    Bitmap.Free;
  end;
end;


var
  Document : FPDF_DOCUMENT;
   Page : FPDF_PAGE;
   Bitmap: FPDF_BITMAP;

  PageCount, Width, Height, Stride, I: Integer;
  InputPDF: string;
begin

  Writeln('size of Pointer: ', SizeOf(Pointer));
  Writeln('size of Integer: ', SizeOf(Integer));



  if ParamCount < 1 then
  begin
    WriteLn('Usage: pdf_to_bitmap <input_pdf>');
    Exit;
  end;

  InputPDF := ParamStr(1);

  // Load pdfium.dll
  //pdfiumModule := LoadLibrary('pdfium.dll');
  //if pdfiumModule = 0 then
  //begin
  //  WriteLn('Error: Failed to load pdfium.dll');
  //  Exit;
  //end;

  // Load the functions
  //if not LoadPdfiumFunctions(pdfiumModule) then
  //begin
  //  WriteLn('Error: Failed to load Pdfium functions');
  //  FreeLibrary(pdfiumModule);
  //  Exit;
  //end;

  // Initialize PDFium
  FPDF_InitLibrary();

  // Load the PDF document
  Document := FPDF_LoadDocument(PAnsiChar(AnsiString(InputPDF)), nil);
  if Document = nil then
  begin
    WriteLn('Error: Failed to open PDF document');
    FPDF_DestroyLibrary();
//    FreeLibrary(pdfiumModule);
    Exit;
  end;

  // Get the number of pages
  PageCount := FPDF_GetPageCount(Document);
  WriteLn('Page Count = ', PageCount);
    {
  if PageCount > 0 then
  begin
    Page := FPDF_LoadPage(Document, 0);
    if Page = nil Then
    begin
          WriteLn('Error: Failed to load page ');
    end;
    FPDF_ClosePage(Page);
  end;
     }

  for I := 0 to PageCount - 1 do
  begin
    WriteLn('Load Page start:', I);
    // Load the page
    Page := FPDF_LoadPage(Document, I);
    if Page = nil then
    begin
      WriteLn('Error: Failed to load page ', I);
      Continue;
    end;

    Width := 612;  // Example width
    Height := 792; // Example height

    // Create a bitmap
    Bitmap := FPDFBitmap_Create(Width, Height, 0);
    FPDFBitmap_FillRect(Bitmap, 0, 0, Width, Height, $FFFFFFFF);

    // Render the page onto the bitmap
    FPDF_RenderPageBitmap(Bitmap, Page, 0, 0, Width, Height, 0, 0);

    // Get the stride
    Stride := FPDFBitmap_GetStride(Bitmap);

    // Save the bitmap to a file
    if not SaveBitmapToFile(Bitmap, Width, Height, Stride, I) then
    begin
      WriteLn('Error: Failed to save page ', I, ' to bitmap.');
    end;

    // Destroy the bitmap
    FPDFBitmap_Destroy(Bitmap);

    // Close the page
    FPDF_ClosePage(Page);
  end;

  WriteLn('Close Document');
  // Close the document
  FPDF_CloseDocument(Document);
  WriteLn('DestroyLibrary');
  // Destroy the library
  FPDF_DestroyLibrary();

  // Free the DLL
//  FreeLibrary(pdfiumModule);

  WriteLn('PDF to bitmap conversion completed.');
end.


