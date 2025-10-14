unit PdfiumLib;

{$MODE DelphiUnicode}

{$A8,B-,E-,F-,G+,H+,I+,J-,K-,M-,N-,P+,Q-,R-,S-,T-,U-,V+,X+,Z1}
{$STRINGCHECKS OFF}

interface

const
  PDFiumDll = 'pdfium.dll';

const
  // More DIB formats
  FPDFBitmap_Unknown = 0; // Unknown or unsupported format.
  FPDFBitmap_Gray    = 1; // Gray scale bitmap, one byte per pixel.
  FPDFBitmap_BGR     = 2; // 3 bytes per pixel, byte order: blue, green, red.
  FPDFBitmap_BGRx    = 3; // 4 bytes per pixel, byte order: blue, green, red, unused.
  FPDFBitmap_BGRA    = 4; // 4 bytes per pixel, byte order: blue, green, red, alpha.

  //定数・列挙型
type
  FPDF_BITMAP = Pointer;
  FPDF_DOCUMENT = Pointer;
  FPDF_PAGE = Pointer;
  FPDF_DWORD = Cardinal;
  FPDF_FORMFILLINFO = Pointer;

procedure InitPDFium(const DllPath: string = '' {$IFDEF PDF_ENABLE_V8}; const ResPath: string = ''{$ENDIF});
procedure FPDF_InitLibrary(); cdecl; external PDFiumDll name 'FPDF_InitLibrary';
procedure FPDF_DestroyLibrary(); cdecl; external PDFiumDll name 'FPDF_DestroyLibrary';

//ドキュメント関連関数
function FPDF_LoadDocument(const FileName: string; const Password: UTF8String): FPDF_DOCUMENT; cdecl;external PDFiumDll;
procedure FPDF_CloseDocument(document: FPDF_DOCUMENT); cdecl;external PDFiumDll;
function FPDF_GetPageCount(document: FPDF_DOCUMENT): Integer; cdecl;external PDFiumDll;

//ページ関連関数
function FPDF_LoadPage(document: FPDF_DOCUMENT; page: Integer): FPDF_PAGE; cdecl;external PDFiumDll;
procedure FPDF_ClosePage(page: FPDF_PAGE); cdecl;external PDFiumDll;
procedure FPDF_RenderPageBitmap(bitmap: FPDF_BITMAP; page: FPDF_PAGE;
  start_x, start_y, size_x, size_y: Integer;rotate: Integer; flags: Integer); cdecl;external PDFiumDll;

//ビットマップ関連関数
function FPDFBitmap_GetWidth(bitmap: FPDF_BITMAP): Integer; cdecl;external PDFiumDll;
function FPDFBitmap_GetHeight(bitmap: FPDF_BITMAP): Integer; cdecl;external PDFiumDll;
function FPDFBitmap_GetStride(bitmap: FPDF_BITMAP): Integer; cdecl;external PDFiumDll;
function FPDFBitmap_CreateEx(width, height: Integer; format: Integer; first_scan: Pointer;
    stride: Integer): FPDF_BITMAP;   cdecl;external PDFiumDll;
procedure FPDFBitmap_Destroy(bitmap: FPDF_BITMAP); cdecl;external PDFiumDll;
function FPDFBitmap_GetBuffer(bitmap: FPDF_BITMAP): Pointer; cdecl;external PDFiumDll;
procedure FPDFBitmap_FillRect(bitmap: FPDF_BITMAP; left: Integer; top: Integer; width: Integer; height: Integer; color: Integer); cdecl;external PDFiumDll;

implementation


procedure InitPDFium(const DllPath: string{$IFDEF PDF_ENABLE_V8}; const ResPath: string{$ENDIF});
begin
  FPDF_InitLibrary();
end;


end.
