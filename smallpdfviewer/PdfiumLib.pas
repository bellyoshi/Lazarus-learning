unit PdfiumLib;

{$MODE DelphiUnicode}

{$A8,B-,E-,F-,G+,H+,I+,J-,K-,M-,N-,P+,Q-,R-,S-,T-,U-,V+,X+,Z1}
{$STRINGCHECKS OFF}

interface

const
  PDFiumDll = 'pdfium.dll';

type
  __FPDF_PTRREC = record end;
  __PFPDF_PTRREC = ^__FPDF_PTRREC;
  
  FPDF_BITMAP = type __PFPDF_PTRREC;
  FPDF_DOCUMENT = type __PFPDF_PTRREC;
  FPDF_PAGE = type __PFPDF_PTRREC;
  FPDF_DWORD = Cardinal;
  FPDF_STRING = PAnsiChar;
  FPDF_BYTESTRING = PAnsiChar;


procedure FPDF_InitLibrary(); cdecl; external PDFiumDll name 'FPDF_InitLibrary';
procedure FPDF_DestroyLibrary(); cdecl; external PDFiumDll name 'FPDF_DestroyLibrary';

function FPDF_LoadDocument(file_path: FPDF_STRING; password: FPDF_BYTESTRING): FPDF_DOCUMENT; cdecl; external PDFiumDll;
procedure FPDF_CloseDocument(document: FPDF_DOCUMENT); cdecl;external PDFiumDll;
function FPDF_GetPageCount(document: FPDF_DOCUMENT): Integer; cdecl;external PDFiumDll;

function FPDF_LoadPage(document: FPDF_DOCUMENT; page: Integer): FPDF_PAGE; cdecl;external PDFiumDll;
procedure FPDF_ClosePage(page: FPDF_PAGE); cdecl;external PDFiumDll;
procedure FPDF_RenderPageBitmap(bitmap: FPDF_BITMAP; page: FPDF_PAGE;
  start_x, start_y, size_x, size_y: Integer;rotate: Integer; flags: Integer); cdecl;external PDFiumDll;

function FPDFBitmap_GetWidth(bitmap: FPDF_BITMAP): Integer; cdecl;external PDFiumDll;
function FPDFBitmap_GetHeight(bitmap: FPDF_BITMAP): Integer; cdecl;external PDFiumDll;
function FPDFBitmap_CreateEx(width, height: Integer; format: Integer; first_scan: Pointer;
    stride: Integer): FPDF_BITMAP;   cdecl;external PDFiumDll;
procedure FPDFBitmap_Destroy(bitmap: FPDF_BITMAP); cdecl;external PDFiumDll;
function FPDFBitmap_GetBuffer(bitmap: FPDF_BITMAP): Pointer; cdecl;external PDFiumDll;
procedure FPDFBitmap_FillRect(bitmap: FPDF_BITMAP; left: Integer; top: Integer; width: Integer; height: Integer; color: Integer); cdecl;external PDFiumDll;

implementation

end.
