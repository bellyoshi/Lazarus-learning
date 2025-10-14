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
  // Helper data type for type safety.
  __FPDF_PTRREC = record end;
  __PFPDF_PTRREC = ^__FPDF_PTRREC;
  
  FPDF_BITMAP = type __PFPDF_PTRREC;
  FPDF_DOCUMENT = type __PFPDF_PTRREC;
  FPDF_PAGE = type __PFPDF_PTRREC;
  FPDF_DWORD = Cardinal;
  FPDF_BOOL = Integer;
  FPDF_STRING = PAnsiChar;
  FPDF_BYTESTRING = PAnsiChar;
  
  // Timer callback function type
  TFPDFTimerCallback = procedure(idEvent: Integer); cdecl;
  
  //// System time structure
  //{$IFDEF MSWINDOWS}
  //PFPDF_SYSTEMTIME = PSystemTime;
  //FPDF_SYSTEMTIME = TSystemTime;
  //{$ELSE}
  //PFPDF_SYSTEMTIME = ^FPDF_SYSTEMTIME;
  //FPDF_SYSTEMTIME = record
  //  wYear: Word;          // years since 1900
  //  wMonth: Word;         // months since January - [0,11]
  //  wDayOfWeek: Word;     // days since Sunday - [0,6]
  //  wDay: Word;           // day of the month - [1,31]
  //  wHour: Word;          // hours since midnight - [0,23]
  //  wMinute: Word;        // minutes after the hour - [0,59]
  //  wSecond: Word;        // seconds after the minute - [0,59]
  //  wMilliseconds: Word;  // milliseconds after the second - [0,999]
  //end;
  //{$ENDIF MSWINDOWS}
  //
  //// Form fill info structure
  //PFPDF_FORMFILLINFO = ^FPDF_FORMFILLINFO;
  //FPDF_FORMFILLINFO = record
  //  // Version number of the interface.
  //  version: Integer;
  //  
  //  // Version 1 methods
  //  Release: procedure(pThis: PFPDF_FORMFILLINFO); cdecl;
  //  FFI_Invalidate: procedure(pThis: PFPDF_FORMFILLINFO; page: FPDF_PAGE; left, top, right, bottom: Double); cdecl;
  //  FFI_OutputSelectedRect: procedure(pThis: PFPDF_FORMFILLINFO; page: FPDF_PAGE; left, top, right, bottom: Double); cdecl;
  //  FFI_SetCursor: procedure(pThis: PFPDF_FORMFILLINFO; nCursorType: Integer); cdecl;
  //  FFI_SetTimer: function(pThis: PFPDF_FORMFILLINFO; uElapse: Integer; lpTimerFunc: TFPDFTimerCallback): Integer; cdecl;
  //  FFI_KillTimer: procedure(pThis: PFPDF_FORMFILLINFO; nTimerID: Integer); cdecl;
  //  FFI_GetLocalTime: function(pThis: PFPDF_FORMFILLINFO): FPDF_SYSTEMTIME; cdecl;
  //  FFI_OnChange: procedure(pThis: PFPDF_FORMFILLINFO); cdecl;
  //  FFI_GetPage: function(pThis: PFPDF_FORMFILLINFO; document: FPDF_DOCUMENT; nPageIndex: Integer): FPDF_PAGE; cdecl;
  //  FFI_GetCurrentPage: function(pThis: PFPDF_FORMFILLINFO; document: FPDF_DOCUMENT): FPDF_PAGE; cdecl;
  //  FFI_GetRotation: function(pThis: PFPDF_FORMFILLINFO; page: FPDF_PAGE): Integer; cdecl;
  //  FFI_ExecuteNamedAction: procedure(pThis: PFPDF_FORMFILLINFO; namedAction: FPDF_BYTESTRING); cdecl;
  //  FFI_SetTextFieldFocus: procedure(pThis: PFPDF_FORMFILLINFO; page: FPDF_PAGE; value: FPDF_BYTESTRING; valueLen: Integer; isFocus: FPDF_BOOL); cdecl;
  //  FFI_DoURIAction: procedure(pThis: PFPDF_FORMFILLINFO; uri: FPDF_BYTESTRING); cdecl;
  //  FFI_DoGoToAction: procedure(pThis: PFPDF_FORMFILLINFO; nPageIndex, zoomMode: Integer; fPosArray: PSingle; sizeofArray: Integer); cdecl;
  //  
  //  // Version 2 methods
  //  FFI_DoURIActionWithKeyboardModifier: procedure(param: PFPDF_FORMFILLINFO; uri: FPDF_BYTESTRING; modifiers: Integer); cdecl;
  //end;

procedure InitPDFium(const DllPath: string = '' {$IFDEF PDF_ENABLE_V8}; const ResPath: string = ''{$ENDIF});
procedure FPDF_InitLibrary(); cdecl; external PDFiumDll name 'FPDF_InitLibrary';
procedure FPDF_DestroyLibrary(); cdecl; external PDFiumDll name 'FPDF_DestroyLibrary';

//ドキュメント関連関数
function FPDF_LoadDocument(file_path: FPDF_STRING; password: FPDF_BYTESTRING): FPDF_DOCUMENT; cdecl; external PDFiumDll;
procedure FPDF_CloseDocument(document: FPDF_DOCUMENT); cdecl;external PDFiumDll;
function FPDF_GetPageCount(document: FPDF_DOCUMENT): Integer; cdecl;external PDFiumDll;

//ページ関連関数
function FPDF_LoadPage(document: FPDF_DOCUMENT; page: Integer): FPDF_PAGE; cdecl;external PDFiumDll;
procedure FPDF_ClosePage(page: FPDF_PAGE); cdecl;external PDFiumDll;
procedure FPDF_RenderPageBitmap(bitmap: FPDF_BITMAP; page: FPDF_PAGE;
  start_x, start_y, size_x, size_y: Integer;rotate: Integer; flags: Integer); cdecl;external PDFiumDll;

//ビットマップ関連関数
function FPDFBitmap_Create(width, height: Integer; alpha: Integer): FPDF_BITMAP; cdecl; external PDFiumDll;
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
