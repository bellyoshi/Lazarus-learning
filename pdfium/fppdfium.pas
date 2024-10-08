unit fpPDFium;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ctypes, dynlibs;

type
  FPDF_DOCUMENT = Pointer;
  FPDF_PAGE = Pointer;
  procedure FPDF_InitLibrary(); cdecl; external 'pdfium.dll';
  function FPDF_LoadDocument(const file_path: PChar; const password: PChar): FPDF_DOCUMENT; cdecl; external 'pdfium.dll' ;
  function FPDF_LoadPage(document: FPDF_DOCUMENT; page_index: cint): FPDF_PAGE; cdecl;external 'pdfium.dll'               ;
  procedure FPDF_RenderPageBitmap(bitmap: Pointer; page: FPDF_PAGE; start_x, start_y, size_x, size_y: cint; rotate: cint; flags: cint); cdecl;external 'pdfium.dll'   ;
  procedure FPDF_CloseDocument(document: FPDF_DOCUMENT); cdecl; external 'pdfium.dll'    ;
  procedure FPDF_ClosePage(page: FPDF_PAGE); cdecl;external 'pdfium.dll'                   ;



implementation



end.

