unit PdfiumUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Math, PdfiumLib;

procedure InitializeLibrary;

implementation

procedure InitializeLibraryImpl;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  FPDF_InitLibrary();
end;

var Initialized: boolean = false;
procedure InitializeLibrary;
begin
  if not Initialized then
  begin
    InitializeLibraryImpl;
    Initialized := true;
  end;
end;

end.
