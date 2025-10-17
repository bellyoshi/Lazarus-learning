unit PdfiumUtils;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Math, PdfiumLib;

procedure InitializeLibrary;

implementation

procedure InitializeLibraryImpl;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  FPDF_InitLibrary();
end;

procedure InitializeLibrary;
{$J+}
const
  Initialized: boolean = false;
{$J-}
begin
  if not Initialized then
  begin
    InitializeLibraryImpl;
    Initialized := true;
  end;
end;

end.
