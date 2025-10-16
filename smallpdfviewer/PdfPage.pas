unit PdfPage;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  PdfiumLib;

type
  TPdfPage = class; // Forward declaration

  IPdfDocument = interface
  end;

  TPdfPage = class(TObject)
  private
    FDocument: IPdfDocument;
    FPage: FPDF_PAGE;
  public
    constructor Create(ADocument: IPdfDocument; APage: FPDF_PAGE);
    destructor Destroy; override;
    procedure Close;
    property Page: FPDF_PAGE read FPage;
  end;

implementation

{ TPdfPage }

constructor TPdfPage.Create(ADocument: IPdfDocument; APage: FPDF_PAGE);
begin
  inherited Create;
  FDocument := ADocument;
  FPage := APage;
end;

destructor TPdfPage.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TPdfPage.Close;
begin
  if FPage <> nil then
  begin
    FPDF_ClosePage(FPage);
    FPage := nil;
  end;
end;




end.
