unit PdfPage;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  PdfiumLib,
  PdfBitmap;

type
  TPdfPage = class; // Forward declaration

  IPdfDocument = interface
    function ReloadPage(APage: TPdfPage): FPDF_PAGE;
  end;

  TPdfPage = class(TObject)
  private
    FDocument: IPdfDocument;
    FPage: FPDF_PAGE;
    procedure Open;
  public
    constructor Create(ADocument: IPdfDocument; APage: FPDF_PAGE);
    destructor Destroy; override;
    procedure Close;
    procedure DrawToPdfBitmap(APdfBitmap: TPdfBitmap; X, Y, Width, Height: Integer);
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

procedure TPdfPage.Open;
begin
  if FPage = nil then
  begin
    FPage := FDocument.ReloadPage(Self);
  end;
end;

procedure TPdfPage.DrawToPdfBitmap(APdfBitmap: TPdfBitmap; X, Y, Width, Height: Integer);
begin
  Open;
  FPDF_RenderPageBitmap(APdfBitmap.Bitmap, FPage, X, Y, Width, Height, 0, 1);
end;

end.
