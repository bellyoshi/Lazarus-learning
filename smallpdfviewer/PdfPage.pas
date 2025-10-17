unit PdfPage;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  PdfiumLib;

type

  TPdfPage = class(TObject)
  private
    FPage: FPDF_PAGE;
  public
    constructor Create(ADocument:FPDF_DOCUMENT ; index : Integer);
    destructor Destroy; override;
    procedure Close;
    property Page: FPDF_PAGE read FPage;
  end;

implementation

{ TPdfPage }

constructor TPdfPage.Create(ADocument : FPDF_DOCUMENT; index : Integer);
begin
  inherited Create;
  FPage := FPDF_LoadPage(ADocument, index);
end;

destructor TPdfPage.Destroy;
begin
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
