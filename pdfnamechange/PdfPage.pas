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
    function GetWidth: Double;
    function GetHeight: Double;
  public
    constructor Create(ADocument:FPDF_DOCUMENT ; index : Integer);
    destructor Destroy; override;
    procedure Close;
    property Page: FPDF_PAGE read FPage;
    property Width: Double read GetWidth;
    property Height: Double read GetHeight;
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

function TPdfPage.GetWidth: Double;
begin
  if FPage <> nil then
    Result := FPDF_GetPageWidth(FPage)
  else
    Result := 0.0;
end;

function TPdfPage.GetHeight: Double;
begin
  if FPage <> nil then
    Result := FPDF_GetPageHeight(FPage)
  else
    Result := 0.0;
end;




end.
