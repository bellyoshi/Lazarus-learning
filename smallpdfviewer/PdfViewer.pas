unit PdfViewer;

{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils, PdfDocument, PdfiumLib, Graphics, PdfRenderer;

type
  TPdfViewer  = class

  private
    FPdfDocument: TPdfDocument;
    FPageIndex: Integer;
    procedure SetPageIndex(AValue: Integer);

  public
    constructor Create(const Filename: string);
    destructor Destroy; override;
    function GetBitmap(Width, Height: Integer): TBitmap;
    function GetPageCount : Integer ;
    function CanNext: Boolean;
    function CanPrevious: Boolean;
    procedure Next;
    procedure Previous;
    property PageCount : Integer read GetPageCount;
    property PageIndex : Integer read FPageIndex write SetPageIndex;
  end;

implementation

procedure TPdfViewer.SetPageIndex(AValue: Integer);
begin
  if AValue < 0 then
    FPageIndex := 0
  else if AValue >= GetPageCount then
    FPageIndex := GetPageCount - 1
  else
    FPageIndex := AValue;
end;

function TPdfViewer.GetPageCount : Integer;
begin
  Result := FPdfDocument.PageCount;
end;

function TPdfViewer.CanNext: Boolean;
begin
  Result := Assigned(FPdfDocument) and (FPageIndex < GetPageCount - 1);
end;

function TPdfViewer.CanPrevious: Boolean;
begin
  Result := Assigned(FPdfDocument) and (FPageIndex > 0);
end;

procedure TPdfViewer.Next;
begin
  if CanNext then
    FPageIndex := FPageIndex + 1;
end;

procedure TPdfViewer.Previous;
begin
  if CanPrevious then
    FPageIndex := FPageIndex - 1;
end;


constructor TPdfViewer.Create(const Filename: string);
begin
  inherited Create;
  FPdfDocument := TPdfDocument.Create;

  if not FileExists(Filename) then
    raise Exception.Create('File not found: ' + Filename);

  FPdfDocument.LoadFromFile(Filename);

  FPageIndex := 0;
end;

destructor TPdfViewer.Destroy;
begin
  FPdfDocument.Close();
  FPdfDocument.Free();
  inherited;
end;

function TPdfViewer.GetBitmap(Width, Height: Integer): TBitmap;
var
  PdfPage: TPdfPage;
  Bitmap: TBitmap;
  LPage: FPDF_PAGE;
begin
  if not Assigned(FPdfDocument) then
    raise Exception.Create('PDF document not loaded');

  PdfPage := FPdfDocument.Pages[FPageIndex];
  if PdfPage = nil then
  begin
    LPage := FPDF_LoadPage(FPdfDocument.Document, FPageIndex);
    PdfPage := TPdfPage.Create(FPdfDocument, LPage);
    FPdfDocument.SetPage(FPageIndex, PdfPage);
  end;

  Bitmap := TBitmap.Create;
  Bitmap.Width := Width;
  Bitmap.Height := Height;

  DrawToBitmap(PdfPage, Bitmap, Width, Height);

  Result := Bitmap;
end;

end.
