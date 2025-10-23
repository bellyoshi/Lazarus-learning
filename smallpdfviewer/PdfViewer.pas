unit PdfViewer;

{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils, PdfDocument, PdfPage, Graphics, PdfRenderer;

type
  TPdfViewer  = class

  private
    FPdfDocument: TPdfDocument;
    FPageIndex: Integer;
    procedure SetPageIndex(AValue: Integer);

  public
    constructor Create(const Filename: string);
    destructor Destroy; override;
    function GetBitmap(ClientWidth, ClientHeight: Integer): TBitmap;
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
  FPdfDocument.Free;
  inherited;
end;

function GetEmptyBitmap(Width, Height: Integer): TBitmap;
begin
  Result := TBitmap.Create;
  Result.Width := Width;
  Result.Height := Height;
end;

function TPdfViewer.GetBitmap(ClientWidth, ClientHeight: Integer): TBitmap;
var
  PdfPage: TPdfPage;
  PageWidth: Double;
  PageHeight: Double;
  AspectRatio: Double;
  Width: Integer;
  Height: Integer;
begin
// ページのサイズからアスペクト比を計算
  PageWidth := FPdfDocument.Pages[FPageIndex].Width;
  PageHeight := FPdfDocument.Pages[FPageIndex].Height;
  AspectRatio := PageWidth / PageHeight;

  // クライアントサイズからビットマップサイズを計算
  if ClientWidth / ClientHeight > AspectRatio then
  begin
    Width := Round(ClientHeight * AspectRatio);
    Height := ClientHeight;
  end
  else
  begin
    Width := ClientWidth;
    Height := Round(ClientWidth / AspectRatio);
  end;

  PdfPage := FPdfDocument.Pages[FPageIndex];
  Result := PdfRenderer.GetBitmap(PdfPage, Width, Height);

end;


end.
