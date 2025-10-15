unit PdfViewer;

{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils, PdfiumCore, PdfBitmap, Graphics;

type
  TPdfViewer  = class

  private
    FPdfDocument: TPdfDocument;
    FPageIndex: Integer;
    procedure SetPageIndex(AValue: Integer);
    procedure DrawToBitmap(Page: TPdfPage; Bitmap: TBitmap; Width, Height: Integer);

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

uses
  GraphType;

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

procedure TPdfViewer.DrawToBitmap(Page: TPdfPage; Bitmap: TBitmap; Width, Height: Integer);
var
  SizeInt: Integer;
  PdfBitmap: TPdfBitmap;
  ImageData: TBytes;
  buffer: Pointer;
  RawImage: TRawImage;
begin
  PdfBitmap := TPdfBitmap.Create(Width, Height, BitmapFormat_bfBGRA);
  try
    PdfBitmap.FillRect(0, 0, Width, Height, $FFFFFFFF);
    Page.DrawToPdfBitmap(PdfBitmap, 0, 0, Width, Height);

    SizeInt := Width * Height * 4;

    ImageData := nil;
    SetLength(ImageData, SizeInt);

    buffer := PdfBitmap.GetBuffer;
    Move(buffer^, ImageData[0], SizeInt);

    RawImage.Init;
    RawImage.Description.Init_BPP32_B8G8R8A8_M1_BIO_TTB(Width, Height);
    RawImage.CreateData(true);
    RawImage.Data:=@ImageData[0];

    Bitmap.LoadFromRawImage(RawImage, false);
  finally
    PdfBitmap.Free;
  end;
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
begin
  if not Assigned(FPdfDocument) then
    raise Exception.Create('PDF document not loaded');

  PdfPage := FPdfDocument.Pages[FPageIndex];

  Bitmap := TBitmap.Create;
  Bitmap.Width := Width;
  Bitmap.Height := Height;

  DrawToBitmap(PdfPage, Bitmap, Width, Height);

  Result := Bitmap;
end;

end.
