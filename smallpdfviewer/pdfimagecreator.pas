unit PdfImageCreator;

{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils, PdfiumCore, Graphics;

procedure DrawToBitmap(Page: TPdfPage; Bitmap: TBitmap; w,h : Integer);

type
  TPdfImageCreator  = class

  private
    FPdfDocument: TPdfDocument;
    FPageIndex: Integer;
    FFilePath: string;
    procedure SetPageIndex(AValue: Integer);

  public
    constructor Create(const Filename: string; PageIndex: Integer = 0);
    destructor Destroy; override;
    function GetBitmap(Width, Height: Integer): TBitmap;
    function GetPageCount : Integer ;
    property PageCount : Integer read GetPageCount;
    property PageIndex : Integer read FPageIndex write SetPageIndex;
  end;

implementation

uses
  GraphType;

procedure TPdfImageCreator.SetPageIndex(AValue: Integer);
begin
  if AValue < 0 then
    FPageIndex := 0
  else if AValue >= GetPageCount then
    FPageIndex := GetPageCount - 1
  else
    FPageIndex := AValue;
end;

function TPdfImageCreator.GetPageCount : Integer;
begin
  Result := FPdfDocument.PageCount;
end;

procedure DrawToBitmap(Page: TPdfPage; Bitmap: TBitmap; w,h : Integer);
var
  SizeInt: Integer;
  PdfBitmap: TPdfBitmap;
  AIMarge: TBytes;
  buffer: Pointer;
  RawImage: TRawImage;
begin
  // PDFium ビットマップを作成
  PdfBitmap := TPdfBitmap.Create(w, h, bfBGRA);
  try
    PdfBitmap.FillRect(0, 0, w, h, $FFFFFFFF); // 背景を白色で塗りつぶし
    Page.DrawToPdfBitmap(PdfBitmap, 0, 0, w, h);

    // PDFium ビットマップのバッファサイズを計算
    SizeInt := w * h * 4; // 4バイト/ピクセル (RGBA)

    // バイト配列を初期化
    AIMarge := nil; // 明示的に初期化
    SetLength(AIMarge, SizeInt);

    // PDFium のバッファを取得して、バイト配列にコピー
    buffer := PdfBitmap.GetBuffer;
    Move(buffer^, AIMarge[0], SizeInt);

    // バイト配列から Delphi のビットマップにデータをコピー
    RawImage.Init;
    RawImage.Description.Init_BPP32_B8G8R8A8_M1_BIO_TTB(w, h);
    RawImage.CreateData(true);
    RawImage.Data:=@AIMarge[0];

    Bitmap.LoadFromRawImage(RawImage, false);
  finally
    PdfBitmap.Free;
  end;
end;
{ TPdfImageCreator }

constructor TPdfImageCreator.Create(const Filename: string; PageIndex: Integer = 0);
begin
  inherited Create;

  FFilePath := Filename;


  // PDF ドキュメントをロード
  FPdfDocument := TPdfDocument.Create;

  if not FileExists(Filename) then
    raise Exception.Create('File not found: ' + Filename);

  FPdfDocument.LoadFromFile(Filename);

  // デフォルトでは、最初のページ (0) を表示
  FPageIndex := PageIndex;

  if (FPageIndex < 0) or (FPageIndex >= FPdfDocument.PageCount) then
    raise Exception.Create('Invalid page index');
end;

destructor TPdfImageCreator.Destroy;
begin
  FPdfDocument.Close();
  FPdfDocument.Free();
  inherited;
end;

function TPdfImageCreator.GetBitmap(Width, Height: Integer): TBitmap;
var
  PdfPage: TPdfPage;
  Bitmap: TBitmap;
begin
  if not Assigned(FPdfDocument) then
    raise Exception.Create('PDF document not loaded');

  PdfPage := FPdfDocument.Pages[FPageIndex];  // 指定されたページを取得

  // ビットマップの作成
  Bitmap := TBitmap.Create;
  Bitmap.Width := Width;
  Bitmap.Height := Height;

  DrawToBitmap(PdfPage, Bitmap, Width, Height);

  Result := Bitmap;
end;

end.
