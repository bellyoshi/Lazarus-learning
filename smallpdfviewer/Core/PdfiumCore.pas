unit PdfiumCore;


{$MODE DelphiUnicode}

{$A8,B-,E-,F-,G+,H+,I+,J-,K-,M-,N-,P+,Q-,R-,S-,T-,U-,V+,X+,Z1}
{$STRINGCHECKS OFF}


interface

{.$UNDEF MSWINDOWS}

uses

  Types,
  SysUtils,
  Classes,
  Contnrs,
  Math,
  PdfiumLib;


type

  TPdfDocument = class;
  TPdfPage = class;

  TPdfBitmapFormat = (
    bfGrays = FPDFBitmap_Gray, // Gray scale bitmap, one byte per pixel.
    bfBGR   = FPDFBitmap_BGR,  // 3 bytes per pixel, byte order: blue, green, red.
    bfBGRx  = FPDFBitmap_BGRx, // 4 bytes per pixel, byte order: blue, green, red, unused.
    bfBGRA  = FPDFBitmap_BGRA  // 4 bytes per pixel, byte order: blue, green, red, alpha.
  );


  // Make the TObject.Create constructor private to hide it, so that the TPdfBitmap.Create
  // overloads won't allow calling TObject.Create.
  _TPdfBitmapHideCtor = class(TObject)
  private
    constructor Create;
  end;

  TPdfBitmap = class(_TPdfBitmapHideCtor)
  private
    FBitmap: FPDF_BITMAP;
    FOwnsBitmap: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FBytesPerScanLine: Integer;
  public
    constructor Create(ABitmap: FPDF_BITMAP; AOwnsBitmap: Boolean = False); overload;
    constructor Create(AWidth, AHeight: Integer; AAlpha: Boolean); overload;
    constructor Create(AWidth, AHeight: Integer; AFormat: TPdfBitmapFormat); overload;
    constructor Create(AWidth, AHeight: Integer; AFormat: TPdfBitmapFormat; ABuffer: Pointer; ABytesPerScanline: Integer); overload;
    destructor Destroy; override;

    procedure FillRect(ALeft, ATop, AWidth, AHeight: Integer; AColor: FPDF_DWORD);
    function GetBuffer: Pointer;

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property BytesPerScanline: Integer read FBytesPerScanLine;
    property Bitmap: FPDF_BITMAP read FBitmap;
  end;

  PPdfFormFillHandler = ^TPdfFormFillHandler;
  TPdfFormFillHandler = record
    FormFillInfo: FPDF_FORMFILLINFO;
    Document: TPdfDocument;
  end;

  TPdfPage = class(TObject)
  private
    FDocument: TPdfDocument;
    FPage: FPDF_PAGE;
    constructor Create(ADocument: TPdfDocument; APage: FPDF_PAGE);
    procedure Open;
  public
    destructor Destroy; override;
    procedure Close;

    // Draw the PDF page without the form field values into the bitmap.
    procedure DrawToPdfBitmap(APdfBitmap: TPdfBitmap; X, Y, Width, Height: Integer);
  end;

  TPdfDocument = class(TObject)
  private
    FDocument: FPDF_DOCUMENT;
    FPages: TObjectList;
    FFileName: string;
    FBuffer: PByte;
    FBytes: TBytes;
    FClosing: Boolean;
    FUnsupportedFeatures: Boolean;

    procedure InternLoadFromFile(const FileName: string; const Password: UTF8String);
    function GetPage(Index: Integer): TPdfPage;
    function GetPageCount: Integer;
    procedure ExtractPage(APage: TPdfPage);
    function ReloadPage(APage: TPdfPage): FPDF_PAGE;
    procedure DocumentLoaded;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: string; const Password: UTF8String = '');
    procedure Close;

    property FileName: string read FFileName;
    property PageCount: Integer read GetPageCount;
    property Pages[Index: Integer]: TPdfPage read GetPage;
  end;

var
  PDFiumDllDir: string = '';

implementation

var
  PDFiumInitCritSect: TRTLCriticalSection;

procedure InitLib;
{$J+}
const
  Initialized: boolean = false;
{$J-}
begin
  if not Initialized then
  begin
    EnterCriticalSection(PDFiumInitCritSect);
    try
      if not Initialized then
      begin
        {$IFDEF CPUX64}
        // PDFium requires all arithmetic exceptions to be masked in 64bit mode
        SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
        {$ENDIF CPUX64}
        InitPDFium(PDFiumDllDir);
        Initialized := true;
      end;
    finally
      LeaveCriticalSection(PDFiumInitCritSect);
    end;
  end;
end;

{ TPdfDocument }

constructor TPdfDocument.Create;
begin
  inherited Create;
  FPages := TObjectList.Create;

  InitLib;
end;

destructor TPdfDocument.Destroy;
begin
  Close;
  FPages.Free;
  inherited Destroy;
end;

procedure TPdfDocument.Close;
begin
  FClosing := True;
  try
    FPages.Clear;
    FUnsupportedFeatures := False;

    if FDocument <> nil then
    begin
      FPDF_CloseDocument(FDocument);
      FDocument := nil;
    end;

    if FBuffer <> nil then
    begin
      FreeMem(FBuffer);
      FBuffer := nil;
    end;
    FBytes := nil;

    FFileName := '';
  finally
    FClosing := False;
  end;
end;

procedure TPdfDocument.LoadFromFile(const FileName: string; const Password: UTF8String);
begin
  Close;
  InternLoadFromFile(FileName, Password);
  FFileName := FileName;
end;

procedure TPdfDocument.InternLoadFromFile(const FileName: string; const Password: UTF8String);
var
  Utf8FileName: UTF8String;
begin
  Utf8FileName := UTF8Encode(FileName);
  FDocument := FPDF_LoadDocument(PAnsiChar(Utf8FileName), PAnsiChar(Pointer(Password)));
  DocumentLoaded;
end;

procedure TPdfDocument.DocumentLoaded;
begin
  FPages.Count := FPDF_GetPageCount(FDocument);
end;

function TPdfDocument.GetPage(Index: Integer): TPdfPage;
var
  LPage: FPDF_PAGE;
begin
  Result := TPdfPage(FPages[Index]);
  if Result = nil then
  begin
    LPage := FPDF_LoadPage(FDocument, Index);
    Result := TPdfPage.Create(Self, LPage);
    FPages[Index] := Result;
  end
end;

function TPdfDocument.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TPdfDocument.ExtractPage(APage: TPdfPage);
begin
  if not FClosing then
    FPages.Extract(APage);
end;

function TPdfDocument.ReloadPage(APage: TPdfPage): FPDF_PAGE;
var
  Index: Integer;
begin
  Index := FPages.IndexOf(APage);
  Result := FPDF_LoadPage(FDocument, Index);
end;

{ TPdfPage }

constructor TPdfPage.Create(ADocument: TPdfDocument; APage: FPDF_PAGE);
begin
  inherited Create;
  FDocument := ADocument;
  FPage := APage;
end;

destructor TPdfPage.Destroy;
begin
  Close;
  FDocument.ExtractPage(Self);
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
  FPDF_RenderPageBitmap(APdfBitmap.FBitmap, FPage, X, Y, Width, Height, 0, 1);
end;



{ _TPdfBitmapHideCtor }

constructor _TPdfBitmapHideCtor.Create;
begin
  inherited Create;
end;


{ TPdfBitmap }


constructor TPdfBitmap.Create(ABitmap: FPDF_BITMAP; AOwnsBitmap: Boolean);
begin
  inherited Create;
  FBitmap := ABitmap;
  FOwnsBitmap := AOwnsBitmap;
  if FBitmap <> nil then
  begin
    FWidth := FPDFBitmap_GetWidth(FBitmap);
    FHeight := FPDFBitmap_GetHeight(FBitmap);
    FBytesPerScanLine := FPDFBitmap_GetStride(FBitmap);
  end;
end;

constructor TPdfBitmap.Create(AWidth, AHeight: Integer; AAlpha: Boolean);
begin
  Create(FPDFBitmap_Create(AWidth, AHeight, Ord(AAlpha)), True);
end;

constructor TPdfBitmap.Create(AWidth, AHeight: Integer; AFormat: TPdfBitmapFormat);
begin
  Create(FPDFBitmap_CreateEx(AWidth, AHeight, Ord(AFormat), nil, 0), True);
end;

constructor TPdfBitmap.Create(AWidth, AHeight: Integer; AFormat: TPdfBitmapFormat; ABuffer: Pointer;
  ABytesPerScanLine: Integer);
begin
  Create(FPDFBitmap_CreateEx(AWidth, AHeight, Ord(AFormat), ABuffer, ABytesPerScanline), True);
end;

destructor TPdfBitmap.Destroy;
begin
  if FOwnsBitmap and (FBitmap <> nil) then
    FPDFBitmap_Destroy(FBitmap);
  inherited Destroy;
end;

function TPdfBitmap.GetBuffer: Pointer;
begin
  if FBitmap <> nil then
    Result := FPDFBitmap_GetBuffer(FBitmap)
  else
    Result := nil;
end;

procedure TPdfBitmap.FillRect(ALeft, ATop, AWidth, AHeight: Integer; AColor: FPDF_DWORD);
begin
  if FBitmap <> nil then
    FPDFBitmap_FillRect(FBitmap, ALeft, ATop, AWidth, AHeight, AColor);
end;

initialization
  InitCriticalSection(PDFiumInitCritSect);

finalization
  DoneCriticalSection(PDFiumInitCritSect);

end.
