unit PdfBitmap;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  PdfiumLib;

const
  BitmapFormat_bfBGRA = 4;

type
  TPdfBitmap = class(TObject)
  private
    FBitmap: FPDF_BITMAP;
    FOwnsBitmap: Boolean;
    FWidth: Integer;
    FHeight: Integer;
  public
    constructor Create(ABitmap: FPDF_BITMAP; AOwnsBitmap: Boolean = False); overload;
    constructor Create(AWidth, AHeight: Integer; AFormat: Integer); overload;
    destructor Destroy; override;

    procedure FillRect(ALeft, ATop, AWidth, AHeight: Integer; AColor: FPDF_DWORD);
    function GetBuffer: Pointer;

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Bitmap: FPDF_BITMAP read FBitmap;
  end;

implementation

constructor TPdfBitmap.Create(ABitmap: FPDF_BITMAP; AOwnsBitmap: Boolean);
begin
  inherited Create;
  FBitmap := ABitmap;
  FOwnsBitmap := AOwnsBitmap;
  if FBitmap <> nil then
  begin
    FWidth := FPDFBitmap_GetWidth(FBitmap);
    FHeight := FPDFBitmap_GetHeight(FBitmap);
  end;
end;

constructor TPdfBitmap.Create(AWidth, AHeight: Integer; AFormat: Integer);
begin
  Create(FPDFBitmap_CreateEx(AWidth, AHeight, AFormat, nil, 0), True);
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

end.
