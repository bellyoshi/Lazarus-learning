unit PdfDocument;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  PdfiumLib,
  PdfPage,
  PdfiumInitializer;

type

  TPdfDocument = class
  private
    FDocument: FPDF_DOCUMENT;
    FPages: array of TPdfPage;
    FFileName: string;

    function GetPage(Index: Integer): TPdfPage;
    procedure SetPage(Index: Integer; APage: TPdfPage);
    function GetPageCount: Integer;
    function GetFileName: string;
    procedure InitializePages;
    procedure CloseAllPages;
    procedure CloseDocument;
    procedure FreeAllPages;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: string; const Password: UTF8String = '');

    property FileName: string read GetFileName;
    property PageCount: Integer read GetPageCount;
    property Pages[Index: Integer]: TPdfPage read GetPage write SetPage;
    property Document: FPDF_DOCUMENT read FDocument;
  end;

implementation

{ TPdfDocument }
constructor TPdfDocument.Create;
begin
  inherited Create;
  InitializeLibrary;
end;

destructor TPdfDocument.Destroy;
begin
  CloseAllPages;
  CloseDocument;
  FreeAllPages;
  inherited Destroy;
end;

procedure TPdfDocument.CloseAllPages;
var
  i: Integer;
begin
  for i := 0 to Length(FPages) - 1 do
  begin
    FPages[i].Close;
  end;
end;

procedure TPdfDocument.CloseDocument;
begin
  if FDocument <> nil then
  begin
    FPDF_CloseDocument(FDocument);
    FDocument := nil;
  end;
  FFileName := '';
end;

procedure TPdfDocument.FreeAllPages;
var
  i: Integer;
begin
  for i := 0 to Length(FPages) - 1 do
  begin
    FPages[i].Free;
  end;
  SetLength(FPages, 0);
end;

procedure TPdfDocument.LoadFromFile(const FileName: string; const Password: UTF8String);
var
  Utf8FileName: UTF8String;
begin
  Utf8FileName := UTF8Encode(FileName);
  FDocument := FPDF_LoadDocument(PAnsiChar(Utf8FileName), PAnsiChar(Pointer(Password)));
  InitializePages;
  FFileName := FileName;
end;

procedure TPdfDocument.InitializePages;
var
  i : Integer;
  count : Integer;
begin
  count := FPDF_GetPageCount(FDocument);
  SetLength(FPages, count);
  for i := 0 to count - 1 do
  begin
    FPages[i] := TPdfPage.Create(FDocument, i);
  end;
end;

function TPdfDocument.GetPage(Index: Integer): TPdfPage;
begin
  Result := FPages[Index];
end;

procedure TPdfDocument.SetPage(Index: Integer; APage: TPdfPage);
begin
  FPages[Index] := APage;
end;

function TPdfDocument.GetPageCount: Integer;
begin
  Result := Length(FPages);
end;

function TPdfDocument.GetFileName: string;
begin
  Result := FFileName;
end;

end.
