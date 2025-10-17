unit PdfDocument;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  Math,
  PdfiumLib,
  PdfPage;

type


  TPdfDocument = class
  private
    FDocument: FPDF_DOCUMENT;
    FPages: array of TPdfPage;
    FFileName: string;
    FClosing: Boolean;

    procedure InternLoadFromFile(const FileName: string; const Password: UTF8String);
    function GetPage(Index: Integer): TPdfPage;
    procedure SetPage(Index: Integer; APage: TPdfPage);
    function GetPageCount: Integer;
    procedure DocumentLoaded;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: string; const Password: UTF8String = '');
    procedure Close;

    function GetFileName: string;

    property FileName: string read GetFileName;
    property PageCount: Integer read GetPageCount;
    property Pages[Index: Integer]: TPdfPage read GetPage write SetPage;
    property Document: FPDF_DOCUMENT read FDocument;
  end;

implementation

procedure InitLib;
{$J+}
const
  Initialized: boolean = false;
{$J-}
begin
  if not Initialized then
  begin
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
    FPDF_InitLibrary();
    Initialized := true;
  end;
end;

{ TPdfDocument }
constructor TPdfDocument.Create;
begin
  inherited Create;

  InitLib;
end;

destructor TPdfDocument.Destroy;
var
  i : Integer;
begin
  Close;
  for i := 0 to Length(FPages) - 1 do
  begin
    FPages[i].Free;
  end;
  SetLength(FPages, 0);
  inherited Destroy;
end;

procedure TPdfDocument.Close;
var
  i : Integer;
begin
  FClosing := True;
  try
  for i := 0 to Length(FPages) - 1 do
  begin
    FPages[i].Close;
  end;

    if FDocument <> nil then
    begin
      FPDF_CloseDocument(FDocument);
      FDocument := nil;
    end;

    FFileName := '';

  finally
    FClosing := False;
  end;
end;

procedure TPdfDocument.LoadFromFile(const FileName: string; const Password: UTF8String);
begin
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
