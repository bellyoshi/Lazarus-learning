unit PdfDocument;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  Contnrs,
  Math,
  PdfiumLib,
  PdfPage;

type


  TPdfDocument = class(TInterfacedObject, IPdfDocument)
  private
    FDocument: FPDF_DOCUMENT;
    FPages: TObjectList;
    FFileName: string;
    FClosing: Boolean;

    procedure InternLoadFromFile(const FileName: string; const Password: UTF8String);
    function GetPage(Index: Integer): TPdfPage;
    function GetPageCount: Integer;
    procedure DocumentLoaded;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: string; const Password: UTF8String = '');
    procedure Close;
    procedure SetPage(Index: Integer; APage: TPdfPage);
    function ReloadPage(APage: TPdfPage): FPDF_PAGE;
    function GetFileName: string;

    property FileName: string read GetFileName;
    property PageCount: Integer read GetPageCount;
    property Pages[Index: Integer]: TPdfPage read GetPage;
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
begin
  Result := TPdfPage(FPages[Index]);
end;

function TPdfDocument.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

function TPdfDocument.GetFileName: string;
begin
  Result := FFileName;
end;

function TPdfDocument.ReloadPage(APage: TPdfPage): FPDF_PAGE;
var
  Index: Integer;
begin
  Index := FPages.IndexOf(APage);
  Result := FPDF_LoadPage(FDocument, Index);
end;

procedure TPdfDocument.SetPage(Index: Integer; APage: TPdfPage);
begin
  FPages[Index] := APage;
end;


end.
