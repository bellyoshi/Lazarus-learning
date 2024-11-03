unit ViewerModel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, PdfImageCreator;


type

  TFilesParam = record
    Filename: string;
    Index: Integer;
  end;

  TViewerModel = class
  private
    FViewPdfDocument: TPdfImageCreator;
    FOperationPdfDocument: TPdfImageCreator;
    FFilesList: array of TFilesParam;
    FPdfImageCreatorList : array of TPdfImageCreator;
    FSelectIndex : Integer;

    function GetViewRatio: Double;
    function GetThumbnailRatio: Double;
    function GetHasViewDocument: Boolean;
    function GetHasOperationDocument: Boolean;
    function GetCanNext: Boolean;
    function GetCanPrevious: Boolean;
    function GetCanLast: Boolean;
    function GetCanFirst: Boolean;
    function GetPageIndex: Integer;
    function GetPageCount: Integer;

  public
    procedure Next;
    procedure SetPageIndex(value: Integer);
    procedure Previous;
    destructor Destroy; override;
    function Open(const Filename: string): Boolean;
    procedure View;
    constructor Create();
    function GetViewBitmap(Width, Height: Integer): TBitmap;
    function GetThumbnailBitmap(Width, Height: Integer): TBitmap;
    property HasViewDocument: Boolean read GetHasViewDocument;
    property HasOperationDocument: Boolean read GetHasOperationDocument;
    property ViewRatio: Double read GetViewRatio;
    property ThumbnailRatio: Double read GetThumbnailRatio;
    property CanNext: Boolean read GetCanNext;
    property CanPrevious: Boolean read GetCanPrevious;
    property CanLast: Boolean read GetCanLast;
    property CanFirst: Boolean read GetCanFirst;
    property PageIndex: Integer read GetPageIndex write SetPageIndex;
    property PageCount: Integer read GetPageCount;

    procedure LastPage();
    procedure FirstPage();

    function GetFileNames: TStringList;

    procedure Select(Index: Integer);
    property SelectIndex : Integer read FSelectIndex;
  end;

var
  model: TViewerModel;

implementation




{ TViewerModel }
function TViewerModel.GetFileNames: TStringList;
var
  i: Integer;
  fileList: TStringList;
begin
  fileList := TStringList.Create;
  try
    for i := 0 to High(FFilesList) do
      fileList.Add(FFilesList[i].Filename);
    Result := fileList;
  except
    fileList.Free;
    raise;
  end;
end;


procedure TViewerModel.LastPage();
begin
  PageIndex := PageCount - 1;
end;

procedure TViewerModel.FirstPage();
begin
  PageIndex := 0;
end;

procedure TViewerModel.SetPageIndex(value: Integer);
begin
  if not Assigned(FOperationPdfDocument) then
    Exit;

  if value < 0 then
    value := 0;

  if value > PageCount - 1 then
    value := PageCount - 1;

  FOperationPdfDocument.PageIndex := value;
end;

function TViewerModel.GetPageCount: Integer;
begin
  if not Assigned(FOperationPdfDocument) then
    Result := 0
  else
    Result := FOperationPdfDocument.PageCount;
end;

function TViewerModel.GetPageIndex: Integer;
begin
  if not Assigned(FOperationPdfDocument) then
    Result := 0
  else
    Result := FOperationPdfDocument.PageIndex;
end;

procedure TViewerModel.Previous;
begin
  if Assigned(FOperationPdfDocument) then
    FOperationPdfDocument.PageIndex := FOperationPdfDocument.PageIndex - 1;
end;

procedure TViewerModel.Next;
begin
  if Assigned(FOperationPdfDocument) then
    FOperationPdfDocument.PageIndex := FOperationPdfDocument.PageIndex + 1;
end;

function TViewerModel.GetHasOperationDocument: Boolean;
begin
  Result := Assigned(FOperationPdfDocument);
end;

function TViewerModel.GetHasViewDocument: Boolean;
begin
  Result := Assigned(FViewPdfDocument);
end;

constructor TViewerModel.Create;
begin
  inherited Create;
  FSelectIndex := -1;
end;

destructor TViewerModel.Destroy;
begin
  if Assigned(FOperationPdfDocument) and (FOperationPdfDocument <> FViewPdfDocument) then
    FOperationPdfDocument.Free;

  if Assigned(FViewPdfDocument) then
    FViewPdfDocument.Free;

  inherited Destroy;
end;
function TViewerModel.Open(const Filename: string): Boolean;
var
  pdfDocument: TPdfImageCreator;
  newFileParam: TFilesParam;
begin
  try
    pdfDocument := TPdfImageCreator.Create(Filename);
    FOperationPdfDocument := pdfDocument;

    // Add pdfDocument to FPdfImageCreatorList
    SetLength(FPdfImageCreatorList, Length(FPdfImageCreatorList) + 1);
    FPdfImageCreatorList[High(FPdfImageCreatorList)] := pdfDocument;

    // Add filename to FFilesList
    newFileParam.Filename := Filename;
    newFileParam.Index := High(FPdfImageCreatorList);
    SetLength(FFilesList, Length(FFilesList) + 1);
    FFilesList[High(FFilesList)] := newFileParam;
    FSelectIndex := newFileParam.Index;

    Result := True;
  except
    Result := False;
  end;
end;


procedure TViewerModel.View;
begin
  FViewPdfDocument := FOperationPdfDocument;
end;

function TViewerModel.GetViewBitmap(Width, Height: Integer): TBitmap;
begin
  if Assigned(FViewPdfDocument) then
    Result := FViewPdfDocument.GetBitmap(Width, Height)
  else
    Result := nil;
end;

function TViewerModel.GetThumbnailBitmap(Width, Height: Integer): TBitmap;
begin
  if Assigned(FOperationPdfDocument) then
    Result := FOperationPdfDocument.GetBitmap(Width, Height)
  else
    Result := nil;
end;

function TViewerModel.GetViewRatio: Double;
begin
  if Assigned(FViewPdfDocument) then
    Result := FViewPdfDocument.Ratio
  else
    Result := 1;
end;

function TViewerModel.GetThumbnailRatio: Double;
begin
  if Assigned(FOperationPdfDocument) then
    Result := FOperationPdfDocument.Ratio
  else
    Result := 1;
end;

function TViewerModel.GetCanNext: Boolean;
begin
  Result := Assigned(FOperationPdfDocument) and (FOperationPdfDocument.PageIndex < FOperationPdfDocument.PageCount - 1);
end;

function TViewerModel.GetCanPrevious: Boolean;
begin
  Result := Assigned(FOperationPdfDocument) and (FOperationPdfDocument.PageIndex > 0);
end;

function TViewerModel.GetCanLast: Boolean;
begin
  Result := Assigned(FOperationPdfDocument) and (PageIndex <> PageCount - 1);
end;

function TViewerModel.GetCanFirst: Boolean;
begin
  Result := Assigned(FOperationPdfDocument) and (PageIndex <> 0);
end;



procedure TViewerModel.Select(Index: Integer);
begin
  FOperationPdfDocument := FPdfImageCreatorList[index];
  FSelectIndex := index;
end;

end.

