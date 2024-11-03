unit ViewerModel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, PdfImageCreator;


type

  TFilesParam = record
    Filename: string;
//    Selected: Boolean;
    Index: Integer;
  end;

  TRepogitory = class
  private
    FFilesList: array of TFilesParam;
    FPdfImageCreatorList : array of TPdfImageCreator;
    FSelectIndex : Integer;
    FViewPdfDocument: TPdfImageCreator;
    FOperationPdfDocument: TPdfImageCreator;
  public
    procedure AddFile(filename: String);
    property OperationFile : TPdfImageCreator read FOperationPdfDocument write FOperationPdfDocument;
    property ViewFile : TPdfImageCreator read FViewPdfDocument write FViewPdfDocument;
    function GetFileNames: TStringList;
    procedure Select(Index: Integer);
    property SelectIndex : Integer read FSelectIndex;
    constructor Create;
  end;

  TViewerModel = class
  private
    FRepogitory : TRepogitory;

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
    property Repogitory : TRepogitory read FRepogitory;

    procedure LastPage();
    procedure FirstPage();




  end;

var
  model: TViewerModel;

implementation

{ TRepogitory }
constructor TRepogitory.Create();
begin
  inherited Create;
  FSelectIndex := -1;
end;

procedure TRepogitory.Select(Index: Integer);
begin
  FOperationPdfDocument := FPdfImageCreatorList[index];
  FSelectIndex := index;
end;

procedure TRepogitory.AddFile(filename :String);
var
    newFileParam: TFilesParam;
  pdfDocument : TPdfImageCreator;
begin

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
end;

{ TViewerModel }
function TRepogitory.GetFileNames: TStringList;
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

  if not Assigned(Repogitory.OperationFile) then
    Exit;

  if value < 0 then
    value := 0;

  if value > PageCount - 1 then
    value := PageCount - 1;

  Repogitory.OperationFile.PageIndex := value;
end;

function TViewerModel.GetPageCount: Integer;
begin
  if not Assigned(Repogitory.OperationFile) then
    Result := 0
  else
    Result := Repogitory.OperationFile.PageCount;
end;

function TViewerModel.GetPageIndex: Integer;
begin
  if not Assigned(Repogitory.OperationFile) then
    Result := 0
  else
    Result := Repogitory.OperationFile.PageIndex;
end;

procedure TViewerModel.Previous;
begin
  if Assigned(Repogitory.OperationFile) then
    Repogitory.OperationFile.PageIndex := Repogitory.OperationFile.PageIndex - 1;
end;

procedure TViewerModel.Next;
begin
  if Assigned(Repogitory.OperationFile) then
    Repogitory.OperationFile.PageIndex := Repogitory.OperationFile.PageIndex + 1;
end;

function TViewerModel.GetHasOperationDocument: Boolean;
begin
  Result := Assigned(Repogitory.OperationFile);
end;

function TViewerModel.GetHasViewDocument: Boolean;
begin
  Result := Assigned(Repogitory.ViewFile);
end;

constructor TViewerModel.Create;
begin
  inherited Create;
  FRepogitory := TRepogitory.Create;

end;

destructor TViewerModel.Destroy;
begin
  Repogitory.Destroy;

  inherited Destroy;
end;
function TViewerModel.Open(const Filename: string): Boolean;


begin
  try
    Repogitory.AddFile(FileName);

    Result := True;
  except
    Result := False;
  end;
end;


procedure TViewerModel.View;
begin
  Repogitory.ViewFile := Repogitory.OperationFile;
end;

function TViewerModel.GetViewBitmap(Width, Height: Integer): TBitmap;
begin
  if Assigned(Repogitory.ViewFile) then
    Result := Repogitory.ViewFile.GetBitmap(Width, Height)
  else
    Result := nil;
end;

function TViewerModel.GetThumbnailBitmap(Width, Height: Integer): TBitmap;
begin
  if Assigned(Repogitory.OperationFile) then
    Result := Repogitory.OperationFile.GetBitmap(Width, Height)
  else
    Result := nil;
end;

function TViewerModel.GetViewRatio: Double;
begin
  if Assigned(Repogitory.ViewFile) then
    Result := Repogitory.ViewFile.Ratio
  else
    Result := 1;
end;

function TViewerModel.GetThumbnailRatio: Double;
begin
  if Assigned(Repogitory.OperationFile) then
    Result := Repogitory.OperationFile.Ratio
  else
    Result := 1;
end;

function TViewerModel.GetCanNext: Boolean;
begin
  Result := Assigned(Repogitory.OperationFile) and (Repogitory.OperationFile.PageIndex < Repogitory.OperationFile.PageCount - 1);
end;

function TViewerModel.GetCanPrevious: Boolean;
begin
  Result := Assigned(Repogitory.OperationFile) and (Repogitory.OperationFile.PageIndex > 0);
end;

function TViewerModel.GetCanLast: Boolean;
begin
  Result := Assigned(Repogitory.OperationFile) and (PageIndex <> PageCount - 1);
end;

function TViewerModel.GetCanFirst: Boolean;
begin
  Result := Assigned(Repogitory.OperationFile) and (PageIndex <> 0);
end;





end.

