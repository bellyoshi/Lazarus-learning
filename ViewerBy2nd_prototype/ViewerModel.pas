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

type
  TBackground = class
  private
    FColor: TColor;
  public
    constructor Create(AColor: TColor = clBlack);
    function GetBitmap(Width, Height: Integer): TBitmap;
    property Color : TColor read FColor;
  end;

  TRepogitory = class
  private
    FFilesList: array of TFilesParam;
    FPdfImageCreatorList : array of TPdfImageCreator;
    FSelectIndex : Integer;
    FViewPdfDocument: TPdfImageCreator;
    FOperationPdfDocument: TPdfImageCreator;
  public
        procedure Disselect;
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
    FBackground : TBackground;
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
    function GetOperationFile : TPdfImageCreator;
    property OperationFile : TPdfImageCreator read GetOperationFile;

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
    property Background : TBackground read FBackground;


    procedure LastPage();
    procedure FirstPage();




  end;

var
  model: TViewerModel;

implementation

{TBackground}
constructor TBackground.Create(AColor: TColor = clBlack);
begin
  inherited Create;
  FColor := AColor;
end;

function TBackground.GetBitmap(Width, Height: Integer): TBitmap;
begin
  Result := TBitmap.Create;
  try
    Result.Width := Width;
    Result.Height := Height;
    Result.Canvas.Brush.Color := FColor;
    Result.Canvas.FillRect(Rect(0, 0, Width, Height));
  except
    Result.Free;
    raise;
  end;
end;

{ TRepogitory }

constructor TRepogitory.Create();
begin
  inherited Create;
  FSelectIndex := -1;
  ViewFile := nil;
end;

procedure TRepogitory.Select(Index: Integer);
begin
  FOperationPdfDocument := FPdfImageCreatorList[index];
  FSelectIndex := index;
end;

procedure TRepogitory.DisSelect;
begin
  FSelectIndex:= -1;
      FViewPdfDocument := nil;
    FOperationPdfDocument:= nil;
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
function TViewerModel.GetOperationFile : TPdfImageCreator;
begin
  Result := Repogitory.OperationFile;
end;

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

  OperationFile.PageIndex := value;
end;

function TViewerModel.GetPageCount: Integer;
begin
  if not Assigned(Repogitory.OperationFile) then
    Result := 0
  else
    Result := OperationFile.PageCount;
end;

function TViewerModel.GetPageIndex: Integer;
begin
  if not Assigned(Repogitory.OperationFile) then
    Result := 0
  else
    Result := OperationFile.PageIndex;
end;

procedure TViewerModel.Previous;
begin
  if Assigned(Repogitory.OperationFile) then
    OperationFile.PageIndex := OperationFile.PageIndex - 1;
end;

procedure TViewerModel.Next;
begin
  if Assigned(Repogitory.OperationFile) then
    OperationFile.PageIndex := OperationFile.PageIndex + 1;
end;

function TViewerModel.GetHasOperationDocument: Boolean;
begin
  Result := Assigned(OperationFile);
end;

function TViewerModel.GetHasViewDocument: Boolean;
begin
  Result := Assigned(Repogitory.ViewFile);
end;

constructor TViewerModel.Create;
begin
  inherited Create;
  FRepogitory := TRepogitory.Create;
  FBackground := TBackground.Create;

end;

destructor TViewerModel.Destroy;
begin
  FRepogitory.Destroy;
  FBackground.Destroy;

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
  Repogitory.ViewFile := OperationFile;
end;

function TViewerModel.GetViewBitmap(Width, Height: Integer): TBitmap;
begin
  if Assigned(Repogitory.ViewFile) then
    Result := Repogitory.ViewFile.GetBitmap(Width, Height)
  else
    Result := FBackground.GetBitmap(Width, Height);
end;

function TViewerModel.GetThumbnailBitmap(Width, Height: Integer): TBitmap;
begin
  if Assigned(OperationFile) then
    Result := OperationFile.GetBitmap(Width, Height)
  else
    Result := FBackground.GetBitmap(Width, Height);
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
  if Assigned(OperationFile) then
    Result := OperationFile.Ratio
  else
    Result := 1;
end;

function TViewerModel.GetCanNext: Boolean;
begin
  Result := Assigned(OperationFile) and (OperationFile.PageIndex < OperationFile.PageCount - 1);
end;

function TViewerModel.GetCanPrevious: Boolean;
begin
  Result := Assigned(OperationFile) and (OperationFile.PageIndex > 0);
end;

function TViewerModel.GetCanLast: Boolean;
begin
  Result := Assigned(OperationFile) and (PageIndex <> PageCount - 1);
end;

function TViewerModel.GetCanFirst: Boolean;
begin
  Result := Assigned(OperationFile) and (PageIndex <> 0);
end;





end.

