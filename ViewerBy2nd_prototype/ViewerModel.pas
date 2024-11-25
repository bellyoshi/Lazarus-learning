unit ViewerModel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, PdfImageCreator, Generics.Collections;


type

  TFilesParam = class
  public
    Filename: string;
    Selected: Boolean;
    ImageCreator : TPdfImageCreator;
  end;

  TZoom = class
  private
    FRate : Double;
  public
    property Rate : Double read FRate write FRate;
  end;

  TBackground = class
  private
    FColor: TColor;
  public
    constructor Create(AColor: TColor = clBlack);
    function GetBitmap(Width, Height: Integer): TBitmap;
    property Color : TColor read FColor write FColor;
  end;

  TRepogitory = class
  private
    FFilesList: specialize TList<TFilesParam>;

    FViewPdfDocument: TPdfImageCreator;
    FOperationPdfDocument: TPdfImageCreator;
    function GetSelected(I : Integer): Boolean;
    procedure SetSelected(I: Integer; Value : Boolean);
    procedure RecalcSingleSelect();
  public
    procedure Disselect;
    procedure SelectAll();
    procedure Delete();
    procedure AddFile(filename: String);
    property OperationFile : TPdfImageCreator read FOperationPdfDocument write FOperationPdfDocument;
    property ViewFile : TPdfImageCreator read FViewPdfDocument write FViewPdfDocument;
    property Selected [i : Longint] : Boolean Read GetSelected write SetSelected;
    function GetFileNames: TStringList;
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
  FFilesList := specialize TList<TFilesParam>.Create();
  ViewFile := nil;
end;

procedure TRepogitory.Delete();
var
  rList : specialize TList<TFilesParam> ;
  item : TFilesParam;
begin
  rList := specialize TList<TFilesParam>.Create();
  for item in FFilesList do
  begin
    if item.Selected then
    begin
      rList.Add(item);
    end;
  end;
  for item in rList do
  begin
    FFilesList.Remove(item);
    item.Free;
  end;
  rList.Free;
end;

procedure TRepogitory.SelectAll();
var
  i : Integer;
begin
  for i := 0 to FFilesList.Count - 1 do
  begin
    FFilesList.Items[i].Selected := True;
  end;
  RecalcSingleSelect();
end;

procedure TRepogitory.RecalcSingleSelect();
var
  index : Integer;
  i : Integer;
begin
  index := -1;
  for i := 0 to FFilesList.Count - 1 do
  begin
       if FFilesList.Items[i].Selected then begin
         if index = -1 then begin
            index := i;
         end else
         begin
           index := -1;
           break;
         end;
       end;
  end;
  if index = -1 then
  begin
      FOperationPdfDocument := nil;
  end else begin
      FOperationPdfDocument := FFilesList[index].ImageCreator;
  end;
end;


procedure TRepogitory.SetSelected(I: Integer; Value : Boolean);
begin
  FFilesList.Items[i].Selected := Value;
  RecalcSingleSelect();
end;
function TRepogitory.GetSelected(I: Integer): Boolean;
begin
  Result := FFilesList[i].Selected;
end;
procedure TRepogitory.DisSelect;
var
  i : LongInt;
begin
  for i := 0 to FFilesList.Count - 1 do
  begin
    FFilesList.Items[i].Selected := False;
  end;
  FViewPdfDocument := nil;
  FOperationPdfDocument:= nil;
end;

procedure TRepogitory.AddFile(filename :String);
var
  newFileParam: TFilesParam;
  pdfImageCreator : TPdfImageCreator;
  i : Integer;
begin

    pdfImageCreator := TPdfImageCreator.Create(Filename);
    FOperationPdfDocument := pdfImageCreator;

    newFileParam := TFilesParam.Create();
    newFileParam.Filename := Filename;
    newFileParam.ImageCreator := pdfImageCreator;
    FFilesList.Add(newFileParam);

    for i := 0 to FFilesList.Count - 1 do
    begin
      FFilesList.Items[i].Selected := False;
    end;
    newFileParam.Selected := True;

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
    for i := 0 to FFilesList.Count -1 do
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

