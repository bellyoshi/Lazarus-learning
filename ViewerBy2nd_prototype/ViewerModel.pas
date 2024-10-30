unit ViewerModel;

{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils, Graphics, PdfImageCreator;

type
  //todo: rename to model
  TViewerModel = class
  private
    FViewPdfDocument: TPdfImageCreator;
    FOperationPdfDocument: TPdfImageCreator;
    function GetViewRatio: Double;  // ViewRatio getter
    function GetThumbnailRatio: Double;
    function GetHasViewDocument: Boolean;
    function GetHasOperationDocument: Boolean;
    function GetCanNext: Boolean;
    function GetCanPrevious: Boolean; // CanPrevious getter
    function GetPageIndex: Integer  ;

    function GetPageCount: Integer  ;
  public
    procedure Next;
    procedure SetPageIndex(value : Integer);

    procedure Previous;
    destructor Destroy; override;
    function Open(const Filename: string): Boolean;
    procedure View;
    function GetViewBitmap(Width, Height: Integer): TBitmap;
    function GetThumbnailBitmap(Width, Height: Integer): TBitmap;
    property HasViewDocument: Boolean read GetHasViewDocument;
    property HasOperationDocument : Boolean read GetHasOperationDocument;
    property ViewRatio: Double read GetViewRatio;  // Expose ViewRatio property
    property ThumbnailRatio: Double read GetThumbnailRatio;
    property CanNext: Boolean read GetCanNext;
    property CanPrevious: Boolean read GetCanPrevious; // Expose CanPrevious property
    property PageIndex: Integer read GetPageIndex write SetPageIndex;
    property PageCount: Integer read GetPageCount;

  end;

var
  model : TViewerModel;

implementation

{ TViewerModel }

procedure TViewerModel.SetPageIndex(value :  Integer);
begin
  if value < 0 then
  begin
    value := 0;
  end;
  if value > PageCount - 1 then
  begin
    value := PageCount - 1;
  end;

  FOperationPdfDocument.PageIndex := value;
end;

function TViewerModel.GetPageCount:Integer;
begin
     Result := FOperationPdfDocument.PageCount;
end;

function TViewerModel.GetPageIndex:Integer;
begin
     Result := FOperationPdfDocument.PageIndex;
end;

procedure TViewerModel.Previous;
begin
  FOperationPdfDocument.PageIndex :=   FOperationPdfDocument.PageIndex -1;

end;

procedure TViewerModel.Next;
begin
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

destructor TViewerModel.Destroy;
begin
  if Assigned(FOperationPdfDocument) then
    if FOperationPdfDocument <> FViewPdfDocument then
      FOperationPdfDocument.Free;

  if Assigned(FViewPdfDocument) then
    FViewPdfDocument.Free;

  inherited Destroy;
end;

function TViewerModel.Open(const Filename: string): Boolean;
begin
  try
//todo: Free

    // Create the PDF using the TPdfImageCreator class
    FOperationPdfDocument := TPdfImageCreator.Create(Filename);
    Result := True;
  except
    Result := False;
  end;
end;

procedure TViewerModel.View;
begin

  //todo: Free
  // Copy operationPdfDocument to viewPdfDocument
  FViewPdfDocument := FOperationPdfDocument; // Shallow copy for simplicity
end;

function TViewerModel.GetViewBitmap(Width, Height: Integer): TBitmap;
begin
  Result := nil;
  if Assigned(FViewPdfDocument) then
  begin
    Result := FViewPdfDocument.GetBitmap(Width, Height);
  end;
end;

function TViewerModel.GetThumbnailBitmap(Width, Height: Integer): TBitmap;
begin
  Result := nil;
  if Assigned(FOperationPdfDocument) then
  begin
    Result := FOperationPdfDocument.GetBitmap(Width, Height);
  end;
end;

function TViewerModel.GetViewRatio: Double;
begin
  Result := 1;
  if Assigned(FViewPdfDocument) then
  begin
    Result := FViewPdfDocument.Ratio;
  end;
end;

function TViewerModel.GetThumbnailRatio: Double;
begin
  Result := 1;
  if Assigned(FOperationPdfDocument) then
  begin
    Result := FOperationPdfDocument.Ratio;
  end;
end;

function TViewerModel.GetCanNext: Boolean;
begin
  if not Assigned(FOperationPdfDocument) then
  begin
    Result := False;
    Exit;
  end;
  Result := FOperationPdfDocument.PageIndex < FOperationPdfDocument.PageCount - 1;
end;

function TViewerModel.GetCanPrevious: Boolean;
begin
  if not Assigned(FOperationPdfDocument) then
  begin
    Result := False;
    Exit;
  end;
  Result := FOperationPdfDocument.PageIndex > 0;
end;

end.

