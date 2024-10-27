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
    FHasViewDocument: Boolean;
    function GetViewRatio: Double;  // ViewRatio getter
    function GetThumbnailRatio: Double;
  public
    destructor Destroy; override;
    function Open(const Filename: string): Boolean;
    procedure View;
    function GetViewBitmap(Width, Height: Integer): TBitmap;
    function GetThumbnailBitmap(Width, Height: Integer): TBitmap;
    property HasViewDocument: Boolean read FHasViewDocument;
    property ViewRatio: Double read GetViewRatio;  // Expose ViewRatio property
    property ThumbnailRatio: Double read GetThumbnailRatio;
  end;
var
  model : TViewerModel;

implementation
  
  { TViewerModel }

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
    if Assigned(FOperationPdfDocument) then
      if FOperationPdfDocument <> FViewPdfDocument then
         FOperationPdfDocument.Free;

    // Create the PDF using the TPdfImageCreator class
    FOperationPdfDocument := TPdfImageCreator.Create(Filename);
    Result := True;
  except
    Result := False;
  end;
end;

procedure TViewerModel.View;
begin
  if Assigned(FViewPdfDocument) then
    FViewPdfDocument.Free;

  // Copy operationPdfDocument to viewPdfDocument
  FViewPdfDocument := FOperationPdfDocument; // Shallow copy for simplicity
  FHasViewDocument := True;
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

end.

