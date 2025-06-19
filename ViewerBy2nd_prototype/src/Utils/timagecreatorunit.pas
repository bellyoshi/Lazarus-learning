unit TImageCreatorUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics,ImageCreatorUnit, FPImage, FPReadJPEG, FPReadPNG, FPReadBMP, ReSizeBilinearUnit, ViewerBy2ndFileTypes;

type
  TImageCreator = class(TInterfacedObject, IDocmentImageCreator)
  private
    FBitmap: TBitmap;
    procedure LoadFromFile(const AFileName: string);
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    function GetBitmap(Width, Height: Integer): TBitmap;
    function GetRatio(): Double;

    function GetPageIndex: Integer ;
    procedure SetPageIndex(AValue : Integer);
    function GetPageCount : Integer ;
  end;

implementation

constructor TImageCreator.Create(const AFileName : String);
begin
  inherited Create;
  FBitmap := TBitmap.Create;
  LoadFromFile(AFileName);
end;

destructor TImageCreator.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

function TImageCreator.GetBitmap(Width, Height: Integer): TBitmap;
begin
  if (FBitmap.Width = 0) or (FBitmap.Height = 0) then
    raise Exception.Create('No image loaded.');



  Result := ReSizeBilinear(FBitmap, Width, Height);

end;

function TImageCreator.GetRatio(): Double;
begin
  if (FBitmap.Width = 0) or (FBitmap.Height = 0) then
    raise Exception.Create('No image loaded.');
  Result := FBitmap.Width / FBitmap.Height;
end;

procedure TImageCreator.LoadFromFile(const AFileName: string);
var
  Image: TFPCustomImage;
  Reader: TFPCustomImageReader;
begin
  if not IsImageFile(AFileName) then
    raise Exception.Create('Not an image file');

  Image := TFPMemoryImage.Create(0, 0);
  try
    Reader := GetTFPReader(AFileName);
    try
      Image.LoadFromFile(AFileName, Reader);
      FBitmap.Assign(Image);
    finally
      Reader.Free;
    end;
  finally
    Image.Free;
  end;
end;

function TImageCreator.GetPageIndex: Integer ;
begin
  Result := 0;
end;

procedure TImageCreator.SetPageIndex(AValue : Integer);
begin
  //No opration
end;

function TImageCreator.GetPageCount : Integer ;
begin
  Result := 1;
end;

end.

