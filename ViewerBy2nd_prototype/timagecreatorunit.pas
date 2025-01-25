unit TImageCreatorUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics,ImageCreatorUnit, FPImage, FPReadJPEG, FPReadPNG, FPReadBMP;

type
  TImageCreator = class(TInterfacedObject, IImageCreator)
  private
    FBitmap: TBitmap;
  public
    constructor Create;
    destructor Destroy; override;
    function GetBitmap(Width, Height: Integer): TBitmap;
    function GetRatio(): Double;
    procedure LoadFromFile(const FileName: string);
  end;

implementation

constructor TImageCreator.Create;
begin
  inherited Create;
  FBitmap := TBitmap.Create;
end;

destructor TImageCreator.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

function TImageCreator.GetBitmap(Width, Height: Integer): TBitmap;
var
  Ratio: Double;
  ResizedBitmap: TBitmap;
begin
  if (FBitmap.Width = 0) or (FBitmap.Height = 0) then
    raise Exception.Create('No image loaded.');

  Ratio := GetRatio();

  ResizedBitmap := TBitmap.Create;
  try
    ResizedBitmap.SetSize(Width, Height);
    ResizedBitmap.Canvas.StretchDraw(Rect(0, 0, Width, Height), FBitmap);
    Result := ResizedBitmap;
  except
    ResizedBitmap.Free;
    raise;
  end;
end;

function TImageCreator.GetRatio(): Double;
begin
  if (FBitmap.Width = 0) or (FBitmap.Height = 0) then
    raise Exception.Create('No image loaded.');
  Result := FBitmap.Width / FBitmap.Height;
end;

procedure TImageCreator.LoadFromFile(const FileName: string);
var
  Ext: string;
  Image: TFPCustomImage;
  Reader: TFPCustomImageReader;
begin
  Ext := LowerCase(ExtractFileExt(FileName));

  Image := TFPMemoryImage.Create(0, 0);
  try
    if Ext = '.jpg' then
      Reader := TFPReaderJPEG.Create
    else if Ext = '.png' then
      Reader := TFPReaderPNG.Create
    else if Ext = '.bmp' then
      Reader := TFPReaderBMP.Create
    else
      raise Exception.Create('Unsupported file format: ' + Ext);

    try
      Image.LoadFromFile(FileName, Reader);
      FBitmap.Assign(Image);
    finally
      Reader.Free;
    end;
  finally
    Image.Free;
  end;
end;

end.

