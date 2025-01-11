unit RotateImageCreatorUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ImageCreatorUnit,Graphics, Math ,
  lcltype ;
type
  TRotateImageCreator = class(TInterfacedObject, IImageCreator)
  private
    FImageCreator: IImageCreator;
    FAngle: Integer;
  public
    constructor Create(AImageCreator: IImageCreator);
    procedure Rotate(Angle: Integer);
    function GetBitmap(Width, Height: Integer): TBitmap;
    function GetRatio() : Double;
  end;

implementation




procedure Rotate90(Bitmap: TBitmap);
var
  x, y: Integer;
  TempBitmap: TBitmap;
begin
  // Create a temporary bitmap to store the rotated image
  TempBitmap := TBitmap.Create;
  try
    // Set the size of the temporary bitmap to the rotated dimensions
    TempBitmap.SetSize(Bitmap.Height, Bitmap.Width);
    TempBitmap.PixelFormat := Bitmap.PixelFormat;

    // Copy pixels from the original bitmap to the temporary bitmap, rotating them
    for y := 0 to Bitmap.Height - 1 do
      for x := 0 to Bitmap.Width - 1 do
        TempBitmap.Canvas.Pixels[Bitmap.Height - 1 - y, x] := Bitmap.Canvas.Pixels[x, y];

    // Now overwrite the original bitmap with the rotated image
    Bitmap.Assign(TempBitmap);
  finally
    TempBitmap.Free;
  end;
end;





procedure Rotate180(Bitmap: TBitmap);
var
  x, y: Integer;
  TempBitmap: TBitmap;
begin
  // Create a temporary bitmap to store the rotated image
  TempBitmap := TBitmap.Create;
  try
    // Set the size of the temporary bitmap to the rotated dimensions
    TempBitmap.SetSize(Bitmap.Width, Bitmap.Height);
    TempBitmap.PixelFormat := Bitmap.PixelFormat;

    // Copy pixels from the original bitmap to the temporary bitmap, rotating them by 180 degrees
    for y := 0 to Bitmap.Height - 1 do
      for x := 0 to Bitmap.Width - 1 do
        TempBitmap.Canvas.Pixels[Bitmap.Width - 1 - x, Bitmap.Height - 1 - y] := Bitmap.Canvas.Pixels[x, y];

    // Now overwrite the original bitmap with the rotated image
    Bitmap.Assign(TempBitmap);
  finally
    TempBitmap.Free;
  end;
end;

procedure Rotate270(Bitmap: TBitmap);
var
  x, y: Integer;
  TempBitmap: TBitmap;
begin
  // Create a temporary bitmap to store the rotated image
  TempBitmap := TBitmap.Create;
  try
    // Set the size of the temporary bitmap to the rotated dimensions
    TempBitmap.SetSize(Bitmap.Height, Bitmap.Width);
    TempBitmap.PixelFormat := Bitmap.PixelFormat;

    // Copy pixels from the original bitmap to the temporary bitmap, rotating them by 270 degrees (or -90)
    for y := 0 to Bitmap.Height - 1 do
      for x := 0 to Bitmap.Width - 1 do
        TempBitmap.Canvas.Pixels[y, Bitmap.Width - 1 - x] := Bitmap.Canvas.Pixels[x, y];

    // Now overwrite the original bitmap with the rotated image
    Bitmap.Assign(TempBitmap);
  finally
    TempBitmap.Free;
  end;
end;



constructor TRotateImageCreator.Create(AImageCreator: IImageCreator);
begin
  inherited Create;
  if AImageCreator = nil then
    raise Exception.Create('ImageCreator cannot be nil');
  FImageCreator := AImageCreator;
  FAngle := 0; // 初期角度は 0 度
end;

procedure TRotateImageCreator.Rotate(Angle: Integer);
begin
  FAngle := Angle;
end;

function TRotateImageCreator.GetBitmap(Width, Height: Integer): TBitmap;
begin
  Result := FImageCreator.GetBitmap(Width, Height);
  case FAngle of

    90:
      Rotate90(Result);
    180:
      Rotate180(Result);
    270:
      Rotate270(Result);
      end;
end;



function TRotateImageCreator.GetRatio(): double;
begin
  if (FAngle = 0) or (FAngle = 180)  then
  begin
    Result := FImageCreator.GetRatio();
  end
  else
  begin
    Result := 1/ FImageCreator.GetRatio();
  end;

end;

end.

