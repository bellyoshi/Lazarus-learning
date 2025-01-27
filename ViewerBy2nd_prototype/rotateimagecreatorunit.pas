unit RotateImageCreatorUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ImageCreatorUnit,Graphics;

type
  TRotateImageCreator = class(TInterfacedObject, IImageCreator)
  private
    FImageCreator: IImageCreator;
    FAngle: Integer;
    FRatio: Double;
  public
    constructor Create(AImageCreator: IImageCreator);
    procedure Rotate(Angle: Integer);
    function GetBitmap(Width, Height: Integer): TBitmap;
    function GetRatio() : Double;
  end;

implementation

uses ReSizeBilinearUnit  ,lcltype ;





constructor TRotateImageCreator.Create(AImageCreator: IImageCreator);
begin
  inherited Create;
  if AImageCreator = nil then
    raise Exception.Create('ImageCreator cannot be nil');
  FImageCreator := AImageCreator;
  FAngle := 0; // 初期角度は 0 度
  FRatio := 1;
end;

procedure TRotateImageCreator.Rotate(Angle: Integer);
begin
  FAngle := Angle;
end;

function TRotateImageCreator.GetBitmap(Width, Height: Integer): TBitmap;
begin

    case FAngle of
    0:
      Result := FImageCreator.GetBitmap(Width, Height);
    90:
      begin
      Result := Rotation90Right(FImageCreator.GetBitmap(Height, Width));

      end;
    180:
      begin
            Result := Rotation180(FImageCreator.GetBitmap(Width, Height));
      end;
    270:
      begin
            Result := Rotation90Left(FImageCreator.GetBitmap(Height, Width));


      end;

    end;
end;



function TRotateImageCreator.GetRatio(): double;
begin
  Result := FImageCreator.GetRatio();
  case FAngle of
    90:
      Result := 1 / FImageCreator.GetRatio();
    270:
      Result := 1 / FImageCreator.GetRatio();

      end;

end;

end.

