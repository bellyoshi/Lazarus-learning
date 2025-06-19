// File: Background.pas
unit Background;

{$mode ObjFPC}{$H+}

interface

uses
  Graphics, Classes, SysUtils;

type
  TBackground = class
  private
    FColor: TColor;
  public
    constructor Create(AColor: TColor = clBlack);
    function GetBitmap(Width, Height: Integer): TBitmap;
    property Color: TColor read FColor write FColor;
  end;

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

end.
