unit ZoomUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PdfImageCreator, Graphics;

type
  TZoom = class
  private
    FRate: Double;
    FImageCreator: TPdfImageCreator;
    procedure SetRate(Value: Double);
    function GetNextZoom(ZoomIn: Boolean): Double;
  public
    constructor Create(ImageCreator: TPdfImageCreator);
    property Rate: Double read FRate write SetRate;
    procedure ZoomIn();
    procedure ZoomOut();
    function GetImage(Width, Height: Integer): TBitmap;
  end;

implementation

constructor TZoom.Create(ImageCreator: TPdfImageCreator);
begin
  inherited Create;
  FImageCreator := ImageCreator;
  FRate := 1.0; // 初期倍率
end;

procedure TZoom.SetRate(Value: Double);
begin
  if Value < 1.0 then
    FRate := 1.0
  else if Value > 10.0 then
    FRate := 10.0
  else
    FRate := Value;
end;

function TZoom.GetNextZoom(ZoomIn: Boolean): Double;
const
  ZoomLevels: array[0..7] of Double = (1.0, 1.25, 1.5, 2.0, 3.0, 5.0, 7.5, 10.0);
var
  I: Integer;
begin
  Result := FRate;
  for I := Low(ZoomLevels) to High(ZoomLevels) do
  begin
    if ZoomIn then
    begin
      if ZoomLevels[I] > FRate then
      begin
        Result := ZoomLevels[I];
        Exit;
      end;
    end
    else
    begin
      if ZoomLevels[I] < FRate then
        Result := ZoomLevels[I]
      else
        Exit;
    end;
  end;
end;

procedure TZoom.ZoomIn();
begin
  Rate := GetNextZoom(True);
end;

procedure TZoom.ZoomOut();
begin
  Rate := GetNextZoom(False);
end;

function TZoom.GetImage(Width, Height: Integer): TBitmap;
var
  ZoomedWidth, ZoomedHeight: Integer;
  SourceImage: TBitmap;
  Rect: TRect;
begin
  ZoomedWidth := Round(Width * FRate);
  ZoomedHeight := Round(Height * FRate);

  // ImageCreatorから拡大した画像を取得
  SourceImage := FImageCreator.GetBitmap(ZoomedWidth, ZoomedHeight);

  // 切り取り範囲を指定
  Rect := TRect.Create(0, 0, Width, Height);

  // 結果用の画像を作成
  Result := TBitmap.Create;
  Result.SetSize(Width, Height);

  // SourceImageの左上部分をResultに描画
  Result.Canvas.CopyRect(Rect, SourceImage.Canvas, Rect);

  // 解放
  SourceImage.Free;
end;



end.

