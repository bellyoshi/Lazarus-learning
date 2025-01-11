unit ZoomUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ImageCreatorUnit, Graphics, Math, ZoomCacheUnit;

const
  ZoomLevels: array[0..7] of Double = (1.0, 1.25, 1.5, 2.0, 3.0, 5.0, 7.5, 10.0);
type
  TZoom = class
  private
    FImageCreator : IImageCreator;
    MouseX: Integer;
    MouseY: Integer;
    CenterX: Integer;
    CenterY: Integer;
    FRate: Double;
    FZoomCache : TZoomCache;
    procedure SetRate(Value: Double);
    function GetNextZoom(ZoomIn: Boolean): Double;
    function CreateRect(dispWidth, dispHeight: Integer):TRect;
  public
    constructor Create(ImageCreator: IImageCreator);
    property Rate: Double read FRate write SetRate;
    procedure ZoomIn();
    procedure ZoomOut();
    procedure fitWindow(WindowWidth, WindowHeight: Integer);
    function GetBitmap(WindowWidth, WindowHeight: Integer): TBitmap;
    procedure MouseDown(X,Y:Integer);
    procedure MouseMove(X,Y:Integer);
  end;

implementation
function RoundToStep(Value: Double; Step: Integer): Integer;
begin
  Result := Round(Value / Step) * Step;
end;

function RoundToStep(Value: Double) : Integer;
const
  PIXEL_STEP = 8;  // widthを8の倍数にしなければLinux環境ですじがでる。
begin
  Result := RoundToStep(Value, PIXEL_STEP);
end;


constructor TZoom.Create(ImageCreator: IImageCreator);
begin
  inherited Create;
  FZoomCache := TZoomCache.Create(ImageCreator);
  FImageCreator := ImageCreator;
  FRate := 1.0; // 初期倍率
  CenterX := -1;
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

procedure TZoom.MouseDown(X,Y:Integer);
begin
  MouseX := X;
  MouseY := Y;
end;

procedure TZoom.MouseMove(X,Y:Integer);
var
  deltaX, deltaY: Integer;
begin
  deltaX :=  MouseX - X;
  deltaY := MouseY - Y;

  CenterX := CenterX + deltaX;
  CenterY := CenterY + deltaY;

  MouseX := X;
  MouseY := Y;


end;

function TZoom.CreateRect(dispWidth, dispHeight: Integer):TRect;
var
  X: Integer;
  Y: Integer;
begin
  X := Math.Max(0, CenterX - dispWidth div 2);
  Y := Math.Max(0, CenterY - dispHeight div 2);

  Result := TRect.Create(X, Y, X + dispWidth, Y + dispHeight);
end;

procedure TZoom.fitWindow(WindowWidth, WindowHeight: Integer);
var
  normalHeight, normalWidth : Integer;
  formRatio: Double;
  Ratio : Double;
begin
  formRatio := WindowWidth / WindowHeight;
  Ratio := FImageCreator.GetRatio();

  if formRatio > Ratio then
  begin
    // 縦が基準

    normalHeight := WindowHeight;
        normalWidth := RoundToStep(normalHeight * Ratio);
  end
  else
  begin
    // 横が基準
    normalWidth := RoundToStep(WindowWidth);
    normalHeight := Round(normalWidth / Ratio);
  end;


  FRate:= WindowWidth / normalWidth;
end;

function TZoom.GetBitmap(WindowWidth, WindowHeight: Integer): TBitmap;
var
  normalHeight, normalWidth : Integer;
  dispHeight, dispWidth : Integer;
  formRatio: Double;
  Ratio : Double;
  ZoomedWidth, ZoomedHeight: Integer;
  sourceRect, destRect: TRect;
  SourceImage : TBitmap;
begin
  formRatio := WindowWidth / WindowHeight;
  Ratio := FImageCreator.GetRatio();

  if formRatio > Ratio then
  begin
    // 縦が基準

    normalHeight := WindowHeight;
    normalWidth := RoundToStep(normalHeight * Ratio);
  end
  else
  begin
    // 横が基準
    normalWidth := RoundToStep(WindowWidth);
    normalHeight := Round(normalWidth / Ratio);
  end;

  ZoomedWidth := Round(normalWidth * FRate);
  ZoomedHeight := Round(normalHeight * FRate);

  // ImageCreatorから拡大した画像を取得
  SourceImage := FZoomCache.GetBitmap(ZoomedWidth, ZoomedHeight);



  if CenterX = -1 Then
  begin
       CenterX := ZoomedWidth div 2;
       CenterY := ZoomedHeight div 2;
  end;

  // 切り取り範囲を指定
  dispHeight:=Min(ZoomedHeight,WindowHeight);
  dispWidth:=Min(ZoomedWidth,WindowWidth);
  sourceRect := CreateRect(dispWidth, dispHeight);
  destRect := TRect.Create(0,0, dispWidth,dispHeight);
  // 結果用の画像を作成
  Result := TBitmap.Create;
  Result.SetSize(dispWidth, dispHeight);

  // SourceImageの左上部分をResultに描画
  Result.Canvas.CopyRect(destRect, SourceImage.Canvas, sourceRect);

end;



end.

