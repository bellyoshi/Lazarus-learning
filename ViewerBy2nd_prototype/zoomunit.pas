unit ZoomUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ImageCreatorUnit, Graphics, Math, ZoomCacheUnit;

const
  MAX_ZOOM_RATE = 5.0;
  DEFAULT_ZOOM_RATE = 1.0;
  ZoomLevels: array[0..5] of Double = (DEFAULT_ZOOM_RATE, 1.25, 1.5, 2.0, 3.0, MAX_ZOOM_RATE);
type
  TZoom = class
  private
    FImageCreator : IImageCreator;
    MouseX: Integer;
    MouseY: Integer;
    CenterX: Double;  // Changed to Double for relative position (0.0 to 1.0)
    CenterY: Double;  // Changed to Double for relative position (0.0 to 1.0)
    FRate: Double;
    //FZoomCache : TZoomCache;
    procedure SetRate(Value: Double);
    function GetNextZoomIn(): Double;
    function GetNextZoomOut(): Double;
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
    procedure CenterClear();
  end;

implementation
procedure TZoom.CenterClear();
begin
  CenterX := 0.5;  // Center of the image
  CenterY := 0.5;  // Center of the image
  //todo:Centerをクラスにする。
  //todo:Rotateメソッドをつくる。 回転しても同じ位置を表示するようにする。
end;

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
  //FZoomCache := TZoomCache.Create(ImageCreator);
  FImageCreator := ImageCreator;
  FRate := DEFAULT_ZOOM_RATE; // 初期倍率
  CenterClear();  // Initialize center position
end;

procedure TZoom.SetRate(Value: Double);
begin
  if Value < DEFAULT_ZOOM_RATE then
    FRate := DEFAULT_ZOOM_RATE
  else if Value > MAX_ZOOM_RATE then
    FRate := MAX_ZOOM_RATE
  else
    FRate := Value;

end;

function TZoom.GetNextZoomIn(): Double;
var
  I: Integer;
begin
  Result := FRate;
  for I := Low(ZoomLevels) to High(ZoomLevels) do
  begin
    if ZoomLevels[I] > FRate then
    begin
      Result := ZoomLevels[I];
      Exit;
    end;
  end;
end;

function TZoom.GetNextZoomOut(): Double;
var
  I: Integer;
begin
  Result := FRate;
  for I := Low(ZoomLevels) to High(ZoomLevels) do
  begin
    if ZoomLevels[I] < FRate then
      Result := ZoomLevels[I]
    else
      Exit;
  end;
end;

procedure TZoom.ZoomIn();
begin
  Rate := GetNextZoomIn();
end;

procedure TZoom.ZoomOut();
begin
  Rate := GetNextZoomOut();
end;

procedure TZoom.MouseDown(X,Y:Integer);
begin
  MouseX := X;
  MouseY := Y;
end;

procedure TZoom.MouseMove(X,Y:Integer);
var
  deltaX, deltaY: Integer;
  imageWidth, imageHeight: Integer;
begin
  deltaX := MouseX - X;
  deltaY := MouseY - Y;

  // Get current image dimensions
  imageWidth := Round(FImageCreator.GetRatio() * 1000 * FRate);  // Approximate width
  imageHeight := Round(1000 * FRate);  // Approximate height

  // Convert pixel movement to relative movement
  if imageWidth > 0 then
    CenterX := CenterX + (deltaX / imageWidth);
  if imageHeight > 0 then
    CenterY := CenterY + (deltaY / imageHeight);

  // Clamp values between 0.0 and 1.0
  CenterX := Math.Max(0.0, Math.Min(1.0, CenterX));
  CenterY := Math.Max(0.0, Math.Min(1.0, CenterY));

  MouseX := X;
  MouseY := Y;
end;

function TZoom.CreateRect(dispWidth, dispHeight: Integer):TRect;
var
  X: Integer;
  Y: Integer;
  imageWidth, imageHeight: Integer;
begin
  // Get current image dimensions
  imageWidth := Round(FImageCreator.GetRatio() * 1000 * FRate);  // Approximate width
  imageHeight := Round(1000 * FRate);  // Approximate height

  // Convert relative position to absolute position
  X := Round(CenterX * imageWidth - dispWidth / 2);
  Y := Round(CenterY * imageHeight - dispHeight / 2);

  // Ensure the rect stays within image bounds
  X := Math.Max(0, Math.Min(X, imageWidth - dispWidth));
  Y := Math.Max(0, Math.Min(Y, imageHeight - dispHeight));

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
  //SourceImage := FZoomCache.GetBitmap(ZoomedWidth, ZoomedHeight);
  SourceImage := FImageCreator.GetBitmap(ZoomedWidth, ZoomedHeight);


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

