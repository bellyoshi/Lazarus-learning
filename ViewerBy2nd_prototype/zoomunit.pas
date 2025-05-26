unit ZoomUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ImageCreatorUnit, Graphics, Math;

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
    procedure SetRate(Value: Double);
    function GetNextZoomIn(): Double;
    function GetNextZoomOut(): Double;

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

  TZoomBitmapCreator = class
  private
    FZoom: TZoom;
    FWindowWidth: Integer;
    FWindowHeight: Integer;
    function ZoomedWidth() : Integer;
    function ZoomedHeight() : Integer;
    function dispHeight() : Integer;
    function dispWidth() : Integer;

    function normalHeight() : Integer;
    function normalWidth() : Integer;
    function CreateRect():TRect;
    function GetFormRatio(): Double;
    function GetImageRatio(): Double;
  public
    constructor Create(Zoom: TZoom; WindowWidth, WindowHeight: Integer);
    function GetBitmap(): TBitmap;
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
  Ratio: Double;
begin
  deltaX := MouseX - X;
  deltaY := MouseY - Y;

  // Get current image dimensions using the same reference size as GetBitmap
  Ratio := FImageCreator.GetRatio();
  imageWidth := Round(1000 * FRate);  // Reference width * zoom rate
  imageHeight := Round(imageWidth / Ratio);  // Calculate height maintaining aspect ratio

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



procedure TZoom.fitWindow(WindowWidth, WindowHeight: Integer);
var
  BitmapCreator: TZoomBitmapCreator;
  begin
  BitmapCreator := TZoomBitmapCreator.Create(Self, WindowWidth, WindowHeight);
  try
    FRate:= WindowWidth / BitmapCreator.normalWidth();
  finally
    BitmapCreator.Free;
  end;

end;

function TZoom.GetBitmap(WindowWidth, WindowHeight: Integer): TBitmap;
var
  BitmapCreator: TZoomBitmapCreator;
begin
  BitmapCreator := TZoomBitmapCreator.Create(Self, WindowWidth, WindowHeight);
  try
    Result := BitmapCreator.GetBitmap();
  finally
    BitmapCreator.Free;
  end;
end;

constructor TZoomBitmapCreator.Create(Zoom: TZoom; WindowWidth, WindowHeight: Integer);
begin
  inherited Create;
  FZoom := Zoom;
  FWindowWidth := WindowWidth;
  FWindowHeight := WindowHeight;
end;

function TZoomBitmapCreator.CreateRect():TRect;
var
  X: Integer;
  Y: Integer;
begin
  // Convert relative position to absolute position
  X := Round(FZoom.CenterX * ZoomedWidth() - dispWidth() / 2);
  Y := Round(FZoom.CenterY * ZoomedHeight() - dispHeight() / 2);

  // Ensure the rect stays within image bounds
  X := Math.Max(0, Math.Min(X, ZoomedWidth() - dispWidth()));
  Y := Math.Max(0, Math.Min(Y, ZoomedHeight() - dispHeight()));

  Result := TRect.Create(X, Y, X + dispWidth(), Y + dispHeight());
end;

function TZoomBitmapCreator.GetFormRatio(): Double;
begin
  Result := FWindowWidth / FWindowHeight;
end;

function TZoomBitmapCreator.GetImageRatio(): Double;
begin
  Result := FZoom.FImageCreator.GetRatio();
end;

function TZoomBitmapCreator.normalHeight() : Integer;
begin
  if GetFormRatio() > GetImageRatio() then
    Result := FWindowHeight
  else
    Result := Round(RoundToStep(FWindowWidth) / GetImageRatio());
end;

function TZoomBitmapCreator.normalWidth() : Integer;
begin
  if GetFormRatio() > GetImageRatio() then
    Result := RoundToStep(FWindowHeight * GetImageRatio())
  else
    Result := RoundToStep(FWindowWidth);
end;

function TZoomBitmapCreator.ZoomedWidth() : Integer;
begin
  Result := Round(normalWidth() * FZoom.FRate);
end;

function TZoomBitmapCreator.ZoomedHeight() : Integer;
begin
  Result := Round(normalHeight() * FZoom.FRate);
end;
function TZoomBitmapCreator.dispHeight() : Integer;
begin
  Result := Min(ZoomedHeight, FWindowHeight);
end;
function TZoomBitmapCreator.dispWidth() : Integer;
begin
  Result := Min(ZoomedWidth, FWindowWidth);
end;

function TZoomBitmapCreator.GetBitmap(): TBitmap;
var
  sourceRect, destRect: TRect;
  SourceImage : TBitmap;
begin

  // ImageCreatorから拡大した画像を取得
  SourceImage := FZoom.FImageCreator.GetBitmap(ZoomedWidth(), ZoomedHeight());
  // Calculate source rectangle based on relative center position
  sourceRect := CreateRect();
  destRect := TRect.Create(0, 0, dispWidth, dispHeight);
  
  // 結果用の画像を作成
  Result := TBitmap.Create;
  Result.SetSize(dispWidth, dispHeight);

  // SourceImageの指定範囲をResultに描画
  Result.Canvas.CopyRect(destRect, SourceImage.Canvas, sourceRect);
end;



end.

