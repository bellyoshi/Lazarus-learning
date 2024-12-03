unit ZoomUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PdfImageCreator, Graphics, Math;

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
    function GetBitmap(WindowWidth, WindowHeight: Integer): TBitmap;
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

function TZoom.GetBitmap(WindowWidth, WindowHeight: Integer): TBitmap;
var
  normalHeight, normalWidth : Integer;
  dispHeight, dispWidth : Integer;
  formRatio: Double;
  Ratio : Double;
  ZoomedWidth, ZoomedHeight: Integer;
  SourceImage: TBitmap;
  Rect: TRect;
begin
  formRatio := WindowWidth / WindowHeight;
  Ratio := FImageCreator.Ratio;

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
  SourceImage := FImageCreator.GetBitmap(ZoomedWidth, ZoomedHeight);


  dispHeight:=Min(ZoomedHeight,WindowHeight);
  dispWidth:=Min(ZoomedWidth,WindowWidth);



  // 切り取り範囲を指定
  Rect := TRect.Create(0, 0, dispWidth, dispHeight);

  // 結果用の画像を作成
  Result := TBitmap.Create;
  Result.SetSize(dispWidth, dispHeight);

  // SourceImageの左上部分をResultに描画
  Result.Canvas.CopyRect(Rect, SourceImage.Canvas, Rect);

  // 解放
  SourceImage.Free;
end;



end.

