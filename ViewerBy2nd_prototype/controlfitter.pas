unit ControlFitter;

{$mode ObjFPC}{$H+}

interface

uses
    Controls;


procedure FitImageSize(Image1: TControl ; ClientWidth, ClientHeight: Integer; Ratio: double);


implementation

function RoundToStep(Value: Integer; Step: Integer): Integer;
begin
  Result := Round(Value / Step) * Step;
end;

function RoundWidthToStep(Value: Integer) : Integer;
const
  PIXEL_STEP = 8;  // widthを8の倍数にしなければLinux環境ですじがでる。
begin
  Result := RoundToStep(Value, PIXEL_STEP);
end;

procedure FitImageSize(Image1: TControl; ClientWidth, ClientHeight: Integer; Ratio: double);
var
  NewHeight, NewWidth : Integer;
  formRatio: Double;
begin
  formRatio := ClientWidth / ClientHeight;

  if formRatio > Ratio then
  begin
    // 縦が基準
    NewHeight := ClientHeight;
    NewWidth := RoundWidthToStep(Round(NewHeight * Ratio));
  end
  else
  begin
    // 横が基準
    NewWidth := RoundWidthToStep(ClientWidth);
    NewHeight := Round(NewWidth / Ratio);
  end;


  // フォームのクライアント領域にImage1をフィットさせる
  Image1.Width := NewWidth;
  Image1.Height := NewHeight;

  // 中央に配置
  Image1.Left := (ClientWidth - NewWidth) div 2;
  Image1.Top := (ClientHeight - NewHeight) div 2;
end;
end.

