unit FormSizeCustomizerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,  ExtCtrls;

type
  TFormSizeCustomizer = class(TObject)
  private
    FRegisteredForm: TForm;
    FIsFullScreen: Boolean;
    FScreenIndex: Integer;
    FOriginalTop: Integer;
    FOriginalLeft: Integer;
    FOriginalWidth: Integer;
    FOriginalHeight: Integer;
    procedure SetIsFullScreen(AValue: Boolean);
    procedure SetScreenIndex(AValue: Integer);
    procedure DoSizer();
  public
    procedure RegistForm(AForm: TForm);
    property IsFullScreen: Boolean read FIsFullScreen write SetIsFullScreen;
    property ScreenIndex: Integer read FScreenIndex write SetScreenIndex;
  end;

var
   FormSizeCustomizer : TFormSizeCustomizer;


implementation

{ TFormSizeCustomizer }

procedure TFormSizeCustomizer.RegistForm(AForm: TForm);
begin
  FRegisteredForm := AForm;
  // 初期値として、元のサイズと位置を保存
  FOriginalTop := AForm.Top;
  FOriginalLeft := AForm.Left;
  FOriginalWidth := AForm.Width;
  FOriginalHeight := AForm.Height;
  FIsFullScreen := False;
  FScreenIndex := 0;  // デフォルトでプライマリモニタ
end;

procedure TFormSizeCustomizer.DoSizer()     ;
begin

    if FRegisteredForm = nil then Exit;



    if FIsFullScreen then
    begin
      // 現在の位置とサイズをバックアップ済みのため直接フルスクリーン化
      if (FScreenIndex >= 0) and (FScreenIndex < Screen.MonitorCount) then
      begin
        FRegisteredForm.Top := Screen.Monitors[FScreenIndex].Top;
        FRegisteredForm.Left := Screen.Monitors[FScreenIndex].Left;
        FRegisteredForm.Width := Screen.Monitors[FScreenIndex].Width;
        FRegisteredForm.Height := Screen.Monitors[FScreenIndex].Height;
        FRegisteredForm.BorderStyle := bsNone;
      end;
    end
    else
    begin
      // フルスクリーンを解除し、元の位置とサイズに戻す
      FRegisteredForm.Top := FOriginalTop;
      FRegisteredForm.Left := FOriginalLeft;
      FRegisteredForm.Width := FOriginalWidth;
      FRegisteredForm.Height := FOriginalHeight;
      FRegisteredForm.BorderStyle := bsSizeable;
    end;
end;

procedure TFormSizeCustomizer.SetIsFullScreen(AValue: Boolean);
begin
  if FIsFullScreen = AValue then Exit;
      FIsFullScreen := AValue;
  DoSizer();
end;


procedure TFormSizeCustomizer.SetScreenIndex(AValue: Integer);
begin
  if (AValue >= 0) and (AValue < Screen.MonitorCount) then
    FScreenIndex := AValue
  else
    raise Exception.Create('Invalid ScreenIndex. Monitor does not exist.');
  DoSizer();
end;

end.

