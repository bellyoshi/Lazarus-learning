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
    FOriginal : TRect;


    procedure SetIsFullScreen(AValue: Boolean);
    procedure SetScreenIndex(AValue: Integer);
    procedure SetTitleVisible(AValue: Boolean);
    function GetTitleVisible() : Boolean;
    function GetCanTitleVisible() : Boolean;
    function GetCanTitleInVisible() : Boolean;
    function GetFullscreenEnabled() : Boolean;
    function GetScreenRect() : TRect;
    procedure DoSizer();
    function GetWindowSize : TRect;
    function GetCurrentWindowSize : TRect;
  public

    property CurrentWindowSize : TRect read GetCurrentWindowSize;
    procedure RegistForm(AForm: TForm);
    property IsFullScreen: Boolean read FIsFullScreen write SetIsFullScreen;
    property ScreenIndex: Integer read FScreenIndex write SetScreenIndex;
    property WindowModeSize : TRect read FOriginal;


    procedure SetOriginalSize(Top, Left , Width, Height: Integer);
    property TitleVisible : Boolean read GetTitleVisible write SetTitleVisible;
    property CanTitleVisible : Boolean read GetCanTitleVisible;
    property CanTitleInVisible : Boolean read GetCanTitleInVisible;
    procedure BackupOriginal();
  end;

var
   FormSizeCustomizer : TFormSizeCustomizer;


implementation

{ TFormSizeCustomizer }
procedure TFormSizeCustomizer.SetOriginalSize(Top, Left , Width, Height: Integer);
begin
  FOriginal.Top:=Top;
  FOriginal.Left:= Left;
  FOriginal.Width:= Width;
  FOriginal.Height:= Height;
  DoSizer();
end;

procedure TFormSizeCustomizer.BackupOriginal();
begin
  If IsFullScreen then Exit;

  FOriginal := FRegisteredForm.BoundsRect;
end;

procedure TFormSizeCustomizer.RegistForm(AForm: TForm);
begin
  FRegisteredForm := AForm;
  DoSizer();

end;

function TFormSizeCustomizer.GetScreenRect : TRect;
begin
  Result.Top := Screen.Monitors[FScreenIndex].Top;
  Result.Left := Screen.Monitors[FScreenIndex].Left;
  Result.Width := Screen.Monitors[FScreenIndex].Width;
  Result.Height := Screen.Monitors[FScreenIndex].Height;
end;

function TFormSizeCustomizer.GetWindowSize : TRect;
begin
  if GetFullscreenEnabled() then
  begin
    Result := GetScreenRect();
  end else begin
    Result := FOriginal;
  end;
end;

function TFormSizeCustomizer.GetCurrentWindowSize : TRect;
begin
  Result := FRegisteredForm.BoundsRect;
end;

function TFormSizeCustomizer.GetFullscreenEnabled() : Boolean;
begin
  Result := FIsFullScreen and (FScreenIndex >= 0) and (FScreenIndex < Screen.MonitorCount);
end;

procedure TFormSizeCustomizer.DoSizer();
begin

    if FRegisteredForm = nil then Exit;

    if GetFullscreenEnabled() then
    begin
      FRegisteredForm.BorderStyle := bsNone;
    end else begin
      FRegisteredForm.BorderStyle := bsSizeable;
    end;

    FRegisteredForm.BoundsRect := GetWindowSize();

end;

procedure TFormSizeCustomizer.SetIsFullScreen(AValue: Boolean);
begin
  if FIsFullScreen = AValue then
     Exit;
  if AValue then
     BackupOriginal();
  FIsFullScreen := AValue;
  DoSizer();
end;

procedure TFormSizeCustomizer.SetTitleVisible(AValue: Boolean);
begin
  if AValue then
  begin
    FRegisteredForm.BorderStyle := bsSizeable;
  end else begin
    FRegisteredForm.BorderStyle := bsNone;
  end;
end;

function TFormSizeCustomizer.GetTitleVisible() : Boolean;
begin
  Result := (FRegisteredForm.BorderStyle = bsSizeable);
end;

function TFormSizeCustomizer.GetCanTitleVisible() : Boolean;
begin
  Result := (Not FIsFullScreen) And (Not TitleVisible);
end;

function TFormSizeCustomizer.GetCanTitleInVisible() : Boolean;
begin
  Result := (Not FIsFullScreen) And TitleVisible;
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

