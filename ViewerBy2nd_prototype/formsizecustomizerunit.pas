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
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SetIsFullScreen(AValue: Boolean);
    procedure SetScreenIndex(AValue: Integer);
    procedure SetTitleVisible(AValue: Boolean);
    function GetTitleVisible() : Boolean;
    function GetCanTitleVisible() : Boolean;
    function GetCanTitleInVisible() : Boolean;
    procedure DoSizer();
  public

    procedure RegistForm(AForm: TForm);
    property IsFullScreen: Boolean read FIsFullScreen write SetIsFullScreen;
    property ScreenIndex: Integer read FScreenIndex write SetScreenIndex;
    property WindowTop : Integer read FOriginalTop;
    property WindowLeft: Integer read FOriginalLeft;
    property WindowWidth: Integer read FOriginalWidth;
    property WindowHeight : Integer read FOriginalHeight;
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
  FOriginalTop:= Top;
  FOriginalLeft:= Left;
  FOriginalWidth:= Width;
  FOriginalHeight:= Height;
  DoSizer();
end;

procedure TFormSizeCustomizer.BackupOriginal();
begin
  If IsFullScreen then Exit;

  FOriginalTop := FRegisteredForm.Top;
  FOriginalLeft := FRegisteredForm.Left;
  FOriginalWidth := FRegisteredForm.Width;
  FOriginalHeight := FRegisteredForm.Height;
end;

procedure TFormSizeCustomizer.RegistForm(AForm: TForm);
begin
  FRegisteredForm := AForm;
  AForm.OnResize:=@FormResize;
  AForm.OnClose:=@FormClose;
  DoSizer();

end;
procedure TFormSizeCustomizer.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  BackupOriginal();   //todo : operation form を閉じたときにはViewer form の close イベント発生しない、
end;

procedure TFormSizeCustomizer.FormResize(Sender: TObject);
begin
  BackupOriginal();
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

