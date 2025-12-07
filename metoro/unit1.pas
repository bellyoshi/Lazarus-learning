unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Windows, Math;

type
  TForm1 = class(TForm)
    PanelPendulum: TPanel;
    PanelControls: TPanel;
    LabelBPM: TLabel;
    LabelBPMValue: TLabel;
    TrackBarTempo: TTrackBar;
    ButtonStartStop: TButton;
    TimerMetronome: TTimer;
    TimerAnimation: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure PanelPendulumPaint(Sender: TObject);
    procedure TrackBarTempoChange(Sender: TObject);
    procedure ButtonStartStopClick(Sender: TObject);
    procedure TimerMetronomeTimer(Sender: TObject);
    procedure TimerAnimationTimer(Sender: TObject);
  private
    FIsRunning: Boolean;
    FPendulumAngle: Double;  // 振り子の角度（ラジアン）
    FPendulumVelocity: Double;  // 振り子の角速度
    FMaxAngle: Double;  // 最大角度（ラジアン）
    FBPM: Integer;  // ビート・パー・ミニッツ
    procedure DrawPendulum(ACanvas: TCanvas; AWidth, AHeight: Integer);
    procedure UpdateMetronomeInterval;
    procedure PlayClickSound;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  GRAVITY = 9.8;  // 重力加速度
  PENDULUM_LENGTH = 120.0;  // 振り子の長さ（ピクセル）
  DAMPING = 0.998;  // 減衰係数（少し緩やかに）

procedure TForm1.FormCreate(Sender: TObject);
begin
  // 初期化
  FBPM := TrackBarTempo.Position;
  FMaxAngle := Pi / 4;  // 45度
  FPendulumAngle := 0;
  FPendulumVelocity := 0;
  FIsRunning := False;
  UpdateMetronomeInterval;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  // フォームの背景を描画
end;

procedure TForm1.PanelPendulumPaint(Sender: TObject);
var
  ACanvas: TCanvas;
begin
  ACanvas := PanelPendulum.Canvas;
  ACanvas.Brush.Color := clWhite;
  ACanvas.FillRect(0, 0, PanelPendulum.Width, PanelPendulum.Height);
  DrawPendulum(ACanvas, PanelPendulum.Width, PanelPendulum.Height);
end;

procedure TForm1.DrawPendulum(ACanvas: TCanvas; AWidth, AHeight: Integer);
var
  CenterX, CenterY: Integer;
  PivotX, PivotY: Integer;
  BobX, BobY: Integer;
  PendulumLength: Integer;
begin
  CenterX := AWidth div 2;
  CenterY := 40;  // 上部から少し下
  PivotX := CenterX;
  PivotY := CenterY;
  
  PendulumLength := Round(PENDULUM_LENGTH);
  
  // 振り子の先端（おもり）の位置を計算
  BobX := PivotX + Round(PendulumLength * Sin(FPendulumAngle));
  BobY := PivotY + Round(PendulumLength * Cos(FPendulumAngle));
  
  // 支点を描画
  ACanvas.Pen.Width := 3;
  ACanvas.Pen.Color := clBlack;
  ACanvas.Brush.Color := clBlack;
  ACanvas.Ellipse(PivotX - 5, PivotY - 5, PivotX + 5, PivotY + 5);
  
  // 振り子の棒を描画
  ACanvas.Pen.Width := 2;
  ACanvas.Pen.Color := clMaroon;
  ACanvas.MoveTo(PivotX, PivotY);
  ACanvas.LineTo(BobX, BobY);
  
  // おもりを描画
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Color := clBlack;
  ACanvas.Brush.Color := clSilver;
  ACanvas.Ellipse(BobX - 15, BobY - 15, BobX + 15, BobY + 15);
  
  // 目盛りを描画（オプション）
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Color := clGray;
  ACanvas.Pen.Style := psDot;
  ACanvas.MoveTo(CenterX - 50, CenterY);
  ACanvas.LineTo(CenterX + 50, CenterY);
end;

procedure TForm1.TrackBarTempoChange(Sender: TObject);
begin
  FBPM := TrackBarTempo.Position;
  LabelBPMValue.Caption := IntToStr(FBPM);
  UpdateMetronomeInterval;
end;

procedure TForm1.UpdateMetronomeInterval;
var
  IntervalMs: Integer;
begin
  if FBPM > 0 then
  begin
    IntervalMs := Round(60000.0 / FBPM);  // ミリ秒に変換
    TimerMetronome.Interval := IntervalMs;
    
    // 振り子の最大角度を計算（テンポに応じて調整）
    FMaxAngle := Pi / 4;  // 45度
  end;
end;

procedure TForm1.ButtonStartStopClick(Sender: TObject);
begin
  FIsRunning := not FIsRunning;
  
  if FIsRunning then
  begin
    ButtonStartStop.Caption := '停止';
    TimerMetronome.Enabled := True;
    TimerAnimation.Enabled := True;
    // 初期状態：振り子を最大角度に設定し、初期速度を与える
    FPendulumAngle := FMaxAngle;
    FPendulumVelocity := -0.5;  // 初期速度（左方向に動き始める）
    PlayClickSound;
    PanelPendulum.Invalidate;  // 即座に描画
  end
  else
  begin
    ButtonStartStop.Caption := '開始';
    TimerMetronome.Enabled := False;
    TimerAnimation.Enabled := False;
    // 振り子を停止位置に戻す
    FPendulumAngle := 0;
    FPendulumVelocity := 0;
    PanelPendulum.Invalidate;
  end;
end;

procedure TForm1.TimerMetronomeTimer(Sender: TObject);
begin
  PlayClickSound;
  // 各ビートで振り子にエネルギーを追加（自然な動きを維持）
  // 振り子が中央付近にある場合のみエネルギーを追加
  if Abs(FPendulumAngle) < Pi / 8 then
  begin
    // 速度にエネルギーを追加（方向は現在の速度に応じて）
    if FPendulumVelocity >= 0 then
      FPendulumVelocity := FPendulumVelocity + 0.3
    else
      FPendulumVelocity := FPendulumVelocity - 0.3;
  end;
end;

procedure TForm1.TimerAnimationTimer(Sender: TObject);
var
  Acceleration: Double;
  DeltaTime: Double;
begin
  if not FIsRunning then Exit;
  
  DeltaTime := TimerAnimation.Interval / 1000.0;  // 秒に変換（16ミリ秒 = 0.016秒）
  
  // 単振り子の物理シミュレーション
  // 加速度 = -(重力加速度 / 振り子の長さ) * sin(角度)
  Acceleration := -(GRAVITY / PENDULUM_LENGTH) * Sin(FPendulumAngle);
  
  // 速度を更新（加速度 × 時間）
  FPendulumVelocity := FPendulumVelocity + Acceleration * DeltaTime;
  
  // 減衰を適用（エネルギー損失）
  FPendulumVelocity := FPendulumVelocity * DAMPING;
  
  // 角度を更新（速度 × 時間）
  FPendulumAngle := FPendulumAngle + FPendulumVelocity * DeltaTime;
  
  // 角度を制限（最大角度を超えないように）
  if Abs(FPendulumAngle) > FMaxAngle then
  begin
    if FPendulumAngle > 0 then
      FPendulumAngle := FMaxAngle
    else
      FPendulumAngle := -FMaxAngle;
    // 最大角度に達したら速度を反転（少し減衰させる）
    FPendulumVelocity := FPendulumVelocity * -0.8;
  end;
  
  // 画面を再描画
  PanelPendulum.Invalidate;
end;

procedure TForm1.PlayClickSound;
begin
  // Windowsのビープ音を再生
  Windows.Beep(800, 50);  // 800Hz、50ミリ秒
end;

end.

