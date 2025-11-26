unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  TPhase = (phRest, phExercise);
  
  TForm1 = class(TForm)
    LabelStatus: TLabel;
    LabelTime: TLabel;
    LabelRound: TLabel;
    ButtonStart: TButton;
    ButtonStop: TButton;
    ButtonReset: TButton;
    Timer1: TTimer;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FRemainingTime: Integer;
    FCurrentRound: Integer;
    FPhase: TPhase;
    FIsRunning: Boolean;
    procedure UpdateDisplay;
    procedure StartPhase;
    procedure NextPhase;
  public

  end;

var
  Form1: TForm1;

const
  EXERCISE_TIME = 20;  // 運動時間（秒）
  REST_TIME = 10;      // 休憩時間（秒）
  TOTAL_ROUNDS = 8;    // 総ラウンド数

implementation

{$R *.lfm}

procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  if not FIsRunning then
  begin
    if FCurrentRound = 0 then
    begin
      // 最初の開始
      FCurrentRound := 1;
      FPhase := phExercise;
      FRemainingTime := EXERCISE_TIME;
    end;
    FIsRunning := True;
    Timer1.Enabled := True;
    ButtonStart.Enabled := False;
    ButtonStop.Enabled := True;
    UpdateDisplay;
  end;
end;

procedure TForm1.ButtonStopClick(Sender: TObject);
begin
  FIsRunning := False;
  Timer1.Enabled := False;
  ButtonStart.Enabled := True;
  ButtonStop.Enabled := False;
  UpdateDisplay;
end;

procedure TForm1.ButtonResetClick(Sender: TObject);
begin
  FIsRunning := False;
  Timer1.Enabled := False;
  FCurrentRound := 0;
  FPhase := phRest;
  FRemainingTime := 0;
  ButtonStart.Enabled := True;
  ButtonStop.Enabled := False;
  UpdateDisplay;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Dec(FRemainingTime);
  
  if FRemainingTime <= 0 then
  begin
    NextPhase;
  end
  else
  begin
    UpdateDisplay;
  end;
end;

procedure TForm1.UpdateDisplay;
var
  Minutes, Seconds: Integer;
  TimeStr: String;
begin
  // 時間表示を更新
  Minutes := FRemainingTime div 60;
  Seconds := FRemainingTime mod 60;
  TimeStr := Format('%.2d:%.2d', [Minutes, Seconds]);
  LabelTime.Caption := TimeStr;
  
  // ステータス表示を更新
  if FIsRunning then
  begin
    if FPhase = phExercise then
    begin
      LabelStatus.Caption := '全力で運動！';
      LabelStatus.Font.Color := clRed;
    end
    else
    begin
      LabelStatus.Caption := '休む';
      LabelStatus.Font.Color := clBlue;
    end;
  end
  else
  begin
    if FCurrentRound = 0 then
      LabelStatus.Caption := '準備完了'
    else
      LabelStatus.Caption := '一時停止中';
    LabelStatus.Font.Color := clBlack;
  end;
  
  // ラウンド表示を更新
  LabelRound.Caption := Format('ラウンド: %d / %d', [FCurrentRound, TOTAL_ROUNDS]);
end;

procedure TForm1.StartPhase;
begin
  if FPhase = phExercise then
  begin
    FRemainingTime := EXERCISE_TIME;
  end
  else
  begin
    FRemainingTime := REST_TIME;
  end;
  UpdateDisplay;
end;

procedure TForm1.NextPhase;
begin
  if FPhase = phExercise then
  begin
    // 運動終了 → 休憩へ
    FPhase := phRest;
    FRemainingTime := REST_TIME;
  end
  else
  begin
    // 休憩終了 → 次のラウンドの運動へ
    Inc(FCurrentRound);
    if FCurrentRound > TOTAL_ROUNDS then
    begin
      // 全ラウンド終了
      FIsRunning := False;
      Timer1.Enabled := False;
      ButtonStart.Enabled := True;
      ButtonStop.Enabled := False;
      LabelStatus.Caption := '完了！';
      LabelStatus.Font.Color := clGreen;
      LabelTime.Caption := '00:00';
      FCurrentRound := TOTAL_ROUNDS;
      Exit;
    end;
    FPhase := phExercise;
    FRemainingTime := EXERCISE_TIME;
  end;
  UpdateDisplay;
end;

end.

