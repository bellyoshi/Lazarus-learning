unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  TTimerState = (tsIdle, tsWorking, tsShortBreak, tsLongBreak);
  
  TForm1 = class(TForm)
    LabelTime: TLabel;
    LabelStatus: TLabel;
    LabelSetCount: TLabel;
    ButtonStartStop: TButton;
    ButtonReset: TButton;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure ButtonStartStopClick(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FState: TTimerState;
    FRemainingSeconds: Integer;
    FSetCount: Integer;
    FIsRunning: Boolean;
    FLongBreakMinutes: Integer;
    
    procedure UpdateDisplay;
    procedure StartTimer;
    procedure StopTimer;
    procedure ResetTimer;
    procedure NextState;
    function FormatTime(Seconds: Integer): String;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  WORK_MINUTES = 25;
  SHORT_BREAK_MINUTES = 5;
  LONG_BREAK_MINUTES = 15; // デフォルトは15分、最大30分

procedure TForm1.FormCreate(Sender: TObject);
begin
  FState := tsIdle;
  FRemainingSeconds := WORK_MINUTES * 60;
  FSetCount := 0;
  FIsRunning := False;
  FLongBreakMinutes := LONG_BREAK_MINUTES;
  UpdateDisplay;
end;

procedure TForm1.UpdateDisplay;
var
  TimeStr: String;
begin
  TimeStr := FormatTime(FRemainingSeconds);
  LabelTime.Caption := TimeStr;
  
  case FState of
    tsIdle:
      begin
        LabelStatus.Caption := '準備完了';
        LabelStatus.Font.Color := clBlack;
      end;
    tsWorking:
      begin
        LabelStatus.Caption := '作業中';
        LabelStatus.Font.Color := clRed;
      end;
    tsShortBreak:
      begin
        LabelStatus.Caption := '短い休憩中';
        LabelStatus.Font.Color := clBlue;
      end;
    tsLongBreak:
      begin
        LabelStatus.Caption := '長い休憩中';
        LabelStatus.Font.Color := clGreen;
      end;
  end;
  
  LabelSetCount.Caption := Format('セット数: %d / 4', [FSetCount]);
  
  if FIsRunning then
    ButtonStartStop.Caption := '停止'
  else
    ButtonStartStop.Caption := '開始';
end;

function TForm1.FormatTime(Seconds: Integer): String;
var
  M, S: Integer;
begin
  M := Seconds div 60;
  S := Seconds mod 60;
  Result := Format('%.2d:%.2d', [M, S]);
end;

procedure TForm1.StartTimer;
begin
  FIsRunning := True;
  Timer1.Enabled := True;
  UpdateDisplay;
end;

procedure TForm1.StopTimer;
begin
  FIsRunning := False;
  Timer1.Enabled := False;
  UpdateDisplay;
end;

procedure TForm1.ResetTimer;
begin
  StopTimer;
  FState := tsIdle;
  FRemainingSeconds := WORK_MINUTES * 60;
  FSetCount := 0;
  UpdateDisplay;
end;

procedure TForm1.NextState;
begin
  case FState of
    tsIdle, tsShortBreak, tsLongBreak:
      begin
        // 作業時間に移行
        FState := tsWorking;
        FRemainingSeconds := WORK_MINUTES * 60;
      end;
    tsWorking:
      begin
        Inc(FSetCount);
        if FSetCount >= 4 then
        begin
          // 4セット終了後は長い休憩
          FState := tsLongBreak;
          FRemainingSeconds := FLongBreakMinutes * 60;
          FSetCount := 0; // セット数をリセット
        end
        else
        begin
          // 短い休憩
          FState := tsShortBreak;
          FRemainingSeconds := SHORT_BREAK_MINUTES * 60;
        end;
      end;
  end;
end;

procedure TForm1.ButtonStartStopClick(Sender: TObject);
begin
  if FIsRunning then
    StopTimer
  else
  begin
    if FState = tsIdle then
    begin
      // アイドル状態から開始する場合は作業時間から始める
      FState := tsWorking;
      FRemainingSeconds := WORK_MINUTES * 60;
    end;
    StartTimer;
  end;
end;

procedure TForm1.ButtonResetClick(Sender: TObject);
begin
  ResetTimer;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Dec(FRemainingSeconds);
  
  if FRemainingSeconds <= 0 then
  begin
    // 時間切れ - 次の状態に移行
    NextState;
    // 通知音を鳴らす（オプション）
    Beep;
  end;
  
  UpdateDisplay;
end;

end.

