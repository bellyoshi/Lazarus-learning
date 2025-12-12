unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Math;

type
  TForm1 = class(TForm)
    LBL_Problem: TLabel;
    EDT_Answer: TEdit;
    BTN_Submit: TButton;
    LBL_Feedback: TLabel;
    LBL_Score: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BTN_SubmitClick(Sender: TObject);
  private
    procedure GenerateProblem;
  public

  end;

var
  Form1: TForm1;
  CorrectAnswer: Integer;
  CurrentQuestion: Integer = 0;
  Score: Integer = 0;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  CurrentQuestion := 0;
  Score := 0;
  Randomize;
  GenerateProblem;
end;

procedure TForm1.GenerateProblem;
var
  Num1, Num2: Integer;
  OpType: Integer;
  OperatorChar: String;
begin
  Inc(CurrentQuestion);
  
  Num1 := RandomRange(1, 99);
  Num2 := RandomRange(1, 99);
  OpType := RandomRange(0, 3); // 0: +, 1: -, 2: *, 3: /
  
  case OpType of
    0: begin
         OperatorChar := '+';
         CorrectAnswer := Num1 + Num2;
       end;
    1: begin
         OperatorChar := '-';
         CorrectAnswer := Num1 - Num2;
       end;
    2: begin
         OperatorChar := '×';
         CorrectAnswer := Num1 * Num2;
       end;
    3: begin
         OperatorChar := '÷';
         // 割り算は整数になるように調整
         CorrectAnswer := Num1;
         Num2 := RandomRange(1, 20); // 割る数を小さくする
         Num1 := Num2 * RandomRange(1, 10); // 割られる数を割る数の倍数にする
         CorrectAnswer := Num1 div Num2;
       end;
  end;
  
  LBL_Problem.Caption := Format('%d %s %d = ?', [Num1, OperatorChar, Num2]);
  LBL_Feedback.Caption := '';
  EDT_Answer.Text := '';
  LBL_Score.Caption := Format('スコア: %d/10', [Score]);
end;

procedure TForm1.BTN_SubmitClick(Sender: TObject);
var
  UserAnswer: Integer;
begin
  UserAnswer := StrToIntDef(EDT_Answer.Text, -99999);
  
  if UserAnswer = CorrectAnswer then
  begin
    Inc(Score);
    LBL_Feedback.Caption := '正解！';
    LBL_Feedback.Font.Color := clGreen;
  end
  else
  begin
    LBL_Feedback.Caption := Format('不正解。正解は %d です。', [CorrectAnswer]);
    LBL_Feedback.Font.Color := clRed;
  end;
  
  LBL_Score.Caption := Format('スコア: %d/10', [Score]);
  
  if CurrentQuestion >= 10 then
  begin
    ShowMessage(Format('10問完了しました！最終スコア: %d/10', [Score]));
    CurrentQuestion := 0;
    Score := 0;
  end;
  
  // 少し待ってから次の問題を表示
  Application.ProcessMessages;
  Sleep(1500);
  
  if CurrentQuestion < 10 then
    GenerateProblem
  else
  begin
    LBL_Problem.Caption := 'すべての問題が完了しました！';
    EDT_Answer.Text := '';
    EDT_Answer.Enabled := False;
    BTN_Submit.Enabled := False;
  end;
end;

end.

