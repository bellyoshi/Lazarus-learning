unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    LabelText: TLabel;
    EditInput: TEdit;
    LabelScore: TLabel;
    LabelTime: TLabel;
    ButtonStart: TButton;
    ButtonReset: TButton;
    Timer1: TTimer;
    LabelStatus: TLabel;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
    procedure EditInputChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditInputKeyPress(Sender: TObject; var Key: char);
  private
    FCurrentText: string;
    FScore: Integer;
    FTimeElapsed: Integer;
    FIsPlaying: Boolean;
    FTexts: TStringList;
    FCurrentIndex: Integer;
    procedure LoadNextText;
    procedure UpdateDisplay;
    procedure CheckInput;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FTexts := TStringList.Create;
  FTexts.Add('Hello World');
  FTexts.Add('Lazarus is great');
  FTexts.Add('Type this text quickly');
  FTexts.Add('Practice makes perfect');
  FTexts.Add('Keep typing to improve');
  FTexts.Add('Speed and accuracy matter');
  FTexts.Add('Focus on the keyboard');
  FTexts.Add('Enjoy the typing game');
  
  FScore := 0;
  FTimeElapsed := 0;
  FIsPlaying := False;
  FCurrentIndex := 0;
  EditInput.Enabled := False;
  UpdateDisplay;
end;

procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  if not FIsPlaying then
  begin
    FIsPlaying := True;
    FScore := 0;
    FTimeElapsed := 0;
    FCurrentIndex := 0;
    EditInput.Enabled := True;
    EditInput.Text := '';
    EditInput.SetFocus;
    Timer1.Enabled := True;
    LoadNextText;
    ButtonStart.Caption := '一時停止';
    UpdateDisplay;
  end
  else
  begin
    FIsPlaying := False;
    Timer1.Enabled := False;
    EditInput.Enabled := False;
    ButtonStart.Caption := '開始';
  end;
end;

procedure TForm1.ButtonResetClick(Sender: TObject);
begin
  FIsPlaying := False;
  Timer1.Enabled := False;
  FScore := 0;
  FTimeElapsed := 0;
  FCurrentIndex := 0;
  EditInput.Text := '';
  EditInput.Enabled := False;
  ButtonStart.Caption := '開始';
  LoadNextText;
  UpdateDisplay;
end;

procedure TForm1.LoadNextText;
begin
  if FTexts.Count > 0 then
  begin
    FCurrentText := FTexts[FCurrentIndex mod FTexts.Count];
    LabelText.Caption := FCurrentText;
    FCurrentIndex := (FCurrentIndex + 1) mod FTexts.Count;
    EditInput.Text := '';
    LabelStatus.Caption := '';
  end;
end;

procedure TForm1.UpdateDisplay;
var
  Minutes, Seconds: Integer;
begin
  Minutes := FTimeElapsed div 60;
  Seconds := FTimeElapsed mod 60;
  LabelScore.Caption := 'スコア: ' + IntToStr(FScore);
  LabelTime.Caption := Format('時間: %d:%02d', [Minutes, Seconds]);
end;

procedure TForm1.CheckInput;
var
  InputText: string;
  i: Integer;
  AllCorrect: Boolean;
begin
  InputText := EditInput.Text;
  AllCorrect := True;
  
  if Length(InputText) > Length(FCurrentText) then
  begin
    LabelStatus.Caption := '文字数が多すぎます';
    LabelStatus.Font.Color := clRed;
    Exit;
  end;
  
  for i := 1 to Length(InputText) do
  begin
    if i <= Length(FCurrentText) then
    begin
      if InputText[i] = FCurrentText[i] then
      begin
        // 正しい文字
      end
      else
      begin
        AllCorrect := False;
        LabelStatus.Caption := '間違った文字があります';
        LabelStatus.Font.Color := clRed;
        Exit;
      end;
    end;
  end;
  
  if AllCorrect and (Length(InputText) = Length(FCurrentText)) then
  begin
    // 完全に一致
    Inc(FScore);
    LabelStatus.Caption := '正解！';
    LabelStatus.Font.Color := clGreen;
    LoadNextText;
    UpdateDisplay;
  end
  else if AllCorrect then
  begin
    LabelStatus.Caption := '入力中...';
    LabelStatus.Font.Color := clBlue;
  end;
end;

procedure TForm1.EditInputChange(Sender: TObject);
begin
  if FIsPlaying then
  begin
    CheckInput;
  end;
end;

procedure TForm1.EditInputKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then // Enterキー
  begin
    Key := #0;
    if FIsPlaying then
    begin
      CheckInput;
    end;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if FIsPlaying then
  begin
    Inc(FTimeElapsed);
    UpdateDisplay;
  end;
end;

end.
