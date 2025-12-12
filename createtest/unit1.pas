unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  QuestionData, Buttons;

type
  TAppMode = (amCreate, amQuiz);

  TForm1 = class(TForm)
    Panel1: TPanel;
    btnCreateMode: TButton;
    btnQuizMode: TButton;
    Panel2: TPanel;
    lblMode: TLabel;
    Panel3: TPanel;
    // 作成モード用コンポーネント
    lblQuestion: TLabel;
    memoQuestion: TMemo;
    lblChoices: TLabel;
    memoChoice1: TMemo;
    memoChoice2: TMemo;
    memoChoice3: TMemo;
    memoChoice4: TMemo;
    lblCorrectAnswer: TLabel;
    editCorrectAnswer: TEdit;
    btnSaveQuestion: TButton;
    lblQuestionList: TLabel;
    listboxQuestions: TListBox;
    btnDeleteQuestion: TButton;
    btnSaveToFile: TButton;
    btnLoadFromFile: TButton;
    // 出題モード用コンポーネント
    lblQuizQuestion: TLabel;
    memoQuizQuestion: TMemo;
    radioGroupChoices: TRadioGroup;
    btnAnswer: TButton;
    lblResult: TLabel;
    lblQuizInfo: TLabel;
    btnNext: TButton;
    btnPrev: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCreateModeClick(Sender: TObject);
    procedure btnQuizModeClick(Sender: TObject);
    procedure btnSaveQuestionClick(Sender: TObject);
    procedure btnDeleteQuestionClick(Sender: TObject);
    procedure btnSaveToFileClick(Sender: TObject);
    procedure btnLoadFromFileClick(Sender: TObject);
    procedure listboxQuestionsClick(Sender: TObject);
    procedure btnAnswerClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
  private
    FQuestionList: TQuestionList;
    FCurrentMode: TAppMode;
    FCurrentQuizIndex: Integer;
    procedure SetMode(AMode: TAppMode);
    procedure UpdateCreateModeUI;
    procedure UpdateQuizModeUI;
    procedure RefreshQuestionList;
    procedure LoadQuestionToForm(Question: TQuestion);
    procedure ClearCreateForm;
    procedure ShowQuizQuestion(Index: Integer);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FQuestionList := TQuestionList.Create;
  FCurrentQuizIndex := 0;
  SetMode(amCreate);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FQuestionList.Free;
end;

procedure TForm1.SetMode(AMode: TAppMode);
begin
  FCurrentMode := AMode;
  if AMode = amCreate then
  begin
    lblMode.Caption := '作成モード';
    UpdateCreateModeUI;
  end
  else
  begin
    lblMode.Caption := '出題モード';
    UpdateQuizModeUI;
  end;
end;

procedure TForm1.UpdateCreateModeUI;
begin
  // 作成モードのコンポーネントを表示
  lblQuestion.Visible := True;
  memoQuestion.Visible := True;
  lblChoices.Visible := True;
  memoChoice1.Visible := True;
  memoChoice2.Visible := True;
  memoChoice3.Visible := True;
  memoChoice4.Visible := True;
  lblCorrectAnswer.Visible := True;
  editCorrectAnswer.Visible := True;
  btnSaveQuestion.Visible := True;
  lblQuestionList.Visible := True;
  listboxQuestions.Visible := True;
  btnDeleteQuestion.Visible := True;
  btnSaveToFile.Visible := True;
  btnLoadFromFile.Visible := True;
  
  // 出題モードのコンポーネントを非表示
  lblQuizQuestion.Visible := False;
  memoQuizQuestion.Visible := False;
  radioGroupChoices.Visible := False;
  btnAnswer.Visible := False;
  lblResult.Visible := False;
  lblQuizInfo.Visible := False;
  btnNext.Visible := False;
  btnPrev.Visible := False;
  
  RefreshQuestionList;
end;

procedure TForm1.UpdateQuizModeUI;
begin
  // 作成モードのコンポーネントを非表示
  lblQuestion.Visible := False;
  memoQuestion.Visible := False;
  lblChoices.Visible := False;
  memoChoice1.Visible := False;
  memoChoice2.Visible := False;
  memoChoice3.Visible := False;
  memoChoice4.Visible := False;
  lblCorrectAnswer.Visible := False;
  editCorrectAnswer.Visible := False;
  btnSaveQuestion.Visible := False;
  lblQuestionList.Visible := False;
  listboxQuestions.Visible := False;
  btnDeleteQuestion.Visible := False;
  btnSaveToFile.Visible := False;
  btnLoadFromFile.Visible := False;
  
  // 出題モードのコンポーネントを表示
  lblQuizQuestion.Visible := True;
  memoQuizQuestion.Visible := True;
  radioGroupChoices.Visible := True;
  btnAnswer.Visible := True;
  lblResult.Visible := True;
  lblQuizInfo.Visible := True;
  btnNext.Visible := True;
  btnPrev.Visible := True;
  
  if FQuestionList.Count > 0 then
  begin
    FCurrentQuizIndex := 0;
    ShowQuizQuestion(0);
  end
  else
  begin
    memoQuizQuestion.Text := '問題が登録されていません。';
    radioGroupChoices.Items.Clear;
    lblQuizInfo.Caption := '問題数: 0';
    btnAnswer.Enabled := False;
    btnNext.Enabled := False;
    btnPrev.Enabled := False;
  end;
end;

procedure TForm1.btnCreateModeClick(Sender: TObject);
begin
  SetMode(amCreate);
end;

procedure TForm1.btnQuizModeClick(Sender: TObject);
begin
  SetMode(amQuiz);
end;

procedure TForm1.btnSaveQuestionClick(Sender: TObject);
var
  Question: TQuestion;
  CorrectAnswer: Integer;
begin
  if Trim(memoQuestion.Text) = '' then
  begin
    ShowMessage('問題文を入力してください。');
    Exit;
  end;
  
  if (Trim(memoChoice1.Text) = '') or (Trim(memoChoice2.Text) = '') or
     (Trim(memoChoice3.Text) = '') or (Trim(memoChoice4.Text) = '') then
  begin
    ShowMessage('すべての選択肢を入力してください。');
    Exit;
  end;
  
  if not TryStrToInt(editCorrectAnswer.Text, CorrectAnswer) then
  begin
    ShowMessage('正解番号を1-4の数字で入力してください。');
    Exit;
  end;
  
  if (CorrectAnswer < 1) or (CorrectAnswer > 4) then
  begin
    ShowMessage('正解番号は1-4の範囲で入力してください。');
    Exit;
  end;
  
  Question := TQuestion.Create;
  Question.Question := memoQuestion.Text;
  Question.Choices.Add(memoChoice1.Text);
  Question.Choices.Add(memoChoice2.Text);
  Question.Choices.Add(memoChoice3.Text);
  Question.Choices.Add(memoChoice4.Text);
  Question.CorrectAnswer := CorrectAnswer - 1; // 0-based index
  
  FQuestionList.Add(Question);
  RefreshQuestionList;
  ClearCreateForm;
  ShowMessage('問題を保存しました。');
end;

procedure TForm1.btnDeleteQuestionClick(Sender: TObject);
begin
  if listboxQuestions.ItemIndex >= 0 then
  begin
    if MessageDlg('確認', 'この問題を削除しますか？', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      FQuestionList.Delete(listboxQuestions.ItemIndex);
      RefreshQuestionList;
      ClearCreateForm;
    end;
  end
  else
    ShowMessage('削除する問題を選択してください。');
end;

procedure TForm1.btnSaveToFileClick(Sender: TObject);
begin
  if FQuestionList.Count = 0 then
  begin
    ShowMessage('保存する問題がありません。');
    Exit;
  end;
  
  FQuestionList.SaveToFile('questions.json');
  ShowMessage('問題をファイルに保存しました。');
end;

procedure TForm1.btnLoadFromFileClick(Sender: TObject);
begin
  if FileExists('questions.json') then
  begin
    FQuestionList.LoadFromFile('questions.json');
    RefreshQuestionList;
    ShowMessage('問題をファイルから読み込みました。');
  end
  else
    ShowMessage('ファイルが見つかりません。');
end;

procedure TForm1.listboxQuestionsClick(Sender: TObject);
begin
  if listboxQuestions.ItemIndex >= 0 then
    LoadQuestionToForm(FQuestionList[listboxQuestions.ItemIndex]);
end;

procedure TForm1.RefreshQuestionList;
var
  i: Integer;
begin
  listboxQuestions.Items.Clear;
  for i := 0 to FQuestionList.Count - 1 do
    listboxQuestions.Items.Add(Format('問題 %d: %s', [i + 1, 
      Copy(FQuestionList[i].Question, 1, 30)]));
end;

procedure TForm1.LoadQuestionToForm(Question: TQuestion);
begin
  memoQuestion.Text := Question.Question;
  if Question.Choices.Count >= 1 then
    memoChoice1.Text := Question.Choices[0]
  else
    memoChoice1.Text := '';
  if Question.Choices.Count >= 2 then
    memoChoice2.Text := Question.Choices[1]
  else
    memoChoice2.Text := '';
  if Question.Choices.Count >= 3 then
    memoChoice3.Text := Question.Choices[2]
  else
    memoChoice3.Text := '';
  if Question.Choices.Count >= 4 then
    memoChoice4.Text := Question.Choices[3]
  else
    memoChoice4.Text := '';
  editCorrectAnswer.Text := IntToStr(Question.CorrectAnswer + 1);
end;

procedure TForm1.ClearCreateForm;
begin
  memoQuestion.Text := '';
  memoChoice1.Text := '';
  memoChoice2.Text := '';
  memoChoice3.Text := '';
  memoChoice4.Text := '';
  editCorrectAnswer.Text := '';
  listboxQuestions.ItemIndex := -1;
end;

procedure TForm1.ShowQuizQuestion(Index: Integer);
var
  Question: TQuestion;
  i: Integer;
begin
  if (Index < 0) or (Index >= FQuestionList.Count) then
    Exit;
  
  Question := FQuestionList[Index];
  memoQuizQuestion.Text := Question.Question;
  
  radioGroupChoices.Items.Clear;
  for i := 0 to Question.Choices.Count - 1 do
    radioGroupChoices.Items.Add(Question.Choices[i]);
  
  radioGroupChoices.ItemIndex := -1;
  lblResult.Caption := '';
  lblQuizInfo.Caption := Format('問題 %d / %d', [Index + 1, FQuestionList.Count]);
  
  btnPrev.Enabled := Index > 0;
  btnNext.Enabled := Index < FQuestionList.Count - 1;
  btnAnswer.Enabled := True;
end;

procedure TForm1.btnAnswerClick(Sender: TObject);
var
  Question: TQuestion;
begin
  if FQuestionList.Count = 0 then
    Exit;
  
  Question := FQuestionList[FCurrentQuizIndex];
  
  if radioGroupChoices.ItemIndex < 0 then
  begin
    ShowMessage('選択肢を選んでください。');
    Exit;
  end;
  
  if radioGroupChoices.ItemIndex = Question.CorrectAnswer then
    lblResult.Caption := '正解！'
  else
    lblResult.Caption := Format('不正解。正解は %d 番です。', [Question.CorrectAnswer + 1]);
  
  btnAnswer.Enabled := False;
end;

procedure TForm1.btnNextClick(Sender: TObject);
begin
  if FCurrentQuizIndex < FQuestionList.Count - 1 then
  begin
    Inc(FCurrentQuizIndex);
    ShowQuizQuestion(FCurrentQuizIndex);
  end;
end;

procedure TForm1.btnPrevClick(Sender: TObject);
begin
  if FCurrentQuizIndex > 0 then
  begin
    Dec(FCurrentQuizIndex);
    ShowQuizQuestion(FCurrentQuizIndex);
  end;
end;

end.

