unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, fpjson, jsonparser, DateUtils;

type
  THabitData = record
    Name: string;
    Streak: Integer;        // 連続記録日数
    LastCheckDate: TDate;  // 最後にチェックした日付
    TodayChecked: Boolean;  // 今日チェック済みか
  end;

  TForm1 = class(TForm)
    PanelTitle: TPanel;
    PanelHabit1: TPanel;
    CheckBox1: TCheckBox;
    LabelHabit1: TLabel;
    LabelStreak1: TLabel;
    LabelToday1: TLabel;
    PanelHabit2: TPanel;
    CheckBox2: TCheckBox;
    LabelHabit2: TLabel;
    LabelStreak2: TLabel;
    LabelToday2: TLabel;
    PanelHabit3: TPanel;
    CheckBox3: TCheckBox;
    LabelHabit3: TLabel;
    LabelStreak3: TLabel;
    LabelToday3: TLabel;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure LabelHabit1Click(Sender: TObject);
    procedure LabelHabit2Click(Sender: TObject);
    procedure LabelHabit3Click(Sender: TObject);
  private
    Habits: array[1..3] of THabitData;
    DataFileName: string;
    LastCheckDate: TDate;  // アプリ最後にチェックした日付
    
    procedure LoadData;
    procedure SaveData;
    procedure UpdateUI;
    procedure CheckDateReset;
    procedure UpdateHabitStatus(Index: Integer);
    procedure EditHabitName(Index: Integer);
    function GetDataPath: string;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  // データファイルのパスを設定
  DataFileName := GetDataPath + 'habits.json';
  
  // 初期化
  LastCheckDate := Date;
  for i := 1 to 3 do
  begin
    Habits[i].Name := '習慣' + IntToStr(i) + ': [クリックして編集]';
    Habits[i].Streak := 0;
    Habits[i].LastCheckDate := 0;
    Habits[i].TodayChecked := False;
  end;
  
  // データを読み込む
  LoadData;
  
  // 日付チェック（日付が変わったらリセット）
  CheckDateReset;
  
  // UIを更新
  UpdateUI;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveData;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  UpdateHabitStatus(1);
end;

procedure TForm1.CheckBox2Change(Sender: TObject);
begin
  UpdateHabitStatus(2);
end;

procedure TForm1.CheckBox3Change(Sender: TObject);
begin
  UpdateHabitStatus(3);
end;

procedure TForm1.LabelHabit1Click(Sender: TObject);
begin
  EditHabitName(1);
end;

procedure TForm1.LabelHabit2Click(Sender: TObject);
begin
  EditHabitName(2);
end;

procedure TForm1.LabelHabit3Click(Sender: TObject);
begin
  EditHabitName(3);
end;

procedure TForm1.CheckDateReset;
var
  i: Integer;
  Today: TDate;
begin
  Today := Date;
  
  // 日付が変わった場合
  if Today > LastCheckDate then
  begin
    for i := 1 to 3 do
    begin
      // 昨日チェックしていなかった場合は連続記録をリセット
      if (Habits[i].LastCheckDate > 0) and (Habits[i].LastCheckDate < Today - 1) then
      begin
        Habits[i].Streak := 0;
      end;
      
      // 今日のチェック状態をリセット
      Habits[i].TodayChecked := False;
    end;
    
    LastCheckDate := Today;
    SaveData;
  end;
end;

procedure TForm1.UpdateHabitStatus(Index: Integer);
var
  Today: TDate;
begin
  Today := Date;
  
  // 日付チェック
  CheckDateReset;
  
  if Index < 1 then Exit;
  if Index > 3 then Exit;
  
  // チェックボックスの状態に応じて更新
  case Index of
    1: Habits[Index].TodayChecked := CheckBox1.Checked;
    2: Habits[Index].TodayChecked := CheckBox2.Checked;
    3: Habits[Index].TodayChecked := CheckBox3.Checked;
  end;
  
  // チェックされた場合
  if Habits[Index].TodayChecked then
  begin
    // 最後にチェックした日付を確認
    if (Habits[Index].LastCheckDate = 0) or 
       (Habits[Index].LastCheckDate < Today - 1) then
    begin
      // 新しい連続記録開始
      Habits[Index].Streak := 1;
    end
    else if Habits[Index].LastCheckDate = Today - 1 then
    begin
      // 連続記録を継続
      Inc(Habits[Index].Streak);
    end
    else if Habits[Index].LastCheckDate = Today then
    begin
      // 今日既にチェック済み（何もしない）
    end;
    
    Habits[Index].LastCheckDate := Today;
  end;
  
  // UIを更新
  UpdateUI;
  
  // データを保存
  SaveData;
end;

procedure TForm1.EditHabitName(Index: Integer);
var
  NewName: string;
begin
  if Index < 1 then Exit;
  if Index > 3 then Exit;
  
  NewName := InputBox('習慣名の編集', 
    '習慣' + IntToStr(Index) + 'の名前を入力してください:', 
    Habits[Index].Name);
  
  if Trim(NewName) <> '' then
  begin
    Habits[Index].Name := Trim(NewName);
    UpdateUI;
    SaveData;
  end;
end;

procedure TForm1.UpdateUI;
begin
  // 習慣1
  LabelHabit1.Caption := Habits[1].Name;
  LabelStreak1.Caption := '連続記録: ' + IntToStr(Habits[1].Streak) + '日';
  CheckBox1.Checked := Habits[1].TodayChecked;
  if Habits[1].TodayChecked then
    LabelToday1.Caption := '今日: ✓ 達成'
  else
    LabelToday1.Caption := '今日: 未達成';
  
  // 習慣2
  LabelHabit2.Caption := Habits[2].Name;
  LabelStreak2.Caption := '連続記録: ' + IntToStr(Habits[2].Streak) + '日';
  CheckBox2.Checked := Habits[2].TodayChecked;
  if Habits[2].TodayChecked then
    LabelToday2.Caption := '今日: ✓ 達成'
  else
    LabelToday2.Caption := '今日: 未達成';
  
  // 習慣3
  LabelHabit3.Caption := Habits[3].Name;
  LabelStreak3.Caption := '連続記録: ' + IntToStr(Habits[3].Streak) + '日';
  CheckBox3.Checked := Habits[3].TodayChecked;
  if Habits[3].TodayChecked then
    LabelToday3.Caption := '今日: ✓ 達成'
  else
    LabelToday3.Caption := '今日: 未達成';
end;

function TForm1.GetDataPath: string;
begin
  // アプリケーションの実行ファイルと同じディレクトリ
  Result := ExtractFilePath(Application.ExeName);
end;

procedure TForm1.LoadData;
var
  JSONData: TJSONData;
  JSONArray: TJSONArray;
  JSONItem: TJSONObject;
  FileContent: TStringList;
  Parser: TJSONParser;
  i: Integer;
  DateStr: string;
begin
  // ファイルが存在しない場合は初期値のまま
  if not FileExists(DataFileName) then
    Exit;
  
  try
    FileContent := TStringList.Create;
    try
      FileContent.LoadFromFile(DataFileName);
      Parser := TJSONParser.Create(FileContent.Text);
      try
        JSONData := Parser.Parse;
        try
          if JSONData is TJSONObject then
          begin
            // 最後にチェックした日付
            if TJSONObject(JSONData).Find('LastCheckDate') <> nil then
            begin
              DateStr := TJSONObject(JSONData).Get('LastCheckDate', '');
              if DateStr <> '' then
                LastCheckDate := StrToDateDef(DateStr, Date);
            end;
            
            // 習慣データ
            if TJSONObject(JSONData).Find('Habits') <> nil then
            begin
              JSONArray := TJSONObject(JSONData).Get('Habits', TJSONArray(nil)) as TJSONArray;
              if Assigned(JSONArray) then
              begin
                for i := 0 to JSONArray.Count - 1 do
                begin
                  if i >= 3 then Break;
                  
                  JSONItem := JSONArray.Items[i] as TJSONObject;
                  if Assigned(JSONItem) then
                  begin
                    Habits[i + 1].Name := JSONItem.Get('Name', '習慣' + IntToStr(i + 1));
                    Habits[i + 1].Streak := JSONItem.Get('Streak', 0);
                    DateStr := JSONItem.Get('LastCheckDate', '');
                    if DateStr <> '' then
                      Habits[i + 1].LastCheckDate := StrToDateDef(DateStr, 0)
                    else
                      Habits[i + 1].LastCheckDate := 0;
                    Habits[i + 1].TodayChecked := JSONItem.Get('TodayChecked', False);
                  end;
                end;
              end;
            end;
          end;
        finally
          JSONData.Free;
        end;
      finally
        Parser.Free;
      end;
    finally
      FileContent.Free;
    end;
  except
    // エラーが発生した場合は初期値のまま
    on E: Exception do
    begin
      // エラーを無視して続行
    end;
  end;
end;

procedure TForm1.SaveData;
var
  JSONData: TJSONObject;
  JSONArray: TJSONArray;
  JSONItem: TJSONObject;
  FileContent: TStringList;
  i: Integer;
begin
  try
    JSONData := TJSONObject.Create;
    try
      // 最後にチェックした日付
      JSONData.Add('LastCheckDate', DateToStr(LastCheckDate));
      
      // 習慣データ
      JSONArray := TJSONArray.Create;
      for i := 1 to 3 do
      begin
        JSONItem := TJSONObject.Create;
        JSONItem.Add('Name', Habits[i].Name);
        JSONItem.Add('Streak', Habits[i].Streak);
        if Habits[i].LastCheckDate > 0 then
          JSONItem.Add('LastCheckDate', DateToStr(Habits[i].LastCheckDate))
        else
          JSONItem.Add('LastCheckDate', '');
        JSONItem.Add('TodayChecked', Habits[i].TodayChecked);
        JSONArray.Add(JSONItem);
      end;
      JSONData.Add('Habits', JSONArray);
      
      // ファイルに保存
      FileContent := TStringList.Create;
      try
        FileContent.Text := JSONData.AsJSON;
        FileContent.SaveToFile(DataFileName);
      finally
        FileContent.Free;
      end;
    finally
      JSONData.Free;
    end;
  except
    // エラーが発生した場合は無視
    on E: Exception do
    begin
      // エラーを無視して続行
    end;
  end;
end;

end.
