unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, CheckLst,
  ComCtrls, FileUtil;

type
  TForm1 = class(TForm)
    CheckListBox1: TCheckListBox;
    Edit1: TEdit;
    ButtonAdd: TButton;
    ButtonDelete: TButton;
    ButtonUp: TButton;
    ButtonDown: TButton;
    ButtonClear: TButton;
    StatusBar1: TStatusBar;
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonUpClick(Sender: TObject);
    procedure ButtonDownClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure CheckListBox1Click(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    DataFileName: string;
    procedure SaveData;
    procedure LoadData;
    procedure UpdateStatus;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // データファイルのパスを設定（実行ファイルと同じディレクトリ）
  DataFileName := ExtractFilePath(Application.ExeName) + 'shopping_list.txt';
  LoadData;
  UpdateStatus;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveData;
end;

procedure TForm1.SaveData;
var
  i: Integer;
  sl: TStringList;
  line: string;
begin
  try
    sl := TStringList.Create;
    try
      for i := 0 to CheckListBox1.Items.Count - 1 do
      begin
        // チェック状態とアイテム名を保存（形式: "1:牛乳" または "0:牛乳"）
        if CheckListBox1.Checked[i] then
          line := '1:' + CheckListBox1.Items[i]
        else
          line := '0:' + CheckListBox1.Items[i];
        sl.Add(line);
      end;
      sl.SaveToFile(DataFileName);
      UpdateStatus;
    finally
      sl.Free;
    end;
  except
    on E: Exception do
      ShowMessage('保存エラー: ' + E.Message);
  end;
end;

procedure TForm1.LoadData;
var
  sl: TStringList;
  i: Integer;
  line: string;
  checked: Boolean;
  itemName: string;
begin
  if not FileExists(DataFileName) then
    Exit;

  try
    sl := TStringList.Create;
    try
      sl.LoadFromFile(DataFileName);
      CheckListBox1.Items.Clear;
      for i := 0 to sl.Count - 1 do
      begin
        line := sl[i];
        if (Length(line) >= 2) and (line[1] in ['0', '1']) and (line[2] = ':') then
        begin
          checked := line[1] = '1';
          itemName := Copy(line, 3, Length(line));
          CheckListBox1.Items.Add(itemName);
          CheckListBox1.Checked[CheckListBox1.Items.Count - 1] := checked;
        end;
      end;
    finally
      sl.Free;
    end;
  except
    on E: Exception do
      ShowMessage('読み込みエラー: ' + E.Message);
  end;
end;

procedure TForm1.UpdateStatus;
var
  total, checked: Integer;
  i: Integer;
begin
  total := CheckListBox1.Items.Count;
  checked := 0;
  for i := 0 to CheckListBox1.Items.Count - 1 do
    if CheckListBox1.Checked[i] then
      Inc(checked);
  StatusBar1.SimpleText := Format('合計: %d アイテム / チェック済み: %d', [total, checked]);
end;

procedure TForm1.ButtonAddClick(Sender: TObject);
var
  itemText: string;
begin
  itemText := Trim(Edit1.Text);
  if itemText = '' then
  begin
    ShowMessage('アイテム名を入力してください。');
    Edit1.SetFocus;
    Exit;
  end;

  // 重複チェック（オプション）
  if CheckListBox1.Items.IndexOf(itemText) >= 0 then
  begin
    if MessageDlg('確認', Format('「%s」は既にリストにあります。追加しますか？', [itemText]),
      mtConfirmation, [mbYes, mbNo], 0) = mrNo then
      Exit;
  end;

  CheckListBox1.Items.Add(itemText);
  CheckListBox1.Checked[CheckListBox1.Items.Count - 1] := False;
  Edit1.Text := '';
  Edit1.SetFocus;
  SaveData;
  UpdateStatus;
end;

procedure TForm1.ButtonDeleteClick(Sender: TObject);
var
  index: Integer;
begin
  index := CheckListBox1.ItemIndex;
  if index < 0 then
  begin
    ShowMessage('削除するアイテムを選択してください。');
    Exit;
  end;

  CheckListBox1.Items.Delete(index);
  SaveData;
  UpdateStatus;
end;

procedure TForm1.ButtonUpClick(Sender: TObject);
var
  index: Integer;
  checked: Boolean;
  itemText: string;
begin
  index := CheckListBox1.ItemIndex;
  if index <= 0 then
    Exit;

  // 現在のアイテムの情報を保存
  itemText := CheckListBox1.Items[index];
  checked := CheckListBox1.Checked[index];

  // アイテムを削除して上に挿入
  CheckListBox1.Items.Delete(index);
  CheckListBox1.Items.Insert(index - 1, itemText);
  CheckListBox1.Checked[index - 1] := checked;
  CheckListBox1.ItemIndex := index - 1;
  SaveData;
end;

procedure TForm1.ButtonDownClick(Sender: TObject);
var
  index: Integer;
  checked: Boolean;
  itemText: string;
begin
  index := CheckListBox1.ItemIndex;
  if (index < 0) or (index >= CheckListBox1.Items.Count - 1) then
    Exit;

  // 現在のアイテムの情報を保存
  itemText := CheckListBox1.Items[index];
  checked := CheckListBox1.Checked[index];

  // アイテムを削除して下に挿入
  CheckListBox1.Items.Delete(index);
  CheckListBox1.Items.Insert(index + 1, itemText);
  CheckListBox1.Checked[index + 1] := checked;
  CheckListBox1.ItemIndex := index + 1;
  SaveData;
end;

procedure TForm1.ButtonClearClick(Sender: TObject);
begin
  if CheckListBox1.Items.Count = 0 then
    Exit;

  if MessageDlg('確認', 'すべてのアイテムを削除しますか？',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    CheckListBox1.Items.Clear;
    SaveData;
    UpdateStatus;
  end;
end;

procedure TForm1.CheckListBox1Click(Sender: TObject);
begin
  // チェック状態が変更されたら自動保存
  SaveData;
  UpdateStatus;
end;

procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: char);
begin
  // Enterキーで追加
  if Key = #13 then
  begin
    Key := #0;
    ButtonAddClick(Sender);
  end;
end;

end.
