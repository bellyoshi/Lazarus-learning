unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  Clipbrd, LCLType, StrUtils;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    FontDialog1: TFontDialog;
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem16Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure MenuItem20Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure Memo1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FFileName: string;
    FModified: Boolean;
    FSearchText: string;
    FSearchPos: Integer;
    procedure SetModified(Value: Boolean);
    function SaveFile: Boolean;
    function SaveFileAs: Boolean;
    function SearchText(const SearchStr: string; StartPos: Integer): Integer;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.SetModified(Value: Boolean);
begin
  FModified := Value;
  if FModified then
    Caption := 'メモ帳 - ' + ExtractFileName(FFileName) + ' *'
  else
    Caption := 'メモ帳 - ' + ExtractFileName(FFileName);
end;

function TForm1.SaveFile: Boolean;
var
  F: TextFile;
  i: Integer;
begin
  Result := False;
  if FFileName = '' then
    Result := SaveFileAs
  else
  begin
    try
      AssignFile(F, FFileName);
      Rewrite(F);
      for i := 0 to Memo1.Lines.Count - 1 do
        Writeln(F, Memo1.Lines[i]);
      CloseFile(F);
      SetModified(False);
      Result := True;
    except
      on E: Exception do
        ShowMessage('保存エラー: ' + E.Message);
    end;
  end;
end;

function TForm1.SaveFileAs: Boolean;
begin
  Result := False;
  if SaveDialog1.Execute then
  begin
    FFileName := SaveDialog1.FileName;
    Result := SaveFile;
  end;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  // 新規
  if FModified then
  begin
    case MessageDlg('変更が保存されていません。保存しますか？',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        if not SaveFile then Exit;
      mrCancel:
        Exit;
    end;
  end;
  Memo1.Clear;
  FFileName := '';
  SetModified(False);
  Caption := 'メモ帳';
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
var
  F: TextFile;
  Line: string;
begin
  // 開く
  if FModified then
  begin
    case MessageDlg('変更が保存されていません。保存しますか？',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        if not SaveFile then Exit;
      mrCancel:
        Exit;
    end;
  end;
  
  if OpenDialog1.Execute then
  begin
    try
      Memo1.Clear;
      AssignFile(F, OpenDialog1.FileName);
      Reset(F);
      while not EOF(F) do
      begin
        Readln(F, Line);
        Memo1.Lines.Add(Line);
      end;
      CloseFile(F);
      FFileName := OpenDialog1.FileName;
      SetModified(False);
    except
      on E: Exception do
        ShowMessage('ファイルを開くエラー: ' + E.Message);
    end;
  end;
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  // 保存
  SaveFile;
end;

procedure TForm1.MenuItem5Click(Sender: TObject);
begin
  // 名前を付けて保存
  SaveFileAs;
end;

procedure TForm1.MenuItem7Click(Sender: TObject);
begin
  // 終了
  Close;
end;

procedure TForm1.MenuItem8Click(Sender: TObject);
begin
  // 切り取り
  Memo1.CutToClipboard;
end;

procedure TForm1.MenuItem9Click(Sender: TObject);
begin
  // コピー
  Memo1.CopyToClipboard;
end;

procedure TForm1.MenuItem10Click(Sender: TObject);
begin
  // 貼り付け
  Memo1.PasteFromClipboard;
end;

procedure TForm1.MenuItem11Click(Sender: TObject);
begin
  // すべて選択
  Memo1.SelectAll;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := True;
  if FModified then
  begin
    case MessageDlg('変更が保存されていません。保存しますか？',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        CanClose := SaveFile;
      mrNo:
        CanClose := True;
      mrCancel:
        CanClose := False;
    end;
  end;
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
  if not FModified then
    SetModified(True);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FFileName := '';
  FModified := False;
  FSearchText := '';
  FSearchPos := 0;
  Caption := 'メモ帳';
end;

function TForm1.SearchText(const SearchStr: string; StartPos: Integer): Integer;
var
  MemoText: string;
  FoundPos: Integer;
begin
  Result := -1;
  if SearchStr = '' then Exit;
  
  MemoText := Memo1.Text;
  if StartPos < 1 then StartPos := 1;
  if StartPos > Length(MemoText) then Exit;
  
  FoundPos := PosEx(SearchStr, MemoText, StartPos);
  if FoundPos > 0 then
  begin
    Result := FoundPos;
    Memo1.SelStart := FoundPos - 1;
    Memo1.SelLength := Length(SearchStr);
    Memo1.SetFocus;
  end;
end;

procedure TForm1.MenuItem15Click(Sender: TObject);
var
  SearchStr: string;
begin
  // 検索
  if InputQuery('検索', '検索する文字列:', SearchStr) then
  begin
    if SearchStr <> '' then
    begin
      FSearchText := SearchStr;
      FSearchPos := 1;
      if SearchText(FSearchText, FSearchPos) = -1 then
        ShowMessage('見つかりませんでした。');
    end;
  end;
end;

procedure TForm1.MenuItem16Click(Sender: TObject);
var
  FoundPos: Integer;
begin
  // 次を検索
  if FSearchText = '' then
  begin
    MenuItem15Click(Sender);
    Exit;
  end;
  
  FSearchPos := Memo1.SelStart + Memo1.SelLength + 1;
  FoundPos := SearchText(FSearchText, FSearchPos);
  if FoundPos = -1 then
  begin
    // 最初から再検索
    FoundPos := SearchText(FSearchText, 1);
    if FoundPos = -1 then
      ShowMessage('見つかりませんでした。');
  end;
end;

procedure TForm1.MenuItem18Click(Sender: TObject);
begin
  // フォント
  FontDialog1.Font := Memo1.Font;
  if FontDialog1.Execute then
    Memo1.Font := FontDialog1.Font;
end;

procedure TForm1.MenuItem19Click(Sender: TObject);
begin
  // 折り返し
  MenuItem19.Checked := not MenuItem19.Checked;
  Memo1.WordWrap := MenuItem19.Checked;
  if MenuItem19.Checked then
    Memo1.ScrollBars := ssVertical
  else
    Memo1.ScrollBars := ssBoth;
end;

procedure TForm1.MenuItem20Click(Sender: TObject);
begin
  // バージョン情報
  ShowMessage('シンプルメモ帳' + LineEnding + 
              'Version 1.0' + LineEnding + 
              'Lazarusで作成されたシンプルなメモ帳アプリケーションです。');
end;

end.

