unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, FileUtil, Process, ComCtrls, ShellAPI, Windows;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    EditAppName: TEdit;
    Label2: TLabel;
    EditProjectFolder: TEdit;
    Label3: TLabel;
    EditPrompt: TEdit;
    ButtonCreate: TButton;
    StatusBar1: TStatusBar;
    procedure ButtonCreateClick(Sender: TObject);
    procedure EditAppNameKeyPress(Sender: TObject; var Key: char);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.ButtonCreateClick(Sender: TObject);
var
  AppName: String;
  ProjectFolder: String;
  AppFolder: String;
  TemplatePath: String;
  Prompt: String;
  CursorArgs: String;
begin
  StatusBar1.SimpleText := '';
  AppName := Trim(EditAppName.Text);
  ProjectFolder := Trim(EditProjectFolder.Text);
  Prompt := Trim(EditPrompt.Text);
  
  // 入力チェック
  if AppName = '' then
  begin
    StatusBar1.SimpleText := 'エラー: アプリ名を入力してください。';
    Exit;
  end;
  
  if ProjectFolder = '' then
  begin
    StatusBar1.SimpleText := 'エラー: プロジェクト配置フォルダを入力してください。';
    Exit;
  end;
  
  // プロジェクト配置フォルダの存在確認
  if not DirectoryExists(ProjectFolder) then
  begin
    StatusBar1.SimpleText := 'エラー: プロジェクト配置フォルダが存在しません: ' + ProjectFolder;
    Exit;
  end;
  
  // アプリフォルダのパス作成
  AppFolder := IncludeTrailingPathDelimiter(ProjectFolder) + AppName;
  
  // アプリフォルダが既に存在する場合
  if DirectoryExists(AppFolder) then
  begin
    if MessageDlg('確認', 'フォルダが既に存在します。続行しますか？', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    begin
      StatusBar1.SimpleText := 'キャンセルされました。';
      Exit;
    end;
  end;
  
  // テンプレートフォルダのパス
  TemplatePath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'template';
  
  // テンプレートフォルダの存在確認
  if not DirectoryExists(TemplatePath) then
  begin
    StatusBar1.SimpleText := 'エラー: テンプレートフォルダが存在しません: ' + TemplatePath;
    Exit;
  end;
  
  try
    StatusBar1.SimpleText := 'プロジェクトを作成中...';
    Application.ProcessMessages;
    
    // アプリフォルダを作成
    if not DirectoryExists(AppFolder) then
      CreateDir(AppFolder);
    
    // テンプレートの中身をコピー
    CopyDirTree(TemplatePath, AppFolder, [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime]);
    
    StatusBar1.SimpleText := 'プロジェクトを作成しました: ' + AppFolder;
    
    // Cursorでアプリフォルダを開く（プロンプト付き）
    if Prompt <> '' then
      CursorArgs := AppFolder + ' --prompt "' + Prompt + '"'
    else
      CursorArgs := AppFolder;
    ShellExecute(0, 'open', PChar('cursor'), PChar(CursorArgs), nil, SW_SHOWNORMAL);
    
  except
    on E: Exception do
    begin
      StatusBar1.SimpleText := 'エラーが発生しました: ' + E.Message;
    end;
  end;
end;

procedure TForm1.EditAppNameKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then  // Enterキー
  begin
    Key := #0;  // デフォルトの動作をキャンセル
    ButtonCreateClick(Sender);
  end;
end;

end.

