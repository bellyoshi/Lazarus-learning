unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  PdfViewer;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Edit1: TEdit;
    Button4: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FPdfViewer: TPdfViewer;
    FCurrentFileName: string;
    procedure LoadPdfFile(const FileName: string);
    procedure UpdatePageDisplay;
    procedure UpdatePageInfo;
    procedure UpdatePage;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FPdfViewer := nil;
  FCurrentFileName := '';
  Edit1.Text := '';
  Edit1.ReadOnly := True;  // 編集ボタン押下前は読み取り専用
  Button4.Enabled := False;
  Button5.Enabled := False;
  UpdatePage;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FPdfViewer) then
    FPdfViewer.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    LoadPdfFile(OpenDialog1.FileName);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if Assigned(FPdfViewer) then
  begin
    FPdfViewer.Previous;
    UpdatePage;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if Assigned(FPdfViewer) then
  begin
    FPdfViewer.Next;
    UpdatePage;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  // 編集ボタンクリック時：テキストボックスを編集可能に、保存ボタンを有効化
  Edit1.ReadOnly := False;
  Button4.Enabled := False;
  Button5.Enabled := True;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  NewFileName: string;
  OldFileName: string;
  FilePath: string;
begin
  // 保存ボタンクリック時：PDFをクローズ→ファイル名変更→再オープン→ボタン状態リセット
  if (FCurrentFileName = '') or (Edit1.Text = '') then
    Exit;
  
  try
    OldFileName := FCurrentFileName;
    FilePath := ExtractFilePath(OldFileName);
    NewFileName := FilePath + Edit1.Text;
    
    // 拡張子が.pdfでない場合は追加
    if LowerCase(ExtractFileExt(NewFileName)) <> '.pdf' then
      NewFileName := NewFileName + '.pdf';
    
    // 同じファイル名の場合は何もしない
    if OldFileName = NewFileName then
    begin
      Edit1.ReadOnly := True;  // 編集ボタン押下前は読み取り専用
      Button4.Enabled := True;
      Button5.Enabled := False;
      Exit;
    end;
    
    // PDFをクローズ
    if Assigned(FPdfViewer) then
    begin
      FPdfViewer.Free;
      FPdfViewer := nil;
    end;
    
    // ファイル名を変更
    if RenameFile(OldFileName, NewFileName) then
    begin
      // 変更した名前のファイルを再度開く
      LoadPdfFile(NewFileName);
    end
    else
    begin
      ShowMessage('ファイル名の変更に失敗しました。');
      // 元のファイルを再度開く
      LoadPdfFile(OldFileName);
    end;
  except
    on E: Exception do
    begin
      ShowMessage('エラーが発生しました: ' + E.Message);
      // エラー時は元のファイルを再度開く
      if FileExists(OldFileName) then
        LoadPdfFile(OldFileName);
    end;
  end;
end;

procedure TForm1.LoadPdfFile(const FileName: string);
begin
  try
    if Assigned(FPdfViewer) then
    begin
      FPdfViewer.Free;
    end;

    FPdfViewer := TPdfViewer.Create(FileName);
    FCurrentFileName := FileName;
    Edit1.Text := ExtractFileName(FileName);

    UpdatePage;
    
    Caption := 'PDF Viewer - ' + ExtractFileName(FileName);
    
    // ボタン状態をリセット（編集ボタン押下前は読み取り専用）
    Edit1.ReadOnly := True;  // 編集ボタン押下前は読み取り専用
    Button4.Enabled := True;  // PDFファイルが開かれたので編集ボタンを有効化
    Button5.Enabled := False;
  except
    on E: Exception do
    begin
      ShowMessage('PDFファイルの読み込みに失敗しました: ' + E.Message);
      if Assigned(FPdfViewer) then
      begin
        FPdfViewer.Free;
        FPdfViewer := nil;
      end;
      FCurrentFileName := '';
      Edit1.Text := '';
      Edit1.ReadOnly := True;  // 編集ボタン押下前は読み取り専用
      Button4.Enabled := False;  // ファイルが開かれていないので編集ボタンを無効化
      Button5.Enabled := False;
    end;
  end;
end;

procedure TForm1.UpdatePageDisplay;
var
  Bitmap: TBitmap;
begin
  if not Assigned(FPdfViewer) then
  begin
    Image1.Picture.Clear;
    Exit;
  end;
  
  Bitmap := FPdfViewer.GetBitmap(Panel1.ClientWidth, Panel1.ClientHeight);
  Image1.Width := Bitmap.Width;
  Image1.Height := Bitmap.Height;
  Image1.Picture.Assign(Bitmap);
  Bitmap.Free;

end;

procedure TForm1.UpdatePageInfo;
begin
  if Assigned(FPdfViewer) then
    Label2.Caption := Format('ページ %d / %d', [FPdfViewer.PageIndex + 1, FPdfViewer.PageCount])
  else
    Label2.Caption := 'ページ 0 / 0';
    
  Button2.Enabled := Assigned(FPdfViewer) and FPdfViewer.CanPrevious;
  Button3.Enabled := Assigned(FPdfViewer) and FPdfViewer.CanNext;
end;

procedure TForm1.UpdatePage;
begin
  UpdatePageDisplay;
  UpdatePageInfo;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Panel1.Left := 0;
  Panel1.Top:=50;
  Panel1.Width:= Form1.Width;
  Panel1.Height:=Form1.Height - Panel1.Top;
  // フォームがリサイズされた時にページ表示を更新
  if Assigned(FPdfViewer) then
    UpdatePage;
end;

end.

