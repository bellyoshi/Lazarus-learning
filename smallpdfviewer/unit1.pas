unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  PdfViewer;

type
  TForm1 = class(TForm)
    Image1: TImage;
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FPdfViewer: TPdfViewer;
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

procedure TForm1.LoadPdfFile(const FileName: string);
begin
  try
    if Assigned(FPdfViewer) then
    begin
      FPdfViewer.Free;
    end;

    FPdfViewer := TPdfViewer.Create(FileName);

    UpdatePage;
    
    Caption := 'PDF Viewer - ' + ExtractFileName(FileName);
  except
    on E: Exception do
    begin
      ShowMessage('PDFファイルの読み込みに失敗しました: ' + E.Message);
      if Assigned(FPdfViewer) then
      begin
        FPdfViewer.Free;
        FPdfViewer := nil;
      end;
    end;
  end;
end;

procedure TForm1.UpdatePageDisplay;
var
  Bitmap: TBitmap;
  PageWidth, PageHeight: Double;
  AspectRatio: Double;
  DisplayWidth, DisplayHeight: Integer;
  ImageWidth, ImageHeight: Integer;
begin
  if not Assigned(FPdfViewer) then
  begin
    Image1.Picture.Clear;
    Exit;
  end;
  
  try
    // ページのサイズを取得
    PageWidth := FPdfViewer.GetCurrentPageWidth;
    PageHeight := FPdfViewer.GetCurrentPageHeight;
    
    if (PageWidth > 0) and (PageHeight > 0) then
    begin
      // ページのアスペクト比を計算
      AspectRatio := PageWidth / PageHeight;
      
      // 利用可能な表示領域のサイズを取得
      ImageWidth := Image1.Width;
      ImageHeight := Image1.Height;
      
      // アスペクト比を維持しながら表示サイズを計算
      if (ImageWidth / ImageHeight) > AspectRatio then
      begin
        // 高さに合わせる
        DisplayHeight := ImageHeight;
        DisplayWidth := Round(ImageHeight * AspectRatio);
      end
      else
      begin
        // 幅に合わせる
        DisplayWidth := ImageWidth;
        DisplayHeight := Round(ImageWidth / AspectRatio);
      end;
      
      // ビットマップを生成（アスペクト比を維持したサイズで）
      Bitmap := FPdfViewer.GetBitmap(DisplayWidth, DisplayHeight);
    end
    else
    begin
      // ページサイズが取得できない場合は元のサイズで表示
      Bitmap := FPdfViewer.GetBitmap(Image1.Width, Image1.Height);
    end;
    
    try
      Image1.Picture.Assign(Bitmap);
    finally
      Bitmap.Free;
    end;
  except
    on E: Exception do
      ShowMessage('ページの表示に失敗しました: ' + E.Message);
  end;
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
  // フォームがリサイズされた時にページ表示を更新
  if Assigned(FPdfViewer) then
    UpdatePageDisplay;
end;

end.

