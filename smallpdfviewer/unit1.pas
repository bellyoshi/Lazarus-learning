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

