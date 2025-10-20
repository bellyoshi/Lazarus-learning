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
  
  try
    Bitmap := FPdfViewer.GetBitmap(Image1.Width, Image1.Height);
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

end.

