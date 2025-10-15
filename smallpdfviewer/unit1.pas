unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  PdfImageCreator;

type
  TForm1 = class(TForm)
    Image1: TImage;
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FPdfImageCreator: TPdfImageCreator;
    FCurrentPage: Integer;
    procedure LoadPdfFile(const FileName: string);
    procedure UpdatePageDisplay;
    procedure UpdatePageInfo;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCurrentPage := 0;
  FPdfImageCreator := nil;
  UpdatePageInfo;
  
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  
  if Assigned(FPdfImageCreator) then
    FPdfImageCreator.Free;
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
  if Assigned(FPdfImageCreator) and (FCurrentPage > 0) then
  begin
    Dec(FCurrentPage);
    UpdatePageDisplay;
    UpdatePageInfo;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if Assigned(FPdfImageCreator) and (FCurrentPage < FPdfImageCreator.PageCount - 1) then
  begin
    Inc(FCurrentPage);
    UpdatePageDisplay;
    UpdatePageInfo;
  end;
end;

procedure TForm1.LoadPdfFile(const FileName: string);
var
  Bitmap: TBitmap;
begin
  
  try
    if Assigned(FPdfImageCreator) then
      FPdfImageCreator.Free;
    
    FPdfImageCreator := TPdfImageCreator.Create(FileName, 0);
    FCurrentPage := 0;
    
    UpdatePageDisplay;
    UpdatePageInfo;
    
    Caption := 'PDF Viewer - ' + ExtractFileName(FileName);
  except
    on E: Exception do
    begin
      ShowMessage('PDFファイルの読み込みに失敗しました: ' + E.Message);
      if Assigned(FPdfImageCreator) then
      begin
        FPdfImageCreator.Free;
        FPdfImageCreator := nil;
      end;
    end;
  end;
end;

procedure TForm1.UpdatePageDisplay;
var
  Bitmap: TBitmap;
begin
  if not Assigned(FPdfImageCreator) then
  begin
    Image1.Picture.Clear;
    Exit;
  end;
  
  try
    FPdfImageCreator.PageIndex := FCurrentPage;
    Bitmap := FPdfImageCreator.GetBitmap(Image1.Width, Image1.Height);
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
  if Assigned(FPdfImageCreator) then
    Label2.Caption := Format('ページ %d / %d', [FCurrentPage + 1, FPdfImageCreator.PageCount])
  else
    Label2.Caption := 'ページ 0 / 0';
    
  Button2.Enabled := Assigned(FPdfImageCreator) and (FCurrentPage > 0);
  Button3.Enabled := Assigned(FPdfImageCreator) and (FCurrentPage < FPdfImageCreator.PageCount - 1);
end;

end.

