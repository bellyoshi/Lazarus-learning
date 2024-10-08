unit Unit2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
   fpvectorial,pdfvectorialreader;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure LoadPDF(FileName: String);
  private

  public

  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
      // 例: 線を描く
  Form2.Canvas.Pen.Color := clRed;
  Form2.Canvas.MoveTo(10, 10);  // 開始座標
  Form2.Canvas.LineTo(100, 100); // 終了座標

  // 例: 円を描く
  Form2.Canvas.Brush.Color := clBlue;
  Form2.Canvas.Ellipse(50, 50, 150, 150);
end;

procedure TForm2.LoadPDF(FileName: string);
var
  PDFDocument: TvVectorialDocument;
begin
  PDFDocument := TvVectorialDocument.Create;
  try
    PDFDocument.ReadFromFile(FileName, vfPDF);
    // 1ページ目を描画（必要に応じてページ番号を変更）
    PDFDocument.GetPage(0).Render(
    Image1.Picture.Bitmap.Canvas);
  finally
    PDFDocument.Free;
  end;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute() then
  begin
    Exit;
  end;
  LoadPDF(OpenDialog1.FileName);


end;

procedure TForm2.FormPaint(Sender: TObject);
begin
  // 例: 線を描く
  Canvas.Pen.Color := clRed;
  Canvas.MoveTo(10, 10);  // 開始座標
  Canvas.LineTo(100, 100); // 終了座標

  // 例: 円を描く
  Canvas.Brush.Color := clBlue;
  Canvas.Ellipse(50, 50, 150, 150);
end;

procedure TForm2.Image1Click(Sender: TObject);
begin

end;

end.

