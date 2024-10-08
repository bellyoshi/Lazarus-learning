unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
uses
  fpPDFium;
procedure ShowPDF(const fileName: string);
var
  doc: FPDF_DOCUMENT;
  page: FPDF_PAGE;
  bmp: TBitmap;
begin

  FPDF_InitLibrary();
  // PDFドキュメントを読み込む
  doc := FPDF_LoadDocument(PChar(fileName), nil);
  if doc = nil then
  begin
    ShowMessage('Failed to open PDF document');
    Exit;
  end;

  // 最初のページを読み込む
  page := FPDF_LoadPage(doc, 1);
  if page = nil then
  begin
    ShowMessage('Failed to load PDF page');
    FPDF_CloseDocument(doc);
    Exit;
  end;

  // 描画用のビットマップを作成
  bmp := TBitmap.Create;
  try
    bmp.SetSize(800, 1200); // 800x1200ピクセルのサイズを指定
    bmp.Canvas.FillRect(0, 0, 800, 1200);

    // PDFページをビットマップにレンダリング
    FPDF_RenderPageBitmap(bmp.RawImage.Data, page, 0, 0, bmp.Width, bmp.Height, 0, 0);

    // ビットマップを表示する処理 (フォーム上のキャンバスに描画)
    Form1.Image1.Canvas.Draw(0, 0, bmp);
  finally
    bmp.Free;
    FPDF_ClosePage(page);
    FPDF_CloseDocument(doc);

  end;
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
     ShowPDF(OpenDialog1.FileName);
end;

procedure TForm1.Image1Click(Sender: TObject);
begin

end;

end.

