program project1;

{$mode objfpc}{$H+} {$codepage utf8}  // これを指定しないと日本語は文字化けします
uses
  Classes, SysUtils,                 fppdf; // PDF新規作成用ユニット
var
  pdf: TPDFDocument;
  page: TPDFPage;
  section: TPDFSection;
  fontidx2: integer;
  svName: string;
begin
  svName:= 'sample.pdf';
  pdf:= TPDFDocument.Create(nil);
  try
    pdf.StartDocument;
    section:= pdf.Sections.AddSection;
    // ページを追加
    page:= pdf.Pages.AddPage;
    page.Orientation:= ppoLandscape; // 横向き 縦向きは、ppoPortrait
    page.UnitOfMeasure:= uomPixels;  // 単位ピクセル
    page.PaperType:= ptA4;  // A4サイズ用紙
    section.AddPage(page);
    // フォントファイルパスとフォント名前を指定する必要があります。
    fontidx2:= pdf.AddFont('c:\windows\fonts\yumin.ttf', '游明朝 標準');
    page.SetFont(fontidx2,40); // フォントサイズを40
    page.WriteText(20,200, 'こんにちは LENA 画像さん'); // Windowsフォントを指定

    page.SetColor(clGreen, false); // 緑色
    page.WriteText(100,300, 'Free Pasca new pdf text'); // ライン線を描く
    page.SetColor(clBlack, true); // 黒色
    page.SetPenStyle(ppsSolid, 5);
    page.DrawLine(100, 190, 300, 190, 5);
    page.SetColor(clRed, true); // 赤色
    page.DrawLine(100, 170, 300, 170, 3);
    pdf.SaveToFile(svName); // PDFファイル保存
  finally
    pdf.free;
  end;
end.

