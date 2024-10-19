unit Unit1;



interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, PdfiumCtrl,
  PdfiumCore, PdfiumLib, unit2;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCtrl: TPdfControl;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IFDEF CPUX64}
  //PDFiumDllDir := ExtractFilePath(ParamStr(0)) + 'x64\V8XFA';
  PDFiumDllDir := ExtractFilePath(ParamStr(0)) + 'x64';
  {$ELSE}
  PDFiumDllDir := ExtractFilePath(ParamStr(0)) + 'x86';
  {$ENDIF CPUX64}

  {
  FCtrl := TPdfControl.Create(Self);
    FCtrl.Align := alClient;
  FCtrl.Parent := Self;
  FCtrl.SendToBack; // put the control behind the buttons
  FCtrl.Color := clGray;
  //FCtrl.Color := clWhite;
  //FCtrl.PageBorderColor := clBlack;
  //FCtrl.PageShadowColor := clDkGray;
  FCtrl.ScaleMode := smFitWidth;
  //FCtrl.PageColor := RGB(255, 255, 200);
//  FCtrl.OnWebLinkClick := WebLinkClick; // disabled due to loTreatWebLinkAsUriAnnotationLink + loAutoOpenURI
//  FCtrl.OnAnnotationLinkClick := AnnotationLinkClick;
  FCtrl.LinkOptions := FCtrl.LinkOptions - [loAutoOpenURI] {+ cPdfControlAllAutoLinkOptions};
//  FCtrl.OnPrintDocument := PrintDocument;
  if FileExists(ParamStr(1)) then
  begin
    FCtrl.LoadFromFile(ParamStr(1));
  end
  else if OpenDialog1.Execute() then
  begin
    FCtrl.LoadFromFile(OpenDialog1.FileName);
  end
  else
  begin
    Application.ShowMainForm := False;
    Application.Terminate;
  end;
  }
end;



procedure LoadBitmapFromRawImage(Bitmap: TBitmap; const AIMarge: TBytes; Width, Height: Integer);
var
  Y, RowSize: Integer;
  SrcPtr: PByte;
  DestPtr: Pointer;
begin
  // ビットマップのサイズとピクセルフォーマットを設定
  Bitmap.Width := Width;
  Bitmap.Height := Height;
  Bitmap.PixelFormat := pf32bit; // 32bit (RGBA) の画像データ

  // 1行あたりのデータサイズ (4バイト/ピクセル * 幅)
  RowSize := Width * 4;

  // 生データの先頭ポインタ
  SrcPtr := @AIMarge[0];

  // 行ごとにデータをコピー
  for Y := 0 to Height - 1 do
  begin
    // 現在の行の先頭のメモリポインタを取得
    DestPtr := Bitmap.ScanLine[Y];
    // 1行分のデータをコピー
    Move(SrcPtr^, DestPtr^, RowSize);
    // 次の行に進む
    Inc(SrcPtr, RowSize);
  end;
end;


procedure DrawToBitmap(Page: TPdfPage; Bitmap: TBitmap);
var
  w, h, SizeInt: Integer;
  PdfBitmap: TPdfBitmap;
  AIMarge: TBytes;
  buffer: Pointer;
begin
  // ページの幅と高さを取得
  w := Trunc(Page.Width);
  h := Trunc(Page.Height);

  // PDFium ビットマップを作成
  PdfBitmap := TPdfBitmap.Create(w, h, bfBGRA);
  try
    PdfBitmap.FillRect(0, 0, w, h, $00FF00); // 背景を緑色で塗りつぶし
    Page.DrawToPdfBitmap(PdfBitmap, 0, 0, w, h);

    // PDFium ビットマップのバッファサイズを計算
    SizeInt := w * h * 4; // 4バイト/ピクセル (RGBA)

    // バイト配列を初期化
    SetLength(AIMarge, SizeInt);

    // PDFium のバッファを取得して、バイト配列にコピー
    buffer := PdfBitmap.GetBuffer;
    Move(buffer^, AIMarge[0], SizeInt);

    // バイト配列から Delphi のビットマップにデータをコピー
    LoadBitmapFromRawImage(Bitmap, AIMarge, w, h);
  finally
    PdfBitmap.Free;
  end;
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  pdfDocument: TPdfDocument;
  page: TPdfPage;
  Bitmap: TBitmap;
begin
  // PDFファイルのみを選択できるようにフィルタを設定
  OpenDialog1.Filter := 'PDF Files|*.pdf';

  if not OpenDialog1.Execute then Exit;

  pdfDocument := TPdfDocument.Create;
  try
    pdfDocument.LoadFromFile(OpenDialog1.FileName);

    // 最初のページを取得
    page := pdfDocument.Pages[0];

    // PDFium ページを Delphi ビットマップに描画
    Bitmap := TBitmap.Create;
    try
      DrawToBitmap(page, Bitmap);

      Form2.SetBitmap(Bitmap);
      Form2.Show;
    finally
      Bitmap.Free;
    end;
  finally
    pdfDocument.Free;
  end;
end;



end.

