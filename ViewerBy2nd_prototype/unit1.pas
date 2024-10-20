unit Unit1;



interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls ,
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


      Form2.SetPage(page);
      Form2.Show();

  finally
 //   pdfDocument.Free;
  end;
end;



end.

