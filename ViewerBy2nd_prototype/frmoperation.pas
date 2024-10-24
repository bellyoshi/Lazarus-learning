unit frmOperation;



interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls ,
  PdfiumCore, PdfiumLib, frmViewer, PdfImageUnit;

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

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // PDFファイルのみを選択できるようにフィルタを設定
  OpenDialog1.Filter := 'PDF Files|*.pdf';

  if not OpenDialog1.Execute then Exit;

  pdfImageCreator := TPdfImageCreator.Create(OpenDialog1.FileName);


  Form2.SetPage();
  Form2.Show();
 //   pdfDocument.Free;

end;



end.

