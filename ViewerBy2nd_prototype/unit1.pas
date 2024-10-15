unit Unit1;



interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, PdfiumCtrl,PdfiumCore;

type

  { TForm1 }

  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
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
end;

end.

