unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LCLType, ExtCtrls,
  StdCtrls,PdfImageCreator,ControlFitter ;

type

  { TForm1 }

  TForm1 = class(TForm)
    OpenButton: TButton;
    Panel1: TPanel;
    SrcFileNameLabel: TLabel;
    RenameButton: TButton;
    DestFileNameEdit: TEdit;
    Image1: TImage;
    OpenDialog1: TOpenDialog;


    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure OpenButtonClick(Sender: TObject);
    procedure RenameButtonClick(Sender: TObject);
  private
    FPdfDocument : TPdfImageCreator;
    procedure CloseFile();
  public
    procedure OpenFile(filename: String);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.OpenFile(filename : String);
var
  Bitmap : TBitmap;
begin
  SrcFileNameLabel.Caption:=filename;
  DestFileNameEdit.Caption:=filename;
  FPdfDocument := TPdfImageCreator.Create(filename);
  FitImageSize(Image1, Panel1.Width, Panel1.Height, FPdfDocument.Ratio);
  Bitmap := FPdfDocument.GetBitmap(Image1.Width, Image1.Height);
  Image1.Picture.Bitmap.Assign(Bitmap);
  Bitmap.Free;
end;

procedure TForm1.CloseFile();
begin
  If Assigned(FPdfDocument) then
  begin
    FPdfDocument.Free;
  end;

end;





procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of string
  );
begin
   if Length(FileNames) > 0 then
    OpenFile(FileNames[0]);  // 最初のファイルをオープン
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
     AllowDropFiles:=True;
end;

procedure TForm1.OpenButtonClick(Sender: TObject);
begin

      OpenDialog1.Filter := 'PDF Files|*.PDF';
      if OpenDialog1.Execute then
        OpenFile(OpenDialog1.FileName); // 選択されたファイルをオープン

end;

procedure TForm1.RenameButtonClick(Sender: TObject);
begin
  //Close File
  Self.CloseFile();
  //Rename File
  RenameFile(SrcFileNameLabel.Caption, DestFileNameEdit.Caption);
  //Reopen File
  Self.OpenFile(DestFileNameEdit.Caption);
end;

end.

