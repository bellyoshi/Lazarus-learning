unit frmOperation;



interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls ,
  PdfiumCore, PdfiumLib, frmViewer, ViewerModel, ControlFitter, PageFormUnit;

type

  { TOperationForm }

  TOperationForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    PageCountLabel: TLabel;
    NextButton: TButton;
    PreviousButton: TButton;
    Image1: TImage;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure PageCountLabelClick(Sender: TObject);
    procedure PreviousButtonClick(Sender: TObject);

  private
    procedure LoadBitmap;
    procedure SetCtlEnabled();
  public

  end;

var
  OperationForm: TOperationForm;

implementation

{$R *.lfm}

{ TOperationForm }

procedure TOperationForm.SetCtlEnabled();
begin
  Button2.Enabled:=model.HasOperationDocument;
  NextButton.Enabled:= model.CanNext;
  PreviousButton.Enabled := model.CanPrevious;
  PageCountLabel.Caption:= Format('%d / %d', [model.PageIndex + 1, model.PageCount]);
end;

procedure TOperationForm.FormCreate(Sender: TObject);
begin
  {$IFDEF CPUX64}
  //PDFiumDllDir := ExtractFilePath(ParamStr(0)) + 'x64\V8XFA';
  PDFiumDllDir := ExtractFilePath(ParamStr(0)) + 'x64';
  {$ELSE}
  PDFiumDllDir := ExtractFilePath(ParamStr(0)) + 'x86';
  {$ENDIF CPUX64}
    model := TViewerModel.Create;
end;

procedure TOperationForm.NextButtonClick(Sender: TObject);
begin
  model.Next;
  SetCtlEnabled();
  LoadBitmap();
end;

procedure TOperationForm.PageCountLabelClick(Sender: TObject);
begin
  if not model.HasOperationDocument then
  begin
    Exit;
  end;
  PageForm.SetPage(model.PageIndex, model.PageCount);
  PageForm.Show;
end;

procedure TOperationForm.PreviousButtonClick(Sender: TObject);
begin
  model.Previous;
  SetCtlEnabled;
  LoadBitmap;

end;

procedure TOperationForm.LoadBitmap;
var
  Bitmap : TBitmap;
begin

  FitImageSize(Image1, Panel1.Width, Panel1.Height, model.ThumbnailRatio);

  try
    // PDFium ページを Delphi ビットマップに描画
    Bitmap := model.GetThumbnailBitmap(Image1.Width, Image1.Height);
    Image1.Picture.Bitmap.Assign(Bitmap);
  finally
    Bitmap.Free;
  end;

end;

procedure TOperationForm.Button1Click(Sender: TObject);
begin
  // PDFファイルのみを選択できるようにフィルタを設定
  OpenDialog1.Filter := 'PDF Files|*.pdf';

  if not OpenDialog1.Execute then Exit;


  model.Open(OpenDialog1.FileName);
  SetCtlEnabled();
  LoadBitmap;

end;


procedure TOperationForm.Button2Click(Sender: TObject);
begin
  model.View();
  SetCtlEnabled();
  ViewerForm.LoadBitmap();
  ViewerForm.Show();
end;

procedure TOperationForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  model.Free;
end;



end.

