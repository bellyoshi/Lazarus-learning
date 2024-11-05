unit frmViewer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
   ViewerModel, ControlFitter, FormSizeCustomizerUnit;

type

  { TViewerForm }

  TViewerForm = class(TForm)
    Image1: TImage;
    MenuItemFullScreen: TMenuItem;
    MenuItemWindowMode: TMenuItem;
    MenuItemClose: TMenuItem;
    PopupMenu1: TPopupMenu;

    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);

    procedure FormContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);

    procedure MenuItemCloseClick(Sender: TObject);
    procedure MenuItemFullScreenClick(Sender: TObject);
    procedure MenuItemWindowModeClick(Sender: TObject);

  private
    FIsFullScreen: Boolean;
    procedure SetIsFullScreen(Value: Boolean);
    procedure LoadBitmap();
  public
    property IsFullScreen: Boolean read FIsFullScreen write SetIsFullScreen;
    procedure ShowDocument();
  end;

var
  ViewerForm: TViewerForm;

implementation

{$R *.lfm}

procedure TViewerForm.ShowDocument();
begin
    model.View();
    LoadBitmap();
    Show();
end;


procedure TViewerForm.FormCreate(Sender: TObject);
begin
  FormSizeCustomizer := TFormSizeCustomizer.Create;
  FormSizeCustomizer.RegistForm(ViewerForm);
end;




procedure TViewerForm.LoadBitmap();
var
  Bitmap: TBitmap;
begin
  if not model.HasViewDocument then Exit;

  FitImageSize(Image1, ClientWidth, ClientHeight, model.ViewRatio);

  try
    // PDFium ページを Delphi ビットマップに描画
    Bitmap := model.GetViewBitmap(Image1.Width, Image1.Height);
    Image1.Picture.Bitmap.Assign(Bitmap);
  finally
    Bitmap.Free;
  end;
end;

procedure TViewerForm.FormResize(Sender: TObject);
begin
  LoadBitmap();
end;

procedure TViewerForm.SetIsFullScreen(Value: Boolean);
begin
  FormSizeCustomizer.IsFullScreen := Value;
end;

procedure TViewerForm.FormContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  PopupMenu1.PopUp(MousePos.X, MousePos.Y);
  Handled := True;  // イベントを処理済みにする
end;

procedure TViewerForm.MenuItemFullScreenClick(Sender: TObject);
begin
  IsFullScreen := True;  // フルスクリーンに切り替える
end;

procedure TViewerForm.MenuItemWindowModeClick(Sender: TObject);
begin
  IsFullScreen := False;  // ウインドウモードに切り替える
end;



procedure TViewerForm.MenuItemCloseClick(Sender: TObject);
begin
  Close;  // フォームを閉じる
end;

end.

