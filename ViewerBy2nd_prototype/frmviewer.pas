unit frmViewer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
   ViewerModel, FormSizeCustomizerUnit, IViewUnit;

type

  { TViewerForm }

  TViewerForm = class(TForm, IView)
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
    procedure UpdateView();
  end;

var
  ViewerForm: TViewerForm;

implementation

{$R *.lfm}

procedure TViewerForm.ShowDocument();
begin
    model.View();
    UpdateView();
    Show();
end;

procedure TViewerForm.UpdateView();
begin
  LoadBitmap();
  Self.Color:=model.Background.Color;
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


  try
    // PDFium ページを Delphi ビットマップに描画
    Bitmap := model.GetViewBitmap(ClientWidth, ClientHeight);
    Image1.Width := Bitmap.Width;
    Image1.Height := Bitmap.Height;
    Image1.Left := (ClientWidth - Bitmap.Width) div 2;
    Image1.Top := (ClientHeight - Bitmap.Height) div 2;

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

