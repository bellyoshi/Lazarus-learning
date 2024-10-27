unit frmViewer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
   ViewerModel, ControlFitter;

type

  { TForm2 }

  TForm2 = class(TForm)
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
    procedure StretchImage();

  public
    procedure SetPage();
    property IsFullScreen: Boolean read FIsFullScreen write SetIsFullScreen;

  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}





procedure TForm2.FormCreate(Sender: TObject);
begin
{todo : initialize}

end;



procedure TForm2.StretchImage();
var
  Bitmap : TBitmap;
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




procedure TForm2.FormResize(Sender: TObject);
begin
  StretchImage;
end;

procedure TForm2.SetIsFullScreen(Value: Boolean);
begin
  if FIsFullScreen = Value then Exit; // 既にフルスクリーン状態なら何もしない

  FIsFullScreen := Value;

  if FIsFullScreen then
  begin
    // フルスクリーンモードに切り替える
    BorderStyle := bsNone;
    WindowState := wsMaximized;
  end
  else
  begin
    // ウィンドウモードに戻す
    BorderStyle := bsSizeable;
    WindowState := wsNormal;
  end;
end;

procedure TForm2.FormContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  PopupMenu1.PopUp(MousePos.X, MousePos.Y);
  Handled := True;  // イベントを処理済みにする
end;




procedure TForm2.MenuItemFullScreenClick(Sender: TObject);
begin
  IsFullScreen := True;  // フルスクリーンに切り替える
end;

procedure TForm2.MenuItemWindowModeClick(Sender: TObject);
begin
  IsFullScreen := False;  // ウインドウモードに切り替える
end;

procedure TForm2.MenuItemCloseClick(Sender: TObject);
begin
  Close;  // フォームを閉じる
end;

procedure TForm2.SetPage();//too: rename LoadBitmap
begin
      StretchImage;
end;


end.

