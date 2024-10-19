unit Unit2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus;

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
  public
    procedure SetBitmap(Bitmap: TBitmap);
    property IsFullScreen: Boolean read FIsFullScreen write SetIsFullScreen;

  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}
procedure TForm2.FormCreate(Sender: TObject);
begin
  // Image1のStretchプロパティをTrueに設定して画像が伸縮するようにする
  Image1.Stretch := True;
end;

procedure TForm2.FormResize(Sender: TObject);
begin
  // フォームのクライアント領域にImage1をフィットさせる
  Image1.Top:= 0;
  Image1.Left:= 0;
  Image1.Width := ClientWidth;
  Image1.Height := ClientHeight;
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
  PopupMenu1.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
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



procedure TForm2.SetBitmap(Bitmap : TBitmap);
  begin
  if Assigned(Bitmap) then
  begin
    Image1.Picture.Bitmap.Assign(Bitmap);
  end;
end;
end.

