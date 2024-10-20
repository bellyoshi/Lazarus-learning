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
    HasBitmap : Boolean;
    FIsFullScreen: Boolean;
    Ratio: Double;
    procedure SetIsFullScreen(Value: Boolean);
    procedure StretchImage();

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
  HasBitmap := False;

end;

procedure TForm2.StretchImage();
var
  formRatio : Double;
  NewWidth, NewHeight: Integer;
begin
  if not HasBitmap then Exit;
  formRatio := ClientWidth / ClientHeight;
  if formRatio > Ratio then
  begin
    // 縦が基準
    NewHeight := ClientHeight;
    NewWidth := Round(NewHeight * Ratio);
  end
  else
  begin
    // 横が基準
    NewWidth := ClientWidth;
    NewHeight := Round(NewWidth / Ratio);
  end;

  // Stretchしておく
  Image1.Stretch := True;
  // フォームのクライアント領域にImage1をフィットさせる
  Image1.Width := NewWidth;
  Image1.Height := NewHeight;

  // 中央に配置
  Image1.Left := (ClientWidth - NewWidth) div 2;
  Image1.Top := (ClientHeight - NewHeight) div 2;

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
    HasBitmap := True;
    StretchImage;
        // 画像のアスペクト比を計算
    Ratio := Bitmap.Width / Bitmap.Height;
  end;
end;
end.

