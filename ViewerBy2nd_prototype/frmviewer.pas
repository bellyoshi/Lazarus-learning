unit frmViewer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
   PdfImageRepository;

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
    procedure SetIsFullScreen(Value: Boolean);
    procedure StretchImage();
    procedure SetBitmap(Bitmap: TBitmap);
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
  HasBitmap := False;

end;

function RoundToStep(Value: Integer; Step: Integer): Integer;
begin
  Result := Round(Value / Step) * Step;
end;

function RoundWidthToStep(Value: Integer) : Integer;
const
  PIXEL_STEP = 8;  // widthを8の倍数にしなければLinux環境ですじがでる。
begin
  Result := RoundToStep(Value, PIXEL_STEP);
end;

procedure TForm2.StretchImage();
var
  formRatio : Double;
  NewWidth, NewHeight: Integer;
  Bitmap : TBitmap;
begin
  if not HasBitmap then Exit;
  formRatio := ClientWidth / ClientHeight;

  if formRatio > repository.ViewRatio then
  begin
    // 縦が基準
    NewHeight := ClientHeight;
    NewWidth := RoundWidthToStep(Round(NewHeight * repository.ViewRatio));
  end
  else
  begin
    // 横が基準
    NewWidth := RoundWidthToStep(ClientWidth);
    NewHeight := Round(NewWidth / repository.ViewRatio);
  end;


  // フォームのクライアント領域にImage1をフィットさせる
  Image1.Width := NewWidth;
  Image1.Height := NewHeight;

  // 中央に配置
  Image1.Left := (ClientWidth - NewWidth) div 2;
  Image1.Top := (ClientHeight - NewHeight) div 2;

  try
    // PDFium ページを Delphi ビットマップに描画
    Bitmap := repository.GetViewBitmap(NewWidth, NewHeight);
    SetBitmap(Bitmap);
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
      HasBitmap := True;
      StretchImage;
end;

procedure TForm2.SetBitmap(Bitmap : TBitmap);

begin

  Image1.Picture.Bitmap.Assign(Bitmap);


end;

end.

