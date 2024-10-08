unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtDlgs,
  ExtCtrls, ComCtrls,
   LCLType, LCLProc, LCLIntf, IntfGraphics, Math;


type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    SavePictureDialog1: TSavePictureDialog;
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure ScrollBox1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
 img_ori, img_buf: TPicture; pic_flag, conv_flag: boolean;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ScrollBox1Click(Sender: TObject);
begin

end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin

end;

procedure TForm1.Image1Click(Sender: TObject);
begin

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Form1.Width:= 640;
  Form1.Height:= 480;
  Form1.Caption:= 'Test02-image';
  img_ori:= TPicture.Create;

 img_buf:= TPicture.Create;
 pic_flag:= false;
 MainMenu1.Items[0].Items[1].Enabled:= false; // Save Menu
 conv_flag:= false;
 MainMenu1.Items[1].Enabled:= false; // Convert Menu


end;

end.

