unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    MainMenu1: TMainMenu;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Label1.Caption := TimeToStr(now);

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Form1.Caption:= 'Test01-Clock';
  Label1.Color:= clBlack;
  Label1.Font.Size:= 70;
  Label1.Font.Color:= clAqua;
  Label1.Caption:= '00:00:00';

end;

end.

