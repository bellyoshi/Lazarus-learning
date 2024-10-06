unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Unit2;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  Form2 : TForm2;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);

begin
  Form2.BorderStyle := bsNone;     // ボーダーを非表示
  Form2.WindowState := wsMaximized; // 最大化
  Form2.Show;                      // フォームを表示

end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

end.

