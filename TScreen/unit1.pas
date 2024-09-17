unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // モニタが2つ以上接続されているか確認
  if Screen.MonitorCount > 1 then
  begin
    // セカンドモニタにフォームを表示
    Form1.Left := Screen.Monitors[1].Left;
    Form1.Top := Screen.Monitors[1].Top;
    Form1.Width := Screen.Monitors[1].Width;
    Form1.Height := Screen.Monitors[1].Height;
  end
  else
  begin
    // セカンドモニタがない場合はメインモニタに表示
    Form1.Left := Screen.Monitors[0].Left;
    Form1.Top := Screen.Monitors[0].Top;
    Form1.Width := Screen.Monitors[0].Width;
    Form1.Height := Screen.Monitors[0].Height;
  end;
end;

end.

