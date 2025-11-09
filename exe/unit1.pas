unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UnitExeGenerator;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  SaveDialog1.Filter := '実行ファイル (*.exe)|*.exe|すべてのファイル (*.*)|*.*';
  SaveDialog1.DefaultExt := 'exe';
  SaveDialog1.FileName := 'hello.exe';
  
  if SaveDialog1.Execute then
  begin
    try
      GenerateHelloWorldExe(SaveDialog1.FileName);
      ShowMessage('PE32+形式のexeファイルを生成しました: ' + SaveDialog1.FileName);
    except
      on E: Exception do
        ShowMessage('エラーが発生しました: ' + E.Message);
    end;
  end;
end;

end.

