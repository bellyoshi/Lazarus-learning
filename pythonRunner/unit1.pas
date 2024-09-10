unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus
  ,Process;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnOpen: TButton;
    btnRun: TButton;
    btnClear: TButton;
    Edit1: TEdit;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    procedure btnClearClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnRunClick(Sender: TObject);
var
  pyProcess: TProcess;
  outs: TStringList;
  spyscript: string;
begin
  if Edit1.Text = '' then exit;
  spyscript:= Edit1.Text;
  try
    pyProcess := TProcess.Create(nil);
    pyProcess.Executable := 'python';
    pyProcess.Parameters.Add(spyscript);
    pyProcess.Options := pyProcess.Options + [poWaitOnExit, poUsePipes];
    pyProcess.Execute; // run python
    outs := TStringList.Create;
    outs.LoadFromStream(pyProcess.Output);
    Memo1.Lines:= outs;
  finally
    pyProcess.Free;
    outs.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Form1.Caption:= 'Test05-Process';
  Edit1.Clear;
  Memo1.Clear;
  OpenDialog1.DefaultExt:='.py';
end;

procedure TForm1.btnClearClick(Sender: TObject);
begin
  Memo1.Clear;
  Edit1.Clear;
end;

procedure TForm1.btnOpenClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  Edit1.Text:= OpenDialog1.FileName;


end;

end.

