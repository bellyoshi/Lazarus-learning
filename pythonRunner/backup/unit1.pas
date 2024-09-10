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
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

