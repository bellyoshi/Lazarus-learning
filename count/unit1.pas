unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    LabelCount: TLabel;
    ButtonPlus: TButton;
    ButtonMinus: TButton;
    procedure ButtonPlusClick(Sender: TObject);
    procedure ButtonMinusClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCount: Integer;
    procedure UpdateCountDisplay;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCount := 0;
  UpdateCountDisplay;
end;

procedure TForm1.UpdateCountDisplay;
begin
  LabelCount.Caption := IntToStr(FCount);
end;

procedure TForm1.ButtonPlusClick(Sender: TObject);
begin
  Inc(FCount);
  UpdateCountDisplay;
end;

procedure TForm1.ButtonMinusClick(Sender: TObject);
begin
  Dec(FCount);
  UpdateCountDisplay;
end;

end.

