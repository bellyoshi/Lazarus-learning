unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Display: TEdit;
    Btn0: TButton;
    Btn1: TButton;
    Btn2: TButton;
    Btn3: TButton;
    Btn4: TButton;
    Btn5: TButton;
    Btn6: TButton;
    Btn7: TButton;
    Btn8: TButton;
    Btn9: TButton;
    BtnPlus: TButton;
    BtnEquals: TButton;
    BtnClear: TButton;
    procedure NumberButtonClick(Sender: TObject);
    procedure BtnPlusClick(Sender: TObject);
    procedure BtnEqualsClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
  private
    FCurrentValue: Double;
    FStoredValue: Double;
    FOperation: String;
    FNewNumber: Boolean;
    procedure UpdateDisplay;
    procedure ProcessNumber(const NumStr: String);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.UpdateDisplay;
begin
  Display.Text := FloatToStr(FCurrentValue);
end;

procedure TForm1.ProcessNumber(const NumStr: String);
begin
  if FNewNumber then
  begin
    FCurrentValue := 0;
    FNewNumber := False;
  end;
  
  if FCurrentValue = 0 then
    FCurrentValue := StrToFloat(NumStr)
  else
    FCurrentValue := StrToFloat(FloatToStr(FCurrentValue) + NumStr);
  
  UpdateDisplay;
end;

procedure TForm1.NumberButtonClick(Sender: TObject);
var
  NumStr: String;
begin
  NumStr := (Sender as TButton).Caption;
  ProcessNumber(NumStr);
end;

procedure TForm1.BtnPlusClick(Sender: TObject);
begin
  if FOperation <> '' then
  begin
    // 前の演算を実行
    FStoredValue := FStoredValue + FCurrentValue;
  end
  else
  begin
    FStoredValue := FCurrentValue;
  end;
  
  FOperation := '+';
  FNewNumber := True;
  FCurrentValue := 0;
end;

procedure TForm1.BtnEqualsClick(Sender: TObject);
begin
  if FOperation = '+' then
  begin
    FCurrentValue := FStoredValue + FCurrentValue;
    FOperation := '';
    FStoredValue := 0;
    FNewNumber := True;
    UpdateDisplay;
  end;
end;

procedure TForm1.BtnClearClick(Sender: TObject);
begin
  FCurrentValue := 0;
  FStoredValue := 0;
  FOperation := '';
  FNewNumber := True;
  UpdateDisplay;
end;

end.

