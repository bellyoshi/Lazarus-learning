unit UFormController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type
  { TFormController }
  TFormController = class
  private
    FMainForm: TForm;
  public
    procedure RegisterForm(AForm: TForm);
    procedure ShowForm;
  end;

var
  FormController: TFormController;

implementation

procedure TFormController.RegisterForm(AForm: TForm);
begin
  FMainForm := AForm;
end;

procedure TFormController.ShowForm;
begin
  if Assigned(FMainForm) then
  begin
    FMainForm.Show;
    FMainForm.BringToFront;
  end;
end;

initialization
  FormController := TFormController.Create;

finalization
  FreeAndNil(FormController);

end.

