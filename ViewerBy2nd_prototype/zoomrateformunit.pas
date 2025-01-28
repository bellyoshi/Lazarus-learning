unit ZoomRateFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ViewerModel;

type

  { TZoomRateForm }

  TZoomRateForm = class(TForm)
    ZoomRateTextBox: TEdit;
  private

  public
    procedure Display();
  end;

var
  ZoomRateForm: TZoomRateForm;

implementation

{$R *.lfm}



{ TForm1 }
 procedure TZoomRateForm.Display();
begin
  ZoomRateTextBox.Text := IntToStr(
                       Round(model.Zoom.Rate*100));
  Show();
end;


end.

