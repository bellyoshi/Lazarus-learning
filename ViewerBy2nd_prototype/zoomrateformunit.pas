unit ZoomRateFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ViewerModel, ZoomUnit
  ,IViewUnit;

type

  { TZoomRateForm }

  TZoomRateForm = class(TForm)
    CancelButton: TButton;
    OkButton: TButton;
    ZoomRateTextBox: TEdit;
    procedure CancelButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure ZoomRateTextBoxChange(Sender: TObject);
    procedure ZoomRateTextBoxKeyPress(Sender: TObject; var Key: char);
  private

  public
    procedure Display();
  end;

var
  ZoomRateForm: TZoomRateForm;

implementation

{$R *.lfm}


procedure TZoomRateForm.OkButtonClick(Sender: TObject);
var
  rawInput: Double;
  percentageValue: Double;
begin
  if not TryStrToFloat(ZoomRateTextBox.Text, rawInput) then
  begin
    ShowMessage('有効な数値を入力してください。');
    Exit;
  end;
  
  // Convert percentage to decimal
  percentageValue := rawInput / 100;
  
  if (percentageValue < DEFAULT_ZOOM_RATE) or (percentageValue > MAX_ZOOM_RATE) then
  begin
    ShowMessage('ズーム率は ' + FloatToStr(DEFAULT_ZOOM_RATE * 100) + '% から ' + FloatToStr(MAX_ZOOM_RATE * 100) + '% の間で入力してください。');
    Exit;
  end;
  
  model.Zoom.Rate := percentageValue;
  formManager.Update;
  Close;
end;

procedure TZoomRateForm.ZoomRateTextBoxChange(Sender: TObject);
begin

end;

procedure TZoomRateForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

{ TForm1 }
 procedure TZoomRateForm.Display();
begin
  ZoomRateTextBox.Text := IntToStr(
                       Round(model.Zoom.Rate*100));
  Show();
end;

procedure TZoomRateForm.ZoomRateTextBoxKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then // Enter key
  begin
    Key := #0; // Prevent the default beep
    OkButtonClick(Sender);
  end;
end;

end.

