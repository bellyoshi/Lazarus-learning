unit PageFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ViewerModel
  ,IViewUnit;

type

  { TForm3 }

  { TPageForm }

  TPageForm = class(TForm)

    CancelButton: TButton;
    OkButton: TButton;
    Label1: TLabel;
    PageIndexEdit: TEdit;
    PageCountLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure Display();

  end;

var
  PageForm: TPageForm;

implementation

procedure TPageForm.Display();
begin
  PageCountLabel.Caption := IntToStr(model.PageCount);
  PageIndexEdit.Caption:= IntToStr(model.PageIndex + 1);
  Show();
end;

procedure TPageForm.OkButtonClick(Sender: TObject);
var
  str : String;
  i : Integer;
begin
  str:= PageIndexEdit.Caption;
  if not TryStrToInt(str, i) then
  begin
    ShowMessage('数字を入力してください');
    Exit;
  end;

  model.PageIndex := i - 1;
  formManager.Update;
  Close;


end;

procedure TPageForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;



{$R *.lfm}



end.

