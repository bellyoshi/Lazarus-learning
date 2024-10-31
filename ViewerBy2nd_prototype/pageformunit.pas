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

  private
    FView : IView;
    procedure SetView(value : IView);
  public
    property  View : IView write SetView;
  end;

var
  PageForm: TPageForm;

implementation

procedure TPageForm.SetView(value: IView);
begin
  FView := Value;
  PageCountLabel.Caption := IntToStr(model.PageCount);
  PageIndexEdit.Caption:= IntToStr(model.PageIndex + 1);
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
  FView.UpdateView;
  Close;


end;

procedure TPageForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;



{$R *.lfm}



end.

