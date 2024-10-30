unit PageFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ViewerModel;

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
    FPageCount : Integer;
    FPageIndex : Integer;
  public
    procedure SetPage(PageIndex,PageCount : Integer);

  end;

var
  PageForm: TPageForm;

implementation

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

  model.PageIndex := i;
  Close;
  //todo update main forms

end;

procedure TPageForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TPageForm.SetPage(PageIndex, PageCount : Integer);
begin
  FPageIndex:= PageIndex;
  FPageCount:= PageCount;
  PageCountLabel.Caption := IntToStr(PageCount);
  PageIndexEdit.Caption:= IntToStr(PageIndex);
end;

{$R *.lfm}



end.

