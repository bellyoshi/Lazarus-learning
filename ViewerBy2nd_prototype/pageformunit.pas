unit PageFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm3 }

  TPageForm = class(TForm)
    CancelButton: TButton;
    OkButton: TButton;
    Label1: TLabel;
    PageIndexEdit: TEdit;
    PageCountLabel: TLabel;
  private

  public

  end;

var
  PageForm: TPageForm;

implementation

{$R *.lfm}

end.

