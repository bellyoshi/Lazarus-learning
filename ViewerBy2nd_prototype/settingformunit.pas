unit SettingFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, frmViewer;

type

  { TSettingForm }

  TSettingForm = class(TForm)
    OkButton: TButton;
    ComboBox1: TComboBox;
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private

  public

  end;

var
  SettingForm: TSettingForm;

implementation

{$R *.lfm}

{ TSettingForm }
procedure PopulateScreenList(ComboBox: TComboBox);
var
  i: Integer;
begin
  ComboBox.Items.Clear;
  for i := 0 to Screen.MonitorCount - 1 do
  begin
    ComboBox.Items.Add('Screen ' + IntToStr(i + 1) + ' (' +
      IntToStr(Screen.Monitors[i].Width) + 'x' +
      IntToStr(Screen.Monitors[i].Height) + ')');
  end;
  ComboBox.ItemIndex := 0; // デフォルトで最初のスクリーンを選択
end;

procedure TSettingForm.FormCreate(Sender: TObject);
begin
  PopulateScreenList(ComboBox1); // フォーム作成時にスクリーン一覧を表示
end;

procedure TSettingForm.ComboBox1Change(Sender: TObject);
var
  i : Integer;
  display : TMonitor;
begin
  i := ComboBox1.ItemIndex;
  if (0 <= i) and (i < Screen.MonitorCount) then
  begin
    display := Screen.Monitors[i];
    ViewerForm.Top := display.Top;
    ViewerForm.Left:= display.Left;
    ViewerForm.Width:=display.Width;
    ViewerForm.Height:=display.Height;
  end;
end;

procedure TSettingForm.OkButtonClick(Sender: TObject);
begin
  Close();
end;

end.

