unit SettingFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  FormSizeCustomizerUnit,  IViewUnit, ViewerModel;

type

  { TSettingForm }

  TSettingForm = class(TForm)
    ColorDialog1: TColorDialog;
    OkButton: TButton;
    ComboBox1: TComboBox;
    Panel1: TPanel;
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
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
  Panel1.Color:=model.Background.Color;
end;

procedure TSettingForm.ComboBox1Change(Sender: TObject);
var
  i : Integer;
  display : TMonitor;
begin
  i := ComboBox1.ItemIndex;
  FormSizeCustomizer.ScreenIndex:=i;
  FormSizeCustomizer.IsFullScreen := True;
end;

procedure TSettingForm.OkButtonClick(Sender: TObject);
begin
  Close();
end;

procedure TSettingForm.Panel1Click(Sender: TObject);
begin
  if not ColorDialog1.Execute then
  begin
    exit;
  end;
  model.Background.Color := ColorDialog1.Color;
  Panel1.Color:=model.Background.Color;
  formManager.Update;
end;

end.

