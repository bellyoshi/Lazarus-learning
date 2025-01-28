program viewer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frmOperation, frmViewer, ViewerModel, PageFormUnit, SettingFormUnit,
  IViewUnit, AboutUnit, SettingLoaderUnit, ZoomRateFormUnit;

{$R *.res}

//TODO: 設定ファイルの読み込み
//TODO: 設定の初期化
//TODO: 設定からファイルリストの読み込み
//TODO: 設定からファイルリストの書き込み
begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;

  SettingLoader := TSettingLoader.Create;
  SettingLoader.Load;

  Application.CreateForm(TOperationForm, OperationForm);
  Application.CreateForm(TViewerForm, ViewerForm);
  Application.CreateForm(TPageForm, PageForm);
  Application.CreateForm(TSettingForm, SettingForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TZoomRateForm, ZoomRateForm);

  formManager := TFormManager.Create;
  formManager.RegisterView(ViewerForm);
  formManager.RegisterView(OperationForm);

  ViewerForm.Show();
  SettingLoader.ApplySettings;

  Application.Run;
end.

