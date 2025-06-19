program Viewer;

{$mode ObjFPC}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,

  frmOperation,
  frmViewer,
  ViewerModel,
  RepogitoryUnit,
  FormSizeCustomizerUnit,
  PageFormUnit,
  SettingFormUnit,
  IViewUnit,
  ZoomUnit,
  AboutUnit,
  ZoomRateFormUnit,
  SettingLoaderUnit,
  ViewerBy2ndFileTypes,
  ViewerBy2ndPlayer,
  TMovieImageCreatorUnit,
  UFormController,
  FormDispatcherUnit
  { you can add units after this };

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

  player := TViewerBy2ndPlayer.Create;

  Application.CreateForm(TOperationForm, OperationForm);
  Application.CreateForm(TViewerForm, ViewerForm);
  Application.CreateForm(TPageForm, PageForm);
  Application.CreateForm(TSettingForm, SettingForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TZoomRateForm, ZoomRateForm);

  formManager := TFormManager.Create;
  formManager.RegisterView(ViewerForm);
  formManager.RegisterView(OperationForm);

  FormController.RegisterForm(OperationForm);
  ViewerForm.Show();
  SettingLoader.ApplySettings;

  Application.Run;
end.

