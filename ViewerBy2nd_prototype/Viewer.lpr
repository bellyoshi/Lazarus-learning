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
  Forms, frmOperation, frmViewer, ViewerModel, PageFormUnit, SettingFormUnit


  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TOperationForm, OperationForm);
  Application.CreateForm(TViewerForm, ViewerForm);
  Application.CreateForm(TPageForm, PageForm);
  Application.CreateForm(TSettingForm, SettingForm);
  Application.Run;
end.

