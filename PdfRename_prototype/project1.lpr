program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unit1, ControlFitter, PdfImageCreator, PdfiumCore, PdfiumLib
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  if ParamCount > 0 then
  begin
    // 最初の引数を取得しファイルをオープン
    Form1.OpenFile(ParamStr(1));
  end;
  Application.Run;
end.

