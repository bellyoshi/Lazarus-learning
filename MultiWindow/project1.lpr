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
  Forms, unit1, unit2
  { you can add units after this };

{$R *.res}


begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.Title:= 'My Application desu';
  Application.CreateForm(TForm2, Form2);

    Form1a := TForm1.Create(nil);
    Form1b := TForm1.Create(nil);
    Form1a.Show;
    Form1b.show();
  Application.Run;
end.

