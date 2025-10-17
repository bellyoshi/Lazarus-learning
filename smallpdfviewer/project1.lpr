program project1;

{$mode objfpc}{$H+}

uses

  Interfaces,
  Forms, Unit1, PdfDocument, PdfPage, PdfRenderer, PdfViewer, PdfiumUtils,
  Classes, SysUtils, Windows;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  {$PUSH}{$WARN 5044 OFF}
  Application.MainFormOnTaskbar:=True;
  {$POP}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

