unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure Rotate180(Bitmap: TBitmap);
var
  x, y: Integer;
  TempBitmap: TBitmap;
begin
  // Create a temporary bitmap to store the rotated image
  TempBitmap := TBitmap.Create;
  try
    // Set the size of the temporary bitmap to the rotated dimensions
    TempBitmap.SetSize(Bitmap.Width, Bitmap.Height);
    TempBitmap.PixelFormat := Bitmap.PixelFormat;

    // Copy pixels from the original bitmap to the temporary bitmap, rotating them by 180 degrees
    for y := 0 to Bitmap.Height - 1 do
      for x := 0 to Bitmap.Width - 1 do
        TempBitmap.Canvas.Pixels[Bitmap.Width - 1 - x, Bitmap.Height - 1 - y] := Bitmap.Canvas.Pixels[x, y];

    // Now overwrite the original bitmap with the rotated image
    Bitmap.Assign(TempBitmap);
  finally
    TempBitmap.Free;
  end;
end;

procedure Rotate270(Bitmap: TBitmap);
var
  x, y: Integer;
  TempBitmap: TBitmap;
begin
  // Create a temporary bitmap to store the rotated image
  TempBitmap := TBitmap.Create;
  try
    // Set the size of the temporary bitmap to the rotated dimensions
    TempBitmap.SetSize(Bitmap.Height, Bitmap.Width);
    TempBitmap.PixelFormat := Bitmap.PixelFormat;

    // Copy pixels from the original bitmap to the temporary bitmap, rotating them by 270 degrees (or -90)
    for y := 0 to Bitmap.Height - 1 do
      for x := 0 to Bitmap.Width - 1 do
        TempBitmap.Canvas.Pixels[y, Bitmap.Width - 1 - x] := Bitmap.Canvas.Pixels[x, y];

    // Now overwrite the original bitmap with the rotated image
    Bitmap.Assign(TempBitmap);
  finally
    TempBitmap.Free;
  end;
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  Bitmap : TBitmap;
begin
  OpenDialog1.Execute;
  Bitmap := TBitmap.Create;
  Bitmap.LoadFromFile(OpenDialog1.FileName);
  SaveDialog1.Execute;
  Rotate270(Bitmap);
  Bitmap.SaveToFile(SaveDialog1.FileName);

end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

end.

