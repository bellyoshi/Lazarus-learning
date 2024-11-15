unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LCLType, StdCtrls,
  Windows;

type
  TResizeDirection = (rdNone, rdLeft, rdRight, rdTop, rdBottom, rdTopLeft, rdTopRight, rdBottomLeft, rdBottomRight);

  { TForm1 }

  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FDragging: Boolean;
    FResizing: Boolean;
    FResizeDirection: TResizeDirection;
    FDragStartPosX: Integer;
    FDragStartPosY: Integer;
    procedure UpdateResizeDirection(X, Y: Integer);
    procedure PerformResize(X, Y: Integer);
    procedure Log(message :string);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

const
  RESIZE_MARGIN = 10; // サイズ変更エリアのマージン

procedure TForm1.Log(message :string);
begin
  Memo1.Lines.Insert(0, message);
end;

procedure TForm1.UpdateResizeDirection(X, Y: Integer);
begin
  if (X <= RESIZE_MARGIN) and (Y <= RESIZE_MARGIN) then
    FResizeDirection := rdTopLeft
  else if (X >= ClientWidth - RESIZE_MARGIN) and (Y <= RESIZE_MARGIN) then
    FResizeDirection := rdTopRight
  else if (X <= RESIZE_MARGIN) and (Y >= ClientHeight - RESIZE_MARGIN) then
    FResizeDirection := rdBottomLeft
  else if (X >= ClientWidth - RESIZE_MARGIN) and (Y >= ClientHeight - RESIZE_MARGIN) then
    FResizeDirection := rdBottomRight
  else if (X <= RESIZE_MARGIN) then
    FResizeDirection := rdLeft
  else if (X >= ClientWidth - RESIZE_MARGIN) then
    FResizeDirection := rdRight
  else if (Y <= RESIZE_MARGIN) then
    FResizeDirection := rdTop
  else if (Y >= ClientHeight - RESIZE_MARGIN) then
    FResizeDirection := rdBottom
  else
    FResizeDirection := rdNone;
end;

const
  MIN_FORM_SIZE = 100;   // フォームの最小サイズ（幅と高さ）

procedure TForm1.PerformResize(X, Y: Integer);
var
  DeltaX, DeltaY: Integer;
begin

  DeltaX := X - FDragStartPosX;
  DeltaY := Y - FDragStartPosY;

  case FResizeDirection of
    rdLeft:
      begin
      if Width - DeltaX >= MIN_FORM_SIZE then
      begin
        Left := Left + DeltaX;
        Width := Width - DeltaX;

      end;
      FDragStartPosX := 0;
      FDragStartPosY := Y;
      end;
    rdRight:
      begin
        if Width + DeltaX >= MIN_FORM_SIZE then
          Width := Width + DeltaX;
        FDragStartPosX := X;
        FDragStartPosY := Y;
      end;
    rdTop:
      if Height - DeltaY >= MIN_FORM_SIZE then
      begin
        Top := Top + DeltaY;
        Height := Height - DeltaY;
        FDragStartPosX := X;
        FDragStartPosY := 0;
      end;

    rdBottom:
      begin
      if Height + DeltaY >= MIN_FORM_SIZE then
        Height := Height + DeltaY;
        FDragStartPosX := X;
        FDragStartPosY := Y
      end;
    rdTopLeft:
      begin
        if Width - DeltaX >= MIN_FORM_SIZE then
        begin
          Left := Left + DeltaX;
          Width := Width - DeltaX;
        end;
        if Height - DeltaY >= MIN_FORM_SIZE then
        begin
          Top := Top + DeltaY;
          Height := Height - DeltaY;
        end;
          FDragStartPosX := 0;
          FDragStartPosY := 0;
      end;

    rdTopRight:
      begin
        if Width + DeltaX >= MIN_FORM_SIZE then
          Width := Width + DeltaX;
        if Height - DeltaY >= MIN_FORM_SIZE then
        begin
          Top := Top + DeltaY;
          Height := Height - DeltaY;
        end;
          FDragStartPosX := Width;
          FDragStartPosY := 0;
      end;

    rdBottomLeft:
      begin
        if Width - DeltaX >= MIN_FORM_SIZE then
        begin
          Left := Left + DeltaX;
          Width := Width - DeltaX;
        end;
        if Height + DeltaY >= MIN_FORM_SIZE then
          Height := Height + DeltaY;
        FDragStartPosX := 0;
        FDragStartPosY := Height;
      end;

    rdBottomRight:
      begin
        if Width + DeltaX >= MIN_FORM_SIZE then
          Width := Width + DeltaX;
        if Height + DeltaY >= MIN_FORM_SIZE then
          Height := Height + DeltaY;
        FDragStartPosX := Width;
        FDragStartPosY := Height;
      end;
    else
      begin
              FDragStartPosX := X;
      FDragStartPosY := Y;
      end;

  end;


end;


procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    UpdateResizeDirection(X, Y);
    if FResizeDirection <> rdNone then
    begin
      FResizing := True;
      FDragStartPosX := X;
      FDragStartPosY := Y;
    end
    else
    begin
      FDragging := True;
      FDragStartPosX := X;
      FDragStartPosY := Y;
    end;
  end;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if FResizing then
  begin
    PerformResize(X, Y);
  end
  else if FDragging then
  begin
    Left := Left + (X - FDragStartPosX);
    Top := Top + (Y - FDragStartPosY);
  end
  else
  UpdateResizeDirection(X, Y);
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  FResizing := False;
  FDragStartPosY := Y;
  FDragStartPosX := X;
end;

end.

