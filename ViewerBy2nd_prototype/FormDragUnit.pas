unit FormDragUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, LCLType;

type
  TResizeDirection = (rdNone, rdLeft, rdRight, rdTop, rdBottom, rdTopLeft, rdTopRight, rdBottomLeft, rdBottomRight);

  TFormDrag = class
  private
    FForm: TForm;
    FDragging: Boolean;
    FResizing: Boolean;
    FResizeDirection: TResizeDirection;
    FDragStartPosX: Integer;
    FDragStartPosY: Integer;
    const
      RESIZE_MARGIN = 10;
      MIN_FORM_SIZE = 100;
    procedure UpdateResizeDirection(X, Y: Integer);
    procedure PerformResize(X, Y: Integer);
  public
    constructor Create(AForm: TForm);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  end;

implementation

{ TFormDrag }

constructor TFormDrag.Create(AForm: TForm);
begin
  FForm := AForm;
  FDragging := False;
  FResizing := False;

  // イベントを設定
  FForm.OnMouseDown := @FormMouseDown;
  FForm.OnMouseMove := @FormMouseMove;
  FForm.OnMouseUp := @FormMouseUp;
end;

procedure TFormDrag.UpdateResizeDirection(X, Y: Integer);
begin
  if (X <= RESIZE_MARGIN) and (Y <= RESIZE_MARGIN) then
    FResizeDirection := rdTopLeft
  else if (X >= FForm.ClientWidth - RESIZE_MARGIN) and (Y <= RESIZE_MARGIN) then
    FResizeDirection := rdTopRight
  else if (X <= RESIZE_MARGIN) and (Y >= FForm.ClientHeight - RESIZE_MARGIN) then
    FResizeDirection := rdBottomLeft
  else if (X >= FForm.ClientWidth - RESIZE_MARGIN) and (Y >= FForm.ClientHeight - RESIZE_MARGIN) then
    FResizeDirection := rdBottomRight
  else if (X <= RESIZE_MARGIN) then
    FResizeDirection := rdLeft
  else if (X >= FForm.ClientWidth - RESIZE_MARGIN) then
    FResizeDirection := rdRight
  else if (Y <= RESIZE_MARGIN) then
    FResizeDirection := rdTop
  else if (Y >= FForm.ClientHeight - RESIZE_MARGIN) then
    FResizeDirection := rdBottom
  else
    FResizeDirection := rdNone;
end;

procedure TFormDrag.PerformResize(X, Y: Integer);
var
  DeltaX, DeltaY: Integer;
  Width : Integer;
  Height : Integer;
  Left : Integer;
  Top : Integer;
begin

  DeltaX := X - FDragStartPosX;
  DeltaY := Y - FDragStartPosY;

  Width := FForm.Width;
  Height := FForm.Height;
  Left := FForm.Left;
  Top := FForm.Top;

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
  
  FForm.Width := Width;
  FForm.Height := Height;
  FForm.Left := Left;
  FForm.Top := Top;

end;

procedure TFormDrag.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    UpdateResizeDirection(X, Y);
    if FResizeDirection <> rdNone then
    begin
      FResizing := True;
    end
    else
    begin
      FDragging := True;
    end;
    FDragStartPosX := X;
    FDragStartPosY := Y;
  end;
end;

procedure TFormDrag.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if FResizing then
    PerformResize(X, Y)
  else if FDragging then
  begin
    FForm.Left := FForm.Left + (X - FDragStartPosX);
    FForm.Top := FForm.Top + (Y - FDragStartPosY);
  end;
end;

procedure TFormDrag.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  FResizing := False;
end;

end.
