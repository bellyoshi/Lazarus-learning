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
begin
  DeltaX := X - FDragStartPosX;
  DeltaY := Y - FDragStartPosY;

  case FResizeDirection of
    rdLeft:
      if FForm.Width - DeltaX >= MIN_FORM_SIZE then
      begin
        FForm.Left := FForm.Left + DeltaX;
        FForm.Width := FForm.Width - DeltaX;
      end;
    rdRight:
      if FForm.Width + DeltaX >= MIN_FORM_SIZE then
        FForm.Width := FForm.Width + DeltaX;
    rdTop:
      if FForm.Height - DeltaY >= MIN_FORM_SIZE then
      begin
        FForm.Top := FForm.Top + DeltaY;
        FForm.Height := FForm.Height - DeltaY;
      end;
    rdBottom:
      if FForm.Height + DeltaY >= MIN_FORM_SIZE then
        FForm.Height := FForm.Height + DeltaY;
    rdTopLeft:
      begin
        if FForm.Width - DeltaX >= MIN_FORM_SIZE then
        begin
          FForm.Left := FForm.Left + DeltaX;
          FForm.Width := FForm.Width - DeltaX;
        end;
        if FForm.Height - DeltaY >= MIN_FORM_SIZE then
        begin
          FForm.Top := FForm.Top + DeltaY;
          FForm.Height := FForm.Height - DeltaY;
        end;
      end;
    rdTopRight:
      begin
        if FForm.Width + DeltaX >= MIN_FORM_SIZE then
          FForm.Width := FForm.Width + DeltaX;
        if FForm.Height - DeltaY >= MIN_FORM_SIZE then
        begin
          FForm.Top := FForm.Top + DeltaY;
          FForm.Height := FForm.Height - DeltaY;
        end;
      end;
    rdBottomLeft:
      begin
        if FForm.Width - DeltaX >= MIN_FORM_SIZE then
        begin
          FForm.Left := FForm.Left + DeltaX;
          FForm.Width := FForm.Width - DeltaX;
        end;
        if FForm.Height + DeltaY >= MIN_FORM_SIZE then
          FForm.Height := FForm.Height + DeltaY;
      end;
    rdBottomRight:
      begin
        if FForm.Width + DeltaX >= MIN_FORM_SIZE then
          FForm.Width := FForm.Width + DeltaX;
        if FForm.Height + DeltaY >= MIN_FORM_SIZE then
          FForm.Height := FForm.Height + DeltaY;
      end;
  end;

  FDragStartPosX := X;
  FDragStartPosY := Y;
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
