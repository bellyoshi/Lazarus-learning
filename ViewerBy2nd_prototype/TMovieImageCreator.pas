unit TMovieImageCreatorUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ImageCreatorUnit;

type
  TMovieImageCreator = class(TInterfacedObject, IDocmentImageCreator)
  private
    FFileName: string;
  public
    constructor Create(const AFileName: string);
    function GetBitmap(Width, Height: Integer): TBitmap;
    function GetRatio(): Double;
    function GetPageIndex: Integer;
    procedure SetPageIndex(AValue: Integer);
    function GetPageCount: Integer;
  end;

implementation

constructor TMovieImageCreator.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
end;

function TMovieImageCreator.GetBitmap(Width, Height: Integer): TBitmap;
begin
  Result := TBitmap.Create;
  try
    Result.Width := Width;
    Result.Height := Height;
    Result.Canvas.Brush.Color := clBlack;
    Result.Canvas.FillRect(Rect(0, 0, Width, Height));
  except
    Result.Free;
    raise;
  end;
end;

function TMovieImageCreator.GetRatio(): Double;
begin
  Result := 16.0 / 9.0; // 一般的な動画のアスペクト比
end;

function TMovieImageCreator.GetPageIndex: Integer;
begin
  Result := 0;
end;

procedure TMovieImageCreator.SetPageIndex(AValue: Integer);
begin
  // 動画はページの概念がないため、何もしない
end;

function TMovieImageCreator.GetPageCount: Integer;
begin
  Result := 1; // 動画は1つのストリームとして扱う
end;

end. 