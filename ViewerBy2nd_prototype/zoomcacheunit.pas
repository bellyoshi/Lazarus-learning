unit ZoomCacheUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Generics.Collections, PdfImageCreator;

type
  TZoomCacheItem = record
    Width: Integer;
    Height: Integer;
    Bitmap: TBitmap;
  end;

  TZoomCache = class
  private
    FImageCreator: TPdfImageCreator;
    FCache: specialize TList<TZoomCacheItem>;
    FMaxCacheSize: Integer;
    function FindInCache(Width, Height: Integer): TBitmap;
    procedure AddToCache(Width, Height: Integer; Bitmap: TBitmap);
  public
    constructor Create(AImageCreator: TPdfImageCreator; AMaxCacheSize: Integer = 10);
    destructor Destroy; override;
    function GetBitmap(Width, Height: Integer): TBitmap;
  end;

implementation

constructor TZoomCache.Create(AImageCreator: TPdfImageCreator; AMaxCacheSize: Integer);
begin
  inherited Create;
  FImageCreator := AImageCreator;
  FMaxCacheSize := AMaxCacheSize;
  FCache := specialize TList<TZoomCacheItem>.Create;
end;

destructor TZoomCache.Destroy;
var
  Item: TZoomCacheItem;
begin
  for Item in FCache do
    Item.Bitmap.Free;
  FCache.Free;
  inherited Destroy;
end;

function TZoomCache.FindInCache(Width, Height: Integer): TBitmap;
var
  Item: TZoomCacheItem;
begin
  for Item in FCache do
  begin
    if (Item.Width = Width) and (Item.Height = Height) then
      Exit(Item.Bitmap);
  end;
  Result := nil;
end;

procedure TZoomCache.AddToCache(Width, Height: Integer; Bitmap: TBitmap);
var
  Item: TZoomCacheItem;
begin
  if FCache.Count >= FMaxCacheSize then
  begin
    FCache[0].Bitmap.Free;
    FCache.Delete(0);
  end;

  Item.Width := Width;
  Item.Height := Height;
  Item.Bitmap := Bitmap;
  FCache.Add(Item);
end;

function TZoomCache.GetBitmap(Width, Height: Integer): TBitmap;
var
  CachedBitmap: TBitmap;
begin
  CachedBitmap := FindInCache(Width, Height);
  if Assigned(CachedBitmap) then
    Exit(CachedBitmap);

  Result := FImageCreator.GetBitmap(Width, Height);
  AddToCache(Width, Height, Result);
end;

end.

