unit ImageCreatorUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Graphics;

type
  // ドキュメント画像作成のためのインターフェース
  IDocmentImageCreator = interface
    ['{12345678-1234-1234-1234-123456789012}']
    function GetBitmap(Width, Height: Integer): TBitmap;
    function GetPageCount: Integer;
  end;

implementation

end.
