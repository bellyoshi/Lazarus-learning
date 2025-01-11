unit ImageCreatorUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics;


type
  IImageCreator = interface

    function GetBitmap(Width, Height: Integer): TBitmap; // ビットマップ取得ー
    function GetRatio(): Double;
  end;

implementation

end.

