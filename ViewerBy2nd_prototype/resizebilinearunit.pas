unit ReSizeBilinearUnit;
//Bitmapの編集をする関数群

{$mode ObjFPC}{$H+}

interface

uses
  Classes,   Graphics   ;

function ReSizeBilinear(src_pic: TBitmap; wx, hy: integer): TBitmap;
function Rotation90Right(src_pic: TBitmap): TBitmap;
function Rotation180(src_pic: TBitmap): TBitmap;
function Rotation90Left(src_pic: TBitmap): TBitmap;

implementation
uses
   SysUtils,   LCLType, LCLProc, LCLIntf, IntfGraphics, FPImage;
function ReSizeBilinear(src_pic: TBitmap; wx, hy: integer): TBitmap;
var
  x, y, w, h, ix, iy, r, g, b: integer;
  nw, nh: integer;
  wf, hf, s, t: double;
  clr_set: TColor;
  clrs: array[0..3] of TColor;
  TempImg_r, TempImg_s: TLazIntfImage;
  ret_pic: TBitmap;
begin
   w:= src_pic.width;
   h:= src_pic.height; //倍率計算
   // ピクセル単位指定
   wf:= wx/w; hf:= hy/h;
   nw:= wx; nh:= hy;

  r:= 0; g:= 0; b:= 0;
  try

    ret_pic := TBitmap.Create;
    ret_pic.SetSize(nw,nh); // 倍率のサイズ設定
    TempImg_r:= ret_pic.CreateIntfImage;
    TempImg_s:= src_pic.CreateIntfImage;
    for y:= 0 to nh - 1 do begin
      for x:= 0 to nw - 1 do begin
        ix:= round(x/wf);
        iy:= round(y/hf);
        s:= abs((x/wf)-ix);
        t:= abs((y/hf)-iy);
        if (ix >= 0) and (ix < w-2) and (iy >= 0) and (iy < h-2) then
        begin
          clrs[0]:= TempImg_s.TColors[ix,iy];
          clrs[1]:= TempImg_s.TColors[ix+1,iy];
          clrs[2]:= TempImg_s.TColors[ix,iy+1];
          clrs[3]:= TempImg_s.TColors[ix+1,iy+1];
          r:= round( (1-s) * (1-t) * Red(clrs[0]) + s * (1-t) * Red(clrs[1]) + (1-s) * t * Red(clrs[2]) + s * t * Red(clrs[3]));
          g:= round( (1-s) * (1-t) * Green(clrs[0])  + s * (1-t) * Green(clrs[1]) + (1-s) * t * Green(clrs[2]) + s * t * Green(clrs[3]));
          b:= round( (1-s) * (1-t) * Blue(clrs[0])  + s * (1-t) * Blue(clrs[1]) + (1-s) * t * Blue(clrs[2])+ s * t * Blue(clrs[3]));
        end;
        // Red
        if r > 255 then r:= 255
        else if r < 0 then r:= 0;
        // Green
        if g > 255 then g:= 255
        else if g < 0 then g:= 0;
        // Blue
        if b > 255 then b:= 255
        else if b < 0 then b:= 0;
        clr_set:= RGBtoColor(r,g,b);
        TempImg_r.Colors[x,y]:= TColorToFPColor(clr_set);
        r:= 0; g:= 0; b:= 0;
        end;
      end;
    ret_pic.LoadFromIntfImage(TempImg_r);
  finally
    TempImg_r.free;
    TempImg_s.free;
  end;
  result:= ret_pic;
end;
// 引数src_picの画像を90度右に回転します。
function Rotation90Right(src_pic: TBitmap): TBitmap;
var
  x, y, w, h: integer;
  clr: TFPColor;
  TempImg_r, TempImg_s: TLazIntfImage;
  ret_pic: TBitmap;
begin
  w:= src_pic.width;
  h:= src_pic.height;
  try
    ret_pic:= TBitmap.Create;
    ret_pic.SetSize(h,w);
    TempImg_r:= ret_pic.CreateIntfImage;
    TempImg_s:= src_pic.CreateIntfImage;
    for y:= 0 to h - 1 do begin
      for x:= 0 to w - 1 do begin
        clr:= TempImg_s.Colors[x,y];
        TempImg_r.Colors[(h-1)-y,x]:= clr;
      end;
    end;
    ret_pic.LoadFromIntfImage(TempImg_r);
    finally
      TempImg_r.free;
      TempImg_s.free;
    end;
    result:= ret_pic;
end;
// 引数src_picの画像を90度左に回転します。
function Rotation90Left(src_pic: TBitmap): TBitmap;
var
  x, y, w, h: integer;
  clr: TFPColor;
  TempImg_r, TempImg_s: TLazIntfImage;
  ret_pic: TBitmap;
begin
  w := src_pic.Width;
  h := src_pic.Height;
  try
    ret_pic := TBitmap.Create;
    ret_pic.SetSize(h, w);
    TempImg_r := ret_pic.CreateIntfImage;
    TempImg_s := src_pic.CreateIntfImage;
    for y := 0 to h - 1 do begin
      for x := 0 to w - 1 do
      begin
        clr := TempImg_s.Colors[x, y];
        TempImg_r.Colors[y, (w - 1) - x] := clr;
      end;
    end;
    ret_pic.LoadFromIntfImage(TempImg_r);
  finally
    TempImg_r.Free;
    TempImg_s.Free;
  end;
  Result := ret_pic;
end;

// 引数src_picの画像を180度回転します。
function Rotation180(src_pic: TBitmap): TBitmap;
var
  x, y, w, h: integer;
  clr: TFPColor;
  TempImg_r, TempImg_s: TLazIntfImage;
  ret_pic: TBitmap;
begin
  w := src_pic.Width;
  h := src_pic.Height;
  try
    ret_pic := TBitmap.Create;
    ret_pic.SetSize(w, h);
    TempImg_r := ret_pic.CreateIntfImage;
    TempImg_s := src_pic.CreateIntfImage;
    for y := 0 to h - 1 do begin
      for x := 0 to w - 1 do
      begin
        clr := TempImg_s.Colors[x, y];
        TempImg_r.Colors[(w - 1) - x, (h - 1) - y] := clr;
      end;
    end;
    ret_pic.LoadFromIntfImage(TempImg_r);
  finally
    TempImg_r.Free;
    TempImg_s.Free;
  end;
  Result := ret_pic;
end;


end.

