// File: FilesParam.pas
unit FilesParam;

{$mode ObjFPC}{$H+}

interface

uses
  Graphics,SysUtils, PdfImageCreator, RotateImageCreatorUnit, ZoomUnit, ImageCreatorUnit, TImageCreatorUnit;

type
  TFilesParam = class
    Filename: string;
    Selected: Boolean;
    ImageCreator: IDocmentImageCreator;
    RotateImageCreator: TRotateImageCreator;
    Zoom: TZoom;
  public
    constructor Create(AFileName: string; ASelected: Boolean);
  end;

implementation

{TFilesParam}

constructor TFilesParam.Create(AFileName : string; ASelected : Boolean);
var
  Ext : String;
begin
  inherited Create;

  Ext := LowerCase(ExtractFileExt(AFileName));
  if (Ext = '.jpg') or (Ext = '.png') or (Ext = '.bmp') then
  begin
    ImageCreator := TImageCreator.Create(AFileName);
  end else if Ext = '.pdf' then
  begin
    ImageCreator := TPdfImageCreator.Create(AFileName);
  end;
  RotateImageCreator:= TRotateImageCreator.Create(ImageCreator);
  Zoom := TZoom.Create(RotateImageCreator);
  Filename := AFileName;
  Selected := ASelected;
end;

end.
