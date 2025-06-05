// File: FilesParam.pas
unit FilesParam;

{$mode ObjFPC}{$H+}

interface

uses
  Graphics, SysUtils, PdfImageCreator, RotateImageCreatorUnit, ZoomUnit, 
  ImageCreatorUnit, TImageCreatorUnit, ViewerBy2ndFileTypes;

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
begin
  inherited Create;

  if IsImageFile(AFileName) then
  begin
    ImageCreator := TImageCreator.Create(AFileName);
  end else if IsPDFFile(AFileName) then
  begin
    ImageCreator := TPdfImageCreator.Create(AFileName);
  end;
  RotateImageCreator:= TRotateImageCreator.Create(ImageCreator);
  Zoom := TZoom.Create(RotateImageCreator);
  Filename := AFileName;
  Selected := ASelected;
end;

end.
