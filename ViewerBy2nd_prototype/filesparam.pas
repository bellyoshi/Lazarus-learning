// File: FilesParam.pas
unit FilesParam;

{$mode ObjFPC}{$H+}

interface

uses
  Graphics, PdfImageCreator, RotateImageCreatorUnit, ZoomUnit;

type
  TFilesParam = class
    Filename: string;
    Selected: Boolean;
    ImageCreator: TPdfImageCreator;
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
  ImageCreator := TPdfImageCreator.Create(AFileName);
  RotateImageCreator:= TRotateImageCreator.Create(ImageCreator);
  Zoom := TZoom.Create(RotateImageCreator);
  Filename := AFileName;
  Selected := ASelected;
end;

end.
