unit ViewerBy2ndFileTypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FPImage, FPReadJPEG, FPReadPNG, FPReadBMP;

function CanOpen(const FileName: string): Boolean;
function IsImageFile(const FileName: string): Boolean;
function IsPDFFile(const FileName: string): Boolean;
function GetFileFilter: string;
function GetTFPReader(const FileName: string): TFPCustomImageReader;

implementation

function IsImageFile(const FileName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  Result := (Ext = '.jpg') or (Ext = '.jpeg') or (Ext = '.png') or (Ext = '.bmp');
end;

function IsPDFFile(const FileName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  Result := Ext = '.pdf';
end;

function CanOpen(const FileName: string): Boolean;
begin
  Result := IsImageFile(FileName) or IsPDFFile(FileName);
end;

function GetFileFilter: string;
begin
  Result := 'PDF Files|*.pdf|Image Files|*.jpg;*.jpeg;*.png;*.bmp|All Files|*.*';
end;

function GetTFPReader(const FileName: string): TFPCustomImageReader;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  if (Ext = '.jpg') or (Ext = '.jpeg') then
    Result := TFPReaderJPEG.Create
  else if Ext = '.png' then
    Result := TFPReaderPNG.Create
  else if Ext = '.bmp' then
    Result := TFPReaderBMP.Create
  else
    raise Exception.Create('Unsupported image format: ' + Ext);
end;

end.

