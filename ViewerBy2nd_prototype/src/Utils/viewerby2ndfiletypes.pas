unit ViewerBy2ndFileTypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FPImage, FPReadJPEG, FPReadPNG, FPReadBMP;

const
  IMAGE_EXTENSIONS: array[0..3] of string = ('.jpg', '.jpeg', '.png', '.bmp');
  PDF_EXTENSIONS: array[0..0] of string = ('.pdf');
  MOVIE_EXTENSIONS: array[0..3] of string = ('.mp4', '.avi', '.mov', '.wmv');

function CanOpen(const FileName: string): Boolean;
function IsImageFile(const FileName: string): Boolean;
function IsPDFFile(const FileName: string): Boolean;
function IsMovie(const FileName: string): Boolean;
function GetFileFilter: string;
function GetTFPReader(const FileName: string): TFPCustomImageReader;

implementation

function IsImageFile(const FileName: string): Boolean;
var
  Ext: string;
  i: Integer;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  Result := False;
  for i := Low(IMAGE_EXTENSIONS) to High(IMAGE_EXTENSIONS) do
  begin
    if Ext = IMAGE_EXTENSIONS[i] then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function IsPDFFile(const FileName: string): Boolean;
var
  Ext: string;
  i: Integer;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  Result := False;
  for i := Low(PDF_EXTENSIONS) to High(PDF_EXTENSIONS) do
  begin
    if Ext = PDF_EXTENSIONS[i] then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function IsMovie(const FileName: string): Boolean;
var
  Ext: string;
  i: Integer;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  Result := False;
  for i := Low(MOVIE_EXTENSIONS) to High(MOVIE_EXTENSIONS) do
  begin
    if Ext = MOVIE_EXTENSIONS[i] then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function CanOpen(const FileName: string): Boolean;
begin
  Result := IsImageFile(FileName) or IsPDFFile(FileName) or IsMovie(FileName);
end;

function GetFileFilter: string;
var
  ImageFilter, MovieFilter: string;
  i: Integer;
begin
  // Build image filter
  ImageFilter := '';
  for i := Low(IMAGE_EXTENSIONS) to High(IMAGE_EXTENSIONS) do
  begin
    if i > Low(IMAGE_EXTENSIONS) then
      ImageFilter := ImageFilter + ';';
    ImageFilter := ImageFilter + '*' + IMAGE_EXTENSIONS[i];
  end;

  // Build movie filter
  MovieFilter := '';
  for i := Low(MOVIE_EXTENSIONS) to High(MOVIE_EXTENSIONS) do
  begin
    if i > Low(MOVIE_EXTENSIONS) then
      MovieFilter := MovieFilter + ';';
    MovieFilter := MovieFilter + '*' + MOVIE_EXTENSIONS[i];
  end;

  Result := Format('PDF Files|*.pdf|Image Files|%s|Movie Files|%s|All Files|*.*',
    [ImageFilter, MovieFilter]);
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

