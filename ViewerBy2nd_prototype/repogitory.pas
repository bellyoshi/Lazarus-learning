// File: Repogitory.pas
unit Repogitory;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, FilesParam, PdfImageCreator;
type
  TRepogitory = class
  private
    FFilesList: specialize TList<TFilesParam>;
    FViewFile: TFilesParam;

    function GetSelected(I: Integer): Boolean;
    procedure SetSelected(I: Integer; Value: Boolean);
  public
    function GetSelectedIndex: Integer;
    constructor Create;
    procedure Disselect;
    procedure SelectAll;
    procedure Delete;
    procedure AddFile(Filename: String);
    function GetSelectedFile: TFilesParam;
    function GetFileNames: TStringList;
    property ViewFile: TFilesParam read FViewFile write FViewFile;
    property Selected[I: Integer]: Boolean read GetSelected write SetSelected;
  end;

implementation
{ TRepogitory }

constructor TRepogitory.Create();
begin
  inherited Create;
  FFilesList := specialize TList<TFilesParam>.Create();
  FViewFile := nil;
end;

procedure TRepogitory.Delete();
var
  rList : specialize TList<TFilesParam> ;
  item : TFilesParam;
begin
  rList := specialize TList<TFilesParam>.Create();
  for item in FFilesList do
  begin
    if item.Selected then
    begin
      rList.Add(item);
    end;
  end;
  for item in rList do
  begin
    FFilesList.Remove(item);
    item.Free;
  end;
  rList.Free;
end;

procedure TRepogitory.SelectAll();
var
  i : Integer;
begin
  for i := 0 to FFilesList.Count - 1 do
  begin
    FFilesList.Items[i].Selected := True;
  end;
end;

function TRepogitory.GetSelectedIndex : Integer;
var
  index : Integer;
  i : Integer;
begin
  index := -1;
  for i := 0 to FFilesList.Count - 1 do
  begin
       if FFilesList.Items[i].Selected then begin
         if index = -1 then begin
            index := i;
         end else
         begin
           index := -1;
           break;
         end;
       end;
  end;
  Result := index;
end;

function TRepogitory.GetSelectedFile() : TFilesParam;
var
  index : Integer;
begin
  index := GetSelectedIndex();
  if index = -1 then
  begin
    Result := nil;
  end
  else
  begin
    Result := FFilesList.Items[index];
  end;

end;

procedure TRepogitory.SetSelected(I: Integer; Value : Boolean);
begin
  FFilesList.Items[i].Selected := Value;
//  RecalcSingleSelect();
end;
function TRepogitory.GetSelected(I: Integer): Boolean;
begin
  Result := FFilesList[i].Selected;
end;
procedure TRepogitory.DisSelect;
var
  i : LongInt;
begin
  for i := 0 to FFilesList.Count - 1 do
  begin
    FFilesList.Items[i].Selected := False;
  end;
  FViewFile := nil;
//  FOperationPdfDocument:= nil;
end;

procedure TRepogitory.AddFile(filename :String);
var
  newFileParam: TFilesParam;
  pdfImageCreator : TPdfImageCreator;
  i : Integer;
begin
  for i := 0 to FFilesList.Count - 1 do
  begin
    FFilesList.Items[i].Selected := False;
  end;
  newFileParam := TFilesParam.Create(Filename, true);
  FFilesList.Add(newFileParam);



end;

function TRepogitory.GetFileNames: TStringList;
var
  i: Integer;
  fileList: TStringList;
begin
  fileList := TStringList.Create;
  try
    for i := 0 to FFilesList.Count -1 do
      fileList.Add(FFilesList[i].Filename);
    Result := fileList;
  except
    fileList.Free;
    raise;
  end;
end;
end.

