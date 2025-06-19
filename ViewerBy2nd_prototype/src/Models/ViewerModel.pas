unit ViewerModel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, PdfImageCreator, Generics.Collections, ZoomUnit, RotateImageCreatorUnit, FilesParam, Background, RepogitoryUnit, ViewerBy2ndFileTypes;

type
  TViewerModel = class
  private
    FRepogitory: TRepogitory;
    FBackground: TBackground;
    FUpdated: Boolean;
    function GetViewRatio: Double;
    function GetThumbnailRatio: Double;
    function GetHasViewDocument: Boolean;
    function GetHasOperationDocument: Boolean;
    function GetCanNext: Boolean;
    function GetCanPrevious: Boolean;
    function GetCanLast: Boolean;
    function GetCanFirst: Boolean;
    function GetPageIndex: Integer;
    function GetPageCount: Integer;
    function GetCanZoomOut: Boolean;
    function GetCanZoomIn: Boolean;
    function GetCanZoom: Boolean;
    function GetCanRotate: Boolean;
    function GetOperationFile: TFilesParam;
    function GetZoom: TZoom;
    property OperationFile : TFilesParam read GetOperationFile;
  public
    procedure Next;
    procedure SetPageIndex(Value: Integer);
    procedure Previous;
    destructor Destroy; override;
    function Open(const Filename: string): Boolean;
    procedure View;
    constructor Create;
    function GetViewBitmap(Width, Height: Integer): TBitmap;
    function GetThumbnailBitmap(Width, Height: Integer): TBitmap;
    function IsMovieFile: Boolean;
    procedure SetUpdated(Value: Boolean);
    procedure ClearUpdated;
    procedure ZoomIn;
    procedure ZoomOut;
    procedure FitWindow(Width, Height: Integer);
    procedure ViewAll;
    procedure MouseWheel(WheelDelta: Integer; X, Y: Integer; Shift: TShiftState);
    property HasViewDocument: Boolean read GetHasViewDocument;
    property HasOperationDocument: Boolean read GetHasOperationDocument;
    property ViewRatio: Double read GetViewRatio;
    property ThumbnailRatio: Double read GetThumbnailRatio;
    property CanNext: Boolean read GetCanNext;
    property CanPrevious: Boolean read GetCanPrevious;
    property CanLast: Boolean read GetCanLast;
    property CanFirst: Boolean read GetCanFirst;
    property CanZoomIn: Boolean read GetCanZoomIn;
    property CanZoomOut: Boolean read GetCanZoomOut;
    property CanZoom: Boolean read GetCanZoom;
    property PageIndex: Integer read GetPageIndex write SetPageIndex;
    property PageCount: Integer read GetPageCount;
    property Repogitory: TRepogitory read FRepogitory;
    property Background: TBackground read FBackground;
    property Zoom: TZoom read GetZoom;
    property Updated: Boolean read FUpdated;
    procedure Rotate(Angle: Integer);
    property CanRotate: Boolean read GetCanRotate;
    procedure LastPage;
    procedure FirstPage;
    procedure DeleteSelectedFiles;
    procedure DisselectAll;
    procedure SelectAllFiles;
    procedure AddFile(const Filename: string);
    function GetSelectedFile: TFilesParam;
    function GetFileNames: TStringList;
    procedure SetFileSelected(Index: Integer; Selected: Boolean);
    function GetFileSelected(Index: Integer): Boolean;
    function GetFileCount: Integer;
    function GetFileName(Index: Integer): String;
  end;

var
  model: TViewerModel;

implementation

{ TViewerModel }
procedure TViewerModel.Rotate(Angle : Integer);
begin
     OperationFile.RotateImageCreator.Rotate(Angle);
     OperationFile.Zoom.CenterClear();
     FUpdated := True;
end;

function TViewerModel.GetZoom : TZoom;
begin
  if Assigned( Repogitory.GetSelectedFile) then
  Result := Repogitory.GetSelectedFile.Zoom;
end;

function TViewerModel.GetCanZoom : Boolean;
begin
  Result := False;
  if not Assigned( Repogitory.GetSelectedFile) then
    Exit;
  Result := True;
end;

function TViewerModel.GetCanRotate : Boolean;
begin
  Result := False;
  if not Assigned( Repogitory.GetSelectedFile) then
    Exit;
  Result := True;

end;

function TViewerModel.GetCanZoomIn : Boolean;
begin
  Result := False;
  if not GetCanZoom then
    Exit;
  If 5.0 <= Repogitory.GetSelectedFile.Zoom.Rate then
    Exit;
  Result := True;

end;
function TViewerModel.GetCanZoomOut : Boolean;
begin
  Result := False;
  if not GetCanZoom then
    Exit;
  If Repogitory.GetSelectedFile.Zoom.Rate <= 1.0 then
    Exit;
  Result := True;
end;

function TViewerModel.GetOperationFile : TFilesParam;
begin
      Result :=  Repogitory.GetSelectedFile;

end;



procedure TViewerModel.LastPage();
begin
  PageIndex := PageCount - 1;
  FUpdated := True;
end;

procedure TViewerModel.FirstPage();
begin
  PageIndex := 0;
  FUpdated := True;
end;

procedure TViewerModel.SetPageIndex(value: Integer);
begin
  if Repogitory.GetSelectedIndex = -1 then
    Exit;

  if value < 0 then
    value := 0;

  if value > PageCount - 1 then
    value := PageCount - 1;

  OperationFile.ImageCreator.SetPageIndex(value);
  FUpdated := True;
end;

function TViewerModel.GetPageCount: Integer;
begin
  if Repogitory.GetSelectedIndex = -1 then
    Result := 0
  else
  Result := OperationFile.ImageCreator.GetPageCount;
end;

function TViewerModel.GetPageIndex: Integer;
begin
  if Repogitory.GetSelectedIndex = -1 then
    Result := 0
  else
  Result := OperationFile.ImageCreator.GetPageIndex;
end;

procedure TViewerModel.Previous;
begin
  if 0 <= Repogitory.GetSelectedIndex then
    PageIndex := PageIndex - 1;
  FUpdated := True;
end;

procedure TViewerModel.Next;
begin
  if 0 <= Repogitory.GetSelectedIndex then
    PageIndex := PageIndex + 1;
  FUpdated := True;
end;

function TViewerModel.GetHasOperationDocument: Boolean;
begin
  Result := Assigned(OperationFile);
end;

function TViewerModel.GetHasViewDocument: Boolean;
begin
  Result := Assigned(Repogitory.ViewFile);
end;

constructor TViewerModel.Create;
begin
  inherited Create;
  FRepogitory := TRepogitory.Create;
  FBackground := TBackground.Create;
  FUpdated := False;
end;

destructor TViewerModel.Destroy;
begin
  FRepogitory.Destroy;
  FBackground.Destroy;

  inherited Destroy;
end;
function TViewerModel.Open(const Filename: string): Boolean;
begin
  try
    Repogitory.AddFile(FileName);
    FUpdated := True;
    Result := True;
  except
    Result := False;
  end;
end;


procedure TViewerModel.View;
begin
  Repogitory.ViewFile := OperationFile;
  FUpdated := True;
end;

function TViewerModel.GetViewBitmap(Width, Height: Integer): TBitmap;
begin
  if Assigned(Repogitory.ViewFile) then
    Result := Repogitory.ViewFile.Zoom.GetBitmap(Width, Height)
  else
    Result := FBackground.GetBitmap(Width, Height);
end;

function TViewerModel.GetThumbnailBitmap(Width, Height: Integer): TBitmap;
begin
  if Assigned(OperationFile) then
    Result := OperationFile.Zoom.GetBitmap(Width, Height)
  else
    Result := FBackground.GetBitmap(Width, Height);
end;

function TViewerModel.GetViewRatio: Double;
begin
  if Assigned(Repogitory.ViewFile) then
  begin
    Result := Repogitory.ViewFile.RotateImageCreator.GetRatio();
  end
  else
  begin
    Result := 1;
  end;
end;

function TViewerModel.GetThumbnailRatio: Double;
begin
  if Assigned(OperationFile) then
  begin
    Result := OperationFile.RotateImageCreator.GetRatio();
  end
  else
  begin
    Result := 1;
  end;
end;

function TViewerModel.GetCanNext: Boolean;
begin
  Result := Assigned(OperationFile) and (PageIndex < PageCount - 1);
end;

function TViewerModel.GetCanPrevious: Boolean;
begin
  Result := Assigned(OperationFile) and (PageIndex > 0);
end;

function TViewerModel.GetCanLast: Boolean;
begin
  Result := Assigned(OperationFile) and (PageIndex <> PageCount - 1);
end;

function TViewerModel.GetCanFirst: Boolean;
begin
  Result := Assigned(OperationFile) and (PageIndex <> 0);
end;

function TViewerModel.IsMovieFile: Boolean;
begin
  Result := False;
  if not Assigned(OperationFile) then
    Exit;
  Result := IsMovie(OperationFile.Filename);
end;

procedure TViewerModel.SetUpdated(Value: Boolean);
begin
  FUpdated := Value;
end;

procedure TViewerModel.ClearUpdated;
begin
  FUpdated := False;
end;

procedure TViewerModel.ZoomIn;
begin
  if Assigned(Repogitory.GetSelectedFile) then
  begin
    Repogitory.GetSelectedFile.Zoom.ZoomIn();
    FUpdated := True;
  end;
end;

procedure TViewerModel.ZoomOut;
begin
  if Assigned(Repogitory.GetSelectedFile) then
  begin
    Repogitory.GetSelectedFile.Zoom.ZoomOut();
    FUpdated := True;
  end;
end;

procedure TViewerModel.FitWindow(Width, Height: Integer);
begin
  if Assigned(Repogitory.GetSelectedFile) then
  begin
    Repogitory.GetSelectedFile.Zoom.fitWindow(Width, Height);
    FUpdated := True;
  end;
end;

procedure TViewerModel.ViewAll;
begin
  if Assigned(Repogitory.GetSelectedFile) then
  begin
    Repogitory.GetSelectedFile.Zoom.Rate := 1.0;
    FUpdated := True;
  end;
end;

procedure TViewerModel.MouseWheel(WheelDelta: Integer; X, Y: Integer; Shift: TShiftState);
begin
  if Assigned(Repogitory.GetSelectedFile) and CanZoom then
  begin
    Repogitory.GetSelectedFile.Zoom.MouseWheel(WheelDelta, X, Y, Shift);
    FUpdated := True;
  end;
end;

procedure TViewerModel.DeleteSelectedFiles;
begin
  Repogitory.Delete;
  FUpdated := True;
end;

procedure TViewerModel.DisselectAll;
begin
  Repogitory.Disselect;
  FUpdated := True;
end;

procedure TViewerModel.SelectAllFiles;
begin
  Repogitory.SelectAll;
  FUpdated := True;
end;

procedure TViewerModel.AddFile(const Filename: string);
begin
  Repogitory.AddFile(Filename);
  FUpdated := True;
end;

function TViewerModel.GetSelectedFile: TFilesParam;
begin
  Result := Repogitory.GetSelectedFile;
end;

function TViewerModel.GetFileNames: TStringList;
begin
  Result := Repogitory.GetFileNames;
end;

procedure TViewerModel.SetFileSelected(Index: Integer; Selected: Boolean);
begin
  Repogitory.Selected[Index] := Selected;
  FUpdated := True;
end;

function TViewerModel.GetFileSelected(Index: Integer): Boolean;
begin
  Result := Repogitory.Selected[Index];
end;

function TViewerModel.GetFileCount: Integer;
begin
  Result := Repogitory.GetCount;
end;

function TViewerModel.GetFileName(Index: Integer): String;
begin
  Result := Repogitory.GetFileName(Index);
end;
end.

