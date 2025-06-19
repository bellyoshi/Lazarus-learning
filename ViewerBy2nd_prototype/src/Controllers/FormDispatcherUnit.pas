unit FormDispatcherUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, frmViewer, PageFormUnit, SettingFormUnit, AboutUnit, ZoomRateFormUnit;

type
  TFormDispatcher = class
  private
    class var FInstance: TFormDispatcher;
    constructor Create;
  public
    class function GetInstance: TFormDispatcher;
    class procedure ReleaseInstance;
    
    // フォーム表示メソッド
    procedure ShowViewerForm;
    procedure ShowPageForm;
    procedure ShowSettingForm;
    procedure ShowAboutForm;
    procedure ShowZoomRateForm;
    
    // フォーム操作メソッド
    procedure CloseViewerForm;
    
    destructor Destroy; override;
  end;

// グローバル関数（利便性のため）
function FormDispatcher: TFormDispatcher;

implementation

var
  FFormDispatcher: TFormDispatcher;

function FormDispatcher: TFormDispatcher;
begin
  if not Assigned(FFormDispatcher) then
    FFormDispatcher := TFormDispatcher.Create;
  Result := FFormDispatcher;
end;

{ TFormDispatcher }

constructor TFormDispatcher.Create;
begin
  inherited Create;
end;

class function TFormDispatcher.GetInstance: TFormDispatcher;
begin
  if not Assigned(FInstance) then
    FInstance := TFormDispatcher.Create;
  Result := FInstance;
end;

class procedure TFormDispatcher.ReleaseInstance;
begin
  if Assigned(FInstance) then
  begin
    FInstance.Free;
    FInstance := nil;
  end;
end;

procedure TFormDispatcher.ShowViewerForm;
begin
  ViewerForm.ShowDocument;
end;

procedure TFormDispatcher.ShowPageForm;
begin
  PageForm.Display;
end;

procedure TFormDispatcher.ShowSettingForm;
begin
  SettingForm.Show;
end;

procedure TFormDispatcher.ShowAboutForm;
begin
  AboutForm.Show;
end;

procedure TFormDispatcher.ShowZoomRateForm;
begin
  ZoomRateForm.Display;
end;

procedure TFormDispatcher.CloseViewerForm;
begin
  ViewerForm.Close;
end;

destructor TFormDispatcher.Destroy;
begin
  inherited Destroy;
end;

initialization
  FFormDispatcher := nil;

finalization
  if Assigned(FFormDispatcher) then
    FFormDispatcher.Free;

end. 