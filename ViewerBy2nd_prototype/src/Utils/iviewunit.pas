unit IViewUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  // IViewインターフェースの定義
  IView = interface
    procedure UpdateView;
  end;

  // TFormManagerクラスの定義
  TFormManager = class
  private
    FViews: specialize TList<IView>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterView(AView: IView);
    procedure Update; // すべてのフォームの UpdateView を呼び出す
  end;

var
  formManager : TFormManager;

implementation


constructor TFormManager.Create;
begin
  FViews := specialize TList<IView>.Create;
end;

destructor TFormManager.Destroy;
begin
  FViews.Free;
  inherited Destroy;
end;

procedure TFormManager.RegisterView(AView: IView);
begin
  FViews.Add(AView);
end;

procedure TFormManager.Update;
var
  View: IView;
begin
  for View in FViews do
  begin
    View.UpdateView; // 各フォームの UpdateView を呼び出す
  end;
end;



end.
