unit ElementDetailForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  PeriodicTableData;

procedure ShowElementDetail(const Element: TElement);

implementation

{$R *.lfm}

type
  TElementDetailDialog = class(TForm)
    Panel1: TPanel;
    LabelTitle: TLabel;
    LabelAtomicNumber: TLabel;
    LabelSymbol: TLabel;
    LabelName: TLabel;
    LabelNameJP: TLabel;
    LabelAtomicWeight: TLabel;
    LabelGroup: TLabel;
    LabelPeriod: TLabel;
    LabelCategory: TLabel;
    ButtonClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
  private
    procedure SetElement(const Element: TElement);
  public
    property Element: TElement write SetElement;
  end;

var
  ElementDetailDialog: TElementDetailDialog;

procedure ShowElementDetail(const Element: TElement);
begin
  if ElementDetailDialog = nil then
  begin
    ElementDetailDialog := TElementDetailDialog.Create(Application);
  end;
  ElementDetailDialog.Element := Element;
  ElementDetailDialog.ShowModal;
end;

{ TElementDetailDialog }

procedure TElementDetailDialog.FormCreate(Sender: TObject);
begin
  // フォームは .lfm ファイルから読み込まれる
end;

procedure TElementDetailDialog.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TElementDetailDialog.SetElement(const Element: TElement);
begin
  LabelAtomicNumber.Caption := '原子番号: ' + IntToStr(Element.AtomicNumber);
  LabelSymbol.Caption := Element.Symbol;
  LabelName.Caption := '英語名: ' + Element.Name;
  LabelNameJP.Caption := '日本語名: ' + Element.NameJP;
  LabelAtomicWeight.Caption := '原子量: ' + Element.AtomicWeight;
  if Element.Group > 0 then
    LabelGroup.Caption := '族: ' + IntToStr(Element.Group)
  else
    LabelGroup.Caption := '族: -';
  if Element.Period > 0 then
    LabelPeriod.Caption := '周期: ' + IntToStr(Element.Period)
  else
    LabelPeriod.Caption := '周期: -';
  LabelCategory.Caption := '分類: ' + Element.Category;
end;

initialization

finalization
  if ElementDetailDialog <> nil then
    ElementDetailDialog.Free;

end.

