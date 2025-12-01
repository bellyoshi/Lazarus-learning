unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  PeriodicTableData, ElementDetailForm;

type
  TElementButton = class(TPanel)
  private
    FElement: TElement;
    FLabelNumber: TLabel;
    FLabelSymbol: TLabel;
    FLabelName: TLabel;
    procedure SetElement(const AElement: TElement);
    procedure ButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    property Element: TElement read FElement write SetElement;
  end;

  TForm1 = class(TForm)
    ScrollBox1: TScrollBox;
    PanelMain: TPanel;
    LabelTitle: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    ElementButtons: array[1..ELEMENT_COUNT] of TElementButton;
    procedure CreatePeriodicTable;
    function GetElementColor(const Category: string): TColor;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TElementButton }

constructor TElementButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 70;
  Height := 80;
  BevelOuter := bvLowered;
  BorderWidth := 1;
  ParentFont := False;
  Font.Size := 8;
  
  FLabelNumber := TLabel.Create(Self);
  FLabelNumber.Parent := Self;
  FLabelNumber.Left := 2;
  FLabelNumber.Top := 2;
  FLabelNumber.Font.Size := 7;
  FLabelNumber.Font.Style := [fsBold];
  FLabelNumber.AutoSize := True;
  
  FLabelSymbol := TLabel.Create(Self);
  FLabelSymbol.Parent := Self;
  FLabelSymbol.Left := 0;
  FLabelSymbol.Top := 15;
  FLabelSymbol.Width := 70;
  FLabelSymbol.Height := 25;
  FLabelSymbol.Alignment := taCenter;
  FLabelSymbol.Font.Size := 14;
  FLabelSymbol.Font.Style := [fsBold];
  FLabelSymbol.Layout := tlCenter;
  
  FLabelName := TLabel.Create(Self);
  FLabelName.Parent := Self;
  FLabelName.Left := 0;
  FLabelName.Top := 40;
  FLabelName.Width := 70;
  FLabelName.Height := 40;
  FLabelName.Alignment := taCenter;
  FLabelName.Font.Size := 7;
  FLabelName.WordWrap := True;
  FLabelName.Layout := tlCenter;
  
  OnClick := @ButtonClick;
end;

procedure TElementButton.SetElement(const AElement: TElement);
begin
  FElement := AElement;
  FLabelNumber.Caption := IntToStr(AElement.AtomicNumber);
  FLabelSymbol.Caption := AElement.Symbol;
  FLabelName.Caption := AElement.NameJP;
  Color := Form1.GetElementColor(AElement.Category);
end;

procedure TElementButton.ButtonClick(Sender: TObject);
begin
  ShowElementDetail(FElement);
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := '周期表 (Periodic Table)';
  CreatePeriodicTable;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  PanelMain.Width := ScrollBox1.ClientWidth;
end;

function TForm1.GetElementColor(const Category: string): TColor;
begin
  if Category = 'アルカリ金属' then
    Result := $E0E0FF  // 薄い青
  else if Category = 'アルカリ土類金属' then
    Result := $FFE0E0  // 薄い赤
  else if Category = '遷移金属' then
    Result := $E0FFE0  // 薄い緑
  else if Category = '金属' then
    Result := $E0E0E0  // 灰色
  else if Category = '半金属' then
    Result := $E0FFFF  // 薄いシアン
  else if Category = '非金属' then
    Result := $FFFFE0  // 薄い黄
  else if Category = 'ハロゲン' then
    Result := $FFE0FF  // 薄いマゼンタ
  else if Category = '貴ガス' then
    Result := $E0FFE0  // 薄い緑
  else if Category = 'ランタノイド' then
    Result := $FFE0C0  // 薄いオレンジ
  else if Category = 'アクチノイド' then
    Result := $FFC0E0  // 薄いピンク
  else
    Result := clWhite;
end;

procedure TForm1.CreatePeriodicTable;
const
  CELL_WIDTH = 72;
  CELL_HEIGHT = 82;
  SPACING = 2;
var
  i, x, y, Group, Period: Integer;
  LanthanideStart, ActinideStart: Integer;
  LanthanideX, LanthanideY: Integer;
begin
  // 元素ボタンを作成
  for i := 1 to ELEMENT_COUNT do
  begin
    ElementButtons[i] := TElementButton.Create(Self);
    ElementButtons[i].Parent := PanelMain;
    ElementButtons[i].Element := Elements[i];
  end;
  
  // 第1周期
  x := CELL_WIDTH + SPACING;
  y := 20;
  ElementButtons[1].Left := x; ElementButtons[1].Top := y;  // H
  ElementButtons[2].Left := x + (CELL_WIDTH + SPACING); ElementButtons[2].Top := y;  // He
  
  // 第2周期
  y := y + CELL_HEIGHT + SPACING;
  for i := 3 to 10 do
  begin
    Group := Elements[i].Group;
    if Group <= 2 then
      x := Group * (CELL_WIDTH + SPACING)
    else
      x := (Group + 10) * (CELL_WIDTH + SPACING);
    ElementButtons[i].Left := x;
    ElementButtons[i].Top := y;
  end;
  
  // 第3周期
  y := y + CELL_HEIGHT + SPACING;
  for i := 11 to 18 do
  begin
    Group := Elements[i].Group;
    if Group <= 2 then
      x := Group * (CELL_WIDTH + SPACING)
    else
      x := (Group + 10) * (CELL_WIDTH + SPACING);
    ElementButtons[i].Left := x;
    ElementButtons[i].Top := y;
  end;
  
  // 第4周期
  y := y + CELL_HEIGHT + SPACING;
  // K, Ca
  ElementButtons[19].Left := 1 * (CELL_WIDTH + SPACING); ElementButtons[19].Top := y;  // K
  ElementButtons[20].Left := 2 * (CELL_WIDTH + SPACING); ElementButtons[20].Top := y;  // Ca
  // 遷移金属 (Sc-Zn)
  x := 3 * (CELL_WIDTH + SPACING);
  for i := 21 to 30 do
  begin
    ElementButtons[i].Left := x;
    ElementButtons[i].Top := y;
    x := x + CELL_WIDTH + SPACING;
  end;
  // Ga, Ge, As, Se, Br, Kr
  for i := 31 to 36 do
  begin
    Group := Elements[i].Group;
    x := (Group + 10) * (CELL_WIDTH + SPACING);
    ElementButtons[i].Left := x;
    ElementButtons[i].Top := y;
  end;
  
  // 第5周期
  y := y + CELL_HEIGHT + SPACING;
  // Rb, Sr
  ElementButtons[37].Left := 1 * (CELL_WIDTH + SPACING); ElementButtons[37].Top := y;  // Rb
  ElementButtons[38].Left := 2 * (CELL_WIDTH + SPACING); ElementButtons[38].Top := y;  // Sr
  // 遷移金属 (Y-Cd)
  x := 3 * (CELL_WIDTH + SPACING);
  for i := 39 to 48 do
  begin
    ElementButtons[i].Left := x;
    ElementButtons[i].Top := y;
    x := x + CELL_WIDTH + SPACING;
  end;
  // In, Sn, Sb, Te, I, Xe
  for i := 49 to 54 do
  begin
    Group := Elements[i].Group;
    x := (Group + 10) * (CELL_WIDTH + SPACING);
    ElementButtons[i].Left := x;
    ElementButtons[i].Top := y;
  end;
  
  // 第6周期
  y := y + CELL_HEIGHT + SPACING;
  // Cs, Ba
  ElementButtons[55].Left := 1 * (CELL_WIDTH + SPACING); ElementButtons[55].Top := y;  // Cs
  ElementButtons[56].Left := 2 * (CELL_WIDTH + SPACING); ElementButtons[56].Top := y;  // Ba
  // La
  ElementButtons[57].Left := 3 * (CELL_WIDTH + SPACING); ElementButtons[57].Top := y;  // La
  // ランタノイド (Ce-Lu) - 下に配置
  LanthanideX := 4 * (CELL_WIDTH + SPACING);
  LanthanideY := y + CELL_HEIGHT + SPACING;
  for i := 58 to 71 do
  begin
    ElementButtons[i].Left := LanthanideX;
    ElementButtons[i].Top := LanthanideY;
    LanthanideX := LanthanideX + CELL_WIDTH + SPACING;
  end;
  // Hf, Ta, W, Re, Os, Ir, Pt, Au, Hg
  x := 4 * (CELL_WIDTH + SPACING);
  for i := 72 to 80 do
  begin
    ElementButtons[i].Left := x;
    ElementButtons[i].Top := y;
    x := x + CELL_WIDTH + SPACING;
  end;
  // Tl, Pb, Bi, Po, At, Rn
  for i := 81 to 86 do
  begin
    Group := Elements[i].Group;
    x := (Group + 10) * (CELL_WIDTH + SPACING);
    ElementButtons[i].Left := x;
    ElementButtons[i].Top := y;
  end;
  
  // 第7周期
  y := y + CELL_HEIGHT + SPACING;
  // Fr, Ra
  ElementButtons[87].Left := 1 * (CELL_WIDTH + SPACING); ElementButtons[87].Top := y;  // Fr
  ElementButtons[88].Left := 2 * (CELL_WIDTH + SPACING); ElementButtons[88].Top := y;  // Ra
  // Ac
  ElementButtons[89].Left := 3 * (CELL_WIDTH + SPACING); ElementButtons[89].Top := y;  // Ac
  // アクチノイド (Th-Lr) - 下に配置
  ActinideStart := 90;
  LanthanideX := 4 * (CELL_WIDTH + SPACING);
  LanthanideY := y + CELL_HEIGHT + SPACING;
  for i := ActinideStart to 103 do
  begin
    ElementButtons[i].Left := LanthanideX;
    ElementButtons[i].Top := LanthanideY;
    LanthanideX := LanthanideX + CELL_WIDTH + SPACING;
  end;
  // Rf, Db, Sg, Bh, Hs, Mt, Ds, Rg, Cn
  x := 4 * (CELL_WIDTH + SPACING);
  for i := 104 to 112 do
  begin
    ElementButtons[i].Left := x;
    ElementButtons[i].Top := y;
    x := x + CELL_WIDTH + SPACING;
  end;
  // Nh, Fl, Mc, Lv, Ts, Og
  for i := 113 to 118 do
  begin
    Group := Elements[i].Group;
    x := (Group + 10) * (CELL_WIDTH + SPACING);
    ElementButtons[i].Left := x;
    ElementButtons[i].Top := y;
  end;
  
  // パネルのサイズを調整
  PanelMain.Height := LanthanideY + CELL_HEIGHT + 20;
  PanelMain.Width := 19 * (CELL_WIDTH + SPACING) + 20;
end;

end.

