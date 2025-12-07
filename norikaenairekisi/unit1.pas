unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  TNihonshiEvent = record
    Year: Integer;
    Person: String;
    Event: String;
  end;
  
  TNihonshiEventArray = array of TNihonshiEvent;

  TForm1 = class(TForm)
    ScrollBox1: TScrollBox;
    procedure FormCreate(Sender: TObject);
  private
    procedure CreateTimelineItem(const AEvent: TNihonshiEvent; var TopPos: Integer);
    function GetNihonshiEvents: TNihonshiEventArray;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  Events: TNihonshiEventArray;
  i: Integer;
  TopPos: Integer;
  TitleLabel: TLabel;
begin
  // タイトルを追加
  TitleLabel := TLabel.Create(Self);
  TitleLabel.Parent := ScrollBox1;
  TitleLabel.Left := 30;
  TitleLabel.Top := 20;
  TitleLabel.Font.Size := 18;
  TitleLabel.Font.Style := [fsBold];
  TitleLabel.Font.Color := clNavy;
  TitleLabel.Caption := '日本史年表';
  TitleLabel.AutoSize := True;
  
  Events := GetNihonshiEvents;
  TopPos := 60;
  
  for i := 0 to Length(Events) - 1 do
  begin
    CreateTimelineItem(Events[i], TopPos);
  end;
  
  // 最後に余白を追加
  TopPos := TopPos + 20;
end;

procedure TForm1.CreateTimelineItem(const AEvent: TNihonshiEvent; var TopPos: Integer);
var
  YearLabel: TLabel;
  PersonLabel: TLabel;
  EventLabel: TLabel;
  LinePanel: TPanel;
  Margin: Integer;
  ItemPanel: TPanel;
  MaxHeight: Integer;
begin
  Margin := 30;
  
  // 各項目を囲むパネル（背景色付き）
  ItemPanel := TPanel.Create(Self);
  ItemPanel.Parent := ScrollBox1;
  ItemPanel.Left := Margin;
  ItemPanel.Top := TopPos;
  ItemPanel.Width := ScrollBox1.ClientWidth - Margin * 2;
  ItemPanel.Height := 45;
  ItemPanel.BevelOuter := bvNone;
  ItemPanel.Color := clWhite;
  ItemPanel.Anchors := [akLeft, akTop, akRight];
  
  // 年代ラベル
  YearLabel := TLabel.Create(Self);
  YearLabel.Parent := ItemPanel;
  YearLabel.Left := 10;
  YearLabel.Top := 8;
  YearLabel.Font.Size := 14;
  YearLabel.Font.Style := [fsBold];
  YearLabel.Font.Color := clNavy;
  YearLabel.Caption := IntToStr(AEvent.Year) + '年';
  YearLabel.AutoSize := True;
  
  MaxHeight := YearLabel.Height;
  
  // 人物ラベル（人物が存在する場合）
  if AEvent.Person <> '' then
  begin
    PersonLabel := TLabel.Create(Self);
    PersonLabel.Parent := ItemPanel;
    PersonLabel.Left := 120;
    PersonLabel.Top := 8;
    PersonLabel.Font.Size := 12;
    PersonLabel.Font.Style := [fsBold];
    PersonLabel.Font.Color := clMaroon;
    PersonLabel.Caption := AEvent.Person + '：';
    PersonLabel.AutoSize := True;
    if PersonLabel.Height > MaxHeight then
      MaxHeight := PersonLabel.Height;
  end;
  
  // 出来事ラベル
  EventLabel := TLabel.Create(Self);
  EventLabel.Parent := ItemPanel;
  if AEvent.Person <> '' then
    EventLabel.Left := 280
  else
    EventLabel.Left := 120;
  EventLabel.Top := 8;
  EventLabel.Font.Size := 12;
  EventLabel.Caption := AEvent.Event;
  EventLabel.AutoSize := False;
  EventLabel.WordWrap := True;
  EventLabel.Width := ItemPanel.Width - EventLabel.Left - 10;
  EventLabel.Height := 30;
  
  // パネルの高さを調整
  if EventLabel.Height + 16 > MaxHeight + 16 then
    ItemPanel.Height := EventLabel.Height + 16
  else
    ItemPanel.Height := MaxHeight + 16;
  
  // 区切り線
  LinePanel := TPanel.Create(Self);
  LinePanel.Parent := ScrollBox1;
  LinePanel.Left := Margin;
  LinePanel.Top := TopPos + ItemPanel.Height;
  LinePanel.Width := ScrollBox1.ClientWidth - Margin * 2;
  LinePanel.Height := 1;
  LinePanel.BevelOuter := bvNone;
  LinePanel.Color := $00E0E0E0;
  LinePanel.Anchors := [akLeft, akTop, akRight];
  
  // 次の位置を計算
  TopPos := TopPos + ItemPanel.Height + 5;
end;

function TForm1.GetNihonshiEvents: TNihonshiEventArray;
begin
  Result := nil;
  SetLength(Result, 50);
  
  // 飛鳥時代
  Result[0].Year := 593;
  Result[0].Person := '聖徳太子';
  Result[0].Event := '冠位十二階・十七条の憲法';
  
  Result[1].Year := 645;
  Result[1].Person := '';
  Result[1].Event := '大化の改新';
  
  // 奈良時代
  Result[2].Year := 710;
  Result[2].Person := '';
  Result[2].Event := '平城京遷都';
  
  Result[3].Year := 794;
  Result[3].Person := '';
  Result[3].Event := '平安京遷都（平安時代開始）';
  
  // 平安時代
  Result[4].Year := 894;
  Result[4].Person := '菅原道真';
  Result[4].Event := '遣唐使廃止';
  
  Result[5].Year := 1016;
  Result[5].Person := '藤原道長';
  Result[5].Event := '摂政就任';
  
  Result[6].Year := 1086;
  Result[6].Person := '白河上皇';
  Result[6].Event := '院政開始';
  
  Result[7].Year := 1156;
  Result[7].Person := '';
  Result[7].Event := '保元の乱';
  
  Result[8].Year := 1159;
  Result[8].Person := '';
  Result[8].Event := '平治の乱';
  
  Result[9].Year := 1185;
  Result[9].Person := '源頼朝';
  Result[9].Event := '壇ノ浦の戦い（平氏滅亡）';
  
  // 鎌倉時代
  Result[10].Year := 1192;
  Result[10].Person := '源頼朝';
  Result[10].Event := '征夷大将軍に就任（鎌倉幕府成立）';
  
  Result[11].Year := 1221;
  Result[11].Person := '';
  Result[11].Event := '承久の乱';
  
  Result[12].Year := 1274;
  Result[12].Person := '';
  Result[12].Event := '文永の役（元寇）';
  
  Result[13].Year := 1281;
  Result[13].Person := '';
  Result[13].Event := '弘安の役（元寇）';
  
  Result[14].Year := 1333;
  Result[14].Person := '';
  Result[14].Event := '鎌倉幕府滅亡';
  
  // 室町時代
  Result[15].Year := 1338;
  Result[15].Person := '足利尊氏';
  Result[15].Event := '室町幕府成立';
  
  Result[16].Year := 1392;
  Result[16].Person := '';
  Result[16].Event := '南北朝統一';
  
  Result[17].Year := 1467;
  Result[17].Person := '';
  Result[17].Event := '応仁の乱';
  
  Result[18].Year := 1573;
  Result[18].Person := '';
  Result[18].Event := '室町幕府滅亡';
  
  // 戦国時代・安土桃山時代
  Result[19].Year := 1543;
  Result[19].Person := '';
  Result[19].Event := '鉄砲伝来';
  
  Result[20].Year := 1549;
  Result[20].Person := 'フランシスコ・ザビエル';
  Result[20].Event := 'キリスト教伝来';
  
  Result[21].Year := 1575;
  Result[21].Person := '織田信長';
  Result[21].Event := '長篠の戦い';
  
  Result[22].Year := 1582;
  Result[22].Person := '織田信長';
  Result[22].Event := '本能寺の変';
  
  Result[23].Year := 1590;
  Result[23].Person := '豊臣秀吉';
  Result[23].Event := '天下統一';
  
  Result[24].Year := 1592;
  Result[24].Person := '豊臣秀吉';
  Result[24].Event := '文禄の役（朝鮮出兵）';
  
  Result[25].Year := 1597;
  Result[25].Person := '豊臣秀吉';
  Result[25].Event := '慶長の役（朝鮮出兵）';
  
  // 江戸時代
  Result[26].Year := 1600;
  Result[26].Person := '徳川家康';
  Result[26].Event := '関ヶ原の戦い';
  
  Result[27].Year := 1603;
  Result[27].Person := '徳川家康';
  Result[27].Event := '江戸幕府成立';
  
  Result[28].Year := 1615;
  Result[28].Person := '';
  Result[28].Event := '大坂夏の陣（豊臣氏滅亡）';
  
  Result[29].Year := 1635;
  Result[29].Person := '';
  Result[29].Event := '参勤交代制度';
  
  Result[30].Year := 1639;
  Result[30].Person := '';
  Result[30].Event := '鎖国完成';
  
  Result[31].Year := 1702;
  Result[31].Person := '';
  Result[31].Event := '赤穂事件（忠臣蔵）';
  
  Result[32].Year := 1853;
  Result[32].Person := 'ペリー';
  Result[32].Event := '黒船来航';
  
  Result[33].Year := 1854;
  Result[33].Person := '';
  Result[33].Event := '日米和親条約';
  
  Result[34].Year := 1858;
  Result[34].Person := '';
  Result[34].Event := '日米修好通商条約';
  
  Result[35].Year := 1867;
  Result[35].Person := '徳川慶喜';
  Result[35].Event := '大政奉還';
  
  Result[36].Year := 1868;
  Result[36].Person := '';
  Result[36].Event := '戊辰戦争・明治維新';
  
  // 明治時代
  Result[37].Year := 1868;
  Result[37].Person := '';
  Result[37].Event := '明治時代開始';
  
  Result[38].Year := 1871;
  Result[38].Person := '';
  Result[38].Event := '廃藩置県';
  
  Result[39].Year := 1889;
  Result[39].Person := '';
  Result[39].Event := '大日本帝国憲法公布';
  
  Result[40].Year := 1894;
  Result[40].Person := '';
  Result[40].Event := '日清戦争';
  
  Result[41].Year := 1904;
  Result[41].Person := '';
  Result[41].Event := '日露戦争';
  
  Result[42].Year := 1910;
  Result[42].Person := '';
  Result[42].Event := '韓国併合';
  
  Result[43].Year := 1912;
  Result[43].Person := '';
  Result[43].Event := '明治時代終了';
  
  // 大正時代
  Result[44].Year := 1912;
  Result[44].Person := '';
  Result[44].Event := '大正時代開始';
  
  Result[45].Year := 1923;
  Result[45].Person := '';
  Result[45].Event := '関東大震災';
  
  Result[46].Year := 1926;
  Result[46].Person := '';
  Result[46].Event := '大正時代終了';
  
  // 昭和時代
  Result[47].Year := 1926;
  Result[47].Person := '';
  Result[47].Event := '昭和時代開始';
  
  Result[48].Year := 1945;
  Result[48].Person := '';
  Result[48].Event := '終戦（第二次世界大戦終了）';
  
  Result[49].Year := 1989;
  Result[49].Person := '';
  Result[49].Event := '昭和時代終了・平成時代開始';
end;

end.

