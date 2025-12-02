unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  TCountry = record
    Name: string;
    NameJP: string;
    Code: string; // ISO 3166-1 alpha-2 code
  end;
  
  TIntegerArray = array of Integer;

  TForm1 = class(TForm)
    Image1: TImage;
    LabelQuestion: TLabel;
    LabelScore: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ButtonStart: TButton;
    LabelFeedback: TLabel;
    LabelQuestionNum: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonAnswerClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Countries: array of TCountry;
    CurrentQuestion: Integer;
    CorrectAnswer: Integer;
    Score: Integer;
    TotalQuestions: Integer;
    AnswerButtons: array[0..3] of TButton;
    CurrentCountryIndices: array[0..3] of Integer; // 現在表示中の国のインデックス
    procedure InitializeCountries;
    procedure SetCountry(Index: Integer; AName, ANameJP, ACode: string);
    procedure LoadNextQuestion;
    procedure LoadFlagImage(CountryCode: string);
    function GetRandomCountries(Count: Integer; ExcludeIndex: Integer): TIntegerArray;
    procedure EnableAnswerButtons(AEnabled: Boolean);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize; // ランダム数生成器を初期化
  InitializeCountries;
  AnswerButtons[0] := Button1;
  AnswerButtons[1] := Button2;
  AnswerButtons[2] := Button3;
  AnswerButtons[3] := Button4;
  Score := 0;
  TotalQuestions := 0;
  LabelScore.Caption := 'スコア: 0 / 0';
  LabelQuestionNum.Caption := '';
  LabelFeedback.Caption := '';
  LabelQuestion.Caption := '「開始」ボタンを押してクイズを開始してください';
  EnableAnswerButtons(False);
  Image1.Picture.Clear;
end;

procedure TForm1.SetCountry(Index: Integer; AName, ANameJP, ACode: string);
begin
  Countries[Index].Name := AName;
  Countries[Index].NameJP := ANameJP;
  Countries[Index].Code := ACode;
end;

procedure TForm1.InitializeCountries;
begin
  // 全世界の主要国（196カ国）
  SetLength(Countries, 196);
  
  // アジア
  SetCountry(0, 'Afghanistan', 'アフガニスタン', 'af');
  SetCountry(1, 'Armenia', 'アルメニア', 'am');
  SetCountry(2, 'Azerbaijan', 'アゼルバイジャン', 'az');
  SetCountry(3, 'Bahrain', 'バーレーン', 'bh');
  SetCountry(4, 'Bangladesh', 'バングラデシュ', 'bd');
  SetCountry(5, 'Bhutan', 'ブータン', 'bt');
  SetCountry(6, 'Brunei', 'ブルネイ', 'bn');
  SetCountry(7, 'Cambodia', 'カンボジア', 'kh');
  SetCountry(8, 'China', '中国', 'cn');
  SetCountry(9, 'Cyprus', 'キプロス', 'cy');
  SetCountry(10, 'Georgia', 'ジョージア', 'ge');
  SetCountry(11, 'India', 'インド', 'in');
  SetCountry(12, 'Indonesia', 'インドネシア', 'id');
  SetCountry(13, 'Iran', 'イラン', 'ir');
  SetCountry(14, 'Iraq', 'イラク', 'iq');
  SetCountry(15, 'Israel', 'イスラエル', 'il');
  SetCountry(16, 'Japan', '日本', 'jp');
  SetCountry(17, 'Jordan', 'ヨルダン', 'jo');
  SetCountry(18, 'Kazakhstan', 'カザフスタン', 'kz');
  SetCountry(19, 'Kuwait', 'クウェート', 'kw');
  SetCountry(20, 'Kyrgyzstan', 'キルギス', 'kg');
  SetCountry(21, 'Laos', 'ラオス', 'la');
  SetCountry(22, 'Lebanon', 'レバノン', 'lb');
  SetCountry(23, 'Malaysia', 'マレーシア', 'my');
  SetCountry(24, 'Maldives', 'モルディブ', 'mv');
  SetCountry(25, 'Mongolia', 'モンゴル', 'mn');
  SetCountry(26, 'Myanmar', 'ミャンマー', 'mm');
  SetCountry(27, 'Nepal', 'ネパール', 'np');
  SetCountry(28, 'North Korea', '北朝鮮', 'kp');
  SetCountry(29, 'Oman', 'オマーン', 'om');
  SetCountry(30, 'Pakistan', 'パキスタン', 'pk');
  SetCountry(31, 'Palestine', 'パレスチナ', 'ps');
  SetCountry(32, 'Philippines', 'フィリピン', 'ph');
  SetCountry(33, 'Qatar', 'カタール', 'qa');
  SetCountry(34, 'Saudi Arabia', 'サウジアラビア', 'sa');
  SetCountry(35, 'Singapore', 'シンガポール', 'sg');
  SetCountry(36, 'South Korea', '韓国', 'kr');
  SetCountry(37, 'Sri Lanka', 'スリランカ', 'lk');
  SetCountry(38, 'Syria', 'シリア', 'sy');
  SetCountry(39, 'Taiwan', '台湾', 'tw');
  SetCountry(40, 'Tajikistan', 'タジキスタン', 'tj');
  SetCountry(41, 'Thailand', 'タイ', 'th');
  SetCountry(42, 'Timor-Leste', '東ティモール', 'tl');
  SetCountry(43, 'Turkey', 'トルコ', 'tr');
  SetCountry(44, 'Turkmenistan', 'トルクメニスタン', 'tm');
  SetCountry(45, 'United Arab Emirates', 'アラブ首長国連邦', 'ae');
  SetCountry(46, 'Uzbekistan', 'ウズベキスタン', 'uz');
  SetCountry(47, 'Vietnam', 'ベトナム', 'vn');
  SetCountry(48, 'Yemen', 'イエメン', 'ye');
  
  // ヨーロッパ
  SetCountry(49, 'Albania', 'アルバニア', 'al');
  SetCountry(50, 'Andorra', 'アンドラ', 'ad');
  SetCountry(51, 'Austria', 'オーストリア', 'at');
  SetCountry(52, 'Belarus', 'ベラルーシ', 'by');
  SetCountry(53, 'Belgium', 'ベルギー', 'be');
  SetCountry(54, 'Bosnia and Herzegovina', 'ボスニア・ヘルツェゴビナ', 'ba');
  SetCountry(55, 'Bulgaria', 'ブルガリア', 'bg');
  SetCountry(56, 'Croatia', 'クロアチア', 'hr');
  SetCountry(57, 'Czech Republic', 'チェコ', 'cz');
  SetCountry(58, 'Denmark', 'デンマーク', 'dk');
  SetCountry(59, 'Estonia', 'エストニア', 'ee');
  SetCountry(60, 'Finland', 'フィンランド', 'fi');
  SetCountry(61, 'France', 'フランス', 'fr');
  SetCountry(62, 'Germany', 'ドイツ', 'de');
  SetCountry(63, 'Greece', 'ギリシャ', 'gr');
  SetCountry(64, 'Hungary', 'ハンガリー', 'hu');
  SetCountry(65, 'Iceland', 'アイスランド', 'is');
  SetCountry(66, 'Ireland', 'アイルランド', 'ie');
  SetCountry(67, 'Italy', 'イタリア', 'it');
  SetCountry(68, 'Latvia', 'ラトビア', 'lv');
  SetCountry(69, 'Liechtenstein', 'リヒテンシュタイン', 'li');
  SetCountry(70, 'Lithuania', 'リトアニア', 'lt');
  SetCountry(71, 'Luxembourg', 'ルクセンブルク', 'lu');
  SetCountry(72, 'Malta', 'マルタ', 'mt');
  SetCountry(73, 'Moldova', 'モルドバ', 'md');
  SetCountry(74, 'Monaco', 'モナコ', 'mc');
  SetCountry(75, 'Montenegro', 'モンテネグロ', 'me');
  SetCountry(76, 'Netherlands', 'オランダ', 'nl');
  SetCountry(77, 'North Macedonia', '北マケドニア', 'mk');
  SetCountry(78, 'Norway', 'ノルウェー', 'no');
  SetCountry(79, 'Poland', 'ポーランド', 'pl');
  SetCountry(80, 'Portugal', 'ポルトガル', 'pt');
  SetCountry(81, 'Romania', 'ルーマニア', 'ro');
  SetCountry(82, 'Russia', 'ロシア', 'ru');
  SetCountry(83, 'San Marino', 'サンマリノ', 'sm');
  SetCountry(84, 'Serbia', 'セルビア', 'rs');
  SetCountry(85, 'Slovakia', 'スロバキア', 'sk');
  SetCountry(86, 'Slovenia', 'スロベニア', 'si');
  SetCountry(87, 'Spain', 'スペイン', 'es');
  SetCountry(88, 'Sweden', 'スウェーデン', 'se');
  SetCountry(89, 'Switzerland', 'スイス', 'ch');
  SetCountry(90, 'Ukraine', 'ウクライナ', 'ua');
  SetCountry(91, 'United Kingdom', 'イギリス', 'gb');
  SetCountry(92, 'Vatican City', 'バチカン', 'va');
  
  // アフリカ
  SetCountry(93, 'Algeria', 'アルジェリア', 'dz');
  SetCountry(94, 'Angola', 'アンゴラ', 'ao');
  SetCountry(95, 'Benin', 'ベナン', 'bj');
  SetCountry(96, 'Botswana', 'ボツワナ', 'bw');
  SetCountry(97, 'Burkina Faso', 'ブルキナファソ', 'bf');
  SetCountry(98, 'Burundi', 'ブルンジ', 'bi');
  SetCountry(99, 'Cameroon', 'カメルーン', 'cm');
  SetCountry(100, 'Cape Verde', 'カーボベルデ', 'cv');
  SetCountry(101, 'Central African Republic', '中央アフリカ', 'cf');
  SetCountry(102, 'Chad', 'チャド', 'td');
  SetCountry(103, 'Comoros', 'コモロ', 'km');
  SetCountry(104, 'Congo', 'コンゴ', 'cg');
  SetCountry(105, 'DR Congo', 'コンゴ民主共和国', 'cd');
  SetCountry(106, 'Djibouti', 'ジブチ', 'dj');
  SetCountry(107, 'Egypt', 'エジプト', 'eg');
  SetCountry(108, 'Equatorial Guinea', '赤道ギニア', 'gq');
  SetCountry(109, 'Eritrea', 'エリトリア', 'er');
  SetCountry(110, 'Eswatini', 'エスワティニ', 'sz');
  SetCountry(111, 'Ethiopia', 'エチオピア', 'et');
  SetCountry(112, 'Gabon', 'ガボン', 'ga');
  SetCountry(113, 'Gambia', 'ガンビア', 'gm');
  SetCountry(114, 'Ghana', 'ガーナ', 'gh');
  SetCountry(115, 'Guinea', 'ギニア', 'gn');
  SetCountry(116, 'Guinea-Bissau', 'ギニアビサウ', 'gw');
  SetCountry(117, 'Ivory Coast', 'コートジボワール', 'ci');
  SetCountry(118, 'Kenya', 'ケニア', 'ke');
  SetCountry(119, 'Lesotho', 'レソト', 'ls');
  SetCountry(120, 'Liberia', 'リベリア', 'lr');
  SetCountry(121, 'Libya', 'リビア', 'ly');
  SetCountry(122, 'Madagascar', 'マダガスカル', 'mg');
  SetCountry(123, 'Malawi', 'マラウイ', 'mw');
  SetCountry(124, 'Mali', 'マリ', 'ml');
  SetCountry(125, 'Mauritania', 'モーリタニア', 'mr');
  SetCountry(126, 'Mauritius', 'モーリシャス', 'mu');
  SetCountry(127, 'Morocco', 'モロッコ', 'ma');
  SetCountry(128, 'Mozambique', 'モザンビーク', 'mz');
  SetCountry(129, 'Namibia', 'ナミビア', 'na');
  SetCountry(130, 'Niger', 'ニジェール', 'ne');
  SetCountry(131, 'Nigeria', 'ナイジェリア', 'ng');
  SetCountry(132, 'Rwanda', 'ルワンダ', 'rw');
  SetCountry(133, 'Sao Tome and Principe', 'サントメ・プリンシペ', 'st');
  SetCountry(134, 'Senegal', 'セネガル', 'sn');
  SetCountry(135, 'Seychelles', 'セーシェル', 'sc');
  SetCountry(136, 'Sierra Leone', 'シエラレオネ', 'sl');
  SetCountry(137, 'Somalia', 'ソマリア', 'so');
  SetCountry(138, 'South Africa', '南アフリカ', 'za');
  SetCountry(139, 'South Sudan', '南スーダン', 'ss');
  SetCountry(140, 'Sudan', 'スーダン', 'sd');
  SetCountry(141, 'Tanzania', 'タンザニア', 'tz');
  SetCountry(142, 'Togo', 'トーゴ', 'tg');
  SetCountry(143, 'Tunisia', 'チュニジア', 'tn');
  SetCountry(144, 'Uganda', 'ウガンダ', 'ug');
  SetCountry(145, 'Zambia', 'ザンビア', 'zm');
  SetCountry(146, 'Zimbabwe', 'ジンバブエ', 'zw');
  
  // 北米・中米
  SetCountry(147, 'Antigua and Barbuda', 'アンティグア・バーブーダ', 'ag');
  SetCountry(148, 'Bahamas', 'バハマ', 'bs');
  SetCountry(149, 'Barbados', 'バルバドス', 'bb');
  SetCountry(150, 'Belize', 'ベリーズ', 'bz');
  SetCountry(151, 'Canada', 'カナダ', 'ca');
  SetCountry(152, 'Costa Rica', 'コスタリカ', 'cr');
  SetCountry(153, 'Cuba', 'キューバ', 'cu');
  SetCountry(154, 'Dominica', 'ドミニカ', 'dm');
  SetCountry(155, 'Dominican Republic', 'ドミニカ共和国', 'do');
  SetCountry(156, 'El Salvador', 'エルサルバドル', 'sv');
  SetCountry(157, 'Grenada', 'グレナダ', 'gd');
  SetCountry(158, 'Guatemala', 'グアテマラ', 'gt');
  SetCountry(159, 'Haiti', 'ハイチ', 'ht');
  SetCountry(160, 'Honduras', 'ホンジュラス', 'hn');
  SetCountry(161, 'Jamaica', 'ジャマイカ', 'jm');
  SetCountry(162, 'Mexico', 'メキシコ', 'mx');
  SetCountry(163, 'Nicaragua', 'ニカラグア', 'ni');
  SetCountry(164, 'Panama', 'パナマ', 'pa');
  SetCountry(165, 'Saint Kitts and Nevis', 'セントクリストファー・ネーヴィス', 'kn');
  SetCountry(166, 'Saint Lucia', 'セントルシア', 'lc');
  SetCountry(167, 'Saint Vincent', 'セントビンセント', 'vc');
  SetCountry(168, 'Trinidad and Tobago', 'トリニダード・トバゴ', 'tt');
  SetCountry(169, 'United States', 'アメリカ', 'us');
  
  // 南米
  SetCountry(170, 'Argentina', 'アルゼンチン', 'ar');
  SetCountry(171, 'Bolivia', 'ボリビア', 'bo');
  SetCountry(172, 'Brazil', 'ブラジル', 'br');
  SetCountry(173, 'Chile', 'チリ', 'cl');
  SetCountry(174, 'Colombia', 'コロンビア', 'co');
  SetCountry(175, 'Ecuador', 'エクアドル', 'ec');
  SetCountry(176, 'Guyana', 'ガイアナ', 'gy');
  SetCountry(177, 'Paraguay', 'パラグアイ', 'py');
  SetCountry(178, 'Peru', 'ペルー', 'pe');
  SetCountry(179, 'Suriname', 'スリナム', 'sr');
  SetCountry(180, 'Uruguay', 'ウルグアイ', 'uy');
  SetCountry(181, 'Venezuela', 'ベネズエラ', 've');
  
  // オセアニア
  SetCountry(182, 'Australia', 'オーストラリア', 'au');
  SetCountry(183, 'Fiji', 'フィジー', 'fj');
  SetCountry(184, 'Kiribati', 'キリバス', 'ki');
  SetCountry(185, 'Marshall Islands', 'マーシャル諸島', 'mh');
  SetCountry(186, 'Micronesia', 'ミクロネシア', 'fm');
  SetCountry(187, 'Nauru', 'ナウル', 'nr');
  SetCountry(188, 'New Zealand', 'ニュージーランド', 'nz');
  SetCountry(189, 'Palau', 'パラオ', 'pw');
  SetCountry(190, 'Papua New Guinea', 'パプアニューギニア', 'pg');
  SetCountry(191, 'Samoa', 'サモア', 'ws');
  SetCountry(192, 'Solomon Islands', 'ソロモン諸島', 'sb');
  SetCountry(193, 'Tonga', 'トンガ', 'to');
  SetCountry(194, 'Tuvalu', 'ツバル', 'tv');
  SetCountry(195, 'Vanuatu', 'バヌアツ', 'vu');
end;

procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  Score := 0;
  TotalQuestions := 0;
  LabelScore.Caption := 'スコア: 0 / 0';
  ButtonStart.Caption := '新しいクイズ';
  LoadNextQuestion;
end;

procedure TForm1.LoadNextQuestion;
var
  AnswerIndices: TIntegerArray;
  ShuffledIndices: TIntegerArray;
  i, j, temp: Integer;
begin
  if Length(Countries) = 0 then Exit;
  
  // ランダムに正解の国を選ぶ
  CorrectAnswer := Random(Length(Countries));
  
  // 正解を含む4つの選択肢を選ぶ
  AnswerIndices := GetRandomCountries(3, CorrectAnswer);
  SetLength(ShuffledIndices, 4);
  ShuffledIndices[0] := CorrectAnswer;
  for i := 0 to 2 do
    ShuffledIndices[i + 1] := AnswerIndices[i];
  
  // 選択肢をシャッフル
  for i := 3 downto 1 do
  begin
    j := Random(i + 1);
    temp := ShuffledIndices[i];
    ShuffledIndices[i] := ShuffledIndices[j];
    ShuffledIndices[j] := temp;
  end;
  
  // 正解のインデックスを更新
  for i := 0 to 3 do
    if ShuffledIndices[i] = CorrectAnswer then
    begin
      CorrectAnswer := i;
      Break;
    end;
  
  // ボタンに国名を設定し、インデックスを保存
  for i := 0 to 3 do
  begin
    AnswerButtons[i].Caption := Countries[ShuffledIndices[i]].NameJP;
    CurrentCountryIndices[i] := ShuffledIndices[i];
  end;
  
  // 国旗を読み込む（正解の国の国旗）
  LoadFlagImage(Countries[CurrentCountryIndices[CorrectAnswer]].Code);
  
  // UI更新
  Inc(TotalQuestions);
  LabelQuestionNum.Caption := Format('問題 %d', [TotalQuestions]);
  LabelQuestion.Caption := 'この国旗はどこの国ですか？';
  LabelFeedback.Caption := '';
  EnableAnswerButtons(True);
end;



procedure TForm1.LoadFlagImage(CountryCode: string);
var
  FilePath: string;
begin
  try
    // w2560フォルダ内のBMPファイルを読み込む
    FilePath := Format('w2560\%s.bmp', [LowerCase(CountryCode)]);
    
    if FileExists(FilePath) then
    begin
      Image1.Picture.LoadFromFile(FilePath);
    end
    else
    begin
      // ファイルが見つからない場合
      LabelFeedback.Caption := Format('国旗画像ファイルが見つかりません: %s', [FilePath]);
      Image1.Picture.Clear;
    end;
  except
    on E: Exception do
    begin
      // エラー時はメッセージを表示
      LabelFeedback.Caption := '国旗画像の読み込みに失敗しました: ' + E.Message;
      Image1.Picture.Clear;
    end;
  end;
end;

function TForm1.GetRandomCountries(Count: Integer; ExcludeIndex: Integer): TIntegerArray;
var
  i, j: Integer;
  Used: array of Boolean;
  ResultArray: TIntegerArray;
  RandomIndex: Integer;
begin
  SetLength(Used, Length(Countries));
  SetLength(ResultArray, Count);
  
  for i := 0 to Count - 1 do
  begin
    repeat
      RandomIndex := Random(Length(Countries));
    until (RandomIndex <> ExcludeIndex) and not Used[RandomIndex];
    
    Used[RandomIndex] := True;
    ResultArray[i] := RandomIndex;
  end;
  
  Result := ResultArray;
end;

procedure TForm1.ButtonAnswerClick(Sender: TObject);
var
  SelectedButton: TButton;
  SelectedIndex: Integer;
  i: Integer;
begin
  SelectedButton := Sender as TButton;
  
  // どのボタンが押されたか確認
  SelectedIndex := -1;
  for i := 0 to 3 do
    if AnswerButtons[i] = SelectedButton then
    begin
      SelectedIndex := i;
      Break;
    end;
  
  if SelectedIndex = -1 then Exit;
  
  EnableAnswerButtons(False);
  
  // 正誤判定
  if SelectedIndex = CorrectAnswer then
  begin
    Inc(Score);
    LabelFeedback.Caption := '正解！ ✓';
    LabelFeedback.Font.Color := clGreen;
  end
  else
  begin
    LabelFeedback.Caption := Format('不正解。正解は「%s」です。', 
      [Countries[CurrentCountryIndices[CorrectAnswer]].NameJP]);
    LabelFeedback.Font.Color := clRed;
  end;
  
  LabelScore.Caption := Format('スコア: %d / %d', [Score, TotalQuestions]);
  
  // 2秒後に次の問題へ
  Application.ProcessMessages;
  Sleep(2000);
  LoadNextQuestion;
end;

procedure TForm1.EnableAnswerButtons(AEnabled: Boolean);
var
  i: Integer;
begin
  for i := 0 to 3 do
    AnswerButtons[i].Enabled := AEnabled;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // クリーンアップ
end;

end.
