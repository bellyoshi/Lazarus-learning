unit PeriodicTableData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TElement = record
    AtomicNumber: Integer;
    Symbol: string;
    Name: string;
    NameJP: string;
    AtomicWeight: string;
    Group: Integer;
    Period: Integer;
    Category: string;
  end;

const
  ELEMENT_COUNT = 118;

var
  Elements: array[1..ELEMENT_COUNT] of TElement;

procedure InitializeElements;

implementation

procedure SetElement(Index: Integer; ANum: Integer; ASym, AName, ANameJP, AWeight: string; AGroup, APeriod: Integer; ACat: string);
begin
  Elements[Index].AtomicNumber := ANum;
  Elements[Index].Symbol := ASym;
  Elements[Index].Name := AName;
  Elements[Index].NameJP := ANameJP;
  Elements[Index].AtomicWeight := AWeight;
  Elements[Index].Group := AGroup;
  Elements[Index].Period := APeriod;
  Elements[Index].Category := ACat;
end;

procedure InitializeElements;
begin
  // 1-20: 主要元素
  SetElement(1, 1, 'H', 'Hydrogen', '水素', '1.008', 1, 1, '非金属');
  SetElement(2, 2, 'He', 'Helium', 'ヘリウム', '4.003', 18, 1, '貴ガス');
  SetElement(3, 3, 'Li', 'Lithium', 'リチウム', '6.941', 1, 2, 'アルカリ金属');
  SetElement(4, 4, 'Be', 'Beryllium', 'ベリリウム', '9.012', 2, 2, 'アルカリ土類金属');
  SetElement(5, 5, 'B', 'Boron', 'ホウ素', '10.81', 13, 2, '半金属');
  SetElement(6, 6, 'C', 'Carbon', '炭素', '12.01', 14, 2, '非金属');
  SetElement(7, 7, 'N', 'Nitrogen', '窒素', '14.01', 15, 2, '非金属');
  SetElement(8, 8, 'O', 'Oxygen', '酸素', '16.00', 16, 2, '非金属');
  SetElement(9, 9, 'F', 'Fluorine', 'フッ素', '19.00', 17, 2, 'ハロゲン');
  SetElement(10, 10, 'Ne', 'Neon', 'ネオン', '20.18', 18, 2, '貴ガス');
  SetElement(11, 11, 'Na', 'Sodium', 'ナトリウム', '22.99', 1, 3, 'アルカリ金属');
  SetElement(12, 12, 'Mg', 'Magnesium', 'マグネシウム', '24.31', 2, 3, 'アルカリ土類金属');
  SetElement(13, 13, 'Al', 'Aluminum', 'アルミニウム', '26.98', 13, 3, '金属');
  SetElement(14, 14, 'Si', 'Silicon', 'ケイ素', '28.09', 14, 3, '半金属');
  SetElement(15, 15, 'P', 'Phosphorus', 'リン', '30.97', 15, 3, '非金属');
  SetElement(16, 16, 'S', 'Sulfur', '硫黄', '32.07', 16, 3, '非金属');
  SetElement(17, 17, 'Cl', 'Chlorine', '塩素', '35.45', 17, 3, 'ハロゲン');
  SetElement(18, 18, 'Ar', 'Argon', 'アルゴン', '39.95', 18, 3, '貴ガス');
  SetElement(19, 19, 'K', 'Potassium', 'カリウム', '39.10', 1, 4, 'アルカリ金属');
  SetElement(20, 20, 'Ca', 'Calcium', 'カルシウム', '40.08', 2, 4, 'アルカリ土類金属');
  
  // 21-30: 遷移金属
  SetElement(21, 21, 'Sc', 'Scandium', 'スカンジウム', '44.96', 3, 4, '遷移金属');
  SetElement(22, 22, 'Ti', 'Titanium', 'チタン', '47.87', 4, 4, '遷移金属');
  SetElement(23, 23, 'V', 'Vanadium', 'バナジウム', '50.94', 5, 4, '遷移金属');
  SetElement(24, 24, 'Cr', 'Chromium', 'クロム', '52.00', 6, 4, '遷移金属');
  SetElement(25, 25, 'Mn', 'Manganese', 'マンガン', '54.94', 7, 4, '遷移金属');
  SetElement(26, 26, 'Fe', 'Iron', '鉄', '55.85', 8, 4, '遷移金属');
  SetElement(27, 27, 'Co', 'Cobalt', 'コバルト', '58.93', 9, 4, '遷移金属');
  SetElement(28, 28, 'Ni', 'Nickel', 'ニッケル', '58.69', 10, 4, '遷移金属');
  SetElement(29, 29, 'Cu', 'Copper', '銅', '63.55', 11, 4, '遷移金属');
  SetElement(30, 30, 'Zn', 'Zinc', '亜鉛', '65.38', 12, 4, '遷移金属');
  
  // 31-36: 第4周期のpブロック
  SetElement(31, 31, 'Ga', 'Gallium', 'ガリウム', '69.72', 13, 4, '金属');
  SetElement(32, 32, 'Ge', 'Germanium', 'ゲルマニウム', '72.64', 14, 4, '半金属');
  SetElement(33, 33, 'As', 'Arsenic', 'ヒ素', '74.92', 15, 4, '半金属');
  SetElement(34, 34, 'Se', 'Selenium', 'セレン', '78.96', 16, 4, '非金属');
  SetElement(35, 35, 'Br', 'Bromine', '臭素', '79.90', 17, 4, 'ハロゲン');
  SetElement(36, 36, 'Kr', 'Krypton', 'クリプトン', '83.80', 18, 4, '貴ガス');
  
  // 37-54: 第5周期
  SetElement(37, 37, 'Rb', 'Rubidium', 'ルビジウム', '85.47', 1, 5, 'アルカリ金属');
  SetElement(38, 38, 'Sr', 'Strontium', 'ストロンチウム', '87.62', 2, 5, 'アルカリ土類金属');
  SetElement(39, 39, 'Y', 'Yttrium', 'イットリウム', '88.91', 3, 5, '遷移金属');
  SetElement(40, 40, 'Zr', 'Zirconium', 'ジルコニウム', '91.22', 4, 5, '遷移金属');
  SetElement(41, 41, 'Nb', 'Niobium', 'ニオブ', '92.91', 5, 5, '遷移金属');
  SetElement(42, 42, 'Mo', 'Molybdenum', 'モリブデン', '95.96', 6, 5, '遷移金属');
  SetElement(43, 43, 'Tc', 'Technetium', 'テクネチウム', '[98]', 7, 5, '遷移金属');
  SetElement(44, 44, 'Ru', 'Ruthenium', 'ルテニウム', '101.1', 8, 5, '遷移金属');
  SetElement(45, 45, 'Rh', 'Rhodium', 'ロジウム', '102.9', 9, 5, '遷移金属');
  SetElement(46, 46, 'Pd', 'Palladium', 'パラジウム', '106.4', 10, 5, '遷移金属');
  SetElement(47, 47, 'Ag', 'Silver', '銀', '107.9', 11, 5, '遷移金属');
  SetElement(48, 48, 'Cd', 'Cadmium', 'カドミウム', '112.4', 12, 5, '遷移金属');
  SetElement(49, 49, 'In', 'Indium', 'インジウム', '114.8', 13, 5, '金属');
  SetElement(50, 50, 'Sn', 'Tin', 'スズ', '118.7', 14, 5, '金属');
  SetElement(51, 51, 'Sb', 'Antimony', 'アンチモン', '121.8', 15, 5, '半金属');
  SetElement(52, 52, 'Te', 'Tellurium', 'テルル', '127.6', 16, 5, '半金属');
  SetElement(53, 53, 'I', 'Iodine', 'ヨウ素', '126.9', 17, 5, 'ハロゲン');
  SetElement(54, 54, 'Xe', 'Xenon', 'キセノン', '131.3', 18, 5, '貴ガス');
  
  // 55-86: 第6周期（ランタノイド含む）
  SetElement(55, 55, 'Cs', 'Cesium', 'セシウム', '132.9', 1, 6, 'アルカリ金属');
  SetElement(56, 56, 'Ba', 'Barium', 'バリウム', '137.3', 2, 6, 'アルカリ土類金属');
  SetElement(57, 57, 'La', 'Lanthanum', 'ランタン', '138.9', 3, 6, 'ランタノイド');
  SetElement(58, 58, 'Ce', 'Cerium', 'セリウム', '140.1', 0, 0, 'ランタノイド');
  SetElement(59, 59, 'Pr', 'Praseodymium', 'プラセオジム', '140.9', 0, 0, 'ランタノイド');
  SetElement(60, 60, 'Nd', 'Neodymium', 'ネオジム', '144.2', 0, 0, 'ランタノイド');
  SetElement(61, 61, 'Pm', 'Promethium', 'プロメチウム', '[145]', 0, 0, 'ランタノイド');
  SetElement(62, 62, 'Sm', 'Samarium', 'サマリウム', '150.4', 0, 0, 'ランタノイド');
  SetElement(63, 63, 'Eu', 'Europium', 'ユウロピウム', '152.0', 0, 0, 'ランタノイド');
  SetElement(64, 64, 'Gd', 'Gadolinium', 'ガドリニウム', '157.3', 0, 0, 'ランタノイド');
  SetElement(65, 65, 'Tb', 'Terbium', 'テルビウム', '158.9', 0, 0, 'ランタノイド');
  SetElement(66, 66, 'Dy', 'Dysprosium', 'ジスプロシウム', '162.5', 0, 0, 'ランタノイド');
  SetElement(67, 67, 'Ho', 'Holmium', 'ホルミウム', '164.9', 0, 0, 'ランタノイド');
  SetElement(68, 68, 'Er', 'Erbium', 'エルビウム', '167.3', 0, 0, 'ランタノイド');
  SetElement(69, 69, 'Tm', 'Thulium', 'ツリウム', '168.9', 0, 0, 'ランタノイド');
  SetElement(70, 70, 'Yb', 'Ytterbium', 'イッテルビウム', '173.0', 0, 0, 'ランタノイド');
  SetElement(71, 71, 'Lu', 'Lutetium', 'ルテチウム', '175.0', 3, 6, 'ランタノイド');
  SetElement(72, 72, 'Hf', 'Hafnium', 'ハフニウム', '178.5', 4, 6, '遷移金属');
  SetElement(73, 73, 'Ta', 'Tantalum', 'タンタル', '180.9', 5, 6, '遷移金属');
  SetElement(74, 74, 'W', 'Tungsten', 'タングステン', '184.8', 6, 6, '遷移金属');
  SetElement(75, 75, 'Re', 'Rhenium', 'レニウム', '186.2', 7, 6, '遷移金属');
  SetElement(76, 76, 'Os', 'Osmium', 'オスミウム', '190.2', 8, 6, '遷移金属');
  SetElement(77, 77, 'Ir', 'Iridium', 'イリジウム', '192.2', 9, 6, '遷移金属');
  SetElement(78, 78, 'Pt', 'Platinum', '白金', '195.1', 10, 6, '遷移金属');
  SetElement(79, 79, 'Au', 'Gold', '金', '197.0', 11, 6, '遷移金属');
  SetElement(80, 80, 'Hg', 'Mercury', '水銀', '200.6', 12, 6, '遷移金属');
  SetElement(81, 81, 'Tl', 'Thallium', 'タリウム', '204.4', 13, 6, '金属');
  SetElement(82, 82, 'Pb', 'Lead', '鉛', '207.2', 14, 6, '金属');
  SetElement(83, 83, 'Bi', 'Bismuth', 'ビスマス', '208.9', 15, 6, '金属');
  SetElement(84, 84, 'Po', 'Polonium', 'ポロニウム', '[209]', 16, 6, '金属');
  SetElement(85, 85, 'At', 'Astatine', 'アスタチン', '[210]', 17, 6, 'ハロゲン');
  SetElement(86, 86, 'Rn', 'Radon', 'ラドン', '[222]', 18, 6, '貴ガス');
  
  // 87-118: 第7周期（アクチノイド含む）
  SetElement(87, 87, 'Fr', 'Francium', 'フランシウム', '[223]', 1, 7, 'アルカリ金属');
  SetElement(88, 88, 'Ra', 'Radium', 'ラジウム', '[226]', 2, 7, 'アルカリ土類金属');
  SetElement(89, 89, 'Ac', 'Actinium', 'アクチニウム', '[227]', 3, 7, 'アクチノイド');
  SetElement(90, 90, 'Th', 'Thorium', 'トリウム', '232.0', 0, 0, 'アクチノイド');
  SetElement(91, 91, 'Pa', 'Protactinium', 'プロトアクチニウム', '231.0', 0, 0, 'アクチノイド');
  SetElement(92, 92, 'U', 'Uranium', 'ウラン', '238.0', 0, 0, 'アクチノイド');
  SetElement(93, 93, 'Np', 'Neptunium', 'ネプツニウム', '[237]', 0, 0, 'アクチノイド');
  SetElement(94, 94, 'Pu', 'Plutonium', 'プルトニウム', '[244]', 0, 0, 'アクチノイド');
  SetElement(95, 95, 'Am', 'Americium', 'アメリシウム', '[243]', 0, 0, 'アクチノイド');
  SetElement(96, 96, 'Cm', 'Curium', 'キュリウム', '[247]', 0, 0, 'アクチノイド');
  SetElement(97, 97, 'Bk', 'Berkelium', 'バークリウム', '[247]', 0, 0, 'アクチノイド');
  SetElement(98, 98, 'Cf', 'Californium', 'カリホルニウム', '[251]', 0, 0, 'アクチノイド');
  SetElement(99, 99, 'Es', 'Einsteinium', 'アインスタイニウム', '[252]', 0, 0, 'アクチノイド');
  SetElement(100, 100, 'Fm', 'Fermium', 'フェルミウム', '[257]', 0, 0, 'アクチノイド');
  SetElement(101, 101, 'Md', 'Mendelevium', 'メンデレビウム', '[258]', 0, 0, 'アクチノイド');
  SetElement(102, 102, 'No', 'Nobelium', 'ノーベリウム', '[259]', 0, 0, 'アクチノイド');
  SetElement(103, 103, 'Lr', 'Lawrencium', 'ローレンシウム', '[266]', 3, 7, 'アクチノイド');
  SetElement(104, 104, 'Rf', 'Rutherfordium', 'ラザホージウム', '[267]', 4, 7, '遷移金属');
  SetElement(105, 105, 'Db', 'Dubnium', 'ドブニウム', '[268]', 5, 7, '遷移金属');
  SetElement(106, 106, 'Sg', 'Seaborgium', 'シーボーギウム', '[269]', 6, 7, '遷移金属');
  SetElement(107, 107, 'Bh', 'Bohrium', 'ボーリウム', '[270]', 7, 7, '遷移金属');
  SetElement(108, 108, 'Hs', 'Hassium', 'ハッシウム', '[277]', 8, 7, '遷移金属');
  SetElement(109, 109, 'Mt', 'Meitnerium', 'マイトネリウム', '[278]', 9, 7, '遷移金属');
  SetElement(110, 110, 'Ds', 'Darmstadtium', 'ダームスタチウム', '[281]', 10, 7, '遷移金属');
  SetElement(111, 111, 'Rg', 'Roentgenium', 'レントゲニウム', '[282]', 11, 7, '遷移金属');
  SetElement(112, 112, 'Cn', 'Copernicium', 'コペルニシウム', '[285]', 12, 7, '遷移金属');
  SetElement(113, 113, 'Nh', 'Nihonium', 'ニホニウム', '[286]', 13, 7, '金属');
  SetElement(114, 114, 'Fl', 'Flerovium', 'フレロビウム', '[289]', 14, 7, '金属');
  SetElement(115, 115, 'Mc', 'Moscovium', 'モスコビウム', '[290]', 15, 7, '金属');
  SetElement(116, 116, 'Lv', 'Livermorium', 'リバモリウム', '[293]', 16, 7, '金属');
  SetElement(117, 117, 'Ts', 'Tennessine', 'テネシン', '[294]', 17, 7, 'ハロゲン');
  SetElement(118, 118, 'Og', 'Oganesson', 'オガネソン', '[294]', 18, 7, '貴ガス');
end;

initialization
  InitializeElements;

end.

