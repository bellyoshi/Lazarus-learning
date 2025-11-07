unit ShogiUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // 駒の種類
  TKomaKind = (
    kkNone,        // なし
    kkOu,          // 王
    kkGyoku,       // 玉
    kkHisha,       // 飛車
    kkKaku,        // 角
    kkKin,         // 金
    kkGin,         // 銀
    kkKei,         // 桂
    kkKyo,         // 香
    kkFu,          // 歩
    kkRyu,         // 龍（飛車の成り）
    kkUma,         // 馬（角の成り）
    kkNariGin,     // 成銀
    kkNariKei,     // 成桂
    kkNariKyo,     // 成香
    kkTo           // と（歩の成り）
  );

  // 手番
  TTurn = (ttSente, ttGote); // 先手、後手

  // 駒の情報
  TKoma = record
    Kind: TKomaKind;
    IsSente: Boolean;  // 先手の駒かどうか
    IsNari: Boolean;   // 成っているかどうか
  end;

  // 盤面（9x9）
  TBoard = array[1..9, 1..9] of TKoma;

  // 持ち駒
  TMochigoma = record
    Hisha: Integer;
    Kaku: Integer;
    Kin: Integer;
    Gin: Integer;
    Kei: Integer;
    Kyo: Integer;
    Fu: Integer;
  end;

  // 将棋ゲームクラス
  TShogiGame = class
  private
    FBoard: TBoard;
    FTurn: TTurn;
    FSenteMochigoma: TMochigoma;
    FGoteMochigoma: TMochigoma;
    procedure InitializeBoard;
  private
    function CheckPath(FromX, FromY, ToX, ToY: Integer): Boolean;
    function CheckKomaMove(Koma: TKoma; FromX, FromY, ToX, ToY: Integer): Boolean;
    procedure AddMochigoma(Koma: TKoma);
    function CheckNifu(X: Integer; IsSente: Boolean): Boolean;
    procedure NaruKoma(var Koma: TKoma);
    function CanMoveFrom(Kind: TKomaKind; IsSente: Boolean; FromY: Integer): Boolean;
  public
    constructor Create;
    function MustNaru(Koma: TKoma; ToY: Integer): Boolean;
    function GetKoma(X, Y: Integer): TKoma;
    function IsValidMove(FromX, FromY, ToX, ToY: Integer): Boolean;
    function IsValidDrop(Kind: TKomaKind; ToX, ToY: Integer): Boolean;
    function MoveKoma(FromX, FromY, ToX, ToY: Integer; DoNaru: Boolean = False): Boolean;
    function DropKoma(Kind: TKomaKind; ToX, ToY: Integer): Boolean;
    function GetTurn: TTurn;
    procedure ChangeTurn;
    function GetKomaName(Koma: TKoma): String;
    function CanNaru(Koma: TKoma; ToY: Integer; IsSente: Boolean): Boolean;
    function GetMochigoma(IsSente: Boolean): TMochigoma;
    function HasMochigoma(Kind: TKomaKind; IsSente: Boolean): Boolean;
    property Board: TBoard read FBoard;
    property Turn: TTurn read FTurn;
  end;

implementation

constructor TShogiGame.Create;
begin
  InitializeBoard;
  FTurn := ttSente;
  FillChar(FSenteMochigoma, SizeOf(FSenteMochigoma), 0);
  FillChar(FGoteMochigoma, SizeOf(FGoteMochigoma), 0);
end;

procedure TShogiGame.InitializeBoard;
var
  X, Y: Integer;
begin
  // 盤面を初期化（すべて空に）
  for Y := 1 to 9 do
    for X := 1 to 9 do
    begin
      FBoard[X, Y].Kind := kkNone;
      FBoard[X, Y].IsSente := False;
      FBoard[X, Y].IsNari := False;
    end;

  // 後手の駒を配置（上段）
  FBoard[1, 1].Kind := kkKyo;
  FBoard[1, 1].IsSente := False;
  FBoard[1, 1].IsNari := False;
  FBoard[2, 1].Kind := kkKei;
  FBoard[2, 1].IsSente := False;
  FBoard[2, 1].IsNari := False;
  FBoard[3, 1].Kind := kkGin;
  FBoard[3, 1].IsSente := False;
  FBoard[3, 1].IsNari := False;
  FBoard[4, 1].Kind := kkKin;
  FBoard[4, 1].IsSente := False;
  FBoard[4, 1].IsNari := False;
  FBoard[5, 1].Kind := kkGyoku;
  FBoard[5, 1].IsSente := False;
  FBoard[5, 1].IsNari := False;
  FBoard[6, 1].Kind := kkKin;
  FBoard[6, 1].IsSente := False;
  FBoard[6, 1].IsNari := False;
  FBoard[7, 1].Kind := kkGin;
  FBoard[7, 1].IsSente := False;
  FBoard[7, 1].IsNari := False;
  FBoard[8, 1].Kind := kkKei;
  FBoard[8, 1].IsSente := False;
  FBoard[8, 1].IsNari := False;
  FBoard[9, 1].Kind := kkKyo;
  FBoard[9, 1].IsSente := False;
  FBoard[9, 1].IsNari := False;

  FBoard[2, 2].Kind := kkKaku;
  FBoard[2, 2].IsSente := False;
  FBoard[2, 2].IsNari := False;
  FBoard[8, 2].Kind := kkHisha;
  FBoard[8, 2].IsSente := False;
  FBoard[8, 2].IsNari := False;

  for X := 1 to 9 do
  begin
    FBoard[X, 3].Kind := kkFu;
    FBoard[X, 3].IsSente := False;
    FBoard[X, 3].IsNari := False;
  end;

  // 先手の駒を配置（下段）
  for X := 1 to 9 do
  begin
    FBoard[X, 7].Kind := kkFu;
    FBoard[X, 7].IsSente := True;
    FBoard[X, 7].IsNari := False;
  end;

  FBoard[2, 8].Kind := kkHisha;
  FBoard[2, 8].IsSente := True;
  FBoard[2, 8].IsNari := False;
  FBoard[8, 8].Kind := kkKaku;
  FBoard[8, 8].IsSente := True;
  FBoard[8, 8].IsNari := False;

  FBoard[1, 9].Kind := kkKyo;
  FBoard[1, 9].IsSente := True;
  FBoard[1, 9].IsNari := False;
  FBoard[2, 9].Kind := kkKei;
  FBoard[2, 9].IsSente := True;
  FBoard[2, 9].IsNari := False;
  FBoard[3, 9].Kind := kkGin;
  FBoard[3, 9].IsSente := True;
  FBoard[3, 9].IsNari := False;
  FBoard[4, 9].Kind := kkKin;
  FBoard[4, 9].IsSente := True;
  FBoard[4, 9].IsNari := False;
  FBoard[5, 9].Kind := kkOu;
  FBoard[5, 9].IsSente := True;
  FBoard[5, 9].IsNari := False;
  FBoard[6, 9].Kind := kkKin;
  FBoard[6, 9].IsSente := True;
  FBoard[6, 9].IsNari := False;
  FBoard[7, 9].Kind := kkGin;
  FBoard[7, 9].IsSente := True;
  FBoard[7, 9].IsNari := False;
  FBoard[8, 9].Kind := kkKei;
  FBoard[8, 9].IsSente := True;
  FBoard[8, 9].IsNari := False;
  FBoard[9, 9].Kind := kkKyo;
  FBoard[9, 9].IsSente := True;
  FBoard[9, 9].IsNari := False;
end;

function TShogiGame.GetKoma(X, Y: Integer): TKoma;
begin
  if (X >= 1) and (X <= 9) and (Y >= 1) and (Y <= 9) then
    Result := FBoard[X, Y]
  else
  begin
    Result.Kind := kkNone;
    Result.IsSente := False;
    Result.IsNari := False;
  end;
end;

function TShogiGame.IsValidMove(FromX, FromY, ToX, ToY: Integer): Boolean;
var
  FromKoma, ToKoma: TKoma;
begin
  Result := False;
  
  // 範囲チェック
  if (FromX < 1) or (FromX > 9) or (FromY < 1) or (FromY > 9) then Exit;
  if (ToX < 1) or (ToX > 9) or (ToY < 1) or (ToY > 9) then Exit;
  
  FromKoma := FBoard[FromX, FromY];
  ToKoma := FBoard[ToX, ToY];
  
  // 駒がない
  if FromKoma.Kind = kkNone then Exit;
  
  // 同じ手番の駒を取ろうとしている
  if (ToKoma.Kind <> kkNone) and (FromKoma.IsSente = ToKoma.IsSente) then Exit;
  
  // 手番チェック（簡易版）
  if (FTurn = ttSente) and not FromKoma.IsSente then Exit;
  if (FTurn = ttGote) and FromKoma.IsSente then Exit;
  
  // 各駒の動きをチェック
  Result := CheckKomaMove(FromKoma, FromX, FromY, ToX, ToY);
end;

function TShogiGame.MoveKoma(FromX, FromY, ToX, ToY: Integer; DoNaru: Boolean = False): Boolean;
var
  ToKoma: TKoma;
  FromKoma: TKoma;
begin
  Result := False;
  
  if not IsValidMove(FromX, FromY, ToX, ToY) then Exit;
  
  FromKoma := FBoard[FromX, FromY];
  ToKoma := FBoard[ToX, ToY];
  
  // 取った駒を持ち駒に追加
  if ToKoma.Kind <> kkNone then
  begin
    // 成駒は元の駒に戻す
    if ToKoma.IsNari then
    begin
      case ToKoma.Kind of
        kkRyu: ToKoma.Kind := kkHisha;
        kkUma: ToKoma.Kind := kkKaku;
        kkNariGin: ToKoma.Kind := kkGin;
        kkNariKei: ToKoma.Kind := kkKei;
        kkNariKyo: ToKoma.Kind := kkKyo;
        kkTo: ToKoma.Kind := kkFu;
      end;
      ToKoma.IsNari := False;
    end;
    // 取った側（現在の手番）の持ち駒に追加（取られた駒の持ち主とは逆）
    ToKoma.IsSente := not ToKoma.IsSente;
    AddMochigoma(ToKoma);
  end;
  
  // 成り駒の処理
  if MustNaru(FromKoma, ToY) then
  begin
    // 強制成り
    NaruKoma(FromKoma);
  end
  else if DoNaru and CanNaru(FromKoma, ToY, FromKoma.IsSente) then
  begin
    // 選択成り
    NaruKoma(FromKoma);
  end;
  
  // 駒を移動
  FBoard[ToX, ToY] := FromKoma;
  FBoard[FromX, FromY].Kind := kkNone;
  FBoard[FromX, FromY].IsSente := False;
  FBoard[FromX, FromY].IsNari := False;
  
  Result := True;
end;

function TShogiGame.GetTurn: TTurn;
begin
  Result := FTurn;
end;

procedure TShogiGame.ChangeTurn;
begin
  if FTurn = ttSente then
    FTurn := ttGote
  else
    FTurn := ttSente;
end;

function TShogiGame.GetKomaName(Koma: TKoma): String;
begin
  if Koma.Kind = kkNone then
  begin
    Result := '';
    Exit;
  end;
  
  case Koma.Kind of
    kkOu: Result := '王';
    kkGyoku: Result := '玉';
    kkHisha: Result := '飛';
    kkKaku: Result := '角';
    kkKin: Result := '金';
    kkGin: Result := '銀';
    kkKei: Result := '桂';
    kkKyo: Result := '香';
    kkFu: Result := '歩';
    kkRyu: Result := '龍';
    kkUma: Result := '馬';
    kkNariGin: Result := '全';
    kkNariKei: Result := '圭';
    kkNariKyo: Result := '杏';
    kkTo: Result := 'と';
    else Result := '?';
  end;
  
  // 後手の場合は小文字で表示（実際のUIでは色などで区別）
  if not Koma.IsSente then
    Result := LowerCase(Result);
end;

function TShogiGame.CanNaru(Koma: TKoma; ToY: Integer; IsSente: Boolean): Boolean;
begin
  Result := False;
  
  // 既に成っている場合は成れない
  if Koma.IsNari then Exit;
  
  // 王、玉、金は成れない
  if (Koma.Kind = kkOu) or (Koma.Kind = kkGyoku) or (Koma.Kind = kkKin) then Exit;
  
  // 先手の場合、1-3段目で成れる、または移動先が1-3段目
  if IsSente then
    Result := (ToY <= 3)
  // 後手の場合、7-9段目で成れる、または移動先が7-9段目
  else
    Result := (ToY >= 7);
end;

// 進路に他の駒がないかチェック（飛車、角など）
function TShogiGame.CheckPath(FromX, FromY, ToX, ToY: Integer): Boolean;
var
  StepX, StepY: Integer;
  CurrentX, CurrentY: Integer;
begin
  Result := True;
  
  // 移動方向を計算
  if ToX > FromX then StepX := 1
  else if ToX < FromX then StepX := -1
  else StepX := 0;
  
  if ToY > FromY then StepY := 1
  else if ToY < FromY then StepY := -1
  else StepY := 0;
  
  // 進路上の各マスをチェック（目的地の1つ前まで）
  CurrentX := FromX + StepX;
  CurrentY := FromY + StepY;
  
  while (CurrentX <> ToX) or (CurrentY <> ToY) do
  begin
    // 範囲チェック
    if (CurrentX < 1) or (CurrentX > 9) or (CurrentY < 1) or (CurrentY > 9) then
    begin
      Result := False;
      Exit;
    end;
    
    // 駒がある場合は進路が塞がれている
    if FBoard[CurrentX, CurrentY].Kind <> kkNone then
    begin
      Result := False;
      Exit;
    end;
    
    CurrentX := CurrentX + StepX;
    CurrentY := CurrentY + StepY;
  end;
end;

// 各駒の動きをチェック
function TShogiGame.CheckKomaMove(Koma: TKoma; FromX, FromY, ToX, ToY: Integer): Boolean;
var
  DX, DY: Integer;
  IsSente: Boolean;
  ForwardY: Integer; // 前方向（先手は+1、後手は-1）
begin
  Result := False;
  DX := ToX - FromX;
  DY := ToY - FromY;
  IsSente := Koma.IsSente;
  
  // 先手は上方向（Yが減る）、後手は下方向（Yが増える）が前
  // Y=1が上段（後手側）、Y=9が下段（先手側）
  if IsSente then
    ForwardY := -1  // 先手は上方向（Y減少）が前進
  else
    ForwardY := 1;  // 後手は下方向（Y増加）が前進
  
  // 成駒の処理
  if Koma.IsNari then
  begin
    case Koma.Kind of
      kkRyu: // 龍（飛車の成り）
      begin
        // 縦横に何マスでも、または斜めに1マス
        if (DX = 0) or (DY = 0) then
          Result := CheckPath(FromX, FromY, ToX, ToY)
        else if (Abs(DX) = 1) and (Abs(DY) = 1) then
          Result := True
        else
          Result := False;
      end;
      kkUma: // 馬（角の成り）
      begin
        // 斜めに何マスでも、または縦横に1マス
        if (Abs(DX) = Abs(DY)) and (DX <> 0) then
          Result := CheckPath(FromX, FromY, ToX, ToY)
        else if ((Abs(DX) = 1) and (DY = 0)) or ((DX = 0) and (Abs(DY) = 1)) then
          Result := True
        else
          Result := False;
      end;
      kkNariGin, kkNariKei, kkNariKyo, kkTo: // 成銀、成桂、成香、と
      begin
        // 金と同じ動き
        if (Abs(DX) <= 1) and (Abs(DY) <= 1) and not ((DX = 0) and (DY = 0)) then
        begin
          // 後ろ斜めには進めない（後ろ = -ForwardY方向）
          Result := not ((Abs(DX) = 1) and (DY = -ForwardY));
        end;
      end;
      else
        Result := False;
    end;
    Exit;
  end;
  
  // 通常の駒の動き
  case Koma.Kind of
    kkFu: // 歩
    begin
      // 前に1マス
      Result := (DX = 0) and (DY = ForwardY);
    end;
    
    kkKyo: // 香
    begin
      // 前に何マスでも
      Result := (DX = 0) and (DY * ForwardY > 0) and CheckPath(FromX, FromY, ToX, ToY);
    end;
    
    kkKei: // 桂
    begin
      // 前に2マス、左右に1マス（桂馬跳び）
      // ForwardYが-1なら前は上方向、ForwardYが1なら前は下方向
      Result := (Abs(DX) = 1) and (DY = ForwardY * 2);
    end;
    
    kkGin: // 銀
    begin
      // 前、斜め前に1マス、後ろ斜めに1マス
      // 前 = ForwardY方向、後ろ = -ForwardY方向
      Result := ((DX = 0) and (DY = ForwardY)) or 
                ((Abs(DX) = 1) and (DY = ForwardY)) or 
                ((Abs(DX) = 1) and (DY = -ForwardY));
    end;
    
    kkKin: // 金
    begin
      // 前、横、斜め前に1マス、後ろに1マス
      if (Abs(DX) <= 1) and (Abs(DY) <= 1) and not ((DX = 0) and (DY = 0)) then
      begin
        // 後ろ斜めには進めない（後ろ = -ForwardY方向）
        Result := not ((Abs(DX) = 1) and (DY = -ForwardY));
      end;
    end;
    
    kkKaku: // 角
    begin
      // 斜めに何マスでも
      if (Abs(DX) = Abs(DY)) and (DX <> 0) then
        Result := CheckPath(FromX, FromY, ToX, ToY)
      else
        Result := False;
    end;
    
    kkHisha: // 飛車
    begin
      // 縦横に何マスでも
      if ((DX = 0) and (DY <> 0)) or ((DX <> 0) and (DY = 0)) then
        Result := CheckPath(FromX, FromY, ToX, ToY)
      else
        Result := False;
    end;
    
    kkOu, kkGyoku: // 王、玉
    begin
      // 全方向に1マス
      Result := (Abs(DX) <= 1) and (Abs(DY) <= 1) and not ((DX = 0) and (DY = 0));
    end;
    
    else
      Result := False;
  end;
end;

// 持ち駒に追加
procedure TShogiGame.AddMochigoma(Koma: TKoma);
begin
  if Koma.IsSente then
  begin
    case Koma.Kind of
      kkHisha: Inc(FSenteMochigoma.Hisha);
      kkKaku: Inc(FSenteMochigoma.Kaku);
      kkKin: Inc(FSenteMochigoma.Kin);
      kkGin: Inc(FSenteMochigoma.Gin);
      kkKei: Inc(FSenteMochigoma.Kei);
      kkKyo: Inc(FSenteMochigoma.Kyo);
      kkFu: Inc(FSenteMochigoma.Fu);
    end;
  end
  else
  begin
    case Koma.Kind of
      kkHisha: Inc(FGoteMochigoma.Hisha);
      kkKaku: Inc(FGoteMochigoma.Kaku);
      kkKin: Inc(FGoteMochigoma.Kin);
      kkGin: Inc(FGoteMochigoma.Gin);
      kkKei: Inc(FGoteMochigoma.Kei);
      kkKyo: Inc(FGoteMochigoma.Kyo);
      kkFu: Inc(FGoteMochigoma.Fu);
    end;
  end;
end;

// 二歩チェック
function TShogiGame.CheckNifu(X: Integer; IsSente: Boolean): Boolean;
var
  Y: Integer;
begin
  Result := False;
  // 同じ筋（X座標）に歩があるかチェック
  for Y := 1 to 9 do
  begin
    if (FBoard[X, Y].Kind = kkFu) and (FBoard[X, Y].IsSente = IsSente) then
    begin
      Result := True; // 二歩
      Exit;
    end;
  end;
end;

// 打つ手が有効かチェック
function TShogiGame.IsValidDrop(Kind: TKomaKind; ToX, ToY: Integer): Boolean;
var
  IsSente: Boolean;
  Mochigoma: TMochigoma;
begin
  Result := False;
  
  // 範囲チェック
  if (ToX < 1) or (ToX > 9) or (ToY < 1) or (ToY > 9) then Exit;
  
  // 手番チェック
  IsSente := (FTurn = ttSente);
  
  // 持ち駒があるかチェック
  if not HasMochigoma(Kind, IsSente) then Exit;
  
  // 打つ場所に駒があるかチェック
  if FBoard[ToX, ToY].Kind <> kkNone then Exit;
  
  // 歩の場合の特殊ルール
  if Kind = kkFu then
  begin
    // 二歩チェック
    if CheckNifu(ToX, IsSente) then Exit;
    
    // 打ち歩詰めチェック（簡易版 - 実装省略）
    // 行き場のない段に打てない
    if IsSente then
    begin
      if ToY = 1 then Exit; // 先手は1段目に打てない
    end
    else
    begin
      if ToY = 9 then Exit; // 後手は9段目に打てない
    end;
  end;
  
  // 香車、桂馬の場合の特殊ルール
  if (Kind = kkKyo) or (Kind = kkKei) then
  begin
    // 行き場のない段に打てない
    if IsSente then
    begin
      if ToY = 1 then Exit; // 先手は1段目に打てない
      if (Kind = kkKei) and (ToY = 2) then Exit; // 桂馬は2段目にも打てない
    end
    else
    begin
      if ToY = 9 then Exit; // 後手は9段目に打てない
      if (Kind = kkKei) and (ToY = 8) then Exit; // 桂馬は8段目にも打てない
    end;
  end;
  
  // 移動不可能な場所に打てない
  if not CanMoveFrom(Kind, IsSente, ToY) then Exit;
  
  // 香の場合
  if Kind = kkKyo then
  begin
    if IsSente then
    begin
      if ToY = 1 then Exit; // 先手は1段目に打てない
    end
    else
    begin
      if ToY = 9 then Exit; // 後手は9段目に打てない
    end;
  end;
  
  // 桂の場合
  if Kind = kkKei then
  begin
    if IsSente then
    begin
      if (ToY = 1) or (ToY = 2) then Exit; // 先手は1-2段目に打てない
    end
    else
    begin
      if (ToY = 8) or (ToY = 9) then Exit; // 後手は8-9段目に打てない
    end;
  end;
  
  Result := True;
end;

// 持ち駒を打つ
function TShogiGame.DropKoma(Kind: TKomaKind; ToX, ToY: Integer): Boolean;
var
  IsSente: Boolean;
  Mochigoma: ^TMochigoma;
begin
  Result := False;
  
  if not IsValidDrop(Kind, ToX, ToY) then Exit;
  
  IsSente := (FTurn = ttSente);
  
  // 持ち駒を減らす
  if IsSente then
    Mochigoma := @FSenteMochigoma
  else
    Mochigoma := @FGoteMochigoma;
  
  case Kind of
    kkHisha: Dec(Mochigoma^.Hisha);
    kkKaku: Dec(Mochigoma^.Kaku);
    kkKin: Dec(Mochigoma^.Kin);
    kkGin: Dec(Mochigoma^.Gin);
    kkKei: Dec(Mochigoma^.Kei);
    kkKyo: Dec(Mochigoma^.Kyo);
    kkFu: Dec(Mochigoma^.Fu);
  end;
  
  // 駒を配置
  FBoard[ToX, ToY].Kind := Kind;
  FBoard[ToX, ToY].IsSente := IsSente;
  FBoard[ToX, ToY].IsNari := False;
  
  Result := True;
end;

// 持ち駒を取得
function TShogiGame.GetMochigoma(IsSente: Boolean): TMochigoma;
begin
  if IsSente then
    Result := FSenteMochigoma
  else
    Result := FGoteMochigoma;
end;

// 持ち駒があるかチェック
function TShogiGame.HasMochigoma(Kind: TKomaKind; IsSente: Boolean): Boolean;
var
  Mochigoma: TMochigoma;
begin
  Mochigoma := GetMochigoma(IsSente);
  case Kind of
    kkHisha: Result := Mochigoma.Hisha > 0;
    kkKaku: Result := Mochigoma.Kaku > 0;
    kkKin: Result := Mochigoma.Kin > 0;
    kkGin: Result := Mochigoma.Gin > 0;
    kkKei: Result := Mochigoma.Kei > 0;
    kkKyo: Result := Mochigoma.Kyo > 0;
    kkFu: Result := Mochigoma.Fu > 0;
    else Result := False;
  end;
end;

// 強制成りかチェック
function TShogiGame.MustNaru(Koma: TKoma; ToY: Integer): Boolean;
var
  IsSente: Boolean;
begin
  Result := False;
  
  // 既に成っている場合は成れない
  if Koma.IsNari then Exit;
  
  // 王、玉、金は成れない
  if (Koma.Kind = kkOu) or (Koma.Kind = kkGyoku) or (Koma.Kind = kkKin) then Exit;
  
  IsSente := Koma.IsSente;
  
  // 歩、香車は最後の行に移動したら強制成り
  if (Koma.Kind = kkFu) or (Koma.Kind = kkKyo) then
  begin
    if IsSente then
      Result := (ToY = 1)  // 先手は1段目
    else
      Result := (ToY = 9); // 後手は9段目
  end
  // 桂馬は2段目・最後の行に移動したら強制成り
  else if Koma.Kind = kkKei then
  begin
    if IsSente then
      Result := (ToY <= 2)  // 先手は1-2段目
    else
      Result := (ToY >= 8); // 後手は8-9段目
  end;
end;

// 駒を成る
procedure TShogiGame.NaruKoma(var Koma: TKoma);
begin
  if Koma.IsNari then Exit;
  
  case Koma.Kind of
    kkHisha: Koma.Kind := kkRyu;
    kkKaku: Koma.Kind := kkUma;
    kkGin: Koma.Kind := kkNariGin;
    kkKei: Koma.Kind := kkNariKei;
    kkKyo: Koma.Kind := kkNariKyo;
    kkFu: Koma.Kind := kkTo;
  end;
  Koma.IsNari := True;
end;

// その位置から移動可能かチェック（打ち駒用）
function TShogiGame.CanMoveFrom(Kind: TKomaKind; IsSente: Boolean; FromY: Integer): Boolean;
var
  ForwardY: Integer;
  TestKoma: TKoma;
  X, Y: Integer;
begin
  Result := False;
  
  // 王、玉、金はどこからでも移動可能
  if (Kind = kkOu) or (Kind = kkGyoku) or (Kind = kkKin) then
  begin
    Result := True;
    Exit;
  end;
  
  if IsSente then
    ForwardY := -1  // 先手は上方向が前
  else
    ForwardY := 1;  // 後手は下方向が前
  
  TestKoma.Kind := Kind;
  TestKoma.IsSente := IsSente;
  TestKoma.IsNari := False;
  
  // その位置から1マスでも移動できるかチェック
  for X := 1 to 9 do
  begin
    for Y := 1 to 9 do
    begin
      if (X = 5) and (Y = FromY) then Continue; // 現在位置はスキップ
      
      if CheckKomaMove(TestKoma, 5, FromY, X, Y) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

end.

