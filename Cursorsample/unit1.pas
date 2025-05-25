unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls, StrUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
  private
    FCurrentPlayer: Char;
    FGameBoard: array[1..9] of Char;
    function CheckWinner: Boolean;
    function IsBoardFull: Boolean;
    procedure ResetGame;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ResetGame;
end;

procedure TForm1.ResetGame;
var
  i: Integer;
begin
  FCurrentPlayer := 'O';
  for i := 1 to 9 do
    FGameBoard[i] := ' ';
  Button1.Caption := '';
  Button2.Caption := '';
  Button3.Caption := '';
  Button4.Caption := '';
  Button5.Caption := '';
  Button6.Caption := '';
  Button7.Caption := '';
  Button8.Caption := '';
  Button9.Caption := '';
end;

function TForm1.CheckWinner: Boolean;
const
  WinningCombinations: array[1..8, 1..3] of Integer = (
    (1, 2, 3), (4, 5, 6), (7, 8, 9), // 横
    (1, 4, 7), (2, 5, 8), (3, 6, 9), // 縦
    (1, 5, 9), (3, 5, 7)             // 斜め
  );
var
  i: Integer;
begin
  Result := False;
  for i := 1 to 8 do
  begin
    if (FGameBoard[WinningCombinations[i, 1]] <> ' ') and
       (FGameBoard[WinningCombinations[i, 1]] = FGameBoard[WinningCombinations[i, 2]]) and
       (FGameBoard[WinningCombinations[i, 2]] = FGameBoard[WinningCombinations[i, 3]]) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TForm1.IsBoardFull: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to 9 do
    if FGameBoard[i] = ' ' then
    begin
      Result := False;
      Exit;
    end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if (Button1.Caption = '') and (FGameBoard[1] = ' ') then
  begin
    Button1.Caption := FCurrentPlayer;
    FGameBoard[1] := FCurrentPlayer;
    if CheckWinner then
    begin
      ShowMessage(FCurrentPlayer + 'の勝ちです！');
      ResetGame;
    end
    else if IsBoardFull then
    begin
      ShowMessage('引き分けです！');
      ResetGame;
    end
    else
      if FCurrentPlayer = 'O' then
        FCurrentPlayer := 'X'
      else
        FCurrentPlayer := 'O';
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if (Button2.Caption = '') and (FGameBoard[2] = ' ') then
  begin
    Button2.Caption := FCurrentPlayer;
    FGameBoard[2] := FCurrentPlayer;
    if CheckWinner then
    begin
      ShowMessage(FCurrentPlayer + 'の勝ちです！');
      ResetGame;
    end
    else if IsBoardFull then
    begin
      ShowMessage('引き分けです！');
      ResetGame;
    end
    else
      if FCurrentPlayer = 'O' then
        FCurrentPlayer := 'X'
      else
        FCurrentPlayer := 'O';
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if (Button3.Caption = '') and (FGameBoard[3] = ' ') then
  begin
    Button3.Caption := FCurrentPlayer;
    FGameBoard[3] := FCurrentPlayer;
    if CheckWinner then
    begin
      ShowMessage(FCurrentPlayer + 'の勝ちです！');
      ResetGame;
    end
    else if IsBoardFull then
    begin
      ShowMessage('引き分けです！');
      ResetGame;
    end
    else
      if FCurrentPlayer = 'O' then
        FCurrentPlayer := 'X'
      else
        FCurrentPlayer := 'O';
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if (Button4.Caption = '') and (FGameBoard[4] = ' ') then
  begin
    Button4.Caption := FCurrentPlayer;
    FGameBoard[4] := FCurrentPlayer;
    if CheckWinner then
    begin
      ShowMessage(FCurrentPlayer + 'の勝ちです！');
      ResetGame;
    end
    else if IsBoardFull then
    begin
      ShowMessage('引き分けです！');
      ResetGame;
    end
    else
      if FCurrentPlayer = 'O' then
        FCurrentPlayer := 'X'
      else
        FCurrentPlayer := 'O';
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  if (Button5.Caption = '') and (FGameBoard[5] = ' ') then
  begin
    Button5.Caption := FCurrentPlayer;
    FGameBoard[5] := FCurrentPlayer;
    if CheckWinner then
    begin
      ShowMessage(FCurrentPlayer + 'の勝ちです！');
      ResetGame;
    end
    else if IsBoardFull then
    begin
      ShowMessage('引き分けです！');
      ResetGame;
    end
    else
      if FCurrentPlayer = 'O' then
        FCurrentPlayer := 'X'
      else
        FCurrentPlayer := 'O';
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  if (Button6.Caption = '') and (FGameBoard[6] = ' ') then
  begin
    Button6.Caption := FCurrentPlayer;
    FGameBoard[6] := FCurrentPlayer;
    if CheckWinner then
    begin
      ShowMessage(FCurrentPlayer + 'の勝ちです！');
      ResetGame;
    end
    else if IsBoardFull then
    begin
      ShowMessage('引き分けです！');
      ResetGame;
    end
    else
      if FCurrentPlayer = 'O' then
        FCurrentPlayer := 'X'
      else
        FCurrentPlayer := 'O';
  end;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  if (Button7.Caption = '') and (FGameBoard[7] = ' ') then
  begin
    Button7.Caption := FCurrentPlayer;
    FGameBoard[7] := FCurrentPlayer;
    if CheckWinner then
    begin
      ShowMessage(FCurrentPlayer + 'の勝ちです！');
      ResetGame;
    end
    else if IsBoardFull then
    begin
      ShowMessage('引き分けです！');
      ResetGame;
    end
    else
      if FCurrentPlayer = 'O' then
        FCurrentPlayer := 'X'
      else
        FCurrentPlayer := 'O';
  end;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  if (Button8.Caption = '') and (FGameBoard[8] = ' ') then
  begin
    Button8.Caption := FCurrentPlayer;
    FGameBoard[8] := FCurrentPlayer;
    if CheckWinner then
    begin
      ShowMessage(FCurrentPlayer + 'の勝ちです！');
      ResetGame;
    end
    else if IsBoardFull then
    begin
      ShowMessage('引き分けです！');
      ResetGame;
    end
    else
      if FCurrentPlayer = 'O' then
        FCurrentPlayer := 'X'
      else
        FCurrentPlayer := 'O';
  end;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  if (Button9.Caption = '') and (FGameBoard[9] = ' ') then
  begin
    Button9.Caption := FCurrentPlayer;
    FGameBoard[9] := FCurrentPlayer;
    if CheckWinner then
    begin
      ShowMessage(FCurrentPlayer + 'の勝ちです！');
      ResetGame;
    end
    else if IsBoardFull then
    begin
      ShowMessage('引き分けです！');
      ResetGame;
    end
    else
      if FCurrentPlayer = 'O' then
        FCurrentPlayer := 'X'
      else
        FCurrentPlayer := 'O';
  end;
end;

end.

