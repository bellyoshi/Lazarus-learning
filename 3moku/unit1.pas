unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Unit2;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    LabelStatus: TLabel;
    LabelPlayer: TLabel;
    ButtonReset: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
  private
    FGame: TThreeMoku;
    FButtons: array[0..2, 0..2] of TButton;
    
    procedure CreateGameBoard;
    procedure UpdateDisplay;
    procedure ButtonClick(Sender: TObject);
    procedure EnableButtons(Enable: Boolean);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FGame := TThreeMoku.Create;
  CreateGameBoard;
  UpdateDisplay;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FGame.Free;
end;

procedure TForm1.CreateGameBoard;
var
  i, j: Integer;
  btn: TButton;
  btnSize: Integer;
  spacing: Integer;
  startX, startY: Integer;
begin
  btnSize := 60;
  spacing := 5;
  startX := 10;
  startY := 10;
  
  for i := 0 to 2 do
  begin
    for j := 0 to 2 do
    begin
      btn := TButton.Create(Self);
      btn.Parent := Panel1;
      btn.Left := startX + j * (btnSize + spacing);
      btn.Top := startY + i * (btnSize + spacing);
      btn.Width := btnSize;
      btn.Height := btnSize;
      btn.Font.Size := 24;
      btn.Font.Style := [fsBold];
      btn.Caption := '';
      btn.Tag := i * 3 + j;
      btn.OnClick := @ButtonClick;
      FButtons[i, j] := btn;
    end;
  end;
end;

procedure TForm1.ButtonClick(Sender: TObject);
var
  Row, Col: Integer;
  ButtonTag: Integer;
begin
  if not (Sender is TButton) then
    Exit;
    
  ButtonTag := (Sender as TButton).Tag;
  Row := ButtonTag div 3;
  Col := ButtonTag mod 3;
  
  if FGame.MakeMove(Row, Col) then
  begin
    UpdateDisplay;
  end;
end;

procedure TForm1.UpdateDisplay;
var
  i, j: Integer;
  btn: TButton;
  cellState: TCellState;
begin
  // ボードの状態を更新
  for i := 0 to 2 do
  begin
    for j := 0 to 2 do
    begin
      btn := FButtons[i, j];
      cellState := FGame.Board[i, j];
      
      case cellState of
        csEmpty: btn.Caption := '';
        csX: btn.Caption := 'X';
        csO: btn.Caption := 'O';
      end;
    end;
  end;
  
  // ゲーム状態に応じて表示を更新
  case FGame.GameState of
    gsPlaying:
    begin
      LabelStatus.Caption := 'ゲーム中';
      LabelPlayer.Caption := '現在のプレイヤー: ' + FGame.GetCurrentPlayerSymbol;
      EnableButtons(True);
    end;
    gsXWon:
    begin
      LabelStatus.Caption := 'Xの勝利！';
      LabelPlayer.Caption := '';
      EnableButtons(False);
    end;
    gsOWon:
    begin
      LabelStatus.Caption := 'Oの勝利！';
      LabelPlayer.Caption := '';
      EnableButtons(False);
    end;
    gsDraw:
    begin
      LabelStatus.Caption := '引き分け';
      LabelPlayer.Caption := '';
      EnableButtons(False);
    end;
  end;
end;

procedure TForm1.EnableButtons(Enable: Boolean);
var
  i, j: Integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      FButtons[i, j].Enabled := Enable and (FGame.Board[i, j] = csEmpty);
end;

procedure TForm1.ButtonResetClick(Sender: TObject);
begin
  FGame.Reset;
  UpdateDisplay;
end;

end.

