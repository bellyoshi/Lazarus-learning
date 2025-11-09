unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Unit1;

type
  TPieceSelection = (psQueen, psRook, psBishop, psKnight);

type
  TForm2 = class(TForm)
    Label1: TLabel;
    ButtonQueen: TButton;
    ButtonRook: TButton;
    ButtonBishop: TButton;
    ButtonKnight: TButton;
    procedure ButtonQueenClick(Sender: TObject);
    procedure ButtonRookClick(Sender: TObject);
    procedure ButtonBishopClick(Sender: TObject);
    procedure ButtonKnightClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FSelectedPiece: TPieceSelection;
    FPawnColor: TPieceColor;
  public
    function SelectPromotion(AColor: TPieceColor): TPieceType;
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

procedure TForm2.FormShow(Sender: TObject);
var
  QueenSymbol, RookSymbol, BishopSymbol, KnightSymbol: String;
begin
  FSelectedPiece := psQueen;  // デフォルトはクイーン
  
  // 色に応じたUnicode文字を設定
  if FPawnColor = pcWhite then
  begin
    QueenSymbol := '♕';   // 白のクイーン
    RookSymbol := '♖';    // 白のルーク
    BishopSymbol := '♗';  // 白のビショップ
    KnightSymbol := '♘';  // 白のナイト
  end
  else
  begin
    QueenSymbol := '♛';   // 黒のクイーン
    RookSymbol := '♜';    // 黒のルーク
    BishopSymbol := '♝';  // 黒のビショップ
    KnightSymbol := '♞';  // 黒のナイト
  end;
  
  // ボタンのキャプションに絵文字を設定
  ButtonQueen.Caption := QueenSymbol + ' クイーン';
  ButtonRook.Caption := RookSymbol + ' ルーク';
  ButtonBishop.Caption := BishopSymbol + ' ビショップ';
  ButtonKnight.Caption := KnightSymbol + ' ナイト';
  
  // フォントをUnicode対応に設定
  ButtonQueen.Font.Name := 'Segoe UI Symbol';
  ButtonQueen.Font.Size := 16;
  ButtonRook.Font.Name := 'Segoe UI Symbol';
  ButtonRook.Font.Size := 16;
  ButtonBishop.Font.Name := 'Segoe UI Symbol';
  ButtonBishop.Font.Size := 16;
  ButtonKnight.Font.Name := 'Segoe UI Symbol';
  ButtonKnight.Font.Size := 16;
end;

function TForm2.SelectPromotion(AColor: TPieceColor): TPieceType;
begin
  FPawnColor := AColor;
  FSelectedPiece := psQueen;  // デフォルト
  
  // モーダルダイアログとして表示
  if ShowModal = mrOK then
  begin
    case FSelectedPiece of
      psQueen: Result := ptQueen;
      psRook: Result := ptRook;
      psBishop: Result := ptBishop;
      psKnight: Result := ptKnight;
    end;
  end
  else
    Result := ptQueen;  // キャンセル時はデフォルトでクイーン
end;

procedure TForm2.ButtonQueenClick(Sender: TObject);
begin
  FSelectedPiece := psQueen;
  ModalResult := mrOK;
end;

procedure TForm2.ButtonRookClick(Sender: TObject);
begin
  FSelectedPiece := psRook;
  ModalResult := mrOK;
end;

procedure TForm2.ButtonBishopClick(Sender: TObject);
begin
  FSelectedPiece := psBishop;
  ModalResult := mrOK;
end;

procedure TForm2.ButtonKnightClick(Sender: TObject);
begin
  FSelectedPiece := psKnight;
  ModalResult := mrOK;
end;

end.

