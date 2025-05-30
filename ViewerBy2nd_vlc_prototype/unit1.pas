unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, lclvlc, vlc, libvlc;

const
  TRACKBAR_MAX = 10000;
  TRACKBAR_MARGIN = 10;
  VIDEO_FILTER = 'Video files|*.mp4;*.avi;*.mkv;*.wmv|All files|*.*';

type

  { TForm1 }

  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    PlayButton: TButton;
    Panel1: TPanel;
    OpenButton: TButton;
    StopButton: TButton;
    TrackBar1: TTrackBar;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure PlayButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);

  private
    IsProgrammaticUpdate: Boolean;
    Player: TLCLVlcPlayer;
    procedure UpdateTrackBar;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Player := TLCLVlcPlayer.Create(Self);
  Player.ParentWindow := Panel1;
  TrackBar1.Enabled := False;
  PlayButton.Enabled := False;
  StopButton.Enabled := False;
  IsProgrammaticUpdate := False;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Player);
end;

procedure TForm1.OpenButtonClick(Sender: TObject);
begin
  OpenDialog1.Filter := VIDEO_FILTER;
  if OpenDialog1.Execute then
  begin
    Player.PlayFile(OpenDialog1.FileName);
    TrackBar1.Enabled := True;
    PlayButton.Enabled := True;
    StopButton.Enabled := True;
    TrackBar1.Max := TRACKBAR_MAX;

    Timer1.Enabled := True;
  end;
end;

procedure TForm1.PlayButtonClick(Sender: TObject);
begin
  Player.Play;
end;

procedure TForm1.StopButtonClick(Sender: TObject);
begin
  Player.Stop;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  If IsProgrammaticUpdate Then Exit;
  if not Assigned(Player) then Exit;
  if Player.VideoLength = 0 then Exit;

  Player.VideoPosition := TrackBar1.Position * Player.VideoLength div TrackBar1.Max;
end;

procedure TForm1.TrackBar1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  NewPosition: Integer;
  SliderWidth: Integer;
begin
  SliderWidth := TrackBar1.Width - (TRACKBAR_MARGIN * 2);
  
  // マウス位置をスライダー領域内に調整
  X := X - TRACKBAR_MARGIN;
  if X < 0 then X := 0;
  if X > SliderWidth then X := SliderWidth;
  
  // 新しい位置を計算
  NewPosition := Round(X / SliderWidth * TrackBar1.Max);
  
  // 範囲チェック
  if NewPosition < 0 then NewPosition := 0;
  if NewPosition > TrackBar1.Max then NewPosition := TrackBar1.Max;
  
  // 位置を更新
  TrackBar1.Position := NewPosition;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  UpdateTrackBar;
end;

procedure TForm1.UpdateTrackBar;
var
  pos: Integer;
begin

  if Not Assigned(Player) then Exit;

  if Player.VideoLength = 0 then Exit;

  pos :=  Round(Player.VideoPosition / Player.VideoLength * TrackBar1.Max);
  if pos >= 0 then
    IsProgrammaticUpdate := True;
    TrackBar1.Position := pos;
    IsProgrammaticUpdate := False;


end;





end.

