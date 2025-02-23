unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef unix}cthreads,{$endif}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  vlc;

type
  { TMainForm }
  TMainForm = class(TForm)
    BtnPlay: TButton;
    BtnPause: TButton;
    BtnStop: TButton;
    BtnOpen: TButton;
    TBVolume: TTrackBar;
    TBVideo: TTrackBar;
    OpenDialog: TOpenDialog;
    PVideo: TPanel;
    LTime: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BtnOpenClick(Sender: TObject);
    procedure BtnPlayClick(Sender: TObject);
    procedure BtnPauseClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure TBVolumeChange(Sender: TObject);
    procedure TBVideoChange(Sender: TObject);
  private
    FPlayer: TVLCPlayer;
    procedure DoTimeChanged(Sender: TObject; const time: TDateTime);
    procedure DoPositionChanged(Sender: TObject; const APos: Double);
    procedure SetNewPosition;
    procedure DisplayTime;
  end;

var
  MainForm: TMainForm;
  FCurrentTime, FNewLength: TDateTime;
  FNewPosition: Integer;
  FShowing: Boolean;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FPlayer := TLCLVLCPlayer.Create(Self);
  FPlayer.ParentWindow := PVideo;
  FPlayer.OnTimeChanged := @DoTimeChanged;
  FPlayer.OnPositionChanged := @DoPositionChanged;
  FPlayer.UseEvents := True;
end;

procedure TMainForm.BtnOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    FPlayer.PlayFile(OpenDialog.FileName);
    Caption := 'Video Player - ' + ExtractFileName(OpenDialog.FileName);
  end;
end;

procedure TMainForm.BtnPlayClick(Sender: TObject);
begin
  FPlayer.Play;
end;

procedure TMainForm.BtnPauseClick(Sender: TObject);
begin
  FPlayer.Pause;
end;

procedure TMainForm.BtnStopClick(Sender: TObject);
begin
  FPlayer.Stop;
end;

procedure TMainForm.TBVolumeChange(Sender: TObject);
begin
  FPlayer.AudioVolume := TBVolume.Position;
end;

procedure TMainForm.TBVideoChange(Sender: TObject);
begin
  if not FShowing then
    FPlayer.VideoFractionalPosition := TBVideo.Position / 100;
end;

procedure TMainForm.DoTimeChanged(Sender: TObject; const time: TDateTime);
begin
  FCurrentTime := time;
  TThread.Synchronize(nil, @DisplayTime);
end;

procedure TMainForm.DoPositionChanged(Sender: TObject; const APos: Double);
begin
  FNewPosition := Round(APos * 100);
  TThread.Synchronize(nil, @SetNewPosition);
end;

procedure TMainForm.SetNewPosition;
begin
  FShowing := True;
  try
    TBVideo.Position := FNewPosition;
  finally
    FShowing := False;
  end;
end;

procedure TMainForm.DisplayTime;
var
  s: string;
  function TtoS(T: TDateTime): string;
  var
    h, m, s, ms: Word;
  begin
    DecodeTime(T, h, m, s, ms);
    if h > 0 then
      Result := FormatDateTime('hh:nn:ss', T)
    else
      Result := FormatDateTime('nn:ss', T);
  end;
begin
  s := '/';
  if FNewLength > 0 then
    s := s + TtoS(FNewLength)
  else
    s := s + '?';
  if (FCurrentTime > 0) then
    s := TtoS(FCurrentTime) + s
  else
    s := '0:0' + s;
  LTime.Caption := s;
end;

end.

