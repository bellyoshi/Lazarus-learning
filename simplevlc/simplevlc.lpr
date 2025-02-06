program simplevlc;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, vlc;

var
  VLCPlayer: TVLCMediaPlayer;
begin
  VLCPlayer := TVLCMediaPlayer.Create(nil);
  try
    VLCPlayer.PlayFile('C:\Users\catik\Videos\JWLibrary\1112024026_J_cnt_1_r360P.mp4');  // 再生する動画のパスに変更
    WriteLn('Playing video. Press Enter to stop.');
    ReadLn;
    VLCPlayer.Stop;
  finally
    VLCPlayer.Free;
  end;
end.

