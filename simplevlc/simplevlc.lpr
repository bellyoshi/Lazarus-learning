program simplevlc;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, vlc;

var
  VLCPlayer: TVLCMediaPlayer;
  FileName: string;

function IsValidMP4(const AFileName: string): Boolean;
begin
  Result := FileExists(AFileName) and (LowerCase(ExtractFileExt(AFileName)) = '.mp4');
end;

begin
  repeat
    Write('再生するMP4ファイルのパスを入力してください: ');
    ReadLn(FileName);
    if not FileExists(FileName) then
      WriteLn('ファイルが存在しません。もう一度入力してください。')
    else if LowerCase(ExtractFileExt(FileName)) <> '.mp4' then
      WriteLn('MP4ファイルではありません。もう一度入力してください。');
  until IsValidMP4(FileName);

  VLCPlayer := TVLCMediaPlayer.Create(nil);
  try
    VLCPlayer.PlayFile(FileName);
    WriteLn('Playing video. Press Enter to stop.');
    ReadLn;
    VLCPlayer.Stop;
  finally
    VLCPlayer.Free;
  end;
end.

