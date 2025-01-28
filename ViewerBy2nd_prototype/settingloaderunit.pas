unit SettingLoaderUnit;


{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, FormSizeCustomizerUnit,IniFiles;

{
//Load
設定ファイルを読み込む

設定ファイルの内容
・スクリーン番号
・フルスクリーンかウインドウモードか
・ウインドウモードの場合ウインドウサイズと位置

設定ファイルが有効であるかを確認する。
・スクリーン番号はScreen.MonitorCountに入っているか
→入っていなければセカンドモニターを探す。
→セカンドモニターがなければフルスクリーンではない。スクリーン番号は0、ウインドウモードにする。
・ウインドウサイズがモニターサイズ以内か。

設定を反映する。
FormSizeCustomizer.ScreenIndex = スクリーン番号
FormSizeCustomizer.IsFullScreen = フルスクリーンならTrue
FormSizeCustomizer.SetOriginalSize(Top, Left , Width, Height: Integer);

//Save
設定ファイルをほぞんする。
}


type
  TSettingLoader = class
  private
    FScreenIndex: Integer;
    FIsFullScreen: Boolean;
    FWindowTop: Integer;
    FWindowLeft: Integer;
    FWindowWidth: Integer;
    FWindowHeight: Integer;
    const
      SETTINGS_FILE = 'settings.ini';
    procedure LoadDefaults;
    procedure ValidateSettings;
    procedure CollectSettings;
  public
    constructor Create;
    procedure Load;
    procedure Save;
    procedure ApplySettings;
  end;

var
  SettingLoader: TSettingLoader;

implementation

constructor TSettingLoader.Create;
begin
  //no op
end;

procedure TSettingLoader.LoadDefaults;
begin
  // セカンドモニターの確認
  if Screen.MonitorCount > 1 then
  begin
    FIsFullScreen := True;
    FScreenIndex := 1  // セカンドモニターをデフォルトに設定
  end
  else
  begin
    FIsFullScreen := False; // デフォルトはウィンドウモード
    FScreenIndex := 0; // プライマリモニターをデフォルトに設定
  end;


  FWindowTop := 100;
  FWindowLeft := 100;
  FWindowWidth := 800;
  FWindowHeight := 600;
end;

procedure TSettingLoader.ApplySettings;
begin
  FormSizeCustomizer.ScreenIndex := FScreenIndex;
  FormSizeCustomizer.IsFullScreen := FIsFullScreen;
  FormSizeCustomizer.SetOriginalSize(FWindowTop, FWindowLeft, FWindowWidth, FWindowHeight);
end;

procedure TSettingLoader.Load;
var
  SettingsFile: TIniFile;
begin
  if not FileExists(SETTINGS_FILE) then
  begin
    LoadDefaults;  // デフォルト値をロード
    Exit;
  end;

  SettingsFile := TIniFile.Create(SETTINGS_FILE);
  try
    FScreenIndex := SettingsFile.ReadInteger('Display', 'ScreenIndex', 0);
    FIsFullScreen := SettingsFile.ReadBool('Display', 'IsFullScreen', False);
    FWindowTop := SettingsFile.ReadInteger('Window', 'Top', 100);
    FWindowLeft := SettingsFile.ReadInteger('Window', 'Left', 100);
    FWindowWidth := SettingsFile.ReadInteger('Window', 'Width', 800);
    FWindowHeight := SettingsFile.ReadInteger('Window', 'Height', 600);

    ValidateSettings; // 設定の妥当性を確認
  finally
    SettingsFile.Free;
  end;
end;

procedure TSettingLoader.ValidateSettings;
var
  MonitorCount: Integer;
begin
  MonitorCount := Screen.MonitorCount;
  if (FScreenIndex < 0) or (FScreenIndex >= MonitorCount) then
  begin
    LoadDefaults;
  end;

end;

procedure TSettingLoader.CollectSettings;
begin
  // FormSizeCustomizer から現在の設定を取得
  FScreenIndex := FormSizeCustomizer.ScreenIndex;
  FIsFullScreen := FormSizeCustomizer.IsFullScreen;
  FormSizeCustomizer.BackupOriginal();
  FWindowTop := FormSizeCustomizer.WindowTop;
  FWindowLeft := FormSizeCustomizer.WindowLeft;
  FWindowWidth := FormSizeCustomizer.WindowWidth;
  FWindowHeight := FormSizeCustomizer.WindowHeight;

end;

procedure TSettingLoader.Save;
var
  SettingsFile: TIniFile;
begin
  // 最新の設定を回収
  CollectSettings;

  // 設定を保存
  SettingsFile := TIniFile.Create(SETTINGS_FILE);
  try
    SettingsFile.WriteInteger('Display', 'ScreenIndex', FScreenIndex);
    SettingsFile.WriteBool('Display', 'IsFullScreen', FIsFullScreen);
    SettingsFile.WriteInteger('Window', 'Top', FWindowTop);
    SettingsFile.WriteInteger('Window', 'Left', FWindowLeft);
    SettingsFile.WriteInteger('Window', 'Width', FWindowWidth);
    SettingsFile.WriteInteger('Window', 'Height', FWindowHeight);

  finally
    SettingsFile.Free;
  end;
end;


end.


