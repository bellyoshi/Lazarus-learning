unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Windows;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    hClient: THandle;
    procedure UpdateWiFiStrength;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  wlanapi;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  dwVersion: DWORD;
  dwResult: DWORD;
begin
  dwVersion := 2;
  dwResult := WlanOpenHandle(dwVersion, nil, @dwVersion, @hClient);
  
  if dwResult <> ERROR_SUCCESS then
  begin
    ShowMessage('WiFi APIの初期化に失敗しました。エラーコード: ' + IntToStr(dwResult));
    hClient := 0;
    Timer1.Enabled := False;
  end
  else
  begin
    Timer1.Interval := 1000; // 1秒
    Timer1.Enabled := True;
    UpdateWiFiStrength; // 初回更新
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if hClient <> 0 then
    WlanCloseHandle(hClient, nil);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  UpdateWiFiStrength;
end;

procedure TForm1.UpdateWiFiStrength;
var
  pInterfaceList: PWLAN_INTERFACE_INFO_LIST;
  dwResult: DWORD;
  i: Integer;
  pInterface: PWLAN_INTERFACE_INFO;
  pBssList: PWLAN_BSS_LIST;
  j: Integer;
  pBssEntry: PWLAN_BSS_ENTRY;
  SignalQuality: Integer;
  SSID: String;
  MaxSignal: Integer;
  BestSSID: String;
  pConnectionAttributes: PWLAN_CONNECTION_ATTRIBUTES;
  dwDataSize: DWORD;
  ConnectedSSID: String;
  FoundConnected: Boolean;
  ppData: PPVOID;
begin
  if hClient = 0 then
    Exit;

  // インターフェースリストを取得
  dwResult := WlanEnumInterfaces(hClient, nil, @pInterfaceList);
  
  if dwResult <> ERROR_SUCCESS then
  begin
    Label1.Caption := 'エラー: WiFiインターフェースを取得できません';
    Label2.Caption := '';
    Label3.Caption := '';
    Label4.Caption := '';
    Exit;
  end;

  try
    if (pInterfaceList = nil) or (pInterfaceList^.dwNumberOfItems = 0) then
    begin
      Label1.Caption := 'WiFiインターフェースが見つかりません';
      Label2.Caption := '';
      Label3.Caption := '';
      Label4.Caption := '';
      Exit;
    end;

    // 最初のアクティブなインターフェースを使用
    pInterface := nil;
    for i := 0 to Integer(pInterfaceList^.dwNumberOfItems) - 1 do
    begin
      if pInterfaceList^.InterfaceInfo[i].isState = wlan_interface_state_connected then
      begin
        pInterface := @pInterfaceList^.InterfaceInfo[i];
        Break;
      end;
    end;

    if pInterface = nil then
    begin
      Label1.Caption := '接続されているWiFiが見つかりません';
      Label2.Caption := '';
      Label3.Caption := '';
      Label4.Caption := '';
      Exit;
    end;

    // 接続情報を取得
    dwDataSize := 0;
    pConnectionAttributes := nil;
    ppData := @pConnectionAttributes;
    dwResult := WlanQueryInterface(hClient, @pInterface^.InterfaceGuid,
      wlan_intf_opcode_current_connection, nil, @dwDataSize,
      ppData, nil);

    // 接続されているSSIDを取得
    ConnectedSSID := '';
    try
      if (dwResult = ERROR_SUCCESS) and (pConnectionAttributes <> nil) and
         (dwDataSize > 0) then
      begin
        if (pConnectionAttributes^.wlanAssociationAttributes.dot11Ssid.uSSIDLength > 0) and
           (pConnectionAttributes^.wlanAssociationAttributes.dot11Ssid.uSSIDLength <= 32) then
        begin
          SetLength(ConnectedSSID, pConnectionAttributes^.wlanAssociationAttributes.dot11Ssid.uSSIDLength);
          Move(pConnectionAttributes^.wlanAssociationAttributes.dot11Ssid.ucSSID[0],
               ConnectedSSID[1], pConnectionAttributes^.wlanAssociationAttributes.dot11Ssid.uSSIDLength);
        end;
      end;
    except
      // エラーが発生した場合は空文字列のまま
      ConnectedSSID := '';
    end;

    // BSSリストを取得
    pBssList := nil;
    dwResult := WlanGetNetworkBssList(hClient, @pInterface^.InterfaceGuid,
      nil, dot11_BSS_type_any, False, nil, @pBssList);

    if dwResult <> ERROR_SUCCESS then
    begin
      Label1.Caption := 'エラー: ネットワーク情報を取得できません';
      Label2.Caption := '';
      Label3.Caption := '';
      Label4.Caption := '';
      if pConnectionAttributes <> nil then
      begin
        try
          WlanFreeMemory(pConnectionAttributes);
        except
          // メモリ解放エラーは無視
        end;
      end;
      Exit;
    end;

    try
      if (pBssList = nil) or (pBssList^.dwNumberOfItems = 0) then
      begin
        Label1.Caption := '利用可能なネットワークが見つかりません';
        Label2.Caption := '';
        Label3.Caption := '';
        Label4.Caption := '';
        Exit;
      end;

      // 最も強い信号を探す
      MaxSignal := -200;
      BestSSID := '';
      FoundConnected := False;
      
      for j := 0 to Integer(pBssList^.dwNumberOfItems) - 1 do
      begin
        pBssEntry := @pBssList^.wlanBssEntries[j];
        
        // SSIDを取得（安全に）
        SSID := '';
        if (pBssEntry^.dot11Ssid.uSSIDLength > 0) and
           (pBssEntry^.dot11Ssid.uSSIDLength <= 32) then
        begin
          SetLength(SSID, pBssEntry^.dot11Ssid.uSSIDLength);
          Move(pBssEntry^.dot11Ssid.ucSSID[0], SSID[1], pBssEntry^.dot11Ssid.uSSIDLength);
        end;
        
        // 信号強度（RSSI）を取得
        SignalQuality := pBssEntry^.lRssi;
        
        // 接続されているネットワークを優先
        if (ConnectedSSID <> '') and (SSID <> '') and (SSID = ConnectedSSID) then
        begin
          // 接続されているネットワーク
          Label1.Caption := '接続中: ' + SSID;
          Label2.Caption := '信号強度: ' + IntToStr(SignalQuality) + ' dBm';
          
          // 信号強度をパーセンテージに変換（-100 dBm = 0%, -50 dBm = 100%）
          SignalQuality := ((SignalQuality + 100) * 100) div 50;
          if SignalQuality < 0 then SignalQuality := 0;
          if SignalQuality > 100 then SignalQuality := 100;
          
          Label3.Caption := '品質: ' + IntToStr(SignalQuality) + '%';
          Label4.Caption := FormatDateTime('更新時刻: yyyy/mm/dd hh:nn:ss', Now);
          
          FoundConnected := True;
          Break;
        end;
        
        // 最大信号強度を更新
        if (SSID <> '') and (SignalQuality > MaxSignal) then
        begin
          MaxSignal := SignalQuality;
          BestSSID := SSID;
        end;
      end;

      // 接続されているネットワークが見つからなかった場合、最も強い信号を表示
      if not FoundConnected and (BestSSID <> '') then
      begin
        Label1.Caption := '最強信号: ' + BestSSID;
        Label2.Caption := '信号強度: ' + IntToStr(MaxSignal) + ' dBm';
        
        SignalQuality := ((MaxSignal + 100) * 100) div 50;
        if SignalQuality < 0 then SignalQuality := 0;
        if SignalQuality > 100 then SignalQuality := 100;
        
        Label3.Caption := '品質: ' + IntToStr(SignalQuality) + '%';
        Label4.Caption := FormatDateTime('更新時刻: yyyy/mm/dd hh:nn:ss', Now);
      end;

    finally
      if pBssList <> nil then
      begin
        try
          WlanFreeMemory(pBssList);
        except
          // メモリ解放エラーは無視
        end;
      end;
    end;

    if pConnectionAttributes <> nil then
    begin
      try
        WlanFreeMemory(pConnectionAttributes);
      except
        // メモリ解放エラーは無視
      end;
    end;

  finally
    if pInterfaceList <> nil then
    begin
      try
        WlanFreeMemory(pInterfaceList);
      except
        // メモリ解放エラーは無視
      end;
    end;
  end;
end;

end.
