unit wlanapi;

{$mode objfpc}{$H+}

interface

uses
  Windows;

const
  wlanapi_dll = 'wlanapi.dll';

type
  GUID = Windows.GUID;
  DWORD = Windows.DWORD;
  HANDLE = Windows.HANDLE;
  THandle = Windows.THandle;
  BOOL = Windows.BOOL;
  PVOID = Windows.PVOID;
  PPVOID = ^PVOID;
  LPCGUID = ^GUID;

  DOT11_SSID = record
    uSSIDLength: ULONG;
    ucSSID: array[0..31] of UCHAR;
  end;
  PDOT11_SSID = ^DOT11_SSID;

  DOT11_BSS_TYPE = (
    dot11_BSS_type_infrastructure = 1,
    dot11_BSS_type_independent = 2,
    dot11_BSS_type_any = 3
  );

  WLAN_INTERFACE_STATE = (
    wlan_interface_state_not_ready = 0,
    wlan_interface_state_connected = 1,
    wlan_interface_state_ad_hoc_network_formed = 2,
    wlan_interface_state_disconnecting = 3,
    wlan_interface_state_disconnected = 4,
    wlan_interface_state_associating = 5,
    wlan_interface_state_discovering = 6,
    wlan_interface_state_authenticating = 7
  );

  WLAN_ASSOCIATION_ATTRIBUTES = record
    dot11Ssid: DOT11_SSID;
    dot11BssType: DOT11_BSS_TYPE;
    dot11Bssid: array[0..5] of UCHAR;
    dot11PhyType: DWORD;
    uDot11PhyIndex: ULONG;
    wlanSignalQuality: ULONG;
    ulRxRate: ULONG;
    ulTxRate: ULONG;
  end;

  WLAN_SECURITY_ATTRIBUTES = record
    bSecurityEnabled: BOOL;
    bOneXEnabled: BOOL;
    dot11AuthAlgorithm: DWORD;
    dot11CipherAlgorithm: DWORD;
  end;

  WLAN_CONNECTION_ATTRIBUTES = record
    isState: WLAN_INTERFACE_STATE;
    wlanConnectionMode: DWORD;
    strProfileName: array[0..255] of WCHAR;
    wlanAssociationAttributes: WLAN_ASSOCIATION_ATTRIBUTES;
    wlanSecurityAttributes: WLAN_SECURITY_ATTRIBUTES;
  end;
  PWLAN_CONNECTION_ATTRIBUTES = ^WLAN_CONNECTION_ATTRIBUTES;

  WLAN_INTERFACE_INFO = record
    InterfaceGuid: GUID;
    strInterfaceDescription: array[0..255] of WCHAR;
    isState: WLAN_INTERFACE_STATE;
  end;
  PWLAN_INTERFACE_INFO = ^WLAN_INTERFACE_INFO;

  WLAN_INTERFACE_INFO_LIST = record
    dwNumberOfItems: DWORD;
    dwIndex: DWORD;
    InterfaceInfo: array[0..0] of WLAN_INTERFACE_INFO;
  end;
  PWLAN_INTERFACE_INFO_LIST = ^WLAN_INTERFACE_INFO_LIST;

  DOT11_MAC_ADDRESS = array[0..5] of UCHAR;

  DOT11_PHY_TYPE = (
    dot11_phy_type_unknown = 0,
    dot11_phy_type_any = 0,
    dot11_phy_type_fhss = 1,
    dot11_phy_type_dsss = 2,
    dot11_phy_type_irbaseband = 3,
    dot11_phy_type_ofdm = 4,
    dot11_phy_type_hrdsss = 5,
    dot11_phy_type_erp = 6,
    dot11_phy_type_ht = 7,
    dot11_phy_type_vht = 8,
    dot11_phy_type_dmg = 9,
    dot11_phy_type_he = 10,
    dot11_phy_type_eht = 11,
    dot11_phy_type_IHV_start = $80000000,
    dot11_phy_type_IHV_end = $ffffffff
  );

  WLAN_BSS_ENTRY = record
    dot11Ssid: DOT11_SSID;
    uPhyId: ULONG;
    dot11Bssid: DOT11_MAC_ADDRESS;
    dot11BssType: DOT11_BSS_TYPE;
    dot11BssPhyType: DOT11_PHY_TYPE;
    lRssi: LONG;
    uLinkQuality: ULONG;
    bInRegDomain: BOOL;
    usBeaconPeriod: USHORT;
    ullTimestamp: ULONGLONG;
    ullHostTimestamp: ULONGLONG;
    usCapabilityInformation: USHORT;
    ulChCenterFrequency: ULONG;
    wlanRateSet: record
      uRateSetLength: ULONG;
      usRateSet: array[0..125] of USHORT;
    end;
    ulIeOffset: ULONG;
    ulIeSize: ULONG;
  end;
  PWLAN_BSS_ENTRY = ^WLAN_BSS_ENTRY;

  WLAN_BSS_LIST = record
    dwTotalSize: DWORD;
    dwNumberOfItems: DWORD;
    wlanBssEntries: array[0..0] of WLAN_BSS_ENTRY;
  end;
  PWLAN_BSS_LIST = ^WLAN_BSS_LIST;
  PPWLAN_BSS_LIST = ^PWLAN_BSS_LIST;
  PPWLAN_INTERFACE_INFO_LIST = ^PWLAN_INTERFACE_INFO_LIST;

function WlanOpenHandle(
  dwClientVersion: DWORD;
  pReserved: PVOID;
  pdwNegotiatedVersion: PDWORD;
  phClientHandle: PHANDLE
): DWORD; stdcall; external wlanapi_dll;

function WlanCloseHandle(
  hClientHandle: HANDLE;
  pReserved: PVOID
): DWORD; stdcall; external wlanapi_dll;

function WlanEnumInterfaces(
  hClientHandle: HANDLE;
  pReserved: PVOID;
  ppInterfaceList: PPWLAN_INTERFACE_INFO_LIST
): DWORD; stdcall; external wlanapi_dll;

function WlanGetNetworkBssList(
  hClientHandle: HANDLE;
  pInterfaceGuid: LPCGUID;
  pDot11Ssid: PDOT11_SSID;
  dot11BssType: DOT11_BSS_TYPE;
  bSecurityEnabled: BOOL;
  pReserved: PVOID;
  ppWlanBssList: PPWLAN_BSS_LIST
): DWORD; stdcall; external wlanapi_dll;

type
  WLAN_INTF_OPCODE = DWORD;

const
  wlan_intf_opcode_current_connection: WLAN_INTF_OPCODE = $0000000000000001;

function WlanQueryInterface(
  hClientHandle: HANDLE;
  pInterfaceGuid: LPCGUID;
  OpCode: WLAN_INTF_OPCODE;
  pReserved: PVOID;
  pdwDataSize: PDWORD;
  ppData: PPVOID;
  pWlanOpcodeValueType: PVOID
): DWORD; stdcall; external wlanapi_dll;

procedure WlanFreeMemory(pMemory: PVOID); stdcall; external wlanapi_dll;

implementation

end.

