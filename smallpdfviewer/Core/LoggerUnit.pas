unit LoggerUnit;

{$IFDEF FPC}
  {$MODE DelphiUnicode}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF FPC}

{$IFNDEF FPC}
  {$A8,B-,E-,F-,G+,H+,I+,J-,K-,M-,N-,P+,Q-,R-,S-,T-,U-,V+,X+,Z1}
  {$STRINGCHECKS OFF}
{$ENDIF ~FPC}

interface

uses
  {$IFDEF FPC}
    {$IFDEF MSWINDOWS}
  Windows, SysUtils;
    {$ELSE}
  dynlibs, SysUtils;
    {$ENDIF MSWINDOWS}
  {$ELSE}
    {$IF CompilerVersion >= 23.0} // XE2+
  WinApi.Windows, SysUtils;
    {$ELSE}
  Windows, SysUtils;
    {$IFEND}
  {$ENDIF FPC}

type
  // Logger class
  TLogger = class
  private
    FEnabled: Boolean;
    FLogFileName: string;
    FLogFile: TextFile;
    FFileOpened: Boolean;
    procedure WriteOutput(const Msg: string);
    procedure OpenLogFile;
    procedure CloseLogFile;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Log(const Msg: string); overload;
    procedure Log(const Msg: string; const Args: array of const); overload;
    procedure LogError(const Msg: string); overload;
    procedure LogError(const Msg: string; const Args: array of const); overload;
    procedure LogInfo(const Msg: string); overload;
    procedure LogInfo(const Msg: string; const Args: array of const); overload;
    procedure LogDebug(const Msg: string); overload;
    procedure LogDebug(const Msg: string; const Args: array of const); overload;
    property Enabled: Boolean read FEnabled write FEnabled;
    property LogFileName: string read FLogFileName write FLogFileName;
  end;

var
  Logger: TLogger;

implementation

{ TLogger }

constructor TLogger.Create;
begin
  inherited Create;
  FEnabled := True;
  FLogFileName := 'application.log';
  FFileOpened := False;
  OpenLogFile;
end;

destructor TLogger.Destroy;
begin
  CloseLogFile;
  inherited Destroy;
end;

procedure TLogger.OpenLogFile;
var
  FileExistsFlag: Boolean;
begin
  if not FFileOpened and (FLogFileName <> '') then
  begin
    try
      FileExistsFlag := FileExists(FLogFileName);
      AssignFile(FLogFile, FLogFileName);
      if FileExistsFlag then
        Append(FLogFile)
      else
        Rewrite(FLogFile);
      
      // UTF-8 BOMを書き込み（新規ファイルの場合のみ）
      if not FileExistsFlag then
      begin
        Write(FLogFile, #$EF#$BB#$BF); // UTF-8 BOM
      end;
      
      FFileOpened := True;
    except
      FFileOpened := False;
    end;
  end;
end;

procedure TLogger.CloseLogFile;
begin
  if FFileOpened then
  begin
    try
      CloseFile(FLogFile);
    except
      // エラーを無視
    end;
    FFileOpened := False;
  end;
end;

procedure TLogger.WriteOutput(const Msg: string);
var
  TimeStr: string;
  LogMsg: string;
  UTF8Msg: UTF8String;
begin
  if FEnabled and FFileOpened then
  begin
    TimeStr := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
    LogMsg := '[' + TimeStr + '] ' + Msg;
    try
      // UTF-8エンコーディングで書き込み
      UTF8Msg := UTF8Encode(LogMsg);
      WriteLn(FLogFile, UTF8Msg);
      Flush(FLogFile);
    except
      // ファイル書き込みエラーを無視
    end;
  end;
end;

procedure TLogger.Log(const Msg: string);
begin
  WriteOutput('[LOG] ' + Msg);
end;

procedure TLogger.Log(const Msg: string; const Args: array of const);
begin
  WriteOutput('[LOG] ' + Format(Msg, Args));
end;

procedure TLogger.LogError(const Msg: string);
begin
  WriteOutput('[ERROR] ' + Msg);
end;

procedure TLogger.LogError(const Msg: string; const Args: array of const);
begin
  WriteOutput('[ERROR] ' + Format(Msg, Args));
end;

procedure TLogger.LogInfo(const Msg: string);
begin
  WriteOutput('[INFO] ' + Msg);
end;

procedure TLogger.LogInfo(const Msg: string; const Args: array of const);
begin
  WriteOutput('[INFO] ' + Format(Msg, Args));
end;

procedure TLogger.LogDebug(const Msg: string);
begin
  WriteOutput('[DEBUG] ' + Msg);
end;

procedure TLogger.LogDebug(const Msg: string; const Args: array of const);
begin
  WriteOutput('[DEBUG] ' + Format(Msg, Args));
end;

initialization
  Logger := TLogger.Create;

finalization
  if Logger <> nil then
  begin
    Logger.Free;
    Logger := nil;
  end;

end.
