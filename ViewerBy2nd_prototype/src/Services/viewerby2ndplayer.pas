unit ViewerBy2ndPlayer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, lclvlc, vlc,  Forms, Controls,  ExtCtrls;



type
  TViewerBy2ndPlayer = class
  private
    FThumbnailPlayer: TLCLVlcPlayer;
    FViewerPlayer: TLCLVlcPlayer;
    FOnPositionChanged: TNotifyEvent;
    FOnLengthChanged: TNotifyEvent;
    FOnStateChanged: TNotifyEvent;
    FOwnerForm: TForm;
    FParentPanel: TPanel;
    FViewerForm: TForm;
    FFileName: string;
    procedure InitializeThumbnailPlayer;
    procedure InitializeViewerPlayer(AViewerPanel: TPanel);
    function GetVideoLength: Int64;
    function GetVideoPosition: Int64;
    procedure SetVideoPosition(const Value: Int64);
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure RegisterThumbnail(AOwnerForm: TForm; AParentPanel: TPanel);
    procedure RegisterViewer(AViewerForm: TForm; AViewerPanel: TPanel);
    procedure PlayFile(const FileName: string);
    procedure Play;
    procedure Pause;
    procedure Stop;
    procedure SetPosition(Position: Int64);
    
    property ThumbnailPlayer: TLCLVlcPlayer read FThumbnailPlayer;
    property ViewerPlayer: TLCLVlcPlayer read FViewerPlayer;
    property VideoLength: Int64 read GetVideoLength;
    property VideoPosition: Int64 read GetVideoPosition write SetVideoPosition;
    property OnPositionChanged: TNotifyEvent read FOnPositionChanged write FOnPositionChanged;
    property OnLengthChanged: TNotifyEvent read FOnLengthChanged write FOnLengthChanged;
    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
    property FileName: string read FFileName;
  end;

var
  player: TViewerBy2ndPlayer;

implementation

constructor TViewerBy2ndPlayer.Create;
begin
  inherited Create;
end;

destructor TViewerBy2ndPlayer.Destroy;
begin
  if Assigned(FThumbnailPlayer) then
    FreeAndNil(FThumbnailPlayer);
  if Assigned(FViewerPlayer) then
    FreeAndNil(FViewerPlayer);
  inherited Destroy;
end;

procedure TViewerBy2ndPlayer.RegisterThumbnail(AOwnerForm: TForm; AParentPanel: TPanel);
begin
  FOwnerForm := AOwnerForm;
  FParentPanel := AParentPanel;
  InitializeThumbnailPlayer;
end;

procedure TViewerBy2ndPlayer.InitializeThumbnailPlayer;
begin
  FThumbnailPlayer := TLCLVlcPlayer.Create(FOwnerForm);
  FThumbnailPlayer.ParentWindow := FParentPanel;
end;

procedure TViewerBy2ndPlayer.InitializeViewerPlayer(AViewerPanel: TPanel);
begin
  if not Assigned(FViewerPlayer) then
  begin
    FViewerPlayer := TLCLVlcPlayer.Create(FOwnerForm);
    FViewerPlayer.ParentWindow := AViewerPanel;
  end;
end;

procedure TViewerBy2ndPlayer.RegisterViewer(AViewerForm: TForm; AViewerPanel: TPanel);
begin
  FViewerForm := AViewerForm;
  InitializeViewerPlayer(AViewerPanel);
end;

procedure TViewerBy2ndPlayer.PlayFile(const FileName: string);
begin
  if not Assigned(FThumbnailPlayer) then Exit;
  
  // 既存の再生を停止
  FThumbnailPlayer.Stop;
  if Assigned(FViewerPlayer) then
    FViewerPlayer.Stop;
  
  FFileName := FileName;
  FThumbnailPlayer.PlayFile(FileName);
  if Assigned(FViewerPlayer) then
    FViewerPlayer.PlayFile(FileName);
end;

procedure TViewerBy2ndPlayer.Play;
begin
  if not Assigned(FThumbnailPlayer) then Exit;
  
  // サムネイルプレーヤーの再生
  FThumbnailPlayer.Play;
  
  // ビューアープレーヤーの再生
  if Assigned(FViewerPlayer) then
  begin
    // ファイル名が指定されているが、ビデオが読み込まれていない場合は再読み込み
    if (FFileName <> '') and (FViewerPlayer.VideoLength = 0) then
      FViewerPlayer.PlayFile(FFileName);
    FViewerPlayer.Play;
  end;
end;

procedure TViewerBy2ndPlayer.Pause;
begin
  if not Assigned(FThumbnailPlayer) then Exit;
  FThumbnailPlayer.Pause;
  if Assigned(FViewerPlayer) then
    FViewerPlayer.Pause;
end;

procedure TViewerBy2ndPlayer.Stop;
begin
  if not Assigned(FThumbnailPlayer) then Exit;
  FThumbnailPlayer.Stop;
  if Assigned(FViewerPlayer) then
    FViewerPlayer.Stop;
end;

procedure TViewerBy2ndPlayer.SetPosition(Position: Int64);
begin
  if not Assigned(FThumbnailPlayer) then Exit;
  FThumbnailPlayer.VideoPosition := Position;
  if Assigned(FViewerPlayer) then
    FViewerPlayer.VideoPosition := Position;
end;

function TViewerBy2ndPlayer.GetVideoLength: Int64;
begin
  if Assigned(FThumbnailPlayer) then
    Result := FThumbnailPlayer.VideoLength
  else
    Result := 0;
end;

function TViewerBy2ndPlayer.GetVideoPosition: Int64;
begin
  if Assigned(FThumbnailPlayer) then
    Result := FThumbnailPlayer.VideoPosition
  else
    Result := 0;
end;

procedure TViewerBy2ndPlayer.SetVideoPosition(const Value: Int64);
begin
  if Assigned(FThumbnailPlayer) then
    FThumbnailPlayer.VideoPosition := Value;
  if Assigned(FViewerPlayer) then
    FViewerPlayer.VideoPosition := Value;
end;

end.

