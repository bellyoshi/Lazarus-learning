unit ViewerBy2ndPlayer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, lclvlc, vlc, libvlc, Forms, Controls, StdCtrls, ExtCtrls;

type
  TViewerBy2ndPlayer = class
  private
    FPlayer: TLCLVlcPlayer;
    FOnPositionChanged: TNotifyEvent;
    FOnLengthChanged: TNotifyEvent;
    FOnStateChanged: TNotifyEvent;
    FOwnerForm: TForm;
    FParentPanel: TPanel;
    procedure InitializePlayer;
  public
    constructor Create(AOwnerForm: TForm; AParentPanel: TPanel);
    destructor Destroy; override;
    
    procedure PlayFile(const FileName: string);
    procedure Play;
    procedure Pause;
    procedure Stop;
    procedure SetPosition(Position: Int64);
    
    property Player: TLCLVlcPlayer read FPlayer;
    property OnPositionChanged: TNotifyEvent read FOnPositionChanged write FOnPositionChanged;
    property OnLengthChanged: TNotifyEvent read FOnLengthChanged write FOnLengthChanged;
    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
  end;

implementation

constructor TViewerBy2ndPlayer.Create(AOwnerForm: TForm; AParentPanel: TPanel);
begin
  inherited Create;
  FOwnerForm := AOwnerForm;
  FParentPanel := AParentPanel;
  InitializePlayer;
end;

destructor TViewerBy2ndPlayer.Destroy;
begin
  if Assigned(FPlayer) then
    FreeAndNil(FPlayer);
  inherited Destroy;
end;

procedure TViewerBy2ndPlayer.InitializePlayer;
begin
  FPlayer := TLCLVlcPlayer.Create(FOwnerForm);
  FPlayer.ParentWindow := FParentPanel;
  // Set up event handlers if needed
end;

procedure TViewerBy2ndPlayer.PlayFile(const FileName: string);
begin
  if not Assigned(FPlayer) then Exit;
  FPlayer.PlayFile(FileName);
end;

procedure TViewerBy2ndPlayer.Play;
begin
  if not Assigned(FPlayer) then Exit;
  FPlayer.Play;
end;

procedure TViewerBy2ndPlayer.Pause;
begin
  if not Assigned(FPlayer) then Exit;
  FPlayer.Pause;
end;

procedure TViewerBy2ndPlayer.Stop;
begin
  if not Assigned(FPlayer) then Exit;
  FPlayer.Stop;
end;

procedure TViewerBy2ndPlayer.SetPosition(Position: Int64);
begin
  if not Assigned(FPlayer) then Exit;
  FPlayer.VideoPosition := Position;
end;

end.

