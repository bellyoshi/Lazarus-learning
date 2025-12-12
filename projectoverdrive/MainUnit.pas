unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  EditBtn, Process;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonGenerate: TButton;
    ButtonOpen: TButton;
    ButtonStart: TButton;
    DirEdit1: TDirectoryEdit;
    GroupBoxSpec: TGroupBox;
    GroupBoxLog: TGroupBox;
    Label1: TLabel;
    MemoLog: TMemo;
    MemoSpec: TMemo;
    PanelTop: TPanel;
    PanelActions: TPanel;
    PanelCenter: TPanel;
    Splitter1: TSplitter;
    procedure ButtonGenerateClick(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonOpenClick(Sender: TObject);
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    Process.CommandLine := 'explorer.exe ' + ExtractFileDir(Application.ExeName);
    Process.Execute;
  finally
    Process.Free;
  end;
end;

procedure TForm1.ButtonGenerateClick(Sender: TObject);
var
  IdeaListPath: String;
  IdeaList: TStringList;
  i: Integer;
  Line: String;
  AppName, AppDesc: String;
  SpecContent: String;
  AppDir: String;
  SepPos: Integer;
begin
  IdeaListPath := IncludeTrailingPathDelimiter(DirEdit1.Directory) + 'IDEA_LIST.txt';
  
  if not FileExists(IdeaListPath) then
  begin
    MemoLog.Lines.Add('Error: IDEA_LIST.txt not found in ' + DirEdit1.Directory);
    Exit;
  end;

  IdeaList := TStringList.Create;
  try
    IdeaList.LoadFromFile(IdeaListPath);
    MemoLog.Lines.Add('Loaded IDEA_LIST.txt with ' + IntToStr(IdeaList.Count) + ' lines.');

    for i := 0 to IdeaList.Count - 1 do
    begin
      Line := Trim(IdeaList[i]);
      if Line = '' then Continue;
      
      // Parse "AppName: Description" or "AppName Description"
      SepPos := Pos(':', Line);
      if SepPos > 0 then
      begin
        AppName := Trim(Copy(Line, 1, SepPos - 1));
        AppDesc := Trim(Copy(Line, SepPos + 1, Length(Line)));
      end
      else
      begin
        SepPos := Pos(' ', Line);
        if SepPos > 0 then
        begin
          AppName := Trim(Copy(Line, 1, SepPos - 1));
          AppDesc := Trim(Copy(Line, SepPos + 1, Length(Line)));
        end
        else
        begin
          AppName := Line;
          AppDesc := '';
        end;
      end;
      
      // Sanitize AppName for filename usage
      AppName := StringReplace(AppName, '\', '', [rfReplaceAll]);
      AppName := StringReplace(AppName, '/', '', [rfReplaceAll]);
      AppName := StringReplace(AppName, ':', '', [rfReplaceAll]);
      AppName := StringReplace(AppName, '*', '', [rfReplaceAll]);
      AppName := StringReplace(AppName, '?', '', [rfReplaceAll]);
      AppName := StringReplace(AppName, '"', '', [rfReplaceAll]);
      AppName := StringReplace(AppName, '<', '', [rfReplaceAll]);
      AppName := StringReplace(AppName, '>', '', [rfReplaceAll]);
      AppName := StringReplace(AppName, '|', '', [rfReplaceAll]);
      
      AppDir := IncludeTrailingPathDelimiter(DirEdit1.Directory) + AppName;
      
      if not DirectoryExists(AppDir) then
      begin
        if ForceDirectories(AppDir) then
          MemoLog.Lines.Add('Created folder: ' + AppName)
        else
          MemoLog.Lines.Add('Failed to create folder: ' + AppName);
      end;
      
      SpecContent := MemoSpec.Lines.Text;
      SpecContent := StringReplace(SpecContent, '{APP_NAME}', AppName, [rfReplaceAll]);
      SpecContent := StringReplace(SpecContent, '{APP_DESC}', AppDesc, [rfReplaceAll]);
      
      try
        with TStringList.Create do
        try
          Text := SpecContent;
          SaveToFile(IncludeTrailingPathDelimiter(AppDir) + 'SPEC.md');
          MemoLog.Lines.Add('  -> Generated SPEC.md for ' + AppName);
        finally
          Free;
        end;
      except
        on E: Exception do
          MemoLog.Lines.Add('  -> Error writing SPEC.md: ' + E.Message);
      end;
    end;
    
  finally
    IdeaList.Free;
  end;
end;

procedure TForm1.ButtonStartClick(Sender: TObject);
var
  IdeaListPath: String;
  IdeaList: TStringList;
  i: Integer;
  Line, AppName: String;
  AntigravityCommand: String;
  Process: TProcess;
  AppDir, SpecPath: String;
  SepPos: Integer;
begin
  IdeaListPath := IncludeTrailingPathDelimiter(DirEdit1.Directory) + 'IDEA_LIST.txt';
  
  if not FileExists(IdeaListPath) then
  begin
    MemoLog.Lines.Add('Error: IDEA_LIST.txt not found.');
    Exit;
  end;

  IdeaList := TStringList.Create;
  try
    IdeaList.LoadFromFile(IdeaListPath);
    
    for i := 0 to IdeaList.Count - 1 do
    begin
      Line := Trim(IdeaList[i]);
      if Line = '' then Continue;

      // Parsing must match Generate logic
      SepPos := Pos(':', Line);
      if SepPos > 0 then
        AppName := Trim(Copy(Line, 1, SepPos - 1))
      else
      begin
        SepPos := Pos(' ', Line);
        if SepPos > 0 then
          AppName := Trim(Copy(Line, 1, SepPos - 1))
        else
          AppName := Line;
      end;
      
      // Sanitize AppName
      AppName := StringReplace(AppName, '\', '', [rfReplaceAll]);
      AppName := StringReplace(AppName, '/', '', [rfReplaceAll]);
      AppName := StringReplace(AppName, ':', '', [rfReplaceAll]);
      AppName := StringReplace(AppName, '*', '', [rfReplaceAll]);
      AppName := StringReplace(AppName, '?', '', [rfReplaceAll]);
      AppName := StringReplace(AppName, '"', '', [rfReplaceAll]);
      AppName := StringReplace(AppName, '<', '', [rfReplaceAll]);
      AppName := StringReplace(AppName, '>', '', [rfReplaceAll]);
      AppName := StringReplace(AppName, '|', '', [rfReplaceAll]);
        
      AppDir := IncludeTrailingPathDelimiter(DirEdit1.Directory) + AppName;
      SpecPath := IncludeTrailingPathDelimiter(AppDir) + 'SPEC.md';
      
      if not FileExists(SpecPath) then
      begin
         MemoLog.Lines.Add('Skipping ' + AppName + ': SPEC.md not found.');
         Continue;
      end;
      
      AntigravityCommand := 'antigravity-cli execute --spec="' + SpecPath + '"';
      
      MemoLog.Lines.Add('> Executing for: ' + AppName);
      MemoLog.Lines.Add('  Command: ' + AntigravityCommand);

      Process := TProcess.Create(nil);
      try
        Process.CommandLine := AntigravityCommand;
        Process.Options := Process.Options + [poWaitOnExit, poUsePipes];
        process.ShowWindow := swoHIDE; 
        
        try
          Process.Execute;
          if Process.ExitCode = 0 then
            MemoLog.Lines.Add('  [SUCCESS] Exit Code: 0')
          else
            MemoLog.Lines.Add('  [FAILED] Exit Code: ' + IntToStr(Process.ExitCode));
        except
          on E: Exception do
            MemoLog.Lines.Add('  [ERROR] Failed to execute: ' + E.Message);
        end;

      finally
        Process.Free;
      end;
    end;

  finally
    IdeaList.Free;
  end;
end;

end.
