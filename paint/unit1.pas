unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, ComCtrls, Buttons, ExtDlgs, ColorBox;

type
  TDrawingTool = (dtPen, dtBrush, dtEraser, dtRectangle, dtCircle, dtFill);
  
  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    N2: TMenuItem;
    Clear1: TMenuItem;
    View1: TMenuItem;
    ZoomIn1: TMenuItem;
    ZoomOut1: TMenuItem;
    Zoom1001: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ColorBox1: TColorBox;
    TrackBar1: TTrackBar;
    Label4: TLabel;
    RadioGroup1: TRadioGroup;
    ScrollBox1: TScrollBox;
    PaintBox1: TPaintBox;
    ImageList1: TImageList;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure ColorBox1Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    
    // メニューイベント
    procedure New1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure Redo1Click(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure ZoomIn1Click(Sender: TObject);
    procedure ZoomOut1Click(Sender: TObject);
    procedure Zoom1001Click(Sender: TObject);
    
    // ツールボタンイベント
    procedure PenToolClick(Sender: TObject);
    procedure BrushToolClick(Sender: TObject);
    procedure EraserToolClick(Sender: TObject);
    procedure RectangleToolClick(Sender: TObject);
    procedure CircleToolClick(Sender: TObject);
    procedure FillToolClick(Sender: TObject);
    procedure TestFillClick(Sender: TObject);
    
  private
    FBitmap: TBitmap;
    FCurrentTool: TDrawingTool;
    FCurrentColor: TColor;
    FBrushSize: Integer;
    FIsDrawing: Boolean;
    FStartPoint, FEndPoint: TPoint;
    FUndoStack: TList;
    FRedoStack: TList;
    FZoomFactor: Double;
    FFileName: String;
    
    procedure SaveToUndoStack;
    procedure LoadFromUndoStack(UndoStack, RedoStack: TList);
    procedure ClearRedoStack;
    procedure UpdateStatusBar;
    procedure SetBrushSize(Size: Integer);
    procedure SetCurrentTool(Tool: TDrawingTool);
    procedure DrawShape(ACanvas: TCanvas; StartPt, EndPt: TPoint);
    procedure FloodFill(ACanvas: TCanvas; X, Y: Integer; FillColor, BorderColor: TColor);
    procedure FloodFillAuto(ACanvas: TCanvas; X, Y: Integer; FillColor: TColor);
    
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // 初期化
  FBitmap := TBitmap.Create;
  FBitmap.Width := 600;
  FBitmap.Height := 400;
  FBitmap.Canvas.Brush.Color := clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
  
  
  FCurrentTool := dtPen;
  FCurrentColor := clBlack;
  FBrushSize := 5;
  FIsDrawing := False;
  FZoomFactor := 1.0;
  FFileName := '';
  
  FUndoStack := TList.Create;
  FRedoStack := TList.Create;
  
  // 初期状態をアンドゥスタックに保存
  SaveToUndoStack;
  
  // UI更新
  ColorBox1.Selected := FCurrentColor;
  TrackBar1.Position := FBrushSize;
  Label4.Caption := '線の太さ: ' + IntToStr(FBrushSize);
  RadioGroup1.ItemIndex := Ord(FCurrentTool);
  
  // デバッグ情報
  Caption := 'お絵かきソフト - 初期化完了';
  
  // PaintBoxを強制的に再描画
  PaintBox1.Invalidate;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  // アンドゥ/リドゥスタックのクリーンアップ
  if Assigned(FUndoStack) then
  begin
    for i := 0 to FUndoStack.Count - 1 do
    begin
      if Assigned(FUndoStack[i]) then
        TBitmap(FUndoStack[i]).Free;
    end;
    FUndoStack.Free;
    FUndoStack := nil;
  end;
  
  if Assigned(FRedoStack) then
  begin
    for i := 0 to FRedoStack.Count - 1 do
    begin
      if Assigned(FRedoStack[i]) then
        TBitmap(FRedoStack[i]).Free;
    end;
    FRedoStack.Free;
    FRedoStack := nil;
  end;
  
  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not Assigned(FBitmap) then Exit;
  
  if Button = mbLeft then
  begin
    FIsDrawing := True;
    FStartPoint := Point(X, Y);
    FEndPoint := FStartPoint;
    
    // デバッグ情報
    Caption := Format('お絵かきソフト - マウスダウン: X=%d, Y=%d, ツール=%d', [X, Y, Ord(FCurrentTool)]);
    
    // 塗りつぶしツールの場合は即座に実行
    if FCurrentTool = dtFill then
    begin
      SaveToUndoStack;
      // 自動境界検出による塗りつぶしを実行
      FloodFillAuto(FBitmap.Canvas, X, Y, FCurrentColor);
      PaintBox1.Invalidate;
      
      // デバッグ情報
      Caption := Format('お絵かきソフト - 塗りつぶし実行: X=%d, Y=%d, 色=%d', [X, Y, FCurrentColor]);
    end;
  end;
end;

procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if not Assigned(FBitmap) then Exit;
  
  if FIsDrawing and (FCurrentTool in [dtPen, dtBrush, dtEraser]) then
  begin
    SaveToUndoStack;
    
    FBitmap.Canvas.Pen.Color := FCurrentColor;
    FBitmap.Canvas.Pen.Width := FBrushSize;
    
    if FCurrentTool = dtEraser then
      FBitmap.Canvas.Pen.Color := clWhite;
    
    FBitmap.Canvas.MoveTo(FStartPoint.X, FStartPoint.Y);
    FBitmap.Canvas.LineTo(X, Y);
    
    FStartPoint := Point(X, Y);
    PaintBox1.Invalidate;
    
    // デバッグ情報
    Caption := Format('お絵かきソフト - 描画中: X=%d, Y=%d', [X, Y]);
  end
  else if FIsDrawing and (FCurrentTool in [dtRectangle, dtCircle]) then
  begin
    FEndPoint := Point(X, Y);
    PaintBox1.Invalidate;
    
    // デバッグ情報
    Caption := Format('お絵かきソフト - 図形描画中: X=%d, Y=%d', [X, Y]);
  end;
end;

procedure TForm1.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not Assigned(FBitmap) then Exit;
  
  if FIsDrawing and (Button = mbLeft) then
  begin
    FIsDrawing := False;
    FEndPoint := Point(X, Y);
    
    // デバッグ情報
    Caption := Format('お絵かきソフト - マウスアップ: X=%d, Y=%d', [X, Y]);
    
    if FCurrentTool in [dtRectangle, dtCircle] then
    begin
      SaveToUndoStack;
      DrawShape(FBitmap.Canvas, FStartPoint, FEndPoint);
      PaintBox1.Invalidate;
    end;
  end;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  DestRect: TRect;
begin
  if not Assigned(FBitmap) then 
  begin
    Caption := 'お絵かきソフト - FBitmapがnilです';
    Exit;
  end;
  
  try
    // ズーム対応
    DestRect := Rect(0, 0, Round(FBitmap.Width * FZoomFactor), Round(FBitmap.Height * FZoomFactor));
    PaintBox1.Canvas.StretchDraw(DestRect, FBitmap);
    
    // 図形描画中のプレビュー
    if FIsDrawing and (FCurrentTool in [dtRectangle, dtCircle]) then
    begin
      PaintBox1.Canvas.Pen.Color := FCurrentColor;
      PaintBox1.Canvas.Pen.Width := FBrushSize;
      PaintBox1.Canvas.Pen.Style := psDash;
      DrawShape(PaintBox1.Canvas, FStartPoint, FEndPoint);
      PaintBox1.Canvas.Pen.Style := psSolid;
    end;
    
    // デバッグ情報
    Caption := Format('お絵かきソフト - 描画完了: %dx%d', [FBitmap.Width, FBitmap.Height]);
  except
    on E: Exception do
      Caption := 'お絵かきソフト - 描画エラー: ' + E.Message;
  end;
end;

procedure TForm1.ColorBox1Change(Sender: TObject);
begin
  FCurrentColor := ColorBox1.Selected;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  SetBrushSize(TrackBar1.Position);
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  SetCurrentTool(TDrawingTool(RadioGroup1.ItemIndex));
end;

// メニューイベント実装
procedure TForm1.New1Click(Sender: TObject);
begin
  if MessageDlg('新規作成', '現在の描画を破棄して新規作成しますか？', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    FBitmap.Canvas.Brush.Color := clWhite;
    FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
    ClearRedoStack;
    SaveToUndoStack;
    PaintBox1.Invalidate;
    FFileName := '';
    Caption := 'お絵かきソフト - Paint Application';
  end;
end;

procedure TForm1.Open1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    try
      FBitmap.LoadFromFile(OpenDialog1.FileName);
      FFileName := OpenDialog1.FileName;
      Caption := 'お絵かきソフト - ' + ExtractFileName(FFileName);
      ClearRedoStack;
      SaveToUndoStack;
      PaintBox1.Invalidate;
    except
      on E: Exception do
        MessageDlg('エラー', 'ファイルを開けませんでした: ' + E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TForm1.Save1Click(Sender: TObject);
begin
  if FFileName <> '' then
  begin
    try
      FBitmap.SaveToFile(FFileName);
      Caption := 'お絵かきソフト - ' + ExtractFileName(FFileName);
    except
      on E: Exception do
        MessageDlg('エラー', 'ファイルを保存できませんでした: ' + E.Message, mtError, [mbOK], 0);
    end;
  end
  else
    SaveAs1Click(Self);
end;

procedure TForm1.SaveAs1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    try
      FBitmap.SaveToFile(SaveDialog1.FileName);
      FFileName := SaveDialog1.FileName;
      Caption := 'お絵かきソフト - ' + ExtractFileName(FFileName);
    except
      on E: Exception do
        MessageDlg('エラー', 'ファイルを保存できませんでした: ' + E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Undo1Click(Sender: TObject);
begin
  if FUndoStack.Count > 1 then
  begin
    LoadFromUndoStack(FUndoStack, FRedoStack);
    PaintBox1.Invalidate;
  end;
end;

procedure TForm1.Redo1Click(Sender: TObject);
begin
  if FRedoStack.Count > 0 then
  begin
    LoadFromUndoStack(FRedoStack, FUndoStack);
    PaintBox1.Invalidate;
  end;
end;

procedure TForm1.Clear1Click(Sender: TObject);
begin
  if MessageDlg('クリア', 'すべての描画をクリアしますか？', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    SaveToUndoStack;
    FBitmap.Canvas.Brush.Color := clWhite;
    FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
    PaintBox1.Invalidate;
  end;
end;

procedure TForm1.ZoomIn1Click(Sender: TObject);
begin
  FZoomFactor := FZoomFactor * 1.2;
  PaintBox1.Invalidate;
end;

procedure TForm1.ZoomOut1Click(Sender: TObject);
begin
  FZoomFactor := FZoomFactor / 1.2;
  if FZoomFactor < 0.1 then FZoomFactor := 0.1;
  PaintBox1.Invalidate;
end;

procedure TForm1.Zoom1001Click(Sender: TObject);
begin
  FZoomFactor := 1.0;
  PaintBox1.Invalidate;
end;

// ツールボタンイベント実装
procedure TForm1.PenToolClick(Sender: TObject);
begin
  SetCurrentTool(dtPen);
end;

procedure TForm1.BrushToolClick(Sender: TObject);
begin
  SetCurrentTool(dtBrush);
end;

procedure TForm1.EraserToolClick(Sender: TObject);
begin
  SetCurrentTool(dtEraser);
end;

procedure TForm1.RectangleToolClick(Sender: TObject);
begin
  SetCurrentTool(dtRectangle);
end;

procedure TForm1.CircleToolClick(Sender: TObject);
begin
  SetCurrentTool(dtCircle);
end;

procedure TForm1.FillToolClick(Sender: TObject);
begin
  SetCurrentTool(dtFill);
end;

procedure TForm1.TestFillClick(Sender: TObject);
begin
  if not Assigned(FBitmap) then Exit;
  
  // テスト用の図形を描画
  SaveToUndoStack;
  FBitmap.Canvas.Pen.Color := clBlack;
  FBitmap.Canvas.Pen.Width := 2;
  FBitmap.Canvas.Brush.Style := bsClear;
  FBitmap.Canvas.Rectangle(50, 50, 200, 150);
  FBitmap.Canvas.Rectangle(100, 100, 250, 200);
  
  PaintBox1.Invalidate;
  Caption := 'お絵かきソフト - テスト図形を描画しました。塗りつぶしツールでテストしてください。';
end;

// プライベートメソッド実装
procedure TForm1.SaveToUndoStack;
var
  TempBitmap: TBitmap;
begin
  TempBitmap := TBitmap.Create;
  TempBitmap.Assign(FBitmap);
  FUndoStack.Add(TempBitmap);
  
  // アンドゥスタックのサイズ制限（最大20個）
  if FUndoStack.Count > 20 then
  begin
    TBitmap(FUndoStack[0]).Free;
    FUndoStack.Delete(0);
  end;
end;

procedure TForm1.LoadFromUndoStack(UndoStack, RedoStack: TList);
var
  TempBitmap: TBitmap;
begin
  if not Assigned(UndoStack) or not Assigned(RedoStack) then Exit;
  if UndoStack.Count = 0 then Exit;
  
  try
    // 現在の状態をリドゥスタックに保存
    TempBitmap := TBitmap.Create;
    try
      TempBitmap.Assign(FBitmap);
      RedoStack.Add(TempBitmap);
    except
      TempBitmap.Free;
      raise;
    end;
    
    // アンドゥスタックから復元
    if Assigned(UndoStack[UndoStack.Count - 1]) then
    begin
      FBitmap.Assign(TBitmap(UndoStack[UndoStack.Count - 1]));
      TBitmap(UndoStack[UndoStack.Count - 1]).Free;
    end;
    UndoStack.Delete(UndoStack.Count - 1);
  except
    // エラーが発生した場合は何もしない
  end;
end;

procedure TForm1.ClearRedoStack;
var
  i: Integer;
begin
  if not Assigned(FRedoStack) then Exit;
  
  for i := 0 to FRedoStack.Count - 1 do
  begin
    if Assigned(FRedoStack[i]) then
      TBitmap(FRedoStack[i]).Free;
  end;
  FRedoStack.Clear;
end;

procedure TForm1.UpdateStatusBar;
begin
  // ステータスバーの更新（必要に応じて実装）
end;

procedure TForm1.SetBrushSize(Size: Integer);
begin
  FBrushSize := Size;
  Label4.Caption := '線の太さ: ' + IntToStr(FBrushSize);
end;

procedure TForm1.SetCurrentTool(Tool: TDrawingTool);
begin
  FCurrentTool := Tool;
  RadioGroup1.ItemIndex := Ord(Tool);
end;

procedure TForm1.DrawShape(ACanvas: TCanvas; StartPt, EndPt: TPoint);
begin
  ACanvas.Pen.Color := FCurrentColor;
  ACanvas.Pen.Width := FBrushSize;
  ACanvas.Brush.Style := bsClear;
  
  case FCurrentTool of
    dtRectangle:
      ACanvas.Rectangle(StartPt.X, StartPt.Y, EndPt.X, EndPt.Y);
    dtCircle:
      ACanvas.Ellipse(StartPt.X, StartPt.Y, EndPt.X, EndPt.Y);
  end;
end;

procedure TForm1.FloodFill(ACanvas: TCanvas; X, Y: Integer; FillColor, BorderColor: TColor);
var
  OldColor: TColor;
  Stack: array of TPoint;
  StackTop: Integer;
  CurrentPoint: TPoint;
  i: Integer;
  
  procedure PushPoint(AX, AY: Integer);
  begin
    if (AX >= 0) and (AY >= 0) and (AX < ACanvas.Width) and (AY < ACanvas.Height) then
    begin
      if StackTop >= Length(Stack) then
        SetLength(Stack, Length(Stack) + 1000);
      Stack[StackTop] := Point(AX, AY);
      Inc(StackTop);
    end;
  end;
  
  function PopPoint: TPoint;
  begin
    if StackTop > 0 then
    begin
      Dec(StackTop);
      Result := Stack[StackTop];
    end
    else
      Result := Point(-1, -1);
  end;
  
begin
  if not Assigned(ACanvas) then Exit;
  if (X < 0) or (Y < 0) or (X >= ACanvas.Width) or (Y >= ACanvas.Height) then Exit;
  
  try
    OldColor := ACanvas.Pixels[X, Y];
    
    // 既に同じ色の場合は何もしない
    if OldColor = FillColor then Exit;
    
    // 境界色と同じ場合は塗りつぶさない
    if OldColor = BorderColor then Exit;
    
    // スタックベースのFloodFill実装
    SetLength(Stack, 1000);
    StackTop := 0;
    PushPoint(X, Y);
    
    while StackTop > 0 do
    begin
      CurrentPoint := PopPoint;
      if (CurrentPoint.X < 0) or (CurrentPoint.Y < 0) then Continue;
      
      // 現在のピクセルが塗りつぶし対象の色で、かつ境界色でない場合
      if (ACanvas.Pixels[CurrentPoint.X, CurrentPoint.Y] = OldColor) and 
         (ACanvas.Pixels[CurrentPoint.X, CurrentPoint.Y] <> BorderColor) then
      begin
        ACanvas.Pixels[CurrentPoint.X, CurrentPoint.Y] := FillColor;
        
        // 4方向の隣接ピクセルをスタックに追加
        PushPoint(CurrentPoint.X + 1, CurrentPoint.Y);
        PushPoint(CurrentPoint.X - 1, CurrentPoint.Y);
        PushPoint(CurrentPoint.X, CurrentPoint.Y + 1);
        PushPoint(CurrentPoint.X, CurrentPoint.Y - 1);
      end;
    end;
    
    SetLength(Stack, 0);
  except
    on E: Exception do
    begin
      Caption := 'お絵かきソフト - 塗りつぶしエラー: ' + E.Message;
    end;
  end;
end;

procedure TForm1.FloodFillAuto(ACanvas: TCanvas; X, Y: Integer; FillColor: TColor);
var
  OldColor: TColor;
  Stack: array of TPoint;
  StackTop: Integer;
  CurrentPoint: TPoint;
  Visited: array of array of Boolean;
  i, j: Integer;
  
  procedure PushPoint(AX, AY: Integer);
  begin
    if (AX >= 0) and (AY >= 0) and (AX < ACanvas.Width) and (AY < ACanvas.Height) then
    begin
      if not Visited[AX, AY] then
      begin
        if StackTop >= Length(Stack) then
          SetLength(Stack, Length(Stack) + 1000);
        Stack[StackTop] := Point(AX, AY);
        Inc(StackTop);
        Visited[AX, AY] := True;
      end;
    end;
  end;
  
  function PopPoint: TPoint;
  begin
    if StackTop > 0 then
    begin
      Dec(StackTop);
      Result := Stack[StackTop];
    end
    else
      Result := Point(-1, -1);
  end;
  
begin
  if not Assigned(ACanvas) then Exit;
  if (X < 0) or (Y < 0) or (X >= ACanvas.Width) or (Y >= ACanvas.Height) then Exit;
  
  try
    OldColor := ACanvas.Pixels[X, Y];
    
    // 既に同じ色の場合は何もしない
    if OldColor = FillColor then Exit;
    
    // 訪問済み配列を初期化
    SetLength(Visited, ACanvas.Width, ACanvas.Height);
    for i := 0 to ACanvas.Width - 1 do
      for j := 0 to ACanvas.Height - 1 do
        Visited[i, j] := False;
    
    // スタックベースのFloodFill実装
    SetLength(Stack, 1000);
    StackTop := 0;
    PushPoint(X, Y);
    
    while StackTop > 0 do
    begin
      CurrentPoint := PopPoint;
      if (CurrentPoint.X < 0) or (CurrentPoint.Y < 0) then Continue;
      
      // 現在のピクセルが塗りつぶし対象の色の場合
      if ACanvas.Pixels[CurrentPoint.X, CurrentPoint.Y] = OldColor then
      begin
        ACanvas.Pixels[CurrentPoint.X, CurrentPoint.Y] := FillColor;
        
        // 4方向の隣接ピクセルをスタックに追加
        PushPoint(CurrentPoint.X + 1, CurrentPoint.Y);
        PushPoint(CurrentPoint.X - 1, CurrentPoint.Y);
        PushPoint(CurrentPoint.X, CurrentPoint.Y + 1);
        PushPoint(CurrentPoint.X, CurrentPoint.Y - 1);
      end;
    end;
    
    SetLength(Stack, 0);
    SetLength(Visited, 0, 0);
  except
    on E: Exception do
    begin
      Caption := 'お絵かきソフト - 塗りつぶしエラー: ' + E.Message;
    end;
  end;
end;

end.

