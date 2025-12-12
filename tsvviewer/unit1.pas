unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, Menus, StdCtrls,
  ExtCtrls, Clipbrd, Types;

type
  // TSV行データを保持するレコード
  TTSVRow = record
    Cells: TStringArray;
    OriginalIndex: Integer; // 元の行番号（ソート後も追跡可能）
  end;

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    MenuItemFile: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemSeparator1: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    Panel1: TPanel;
    Label1: TLabel;
    EditFilter: TEdit;
    LabelStatus: TLabel;
    StringGrid1: TStringGrid;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure MenuItemOpenClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemCopyClick(Sender: TObject);
    procedure MenuItemSelectAllClick(Sender: TObject);
    procedure EditFilterChange(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure StringGrid1HeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure StringGrid1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StringGrid1PrepareCanvas(Sender: TObject; ACol, ARow: Integer;
      AState: TGridDrawState);
  private
    FAllRows: array of TTSVRow; // 全データ
    FFilteredRows: array of Integer; // フィルタ後の行インデックス
    FColumnCount: Integer;
    FSortColumn: Integer; // ソート対象列（-1はソートなし）
    FSortAscending: Boolean;
    
    procedure LoadTSVFile(const FileName: String);
    procedure ParseTSVLine(const Line: String; out Cells: TStringArray);
    procedure UpdateGrid;
    procedure ApplyFilter;
    procedure SortData(Column: Integer);
    function CompareRows(const Row1, Row2: Integer): Integer;
  public
    procedure LoadTSVFileFromPath(const FileName: String);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FColumnCount := 0;
  FSortColumn := -1;
  FSortAscending := True;
  StringGrid1.RowCount := 1;
  StringGrid1.ColCount := 1;
  LabelStatus.Caption := 'ファイルを開いてください';
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
  if Length(FileNames) > 0 then
    LoadTSVFile(FileNames[0]);
end;

procedure TForm1.MenuItemOpenClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    LoadTSVFile(OpenDialog1.FileName);
end;

procedure TForm1.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.MenuItemCopyClick(Sender: TObject);
var
  i, j: Integer;
  SelectedRows: array of Integer;
  CopyText: String;
  RowIdx: Integer;
begin
  if (StringGrid1.Selection.Top < 0) or (StringGrid1.Selection.Top = 0) then Exit;
  
  // 選択された行を取得（ヘッダー行を除く）
  SetLength(SelectedRows, 0);
  for i := StringGrid1.Selection.Top to StringGrid1.Selection.Bottom do
  begin
    if (i > 0) and ((i - 1) >= 0) and ((i - 1) < Length(FFilteredRows)) then
    begin
      SetLength(SelectedRows, Length(SelectedRows) + 1);
      SelectedRows[Length(SelectedRows) - 1] := i - 1; // ヘッダー行を考慮
    end;
  end;
  
  if Length(SelectedRows) = 0 then Exit;
  
  // TSV形式でコピー
  CopyText := '';
  for i := 0 to Length(SelectedRows) - 1 do
  begin
    RowIdx := FFilteredRows[SelectedRows[i]];
    if RowIdx < Length(FAllRows) then
    begin
      for j := 0 to Length(FAllRows[RowIdx].Cells) - 1 do
      begin
        if j > 0 then CopyText := CopyText + #9;
        CopyText := CopyText + FAllRows[RowIdx].Cells[j];
      end;
      if i < Length(SelectedRows) - 1 then
        CopyText := CopyText + LineEnding;
    end;
  end;
  
  Clipboard.AsText := CopyText;
end;

procedure TForm1.MenuItemSelectAllClick(Sender: TObject);
var
  SelRect: TGridRect;
begin
  if Length(FFilteredRows) > 0 then
  begin
    SelRect.Left := 1; // 行番号列を除く
    SelRect.Top := 1; // ヘッダー行を除く
    SelRect.Right := StringGrid1.ColCount - 1;
    SelRect.Bottom := Length(FFilteredRows); // ヘッダー行を考慮
    StringGrid1.Selection := SelRect;
  end;
end;

procedure TForm1.EditFilterChange(Sender: TObject);
begin
  ApplyFilter;
end;

procedure TForm1.StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  RowIdx: Integer;
  CellText: String;
  TextRect: TRect;
  DataCol: Integer;
begin
  // 最初の列（ACol = 0）は行番号列
  if ACol = 0 then
  begin
    if ARow = 0 then
      CellText := '' // ヘッダー行の行番号列は空
    else
    begin
      // フィルタ後の行の元の行番号を表示
      RowIdx := ARow - 1;
      if (RowIdx >= 0) and (RowIdx < Length(FFilteredRows)) then
      begin
        RowIdx := FFilteredRows[RowIdx];
        if RowIdx < Length(FAllRows) then
          CellText := IntToStr(FAllRows[RowIdx].OriginalIndex + 1) // 元の行番号（1から始まる）
        else
          CellText := IntToStr(ARow);
      end
      else
        CellText := IntToStr(ARow);
    end;
  end
  else
  begin
    // データ列（ACol >= 1）
    DataCol := ACol - 1; // データ列のインデックス
    
    if ARow = 0 then
    begin
      // ヘッダー行
      if DataCol < FColumnCount then
        CellText := '列 ' + IntToStr(DataCol + 1)
      else
        CellText := '';
    end
    else
    begin
      // データ行
      RowIdx := ARow - 1;
      if (RowIdx >= 0) and (RowIdx < Length(FFilteredRows)) then
      begin
        RowIdx := FFilteredRows[RowIdx];
        if (RowIdx < Length(FAllRows)) and (DataCol < Length(FAllRows[RowIdx].Cells)) then
          CellText := FAllRows[RowIdx].Cells[DataCol]
        else
          CellText := '';
      end
      else
        CellText := '';
    end;
  end;
  
  // 背景色を設定
  if gdSelected in State then
    StringGrid1.Canvas.Brush.Color := clHighlight
  else if (ACol = 0) and (ARow > 0) then
    StringGrid1.Canvas.Brush.Color := clBtnFace // 行番号列は背景色を変える
  else
    StringGrid1.Canvas.Brush.Color := StringGrid1.Color;
  
  // 背景を描画
  StringGrid1.Canvas.FillRect(Rect);
  
  // テキストを描画
  TextRect := Rect;
  InflateRect(TextRect, -2, -2); // パディング
  if ACol = 0 then
  begin
    // 行番号は右寄せ（テキスト幅を考慮）
    StringGrid1.Canvas.TextRect(TextRect, 
      TextRect.Right - StringGrid1.Canvas.TextWidth(CellText) - 4,
      (TextRect.Top + TextRect.Bottom - StringGrid1.Canvas.TextHeight(CellText)) div 2, 
      CellText);
  end
  else
    StringGrid1.Canvas.TextRect(TextRect, TextRect.Left + 2, TextRect.Top, CellText);
end;

procedure TForm1.StringGrid1HeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
var
  DataCol: Integer;
begin
  // 行番号列（Index = 0）はクリック不可
  if IsColumn and (Index > 0) then
  begin
    DataCol := Index - 1; // データ列のインデックス
    if DataCol < FColumnCount then
    begin
      if FSortColumn = DataCol then
        FSortAscending := not FSortAscending
      else
      begin
        FSortColumn := DataCol;
        FSortAscending := True;
      end;
      SortData(FSortColumn);
      UpdateGrid;
    end;
  end;
end;

procedure TForm1.StringGrid1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Ctrl+C でコピー
  if (Key = 67) and (ssCtrl in Shift) then
  begin
    MenuItemCopyClick(Sender);
    Key := 0;
  end
  // Ctrl+A で全選択
  else if (Key = 65) and (ssCtrl in Shift) then
  begin
    MenuItemSelectAllClick(Sender);
    Key := 0;
  end;
end;

procedure TForm1.StringGrid1PrepareCanvas(Sender: TObject; ACol, ARow: Integer;
  AState: TGridDrawState);
begin
  // 選択行の背景色を設定
  if gdSelected in AState then
    StringGrid1.Canvas.Brush.Color := clHighlight;
end;

procedure TForm1.LoadTSVFile(const FileName: String);
var
  FileStream: TFileStream;
  StringList: TStringList;
  i: Integer;
  Cells: TStringArray;
  MaxCols: Integer;
begin
  try
    // ステータスを更新
    LabelStatus.Caption := '読み込み中...';
    Application.ProcessMessages;
    
    // ストリーム読み込みで最適化
    StringList := TStringList.Create;
    try
      FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      try
        StringList.LoadFromStream(FileStream);
      finally
        FileStream.Free;
      end;
      
      // データをクリア
      SetLength(FAllRows, 0);
      FColumnCount := 0;
      MaxCols := 0;
      
      // 容量を事前に確保（パフォーマンス向上）
      if StringList.Count > 0 then
        SetLength(FAllRows, StringList.Count);
      
      // 各行をパース
      for i := 0 to StringList.Count - 1 do
      begin
        // 進捗表示（10000行ごと、または最後の行）
        if ((i mod 10000 = 0) and (i > 0)) or (i = StringList.Count - 1) then
        begin
          LabelStatus.Caption := Format('読み込み中... %d / %d 行', [i + 1, StringList.Count]);
          Application.ProcessMessages;
        end;
        
        ParseTSVLine(StringList[i], Cells);
        if Length(Cells) > MaxCols then
          MaxCols := Length(Cells);
        
        FAllRows[i].Cells := Cells;
        FAllRows[i].OriginalIndex := i;
      end;
      
      FColumnCount := MaxCols;
      FSortColumn := -1;
      FSortAscending := True;
      
      // グリッドを更新
      UpdateGrid;
      
      LabelStatus.Caption := Format('行数: %d, 列数: %d', [Length(FAllRows), FColumnCount]);
      Caption := 'TSV Viewer - ' + ExtractFileName(FileName);
    finally
      StringList.Free;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('ファイルの読み込みに失敗しました: ' + E.Message);
      LabelStatus.Caption := 'エラー: ' + E.Message;
    end;
  end;
end;

procedure TForm1.ParseTSVLine(const Line: String; out Cells: TStringArray);
var
  Parts: TStringArray;
  i: Integer;
begin
  // タブで分割
  Parts := Line.Split(#9);
  SetLength(Cells, Length(Parts));
  for i := 0 to Length(Parts) - 1 do
    Cells[i] := Parts[i];
end;

procedure TForm1.UpdateGrid;
var
  i: Integer;
begin
  ApplyFilter;
  
  // グリッドの列数を設定（行番号列 + データ列）
  StringGrid1.ColCount := FColumnCount + 1;
  
  // 行番号列の幅を設定（行数に応じて動的に調整）
  if Length(FFilteredRows) > 0 then
  begin
    // 最大行番号の桁数に応じて幅を調整
    i := Length(FFilteredRows);
    if i < 10 then
      StringGrid1.ColWidths[0] := 40
    else if i < 100 then
      StringGrid1.ColWidths[0] := 50
    else if i < 1000 then
      StringGrid1.ColWidths[0] := 60
    else if i < 10000 then
      StringGrid1.ColWidths[0] := 70
    else
      StringGrid1.ColWidths[0] := 80;
  end
  else
    StringGrid1.ColWidths[0] := 60;
  
  // グリッドの行数を設定（ヘッダー行 + データ行）
  StringGrid1.RowCount := Length(FFilteredRows) + 1;
  
  // 列幅を自動調整（サンプル行のみを使用して高速化）
  if (FColumnCount > 0) and (Length(FFilteredRows) > 0) then
  begin
    // データ列の幅を設定
    for i := 1 to FColumnCount do
    begin
      StringGrid1.ColWidths[i] := 100; // デフォルト幅
    end;
    // 必要に応じてAutoSizeColumnsを呼び出す（大量データの場合はコメントアウト推奨）
    // StringGrid1.AutoSizeColumns;
  end;
  
  // グリッドを再描画
  StringGrid1.Invalidate;
end;

procedure TForm1.ApplyFilter;
var
  i, j: Integer;
  FilterText: String;
  Match: Boolean;
begin
  FilterText := Trim(LowerCase(EditFilter.Text));
  
  SetLength(FFilteredRows, 0);
  
  if FilterText = '' then
  begin
    // フィルタなし：全行を表示
    SetLength(FFilteredRows, Length(FAllRows));
    for i := 0 to Length(FAllRows) - 1 do
      FFilteredRows[i] := i;
  end
  else
  begin
    // フィルタ適用：検索文字列を含む行のみ
    for i := 0 to Length(FAllRows) - 1 do
    begin
      Match := False;
      for j := 0 to Length(FAllRows[i].Cells) - 1 do
      begin
        if Pos(FilterText, LowerCase(FAllRows[i].Cells[j])) > 0 then
        begin
          Match := True;
          Break;
        end;
      end;
      
      if Match then
      begin
        SetLength(FFilteredRows, Length(FFilteredRows) + 1);
        FFilteredRows[Length(FFilteredRows) - 1] := i;
      end;
    end;
  end;
  
  // ソートが有効な場合は再ソート
  if FSortColumn >= 0 then
    SortData(FSortColumn);
  
  // グリッドの行数を更新
  StringGrid1.RowCount := Length(FFilteredRows) + 1;
  
  // ステータス更新
  if FilterText = '' then
    LabelStatus.Caption := Format('行数: %d, 列数: %d', [Length(FAllRows), FColumnCount])
  else
    LabelStatus.Caption := Format('表示: %d / %d 行, 列数: %d', [Length(FFilteredRows), Length(FAllRows), FColumnCount]);
end;

procedure TForm1.SortData(Column: Integer);
var
  i, j, Temp: Integer;
begin
  if (Column < 0) or (Column >= FColumnCount) or (Length(FFilteredRows) <= 1) then
    Exit;
  
  // バブルソート（シンプルな実装）
  for i := 0 to Length(FFilteredRows) - 2 do
  begin
    for j := i + 1 to Length(FFilteredRows) - 1 do
    begin
      if CompareRows(FFilteredRows[i], FFilteredRows[j]) > 0 then
      begin
        Temp := FFilteredRows[i];
        FFilteredRows[i] := FFilteredRows[j];
        FFilteredRows[j] := Temp;
      end;
    end;
  end;
end;

function TForm1.CompareRows(const Row1, Row2: Integer): Integer;
var
  Val1, Val2: String;
  Num1, Num2: Extended;
begin
  if (Row1 >= Length(FAllRows)) or (Row2 >= Length(FAllRows)) then
  begin
    Result := 0;
    Exit;
  end;
  
  // 指定列の値を取得
  if FSortColumn < Length(FAllRows[Row1].Cells) then
    Val1 := FAllRows[Row1].Cells[FSortColumn]
  else
    Val1 := '';
    
  if FSortColumn < Length(FAllRows[Row2].Cells) then
    Val2 := FAllRows[Row2].Cells[FSortColumn]
  else
    Val2 := '';
  
  // 数値として比較を試みる
  if TryStrToFloat(Val1, Num1) and TryStrToFloat(Val2, Num2) then
  begin
    if Num1 < Num2 then
      Result := -1
    else if Num1 > Num2 then
      Result := 1
    else
      Result := 0;
  end
  else
  begin
    // 文字列として比較
    Result := CompareText(Val1, Val2);
  end;
  
  if not FSortAscending then
    Result := -Result;
end;

procedure TForm1.LoadTSVFileFromPath(const FileName: String);
begin
  if FileExists(FileName) then
    LoadTSVFile(FileName)
  else
    ShowMessage('ファイルが見つかりません: ' + FileName);
end;

end.
