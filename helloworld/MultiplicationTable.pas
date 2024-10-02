program MultiplicationTable;

var
  i, j: integer;

begin
  for i := 1 to 9 do
  begin
    for j := 1 to 9 do
    begin
      write((i * j):4);  // 計算結果のみを出力
    end;
    writeln;  // 改行して次の段を表示
  end;
end.
