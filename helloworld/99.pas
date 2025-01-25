program MultiplicationTable;

uses
  SysUtils;

var
  i, j: Integer;

begin
  for i := 1 to 9 do
  begin
    for j := 1 to 9 do
    begin
      Write(i * j:4);
    end;
    Writeln;
  end;
end.

