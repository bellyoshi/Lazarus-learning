program countrydataprocessor;

uses
  SysUtils;

var
    CountryName,
  Population,
    PreacherMax,
    PopulationPerPreacher,
    PreacherAvg,
    IncreaseRate,
    Baptisms,
    PioneersAvg,
    Congregations,
    BibleLessonsAvg,
    MemorialAttendance: string;

begin

  // 各国データの処理
  while not Eof(Input) do
  begin
    // 国名の読み取り
    ReadLn(CountryName);

    // 各国のデータを順番に読み取る
    ReadLn(Population);
    ReadLn(PreacherMax);
    ReadLn(PopulationPerPreacher);
    ReadLn(PreacherAvg);
    ReadLn(IncreaseRate);
    ReadLn(Baptisms);
    ReadLn(PioneersAvg);
    ReadLn(Congregations);
    ReadLn(BibleLessonsAvg);
    ReadLn(MemorialAttendance);

    // 標準出力にデータを書き込み
    WriteLn(CountryName + ' | ' + Population + ' | ' + PreacherMax + ' | ' + PopulationPerPreacher + ' | ' + PreacherAvg + ' | ' + IncreaseRate + ' | ' + Baptisms + ' | ' + PioneersAvg + ' | ' + Congregations + ' | ' + BibleLessonsAvg + ' | ' + MemorialAttendance);
  end;
end.

