program Project1;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Windows;

type
  // 関数ポインタ型の定義
  TAddFunc = function(a, b: Integer): Integer; cdecl;
  TGetValueof1Func = function: Integer; cdecl;
  TGetMessageFunc = function: PAnsiChar; cdecl;

  // DLLのフルパスを取得する関数
  function GetDLLPath: PChar;
  var
    exePath: array[0..MAX_PATH - 1] of Char;
    exeDirectory: string;
    pos: Integer;
  begin
    // 実行ファイルのパスを取得
    GetModuleFileName(0, exePath, MAX_PATH);
    exeDirectory := exePath;

    // パスからディレクトリ部分を抽出
    pos := LastDelimiter('\', exeDirectory);
    if pos > 0 then
      exeDirectory := Copy(exeDirectory, 1, pos);

    // DLLのフルパスを作成し、PCharに変換して返す
    Result := PChar(exeDirectory + 'cppDLL1.dll');
  end;


var
  hDll: HMODULE;
  add: TAddFunc;
  getValueof1: TGetValueof1Func;
  getMessage: TGetMessageFunc;
  sum, value: Integer;
  message: PAnsiChar;
begin
  // DLLをロード
  hDll := LoadLibrary(GetDLLPath());
  if hDll = 0 then
  begin
    Writeln('DLL could not be loaded!');
    Exit;
  end;

  // DLLから関数のアドレスを取得
  pointer(add) := GetProcAddress(hDll, 'add');
  pointer(getValueof1) := GetProcAddress(hDll, 'getValueof1');
  pointer(getMessage) := GetProcAddress(hDll, 'getMessage');

  // 関数が正しく取得できたか確認
  if (@add = nil) or (@getValueof1 = nil) or (@getMessage = nil) then
  begin
    Writeln('Could not locate functions in the DLL!');
    FreeLibrary(hDll);
    Exit;
  end;

  // 関数を呼び出す
  sum := add(10, 20);
  value := getValueof1();
  message := getMessage();

  // 結果を表示
  Writeln('Sum: ', sum);
  Writeln('Value: ', value);
  Writeln('Message: ', string(message)); // AnsiCharをStringに変換

  // DLLを解放
  FreeLibrary(hDll);
end.

