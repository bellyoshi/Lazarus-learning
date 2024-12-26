unit TestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  TTestCase1= class(TTestCase)
  published
    procedure TestHookUp;
    procedure TestA;
  end;

implementation

procedure TTestCase1.TestHookUp;
begin
  CheckEquals(2, 1 + 1);
end;

procedure TTestCase1.TestA;
begin
  CheckEquals(9 , 2 * 5);
end;

initialization

  RegisterTest(TTestCase1);
end.

