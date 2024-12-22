unit TestCaseExample;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  // Test case class definition
  TTestCase1 = class(TTestCase)
  published
    procedure TestAddition;
    procedure TestSubtraction;
  end;

{ TTestCase1 }

procedure TTestCase1.TestAddition;
begin
  CheckEquals(4, 2 + 2, '2 + 2 should be 4');
end;

procedure TTestCase1.TestSubtraction;
begin
  CheckEquals(2, 5 - 3, '5 - 3 should be 2');
end;

begin
  // Run the test case
  WriteLn('Running test case 1...');
  RegisterTest(TTestCase1);
  RunRegisteredTests;
end.

