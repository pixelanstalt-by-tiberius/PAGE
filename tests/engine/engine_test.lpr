program engine_test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, TextTestRunner, page_global, TestFrameworkProxyIfaces, page_blockmm;

var
  TestResult: ITestResult;
begin
  page_global.RegisterTests;
  page_blockmm.RegisterTests;
  TestResult := RunRegisteredTests;
  { if TestResult.ErrorCount > 0 then Halt(1);
  if TestResult.FailureCount > 0 then Halt(2); }
  end.

