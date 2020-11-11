program engine_test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, TextTestRunner, page_global;

begin
  page_global.RegisterTests;
  RunRegisteredTests;
  { TODO: Check if testresult is returned as application exit code }
end.

