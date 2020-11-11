unit page_global;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DynLibs, TestFramework,
  apihelper in '../../src/starter/apihelper.pas', PAGEAPI;

type

 { TPAGE_TestCase }

 TPAGE_TestCase = class(TTestCase)
 protected
   FlibHandle: TLibHandle;
   FAPIHelper: TPAGE_APIHelper;
   procedure SetUp; override;
   procedure TearDown; override;
 published
   procedure TestBindMethods;
 end;


procedure RegisterTests;

const
  PageLibName: String = {$ifdef LINUX}'../lib' +{$endif} 'page.' + SharedSuffix;

implementation

procedure RegisterTests;
begin
  TestFramework.RegisterTests(TPAGE_TestCase.Suite);
end;

{ TPAGE_TestCase }

procedure TPAGE_TestCase.SetUp;
begin
  FLibHandle := LoadLibrary(PageLibName);
  Check(FLibHandle <> NilHandle, 'Library file not found/loaded');
  FAPIHelper := TPAGE_APIHelper.Create;
end;

procedure TPAGE_TestCase.TearDown;
begin
  Check(UnloadLibrary(FLibHandle), 'Error unloading library');
  FLibHandle := NilHandle;
  FAPIHelper.Free;
end;

procedure TPAGE_TestCase.TestBindMethods;
var
  intLoop: Integer;
begin
  FAPIHelper.GetMethodPointersFromLibrary(FLibHandle);
  Check(FAPIHelper.MethodPointerNum = PAGE_METHOD_NUM,
    'APIHelper method pointer array size mismatch');
  for intLoop := 0 to PAGE_METHOD_NUM-1 do
    Check(FAPIHelper.MethodPointers[intLoop] <> nil,
      Format('Method pointer at index %d is nil', [intLoop]));
end;

end.

