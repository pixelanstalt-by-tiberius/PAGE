unit page_blockmm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DynLibs, TestFramework, page_memorymanager in
    '../../src/engine/page_memorymanager.pas',
    page_blockmemorymanager in
    '../../src/engine/page_blockmemorymanager.pas';

type

 { TPAGE_TestCase }

 { TPageMemoryManagerTestCase }

 TPageMemoryManagerTestCase = class(TTestCase)
 protected
   FMemory: Pointer;
   //FMM: TPageBlockMemoryManager;

   procedure SetUpOnce; override;
   procedure TearDownOnce; override;
 published
   procedure TestGetMem;
   procedure TestFreeMem;
   procedure TestAllocMem;
   procedure TestMemSize;
   procedure TestReinitalization;
 end;


procedure RegisterTests;

implementation

procedure RegisterTests;
begin
  TestFramework.RegisterTests(TPageMemoryManagerTestCase.Suite);
end;

{ TPageMemoryManagerTestCase }

procedure TPageMemoryManagerTestCase.SetUpOnce;
begin
  //inherited SetUpOnce;
  //FMemory := GetMem(1024);
  //FMM := TPageBlockMemoryManager.Create(FMemory, 1024);
  //Check(FMM.Memory = FMemory);
  //FMM.BlockSize := 8;
  //FMM.DoInitialize;
end;

procedure TPageMemoryManagerTestCase.TearDownOnce;
begin
  //inherited TearDownOnce;
  //FMM.Free;
  //FreeMem(FMemory);
end;

procedure TPageMemoryManagerTestCase.TestGetMem;
//var
  //ptrNew: Pointer;
begin
  {ptrNew := nil;
  ptrNew := FMM.PageMMGetMem(SizeOf(Integer));
  Check(ptrNew <> nil, 'Pointer is nil');
  Check((ptrNew > FMemory), 'Pointer out of expected range ' +
    '(lower bounds violation)');
  Check((ptrNew < FMemory+1024), 'Pointer out of expected range ' +
    '(upper bounds violation)');
  Integer(ptrNew^) := 5;
  CheckEquals(5, Integer(ptrNew^), 'Unexpected value');}
end;

procedure TPageMemoryManagerTestCase.TestFreeMem;
begin

end;

procedure TPageMemoryManagerTestCase.TestAllocMem;
begin

end;

procedure TPageMemoryManagerTestCase.TestMemSize;
begin

end;

procedure TPageMemoryManagerTestCase.TestReinitalization;
begin

end;

end.


