unit page_improvedmm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DynLibs, TestFramework, page_memorymanager in
    '../../src/engine/page_memorymanager.pas',
    page_improvedmemorymanager in
    '../../src/engine/page_improvedmemorymanager.pas';

type

 TDynamicPointerArray = array of pointer;
 PDynamicPointerArray = ^TDynamicPointerArray;

 { TPageImprovedMemoryManagerTestCase }

 TPageImprovedMemoryManagerTestCase = class(TTestCase)
 protected
   FMemory: Pointer;
   FMM: TPageImprovedMemoryManager;
   FOneByte, FTwoByte, FFourByte, FEightByte, FFree: PDynamicPointerArray;

   procedure SetUpOnce; override;
   procedure TearDownOnce; override;
 published
   procedure TestGetAndFreeOneInteger;
   procedure FlashOneByteMemory;
   procedure FlashTwoByteMemory;
   procedure FlashFourByteMemory;
   procedure FlashEightByteMemory;
   procedure TestGetAndFree32B;
 end;


procedure RegisterTests;

implementation

procedure RegisterTests;
begin
  TestFramework.RegisterTests(TPageImprovedMemoryManagerTestCase.Suite);
end;

{ TPageImprovedMemoryManagerTestCase }

procedure TPageImprovedMemoryManagerTestCase.SetUpOnce;
begin
  inherited SetUpOnce;
  FMemory := GetMem(1024);
  FMM := TPageImprovedMemoryManager.Create(FMemory, 1024);
  FMM.DoInitialize;
  Check(FMM.OneByteCapacity > 0, 'One byte storage is 0');
  Check(FMM.TwoBytesCapacity > 0, 'Two bytes storage is 0');
  Check(FMM.FourBytesCapacity > 0, 'Four bytes storage is 0');
  Check(FMM.EightBytesCapacity > 0, 'Eight bytes storage is 0');
end;

procedure TPageImprovedMemoryManagerTestCase.TearDownOnce;
begin
  inherited TearDownOnce;
  FMM.Free;
  FreeMem(FMemory);
end;

procedure TPageImprovedMemoryManagerTestCase.TestGetAndFreeOneInteger;
var
  ptrNew: Pointer;
begin
  ptrNew := nil;
  ptrNew := FMM.PageMMGetMem(SizeOf(Integer));
  Check(ptrNew <> nil, 'Pointer is nil');
  Check((ptrNew > FMemory), 'Pointer out of expected range ' +
    '(lower bounds violation)');
  Check((ptrNew < FMemory+1024), 'Pointer out of expected range ' +
    '(upper bounds violation)');
  Integer(ptrNew^) := 5;
  CheckEquals(5, Integer(ptrNew^), 'Unexpected value');
  Check(FMM.PageMMFreeMem(ptrNew) <> 0, 'Returned 0 on FreeMem');
end;

procedure TPageImprovedMemoryManagerTestCase.FlashOneByteMemory;
var
  intLoop: Integer;
  Pointers: array of Pointer;
  OneMorePointer: Pointer;
begin
  FMM.AutoSwitchToBiggerMemory := False;
  SetLength(Pointers, FMM.OneByteCapacity);
  for intLoop := 0 to FMM.OneByteCapacity-1 do
  begin
    Pointers[intLoop] := nil;
    Pointers[intLoop] := FMM.PageMMGetMem(1);
    Check(Pointers[intLoop] <> nil, 'Failed to flash memory: Pointer was nil ' +
      'earlier than expected');
  end;
  OneMorePointer := FMM.PageMMGetMem(1);
  Check(OneMorePointer = nil, 'Failed flash memory test: Pointer was not nil ' +
    'when it should have been');
  for intLoop := 0 to FMM.OneByteCapacity-1 do
  begin
    Check(FMM.PageMMFreeMem(Pointers[intLoop]) <> 0, 'Failed flash memory ' +
      'test: Failed to free memory on index ' + intToStr(intLoop));
    Pointers[intLoop] := nil;
  end;

  for intLoop := 0 to FMM.OneByteCapacity-1 do
  begin
    Pointers[intLoop] := FMM.PageMMGetMem(1);
    Check(Pointers[intLoop] <> nil, 'Failed to flash memory on second run: ' +
      'Pointer was nil earlier than expected (index: '+ IntToStr(intLoop) +
      ')');
  end;
  OneMorePointer := FMM.PageMMGetMem(1);
  Check(OneMorePointer = nil, 'Failed flash memory second run test: Pointer ' +
    'was not nil when it should have been');
  for intLoop := 0 to FMM.OneByteCapacity-1 do
  begin
    Check(FMM.PageMMFreeMem(Pointers[intLoop]) <> 0, 'Failed flash memory ' +
      'test on second run: Failed to free memory on index ' +
      intToStr(intLoop));
    Pointers[intLoop] := nil;
  end;
end;

procedure TPageImprovedMemoryManagerTestCase.FlashTwoByteMemory;
var
  intLoop: Integer;
  Pointers: array of Pointer;
  OneMorePointer: Pointer;
begin
  FMM.AutoSwitchToBiggerMemory := False;
  SetLength(Pointers, FMM.TwoBytesCapacity);
  for intLoop := 0 to FMM.TwoBytesCapacity-1 do
  begin
    Pointers[intLoop] := nil;
    Pointers[intLoop] := FMM.PageMMGetMem(2);
    Check(Pointers[intLoop] <> nil, 'Failed to flash memory: Pointer was nil ' +
      'earlier than expected');
  end;
  OneMorePointer := FMM.PageMMGetMem(2);
  Check(OneMorePointer = nil, 'Failed flash memory test: Pointer was not nil ' +
    'when it should have been');
  for intLoop := 0 to FMM.TwoBytesCapacity-1 do
  begin
    Check(FMM.PageMMFreeMem(Pointers[intLoop]) <> 0, 'Failed flash memory ' +
      'test: Failed to free memory on index ' + intToStr(intLoop));
    Pointers[intLoop] := nil;
  end;

  for intLoop := 0 to FMM.TwoBytesCapacity-1 do
  begin
    Pointers[intLoop] := FMM.PageMMGetMem(2);
    Check(Pointers[intLoop] <> nil, 'Failed to flash memory on second run: ' +
      'Pointer was nil earlier than expected (index: '+ IntToStr(intLoop) +
      ')');
  end;
  OneMorePointer := FMM.PageMMGetMem(2);
  Check(OneMorePointer = nil, 'Failed flash memory second run test: Pointer ' +
    'was not nil when it should have been');
  for intLoop := 0 to FMM.TwoBytesCapacity-1 do
  begin
    Check(FMM.PageMMFreeMem(Pointers[intLoop]) <> 0, 'Failed flash memory ' +
      'test on second run: Failed to free memory on index ' +
      intToStr(intLoop));
    Pointers[intLoop] := nil;
  end;
end;

procedure TPageImprovedMemoryManagerTestCase.FlashFourByteMemory;
var
  intLoop: Integer;
  Pointers: array of Pointer;
  OneMorePointer: Pointer;
begin
  FMM.AutoSwitchToBiggerMemory := False;
  SetLength(Pointers, FMM.FourBytesCapacity);
  for intLoop := 0 to FMM.FourBytesCapacity-1 do
  begin
    Pointers[intLoop] := nil;
    Pointers[intLoop] := FMM.PageMMGetMem(4);
    Check(Pointers[intLoop] <> nil, 'Failed to flash memory: Pointer was nil ' +
      'earlier than expected');
  end;
  OneMorePointer := FMM.PageMMGetMem(4);
  Check(OneMorePointer = nil, 'Failed flash memory test: Pointer was not nil ' +
    'when it should have been');
  for intLoop := 0 to FMM.FourBytesCapacity-1 do
  begin
    Check(FMM.PageMMFreeMem(Pointers[intLoop]) <> 0, 'Failed flash memory ' +
      'test: Failed to free memory on index ' + intToStr(intLoop));
    Pointers[intLoop] := nil;
  end;

  for intLoop := 0 to FMM.FourBytesCapacity-1 do
  begin
    Pointers[intLoop] := FMM.PageMMGetMem(4);
    Check(Pointers[intLoop] <> nil, 'Failed to flash memory on second run: ' +
      'Pointer was nil earlier than expected (index: '+ IntToStr(intLoop) +
      ')');
  end;
  OneMorePointer := FMM.PageMMGetMem(4);
  Check(OneMorePointer = nil, 'Failed flash memory second run test: Pointer ' +
    'was not nil when it should have been');
  for intLoop := 0 to FMM.FourBytesCapacity-1 do
  begin
    Check(FMM.PageMMFreeMem(Pointers[intLoop]) <> 0, 'Failed flash memory ' +
      'test on second run: Failed to free memory on index ' +
      intToStr(intLoop));
    Pointers[intLoop] := nil;
  end;
end;

procedure TPageImprovedMemoryManagerTestCase.FlashEightByteMemory;
var
  intLoop: Integer;
  Pointers: array of Pointer;
  OneMorePointer: Pointer;
begin
  FMM.AutoSwitchToBiggerMemory := False;
  SetLength(Pointers, FMM.EightBytesCapacity);
  for intLoop := 0 to FMM.EightBytesCapacity-1 do
  begin
    Pointers[intLoop] := nil;
    Pointers[intLoop] := FMM.PageMMGetMem(8);
    Check(Pointers[intLoop] <> nil, 'Failed to flash memory: Pointer was nil ' +
      'earlier than expected');
  end;
  OneMorePointer := FMM.PageMMGetMem(8);
  Check(OneMorePointer = nil, 'Failed flash memory test: Pointer was not nil ' +
    'when it should have been');
  for intLoop := 0 to FMM.EightBytesCapacity-1 do
  begin
    Check(FMM.PageMMFreeMem(Pointers[intLoop]) <> 0, 'Failed flash memory ' +
      'test: Failed to free memory on index ' + intToStr(intLoop));
    Pointers[intLoop] := nil;
  end;

  for intLoop := 0 to FMM.EightBytesCapacity-1 do
  begin
    Pointers[intLoop] := FMM.PageMMGetMem(8);
    Check(Pointers[intLoop] <> nil, 'Failed to flash memory on second run: ' +
      'Pointer was nil earlier than expected (index: '+ IntToStr(intLoop) +
      ')');
  end;
  OneMorePointer := FMM.PageMMGetMem(8);
  Check(OneMorePointer = nil, 'Failed flash memory second run test: Pointer ' +
    'was not nil when it should have been');
  for intLoop := 0 to FMM.EightBytesCapacity-1 do
  begin
    Check(FMM.PageMMFreeMem(Pointers[intLoop]) <> 0, 'Failed flash memory ' +
      'test on second run: Failed to free memory on index ' +
      intToStr(intLoop));
    Pointers[intLoop] := nil;
  end;
end;

procedure TPageImprovedMemoryManagerTestCase.TestGetAndFree32B;
var
  ptr32b: Pointer;
  intLoop: Integer;
begin
  ptr32b := FMM.PageMMGetMem(32);
  Check(ptr32b <> nil, 'Failed to get 32b memory');
  FillByte(ptr32b^, 32, 3);
  for intLoop := 0 to 31 do
    Check(Byte((ptr32b+intLoop)^) = 3, 'Failed variable check on index ' +
      IntToStr(intLoop));
  Check(FMM.PageMMFreeMem(ptr32b) <> 0, 'Failed to free 32kb memory');
end;


end.


