unit page_improvedmemorymanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, page_memorymanager, PageApi, Math;

type
  TAllocationBitmap = bitpacked record
    a, b, c, d, e, f, g, h: Boolean;
  end;

  TSpaceBlock = packed record
    NextBlockOffset: Word;
    isFree: Boolean;
  end;
  PSpaceBlock = ^TSpaceBlock;

  TAllocationBitmapArray = array of TAllocationBitmap;
  PAllocationBitmapArray = ^TAllocationBitmapArray;

  { TPageImprovedMemoryManager }

  TPageImprovedMemoryManager = class(TPageMemoryManager)
  private
    function GetEightBytesCapacity: Word;
    function GetFourBytesCapacity: Word;
    function GetOneByteCapacity: Word;
    function GetTwoBytesCapacity: Word;
    procedure SetEightBytesCapacity(AValue: Word);
    procedure SetFourBytesCapacity(AValue: Word);
    procedure SetOneByteCapacity(AValue: Word);
    procedure SetTwoBytesCapacity(AValue: Word);

  protected
    FOneByteCap, FTwoBytesCap, FFourBytesCap, FEightBytesCap: ^Word;
    FFirstFreePointer: Pointer;
    FOneByteAllocationBitmap, FTwoBytesAllocationBitmap,
      FFourBytesAllocationBitmap, FEightBytesAllocationBitmap: Pointer;

    FAutoSwitchToBiggerMemory: Boolean;

    function InitializeMemoryStructure: Boolean; override;
    function GetFreeSpace(Size: ptruint): Pointer;
    function GetEightByteSpace: Pointer;
    function GetFourByteSpace: Pointer;
    function GetTwoByteSpace: Pointer;
    function GetOneByteSpace: Pointer;

    function FreeFreeSpace(p: Pointer): ptruint;
    function FreeEightByteSpace(p: Pointer): ptruint;
    function FreeFourByteSpace(p: Pointer): ptruint;
    function FreeTwoByteSpace(p: Pointer): ptruint;
    function FreeOneByteSpace(p: Pointer): ptruint;

    function GetFreePointerSize(p: Pointer): ptruint;
  public
    constructor Create(StartingAddress: Pointer; TotalMemorySize: Integer);
    function PageMMGetMem(Size: ptruint): Pointer; override;
    function PageMMFreeMem(p: pointer): ptruint; override;
    function PageMMFreeMemSize(p: pointer;Size: ptruint): ptruint; override;
    function PageMMAllocMem(Size: ptruint): Pointer; override;
    function PageMMReAllocMem(var p: pointer;Size: ptruint): Pointer; override;
    function PageMMMemSize(p: pointer): ptruint; override;
    function PageMMGetHeapStatus: THeapStatus; override;
    function PageMMGetFPCHeapStatus: TFPCHeapStatus; override;

    property OneByteCapacity: Word read GetOneByteCapacity
      write SetOneByteCapacity;
    property TwoBytesCapacity: Word read GetTwoBytesCapacity
      write SetTwoBytesCapacity;
    property FourBytesCapacity: Word read GetFourBytesCapacity
      write SetFourBytesCapacity;
    property EightBytesCapacity: Word read GetEightBytesCapacity
      write SetEightBytesCapacity;

    property AutoSwitchToBiggerMemory: Boolean read FAutoSwitchToBiggerMemory
      write FAutoSwitchToBiggerMemory;
  end;

const
  bitA: Byte = %10000000;
  bitB: Byte = %01000000;
  bitC: Byte = %00100000;
  bitD: Byte = %00010000;
  bitE: Byte = %00001000;
  bitF: Byte = %00000100;
  bitG: Byte = %00000010;
  bitH: Byte = %00000001;

  BITMASK: array[0..7] of Byte = (%10000000, %01000000, %00100000,
    %00010000, %00001000, %00000100, %00000010, %00000001);

  EMPTY_SPACE_BLOCK: TSpaceBlock = (NextBlockOffset: 0; isFree: True);


implementation



{ TPageImprovedMemoryManager }

function TPageImprovedMemoryManager.GetEightBytesCapacity: Word;
begin
  Result := FEightBytesCap^;
end;

function TPageImprovedMemoryManager.GetFourBytesCapacity: Word;
begin
  Result := FFourBytesCap^;
end;

function TPageImprovedMemoryManager.GetOneByteCapacity: Word;
begin
  Result := FOneByteCap^;
end;

function TPageImprovedMemoryManager.GetTwoBytesCapacity: Word;
begin
  Result := FTwoBytesCap^;
end;

procedure TPageImprovedMemoryManager.SetEightBytesCapacity(AValue: Word);
begin
  FEightBytesCap^ := AValue;
end;

procedure TPageImprovedMemoryManager.SetFourBytesCapacity(AValue: Word);
begin
  FFourBytesCap^ := AValue;
end;

procedure TPageImprovedMemoryManager.SetOneByteCapacity(AValue: Word);
begin
  FOneByteCap^ := AValue;
end;

procedure TPageImprovedMemoryManager.SetTwoBytesCapacity(AValue: Word);
begin
  FTwoBytesCap^ := AValue;
end;

function TPageImprovedMemoryManager.InitializeMemoryStructure: Boolean;
var
  intRemaining: Integer;
begin
  intRemaining := FintAddressableMemorySize;
  if (FOneByteCap^ = 0) and (FTwoBytesCap^ = 0) and (FFourBytesCap^ = 0) and
    (FEightBytesCap^ = 0) then
  begin
    // Calculate memory segmentation based on addressable size
    // Try to give space priority in this sequence: 4, 8, 2, 1
    // 4 bytes: 1/4 of free available space
    // 8 bytes: 1/6 of remaining available space
    // 2 bytes: 1/4 of rameining available space
    // 1 byte: 1/6 of remaining available space

    // Calculate capacity for four bytes data section
    if (intRemaining div 16) < High(Word) then
    begin
      FFourBytesCap^ := intRemaining div 16;
      Dec(intRemaining, FFourBytesCap^*4);
      { TODO: Calculate next boundary and regard remaining space }
      while FFourBytesCap^ mod 8 <> 0 do
      begin
        Inc(FFourBytesCap^);
        Dec(intRemaining, 4);
      end;
      Dec(intRemaining, FFourBytesCap^ div 8); // Space for allocation Bitmap
    end
    else
    begin
      FFourBytesCap^ := High(Word);
      Dec(intRemaining, High(Word)*4+(High(Word) div 8));
    end;

    // Calculate capacity for eight bytes data section
    if (intRemaining div 48) < High(Word) then
    begin
      FEightBytesCap^ := intRemaining div 48;
      Dec(intRemaining, FEightBytesCap^*8);
      while FEightBytesCap^ mod 8 <> 0 do
      begin
        Inc(FEightBytesCap^);
        Dec(intRemaining, 8);
      end;
      Dec(intRemaining, FEightBytesCap^ div 8); // Space for allocation Bitmap
    end
    else
    begin
      FEightBytesCap^ := High(Word);
      Dec(intRemaining, High(Word)*8+(High(Word) div 8));
    end;

    // Calculate capacity for one byte data section
    if (intRemaining div 8) < High(Word) then
    begin
      FTwoBytesCap^ := intRemaining div 8;
      Dec(intRemaining, FTwoBytesCap^*2);
      while FTwoBytesCap^ mod 8 <> 0 do
      begin
        Inc(FTwoBytesCap^);
        Dec(intRemaining, 2);
      end;
      Dec(intRemaining, FTwoBytesCap^ div 8); // Space for allocation Bitmap
    end
    else
    begin
      FTwoBytesCap^ := High(Word);
      Dec(intRemaining, High(Word)*8+(High(Word) div 8));
    end;


    // Calculate capacity for one byte data section
    if (intRemaining div 6) < High(Word) then
    begin
      FOneByteCap^ := intRemaining div 6;
      Dec(intRemaining, FOneByteCap^);
      while FOneByteCap^ mod 8 <> 0 do
      begin
        Inc(FOneByteCap^);
        Dec(intRemaining);
      end;
      Dec(intRemaining, FOneByteCap^ div 8); // Space for allocation Bitmap
    end
    else
    begin
      FOneByteCap^ := High(Word);
      Dec(intRemaining, High(Word)*8+(High(Word) div 8));
    end;

    PSpaceBlock(FptrAddressableMemory+FOneByteCap^+FTwoBytesCap^*2+
      FFourBytesCap^*4+FEightBytesCap^*8)^ := EMPTY_SPACE_BLOCK;
  end;

  FFirstFreePointer := FptrAddressableMemory+FOneByteCap^+FTwoBytesCap^*2+
    FFourBytesCap^*4+FEightBytesCap^*8;
  FOneByteAllocationBitmap := FptrAddressableMemory+FintAddressableMemorySize-
    SizeOf(Word)*4-(FOneByteCap^ div 8);
  FTwoBytesAllocationBitmap := FOneByteAllocationBitmap-(FTwoBytesCap^ div 8);
  FFourBytesAllocationBitmap := FTwoBytesAllocationBitmap-
    (FFourBytesCap^ div 8);
  FEightBytesAllocationBitmap := FFourBytesAllocationBitmap-
    (FEightBytesCap^ div 8);
end;

function TPageImprovedMemoryManager.GetFreeSpace(Size: ptruint): Pointer;
var
  ptrNext: Pointer;
begin
  Result := nil;
  ptrNext := FFirstFreePointer;
  while Result = nil do
  begin
    if (TSpaceBlock(ptrNext^).NextBlockOffset = 0) or
      (TSpaceBlock(ptrNext^).isFree and
      (TSpaceBlock(ptrNext^).NextBlockOffset >= Size)) then
    begin
      if TSpaceBlock(ptrNext^).NextBlockOffset = 0 then
        if FintAddressableMemorySize-4*SizeOf(Word)-FOneByteCap^-
          FTwoBytesCap^-FFourBytesCap^-FEightBytesCap^-
          (ptrint(FptrAddressableMemory)-ptrint(ptrNext)) >= Size then
        begin
          Result := ptrNext+SizeOf(TSpaceBlock);
          TSpaceBlock(ptrNext^).NextBlockOffset := Size;
          TSpaceBlock(ptrNext^).isFree := False;
          TSpaceBlock((ptrNext+SizeOf(TSpaceBlock)+Size)^) := EMPTY_SPACE_BLOCK;
          Break;
        end
        else
        begin
          // Found last free block but remaining size is too small
          Result := nil;
          Break;
        end;
    end
    else
      ptrNext := ptrNext + SizeOf(TSpaceBlock) +
        TSpaceBlock(ptrNext^).NextBlockOffset;
  end;
end;

function TPageImprovedMemoryManager.GetEightByteSpace: Pointer;
var
  intLoop: Integer;
begin
  Result := nil;
  for intLoop := 0 to FEightBytesCap^-1 do
  begin
    if Byte((FEightBytesAllocationBitmap+(intLoop div 8))^) and
      (BITMASK[intLoop mod 8]) <> BITMASK[intLoop mod 8] then
    begin
      Result := FptrAddressableMemory+FOneByteCap^+FTwoBytesCap^*2+
        FFourBytesCap^*4+intLoop*8;
      Byte((FEightBytesAllocationBitmap+(intLoop div 8))^) :=
        Byte((FEightBytesAllocationBitmap+(intLoop div 8))^) xor
        BITMASK[intLoop mod 8];
      Break;
    end;
  end;

  if (Result = nil) and (FAutoSwitchToBiggerMemory) then
    Result := GetFreeSpace(8);
end;

function TPageImprovedMemoryManager.GetFourByteSpace: Pointer;
var
  intLoop: Integer;
begin
  Result := nil;
  for intLoop := 0 to FFourBytesCap^-1 do
  begin
    if Byte((FFourBytesAllocationBitmap+(intLoop div 8))^) and
      (BITMASK[intLoop mod 8]) <> BITMASK[intLoop mod 8] then
    begin
      Result := FptrAddressableMemory+FOneByteCap^+FTwoBytesCap^*2+intLoop*4;
      Byte((FFourBytesAllocationBitmap+(intLoop div 8))^) :=
        Byte((FFourBytesAllocationBitmap+(intLoop div 8))^) xor
        BITMASK[intLoop mod 8];
      Break;
    end;
  end;

  if (Result = nil) and (FAutoSwitchToBiggerMemory) then
    Result := GetEightByteSpace;
end;

function TPageImprovedMemoryManager.GetTwoByteSpace: Pointer;
var
  intLoop: Integer;
begin
  Result := nil;
  for intLoop := 0 to FTwoBytesCap^-1 do
  begin
    if Byte((FTwoBytesAllocationBitmap+(intLoop div 8))^) and
      (BITMASK[intLoop mod 8]) <> BITMASK[intLoop mod 8] then
    begin
      Result := FptrAddressableMemory+FOneByteCap^+intLoop*2;
      Byte((FTwoBytesAllocationBitmap+(intLoop div 8))^) :=
        Byte((FTwoBytesAllocationBitmap+(intLoop div 8))^) xor
        BITMASK[intLoop mod 8];
      Break;
    end;
  end;

  if (Result = nil) and (FAutoSwitchToBiggerMemory) then
    Result := GetFourByteSpace;
end;

function TPageImprovedMemoryManager.GetOneByteSpace: Pointer;
var
  intLoop: Integer;
begin
  Result := nil;
  for intLoop := 0 to FOneByteCap^-1 do
  begin
    if Byte((FOneByteAllocationBitmap+(intLoop div 8))^) and
      (BITMASK[intLoop mod 8]) <> BITMASK[intLoop mod 8] then
    begin
      Result := FptrAddressableMemory+intLoop;
      Byte((FOneByteAllocationBitmap+(intLoop div 8))^) :=
        Byte((FOneByteAllocationBitmap+(intLoop div 8))^) xor
        BITMASK[intLoop mod 8];
      Break;
    end;
  end;

  if (Result = nil) and (FAutoSwitchToBiggerMemory) then
    Result := GetTwoByteSpace;
end;

function TPageImprovedMemoryManager.FreeFreeSpace(p: Pointer): ptruint;
begin

  TSpaceBlock((p-SizeOf(TSpaceBlock))^).isFree := True;
  Result := TSpaceBlock((p-SizeOf(TSpaceBlock))^).NextBlockOffset;
end;

function TPageImprovedMemoryManager.FreeEightByteSpace(p: Pointer): ptruint;
var
  intOffsetIndex: ptruint;
begin
  if (p >= FptrAddressableMemory+FOneByteCap^+FTwoBytesCap^*2+
    FFourBytesCap^*4) and (p <= FptrAddressableMemory+FOneByteCap^+
    FTwoBytesCap^*2+FFourBytesCap^*4+FEightBytesCap^*8) then
  begin
    intOffsetIndex := (p-(FptrAddressableMemory+FOneByteCap^+FTwoBytesCap^*2+
      FFourBytesCap^*4)) div 8;
    Byte((FEightBytesAllocationBitmap+(intOffsetIndex div 8))^) :=
      Byte((FEightBytesAllocationBitmap+(intOffsetIndex div 8))^) xor BITMASK[(
        intOffsetIndex mod 8)];
    Result := 8;
  end
  else
    Result := 0;
end;

function TPageImprovedMemoryManager.FreeFourByteSpace(p: Pointer): ptruint;
var
  intOffsetIndex: ptruint;
begin
  if (p >= FptrAddressableMemory+FOneByteCap^+FTwoBytesCap^*2) and
    (p <= FptrAddressableMemory+FOneByteCap^+FTwoBytesCap^*2+
      FFourBytesCap^*4) then
  begin
    intOffsetIndex := (p-(FptrAddressableMemory+FOneByteCap^+FTwoBytesCap^*2))
      div 4;
    Byte((FFourBytesAllocationBitmap+(intOffsetIndex div 8))^) :=
      Byte((FFourBytesAllocationBitmap+(intOffsetIndex div 8))^) xor BITMASK[(
        intOffsetIndex mod 8)];
    Result := 4;
  end
  else
    Result := 0;
end;

function TPageImprovedMemoryManager.FreeTwoByteSpace(p: Pointer): ptruint;
var
  intOffsetIndex: ptruint;
begin
  if (p >= FptrAddressableMemory+FOneByteCap^) and
    (p <= FptrAddressableMemory+FOneByteCap^+FTwoBytesCap^*2) then
  begin
    intOffsetIndex := (p-(FptrAddressableMemory+FOneByteCap^)) div 2;
    Byte((FTwoBytesAllocationBitmap+(intOffsetIndex div 8))^) :=
      Byte((FTwoBytesAllocationBitmap+(intOffsetIndex div 8))^) xor BITMASK[(
        intOffsetIndex mod 8)];
    Result := 2;
  end
  else
    Result := 0;
end;

function TPageImprovedMemoryManager.FreeOneByteSpace(p: Pointer): ptruint;
begin
  if (p >= FptrAddressableMemory) and
    (p < FptrAddressableMemory+FOneByteCap^) then
  begin
    Byte((FOneByteAllocationBitmap+((p-FptrAddressableMemory) div 8))^) :=
      Byte((FOneByteAllocationBitmap+((p-FptrAddressableMemory) div 8))^) xor
      BITMASK[(p-FptrAddressableMemory) mod 8];
    Result := 1;
  end
  else
    Result := 0;
end;

function TPageImprovedMemoryManager.GetFreePointerSize(p: Pointer): ptruint;
begin
  if (p-FptrAddressableMemory >=
    FOneByteCap^+FTwoBytesCap^*2+FFourBytesCap^*4+FEightBytesCap^*8) and
    (p < FptrAddressableMemory+FintAddressableMemorySize-4*SizeOf(Word)-
    FOneByteCap^-FTwoBytesCap^-FFourBytesCap^-FEightBytesCap^-
    SizeOf(TSpaceBlock)) then
  begin
    Result := TSpaceBlock((p-SizeOf(TSpaceBlock))^).NextBlockOffset;
  end
  else
    Result := 0;
end;

constructor TPageImprovedMemoryManager.Create(StartingAddress: Pointer;
  TotalMemorySize: Integer);
begin
  inherited Create(StartingAddress, TotalMemorySize);
  FOneByteCap := Pointer(ptruint(FptrAddressableMemory)+
    FintAddressableMemorySize-1);
  FTwoBytesCap := FOneByteCap-1;
  FFourBytesCap := FTwoBytesCap-1;
  FEightBytesCap := FFourBytesCap-1;
  FAutoSwitchToBiggerMemory := True;
end;

function TPageImprovedMemoryManager.PageMMGetMem(Size: ptruint): Pointer;
begin
  if Size > 8 then
    Result := GetFreeSpace(Size)
  else
    if (Size <=8) and (Size > 4) then
      Result := GetEightByteSpace
    else
      if (Size <=4) and (Size > 2) then
        Result := GetFourByteSpace
      else
        if (Size = 2) then
          Result := GetTwoByteSpace
        else
          Result := GetOneByteSpace;
end;

function TPageImprovedMemoryManager.PageMMFreeMem(p: pointer): ptruint;
begin
  Result := 0;
  case PageMMMemSize(p) of
    0: Exit;
    1: Result := FreeOneByteSpace(p);
    2: Result := FreeTwoByteSpace(p);
    4: Result := FreeFourByteSpace(p);
    8: Result := FreeEightByteSpace(p);
    else
      Result := FreeFreeSpace(p);
  end;
end;

function TPageImprovedMemoryManager.PageMMFreeMemSize(p: pointer; Size: ptruint
  ): ptruint;
begin
  Result := PageMMFreeMem(p);
end;

function TPageImprovedMemoryManager.PageMMAllocMem(Size: ptruint): Pointer;
begin
  Result := PageMMGetMem(Size);
  FillByte(Result, Size, 0);
end;

function TPageImprovedMemoryManager.PageMMReAllocMem(var p: pointer; Size: ptruint
  ): Pointer;
var
  oldSize: ptruint;
  pNew: Pointer;
begin
  Result := nil;
  oldSize := PageMMMemSize(p);
  if oldSize > 0 then
  begin
    pNew := PageMMGetMem(Size);
    Move(p, pNew, oldSize);
    PageMMFreeMem(p);
    p := pNew;
    Result := pNew;
  end;
end;

function TPageImprovedMemoryManager.PageMMMemSize(p: pointer): ptruint;
begin
  Result := 0;
  if (p >= FptrAddressableMemory) and
    (p-FptrAddressableMemory < FOneByteCap^) then
    Result := 1
  else
    if (p-FptrAddressableMemory >= FOneByteCap^) and
      (p-FptrAddressableMemory < FOneByteCap^+FTwoBytesCap^*2) then
      Result := 2
    else
      if (p-FptrAddressableMemory >= FOneByteCap^+FTwoBytesCap^*2) and
        (p-FptrAddressableMemory <
          FOneByteCap^+FTwoBytesCap^*2+FFourBytesCap^*4) then
        Result := 4
      else
        if (p-FptrAddressableMemory >=
          FOneByteCap^+FTwoBytesCap^*2+FFourBytesCap^*4) and
          (p-FptrAddressableMemory <
            FOneByteCap^+FTwoBytesCap^*2+FFourBytesCap^*4+
            FEightBytesCap^*8) then
          Result := 8
        else
          Result := GetFreePointerSize(p);
end;

function TPageImprovedMemoryManager.PageMMGetHeapStatus: THeapStatus;
begin

end;

function TPageImprovedMemoryManager.PageMMGetFPCHeapStatus: TFPCHeapStatus;
begin

end;

end.

