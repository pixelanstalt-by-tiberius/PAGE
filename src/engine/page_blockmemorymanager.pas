unit page_blockmemorymanager;

{ TODO: Hardcode SizeOf(MemoryBlock) }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, page_memorymanager, PageApi, Math;

type
  TMemoryBlock = packed record
    Size: Byte; // the following memory chung size in number of Blocks
    isFree: Boolean; { TODO: Maybe magic bits for checking }
  end;

  { TPageBlockMemoryManager }

  TPageBlockMemoryManager = class(TPageMemoryManager)
  private
    FintBlockSize: Word;
  protected
    function InitializeMemoryStructure: Boolean; override;
    function GetMemoryBlockByPointer(p: Pointer): TMemoryBlock; //inline;
    function SetMemoryBlockByPointer(p: Pointer;
      MemoryBlock: TMemoryBlock): Boolean; //inline;
  public
    function PageMMGetMem(Size: ptruint): Pointer; override;
    function PageMMFreeMem(p: pointer): ptruint; override;
    function PageMMFreeMemSize(p: pointer;Size: ptruint): ptruint; override;
    function PageMMAllocMem(Size: ptruint): Pointer; override;
    function PageMMReAllocMem(var p: pointer;Size: ptruint): Pointer; override;
    function PageMMMemSize(p: pointer): ptruint; override;
    function PageMMGetHeapStatus: THeapStatus; override;
    function PageMMGetFPCHeapStatus: TFPCHeapStatus; override;

    property BlockSize: Word read FintBlockSize write FintBlockSize;
  end;

  operator = (memblocka : TMemoryBlock; memblockb : TMemoryBlock) b : boolean;

const
  FREE_MEM_BLOCK: TMemoryBlock = (Size: 0; isFree: True);

implementation

operator=(memblocka: TMemoryBlock; memblockb: TMemoryBlock)b: boolean;
begin
  Result := (memblocka.Size = memblockb.Size) and
    (memblocka.isFree = memblockb.isFree);
end;

{ TPageBlockMemoryManager }

function TPageBlockMemoryManager.InitializeMemoryStructure: Boolean;
begin
  Move(FREE_MEM_BLOCK, FptrAddressableMemory^, SizeOf(FREE_MEM_BLOCK));
  Result := True;
end;

function TPageBlockMemoryManager.GetMemoryBlockByPointer(p: Pointer
  ): TMemoryBlock;
begin
  Move(Pointer(p-SizeOf(TMemoryBlock))^, Result, SizeOf(TMemoryBlock));
end;

function TPageBlockMemoryManager.SetMemoryBlockByPointer(p: Pointer;
  MemoryBlock: TMemoryBlock): Boolean;
begin
  Move(MemoryBlock, Pointer(p-SizeOf(TMemoryBlock))^, SizeOf(TMemoryBlock));
  Result := True;
end;

function TPageBlockMemoryManager.PageMMGetMem(Size: ptruint): Pointer;
var
  ptrOffset, ptrLoop: Pointer;
  MemoryBlock: TMemoryBlock;
begin
  Result := nil;
  ptrOffset := FptrAddressableMemory; { TODO: Maybe set pointer as a const in class var }
  //ptrLoop := ptrOffset;
  while (PtrInt(ptrOffset-FptrAddressableMemory) <=
    PtrInt(FptrAddressableMemory)+ FintMemorySize-FintBlockSize) do
  begin
    Move(ptrOffset^, MemoryBlock, SizeOf(MemoryBlock));
    if (MemoryBlock = FREE_MEM_BLOCK) or ((MemoryBlock.isFree) and
      (MemoryBlock.Size >= Size)) then
    begin
      if (MemoryBlock = FREE_MEM_BLOCK) then
        if Size-PtrInt(ptrOffset-FptrAddressableMemory) < Size then
          Break;

      MemoryBlock.Size := Ceil(Size / FintBlockSize); { TODO: Maybe improve speed }
      MemoryBlock.isFree := False;

      Move(MemoryBlock, ptrOffset^, SizeOf(MemoryBlock));
      Result := Pointer(ptrOffset+SizeOf(MemoryBlock));
      { TODO: Maybe check before reserving any new space }
      if PtrInt(ptrOffset-FptrAddressableMemory)+2*SizeOf(MemoryBlock)+Size <
        FintMemorySize then
        Move(FREE_MEM_BLOCK, Pointer((ptrOffset+SizeOf(MemoryBlock)+Size))^,
          SizeOf(FREE_MEM_BLOCK))
      else
        Exception.Create('Failed to set free memory block information in heap');

      Break;
    end
    else
    begin
      // No free block loaded
      ptrOffset := ptrOffset + MemoryBlock.Size*FintBlockSize;
    end;
  end;
end;

function TPageBlockMemoryManager.PageMMFreeMem(p: pointer): ptruint;
var
  MemoryBlock: TMemoryBlock;
begin
  if (p < FptrAddressableMemory) or (p > FptrMemory+FintMemorySize) then
    Exception.Create('Memory Access Violation');

  Move(Pointer(p-SizeOf(MemoryBlock))^, MemoryBlock, SizeOf(MemoryBlock));
  MemoryBlock.isFree := True;
  Move(MemoryBlock, Pointer(p-SizeOf(MemoryBlock))^, SizeOf(MemoryBlock));
end;

function TPageBlockMemoryManager.PageMMFreeMemSize(p: pointer; Size: ptruint
  ): ptruint;
var
  MemoryBlock: TMemoryBlock;
begin
  if (p < FptrAddressableMemory) or (p > FptrMemory+FintMemorySize) then
    Exception.Create('Memory Access Violation');

  MemoryBlock := GetMemoryBlockByPointer(p); //Move(p-SizeOf(MemoryBlock), MemoryBlock, SizeOf(MemoryBlock));
  if MemoryBlock.Size > Size then
    Exception.Create('Wrong memory chunk size should be freed');

  MemoryBlock.isFree := True;
  SetMemoryBlockByPointer(p, MemoryBlock);
end;

function TPageBlockMemoryManager.PageMMAllocMem(Size: ptruint): Pointer;
begin
  Result := PageMMGetMem(Size);
  FillByte(Result, Size, 0);
end;

function TPageBlockMemoryManager.PageMMReAllocMem(var p: pointer; Size: ptruint
  ): Pointer;
var
  oldMemoryBlock: TMemoryBlock;
begin
  Result := GetMem(Size);
  if p <> nil then
  begin
    oldMemoryBlock := GetMemoryBlockByPointer(p);
    Move(p, Result^, oldMemoryBlock.Size*FintBlockSize);
    oldMemoryBlock.isFree := True;
    SetMemoryBlockByPointer(p, oldMemoryBlock);
  end;
  p := Result;
end;

function TPageBlockMemoryManager.PageMMMemSize(p: pointer): ptruint;
begin
  Result := GetMemoryBlockByPointer(p).Size*FintBlockSize;
end;

function TPageBlockMemoryManager.PageMMGetHeapStatus: THeapStatus;
begin
  { TODO: Implement this }
  Exception.Create('Not implemented');
end;

function TPageBlockMemoryManager.PageMMGetFPCHeapStatus: TFPCHeapStatus;
begin
  { TODO: Implement this }
  Exception.Create('Not implemented');
end;

end.

