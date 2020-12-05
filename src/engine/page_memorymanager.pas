unit page_memorymanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PageApi;

type

  { TPageMemoryManager }

  TPageMemoryManager = class
  protected
    FptrMemory, FptrAddressableMemory: Pointer;
    FintMemorySize: Integer;
    FboolIsInitialized: Boolean;

    function InitializeMemoryStructure: Boolean; virtual; abstract;
  public
    constructor Create(StartingAddress: Pointer; TotalMemorySize: Integer);
    destructor Destroy; override;

    function DoInitialize: Boolean;
    function FreeMemory(Reinitialize: Boolean = False): Boolean;

    function PageMMGetMem(Size:ptruint): Pointer; virtual; abstract;
    function PageMMFreeMem(p:pointer): ptruint; virtual; abstract;
    function PageMMFreeMemSize(p:pointer;Size:ptruint): ptruint; virtual; abstract;
    function PageMMAllocMem(Size:ptruint): Pointer; virtual; abstract;
    function PageMMReAllocMem(var p:pointer;Size:ptruint): Pointer; virtual; abstract;
    function PageMMMemSize(p:pointer): ptruint; virtual; abstract;
    function PageMMGetHeapStatus: THeapStatus; virtual; abstract;
    function PageMMGetFPCHeapStatus: TFPCHeapStatus; virtual; abstract;

    property isInitialized: Boolean read FboolIsInitialized;
    property Memory: Pointer read FptrMemory;
    property AddressableMemory: Pointer read FptrAddressableMemory;
  end;

implementation

{ TPageMemoryManager }

constructor TPageMemoryManager.Create(StartingAddress: Pointer;
  TotalMemorySize: Integer);
begin
  FptrMemory := StartingAddress;
  FintMemorySize := TotalMemorySize;
  FboolIsInitialized := Word(StartingAddress^) = PAGE_MM_MAGIC_BYTES;
  FptrAddressableMemory := fptrMemory+SizeOf(PAGE_MM_MAGIC_BYTES);
end;

destructor TPageMemoryManager.Destroy;
begin

end;

function TPageMemoryManager.DoInitialize: Boolean;
begin
  FillByte(FptrMemory^, FintMemorySize, 0);
  Move(PAGE_MM_MAGIC_BYTES, FptrMemory^, SizeOf(PAGE_MM_MAGIC_BYTES));
  Result := InitializeMemoryStructure;
end;

function TPageMemoryManager.FreeMemory(Reinitialize: Boolean): Boolean;
begin
  if Reinitialize then
    DoInitialize
  else
    FillByte(FptrMemory^, FintMemorySize, 0);
end;

end.

