unit page_memorymanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PageApi, page_eventqueue, Dialogs;

type

  { TODO: Due to Magic Bytes in Memory Wrapper, Magic Bytes here could
          maybe be omitted }

  { TPageMemoryManager }

  TPageMemoryManager = class(TInterfacedObject, IPageMemoryManager)
  protected
    FptrMemory, FptrAddressableMemory: Pointer;
    FintMemorySize, FintAddressableMemorySize: Integer;
    FboolIsInitialized: Boolean;

    function InitializeMemoryStructure: Boolean; virtual; abstract;
    function RestoreFromMemory: Boolean; virtual; abstract;
  public
    constructor Create(StartingAddress: Pointer; TotalMemorySize: Integer); virtual;
    destructor Destroy; override;

    function DoInitialize: Boolean;
    function FreeMemory(Reinitialize: Boolean = False): Boolean;
    function InitializeOrRestore: Boolean;

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

  TPageMemoryManagerClass = class of TPageMemoryManager;

implementation

{ TPageMemoryManager }

constructor TPageMemoryManager.Create(StartingAddress: Pointer;
  TotalMemorySize: Integer);
begin
  FptrMemory := StartingAddress;
  FintMemorySize := TotalMemorySize;
  FboolIsInitialized := Word(StartingAddress^) = PAGE_MM_MAGIC_BYTES;
  FptrAddressableMemory := fptrMemory+SizeOf(PAGE_MM_MAGIC_BYTES);
  fintAddressableMemorySize := FintMemorySize-SizeOf(PAGE_MM_MAGIC_BYTES);
  Self._AddRef; // Prevent object to be freed by accident
end;

destructor TPageMemoryManager.Destroy;
begin
  gEventQueue.CastEventString(etNotification, psMemoryManager, psDebug,
    esWarning, 'Memory Manager was destroyed');
  raise Exception.Create('Memory Manager destroyed');
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
    Result := DoInitialize
  else
  begin
    FillByte(FptrMemory^, FintMemorySize, 0);
    Result := True;
  end;
end;

function TPageMemoryManager.InitializeOrRestore: Boolean;
begin
  if not FboolIsInitialized then
    Result := DoInitialize
  else
    Result := RestoreFromMemory;
end;

end.

