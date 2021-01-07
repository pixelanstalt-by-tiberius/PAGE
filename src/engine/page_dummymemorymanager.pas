unit page_dummymemorymanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, page_memorymanager, PageApi, Math;

type
  { TPageDummyMemoryManager }

  TPageDummyMemoryManager = class(TPageMemoryManager)
  private
    function RestoreFromMemory: Boolean;
  protected
    function InitializeMemoryStructure: Boolean; override;
  public
    constructor Create(StartingAddress: Pointer; TotalMemorySize: Integer); override;
    function PageMMGetMem(Size: ptruint): Pointer; override;
    function PageMMFreeMem(p: pointer): ptruint; override;
    function PageMMFreeMemSize(p: pointer;Size: ptruint): ptruint; override;
    function PageMMAllocMem(Size: ptruint): Pointer; override;
    function PageMMReAllocMem(var p: pointer;Size: ptruint): Pointer; override;
    function PageMMMemSize(p: pointer): ptruint; override;
    function PageMMGetHeapStatus: THeapStatus; override;
    function PageMMGetFPCHeapStatus: TFPCHeapStatus; override;
  end;




implementation


{ TPageDummyMemoryManager }

function TPageDummyMemoryManager.RestoreFromMemory: Boolean;
begin
  Result := True;
end;

function TPageDummyMemoryManager.InitializeMemoryStructure: Boolean;
begin
  Result := True;
end;

constructor TPageDummyMemoryManager.Create(StartingAddress: Pointer;
  TotalMemorySize: Integer);
begin
  inherited Create(StartingAddress, TotalMemorySize);
end;

function TPageDummyMemoryManager.PageMMGetMem(Size: ptruint): Pointer;
begin
  Result := GetMem(Size);
end;

function TPageDummyMemoryManager.PageMMFreeMem(p: pointer): ptruint;
begin
  Result := FreeMem(p);
end;

function TPageDummyMemoryManager.PageMMFreeMemSize(p: pointer; Size: ptruint
  ): ptruint;
begin
  Result := FreeMem(p);
end;

function TPageDummyMemoryManager.PageMMAllocMem(Size: ptruint): Pointer;
begin
  Result := AllocMem(Size);
end;

function TPageDummyMemoryManager.PageMMReAllocMem(var p: pointer; Size: ptruint
  ): Pointer;
begin
  Result := ReAllocMem(p, Size);
end;

function TPageDummyMemoryManager.PageMMMemSize(p: pointer): ptruint;
begin
  Result := MemSize(p);
end;

function TPageDummyMemoryManager.PageMMGetHeapStatus: THeapStatus;
begin
  Result := GetHeapStatus;
end;

function TPageDummyMemoryManager.PageMMGetFPCHeapStatus: TFPCHeapStatus;
begin
  Result := GetFPCHeapStatus;
end;

end.

