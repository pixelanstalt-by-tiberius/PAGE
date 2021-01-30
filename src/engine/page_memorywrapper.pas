unit page_memorywrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PageAPI, SDL2, page_memorymanager,
  page_improvedmemorymanager, page_dummymemorymanager, page_EventQueue;

type
  TPageWRAMLayout = packed record
    isInitializedMagicBytes: array[0..2] of Char;
    SDLWindow: PSDL_Window;
    SDLRenderer: PSDL_Renderer;

    boolExitGameLoop: Boolean;
    boolRenderOneFrame: Boolean;
  end;

  TPageVRAMLayout = packed record
    isInitializedMagicBytes: array[0..2] of Char;
    RenderEngine: TPageRenderEngineInfo;
    TextureManagerInfo: TPageTextureManagerInfo;
    Tilemaps: TPageTilemaps;
    // Anything afterwards is memory managed by a memory manager
  end;

  { TPageMemoryWrapper }

  TPageMemoryWrapper = class(TInterfacedObject, IPageMemoryWrapper)
  private
    FVRAMPointer: Pointer;
    FWRAMPointer: Pointer;
    FVRAMSize: Integer;
    FWRAMSize: Integer;

    FWRAMMemMan, FVRAMMemMan: TPageMemoryManager;
    FMemoryManager: TPageMemoryManagerClass;

    FOnAfterVRAMInitialized, FOnAfterWRAMInitialized: TNotifyEvent;

    function GetExitGameLoop: Boolean;
    function GetRenderEngineInfo: PPageRenderEngineInfo;
    //function GetRenderEngineInfoPointer: Pointer;
    function GetRenderOneFrame: Boolean;
    function GetSDLRenderer: PSDL_Renderer;
    function GetSDLWindow: PSDL_Window;
    function GetTextureManagerInfo: TPageTextureManagerInfo;
    function GetTextureManagerInfoPointer: Pointer;
    function GetTilemapInfo(Index: Integer): TPageTilemapInfo;
    function GetTilemapInfoPointer(Index: Integer): PPageTilemapInfo;
    //function GetTilemaps: TPageTilemaps;
    function GetVRAMInitializationStatus: Boolean;
    function GetVRAMPointer: Pointer;
    function GetVRAMSize: Integer;
    function GetWRAMInitializationStatus: Boolean;
    function GetWRAMPointer: Pointer;
    function GetWRAMSize: Integer;
    procedure SetExitGameLoop(AValue: Boolean);
    //procedure SetRenderEngineInfo(AValue: TPageRenderEngineInfo);
    procedure SetRenderOneFrame(AValue: Boolean);
    procedure SetSDLRenderer(AValue: PSDL_Renderer);
    procedure SetSDLWindow(AValue: PSDL_Window);
    procedure SetTilemapInfo(Index: Integer; AValue: TPageTilemapInfo);
    //procedure SetTilemaps(AValue: TPageTilemaps);

    { procedure InitializeVRAMMemoryManagerIfPossible;
    procedure InitializeWRAMMemoryManagerIfPossible; }
  public
    constructor Create;

    function VRAMMemoryManagerInterface: IPageMemoryManager;
    function WRAMMemoryManagerInterface: IPageMemoryManager;
    function Bind(aWRAM: Pointer; aWRAMSize: Integer; aVRAM: Pointer;
      aVRAMSize: Integer): Boolean;

    procedure InitializeWRAM;
    procedure InitializeVRAM;

    property isWRAMInitialized: Boolean read GetWRAMInitializationStatus;
    property isVRAMInitialized: Boolean read GetVRAMInitializationStatus;

    property VRAM: Pointer read GetVRAMPointer;
    property VRAMSize: Integer read GetVRAMSize;
    property RenderEngineInfo: PPageRenderEngineInfo read GetRenderEngineInfo;
     // write SetRenderEngineInfo;
    //property RenderEngineInfoPointer: Pointer read GetRenderEngineInfoPointer;
    //property Tilemaps: TPageTilemaps read GetTilemaps write SetTilemaps;
    property Tilemaps[Index: Integer]: TPageTilemapInfo read GetTilemapInfo
      write SetTilemapInfo;
    property TilemapsPointer[Index: Integer]: PPageTilemapInfo read
      GetTilemapInfoPointer;

    property WRAM: Pointer read GetWRAMPointer;
    property WRAMSize: Integer read GetWRAMSize ;
    property SDLWindow: PSDL_Window read GetSDLWindow write SetSDLWindow;
    property SDLRenderer: PSDL_Renderer read GetSDLRenderer
      write SetSDLRenderer;
    property ExitGameLoop: Boolean read GetExitGameLoop write SetExitGameLoop;
    property RenderOneFrame: Boolean read GetRenderOneFrame
      write SetRenderOneFrame;

    property OnAfterWRAMInitialized: TNotifyEvent read FOnAfterWRAMInitialized
      write FOnAfterWRAMInitialized;
    property OnAfterVRAMInitialized: TNotifyEvent read FOnAfterVRAMInitialized
      write FOnAfterVRAMInitialized;
  end;

implementation

{ TPageMemoryWrapper }

function TPageMemoryWrapper.GetExitGameLoop: Boolean;
begin
  if FWRAMPointer <> nil then
    Result := TPageWRAMLayout(FWRAMPointer^).boolExitGameLoop
  else
    Result := False;
end;

function TPageMemoryWrapper.GetRenderEngineInfo: PPageRenderEngineInfo;
begin
  if FVRAMPointer <> nil then
    Result := @TPAGEVRAMLayout(FVRAMPointer^).RenderEngine
  else
    Result := nil;
end;

{function TPageMemoryWrapper.GetRenderEngineInfoPointer: Pointer;
begin
  Result := @TPAGEVRAMLayout(FVRAMPointer^).RenderEngine;
end;}

function TPageMemoryWrapper.GetRenderOneFrame: Boolean;
begin
  if FWRAMPointer <> nil then
    Result := TPageWRAMLayout(FWRAMPointer^).boolRenderOneFrame
  else
    Result := False;
end;

function TPageMemoryWrapper.GetSDLRenderer: PSDL_Renderer;
begin
  if FWRAMPointer <> nil then
    Result := TPageWRAMLayout(FWRAMPointer^).SDLRenderer
  else
    Result := nil;
end;

function TPageMemoryWrapper.GetSDLWindow: PSDL_Window;
begin
  if FWRAMPointer <> nil then
    Result := TPageWRAMLayout(FWRAMPointer^).SDLWindow
  else
    Result := nil;
end;

function TPageMemoryWrapper.GetTextureManagerInfo: TPageTextureManagerInfo;
begin
  if FVRAMPointer <> nil then
    Result := TPageVRAMLayout(FVRAMPointer^).TextureManagerInfo;
  { TODO: Return empty info if pointer is nil }
end;

function TPageMemoryWrapper.GetTextureManagerInfoPointer: Pointer;
begin
  Result := @TPageVRAMLayout(FVRAMPointer^).TextureManagerInfo;
end;

function TPageMemoryWrapper.GetTilemapInfo(Index: Integer): TPageTilemapInfo;
begin
  Result := TPAGEVRAMLayout(FVRAMPointer^).Tilemaps[Index];
end;

function TPageMemoryWrapper.GetTilemapInfoPointer(Index: Integer
  ): PPageTilemapInfo;
begin
  Result := @TPAGEVRAMLayout(FVRAMPointer^).Tilemaps[Index];
end;

{function TPageMemoryWrapper.GetTilemaps: TPageTilemaps;
begin
  if FVRAMPointer <> nil then
    Result := TPAGEVRAMLayout(FVRAMPointer^).Tilemaps
  else
    Result := PAGE_EMPTY_TILEMAPS;
end;}

function TPageMemoryWrapper.GetVRAMInitializationStatus: Boolean;
begin
  Result := False;
  if FVRAMPointer <> nil then
    Result := (TPageVRAMLayout(FVRAMPointer^).isInitializedMagicBytes =
      PAGE_VRAM_MAGIC_BYTES);
end;

function TPageMemoryWrapper.GetVRAMPointer: Pointer;
begin
  Result := FVRAMPointer;
end;

function TPageMemoryWrapper.GetVRAMSize: Integer;
begin
  Result := FVRAMSize;
end;

function TPageMemoryWrapper.GetWRAMInitializationStatus: Boolean;
begin
  Result := False;
  if FWRAMPointer <> nil then
    Result := (TPageWRAMLayout(FWRAMPointer^).isInitializedMagicBytes =
      PAGE_WRAM_MAGIC_BYTES);
end;

function TPageMemoryWrapper.GetWRAMPointer: Pointer;
begin
  Result := FWRAMPointer;
end;

function TPageMemoryWrapper.GetWRAMSize: Integer;
begin
  Result := FWRAMSize;
end;

procedure TPageMemoryWrapper.SetExitGameLoop(AValue: Boolean);
begin
  if FWRAMPointer <> nil then
    TPageWRAMLayout(FWRAMPointer^).boolExitGameLoop := AValue;
end;

{procedure TPageMemoryWrapper.SetRenderEngineInfo(AValue: TPageRenderEngineInfo);
begin
  if FVRAMPointer <> nil then
    TPAGEVRAMLayout(FVRAMPointer^).RenderEngine := AValue;
end;}

procedure TPageMemoryWrapper.SetRenderOneFrame(AValue: Boolean);
begin
  if FWRAMPointer <> nil then
    TPageWRAMLayout(FWRAMPointer^).boolRenderOneFrame := AValue;
end;

procedure TPageMemoryWrapper.SetSDLRenderer(AValue: PSDL_Renderer);
begin
  if FWRAMPointer <> nil then
    TPageWRAMLayout(FWRAMPointer^).SDLRenderer := AValue;
end;

procedure TPageMemoryWrapper.SetSDLWindow(AValue: PSDL_Window);
begin
  if FWRAMPointer <> nil then
    TPageWRAMLayout(FWRAMPointer^).SDLWindow := AValue;
end;

procedure TPageMemoryWrapper.SetTilemapInfo(Index: Integer;
  AValue: TPageTilemapInfo);
begin
  if FVRAMPointer <> nil then
    TPageVRAMLayout(FVRAMPointer^).Tilemaps[Index] := AValue;
end;


{procedure TPageMemoryWrapper.SetTilemaps(AValue: TPageTilemaps);
begin
  if FVRAMPointer <> nil then
    TPageVRAMLayout(FVRAMPointer^).Tilemaps := AValue;
end;}

constructor TPageMemoryWrapper.Create;
begin
  FWRAMMemMan := nil;
  FVRAMMemMan := nil;
  FMemoryManager := TPageDummyMemoryManager;
  FOnAfterWRAMInitialized := nil;
  FOnAfterWRAMInitialized := nil;
  Self._AddRef; // Prevent the object to be freed by accident
end;

function TPageMemoryWrapper.VRAMMemoryManagerInterface: IPageMemoryManager;
begin
  Result := nil;
  if Assigned(FVRAMMemMan) then
    Result := FVRAMMemMan as IPageMemoryManager;
end;

function TPageMemoryWrapper.WRAMMemoryManagerInterface: IPageMemoryManager;
begin
  Result := nil;
  if Assigned(FWRAMMemMan) then
    Result := FWRAMMemMan as IPageMemoryManager;
end;

function TPageMemoryWrapper.Bind(aWRAM: Pointer; aWRAMSize: Integer;
  aVRAM: Pointer; aVRAMSize: Integer): Boolean;
begin
  gEventQueue.CastDebugVariable(psMemoryWrapper, 'WRAM', dvPointer, aWRAM,
    SizeOf(aWRAM));
  gEventQueue.CastDebugVariable(psMemoryWrapper, 'WRAM Size', dvInteger,
    @FWRAMSize, SizeOf(FWRAMSize));
  gEventQueue.CastDebugVariable(psMemoryWrapper, 'VRAM', dvPointer, aVRAM,
    SizeOf(aWRAM));
  gEventQueue.CastDebugVariable(psMemoryWrapper, 'VRAM Size', dvInteger,
    @FVRAMSize, SizeOf(FVRAMSize));

  FWRAMPointer := aWRAM;
  FWRAMSize := aWRAMSize;
  if isWRAMInitialized then
  begin
    FWRAMMemMan := FMemoryManager.Create(FWRAMPointer+
      SizeOf(TPageWRAMLayout), FWRAMSize-SizeOf(TPageWRAMLayout));
    if Assigned(FOnAfterWRAMInitialized) then
      FOnAfterWRAMInitialized(Self);
  end;
  FVRAMPointer := aVRAM;
  FVRAMSize := aVRAMSize;
  if isVRAMInitialized then
  begin
    FVRAMMemMan := FMemoryManager.Create(FVRAMPointer+
      SizeOf(TPageVRAMLayout), FVRAMSize-SizeOf(TPageVRAMLayout));
    if Assigned(FOnAfterVRAMInitialized) then
      FOnAfterVRAMInitialized(Self);
  end;
end;

procedure TPageMemoryWrapper.InitializeWRAM;
begin
  with TPageWRAMLayout(FWRAMPointer^) do
  begin
    SDLWindow := nil;
    SDLRenderer := nil;
    boolExitGameLoop := False;
    boolRenderOneFrame := False;
    FWRAMMemMan := FMemoryManager.Create(FWRAMPointer+
      SizeOf(TPageWRAMLayout), FWRAMSize-SizeOf(TPageWRAMLayout));
    FWRAMMemMan.DoInitialize;
    isInitializedMagicBytes := PAGE_WRAM_MAGIC_BYTES;
  end;
  if Assigned(FOnAfterWRAMInitialized) then
    FOnAfterWRAMInitialized(Self);
end;

procedure TPageMemoryWrapper.InitializeVRAM;
begin
  with TPageVRAMLayout(FVRAMPointer^) do
  begin
    RenderEngine := PAGE_EMPTY_RENDERENGINEINFO;
    Tilemaps := PAGE_EMPTY_TILEMAPS;
    FVRAMMemMan := FMemoryManager.Create(FVRAMPointer+
      SizeOf(TPageVRAMLayout), FVRAMSize-SizeOf(TPageVRAMLayout));
    FVRAMMemMan.DoInitialize;
    isInitializedMagicBytes := PAGE_VRAM_MAGIC_BYTES;
  end;
  if Assigned(FOnAfterVRAMInitialized) then
    FOnAfterVRAMInitialized(Self);
end;

end.

