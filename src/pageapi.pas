unit PAGEAPI;

{$mode objfpc}{$H+}

interface

uses
  SDL2;

// Interfaces
const
  SPageMemoryManager = '{E42B8EC7-85F8-4713-993E-646897E2BDFA}';
type
  IPageMemoryManager = interface
    [SPageMemoryManager]
    function DoInitialize: Boolean;
    function FreeMemory(Reinitialize: Boolean = False): Boolean;

    function PageMMGetMem(Size:ptruint): Pointer;
    function PageMMFreeMem(p:pointer): ptruint;
    function PageMMFreeMemSize(p:pointer;Size:ptruint): ptruint;
    function PageMMAllocMem(Size:ptruint): Pointer;
    function PageMMReAllocMem(var p:pointer;Size:ptruint): Pointer;
    function PageMMMemSize(p:pointer): ptruint;
    function PageMMGetHeapStatus: THeapStatus;
    function PageMMGetFPCHeapStatus: TFPCHeapStatus;

    property isInitialized: Boolean;
    property Memory: Pointer;
    property AddressableMemory: Pointer;
  end;

// Structures
type
  TPAGE_RendererInfo = record
    Name: PChar;
    isSoftware: Boolean;
    isAccelerated: Boolean;
    isVSyncPresent: Boolean;
  end;

  TPAGE_RendererInfos = array of TPAGE_RendererInfo;
  PPAGE_RendererInfos = ^TPAGE_RendererInfos;

  TPAGE_RenderSettings = record
    RendererNumber: Integer;
    RenderAccelerated: Boolean;
    EnableVSync: Boolean;
    RenderSizeWidth: Word;
    RenderSizeHeight: Word;
  end;

  TPAGE_WindowSettings = record
    Fullscreen: Boolean;
    WindowSizeWidth: Word;
    WindowSizeHeight: Word;
    WindowX: Word;
    WindowY: Word;
    WindowTitle: PChar;
  end;

  TPAGE_WRAMLayout = packed record
    SDLWindow: PSDL_Window;
    SDLRenderer: PSDL_Renderer;
    boolExitGameLoop: Boolean;
    boolRenderOneFrame: Boolean;
  end;

  { TODO: Maybe height is not neccessary to save here? }
  TPageTilemapInfo = packed record
    Tilemap: Pointer;
    Width, Height: Integer;
  end;
  PPageTileMapInfo = ^TPageTileMapInfo;

  TPageVRAMLayout = packed record
    Tilemap1: TPageTilemapInfo;
    Tilemap2: TPageTilemapInfo;
    Tilemap3: TPageTilemapInfo;
    Tilemap4: TPageTilemapInfo;
    Tilemap5: TPageTilemapInfo;
    Tilemap6: TPageTilemapInfo;
    // Anything afterwards is memory managed by a memory manager
  end;

  TPageTextureID = Integer;
  TPageTileFlags = set of (tfFlipH, tfFlipY, tfPriorityHigh, tfPriorityLow);

  { TODO: Maye packed record for memory purposes }
  TPageTileRecord = record
    TextureID: TPageTextureID;
    Flags: TPageTileFlags;
  end;

  TPAGE_EventType = (etNotification, etRequest);
  TPAGE_SubSystem = (psMain, psDebug, psAudio, psInput, psVideo, psHaptics,
    psROM, psTextureManager);
  TPAGE_SubSystems = set of TPAGE_SubSystem;
  TPAGE_EventMessage = (emEmpty, emString);

  TPAGE_Event = record
    EventType: TPAGE_EventType;
    EventTick: UInt64;
    EventSenderSubsystem: TPAGE_SubSystem;
    EventReceiverSubsystem: TPAGE_SubSystem;
    EventMessage: TPAGE_EventMessage;
    EventMessageString: PChar;
  end;

  TPAGE_EventQueueListener = procedure(aDispatchedEvent: TPAGE_Event);
  PPAGE_EventQueueListener = ^TPAGE_EventQueueListener;

// Methods
type
  TPAGE_Initialize = function(RenderSettings: TPAGE_RenderSettings;
    WindowSettings: TPAGE_WindowSettings): Boolean;
  TPAGE_Finalize = function(): Boolean;
  TPAGE_BindToApp = function(WRAM, VRAM, ROM: Pointer; WRAMSize, VRAMSize,
    ROMSize: Integer): Boolean;
  TPAGE_GetRendererInfos = function(RendererInfos: PPAGE_RendererInfos):
    Boolean;
  TPAGE_EnterGameLoop = function(overrideDelta: Double = -1): Boolean;
  TPAGE_Splashscreen = function(overrideDelta: Double = -1): Boolean;
  TPAGE_AddEventQueueListener = function(aEventListener: TPAGE_EventQueueListener;
    ListenToSubSystems: TPAGE_SubSystems): Boolean;
  TPAGE_CastEvent = procedure(aEvent: TPAGE_Event; aString: PChar = '');


const
  PAGE_METHOD_NUM = 8;
  PAGE_METHOD_NAMES: array[0..PAGE_METHOD_NUM-1] of String = ('PAGE_Do_Initialize',
    'PAGE_Do_Finalize', 'PAGE_Do_BindToApp', 'PAGE_Do_GetRendererInfos',
    'PAGE_Do_EnterGameLoop', 'PAGE_Do_Splashscreen',
    'PAGE_Do_AddEventQueueListener', 'PAGE_Do_CastEvent');

  PAGE_MM_MAGIC_BYTES: Word = 12345; { TODO: Maybe change }

implementation

end.

