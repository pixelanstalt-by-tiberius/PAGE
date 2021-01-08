unit PAGEAPI;

{$mode objfpc}{$H+}

interface

uses
  SDL2;

// Interfaces
const
  SPageMemoryWrapper = '{E24636B0-465A-4835-BFAD-60D699D43235}';
  SPageMemoryManager = '{E42B8EC7-85F8-4713-993E-646897E2BDFA}';

// Structures
type
  TPageStreamPersistenceInfo = record
    Stream: Pointer;
    EntriesOrSize: Integer;
  end;
  PPageStreamPersistenceInfo = ^TPageStreamPersistenceInfo;

  TPageCoordinate2D = record
    X, Y: Word;
  end;

  PPageCoordinate2D = ^TPageCoordinate2D;

  TPAGE_RendererInfo = record
    Name: PChar;
    isSoftware: Boolean;
    isAccelerated: Boolean;
    isVSyncPresent: Boolean;
  end;

  TPageTextureManagerInfo = record
    CurrentTextureInfoCount: Integer;
    TextureInfoPersistence: TPageStreamPersistenceInfo;
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

  {  }

  { TODO: Maybe height is not neccessary to save here? }
  TPageTilemapInfo = packed record
    Tilemap: Pointer;
    Width, Height: Integer;
  end;
  PPageTileMapInfo = ^TPageTileMapInfo;

  TPageTilemaps = packed record
    Tilemap1: TPageTilemapInfo;
    Tilemap2: TPageTilemapInfo;
    Tilemap3: TPageTilemapInfo;
    Tilemap4: TPageTilemapInfo;
    Tilemap5: TPageTilemapInfo;
    Tilemap6: TPageTilemapInfo;
  end;

  TPageRenderEngineInfo = packed record
    TilemapCount: Byte;
    RenderingDimension: TPageCoordinate2D; // = Display Dimension
    CanvasDimension: TPageCoordinate2D;
    TileDimension: TPageCoordinate2D;
    SpriteDimension: TPageCoordinate2D;
  end;
  PPageRenderEngineInfo = ^TPageRenderEngineInfo;

  {  }

  TPageTextureID = Integer;
  TPageTileFlags = set of (tfFlipH, tfFlipV, tfPriorityHigh);//, tfPriorityLow);

  { TODO: Maye packed record for memory purposes }
  TPageTileRecord = record
    TextureID: TPageTextureID;
    Flags: TPageTileFlags;
  end;



  TPageSpriteFlags = set of (sfFlipH, sfFlipV, sfEnableAlpha);

  TPageSprite = record
    TextureID: TPageTextureID;
    Flags: TPageSpriteFlags;
    X, Y: Word;
    Alpha: Byte;
  end;

  TPAGE_EventType = (etNotification, etRequest);
  TPAGE_SubSystem = (psMain, psDebug, psAudio, psInput, psVideo, psHaptics,
    psROM, psTextureManager);
  TPAGE_SubSystems = set of TPAGE_SubSystem;
  TPAGE_EventMessage = (emEmpty, emDebugInfo, emString);

  TPageEventSeverity = (esNotSet, esDebug, esInfo, esWarning, esError,
    esException);

  TPageDebugInfoType = (diString, diVariable);

  TPageEventDebugInfo = record
    InfoType: TPageDebugInfoType;
  end;

  TPageDebugVariableType = (dvPointer, dvByte, dvWord, dvDWord, dvInteger,
    dvInt64, dvUInt64, dvPChar);

  TPageDebugVariable = record
    Name: PChar;
    Address: Pointer;
    Size: ptruint;
    VariableType: TPageDebugVariableType;
  end;

  TPAGE_Event = record
    EventType: TPAGE_EventType;
    EventTick: UInt64;
    EventSenderSubsystem: TPAGE_SubSystem;
    EventReceiverSubsystem: TPAGE_SubSystem;
    case EventMessage: TPAGE_EventMessage of
      emDebugInfo: (
        case DebugInfoType: TPageDebugInfoType of
          diString: (
            DebugString: PChar; );
          diVariable: (
            DebugVariable: TPageDebugVariable; )
        );
      emString: (EventSeverity: TPageEventSeverity; EventMessageString: PChar);
      emEmpty: ();
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
  PAGE_WRAM_MAGIC_BYTES: array[0..2] of Char = ('P', 'G', 'R');
  PAGE_VRAM_MAGIC_BYTES: array[0..2] of Char = ('P', 'G', 'V');

  PAGE_MAX_TILEMAPS = 6;

  PAGE_EMPTY_RENDERENGINEINFO: TPageRenderEngineInfo = (TilemapCount: 0;
    RenderingDimension : (X: 0; Y: 0); CanvasDimension: (X: 0; Y: 0);
    TileDimension : (X: 0; Y: 0); SpriteDimension: (X: 0; Y: 0));

  EMPTY_SPRITE: TPageSprite = (TextureID: -1; Flags: []; X: 0; Y: 0; Alpha: 255);
  SPRITE_COUNT = 64; // Sprites per Layer
  PAGE_MAX_SPRITES = SPRITE_COUNT*PAGE_MAX_TILEMAPS;

  PAGE_COORDINATE2D_NULL: TPageCoordinate2D = (X: 0; Y: 0);

  PAGE_EMPTY_TILEMAPINFO: TPageTilemapInfo = (Tilemap: nil; Width: 0;
    Height: 0);
  PAGE_EMPTY_TILEMAPS: TPageTilemaps = (Tilemap1: (Tilemap: nil; Width: 0;
    Height: 0); Tilemap2: (Tilemap: nil; Width: 0; Height: 0);
    Tilemap3: (Tilemap: nil; Width: 0; Height: 0);
    Tilemap4: (Tilemap: nil; Width: 0; Height: 0);
    Tilemap5: (Tilemap: nil; Width: 0; Height: 0);
    Tilemap6: (Tilemap: nil; Width: 0; Height: 0));

  operator = (spritea : TPageSprite; spriteb : TPageSprite) b : boolean;
  operator = (tilerecorda: TPageTileRecord; tilerecordb: TPageTileRecord) b: boolean;

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

  IPageMemoryWrapper = interface
    [SPageMemoryWrapper]

    function GetSDLRenderer: PSDL_Renderer;
    function GetRenderEngineInfo: TPageRenderEngineInfo;
    function GetRenderEngineInfoPointer: Pointer;
    function GetTextureManagerInfo: TPageTextureManagerInfo;
    function GetTextureManagerInfoPointer: Pointer;

    function VRAMMemoryManagerInterface: IPageMemoryManager;
    function WRAMMemoryManagerInterface: IPageMemoryManager;

    property SDLWindow: PSDL_Window;
    property SDLRenderer: PSDL_Renderer read GetSDLRenderer;
    property RenderEngineInfo: TPageRenderEngineInfo read GetRenderEngineInfo;
    property RenderEngineInfoPointer: Pointer read GetRenderEngineInfoPointer;

    property TextureManagerInfo: TPageTextureManagerInfo
      read GetTextureManagerInfo;
    property TextureManagerInfoPointer: Pointer read GetTextureManagerInfoPointer;
  end;

implementation

operator=(spritea: TPageSprite; spriteb: TPageSprite)b: boolean;
begin
  Result := (spritea.TextureID = spriteb.TextureID) and
    (spritea.flags = spriteb.flags) and
    (spritea.X = spriteb.X) and
    (spritea.Y = spriteb.Y);
end;

operator=(tilerecorda: TPageTileRecord; tilerecordb: TPageTileRecord)b: boolean;
begin
  Result := (tilerecorda.TextureID = tilerecordb.TextureID) and
    (tilerecorda.Flags = tilerecordb.Flags);
end;

end.

