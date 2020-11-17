unit PAGEAPI;

{$mode objfpc}{$H+}

interface

uses
  SDL2;

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

  TPAGE_EventType = (etNotification, etRequest);
  TPAGE_SubSystem = (psMain, psDebug, psAudio, psInput, psVideo, psHaptics,
    psROM);
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

implementation

end.

