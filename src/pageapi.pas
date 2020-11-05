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

  TPAGE_WRAMLayout = packed record
    SDLWindow: PSDL_Window;
    SDLRenderer: PSDL_Renderer;
    boolExitGameLoop: Boolean;
  end;

// Methods
type
  TPAGE_Initialize = function(RendererNum: Integer; Accelerated: Boolean;
    EnableVSYNC: Boolean; Fullscreen: Boolean; X, Y, WinWidth,
    WinHeight: Integer): Boolean;
  TPAGE_Finalize = function(): Boolean;
  TPAGE_BindToApp = function(WRAM, VRAM, ROM: Pointer; WRAMSize, VRAMSize,
    ROMSize: Integer): Boolean;
  TPAGE_GetRendererInfos = function(RendererInfos: PPAGE_RendererInfos):
    Boolean;
  TPAGE_EnterGameLoop = function(overrideDelta: Integer = -1): Boolean;


const
  PAGE_INITIALIZE_METHODNAME = 'PAGE_Do_Initialize';
  PAGE_FINALIZE_METHODNAME = 'PAGE_Do_Finalize';
  PAGE_BINDTOAPP_METHODNAME = 'PAGE_Do_BindToApp';
  PAGE_GETRENDERERINFOS_METHODNAME = 'PAGE_Do_GetRendererInfos';
  PAGE_ENTERGAMELOOP_METHODNAME = 'PAGE_Do_EnterGameLoop';

implementation

end.

