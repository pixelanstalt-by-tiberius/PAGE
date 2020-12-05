unit page_encapsulate;

{$mode objfpc}{$H+}

interface

uses
  cthreads, Classes, SysUtils, PAGE_EventQueue, PAGEApi, SDL2, SDL2_Image,
  page_helpers, page_texturemanager, cmem;


type
  { TODO: Get SDL driver infos }

  { TPixelanstaltGameEngine }

  TPixelanstaltGameEngine = class
  private
    FROM: Pointer;
    FROMSize: Integer;
    FVRAM: Pointer;
    FVRAMSize: Integer;
    FWRAM: Pointer;
    FWRAMSize: Integer;

    FboolShowSplashScreen: Boolean;

    FTextureManager: TPageTextureManager;

    FDispatchedEvents: array[0..MAX_EVENTS] of TPAGE_Event;
    FNumDispatchedEvents: Integer;
    FEventDispatchCriticalSection: TRTLCriticalSection;

    procedure ProcessDispatchedEvents;
  public
    property WRAM: Pointer read FWRAM;
    property WRAMSize: Integer read FWRAMSize;
    property VRAM: Pointer read FVRAM;
    property VRAMSize: Integer read FVRAMSize;
    property ROM: Pointer read FROM;
    property ROMSize: Integer read FROMSize;

    constructor Create;
    destructor Destroy;

    function Initialize(RenderSettings: TPAGE_RenderSettings;
      WindowSettings: TPAGE_WindowSettings): Boolean;
    function Finalize: Boolean;
    function BindToApp(aWRAM, aVRAM, aROM: Pointer; aWRAMSize, aVRAMSize,
      aROMSize: Integer): Boolean;
    function GetRendererInfos(Infos: PPAGE_RendererInfos): Boolean;
    function EnterGameLoop(overrideDelta: Double = -1): Boolean;
    function Splashscreen(overrideDelta: Double = -1): Boolean;
  end;

var
  gPAGE: TPixelanstaltGameEngine;

procedure EventQueueListenerMaster(aDispatchedEvent: TPAGE_Event);

implementation

{ TPixelanstaltGameEngine }

procedure TPixelanstaltGameEngine.ProcessDispatchedEvents;
var
  intEventLoop: Integer;
begin
  { TODO: Try with rolling buffer }
  if FNumDispatchedEvents <= 0 then
    Exit;

  if TryEnterCriticalSection(FEventDispatchCriticalSection) = 0 then
    Exit;

  for intEventLoop := 0 to FNumDispatchedEvents-1 do
  begin
    if (FDispatchedEvents[intEventLoop].EventType = etRequest) and
      (FDispatchedEvents[intEventLoop].EventMessage = emString) then
    begin
      case LowerCase(FDispatchedEvents[intEventLoop].EventMessageString) of
        'break': TPAGE_WRAMLayout(WRAM^).boolExitGameLoop := true;
        'show_splashscreen': Splashscreen;
      end;
    end;
  end;

  FNumDispatchedEvents := 0;

  LeaveCriticalSection(FEventDispatchCriticalSection);
end;

procedure EventQueueListenerMaster(
  aDispatchedEvent: TPAGE_Event);
begin
  EnterCriticalSection(gPage.FEventDispatchCriticalSection);
  if gPage.FNumDispatchedEvents = MAX_EVENTS then
    Exception.Create('Event buffer overflow');

  Inc(gPage.FNumDispatchedEvents);

  gPage.FDispatchedEvents[gPage.FNumDispatchedEvents-1] := aDispatchedEvent;
  if gPage.FDispatchedEvents[gPage.FNumDispatchedEvents-1].EventMessage = emString then
    gPage.FDispatchedEvents[gPage.FNumDispatchedEvents-1].EventMessageString := StrNew(
    aDispatchedEvent.EventMessageString);
  LeaveCriticalSection(gPage.FEventDispatchCriticalSection);
end;

constructor TPixelanstaltGameEngine.Create;
begin
  if Assigned(gPAGE) then
    Exception.Create('Tried to initialize PAGE singleton twice');
  FboolShowSplashScreen := True;
  FNumDispatchedEvents := 0;
  InitCriticalSection(FEventDispatchCriticalSection);
  FTextureManager := TPageTextureManager.Create(nil);
end;

destructor TPixelanstaltGameEngine.Destroy;
begin
  DoneCriticalSection(FEventDispatchCriticalSection);
end;

function TPixelanstaltGameEngine.Initialize(RenderSettings:
  TPAGE_RenderSettings; WindowSettings: TPAGE_WindowSettings): Boolean;
var
  WindowFlags, RendererFlags: UInt32;
  SDL_RendererInfo: TSDL_RendererInfo;
begin
  Result := False;

  { TODO: Handle errors, warnings etc. }
  Result := SDL_Init(SDL_INIT_EVERYTHING) = 0;
  if not Result then
  begin
    gEventQueue.CastEventString(etNotification, psMain, psDebug,
      PChar('Error initializing SDL: ' + SDL_GetError));
  end
  else
  begin
    // Create Window
    if WindowSettings.Fullscreen then
      WindowFlags := SDL_WINDOW_FULLSCREEN
    else
      WindowFlags := SDL_WINDOW_SHOWN;

    TPAGE_WRAMLayout(WRAM^).SDLWindow :=
      SDL_CreateWindow(WindowSettings.WindowTitle, WindowSettings.WindowX,
        WindowSettings.WindowY, WindowSettings.WindowSizeWidth,
        WindowSettings.WindowSizeHeight, WindowFlags);
    if TPAGE_WRAMLayout(WRAM^).SDLWindow = nil then
    begin
      gEventQueue.CastEventString(etNotification, psMain, psDebug,
        PChar('Error creating window: ' + SDL_GetError));
      Result := False;
      gEventQueue.DoDispatchEvents;
      Exit;
    end;

    // Create Renderer
    RendererFlags := 0;
    if RenderSettings.RenderAccelerated then
      RendererFlags := SDL_RENDERER_ACCELERATED
    else
      RendererFlags := SDL_RENDERER_SOFTWARE;

    if RenderSettings.EnableVSync then
      RendererFlags := RendererFlags or SDL_RENDERER_PRESENTVSYNC;

    gEventQueue.CastEventString(etNotification, psMain, psDebug,
      PChar('Creating renderer: ' +
      PAGERenderSettingsToString(RenderSettings)));

    TPAGE_WRAMLayout(WRAM^).SDLRenderer :=
      SDL_CreateRenderer(TPAGE_WRAMLayout(WRAM^).SDLWindow,
      RenderSettings.RendererNumber, RendererFlags);
    if TPAGE_WRAMLayout(WRAM^).SDLRenderer = nil then
    begin
      gEventQueue.CastEventString(etNotification, psMain, psDebug,
        PChar('Error creating renderer: ' + SDL_GetError));
      Result := False;
      gEventQueue.DoDispatchEvents;
      Exit;
    end;

    // Dispatch renderer to subsystems
    FTextureManager.SetRenderer(TPAGE_WRAMLayout(WRAM^).SDLRenderer);


    SDL_GetRendererInfo(TPAGE_WRAMLayout(WRAM^).SDLRenderer, @SDL_RendererInfo);
    gEventQueue.CastEventString(etNotification, psMain, psDebug,
      PChar('Renderer created: ' +
      PAGERenderSettingsToString(SDLRendererInfoToPAGERenderSettings(
      SDL_RendererInfo))));

    if SDL_RenderSetLogicalSize(TPAGE_WRAMLayout(WRAM^).SDLRenderer,
      RenderSettings.RenderSizeWidth, RenderSettings.RenderSizeHeight) <> 0 then
    begin
      gEventQueue.CastEventString(etNotification, psMain, psDebug,
        PChar('Failed to set logical render size: ' + SDL_GetError));
    end;
    SDL_RenderClear(TPAGE_WRAMLayout(WRAM^).SDLRenderer);
    SDL_RenderPresent(TPAGE_WRAMLayout(WRAM^).SDLRenderer);
  end;
  gEventQueue.DoDispatchEvents;
end;

function TPixelanstaltGameEngine.Finalize: Boolean;
begin
  Result := False;
end;

function TPixelanstaltGameEngine.BindToApp(aWRAM, aVRAM, aROM: Pointer;
  aWRAMSize, aVRAMSize, aROMSize: Integer): Boolean;
begin
  Result := False;

  FWRAM := aWRAM;
  FVRAM := aVRAM;
  FROM := aROM;
  FWRAMSize := aWRAMSize;
  FVRAMSize := aVRAMSize;
  FROMSize := aROMSize;

  { TODO: Check if sizes are okay and if RAM and ROM are accessible }

  gEventQueue.AddEventListener(@EventQueueListenerMaster, [psMain]);

  Result := True;
end;

function TPixelanstaltGameEngine.GetRendererInfos(Infos: PPAGE_RendererInfos
  ): Boolean;
var
  SDLRendererInfo: TSDL_RendererInfo;
  intLoop: Integer;
begin
  SetLength(Infos^, SDL_GetNumRenderDrivers);
  for intLoop := 0 to SDL_GetNumRenderDrivers-1 do
  begin
    SDL_GetRenderDriverInfo(intLoop, @SDLRendererInfo);
    with Infos^[intLoop] do
    begin
      Name := SDLRendererInfo.name;
      isSoftware := (SDLRendererInfo.flags and SDL_RENDERER_SOFTWARE) =
        SDL_RENDERER_SOFTWARE;
      isAccelerated := (SDLRendererInfo.flags and SDL_RENDERER_ACCELERATED) =
        SDL_RENDERER_ACCELERATED;
      isVSyncPresent := (SDLRendererInfo.flags and SDL_RENDERER_PRESENTVSYNC) =
        SDL_RENDERER_PRESENTVSYNC;
    end;
  end;
end;

function TPixelanstaltGameEngine.EnterGameLoop(overrideDelta: Double): Boolean;
begin
  { if FboolShowSplashScreen then
    Splashscreen; }
  gEventQueue.CastEventString(etNotification, psMain, psDebug,
    PChar('Entering game loop. OverrideDeltaValue: ' +
    FloatToStr(overrideDelta)));

  while not (TPAGE_WRAMLayout(WRAM^).boolExitGameLoop) do
  begin
    // Check input

    ProcessDispatchedEvents;
    // Do stuff
      // -> load assets
      // -> calculate physics
      // -> render assets to vram

    // Render things
      // -> render vram

    // wait?
    gEventQueue.DoDispatchEvents;
    if (TPAGE_WRAMLayout(WRAM^).boolRenderOneFrame) then
      Break;
  end;

  gEventQueue.CastEventString(etNotification, psMain, psDebug,
    'Exit game loop');
  gEventQueue.DoDispatchEvents;
end;

function TPixelanstaltGameEngine.Splashscreen(overrideDelta: Double): Boolean;
var
  textureLogo1, textureLogo2: PSDL_Texture;
  alpha: Double;
  perfCountFreq, perfCountLast, perfCountCurrent: UInt64;
  delta: Double;
  intState: Integer = 0;
  intTick, intStateChange: UInt64;
  pchMessage: PChar;
begin
  { TODO: Load from resource (assetfile or integrated in so/dll }
  { TODO: No hardcoded filenames! }
  textureLogo1 := IMG_LoadTexture(TPAGE_WRAMLayout(WRAM^).SDLRenderer,
    '../../res/splash_1.png');
  SDL_SetTextureBlendMode(textureLogo1, SDL_BLENDMODE_BLEND);
  textureLogo2 := IMG_LoadTexture(TPAGE_WRAMLayout(WRAM^).SDLRenderer,
    '../../res/splash_2.png');
  SDL_SetTextureBlendMode(textureLogo2, SDL_BLENDMODE_BLEND);

  { TODO: Make global }
  perfCountFreq := SDL_GetPerformanceFrequency;
  perfCountLast := SDL_GetPerformanceCounter;

  SDL_SetRenderDrawColor(TPAGE_WRAMLayout(WRAM^).SDLRenderer, 0, 0, 0, 0);
  SDL_RenderClear(TPAGE_WRAMLayout(WRAM^).SDLRenderer);

  alpha := 0;
  intTick := 0;
  intStateChange := 0;

  while (FboolShowSplashscreen) do
  begin
    perfCountCurrent := SDL_GetPerformanceCounter;
    delta := (perfCountCurrent - perfCountLast)/(perfCountFreq/10);
    SDL_RenderClear(TPAGE_WRAMLayout(WRAM^).SDLRenderer);

    case intState of
      0: begin
           { TODO: Debug only - strip! }
           if intStateChange = 0 then
             intStateChange := SDL_GetTicks;

           // Transition from black to logo only
           SDL_SetTextureAlphaMod(textureLogo1, Round(alpha));
           SDL_RenderCopy(TPAGE_WRAMLayout(WRAM^).SDLRenderer, textureLogo1,
             nil, nil);
           alpha := alpha+(delta*25);
           if alpha > 255 then
           begin
             alpha := 0;
             intState := 1;

             { TODO: Debug only - strip! }
             //pchMessage :=
             gEventQueue.CastEventString(etNotification, psMain, psDebug,
               PChar('State 0->1 (' + IntToStr(SDL_GetTicks-intStateChange) + ' ms)'));
             gEventQueue.CastEventString(etNotification, psMain, psDebug,
               PChar('Current delta ' + FloatToStr(delta) + '*1/10ms)'));
             intStateChange := 0;
           end;
         end;
      1: begin

           { TODO: Debug only - strip! }
           if intStateChange = 0 then
           begin
             intStateChange := SDL_GetTicks;
           end;
           // Transition form logo only to logo with text
           SDL_SetTextureAlphaMod(textureLogo1, 255);
           SDL_SetTextureAlphaMod(textureLogo2, Round(alpha));
           SDL_RenderCopy(TPAGE_WRAMLayout(WRAM^).SDLRenderer, textureLogo1,
             nil, nil);
           SDL_RenderCopy(TPAGE_WRAMLayout(WRAM^).SDLRenderer, textureLogo2,
             nil, nil);

           if (alpha < 255) then
             alpha := alpha+(delta*25);

           if (alpha > 255) then
           begin
             alpha := 255;
             intTick := SDL_GetTicks;
           end;
           if (intTick <> 0) and (SDL_GetTicks - intTick >= 2000) then
           begin
             intState := 2;
             { TODO: Debug only - strip! }
             gEventQueue.CastEventString(etNotification, psMain, psDebug,
               PChar('State 1->2 (' + IntToStr(SDL_GetTicks-intStateChange) + ' ms)'));
             gEventQueue.CastEventString(etNotification, psMain, psDebug,
               PChar('Current delta ' + FloatToStr(delta) + '*1/10ms)'));
             intStateChange := 0;
           end;
         end;
      2: begin
           { TODO: Debug only - strip! }
           if intStateChange = 0 then
             intStateChange := SDL_GetTicks;
           // Transition from logo with text to black
           SDL_SetTextureAlphaMod(textureLogo2, Round(alpha));
           SDL_RenderCopy(TPAGE_WRAMLayout(WRAM^).SDLRenderer, textureLogo2,
             nil, nil);
           alpha := alpha-(delta*25);
           if alpha <= 0 then
           begin
             alpha := 0;
             intState := 3;
             { TODO: Debug only - strip! }
             gEventQueue.CastEventString(etNotification, psMain, psDebug,
               PChar('State 2->3 (' + IntToStr(SDL_GetTicks-intStateChange) + ' ms)'));
             gEventQueue.CastEventString(etNotification, psMain, psDebug,
               PChar('Current delta ' + FloatToStr(delta) + '*1/10ms)'));
             intStateChange := 0;
           end;
         end;
      3: begin
           { TODO: Debug only - strip! }
           if intStateChange = 0 then
             intStateChange := SDL_GetTicks;
           SDL_Delay(1000);
           FboolShowSplashscreen := False;
           { TODO: Debug only - strip! }
           gEventQueue.CastEventString(etNotification, psMain, psDebug,
             PChar('State 3 done (' + IntToStr(SDL_GetTicks-intStateChange) + ' ms)'));
           intStateChange := 0;
         end;
    end;

    perfCountLast := perfCountCurrent;
    SDL_RenderPresent(TPAGE_WRAMLayout(WRAM^).SDLRenderer);
    gEventQueue.DoDispatchEvents;
  end;

  SDL_DestroyTexture(textureLogo1);
  SDL_DestroyTexture(textureLogo2);
  gEventQueue.CastEventString(etNotification, psMain, psDebug,
    'Splashscreen done');
end;

initialization
  gPAGE := TPixelanstaltGameEngine.Create;

finalization
  gPAGE.Free;

end.

