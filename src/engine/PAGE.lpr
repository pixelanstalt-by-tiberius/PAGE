library PAGE;

{$mode objfpc}{$H+}

uses
  {$ifdef UNIX} cthreads,{$endif}
  Classes, SDL2, PageAPI, SDL2_Image, PAGE_EventQueue, SysUtils;

var
  WRAM, VRAM, ROM: Pointer;
  WRAMSize, VRAMSize, ROMSize: Integer;

  boolShowSplashScreen: Boolean = True;

function PAGE_Do_Initialize(RendererNum: Integer; Accelerated: Boolean;
    EnableVSYNC: Boolean; Fullscreen: Boolean; X, Y,
    WinWidth, WinHeight: Integer): Boolean;
var
  WindowFlags, RendererFlags: UInt32;
  nEvent: TPAGE_Event;
begin
  Result := False;

  { TODO: Handle errors, warnings etc. }
  Result := SDL_Init(SDL_INIT_EVERYTHING) = 0;
  if Result then
  begin
    // Create Window
    if Fullscreen then
      WindowFlags := SDL_WINDOW_FULLSCREEN
    else
      WindowFlags := SDL_WINDOW_SHOWN;

    TPAGE_WRAMLayout(WRAM^).SDLWindow :=
      SDL_CreateWindow('PAGE demo', X, Y, WinWidth, WinHeight, WindowFlags);

    // Create Renderer
    RendererFlags := 0;
    if Accelerated then
      RendererFlags := SDL_RENDERER_ACCELERATED
    else
      RendererFlags := SDL_RENDERER_SOFTWARE;

    if EnableVSYNC then
      RendererFlags := RendererFlags or SDL_RENDERER_PRESENTVSYNC;

    TPAGE_WRAMLayout(WRAM^).SDLRenderer :=
      SDL_CreateRenderer(TPAGE_WRAMLayout(WRAM^).SDLWindow, RendererNum,
      RendererFlags);

    SDL_RenderClear(TPAGE_WRAMLayout(WRAM^).SDLRenderer);
    SDL_RenderPresent(TPAGE_WRAMLayout(WRAM^).SDLRenderer);
  end;
end;

function PAGE_Do_Finalize: Boolean;
begin
  Result := False;
end;

function PAGE_Do_BindToApp(aWRAM, aVRAM, aROM: Pointer; aWRAMSize, aVRAMSize,
    aROMSize: Integer): Boolean;
begin
  Result := False;

  WRAM := aWRAM;
  VRAM := aVRAM;
  ROM := aROM;
  WRAMSize := aWRAMSize;
  VRAMSize := aVRAMSize;
  ROMSize := aROMSize;

  { TODO: Check if sizes are okay and if RAM and ROM are accessible }

  Result := True;
end;

function PAGE_Do_GetRendererInfos(Infos: PPAGE_RendererInfos): Boolean;
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

function PAGE_Do_Splashscreen(overrideDelta: Double = -1): Boolean;
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

  while (boolShowSplashscreen) do
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
           boolShowSplashscreen := False;
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

function PAGE_Do_EnterGameLoop(overrideDelta: Double = -1): Boolean;
begin
  PAGE_Do_Splashscreen;
  gEventQueue.CastEventString(etNotification, psMain, psDebug,
    'Entering game loop');
  while not (TPAGE_WRAMLayout(WRAM^).boolExitGameLoop) do
  begin
    // Check input

    // Do stuff
      // -> load assets
      // -> calculate physics
      // -> render assets to vram

    // Render things
      // -> render vram

    // wait?
    gEventQueue.DoDispatchEvents;
  end;
  gEventQueue.CastEventString(etNotification, psMain, psDebug,
    'Exit game loop');
  gEventQueue.DoDispatchEvents;
end;

function PAGE_Do_AddEventQueueListener(aEventListener: TPAGE_EventQueueListener;
  ListenToSubSystems: TPAGE_SubSystems): Boolean;
begin
  gEventQueue.AddEventListener(aEventListener, ListenToSubSystems);
  Result := True;
  { TODO: Handle result }
end;

exports
  PAGE_Do_Initialize, PAGE_Do_Finalize, PAGE_Do_BindToApp,
  PAGE_Do_GetRendererInfos, PAGE_Do_EnterGameLoop, PAGE_Do_Splashscreen,
  PAGE_Do_AddEventQueueListener;

end.

