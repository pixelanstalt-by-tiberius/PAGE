unit page_encapsulate;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef unix}cthreads, {$endif} Classes, SysUtils, PAGE_EventQueue, PAGEApi,
  SDL2, SDL2_Image, page_helpers, page_texturemanager, page_memorywrapper,
  splash_res in '../../res/splash_res.pas', page_spritemanager,
  page_renderengine, page_BitmapFontManager, page_soundmanager;


{ TODO: Window-, Renderer- etc. -initailizations should be separated in
        multiple methods }

{ TODO: Creation, Binding and Initialization of objects must be done
        consistently for all objects (maybe introduce a bind-method to each
        class - this makes sure that this object is initialized when needed or
        bound?) }

type
  { TODO: Get SDL driver infos }

  { TPixelanstaltGameEngine }

  TPixelanstaltGameEngine = class
  private
    FROM: Pointer;
    FROMSize: Integer;

    FMemoryWrapper: TPageMemoryWrapper;

    FboolShowSplashScreen: Boolean;
    FTextureManager: TPageTextureManager;
    FSpriteManager: TPageSpriteManager;
    FRenderEngine: TPageRenderEngine;
    FBitmapFontManager: TPageBMFManager;
    FSoundManager: TPageSoundManager;

    FDispatchedEvents: array[0..MAX_EVENTS] of TPAGE_Event;
    FNumDispatchedEvents: Integer;
    FEventDispatchCriticalSection: TRTLCriticalSection;

    procedure ProcessDispatchedEvents;
    procedure MemoryWrapperAfterWRAMInitialized(Sender: TObject);
    procedure MemoryWrapperAfterVRAMInitialized(Sender: TObject);

    procedure StartPerformanceTest;
    procedure WriteText(TilemapNumber, X, Y: Integer; Text: String);
  public
    property ROM: Pointer read FROM;
    property ROMSize: Integer read FROMSize;

    constructor Create;
    destructor Destroy; override;

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
        'break': FMemoryWrapper.ExitGameLoop := true;
        'show_splashscreen': Splashscreen;
        'fullscreen': SDL_SetWindowFullscreen(FMemoryWrapper.SDLWindow,
                        SDL_WINDOW_FULLSCREEN);
      end;
    end;
  end;

  FNumDispatchedEvents := 0;

  LeaveCriticalSection(FEventDispatchCriticalSection);
end;

procedure TPixelanstaltGameEngine.MemoryWrapperAfterWRAMInitialized(
  Sender: TObject);
begin
  FSoundManager := TPageSoundManager.Create;
  FSoundManager.Initialize;
end;

procedure TPixelanstaltGameEngine.MemoryWrapperAfterVRAMInitialized(
  Sender: TObject);
var
  intLoop: Integer;
begin
  FTextureManager := TPageTextureManager.Create(FMemoryWrapper);
  FRenderEngine := TPageRenderEngine.Create(FMemoryWrapper, @FTextureManager);
  FSpriteManager := TPageSpriteManager.Create;
  FSpriteManager.AssignTextureManager(@FTextureManager);
  FBitmapFontManager := TPageBMFManager.Create(FMemoryWrapper);
  FBitmapFontManager.AssignTextureManager(@FTextureManager);

  for intLoop := 0 to FRenderEngine.TilemapCount-1 do
    FSpriteManager.AssignStream(intLoop, FRenderEngine.SpriteStreams[intLoop]);
end;

procedure TPixelanstaltGameEngine.StartPerformanceTest;
  {$include page_performancetest.inc}

procedure TPixelanstaltGameEngine.WriteText(TilemapNumber, X, Y: Integer;
  Text: String);
var
  tr: TPageTileRecord;
  intLoop: Integer;
begin
  tr := EMPTY_TILE;

  for intLoop := 0 to Length(Text)-1 do
  begin
    tr.TextureID := FBitmapFontManager.TextureID(Text[intLoop+1]);
    FRenderEngine.Tilemaps[TilemapNumber].Map[X+intLoop,Y] := tr;
  end;
end;

procedure EventQueueListenerMaster(
  aDispatchedEvent: TPAGE_Event);
begin
  EnterCriticalSection(gPage.FEventDispatchCriticalSection);
  if gPage.FNumDispatchedEvents = MAX_EVENTS then
    raise Exception.Create('Event buffer overflow');

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
    raise Exception.Create('Tried to initialize PAGE singleton twice');
  FboolShowSplashScreen := True;
  FNumDispatchedEvents := 0;
  InitCriticalSection(FEventDispatchCriticalSection);
  FMemoryWrapper := TPageMemoryWrapper.Create;
  FMemoryWrapper.OnAfterVRAMInitialized := @MemoryWrapperAfterVRAMInitialized;
  FMemoryWrapper.OnAfterWRAMInitialized := @MemoryWrapperAfterWRAMInitialized;
end;

destructor TPixelanstaltGameEngine.Destroy;
begin
  DoneCriticalSection(FEventDispatchCriticalSection);
  FMemoryWrapper.Free;
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
    gEventQueue.CastEventString(etNotification, psMain, psDebug, esError,
      PChar('Error initializing SDL: ' + SDL_GetError));
  end
  else
  begin
    FMemoryWrapper.InitializeWRAM;
    FMemoryWrapper.InitializeVRAM;
    // Create Window
    if WindowSettings.Fullscreen then
      WindowFlags := SDL_WINDOW_FULLSCREEN
    else
      WindowFlags := SDL_WINDOW_SHOWN;

    FMemoryWrapper.SDLWindow := SDL_CreateWindow(WindowSettings.WindowTitle,
      WindowSettings.WindowX, WindowSettings.WindowY,
      WindowSettings.WindowSizeWidth, WindowSettings.WindowSizeHeight,
      WindowFlags);
    if FMemoryWrapper.SDLWindow = nil then
    begin
      gEventQueue.CastEventString(etNotification, psMain, psDebug, esError,
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

    gEventQueue.CastEventString(etNotification, psMain, psDebug, esInfo,
      PChar('Creating renderer: ' +
      PAGERenderSettingsToString(RenderSettings)));

    FMemoryWrapper.SDLRenderer := SDL_CreateRenderer(FMemoryWrapper.SDLWindow,
      RenderSettings.RendererNumber, RendererFlags);
    if FMemoryWrapper.SDLRenderer = nil then
    begin
      gEventQueue.CastEventString(etNotification, psMain, psDebug, esError,
        PChar('Error creating renderer: ' + SDL_GetError));
      Result := False;
      gEventQueue.DoDispatchEvents;
      Exit;
    end;

    SDL_GetRendererInfo(FMemoryWrapper.SDLRenderer, @SDL_RendererInfo);
    gEventQueue.CastEventString(etNotification, psMain, psDebug, esInfo,
      PChar('Renderer created: ' +
      PAGERenderSettingsToString(SDLRendererInfoToPAGERenderSettings(
      SDL_RendererInfo))));

    if SDL_RenderSetLogicalSize(FMemoryWrapper.SDLRenderer,
      RenderSettings.RenderSizeWidth, RenderSettings.RenderSizeHeight) <> 0 then
    begin
      gEventQueue.CastEventString(etNotification, psMain, psDebug, esError,
        PChar('Failed to set logical render size: ' + SDL_GetError));
    end;

    FMemoryWrapper.RenderEngineInfo^.RenderingDimension.X := RenderSettings.
      RenderSizeWidth;
    FMemoryWrapper.RenderEngineInfo^.RenderingDimension.Y := RenderSettings.
      RenderSizeHeight;

    SDL_RenderClear(FMemoryWrapper.SDLRenderer);
    SDL_RenderPresent(FMemoryWrapper.SDLRenderer);
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

  FMemoryWrapper.Bind(aWRAM, aWRAMSize, aVRAM, aVRAMSize);

  FROM := aROM;
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
  if FboolShowSplashScreen then
    Splashscreen;

  if FROM = nil then
  begin
    // Fallback if not ROM is loaded
    gEventQueue.CastEventString(etNotification, psMain, psDebug, esWarning,
      PChar('Entering game loop WITHOUT ROM. Starting Performance Test. OverrideDeltaValue: ' +
      FloatToStr(overrideDelta)));
    StartPerformanceTest;
  end
  else
    gEventQueue.CastEventString(etNotification, psMain, psDebug, esInfo,
      PChar('Entering game loop. OverrideDeltaValue: ' +
      FloatToStr(overrideDelta)));

    while not (FMemoryWrapper.ExitGameLoop) do
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

      { TODO: maybe query in while condition and reset within loop if
              neccessary? }
      if (FMemoryWrapper.RenderOneFrame) then
        Break;
    end;

  gEventQueue.CastEventString(etNotification, psMain, psDebug, esInfo,
    'Exit game loop');
  gEventQueue.DoDispatchEvents;
end;

function TPixelanstaltGameEngine.Splashscreen(overrideDelta: Double): Boolean;
var
  p, a, g, e, fez, page: Integer;
  alpha: Double;
  RectP, RectA, RectG, RectE, RectFEZ, RectPAGE: TSDL_Rect;
  perfCountFreq, perfCountLast, perfCountCurrent: UInt64;
  delta: Double;
  intState: Integer = 0;
  intTick, intStateChange: UInt64;
  SndEvent: TPAGE_Event;
  SndRes: TPageSoundResource;
begin
  p := FTextureManager.AddTextureFromInlineResource(@ppng, SizeOf(ppng),
    rtImagePNG);
  a := FTextureManager.AddTextureFromInlineResource(@apng, SizeOf(apng),
    rtImagePNG);
  g := FTextureManager.AddTextureFromInlineResource(@gpng, SizeOf(gpng),
    rtImagePNG);
  e := FTextureManager.AddTextureFromInlineResource(@epng, SizeOf(epng),
    rtImagePNG);
  fez := FTextureManager.AddTextureFromInlineResource(@fezpng, SizeOf(fezpng),
    rtImagePNG);
  page := FTextureManager.AddTextureFromInlineResource(@pagepng,
    SizeOf(pagepng), rtImagePNG);

  RectP.x := 50;
  RectP.y := 60;
  RectP.h := FTextureManager.GetTextureDimensions(p).y;
  RectP.w := FTextureManager.GetTextureDimensions(p).x;

  RectFEZ.x := RectP.x + RectP.w - FTextureManager.GetTextureDimensions(fez).x +
    10;
  RectFEZ.y := RectP.y - FTextureManager.GetTextureDimensions(fez).y;
  RectFEZ.h := FTextureManager.GetTextureDimensions(fez).y;
  RectFEZ.w := FTextureManager.GetTextureDimensions(fez).x;

  RectA.x := RectP.x + RectP.w + 10;
  RectA.y := RectP.y;
  RectA.h := FTextureManager.GetTextureDimensions(a).y;
  RectA.w := FTextureManager.GetTextureDimensions(a).x;

  RectG.x := RectA.x + RectA.w + 10;
  RectG.y := RectA.y;
  RectG.h := FTextureManager.GetTextureDimensions(g).y;
  RectG.w := FTextureManager.GetTextureDimensions(g).x;

  RectE.x := RectG.x + RectG.w + 10;
  RectE.y := RectG.y;
  RectE.h := FTextureManager.GetTextureDimensions(e).y;
  RectE.w := FTextureManager.GetTextureDimensions(e).x;

  { TODO: Calculate offset }
  RectPAGE.x := 10;
  RectPAGE.y := RectA.y + RectA.h + 15;
  RectPAGE.h := FTextureManager.GetTextureDimensions(page).y;
  RectPAGE.w := FTextureManager.GetTextureDimensions(page).x;

  SndRes.ResourceType := rtInline;
  SndRes.Memory := @loadwav;
  SndRes.Size := Length(loadwav);
  //SndRes.ResourceType := rtFile;
  //SndRes.Filename := PChar('/home/tiberius/temp/pixelanstalt_4.wav');

  SndEvent.EventType := etRequest;
  SndEvent.EventMessage := emPlaySound;
  SndEvent.EventSenderSubsystem := psMain;
  SndEvent.EventReceiverSubsystem := psAudio;
  SndEvent.SoundResource := SndRes;

  gEventQueue.CastEvent(SndEvent);

  //FSoundManager.PlaySound(SndRes);
  //SDL_Delay(4000);

  { TODO: Make global }
  perfCountFreq := SDL_GetPerformanceFrequency;
  perfCountLast := SDL_GetPerformanceCounter;

  SDL_SetRenderDrawColor(FMemoryWrapper.SDLRenderer, 0, 0, 0, 0);
  SDL_RenderClear(FMemoryWrapper.SDLRenderer);

  alpha := 0;
  intTick := 0;
  intStateChange := 0;

  while (FboolShowSplashscreen) do
  begin
    perfCountCurrent := SDL_GetPerformanceCounter;
    delta := (perfCountCurrent - perfCountLast)/(perfCountFreq/10);
    SDL_RenderClear(FMemoryWrapper.SDLRenderer);

    FSoundManager.Update;

    case intState of
      0: begin
           { TODO: Debug only - strip! }
           if intStateChange = 0 then
             intStateChange := SDL_GetTicks;

           // Transition from black to logo only
           SDL_SetTextureAlphaMod(FTextureManager.TexturePointers[p],
             Round(alpha));
           SDL_SetTextureAlphaMod(FTextureManager.TexturePointers[a],
             Round(alpha));
           SDL_SetTextureAlphaMod(FTextureManager.TexturePointers[g],
             Round(alpha));
           SDL_SetTextureAlphaMod(FTextureManager.TexturePointers[e],
             Round(alpha));
           SDL_SetTextureAlphaMod(FTextureManager.TexturePointers[fez],
             Round(alpha));


           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[p], nil, @RectP);
           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[fez], nil, @RectFEZ);
           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[a], nil, @RectA);
           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[g], nil, @RectG);
           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[e], nil, @RectE);

           alpha := alpha+(delta*25);
           if alpha > 255 then
           begin
             alpha := 0;
             intState := 1;
             //FSoundManager.PlaySound(SndRes);

             SDL_SetTextureAlphaMod(FTextureManager.TexturePointers[p], 255);
             SDL_SetTextureAlphaMod(FTextureManager.TexturePointers[a], 255);
             SDL_SetTextureAlphaMod(FTextureManager.TexturePointers[g], 255);
             SDL_SetTextureAlphaMod(FTextureManager.TexturePointers[e], 255);
             SDL_SetTextureAlphaMod(FTextureManager.TexturePointers[fez], 255);

             { TODO: Debug only - strip! }
             //pchMessage :=
             gEventQueue.CastEventString(etNotification, psMain, psDebug, esDebug,
               PChar('State 0->1 (' + IntToStr(SDL_GetTicks-intStateChange) +
               ' ms)'));
             gEventQueue.CastEventString(etNotification, psMain, psDebug, esDebug,
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

           SDL_SetTextureAlphaMod(FTextureManager.TexturePointers[page],
             Round(alpha));

           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[p], nil, @RectP);
           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[fez], nil, @RectFEZ);
           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[a], nil, @RectA);
           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[g], nil, @RectG);
           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[e], nil, @RectE);
           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[page], nil, @RectPAGE);

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
             gEventQueue.CastEventString(etNotification, psMain, psDebug, esDebug,
               PChar('State 1->2 (' + IntToStr(SDL_GetTicks-intStateChange) +
               ' ms)'));
             gEventQueue.CastEventString(etNotification, psMain, psDebug, esDebug,
               PChar('Current delta ' + FloatToStr(delta) + '*1/10ms)'));
             intStateChange := 0;
           end;
         end;
      2: begin
           { TODO: Debug only - strip! }
           if intStateChange = 0 then
             intStateChange := SDL_GetTicks;
           // Transition from logo with text to black
           SDL_SetTextureAlphaMod(FTextureManager.TexturePointers[p],
             Round(alpha));
           SDL_SetTextureAlphaMod(FTextureManager.TexturePointers[a],
             Round(alpha));
           SDL_SetTextureAlphaMod(FTextureManager.TexturePointers[g],
             Round(alpha));
           SDL_SetTextureAlphaMod(FTextureManager.TexturePointers[e],
             Round(alpha));
           //SDL_SetTextureAlphaMod(FTextureManager.TexturePointers[fez], 255);
           SDL_SetTextureAlphaMod(FTextureManager.TexturePointers[page],
             Round(alpha));


           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[p], nil, @RectP);
           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[fez], nil, @RectFEZ);
           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[a], nil, @RectA);
           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[g], nil, @RectG);
           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[e], nil, @RectE);
           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[page], nil, @RectPAGE);

           alpha := alpha-(delta*25);
           if alpha <= 0 then
           begin
             alpha := 255;
             intState := 3;
             { TODO: Debug only - strip! }
             gEventQueue.CastEventString(etNotification, psMain, psDebug, esDebug,
               PChar('State 2->3 (' + IntToStr(SDL_GetTicks-intStateChange) +
               ' ms)'));
             gEventQueue.CastEventString(etNotification, psMain, psDebug, esDebug,
               PChar('Current delta ' + FloatToStr(delta) + '*1/10ms)'));
             intStateChange := 0;
           end;
         end;
      3: begin
           if intStateChange = 0 then
             intStateChange := SDL_GetTicks;

           SDL_SetTextureAlphaMod(FTextureManager.TexturePointers[fez],
             Round(alpha));

           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[p], nil, @RectP);
           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[fez], nil, @RectFEZ);
           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[a], nil, @RectA);
           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[g], nil, @RectG);
           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[e], nil, @RectE);
           SDL_RenderCopy(FMemoryWrapper.SDLRenderer, FTextureManager.
             TexturePointers[page], nil, @RectPAGE);

            alpha := alpha-(delta*25);
           if alpha <= 0 then
           begin
             alpha := 255;
             intState := 4;
             { TODO: Debug only - strip! }
             gEventQueue.CastEventString(etNotification, psMain, psDebug, esDebug,
               PChar('State 3->4 (' + IntToStr(SDL_GetTicks-intStateChange) +
               ' ms)'));
             gEventQueue.CastEventString(etNotification, psMain, psDebug, esDebug,
               PChar('Current delta ' + FloatToStr(delta) + '*1/10ms)'));
             intStateChange := 0;
           end;

         end;
      4: begin
           { TODO: Debug only - strip! }
           if intStateChange = 0 then
             intStateChange := SDL_GetTicks;
           SDL_Delay(1000);
           FboolShowSplashscreen := False;
           { TODO: Debug only - strip! }
           gEventQueue.CastEventString(etNotification, psMain, psDebug, esDebug,
             PChar('State 4 done (' + IntToStr(SDL_GetTicks-intStateChange) +
             ' ms)'));
           intStateChange := 0;
         end;
    end;

    perfCountLast := perfCountCurrent;
    SDL_RenderPresent(FMemoryWrapper.SDLRenderer);
    gEventQueue.DoDispatchEvents;
  end;

  FTextureManager.FreeAllTextures;

  gEventQueue.CastEventString(etNotification, psMain, psDebug, esInfo,
    'Splashscreen done');
end;

initialization
  gPAGE := TPixelanstaltGameEngine.Create;

finalization
  gPAGE.Free;

end.

