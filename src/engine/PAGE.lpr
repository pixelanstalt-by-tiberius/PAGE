library PAGE;

{$mode objfpc}{$H+}

uses
  Classes, SDL2, PageAPI;

var
  WRAM, VRAM, ROM: Pointer;
  WRAMSize, VRAMSize, ROMSize: Integer;


function PAGE_Do_Initialize(RendererNum: Integer; Accelerated: Boolean;
    EnableVSYNC: Boolean; Fullscreen: Boolean; X, Y,
    WinWidth, WinHeight: Integer): Boolean;
var
  WindowFlags, RendererFlags: UInt32;
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

function PAGE_Do_EnterGameLoop(overrideDelta: Integer = -1): Boolean;
var
  r, g, b: Byte;
  fwd: Boolean;
begin
  r := 0;
  g := 0;
  b := 0;
  fwd := true;
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

    if fwd then
    begin
      if r < 255 then
        Inc(r);
      if (r = 255) and (g < 255) then
        Inc(g);
      if (r = 255) and (g = 255) and (b < 255) then
        Inc(b);
      if (r = 255) and (g = 255) and (b = 255) then
        fwd := false;
    end
    else
    begin
      if r > 0 then
        Dec(r);
      if (r = 0) and (g > 0) then
        Dec(g);
      if (r = 0) and (g = 0) and (b > 0) then
        Dec(b);
      if (r = 0) and (g = 0) and (b = 0) then
        fwd := true;
    end;

    SDL_SetRenderDrawColor(TPAGE_WRAMLayout(WRAM^).SDLRenderer, r, g, b, 255);
    SDL_RenderClear(TPAGE_WRAMLayout(WRAM^).SDLRenderer);

    SDL_RenderPresent(TPAGE_WRAMLayout(WRAM^).SDLRenderer);
  end;
end;

exports
  PAGE_Do_Initialize, PAGE_Do_Finalize, PAGE_Do_BindToApp,
  PAGE_Do_GetRendererInfos, PAGE_Do_EnterGameLoop;

end.

