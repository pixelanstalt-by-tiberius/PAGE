library PAGE;

{$mode objfpc}{$H+}

uses
  Classes, SDL2, PageAPI;


function PAGE_Do_Initialize: Boolean;
begin
  Result := False;
end;

function PAGE_Do_Finalize: Boolean;
begin
  Result := False;
end;

function PAGE_Do_BindToApp(WRAM, VRAM, ROM: Pointer; WRAMSize, VRAMSize,
    ROMSize: Integer): Boolean;
begin
  Result := False;
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

exports
  PAGE_Do_Initialize, PAGE_Do_Finalize, PAGE_Do_BindToApp, PAGE_Do_GetRendererInfos;

end.

