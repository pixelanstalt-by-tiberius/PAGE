unit page_helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PAGEAPI, SDL2;

function SDLRendererInfoToPAGERenderSettings(SDLInfo:
  TSDL_RendererInfo): TPAGE_RenderSettings;
function PAGERenderSettingsToString(RenderSettings: TPAGE_RenderSettings
  ): String;

implementation

function SDLRendererInfoToPAGERenderSettings(SDLInfo: TSDL_RendererInfo
  ): TPAGE_RenderSettings;
begin
  Result.EnableVSync := (SDLInfo.flags and SDL_RENDERER_PRESENTVSYNC) =
    SDL_RENDERER_PRESENTVSYNC;
  Result.RenderAccelerated := (SDLInfo.flags and SDL_RENDERER_ACCELERATED) =
    SDL_RENDERER_ACCELERATED;
  Result.RendererNumber := -1;
  Result.RenderSizeWidth := 0;
  Result.RenderSizeHeight := 0;
  if (Result.RenderAccelerated) and
    ((SDLInfo.flags and SDL_RENDERER_SOFTWARE) = SDL_RENDERER_SOFTWARE) then
    Exception.Create('Mode clash between renderer ''Software'' and ' +
      '''Accelerated''');
end;

function PAGERenderSettingsToString(RenderSettings: TPAGE_RenderSettings
  ): String;
begin
  with RenderSettings do
    Result := Format('#: %d, Accelerated: %s, VSync: %s, WxH: %dx%d',
      [RendererNumber, BoolToStr(RenderAccelerated, true),
      BoolToStr(EnableVSync, true), RenderSizeWidth, RenderSizeHeight]);
end;

end.

