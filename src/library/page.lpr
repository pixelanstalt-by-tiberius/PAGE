library page;

{$mode objfpc}{$H+}

uses
  {$ifdef UNIX} cthreads,{$endif}
  Classes, SDL2, PageAPI, SDL2_Image, PAGE_EventQueue, SysUtils, page_helpers,
  page_encapsulate, page_texturemanager, page_memorymanager,
  page_improvedmemorymanager, page_tilemanager, page_renderengine,
  page_memorywrapper, page_spritemanager, page_dummymemorymanager,
  page_BitmapFontManager;

{ var
  gPAGE: TPixelanstaltGameEngine;}

function PAGE_Do_Initialize(RenderSettings: TPAGE_RenderSettings;
  WindowSettings: TPAGE_WindowSettings): Boolean;
begin
  Result := gPAGE.Initialize(RenderSettings, WindowSettings);
end;

function PAGE_Do_Finalize: Boolean;
begin
  Result := gPAGE.Finalize;
end;

function PAGE_Do_BindToApp(aWRAM, aVRAM, aROM: Pointer; aWRAMSize, aVRAMSize,
    aROMSize: Integer): Boolean;
begin
  Result := gPAGE.BindToApp(aWRAM, aVRAM, aROM, aWRAMSize, aVRAMSize, aROMSize);
end;

function PAGE_Do_GetRendererInfos(Infos: PPAGE_RendererInfos): Boolean;
begin
  Result := gPAGE.GetRendererInfos(Infos);
end;

function PAGE_Do_Splashscreen(overrideDelta: Double = -1): Boolean;
begin
  Result := gPAGE.Splashscreen(overrideDelta);
end;

function PAGE_Do_EnterGameLoop(overrideDelta: Double = -1): Boolean;
begin
  Result := gPAGE.EnterGameLoop(overrideDelta);
end;

function PAGE_Do_AddEventQueueListenerProcedure(aEventListener:
    TPAGE_EventQueueListenerProcedure;
  ListenToSubSystems: TPAGE_SubSystems): Boolean; overload;
begin
  gEventQueue.AddEventListener(aEventListener, ListenToSubSystems);
  Result := True;
  { TODO: Handle result }
end;

function PAGE_Do_AddEventQueueListenerMethod(aEventListener: TPAGE_EventQueueListenerMethod;
  ListenToSubSystems: TPAGE_SubSystems): Boolean; overload;
begin
  gEventQueue.AddEventListener(aEventListener, ListenToSubSystems);
  Result := True;
  { TODO: Handle result }
end;

procedure PAGE_Do_CastEvent(aEvent: TPAGE_Event; aString: PChar = '');
begin
  gEventQueue.CastEvent(aEvent, aString);
end;

exports
  PAGE_Do_Initialize, PAGE_Do_Finalize, PAGE_Do_BindToApp,
  PAGE_Do_GetRendererInfos, PAGE_Do_EnterGameLoop, PAGE_Do_Splashscreen,
  PAGE_Do_AddEventQueueListenerProcedure,
  PAGE_Do_AddEventQueueListenerMethod, PAGE_Do_CastEvent;


end.

