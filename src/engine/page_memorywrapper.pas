unit page_memorywrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PageAPI, SDL2, page_improvedmemorymanager;

type
  TPageWRAMLayout = packed record
    SDLWindow: PSDL_Window;
    SDLRenderer: PSDL_Renderer;
    boolExitGameLoop: Boolean;
    boolRenderOneFrame: Boolean;
  end;

  TPageVRAMLayout = packed record
    RenderEngine: TPageRenderEngineInfo;
    Tilemaps: TPageTilemaps;
    // Anything afterwards is memory managed by a memory manager
  end;

  { TPageMemoryWrapper }

  TPageMemoryWrapper = class
  private
    FVRAMPointer: Pointer;
    FWRAMPointer: Pointer;
    FVRAMSize: Integer;
    FWRAMSize: Integer;

    FWRAMMemMan, FVRAMMemMan: TPageImprovedMemoryManager;
    function GetExitGameLoop: Boolean;
    function GetRenderEngineInfo: TPageRenderEngineInfo;
    function GetRenderOneFrame: Boolean;
    function GetSDLRenderer: PSDL_Renderer;
    function GetSDLWindow: PSDL_Window;
    function GetTilemaps: TPageTilemaps;
    function GetVRAMPointer: Pointer;
    function GetVRAMSize: Integer;
    function GetWRAMPointer: Pointer;
    function GetWRAMSize: Integer;
    procedure SetExitGameLoop(AValue: Boolean);
    procedure SetRenderEngineInfo(AValue: TPageRenderEngineInfo);
    procedure SetRenderOneFrame(AValue: Boolean);
    procedure SetSDLRenderer(AValue: PSDL_Renderer);
    procedure SetSDLWindow(AValue: PSDL_Window);
    procedure SetTilemaps(AValue: TPageTilemaps);
    procedure SetVRAMPointer(AValue: Pointer);
    procedure SetVRAMSize(AValue: Integer);
    procedure SetWRAMPointer(AValue: Pointer);
    procedure SetWRAMSize(AValue: Integer);

    procedure InitializeVRAMMemoryManagerIfPossible;
    procedure InitializeWRAMMemoryManagerIfPossible;
  public
    constructor Create(WRAM: Pointer = nil; VRAM: Pointer = nil;
      WRAMSize: Integer = 0; VRAMSize: Integer = 0);

    property VRAM: Pointer read GetVRAMPointer write SetVRAMPointer;
    property VRAMSize: Integer read GetVRAMSize write SetVRAMSize;
    property RenderEngine: TPageRenderEngineInfo read GetRenderEngineInfo
      write SetRenderEngineInfo;
    property Tilemaps: TPageTilemaps read GetTilemaps write SetTilemaps;

    property WRAM: Pointer read GetWRAMPointer write SetWRAMPointer;
    property WRAMSize: Integer read GetWRAMSize write SetWRAMSize;
    property SDLWindow: PSDL_Window read GetSDLWindow write SetSDLWindow;
    property SDLRenderer: PSDL_Renderer read GetSDLRenderer
      write SetSDLRenderer;
    property ExitGameLoop: Boolean read GetExitGameLoop write SetExitGameLoop;
    property RenderOneFrame: Boolean read GetRenderOneFrame
      write SetRenderOneFrame;
  end;

implementation

{ TPageMemoryWrapper }

function TPageMemoryWrapper.GetExitGameLoop: Boolean;
begin
  if FWRAMPointer <> nil then
    Result := TPageWRAMLayout(FWRAMPointer^).boolExitGameLoop
  else
    Result := False;
end;

function TPageMemoryWrapper.GetRenderEngineInfo: TPageRenderEngineInfo;
begin
  if FVRAMPointer <> nil then
    Result := TPAGEVRAMLayout(FVRAMPointer^).RenderEngine
  else
    Result := PAGE_EMPTY_RENDERENGINEINFO;
end;

function TPageMemoryWrapper.GetRenderOneFrame: Boolean;
begin
  if FWRAMPointer <> nil then
    Result := TPageWRAMLayout(FWRAMPointer^).boolRenderOneFrame
  else
    Result := False;
end;

function TPageMemoryWrapper.GetSDLRenderer: PSDL_Renderer;
begin
  if FWRAMPointer <> nil then
    Result := TPageWRAMLayout(FWRAMPointer^).SDLRenderer
  else
    Result := nil;
end;

function TPageMemoryWrapper.GetSDLWindow: PSDL_Window;
begin
  if FWRAMPointer <> nil then
    Result := TPageWRAMLayout(FWRAMPointer^).SDLWindow
  else
    Result := nil;
end;

function TPageMemoryWrapper.GetTilemaps: TPageTilemaps;
begin
  if FVRAMPointer <> nil then
    Result := TPAGEVRAMLayout(FVRAMPointer^).Tilemaps
  else
    Result := PAGE_EMPTY_TILEMAPS;
end;

function TPageMemoryWrapper.GetVRAMPointer: Pointer;
begin
  Result := FVRAMPointer;
end;

function TPageMemoryWrapper.GetVRAMSize: Integer;
begin
  Result := FVRAMSize;
end;

function TPageMemoryWrapper.GetWRAMPointer: Pointer;
begin
  Result := FWRAMPointer;
end;

function TPageMemoryWrapper.GetWRAMSize: Integer;
begin
  Result := FWRAMSize;
end;

procedure TPageMemoryWrapper.SetExitGameLoop(AValue: Boolean);
begin
  if FWRAMPointer <> nil then
    TPageWRAMLayout(FWRAMPointer^).boolExitGameLoop := AValue;
end;

procedure TPageMemoryWrapper.SetRenderEngineInfo(AValue: TPageRenderEngineInfo);
begin
  if FVRAMPointer <> nil then
    TPAGEVRAMLayout(FVRAMPointer^).RenderEngine := AValue;
end;

procedure TPageMemoryWrapper.SetRenderOneFrame(AValue: Boolean);
begin
  if FWRAMPointer <> nil then
    TPageWRAMLayout(FWRAMPointer^).boolRenderOneFrame := AValue;
end;

procedure TPageMemoryWrapper.SetSDLRenderer(AValue: PSDL_Renderer);
begin
  if FWRAMPointer <> nil then
    TPageWRAMLayout(FWRAMPointer^).SDLRenderer := AValue;
end;

procedure TPageMemoryWrapper.SetSDLWindow(AValue: PSDL_Window);
begin
  if FWRAMPointer <> nil then
    TPageWRAMLayout(FWRAMPointer^).SDLWindow := AValue;
end;

procedure TPageMemoryWrapper.SetTilemaps(AValue: TPageTilemaps);
begin
  if FVRAMPointer <> nil then
    TPageVRAMLayout(FVRAMPointer^).Tilemaps := AValue;
end;

procedure TPageMemoryWrapper.SetVRAMPointer(AValue: Pointer);
begin
  FVRAMPointer := AValue;
  InitializeVRAMMemoryManagerIfPossible;
end;

procedure TPageMemoryWrapper.SetVRAMSize(AValue: Integer);
begin
  FVRAMSize := AValue;
  InitializeVRAMMemoryManagerIfPossible;
end;

procedure TPageMemoryWrapper.SetWRAMPointer(AValue: Pointer);
begin
  FWRAMPointer := AValue;
  InitializeWRAMMemoryManagerIfPossible;
end;

procedure TPageMemoryWrapper.SetWRAMSize(AValue: Integer);
begin
  FWRAMSize := AValue;
  InitializeWRAMMemoryManagerIfPossible;
end;

procedure TPageMemoryWrapper.InitializeVRAMMemoryManagerIfPossible;
begin
  if Assigned(FVRAMMemMan) then
    FVRAMMemMan.Free;
  if (FVRAMPointer <> nil) and (FVRAMSize > 0) then
    FVRAMMemMan := TPageImprovedMemoryManager.Create(FVRAMPointer+
      SizeOf(TPageVRAMLayout), FVRAMSize-SizeOf(TPageVRAMLayout));
end;

procedure TPageMemoryWrapper.InitializeWRAMMemoryManagerIfPossible;
begin
  if Assigned(FWRAMMemMan) then
    FWRAMMemMan.Free;
  if (FWRAMPointer <> nil) and (FWRAMSize > 0) then
    FWRAMMemMan := TPageImprovedMemoryManager.Create(FWRAMPointer+
      SizeOf(TPageWRAMLayout), FWRAMSize-SizeOf(TPageWRAMLayout));
end;


constructor TPageMemoryWrapper.Create(WRAM: Pointer; VRAM: Pointer;
  WRAMSize: Integer; VRAMSize: Integer);
begin
  FWRAMMemMan := nil;
  FVRAMMemMan := nil;
  FWRAMPointer := WRAM;
  FVRAMPointer := VRAM;
  FWRAMSize := WRAMSize;
  FVRAMSize := VRAMSize;
  InitializeWRAMMemoryManagerIfPossible;
  InitializeVRAMMemoryManagerIfPossible;
end;

end.

