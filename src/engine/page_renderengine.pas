unit page_renderengine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, page_texturemanager, page_tilemanager, PageAPI, SDL2;

type

  { TPageRenderEngine }

  { TODO: Add transparency/fading effects to whole layers }

  TPageRenderEngine = class
  private
    function GetCanvasHeight: Integer;
    function GetCanvasWidth: Integer;
    function GetSpriteHeight: Word;
    function GetSpriteStream(Index: Integer): Pointer;
    function GetSpriteWidth: Word;
    function GetTileHeight: Word;
    function GetTileMap(Index: Integer): TPageTileMap;
    function GetTilemapCount: Integer;
    function GetTilemapOffset(Index: Integer): TPageCoordinate2D;
    function GetTileWidth: Word;
    procedure SetCanvasHeight(AValue: Integer);
    procedure SetCanvasWidth(AValue: Integer);
    procedure SetSpriteHeight(AValue: Word);
    procedure SetSpriteStream(Index: Integer; AValue: Pointer);
    procedure SetSpriteWidth(AValue: Word);
    procedure SetTileHeight(AValue: Word);
    procedure SetTilemapCount(AValue: Integer);
    procedure SetTilemapOffset(Index: Integer; AValue: TPageCoordinate2D);
    procedure SetTileWidth(AValue: Word);
  protected
    FTilemaps: array[0..PAGE_MAX_TILEMAPS-1] of PPageTileMap;
    FTilemapOffsets: array[0..PAGE_MAX_TILEMAPS-1] of TPageCoordinate2D;
    FSpriteStreams: array[0..PAGE_MAX_TILEMAPS-1] of Pointer;
    FRenderedBackgrounds: array[0..PAGE_MAX_TILEMAPS-1] of Pointer;
    FTextureManager: PPageTextureManager;
    FMemoryWrapper: IPageMemoryWrapper;

    // procedure DoRenderPerTile;
    procedure DoRenderPerLayer;

    procedure InitializeRenderedBackgroundsTextures;

    procedure DoDrawTileTexture(TextureID: TPageTextureID; X, Y: Word; FlipH,
      FlipV: Boolean);
    procedure DoRenderTilemapToTexture(Tilemap: PPageTilemap;
      Texture: PSDL_Texture);
    procedure DoRenderTilemap(Tilemap: PPageTilemap; OffsetX: Integer = 0;
      OffsetY: Integer = 0);
    procedure DoRenderSprite(Sprite: TPageSprite);
  public
    constructor Create(aMemoryWrapper: IPageMemoryWrapper; aTextureManager:
      PPageTextureManager);

    procedure DoRender;
    procedure SetTilemap(Number: Word; TileMap: PPageTileMap);
    procedure SetSpriteStreamPointer(Number: Word;
      SpriteStreamPointer: Pointer);
    procedure BuildTilemaps;
    procedure BuildSpriteStreams;

    property SpriteStreams[Index: Integer]: Pointer read GetSpriteStream
      write SetSpriteStream;
    property Tilemaps[Index: Integer]: TPageTileMap read GetTileMap;
    property TilemapCount: Integer read GetTilemapCount write SetTilemapCount;
    property TilemapOffsets[Index: Integer]: TPageCoordinate2D
      read GetTilemapOffset write SetTilemapOffset;
    property SpriteHeight: Word read GetSpriteHeight write SetSpriteHeight;
    property SpriteWidth: Word read GetSpriteWidth write SetSpriteWidth;
    property TileHeight: Word read GetTileHeight write SetTileHeight;
    property TileWidth: Word read GetTileWidth write SetTileWidth;
    property CanvasHeight: Integer read GetCanvasHeight write SetCanvasHeight;
    property CanvasWidth: Integer read GetCanvasWidth write SetCanvasWidth;
  end;

implementation

{ TPageRenderEngine }

function TPageRenderEngine.GetCanvasHeight: Integer;
begin
  Result := FMemoryWrapper.RenderEngineInfo^.CanvasDimension.Y;
end;

function TPageRenderEngine.GetCanvasWidth: Integer;
begin
  Result := FMemoryWrapper.RenderEngineInfo^.CanvasDimension.X;
end;

function TPageRenderEngine.GetSpriteHeight: Word;
begin
  Result := FMemoryWrapper.RenderEngineInfo^.SpriteDimension.Y;
end;

function TPageRenderEngine.GetSpriteStream(Index: Integer): Pointer;
begin
  { TODO: Range check }
  Result := FSpriteStreams[Index];
end;

function TPageRenderEngine.GetSpriteWidth: Word;
begin
  Result := FMemoryWrapper.RenderEngineInfo^.SpriteDimension.X;
end;

function TPageRenderEngine.GetTileHeight: Word;
begin
  Result := FMemoryWrapper.RenderEngineInfo^.TileDimension.Y;
end;

function TPageRenderEngine.GetTileMap(Index: Integer): TPageTileMap;
begin
  { TODO: Range check }
  Result := FTilemaps[Index]^;
end;

function TPageRenderEngine.GetTilemapCount: Integer;
begin
  Result := FMemoryWrapper.RenderEngineInfo^.TilemapCount;
end;

function TPageRenderEngine.GetTilemapOffset(Index: Integer): TPageCoordinate2D;
begin
  { TODO: Range check }
  Result := FTilemapOffsets[Index];
end;

function TPageRenderEngine.GetTileWidth: Word;
begin
  Result := FMemoryWrapper.RenderEngineInfo^.TileDimension.X;
end;

procedure TPageRenderEngine.SetCanvasHeight(AValue: Integer);
begin
  FMemoryWrapper.RenderEngineInfo^.CanvasDimension.Y := AValue;
end;

procedure TPageRenderEngine.SetCanvasWidth(AValue: Integer);
begin
  FMemoryWrapper.RenderEngineInfo^.CanvasDimension.X := AValue;
end;

procedure TPageRenderEngine.SetSpriteHeight(AValue: Word);
begin
  FMemoryWrapper.RenderEngineInfo^.SpriteDimension.Y := AValue;
end;

procedure TPageRenderEngine.SetSpriteStream(Index: Integer; AValue: Pointer);
begin
  { TODO: Range check }
  { TODO: Free memory if slot was used before }
  FSpriteStreams[Index] := AValue;
end;

procedure TPageRenderEngine.SetSpriteWidth(AValue: Word);
begin
  FMemoryWrapper.RenderEngineInfo^.SpriteDimension.X := AValue;
end;


procedure TPageRenderEngine.SetTileHeight(AValue: Word);
begin
  FMemoryWrapper.RenderEngineInfo^.TileDimension.Y := AValue;
end;

procedure TPageRenderEngine.SetTilemapCount(AValue: Integer);
begin
  if (AValue < 0) then
    FMemoryWrapper.RenderEngineInfo^.TilemapCount := 0
  else
    if (AValue <= PAGE_MAX_TILEMAPS) then
      FMemoryWrapper.RenderEngineInfo^.TilemapCount := AValue
    else
      FMemoryWrapper.RenderEngineInfo^.TilemapCount := PAGE_MAX_TILEMAPS;
end;

procedure TPageRenderEngine.SetTilemapOffset(Index: Integer;
  AValue: TPageCoordinate2D);
begin
  { TODO: Range check }
  FTilemapOffsets[Index] := AValue;
end;

procedure TPageRenderEngine.SetTileWidth(AValue: Word);
begin
  FMemoryWrapper.RenderEngineInfo^.TileDimension.X := AValue;
end;

procedure TPageRenderEngine.DoRenderPerLayer;
var
  intTilemapLoop, intOffset: Integer;
  SrcRect: TSDL_Rect;
begin
  SrcRect.w := FMemoryWrapper.RenderEngineInfo^.RenderingDimension.X;
  SrcRect.h := FMemoryWrapper.RenderEngineInfo^.RenderingDimension.Y;

  SDL_SetRenderDrawBlendMode(FMemoryWrapper.SDLRenderer, SDL_BLENDMODE_BLEND);
  for intTilemapLoop := 0 to TilemapCount-1 do
  begin
    if not FTilemaps[intTilemapLoop]^.isValid then
    begin
      DoRenderTilemapToTexture(FTilemaps[intTilemapLoop],
        FRenderedBackgrounds[intTilemapLoop]);
      FTilemaps[intTilemapLoop]^.Validate;
      SDL_SetRenderDrawBlendMode(FMemoryWrapper.SDLRenderer, SDL_BLENDMODE_BLEND);
    end;

    SrcRect.x := FTilemapOffsets[intTilemapLoop].X;
    SrcRect.y := FTilemapOffsets[intTilemapLoop].Y;

    //DoRenderTilemap(FTilemaps[intTilemapLoop],
    //  FTilemapOffsets[intTilemapLoop].X, FTilemapOffsets[intTilemapLoop].Y);

    //if intTilemapLoop = 0 then
    //SDL_SetRenderDrawBlendMode(FMemoryWrapper.SDLRenderer, SDL_BLENDMODE_BLEND);
    SDL_RenderCopyEx(FMemoryWrapper.SDLRenderer,
      FRenderedBackgrounds[intTilemapLoop], @SrcRect, nil, 0, nil, 0);

    intOffset := 0;
    while (intOffset < SizeOf(TPageSprite)*(SPRITE_COUNT-1)) and
       (TPageSprite((FSpriteStreams[intTilemapLoop]+intOffset)^) <>
        EMPTY_SPRITE) do
    begin
      DoRenderSprite(TPageSprite((FSpriteStreams[intTilemapLoop]+intOffset)^));
      Inc(intOffset, SizeOf(TPageSprite));
    end;
  end;
end;

procedure TPageRenderEngine.InitializeRenderedBackgroundsTextures;
var
  intLoop: Integer;
begin
  for intLoop := 0 to PAGE_MAX_TILEMAPS-1 do
  begin
    FRenderedBackgrounds[intLoop] := SDL_CreateTexture(FMemoryWrapper.
      SDLRenderer, 0, SDL_TEXTUREACCESS_TARGET,
      FMemoryWrapper.RenderEngineInfo^.CanvasDimension.X,
      FMemoryWrapper.RenderEngineInfo^.CanvasDimension.Y);
    SDL_SetTextureBlendMode(FRenderedBackgrounds[intLoop], SDL_BLENDMODE_BLEND);
  end;
end;

procedure TPageRenderEngine.DoDrawTileTexture(TextureID: TPageTextureID; X,
  Y: Word; FlipH, FlipV: Boolean);
var
  FlipFlags: Integer = 0;
  DestRect: TSDL_Rect;
begin
  if FlipH then
    FlipFlags := FlipFlags or SDL_FLIP_HORIZONTAL;
  if FlipV then
    FlipFlags := FlipFlags or SDL_FLIP_VERTICAL;

  DestRect.h := TileHeight;
  DestRect.w := TileHeight;
  DestRect.x := X;
  DestRect.y := Y;

  SDL_RenderCopyEx(FMemoryWrapper.SDLRenderer, FTextureManager^.
    Textures[TextureID].TexturePointer, nil, @DestRect, 0, nil, FlipFlags);
end;

procedure TPageRenderEngine.DoRenderTilemapToTexture(Tilemap: PPageTilemap;
  Texture: PSDL_Texture);
var
  OldRenderTarget: PSDL_Texture;
begin
  OldRenderTarget := SDL_GetRenderTarget(FMemoryWrapper.SDLRenderer);
  if SDL_SetRenderTarget(FMemoryWrapper.SDLRenderer, Texture) <> 0 then
    raise Exception.Create('Failed to change rendering target');
  //SDL_RenderClear(FMemoryWrapper.SDLRenderer);
  SDL_SetRenderDrawBlendMode(FMemoryWrapper.SDLRenderer, SDL_BLENDMODE_NONE);
  SDL_SetRenderDrawColor(FMemoryWrapper.SDLRenderer, 0, 0, 0, 0);
  SDL_RenderFillRect(FMemoryWrapper.SDLRenderer, nil);
  DoRenderTilemap(Tilemap);
  //SDL_RenderPresent(FMemoryWrapper.SDLRenderer);
  SDL_SetRenderTarget(FMemoryWrapper.SDLRenderer, OldRenderTarget);
end;

procedure TPageRenderEngine.DoRenderTilemap(Tilemap: PPageTilemap;
  OffsetX: Integer; OffsetY: Integer);
var
  intX, intY: Integer;
  {$ifdef DEBUG_TILE_BORDER}
    InnerBorderRect: TSDL_Rect;
  {$endif}
begin
  {$ifdef DEBUG_TILE_BORDER}
    InnerBorderRect.h := TileHeight;
    InnerBorderRect.w := TileWidth;
  {$endif}
  SDL_SetRenderDrawBlendMode(FMemoryWrapper.SDLRenderer, SDL_BLENDMODE_BLEND);
  for intX := 0 to Tilemap^.Width-1 do
  begin
    for intY := 0 to Tilemap^.Height-1 do
    begin
      if Tilemap^.Map[intX, intY].TextureID < 0 then
      begin
        {$ifdef DEBUG_TILE_BORDER}
          SDL_SetRenderDrawColor(FMemoryWrapper.SDLRenderer, 0, 255, 0, 127);
          InnerBorderRect.x := TileWidth*intX+OffsetX;
          InnerBorderRect.y := TileHeight*intY+OffsetY;
          SDL_RenderDrawRect(FMemoryWrapper.SDLRenderer, @InnerBorderRect);
        {$endif}
        Continue;
      end;
      DoDrawTileTexture(Tilemap^.Map[intX, intY].TextureID,
        TileWidth*intX+OffsetX, TileHeight*intY+OffsetY,
        tfFlipH in Tilemap^.Map[intX, intY].Flags, tfFlipV in
        Tilemap^.Map[intX, intY].Flags);

      {$ifdef DEBUG_TILE_BORDER}
        SDL_SetRenderDrawColor(FMemoryWrapper.SDLRenderer, 0, 255, 0, 127);
        InnerBorderRect.x := TileWidth*intX+OffsetX;
        InnerBorderRect.y := TileHeight*intY+OffsetY;
        SDL_RenderDrawRect(FMemoryWrapper.SDLRenderer, @InnerBorderRect);
      {$endif}
    end;
  end;
end;

procedure TPageRenderEngine.DoRenderSprite(Sprite: TPageSprite);
var
  DestRect: TSDL_Rect;
  FlipFlags: Integer = 0;
  Alpha: Byte = 255;
begin
  if sfFlipH in Sprite.Flags then
    FlipFlags := FlipFlags or SDL_FLIP_HORIZONTAL;
  if sfFlipV in Sprite.Flags then
    FlipFlags := FlipFlags or SDL_FLIP_VERTICAL;
  if sfEnableAlpha in Sprite.Flags then
    Alpha := Sprite.Alpha;

  DestRect.h := SpriteHeight;
  DestRect.w := SpriteWidth;
  DestRect.x := Sprite.X;
  DestRect.y := Sprite.Y;

  SDL_SetTextureAlphaMod(FTextureManager^.Textures[Sprite.TextureID].
    TexturePointer, Alpha);
  SDL_RenderCopyEx(FMemoryWrapper.SDLRenderer, FTextureManager^.
    Textures[Sprite.TextureID].TexturePointer, nil, @DestRect, 0, nil,
    FlipFlags);
end;

constructor TPageRenderEngine.Create(aMemoryWrapper: IPageMemoryWrapper;
  aTextureManager: PPageTextureManager);
var
  intLoop: Integer;
begin
  FMemoryWrapper := aMemoryWrapper;
  FTextureManager := aTextureManager;
  TilemapCount := 0;
  for intLoop := 0 to PAGE_MAX_TILEMAPS-1 do
  begin
    FTilemaps[intLoop] := nil;
    FTilemapOffsets[intLoop] := PAGE_COORDINATE2D_NULL;
    FSpriteStreams[intLoop] := nil;
    FRenderedBackgrounds[intLoop] := nil;
  end;
end;

procedure TPageRenderEngine.DoRender;
begin
  //SDL_SetRenderDrawColor(FMemoryWrapper.SDLRenderer, 128, 0, 128, 255);
  SDL_RenderClear(FMemoryWrapper.SDLRenderer);
  DoRenderPerLayer;
  SDL_RenderPresent(FMemoryWrapper.SDLRenderer);
end;

procedure TPageRenderEngine.SetTilemap(Number: Word; TileMap: PPageTileMap);
begin
  { TODO: Range check }
  { TODO: Free memory if slot was used before }
  FTilemaps[Number] := TileMap;
end;

procedure TPageRenderEngine.SetSpriteStreamPointer(Number: Word;
  SpriteStreamPointer: Pointer);
begin
  { TODO: Range check }
  { TODO: Free memory if slot was used before }
  FSpriteStreams[Number] := SpriteStreamPointer;
end;

procedure TPageRenderEngine.BuildTilemaps;
var
  intLoop: Integer;
begin
  for intLoop := 0 to High(FTilemaps) do
    if FTilemaps[intLoop] <> nil then
    begin
      FTilemaps[intLoop]^.Free;
      FTilemaps[intLoop] := nil;
    end;

  for intLoop := 0 to TilemapCount-1 do
  begin
    New(FTilemaps[intLoop]);
    FMemoryWrapper.Tilemaps[intLoop] := PAGE_EMPTY_TILEMAPINFO;
    FTilemaps[intLoop]^ := TPageTilemap.Create(FMemoryWrapper.
      TilemapsPointer[intLoop], FMemoryWrapper.VRAMMemoryManagerInterface);
    FTilemaps[intLoop]^.SetMapSize(FMemoryWrapper.RenderEngineInfo^.
      CanvasDimension.X div TileWidth, FMemoryWrapper.RenderEngineInfo^.
      CanvasDimension.Y div TileHeight);
  end;

  InitializeRenderedBackgroundsTextures
end;

procedure TPageRenderEngine.BuildSpriteStreams;
var
  intLoop, intSprites: Integer;
begin
  for intLoop := 0 to High(FSpriteStreams) do
    if FSpriteStreams[intLoop] <> nil then
    begin
      FreeMem(FSpriteStreams[intLoop]);
      FSpriteStreams[intLoop] := nil;
    end;

  for intLoop := 0 to TilemapCount-1 do
  begin
    FSpriteStreams[intLoop] := AllocMem(SizeOf(TPageSprite)*PAGE_MAX_SPRITES);
    for intSprites := 0 to PAGE_MAX_SPRITES-1 do
      Move(EMPTY_SPRITE, (FSpriteStreams[intLoop]+SizeOf(TPageSprite)*intSprites)^,
        SizeOf(TPageSprite));
  end;
end;

end.

